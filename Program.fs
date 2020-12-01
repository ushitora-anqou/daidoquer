// Learn more about F# at http://fsharp.org

open DSharpPlus
open DSharpPlus.CommandsNext
open DSharpPlus.CommandsNext.Attributes
open DSharpPlus.Entities
open DSharpPlus.EventArgs
open DSharpPlus.VoiceNext
open Google.Cloud.TextToSpeech.V1
open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks

type DaidoquerCommand() =
    inherit BaseCommandModule()

    member private this.RespondAsync (ctx: CommandContext) (msg: string) =
        ctx.RespondAsync(msg)
        |> Async.AwaitTask
        |> Async.Ignore

    member private this.Wrap (ctx: CommandContext) (atask: Async<unit>) =
        async {
            try
                do! atask
            with
            | Failure (msg) ->
                eprintfn "Error: %s" msg
                do! this.RespondAsync ctx ("Error: " + msg)
            | err ->
                eprintfn "Error: %A" err
                do! this.RespondAsync ctx "Error: Something goes wrong on our side."
        }
        |> Async.StartAsTask :> Task

    [<Command("join"); Description("Join the channel")>]
    member public this.Join(ctx: CommandContext) =
        async {
            let vnext = ctx.Client.GetVoiceNext()

            if vnext = null
            then failwith "VNext is not enabled or configured."

            let vnc = vnext.GetConnection(ctx.Guild)

            if vnc <> null
            then failwith "Already connected in this guild."

            if ctx.Member = null
               || ctx.Member.VoiceState = null
               || ctx.Member.VoiceState.Channel = null then
                failwith "You are not in a voice channel."

            let chn = ctx.Member.VoiceState.Channel

            eprintfn "Connecting to %s..." chn.Name
            let! vnc = vnext.ConnectAsync(chn) |> Async.AwaitTask
            eprintfn "Connected to %s" chn.Name

            do! vnc.SendSpeakingAsync(false) |> Async.AwaitTask
            do! this.RespondAsync ctx ("Connected to" + chn.Name)
        }
        |> this.Wrap ctx

    [<Command("leave"); Description("Leave the channel")>]
    member public this.Leave(ctx: CommandContext) =
        async {
            let vnext = ctx.Client.GetVoiceNext()

            if vnext = null
            then failwith "VNext is not enabled or configured."

            let vnc = vnext.GetConnection(ctx.Guild)
            if vnc = null then failwith "Not connected in this guid."

            eprintfn "Disconnecting..."
            vnc.Disconnect()
            eprintfn "Disconnected"

            do! this.RespondAsync ctx "Disconnected"
        }
        |> this.Wrap ctx

let getVoiceAsync text (langCode, name) (outStream: VoiceTransmitSink) =
    (* FIXME: outStream can be of type Stream, but VoiceTransmitSink is not Stream,
    which uses extension methods.*)
    async {
        let! client =
            TextToSpeechClient.CreateAsync()
            |> Async.AwaitTask

        let input = new SynthesisInput(Text = text)

        let voice =
            new VoiceSelectionParams(LanguageCode = langCode, Name = name)

        let config =
            new AudioConfig(AudioEncoding = AudioEncoding.Mp3)

        let request =
            new SynthesizeSpeechRequest(Input = input, Voice = voice, AudioConfig = config)

        let! response =
            client.SynthesizeSpeechAsync(request)
            |> Async.AwaitTask

        let bytes = response.AudioContent.ToByteArray()

        use ffmpeg =
            Process.Start
                (new ProcessStartInfo(FileName = "ffmpeg",
                                      Arguments = "-i pipe:0 -ac 2 -f s16le -ar 48000 pipe:1",
                                      RedirectStandardInput = true,
                                      RedirectStandardOutput = true,
                                      UseShellExecute = false))

        let! writer =
            async {
                do! ffmpeg.StandardInput.BaseStream.WriteAsync(bytes, 0, bytes.Length)
                    |> Async.AwaitTask

                ffmpeg.StandardInput.Close()
            }
            |> Async.StartChild

        let! reader =
            async {
                do! ffmpeg.StandardOutput.BaseStream.CopyToAsync(outStream)
                    |> Async.AwaitTask
            }
            |> Async.StartChild

        do! [ writer; reader ]
            |> Async.Parallel
            |> Async.Ignore
    }

exception IgnoreEvent of unit

let onMessage (client: DiscordClient) (args: MessageCreateEventArgs) (voice: VoiceNextExtension) =
    let ignoreEvent () = raise (IgnoreEvent())

    async {
        try
            let msg = args.Message.Content

            if args.Author.IsCurrent || msg.StartsWith("!ddq")
            then ignoreEvent ()

            let vnc = voice.GetConnection(args.Guild)
            if vnc = null then failwith "Not connected in this guild"

            (*
            if args.Message.Content.ToLower().StartsWith("ping") then
                do! args.Message.RespondAsync("pong")
                    |> Async.AwaitTask
                    |> Async.Ignore
            *)

            while vnc.IsPlaying do
                do! vnc.WaitForPlaybackFinishAsync()
                    |> Async.AwaitTask

            eprintfn "%s" msg

            try
                do! vnc.SendSpeakingAsync(true) |> Async.AwaitTask
                let txStream = vnc.GetTransmitSink()
                do! getVoiceAsync msg ("ja-JP", "ja-JP-Wavenet-B") txStream
                do! txStream.FlushAsync() |> Async.AwaitTask

                do! vnc.WaitForPlaybackFinishAsync()
                    |> Async.AwaitTask

                do! vnc.SendSpeakingAsync(false) |> Async.AwaitTask
            with err ->
                do! vnc.SendSpeakingAsync(false) |> Async.AwaitTask
                raise err
        with
        | IgnoreEvent () -> ()
        | Failure (msg) ->
            eprintfn "Error: %s" msg

            do! args.Message.RespondAsync("Error: " + msg)
                |> Async.AwaitTask
                |> Async.Ignore
        | err ->
            eprintfn "Error: %A" err

            do! args.Message.RespondAsync("Error: Something goes wrong on our side.")
                |> Async.AwaitTask
                |> Async.Ignore
    }

[<EntryPoint>]
let main argv =
    let token =
        Environment.GetEnvironmentVariable "DISCORD_TOKEN"

    if token = null
    then failwith "Set envvar DISCORD_TOKEN and LOGFILE"

    printfn "Preparing..."

    let conf =
        new DiscordConfiguration(Token = token, TokenType = TokenType.Bot, AutoReconnect = true)

    let client = new DiscordClient(conf)

    let cconf =
        new CommandsNextConfiguration(EnableMentionPrefix = true, StringPrefixes = [ "!ddq" ])

    let commands = client.UseCommandsNext(cconf)
    commands.RegisterCommands<DaidoquerCommand>()

    let voice = client.UseVoiceNext()

    client.add_MessageCreated
        (new Emzi0767.Utilities.AsyncEventHandler<DiscordClient, MessageCreateEventArgs>(fun s e ->
        (fun () -> onMessage s e voice |> Async.StartAsTask :> Task)
        |> Task.Run
        |> ignore

        Task.CompletedTask))

    printfn "Connecting to the server..."

    client.ConnectAsync()
    |> Async.AwaitTask
    |> Async.RunSynchronously

    printfn "Done."

    Task.Delay(-1)
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> ignore

    0 // return an integer exit code
