// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open Google.Cloud.TextToSpeech.V1
open DSharpPlus
open DSharpPlus.CommandsNext
open DSharpPlus.CommandsNext.Attributes
open DSharpPlus.Entities
open DSharpPlus.VoiceNext

// https://discordapp.com/oauth2/authorize?client_id=782919791207251970&scope=bot&permissions=8

type DaidoquerCommand() =
    [<Command("join"); Description("Join the channel")>]
    member public this.Join(ctx: CommandContext) =
        async {
            try
                let vnext = ctx.Client.GetVoiceNextClient()

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

                do! ctx.RespondAsync("Connected to " + chn.Name)
                    |> Async.AwaitTask
                    |> Async.Ignore
            with
            | Failure (msg) ->
                eprintfn "Error: %s" msg

                do! ctx.RespondAsync(msg)
                    |> Async.AwaitTask
                    |> Async.Ignore
            | err -> eprintfn "Error: %A" err
        }
        |> Async.StartAsTask :> Task

let getVoiceAsync text (langCode, name) outStream =
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

[<EntryPoint>]
let main argv =
    (*
    use output = File.Create("sample.pcm")

    getVoiceAsync "こんにちは世界2" ("ja-JP", "ja-JP-Wavenet-B") output
    |> Async.RunSynchronously
    |> ignore

    0 // return an integer exit code
*)

    let token =
        Environment.GetEnvironmentVariable "DISCORD_TOKEN"

    if token = null
    then failwith "Set envvar DISCORD_TOKEN and LOGFILE"

    printfn "Preparing..."
    let conf = new DiscordConfiguration()
    conf.set_Token token
    conf.set_TokenType TokenType.Bot
    conf.set_AutoReconnect true
    let client = new DiscordClient(conf)

    let cconf = new CommandsNextConfiguration()
    cconf.set_EnableMentionPrefix true
    cconf.set_StringPrefix "!ddq"

    let commands = client.UseCommandsNext(cconf)
    commands.RegisterCommands<DaidoquerCommand>()

    let voice = client.UseVoiceNext()

    (*
    bot.add_MessageCreated
        (new AsyncEventHandler<MessageCreateEventArgs>(fun e ->
        async {
            if e.Message.Content.ToLower().StartsWith("ping") then
                do! e.Message.RespondAsync("pong")
                    |> Async.AwaitTask
                    |> Async.Ignore
        }
        |> Async.StartAsTask :> Task))
*)

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

(*
    printfn "Starting to read log file..."
    use reader = File.OpenText(logfile)

    let regex =
        new Regex(@"^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2} \[(JOIN|LEAVE)\] (.+) (?:joined the game|left the game)$")

    let rec loop () =
        async {
            let! line = Async.AwaitTask <| reader.ReadLineAsync()

            if line = null then
                do! Async.Sleep 1000
                return! loop ()

            let m = regex.Match line
            if not m.Success then return! loop ()

            printfn "%s" line
            do! bot.SendMessageAsync(channel, line) |> Async.AwaitTask.Async.Ignore

            return! loop ()
        }

    printfn "Done."

    [ Task.Delay(-1) |> Async.AwaitTask
      loop () ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    0 // return an integer exit code
*)
