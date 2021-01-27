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
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open System.CodeDom
open System.CodeDom.Compiler

let escapeStr src =
    use writer = new StringWriter()

    use provider = CodeDomProvider.CreateProvider("CSharp")

    provider.GenerateCodeFromExpression(new CodePrimitiveExpression(src), writer, null)
    writer.ToString()

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

            eprintfn "Connecting to %s..."
            <| escapeStr chn.Name

            let! vnc = vnext.ConnectAsync(chn) |> Async.AwaitTask
            eprintfn "Connected to %s" <| escapeStr chn.Name

            do! vnc.SendSpeakingAsync(false) |> Async.AwaitTask
            do! this.RespondAsync ctx ("Connected to " + chn.Name)
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
    (* FIXME: Why do we need type annotation `VoiceTransmitSink`? *)
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
                                      RedirectStandardError = true,
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

// Thanks to: https://www.regextester.com/94502
let regexURL =
    new Regex(@"(?:http(s)?:\/\/)?[\w.-]+(?:\.[\w\.-]+)+[\w\-\._~:/?#[\]@!\$%&'\(\)\*\+,;=.]+", RegexOptions.Compiled)

let regexCode =
    new Regex(@"```.+```", RegexOptions.Compiled ||| RegexOptions.Singleline)

let regexCustomEmoji = new Regex(@"<:([^:]+):[0-9]+>")

let convertMessage msg =
    // For URL
    let msg = regexURL.Replace(msg, " 。ちくわ大明神。 ")

    // For code
    let msg = regexCode.Replace(msg, " 。ちくわ大明神。 ")

    // For custom emoji
    let msg = regexCustomEmoji.Replace(msg, "$1")

    // For length limit
    let si = new System.Globalization.StringInfo(msg)
    let limitLength = 100

    let msg =
        if si.LengthInTextElements > limitLength then
            si.SubstringByTextElements(0, limitLength)
            + " 。以下ちくわ大明神"
        else
            msg

    msg

type VoiceMessage = DiscordGuild * string * DiscordMessage option

let buildMessageProc (voice: VoiceNextExtension) =
    MailboxProcessor<VoiceMessage>.Start
    <| fun inbox ->
        let rec loop () =
            async {
                let! guild, msgStr, dmsgOpt = inbox.Receive()

                try
                    let vnc = voice.GetConnection(guild)

                    if vnc <> null then
                        let msg = convertMessage msgStr

                        if msg <> "" then
                            escapeStr msg
                            |> printfn "Speaking (guild #%d): %s" guild.Id

                            do! vnc.SendSpeakingAsync(true) |> Async.AwaitTask

                            let txStream = vnc.GetTransmitSink()
                            do! getVoiceAsync msg ("ja-JP", "ja-JP-Wavenet-A") txStream
                            do! txStream.FlushAsync() |> Async.AwaitTask

                            do! vnc.WaitForPlaybackFinishAsync()
                                |> Async.AwaitTask

                            do! vnc.SendSpeakingAsync(false) |> Async.AwaitTask
                with err ->
                    eprintfn "Error: %A" err

                    match dmsgOpt with
                    | None -> ()
                    | Some dmsg ->
                        try
                            do! dmsg.RespondAsync("Error: Something goes wrong on our side.")
                                |> Async.AwaitTask
                                |> Async.Ignore
                        with err -> eprintfn "Failed to respond: %A" err

                return! loop ()
            }

        loop ()

let guild2proc: Map<uint64, MailboxProcessor<VoiceMessage>> ref = ref Map.empty

let findProc (voice: VoiceNextExtension) (guild: DiscordGuild) =
    match (!guild2proc).TryFind guild.Id with
    | Some proc -> proc
    | None ->
        let newProc = buildMessageProc voice
        newProc.Error.Add(fun e -> eprintfn "Proc died (Guild #%d)" guild.Id)
        guild2proc := (!guild2proc).Add(guild.Id, newProc)
        newProc

let onMessage (client: DiscordClient) (args: MessageCreateEventArgs) (voice: VoiceNextExtension) =
    if args.Author.IsCurrent
       || args.Message.Content.StartsWith("!ddq") then
        ()
    else
        let proc = findProc voice args.Guild
        proc.Post(args.Guild, args.Message.Content, Some args.Message)

let onVoiceStateUpdated (client: DiscordClient) (args: VoiceStateUpdateEventArgs) (voice: VoiceNextExtension) =
    let proc = findProc voice args.Guild

    if args.Before = null || args.Before.Channel = null then
        let msg = sprintf "%sが参加しました" args.User.Username
        proc.Post(args.Guild, msg, None)
    else if args.After = null || args.After.Channel = null then
        let msg = sprintf "%sが離れました" args.User.Username
        proc.Post(args.Guild, msg, None)


[<EntryPoint>]
let main argv =
    let token =
        Environment.GetEnvironmentVariable "DISCORD_TOKEN"

    if token = null
    then failwith "Set envvar DISCORD_TOKEN and LOGFILE"

    printfn "Preparing..."

    let conf =
        new DiscordConfiguration(Token = token,
                                 TokenType = TokenType.Bot,
                                 AutoReconnect = true,
                                 Intents =
                                     (DiscordIntents.AllUnprivileged
                                      ||| DiscordIntents.GuildMembers))

    let client = new DiscordClient(conf)

    let cconf =
        new CommandsNextConfiguration(EnableMentionPrefix = true, StringPrefixes = [ "!ddq" ])

    let commands = client.UseCommandsNext(cconf)
    commands.RegisterCommands<DaidoquerCommand>()

    let voice = client.UseVoiceNext()

    client.add_MessageCreated
        (new Emzi0767.Utilities.AsyncEventHandler<DiscordClient, MessageCreateEventArgs>(fun s e ->
        onMessage s e voice
        Task.CompletedTask))

    client.add_VoiceStateUpdated
        (new Emzi0767.Utilities.AsyncEventHandler<DiscordClient, VoiceStateUpdateEventArgs>(fun s e ->
        onVoiceStateUpdated s e voice
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
