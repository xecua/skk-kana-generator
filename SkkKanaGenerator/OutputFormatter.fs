module SkkKanaGenerator.OutputFormatter

open SkkKanaGenerator.Types
open SkkKanaGenerator.Kana

// 将来的には入力側も?
// 判別共用体にできるならしたい。できるかな
[<AbstractClass>]
type Formatter(excludeComment: bool) =
    member internal _.excludeComment = excludeComment

    abstract member formatRule: Rule -> string

    abstract member formatComment: string -> string
    default _.formatComment comment = sprintf "# %s" comment

    abstract member formatLine: Line -> string option

    default this.formatLine line =
        match line with
        | Comment c ->
            if this.excludeComment then
                None
            else
                Some(this.formatComment c)
        | Rule r -> Some(this.formatRule r)
        | Empty -> Some ""

    abstract member formatLines: Line seq -> string

    default this.formatLines lines =
        lines |> Seq.choose this.formatLine |> String.concat "\n"

[<Class>]
type AquaSkkFormatter(excludeComment: bool) =
    // https://github.com/codefirst/aquaskk/blob/master/data/config/kana-rule.conf
    // 改行コードはLF、文字コードはEUC-JP
    // ,はRomに限り使用可能で、`&comma;`と記述

    inherit Formatter(excludeComment)

    let formatRom (rom: string) =
        rom.ToCharArray()
        |> Seq.map (fun c ->
            match c with
            | ',' -> "&comma;"
            | _ -> c.ToString())
        |> String.concat ""

    override _.formatRule rule =
        let okuri = rule.Okuri |> Option.map (fun c -> "," + c) |> Option.defaultValue ""

        sprintf
            "%s,%s,%s,%s%s"
            (formatRom rule.Rom)
            rule.Hira
            (convertHira hiraToKana rule.Hira)
            (convertHira hiraToHanKata rule.Hira)
            okuri

[<Class>]
type MacSkkFormatter(excludeComment: bool) =
    // https://github.com/mtgto/macSKK/blob/main/macSKK/kana-rule.conf
    // 改行コードはLF、文字コードはBOMなしのUTF-8
    // :,<shift>;と書くことで:をs-;扱いできる。このあたりの対応は未定

    inherit Formatter(excludeComment)

    let formatRom (rom: string) =
        rom.ToCharArray()
        |> Seq.map (fun c ->
            match c with
            | ',' -> "&comma;"
            | '#' -> "&sharp;"
            | _ -> c.ToString())
        |> String.concat ""

    override _.formatRule rule =
        match rule.Okuri with
        | Some okuri ->
            (sprintf
                "%s,%s,%s,%s,%s"
                (formatRom rule.Rom)
                rule.Hira
                (convertHira hiraToKana rule.Hira)
                (convertHira hiraToHanKata rule.Hira)
                okuri)
        | None -> (sprintf "%s,%s" (formatRom rule.Rom) rule.Hira)

[<Class>]
type LibskkFormatter() =
    // https://github.com/ueno/libskk/blob/master/rules/README.rules

    inherit Formatter(false)

    override _.formatRule rule =
        let okuri = Option.defaultValue "" rule.Okuri
        sprintf """"%s": ["%s", "%s"]""" rule.Rom okuri rule.Hira

    override this.formatLine line =
        match line with
        | Rule r -> Some(this.formatRule r)
        | _ -> None

    override this.formatLines lines =
        "{\n"
        + "  \"define\": [\n"
        + (lines
           |> Seq.choose (fun line ->
               match line with
               | Empty -> None // TODO: 空行にする(カンマの挿入を回避する必要がある)
               | Comment _ -> None
               | Rule r -> Some("    " + (this.formatRule r)))
           |> String.concat ",\n")
        + "\n  ]\n"
        + "}"

[<Class>]
type LibcskkFormatter(excludeComment: bool) =
    // https://github.com/naokiri/cskk/blob/master/assets/rules/default/rule.toml

    inherit Formatter(excludeComment)

    // libxkbcommonのkeysymsに一致するシーケンスは間にスペースを入れる必要がある。
    // 個別に対応するのは大変なので全部スペース区切りにする
    let formatRom (rom: string) =
        rom.ToCharArray()
        |> Seq.map (fun c ->
            match c with
            | ' ' -> "space"
            | '!' -> "exclam"
            | '"' -> "quotedbl"
            | '#' -> "numbersign"
            | '$' -> "dollar"
            | '%' -> "percent"
            | '&' -> "ampersand"
            | '\'' -> "apostrophe"
            | '(' -> "parenleft"
            | ')' -> "parenright"
            | '*' -> "asterisk"
            | '+' -> "plus"
            | ',' -> "comma"
            | '-' -> "minus"
            | '.' -> "period"
            | '/' -> "slash"
            | ':' -> "colon"
            | ';' -> "semicolon"
            | '<' -> "less"
            | '=' -> "equal"
            | '>' -> "greater"
            | '?' -> "question"
            | '@' -> "at"
            | '[' -> "bracketleft"
            | '\\' -> "backslash"
            | ']' -> "bracketright"
            | '^' -> "asciicircum"
            | '_' -> "underscore"
            | '`' -> "grave"
            | '{' -> "braceleft"
            | '|' -> "bar"
            | '}' -> "braceright"
            | '~' -> "asciitilde"
            | c -> c.ToString())
        |> String.concat " "

    override _.formatRule rule =
        let okuri = Option.defaultValue "" rule.Okuri
        sprintf """"%s" = ["%s", "%s"] """ (formatRom rule.Rom) okuri rule.Hira

    override this.formatLines lines =
        "[conversion]\n" + (lines |> Seq.choose this.formatLine |> String.concat "\n")


type private CorvusSKKStyle =
    | Xml
    | Original

[<Class>]
type CorvusSkkFormatter(excludeComment: bool, style: string) =
    // https://github.com/nathancorvussolis/corvusskk/blob/master/installer/config-sample/config%20-%20kana.xml
    // XMLまたは独自(KanaTableファイル)
    // 独自ファイルの形式は https://github.com/nathancorvussolis/corvusskk?tab=readme-ov-file#kanatable%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB

    // 独自の方はコメントは出力できない
    inherit Formatter(excludeComment || style = "original")

    let style =
        match style with
        | "xml" -> CorvusSKKStyle.Xml
        | "original" -> CorvusSKKStyle.Original
        | _ ->
            eprintfn "--style value must be 'xml' or 'original'"
            exit 1

    // XMLに限り<と"と&はエスケープ必要。'と>は不要
    let formatRom (rom: string) =
        if style.IsOriginal then
            rom
        else
            rom.ToCharArray()
            |> Seq.map (fun c ->
                match c with
                | '<' -> "&lt;"
                | '"' -> "&quot;"
                | '&' -> "&amp;"
                | c -> c.ToString())
            |> String.concat ""

    override _.formatComment comment = sprintf "<!-- %s -->" comment

    override _.formatRule rule =
        match style with
        | CorvusSKKStyle.Xml ->
            sprintf
                """<row ro="%s" hi="%s" ka="%s" an="%s" so="%d" />"""
                (formatRom rule.Rom)
                rule.Hira
                (convertHira hiraToKana rule.Hira)
                (convertHira hiraToHanKata rule.Hira)
                (if rule.Okuri.IsSome then 1 else 0) // たぶんokuriって促音しかない

        | CorvusSKKStyle.Original ->
            sprintf
                "%s\t%s\t%s\t%s\t%d"
                (formatRom rule.Rom)
                rule.Hira
                (convertHira hiraToKana rule.Hira)
                (convertHira hiraToHanKata rule.Hira)
                (if rule.Okuri.IsSome then 1 else 0) // たぶんokuriって促音しかない

    override this.formatLines lines =
        match style with
        | CorvusSKKStyle.Xml ->
            "  <section name=\"kana\">\n"
            + "    <list>\n"
            + (lines
               |> Seq.choose this.formatLine
               |> Seq.map (fun s -> "      " + s)
               |> String.concat "\n")
            + "\n    </list>\n"
            + "  </section>"
        | CorvusSKKStyle.Original -> base.formatLines lines

type private SkkeletonStyle =
    | Vim
    | Lua
// TODO: vim9?

[<Class>]
type SkkeletonFormatter(excludeComment: bool, style: string) =
    // VimかLua (register_kanatableの引数)。一応個別ファイルがあるが、Okuriが記述できない
    // vimscriptは行継続にクセあり。特にコメント。 |line-continuation-comment|
    // 関数にマッピングすることもできるが対応は未定
    inherit Formatter(excludeComment)

    let style =
        match style with
        | "vim" -> SkkeletonStyle.Vim
        | "lua" -> SkkeletonStyle.Lua
        | _ ->
            eprintfn "--style value must be one of 'vim', 'lua'"
            exit 1

    override _.formatComment comment =
        (match style with
         | SkkeletonStyle.Vim -> "\" "
         | SkkeletonStyle.Lua -> "-- ")
        + comment

    override _.formatRule rule =
        let okuri =
            rule.Okuri |> Option.map (fun c -> ", \"" + c + "\"") |> Option.defaultValue ""

        match style with
        | SkkeletonStyle.Vim -> sprintf """"%s": ["%s"%s]""" rule.Rom rule.Hira okuri
        | SkkeletonStyle.Lua -> sprintf """["%s"] = {"%s"%s}""" rule.Rom rule.Hira okuri


    override this.formatLines(lines: Line seq) : string =
        match style with
        | SkkeletonStyle.Vim ->
            "{\n"
            + (lines
               |> Seq.choose (fun line ->
                   match line with
                   | Empty -> Some "\\  \n" // 空行でも冒頭の\は必要
                   | Comment c ->
                       if this.excludeComment then
                           None
                       else
                           Some("\"\\ " + c + "\n") // 行継続中のコメントは"\ ではじめる。formatCommentの出番はない
                   | Rule r -> Some("\\  " + (this.formatRule r) + ",\n"))
               |> String.concat "")
            + "\\ }"
        | SkkeletonStyle.Lua ->
            "{\n"
            + (lines
               |> Seq.choose (fun line ->
                   match line with
                   | Empty -> Some "  \n"
                   | Comment c -> if this.excludeComment then None else Some("  " + c + "\n")
                   | Rule r -> Some("  " + (this.formatRule r) + ",\n"))
               |> String.concat "")
            + "}"
