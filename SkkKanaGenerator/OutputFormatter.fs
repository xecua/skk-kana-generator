module SkkKanaGenerator.OutputFormatter

open SkkKanaGenerator.Types
open SkkKanaGenerator.Kana

// 将来的には入力側も
[<AbstractClass>]
type Formatter(excludeComment: bool) =
    member internal this.excludeComment = excludeComment

    abstract member formatRule: Rule -> string

    abstract member formatComment: string -> string
    default this.formatComment comment = sprintf "# %s" comment

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
    // コメントは#
    // ,はRomに限り使用可能で、`&comma;`と記述
    // Okuriはある場合のみ記述する。それ以外は必要

    inherit Formatter(excludeComment)

    let formatRom (rom: string) =
        rom.ToCharArray()
        |> Seq.map (fun c ->
            match c with
            | ',' -> "&comma;"
            | _ -> c.ToString())
        |> String.concat ""

    override this.formatRule rule =
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
    // :,<shift>;と書くことで:をs-;扱いできる。こういうのどうしよう

    inherit Formatter(excludeComment)

    let formatRom (rom: string) =
        rom.ToCharArray()
        |> Seq.map (fun c ->
            match c with
            | ',' -> "&comma;"
            | '#' -> "&sharp;"
            | _ -> c.ToString())
        |> String.concat ""

    override this.formatRule rule =
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

    override this.formatRule rule =
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
           |> Seq.choose this.formatLine
           |> Seq.map (fun s -> "    " + s)
           |> String.concat ",\n")
        + "\n  ]\n"
        + "}"

[<Class>]
type LibcskkFormatter(excludeComment: bool) =
    // https://github.com/naokiri/cskk/blob/master/assets/rules/default/rule.toml
    // toml
    // libxkbcommonのkeysymsに一致するシーケンスは間にスペースを入れる必要がある
    // 個別に対応するのは大変なので全部スペース区切りにする
    inherit Formatter(excludeComment)

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

    override this.formatRule rule =
        let okuri = Option.defaultValue "" rule.Okuri
        sprintf """"%s" = ["%s", "%s"] """ (formatRom rule.Rom) okuri rule.Hira

    override this.formatLines lines =
        "[conversion]\n" + (lines |> Seq.choose this.formatLine |> String.concat "\n")

[<Class>]
type CorvusSkkFormatter() =
    // https://github.com/nathancorvussolis/corvusskk/blob/master/installer/config-sample/config%20-%20kana.xml
    // XML
    // または独自。こっちのがいいか
    // https://github.com/nathancorvussolis/corvusskk?tab=readme-ov-file#kanatable%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB
    // 独自の方はコメントは出力できない
    inherit Formatter(false)

    override this.formatRule(arg: Rule) : string =
        raise (System.NotImplementedException())

[<Class>]
type SkkeletonFormatter(excludeComment: bool) =
    // VimかLua (register_kanatableの引数)。
    // 一応独自もある
    // これでパースできる形 https://github.com/vim-skk/skkeleton/blob/954f2f96e74a0c409f12315278fb1bbef0286b60/denops/skkeleton/kana.ts#L65
    inherit Formatter(excludeComment)

    override this.formatRule(arg: Rule) : string =
        raise (System.NotImplementedException())
