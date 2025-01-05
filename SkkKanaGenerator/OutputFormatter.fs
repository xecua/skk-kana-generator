module SkkKanaGenerator.OutputFormatter

open SkkKanaGenerator.Types
open SkkKanaGenerator.Kana

// 将来的には入力側も
[<AbstractClass>]
type Formatter(includeComment: bool) =
    member internal this.includeComment = includeComment

    abstract member formatRule: Rule -> string

    abstract member formatComment: string -> string
    default this.formatComment comment = sprintf "# %s" comment

    abstract member formatLine: Line -> string option

    default this.formatLine line =
        match line with
        | Comment c ->
            if this.includeComment then
                Some(this.formatComment c)
            else
                None
        | Rule r -> Some(this.formatRule r)
        | Empty -> Some ""

    // jsonとかならオーバーライドが必要
    abstract member formatLines: Line seq -> string

    default this.formatLines lines =
        lines |> Seq.choose this.formatLine |> String.concat "\n"

[<Class>]
type AquaSkkFormatter(includeComment: bool) =
    // https://github.com/codefirst/aquaskk/blob/master/data/config/kana-rule.conf
    // 改行コードはLF、文字コードはEUC-JP
    // コメントは#
    // ,はRomに限り使用可能で、`&comma;`と記述
    // Okuriはある場合のみ記述する

    inherit Formatter(includeComment)

    override this.formatRule(arg: Rule) : string =
        raise (System.NotImplementedException())

[<Class>]
type MacSkkFormatter(includeComment: bool) =
    // https://github.com/mtgto/macSKK/blob/main/macSKK/kana-rule.conf
    // 改行コードはLF、文字コードはBOMなしのUTF-8
    // コメントは#
    // ,は`&comma;`と記述。#は`&sharp;`と記述。
    // Kata以降は省略可能。
    inherit Formatter(includeComment)

    override this.formatRule(arg: Rule) : string =
        raise (System.NotImplementedException())

[<Class>]
type LibskkFormatter() =
    // https://github.com/ueno/libskk/blob/master/rules/README.rules
    // JSONなのでコメントは不可
    // 入力 -> ["Okuri", "Hira"] (KataとHanKataは省略可能)
    // 一部特殊入力は可能
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
type LibcskkFormatter(includeComment: bool) =
    // https://github.com/naokiri/cskk/blob/master/assets/rules/default/rule.toml
    // toml
    // 結構特殊。
    inherit Formatter(includeComment)

    override this.formatRule(arg: Rule) : string =
        raise (System.NotImplementedException())

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
type SkkeletonFormatter(includeComment: bool) =
    // VimかLua (register_kanatableの引数)。
    // 一応独自もある
    // これでパースできる形 https://github.com/vim-skk/skkeleton/blob/954f2f96e74a0c409f12315278fb1bbef0286b60/denops/skkeleton/kana.ts#L65
    inherit Formatter(includeComment)

    override this.formatRule(arg: Rule) : string =
        raise (System.NotImplementedException())
