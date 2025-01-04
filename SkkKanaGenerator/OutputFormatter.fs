module SkkKanaGenerator.OutputFormatter

open SkkKanaGenerator.Types

// 将来的には入力側も
[<AbstractClass>]
type Formatter(includeComment: bool) =
    member internal this.includeComment = includeComment

    abstract member convertRule: Rule -> string

    member this.convertLine(line: Line) : string option =
        match line with
        | Comment c -> if this.includeComment then Some c else None
        | Rule r -> this.convertRule r |> Some
        | Empty -> Some ""

    member this.convertLines = Seq.choose this.convertLine

    // jsonとかならオーバーライドが必要
    member this.format rules =
        rules |> this.convertLines |> String.concat "\n"

[<Class>]
type AquaSkkFormatter(includeComment: bool) =
    // https://github.com/codefirst/aquaskk/blob/master/data/config/kana-rule.conf
    // 改行コードはLF、文字コードはEUC-JP
    // コメントは#
    // ,はRomに限り使用可能で、`&comma;`と記述
    // Okuriはある場合のみ記述する

    inherit Formatter(includeComment)

    override this.convertRule(arg: Rule) : string =
        raise (System.NotImplementedException())

[<Class>]
type MacSkkFormatter(includeComment: bool) =
    // https://github.com/mtgto/macSKK/blob/main/macSKK/kana-rule.conf
    // 改行コードはLF、文字コードはBOMなしのUTF-8
    // コメントは#
    // ,は`&comma;`と記述。#は`&sharp;`と記述。
    // Kata以降は省略可能。
    inherit Formatter(includeComment)

    override this.convertRule(arg: Rule) : string =
        raise (System.NotImplementedException())

[<Class>]
type LibskkFormatter(includeComment: bool) =
    // https://github.com/ueno/libskk/blob/master/rules/README.rules
    // JSON
    // 入力 -> ["Okuri", "Hira"] (KataとHanKataは省略可能)
    // 一部特殊入力は可能
    inherit Formatter(includeComment)

    override this.convertRule(arg: Rule) : string =
        raise (System.NotImplementedException())

[<Class>]
type LibcskkFormatter(includeComment: bool) =
    // https://github.com/naokiri/cskk/blob/master/assets/rules/default/rule.toml
    // toml
    // 結構特殊。~~めんどい~~
    inherit Formatter(includeComment)

    override this.convertRule(arg: Rule) : string =
        raise (System.NotImplementedException())

[<Class>]
type CorvusSkkFormatter() =
    // https://github.com/nathancorvussolis/corvusskk/blob/master/installer/config-sample/config%20-%20kana.xml
    // XML
    // または独自。こっちのがいいか
    // https://github.com/nathancorvussolis/corvusskk?tab=readme-ov-file#kanatable%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB
    // 独自の方はコメントは出力できない
    inherit Formatter(false)

    override this.convertRule(arg: Rule) : string =
        raise (System.NotImplementedException())

[<Class>]
type SkkeletonFormatter(includeComment: bool) =
    // VimかLua (register_kanatableの引数)。
    // 一応独自もある
    // これでパースできる形 https://github.com/vim-skk/skkeleton/blob/954f2f96e74a0c409f12315278fb1bbef0286b60/denops/skkeleton/kana.ts#L65
    inherit Formatter(includeComment)

    override this.convertRule(arg: Rule) : string =
        raise (System.NotImplementedException())
