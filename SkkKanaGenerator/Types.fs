module SkkKanaGenerator.Types

// 変換ルール。値だけ持っとけばいいので構造体レコード
[<Struct>]
type Rule =
    { Rom: string
      Hira: string
      Okuri: option<string> }

// 入力の行
type Line =
    | Comment of string
    | Rule of Rule
    | Empty

// 出力フォーマット。クラスにして出力機能も持たせた方がいいかも?
type OutputFormat =
    | AquaSkk
    | MacSKK
    | Libskk
    | Libcskk
    | CorvusSKK
    | Skkeleton
