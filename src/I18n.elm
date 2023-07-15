module I18n exposing (..)


type alias Label =
    { start : String
    , pause : String
    , reset : String
    , backward : String
    , forward : String
    , minusSign : String
    , displaySetting : String
    , showHour : String
    , showProgress : String
    , fgColor : String
    , fgColorRgb : String
    , fgFont : String
    , fgFontDDinBold : String
    , fgFontLora : String
    , bgColor : String
    , bgColorGB : String
    , bgColorBB : String
    , bgColorTP : String
    }


labelJa : Label
labelJa =
    { start = "開始"
    , pause = "一時停止"
    , reset = "リセット"
    , backward = "1秒戻す"
    , forward = "1秒進める"
    , minusSign = "マイナス"
    , displaySetting = "表示設定"
    , showHour = "1時間以上の動画を見る"
    , showProgress = "カウントダウンを視覚的に表現する"
    , fgColor = "文字色"
    , fgColorRgb = "文字色(RGB)"
    , fgFont = "フォント"
    , fgFontDDinBold = "D-DIN Bold (サンセリフ体)"
    , fgFontLora = "Lora (セリフ体)"
    , bgColor = "背景色"
    , bgColorGB = "グリーンバック(GB, #00ff00)"
    , bgColorBB = "ブルーバック(BB, #0000ff)"
    , bgColorTP = "なし (White)"
    }
