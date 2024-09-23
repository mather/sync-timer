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
    , fgFontRequest: String
    , fgFontDDinBold : String
    , fgFontLora : String
    , fgFontDSEG7ClassicBold : String
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
    , fgFontRequest = "他のフォントをリクエストする"
    , fgFontDDinBold = "D-DIN Bold (サンセリフ体)"
    , fgFontLora = "Lora (セリフ体)"
    , fgFontDSEG7ClassicBold = "DSEG7 (デジタル時計)"
    , bgColor = "背景色"
    , bgColorGB = "グリーンバック(GB, #00ff00)"
    , bgColorBB = "ブルーバック(BB, #0000ff)"
    , bgColorTP = "なし (White)"
    }


labelEn : Label
labelEn =
    { start = "Start"
    , pause = "Pause"
    , reset = "Reset"
    , backward = "Backward 1sec"
    , forward = "Forward 1sec"
    , minusSign = "minus"
    , displaySetting = "Display Settings"
    , showHour = "Show hour digits"
    , showProgress = "Show underline as countdown progress"
    , fgColor = "Text Color (picker)"
    , fgColorRgb = "Text Color (in RGB)"
    , fgFont = "Timer Font"
    , fgFontRequest = "Request another font"
    , fgFontDDinBold = "D-DIN Bold (Sans Serif)"
    , fgFontLora = "Lora (Serif)"
    , fgFontDSEG7ClassicBold = "DSEG7 (Classic digital watch)"
    , bgColor = "Background Color"
    , bgColorGB = "Greenback (GB, #00ff00)"
    , bgColorBB = "Blueback (BB, #0000ff)"
    , bgColorTP = "White (#ffffff)"
    }
