{-# LANGUAGE OverloadedStrings #-}
import           Clay


primaryBoxShadow :: Css
primaryBoxShadow = boxShadowWithSpread nil (px 2) (px 5) nil (rgba 0 0 0 0.3)


darkPrimaryColor :: Color
darkPrimaryColor = "#0097A7"


primaryColor :: Color
primaryColor = "#00BCD4"


lightPrimaryColor :: Color
lightPrimaryColor = "#B2EBF2"


iconColor :: Color
iconColor = "#FFFFFF"


primaryTextColor :: Color
primaryTextColor = "#212121"


secondaryTextColor :: Color
secondaryTextColor = "#757575"


accentColor :: Color
accentColor = "#FFEB3B"


dividerColor :: Color
dividerColor = "#BDBDBD"


margin1 :: Size a -> Css
margin1 x = margin x x x x


padding1 :: Size a -> Css
padding1 x = padding x x x x


defaultStylesheet :: Css
defaultStylesheet = do
    html ? do
        height (pct 100)
    body ? do
        color primaryTextColor
        margin1 nil
        height (pct 100)
        display flex
        flexDirection column
        star # selection ?
            background accentColor
    header ? do
        primaryBoxShadow
    main_ ? do
        padding1 (px 15)
        flexGrow 1
        flexShrink 0
        flexBasis auto
    footer ? do
        padding1 (px 15)
        flexShrink 0
    -- worse then elm
    ".focus" ? do
        backgroundColor accentColor


main :: IO ()
main = putCss defaultStylesheet
