{-# LANGUAGE OverloadedStrings #-}
import           Clay
import qualified Clay.Flexbox as FB


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
        fontFamily [] [monospace]
        color "#563D7C"
        margin1 nil
        height (pct 100)
        display flex
        flexDirection column
        star # selection ?
            background accentColor
    header ? do
        primaryBoxShadow
        nav <? do
          -- Concider a loop of colors
          ":nth-child(1)" & do
            backgroundColor beige
          ":nth-child(2)" & do
            backgroundColor lemonchiffon
          ":nth-child(3)" & do
            backgroundColor papayawhip
          ":nth-last-child(n+2)" & do
            borderBottom solid (px 1) (rgba 86 61 124 0.2) -- beige, khaki
          ul <? do
            display flex
            flexFlow row FB.wrap
            maxWidth (px 1170)
            margin nil auto nil auto
            padding1 nil
            li <? do
              listStyleType none
              a <? do
                textTransform capitalize
                display block
                padding1 (px 15)
                fontSize (px 14)
                textDecoration none
                color "#563D7C"
                ":hover" & backgroundColor "#f9f9f9"
                ".focus" & fontWeight bold
                ":visited" & color "#563D7C"

    main_ ? do
        flexGrow 1
        flexShrink 0
        flexBasis auto
        Clay.div <? do
          maxWidth (px 1170)
          margin nil auto nil auto
          padding1 nil
          section <? do
            padding1 (px 15)
    footer ? do
        flexShrink 0
        Clay.div <? do
          maxWidth (px 1170)
          margin nil auto nil auto
          padding1 nil
          section <? do
            padding1 (px 15)
    -- worse then elm


main :: IO ()
main = putCss defaultStylesheet
