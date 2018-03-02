{-# LANGUAGE OverloadedStrings #-}
import           Clay


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


defaultStylesheet :: Css
defaultStylesheet = do
    body ? do
        color primaryTextColor
        margin1 nil
    header ? do
        background darkPrimaryColor


main :: IO ()
main = putCss defaultStylesheet
