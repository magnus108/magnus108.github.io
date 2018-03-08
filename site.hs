--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (zipWithM_, mapM_)
import           Data.List (insert)
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe)
import           Hakyll
import           System.FilePath

import Text.Blaze.Internal (preEscapedString)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

import Debug.Trace

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "css/*.hs" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

    match "posts/*.markdown" $ do
        addToMenu
        route $ setExtension "html"
        compile $ do
            menu <- getMenu

            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" (postCtx <> menu)
                >>= relativizeUrls

    match "index.html" $ do
        addToMenu
        route idRoute
        compile $ do

            menu <- getMenu

            posts <- recentFirst =<< loadAll ("posts/*.markdown" .&&. hasNoVersion)

            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    menu <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    -- we can have no focus!!!!!!!!!!!!
    match "404.markdown" $ do
        route $ setExtension "html"
        compile $ do
            menu <- getMenu

            pandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" (defaultContext <> menu)

    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    match "posts/index.html" $ do
        route idRoute
        compile $ do
            menu <- getMenu

            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" (defaultContext <> menu)
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

-------------------------------------------------------------------------------
addToMenu :: Rules ()
addToMenu = version "routes" $ compile $ makeItem =<< maybeToRoute Nothing


maybeToRoute :: Maybe String -> Compiler String
maybeToRoute v = fmap (fromMaybe "") (routeForUnderlying v)


routeForUnderlying :: Maybe String -> Compiler (Maybe FilePath)
routeForUnderlying v = getRoute =<< (setUnderlyingVersion v)


setUnderlyingVersion :: Maybe String -> Compiler Identifier
setUnderlyingVersion v = fmap (setVersion v) getUnderlying

-------------------------------------------------------------------------------
loadAllBody :: Pattern -> Compiler [String]
loadAllBody p = do
    items <- loadAll p
    return (fmap itemBody items)

-------------------------------------------------------------------------------
-- Can perhaps be empty in case of 404. This wont work
--ACCTUALLYY i NEED EACH MENU LEVEL TO BE FOCUSED OR NOT...?!
--THATWAY I CAN maybe easier check that we dont have an empty level in the middle of the menu?

data MenuLevel = MenuLevel [FilePath] Focus [FilePath]

data Focus = Focus FilePath | NoFocus FilePath -- phantom type?

focus :: FilePath -> MenuLevel
focus x = MenuLevel [] (Focus x) []

noFocus :: FilePath -> MenuLevel
noFocus x = MenuLevel [] (NoFocus x) []


insertRight :: FilePath -> MenuLevel -> MenuLevel
insertRight y (MenuLevel ls x rs) = MenuLevel ls x (rs ++ [y])


insertFocus :: FilePath -> MenuLevel -> MenuLevel
insertFocus y (MenuLevel ls (NoFocus x) rs) =
    MenuLevel (rs ++ (x:ls)) (Focus y) []
insertFocus y (MenuLevel ls x rs) = MenuLevel (rs ++ ls) (Focus y) []


data Menu = Menu [MenuLevel] [MenuLevel]
-- Menu [Focused MenuLevel] NoFocus/Focus MenuLevel
-- Menu [Focused MenuLevel] NoFocus/Focus MenuLevel
-- Menu [Focused MenuLevel] NoFocus/Focus MenuLevel


emptyMenu :: Menu
emptyMenu = Menu [] []


toList :: Menu -> [MenuLevel]
toList (Menu ls rs) = reverse ls ++ rs


rewind :: Menu -> Menu
rewind (Menu ls rs) = Menu [] (reverse ls ++ rs)


push :: FilePath -> Bool -> Menu -> Menu
push x True (Menu ls []) = Menu ((focus x):ls) []
push x False (Menu ls []) = Menu ((noFocus x):ls) []
push x True (Menu ls (r:rs)) = Menu ((insertFocus x r):ls) rs
push x False (Menu ls (r:rs)) = Menu ((insertRight x r):ls) rs


getMenu :: Compiler (Context String)
getMenu = do
    routes <- moveIndexToFront =<< loadAllBody (hasVersion "routes")
    currentRoute <- maybeToRoute Nothing
    return $ constField "menu" $ showMenu $ buildMenu currentRoute routes


--fix emptymenu, such that it get initialized with correct focus
--perhaps foldl addmenu..
buildMenu :: FilePath -> [FilePath] -> Menu
buildMenu currentRoute = foldl (extendMenu currentRoute) emptyMenu


relevant :: FilePath -> FilePath -> [FilePath]
relevant this other = relevant' (splitPath this) (splitPath other)
    where
        relevant' (x:xs) (y:ys) = y : if x == y then relevant' xs ys else []
        relevant' [] (y:_) = [y]
        relevant' _ _ = []


empty :: FilePath
empty = return pathSeparator


extendMenu :: FilePath -> Menu -> FilePath -> Menu
extendMenu currentRoute menu =
  addMenu menu empty . relevant currentRoute
    where
      focused = splitPath (empty </> currentRoute)
      addMenu mx _ [] = rewind mx
      addMenu mx acc (x:xs) = addMenu (push url focus mx) filePath xs
        where
          filePath = acc </> x
          url = toUrl filePath
          focus = and (zipWith (==) (splitPath filePath) focused)


-------------------------------------------------------------------------------
showMenu :: Menu -> String
showMenu = renderHtml . zipWithM_ showMenuLevel [0..] . toList


showMenuLevel :: Int -> MenuLevel -> H.Html
showMenuLevel d (MenuLevel ls x rs) = H.nav $ H.ul (mapM_ H.li elems)
  where
    elems = map showMenuItem ls ++ (showMenuFocusItem x) : map showMenuItem rs


showMenuItem :: FilePath -> H.Html
showMenuItem e = H.a (H.toHtml name) ! A.href (H.toValue e)
  where
    name = last (splitPath (dropExtension e))


-- worse then elm
showMenuFocusItem :: Focus -> H.Html
showMenuFocusItem (Focus e) = showMenuItem e ! A.class_ "focus"
showMenuFocusItem (NoFocus e) = showMenuItem e

-------------------------------------------------------------------------------
-- Could split this up. Is this even worth it?
moveIndexToFront :: MonadMetadata m => [String] -> m [String]
moveIndexToFront itemList =
    return (moveToFront "index.html" itemList)
        where
          moveToFront x xs =
            case break (\y -> y == x) xs of
              (a, y:ys) -> y:a ++ ys
              (a, ys) -> a ++ ys
