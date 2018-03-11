--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (zipWithM_, mapM_)
import           Data.List (insert, deleteBy)
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
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = normalise (takeDirectory p </> takeBaseName p </> "index.html")
                            where p = toFilePath ident


cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "css/*.hs" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])


    match "cv.markdown" $ do
        addToMenu toTitle
        route $ cleanRoute
        compile $ do
            menu <- getMenu

            pandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" (defaultContext <> menu)
              >>= relativizeUrls
              >>= cleanIndexHtmls


    match "posts/**.markdown" $ do
        addToMenu toTitle
        route $ cleanRoute
        compile $ do
            menu <- getMenu

            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" (postCtx <> menu)
                >>= relativizeUrls
                >>= cleanIndexHtmls

    match "index.html" $ do
        addToMenu toTitle
        route idRoute
        compile $ do

            menu <- getMenu

            posts <- recentFirst =<< loadAll ("posts/**.markdown" .&&. hasNoVersion)

            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    menu <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexHtmls


    match "404.markdown" $ do
        route $ cleanRoute
        compile $ do
            menu <- getMenu

            pandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" (defaultContext <> menu)
              >>= relativizeUrls
              >>= cleanIndexHtmls

    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    --should load categories
    match "posts/**.html" $ do
        route idRoute
        compile $ do
            menu <- getMenu

            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" (defaultContext <> menu)
                >>= relativizeUrls
                >>= cleanIndexHtmls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

-------------------------------------------------------------------------------
instance Writable (a, b) where
  write p = write p


addToMenu :: (FilePath -> FilePath) -> Rules ()
addToMenu f = version "routes" $ compile $ makeRouteItem f =<< maybeToRoute Nothing


toTitle :: FilePath -> FilePath
toTitle "index.html" = "Home"
toTitle "cv/index.html" = "Curriculum Vitae"
toTitle x = dropIndex x


dropIndex :: FilePath -> FilePath
dropIndex p | takeBaseName p == "index" = dropFileName p
            | otherwise                 = p


--dette giver ikke mening som det er lige nu...
makeRouteItem :: (String -> String) -> String -> Compiler (Item (String, String))
makeRouteItem f x =  makeItem (x, f x)


maybeToRoute :: Maybe String -> Compiler String
maybeToRoute v = fmap (fromMaybe "") (routeForUnderlying v)


routeForUnderlying :: Maybe String -> Compiler (Maybe FilePath)
routeForUnderlying v = getRoute =<< (setUnderlyingVersion v)


setUnderlyingVersion :: Maybe String -> Compiler Identifier
setUnderlyingVersion v = fmap (setVersion v) getUnderlying

-------------------------------------------------------------------------------
loadAllBody :: Pattern -> Compiler [(String, String)]
loadAllBody p = do
    items <- loadAll p
    return (fmap itemBody items)

-------------------------------------------------------------------------------
-- Can perhaps be empty in case of 404. This wont work
--ACCTUALLYY i NEED EACH MENU LEVEL TO BE FOCUSED OR NOT...?!
--THATWAY I CAN maybe easier check that we dont have an empty level in the middle of the menu?

-- change ordering of filepath and title.
-- overvej record type
data MenuLevel = MenuLevel [(FilePath, Title)] Focus [(FilePath, Title)] deriving Show

type Title = String

data Focus = Focus (FilePath, Title) | NoFocus (FilePath, Title) deriving Show -- phantom type?

focus :: (FilePath, Title) -> MenuLevel
focus x = MenuLevel [] (Focus x) []

noFocus :: (FilePath, Title) -> MenuLevel
noFocus x = MenuLevel [] (NoFocus x) []


--such function much wow.
insertRight :: (FilePath, Title) -> MenuLevel -> MenuLevel
insertRight y (MenuLevel ls (NoFocus x) rs) =
    if x == y then
      MenuLevel (rs ++ ls) (NoFocus y) []
    else
      MenuLevel (delete y ls) (NoFocus x) (delete y rs ++ [y]) --concider foldr
insertRight y (MenuLevel ls x rs) =
  MenuLevel (delete y ls) x (delete y rs ++ [y])


delete = deleteBy (\a b -> (fst a) == (fst b))

insertFocus :: (FilePath, Title) -> MenuLevel -> MenuLevel
insertFocus y (MenuLevel ls (NoFocus x) rs) = MenuLevel (rs ++ (x:ls)) (Focus y) []
insertFocus y (MenuLevel ls x rs) = MenuLevel (rs ++ ls) (Focus y) []


data Menu = Menu [MenuLevel] [MenuLevel] deriving Show
-- Menu [Focused MenuLevel] NoFocus/Focus MenuLevel
-- Menu [Focused MenuLevel] NoFocus/Focus MenuLevel
-- Menu [Focused MenuLevel] NoFocus/Focus MenuLevel


emptyMenu :: Menu
emptyMenu = Menu [] []


toList :: Menu -> [MenuLevel]
toList (Menu ls rs) = reverse ls ++ rs


rewind :: Menu -> Menu
rewind (Menu ls rs) = Menu [] (reverse ls ++ rs)


push :: (FilePath, Title) -> Bool -> Menu -> Menu
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
buildMenu :: FilePath -> [(FilePath, Title)] -> Menu
buildMenu currentRoute = foldl (extendMenu currentRoute) emptyMenu


relevant :: FilePath -> (FilePath, Title) -> [(FilePath, Title)]
relevant this (other, title) = relevant' (splitPath this) (splitPath other)
    where
        relevant' _ ([y@"index.html"]) = [(y, title)]
        relevant' (x:xs) ("cv/":["index.html"]) = [("cv/", title)]
        relevant' (x:xs) (y:["index.html"]) = [(y, dropTrailingPathSeparator y)]
        relevant' (x:xs) (y:ys) = (y, y) : if x == y then relevant' xs ys else []
        relevant' [] (y:["index.html"]) = [(y, dropTrailingPathSeparator y)]
        relevant' [] (y:_) = [(y, y)]
        relevant' _ _ = []


reCleanRoute :: String -> String
reCleanRoute s = if s == "/index.html" then s else replaceAll pattern replacement s
  where
    pattern = "/index.html"
    replacement = const ".html"


--addtomenu tager navn!

empty :: FilePath
empty = return pathSeparator


extendMenu :: FilePath -> Menu -> (FilePath, Title) -> Menu
extendMenu currentRoute menu =
  addMenu menu empty . relevant currentRoute
    where
      focused = splitPath (empty </> currentRoute)
      addMenu mx _ [] = rewind mx
      addMenu mx acc (x:xs) = case (fst x) of
        "index.html" | acc /= empty -> rewind mx
        _ -> addMenu (push url focus mx) filePath xs
        where
          filePath = acc </> (fst x) --DROPINDEX FUNCTION RIGHT HERE istedet for case x of
          url = (toUrl filePath, snd x)
          focus = and (zipWith (==) (splitPath (reCleanRoute filePath)) focused)


-------------------------------------------------------------------------------
showMenu :: Menu -> String
showMenu = renderHtml . zipWithM_ showMenuLevel [0..] . toList


showMenuLevel :: Int -> MenuLevel -> H.Html
showMenuLevel d (MenuLevel ls x rs) = H.nav $ H.ul (mapM_ H.li elems)
  where
    -- concider to list
    elems = map (showMenuItem) (reverse ls) ++ (showMenuFocusItem x) : map showMenuItem rs

--bad structure if we need this information elsewhere in our code.
showMenuItem :: (FilePath, Title) -> H.Html
showMenuItem (e, n) = H.a (H.toHtml n) ! A.href (H.toValue e)
--  where
 --   name = last (splitPath (dropExtension n))
  --  name' =
   --   case name of
    --    "index" -> "home"
     --   "cv" -> "Curriculum Vitae"
      --  _ -> name


-- worse then elm
showMenuFocusItem :: Focus -> H.Html
showMenuFocusItem (Focus e) = showMenuItem e ! A.class_ "focus"
showMenuFocusItem (NoFocus e) = showMenuItem e

-------------------------------------------------------------------------------
-- Could split this up. Is this even worth it?
moveIndexToFront :: MonadMetadata m => [(String, String)] -> m [(String, String)]
moveIndexToFront itemList =
    return (moveToFront "index.html" itemList)
        where
          moveToFront x xs =
            case break (\y -> (fst y) == x) xs of
              (a, y:ys) -> y:a ++ ys
              (a, ys) -> a ++ ys
