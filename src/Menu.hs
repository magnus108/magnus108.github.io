{-# LANGUAGE OverloadedStrings #-}
module Menu
where

import Hakyll
import Control.Monad
import Control.Arrow hiding (left, right)
import System.FilePath
import Data.List
import Data.Maybe
import Data.Function


import Text.Blaze.Internal (preEscapedString)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

--should move this
import qualified Data.Map.Lazy as M (fromList, findWithDefault)

import Debug.Trace


-------------------------------------------------------------------------------
-- fix working with maybe
addToMenu :: Rules ()
addToMenu = version "routes" $ compile $ makeItem =<< maybeToRoute Nothing


maybeToRoute :: Maybe String -> Compiler String
maybeToRoute v = fmap (fromMaybe "") (routeForUnderlying v)


routeForUnderlying :: Maybe String -> Compiler (Maybe FilePath)
routeForUnderlying v = getRoute =<< (setUnderlyingVersion v)


setUnderlyingVersion :: Maybe String -> Compiler Identifier
setUnderlyingVersion v = fmap (setVersion v) getUnderlying


-------------------------------------------------------------------------------
relevant :: FilePath -> FilePath -> [FilePath]
relevant = relevant' `on` (splitDirectories . dropIndex)
  where relevant' _ (y:[]) = [y]
        relevant' [] (y:_) = [addTrailingPathSeparator y]
        relevant' (x:xs) (y:ys) =
          (addTrailingPathSeparator y) : if x == y then relevant' xs ys else []
        relevant' _ _ = []


buildMenu currentRoute = fromLists currentRoute . map nub . transpose
  . map namePath . map (relevant currentRoute)


namePath :: [FilePath] -> [MenuItem]
namePath xs = map (id &&& name) xs
  where path = joinPath xs
        --Naming x is problematic as we want to
        name x = M.findWithDefault x path names


names = M.fromList [(".", "home"), ("cv", "cv")]


dropIndex :: FilePath -> FilePath
dropIndex p | takeBaseName p == "index" = dropFileName p
            | otherwise                 = p


type MenuItem = (FilePath, String)
data MenuLevel = MenuLevel [MenuItem] MenuItem [MenuItem] deriving (Show)
data Menu = Menu [MenuLevel] (Maybe [MenuItem]) deriving (Show)

-- unideal
emptyMenu :: Menu
emptyMenu = Menu [] Nothing


findAndSetMenuFocus :: FilePath -> MenuLevel -> Maybe MenuLevel
findAndSetMenuFocus filePath menuLevel =
  if dropTrailingPathSeparator (fst (focus menuLevel)) == filePath then
      Just menuLevel
  else
      findAndSetMenuFocus filePath =<< next menuLevel


fromListToMenuLevel :: [MenuItem] -> Maybe MenuLevel
fromListToMenuLevel [] = Nothing
fromListToMenuLevel (x:xs) = Just (MenuLevel [] x xs)


focus :: MenuLevel -> MenuItem
focus (MenuLevel ls x rs) = x


right :: MenuLevel -> [MenuItem]
right (MenuLevel ls x rs) = rs


left :: MenuLevel -> [MenuItem]
left (MenuLevel ls x rs) = reverse ls


next :: MenuLevel -> Maybe MenuLevel
next (MenuLevel ls x []) = Nothing
next (MenuLevel ls x (r:rs)) = Just (MenuLevel (x:ls) r rs)


-- overvej et zipper library...
-- overvej at lave menuen før transpose.....

empty :: FilePath
empty = return pathSeparator


fromLists :: FilePath -> [[MenuItem]] -> Menu
fromLists = fromLists' emptyMenu empty . splitDirectories . dropIndex
  where
    fromLists' (Menu xs Nothing) acc [] (y:_) = Menu xs (Just (map (first (\x -> toUrl (acc </> x))) y))
    fromLists' menu acc (x:xs) (y:ys) = add (fromLists' menu (acc </> x) xs ys) (zipperMap (first (\x -> (acc </> x)))  (fromJust (findAndSetMenuFocus x =<< fromListToMenuLevel y)))
    fromLists' menu acc [] _ = menu
    fromLists' menu _ _ _ = error "Error making menu"


add :: Menu -> MenuLevel -> Menu
add (Menu xs x) y = Menu (y:xs) x

-------------------------------------------------------------------------------
zipperMap f (MenuLevel ls x rs) = MenuLevel (map f ls) (f x) (map f rs)


-------------------------------------------------------------------------------
-- fix toListFromMenu
showMenu :: Menu -> String
showMenu m = renderHtml $ do
  zipWithM_ showMenuLevel [0..] menulevels
  H.nav $ H.ul (mapM_ (showLastMenu len) (lastMenu m))
    where
      menulevels = toListFromMenu m
      len = length menulevels


--since zipWithM_ is for any monad we dont need these..
toListFromMenu :: Menu -> [MenuLevel]
toListFromMenu (Menu xs _) = xs


lastMenu :: Menu -> [MenuItem]
lastMenu (Menu _ (Just x)) = x
lastMenu (Menu _ _) = []


--since zipWithM_ is for any monad we dont need these..
showLastMenu :: Int -> MenuItem -> H.Html
showLastMenu d x = H.li (showMenuItem x)


--we dont use d at all....
showMenuLevel :: Int -> MenuLevel -> H.Html
showMenuLevel d x = H.nav $ H.ul (mapM_ H.li elems)
  where
    elems = map showMenuItem (left x) ++
            [(showMenuFocusItem (focus x))] ++
            (map showMenuItem (right x))


showMenuItem :: MenuItem -> H.Html
showMenuItem x = H.a (H.toHtml (snd x)) ! A.href (H.toValue (fst x))


showMenuFocusItem :: MenuItem -> H.Html
showMenuFocusItem x = showMenuItem x ! A.class_ "focus"
