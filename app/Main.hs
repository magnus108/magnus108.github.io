{-# LANGUAGE OverloadedStrings #-}
module Main where
--------------------------------------------------------------------------------
import           Data.Monoid ((<>))
import           Hakyll
import           System.FilePath
import           Data.Binary
import           Data.Typeable

import Menu
  ( Menu
  , addToMenu
  , showMenu
  , maybeToRoute
  , buildMenu
  )

--------------------------------------------------------------------------------
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      normalise (takeDirectory p </> takeBaseName p </> "index.html")
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
        addToMenu
        route $ cleanRoute
        compile $ do
            ctx <- menuCtx

            pandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" ctx
              >>= relativizeUrls
              >>= cleanIndexHtmls


    match "posts/**.markdown" $ do
        addToMenu
        route $ cleanRoute
        compile $ do
            ctx <- menuCtx

            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexHtmls

    match "travels/**.markdown" $ do
        addToMenu
        route $ cleanRoute
        compile $ do
            ctx <- menuCtx

            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexHtmls

    match "index.html" $ do
        addToMenu
        route idRoute
        compile $ do

            ctx <- menuCtx

            posts <- recentFirst =<< loadAllNoVersion ("posts/**.markdown")

            let indexCtx = listField "posts" postCtx (return posts) <> ctx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexHtmls


--    match "404.markdown" $ do
--        route $ cleanRoute
--        compile $ do
--            ctx <- menuCtx

 --           pandocCompiler
  --            >>= loadAndApplyTemplate "templates/default.html" ctx
   --           >>= relativizeUrls
    --          >>= cleanIndexHtmls

    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    --should load categories
    match "posts/**.html" $ do
        route idRoute
        compile $ do
            ctx <- menuCtx

            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexHtmls

    --should load categories
    match "travels/**.html" $ do
        route idRoute
        compile $ do
            ctx <- menuCtx

            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexHtmls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

menuCtx :: Compiler (Context String)
menuCtx = do
  menu <- getMenu
  return (menu <> defaultContext)

-------------------------------------------------------------------------------
loadAllRoutes :: Compiler [FilePath]
loadAllRoutes =
  moveFilePathToFront "index.html"
      =<< moveFilePathToFront "cv/index.html"
      =<< loadAllBody (hasVersion "routes")


loadAllNoVersion :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
loadAllNoVersion p = loadAll (p .&&. hasNoVersion)


loadAllBody :: Pattern -> Compiler [String]
loadAllBody p = do
    items <- loadAll p
    return (fmap itemBody items)


getMenu :: Compiler (Context String)
getMenu = do
    routes <- loadAllRoutes
    currentRoute <- maybeToRoute Nothing
    return $ constField "menu" $ showMenu $ buildMenu currentRoute routes

-------------------------------------------------------------------------------
moveFilePathToFront :: MonadMetadata m => FilePath -> [String] -> m [String]
moveFilePathToFront s itemList =
    return (moveToFront s itemList)
        where
          moveToFront x xs =
            case break (\y -> y == x) xs of
              (a, y:ys) -> y:a ++ ys
              (a, ys) -> a ++ ys
