module Menu
where

import Hakyll
import Data.Maybe (fromMaybe)


addToMenu :: Rules ()
addToMenu = version "routes" $ compile $ makeItem =<< maybeToRoute Nothing


maybeToRoute :: Maybe String -> Compiler String
maybeToRoute v = fmap (fromMaybe "") (routeForUnderlying v)


routeForUnderlying :: Maybe String -> Compiler (Maybe FilePath)
routeForUnderlying v = getRoute =<< (setUnderlyingVersion v)


setUnderlyingVersion :: Maybe String -> Compiler Identifier
setUnderlyingVersion v = fmap (setVersion v) getUnderlying
