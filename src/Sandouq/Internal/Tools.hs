module Sandouq.Internal.Tools where

import qualified Data.Map as Map


getval :: Ord k => Map.Map k a -> k -> a
getval m k = case Map.lookup k m of
               Just v    -> v
               otherwise -> error "element not in the map"