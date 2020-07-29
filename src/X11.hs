{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module X11 where

import Data.Text
import GI.Gdk as Gdk
import GI.GdkX11
import GI.Wnck as Wnck

getWindowName :: Gdk.Window -> IO Text
getWindowName w = castTo X11Window w >>= \case
  Nothing -> return ""
  Just w' -> do
    (ww :: Wnck.Window) <- #getXid w' >>= windowGet
    #hasName ww >>= \case
      True -> #getName ww
      False -> return ""
