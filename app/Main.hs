{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where

import Control.Monad
-- import Control.Monad.Extra
import Copied
import Data.GI.Base.GObject
import Data.IORef
import Data.Maybe
import Data.Text as T (Text, pack, intercalate)
import Data.Time.Clock
import Data.Word
import GI.Gdk hiding (Window)
import GI.GdkPixbuf
import GI.Gio hiding (Application, applicationNew)
import GI.Gtk hiding (main, listStoreAppend, ListStore)
import X11

data State = State { listStore :: ListStore
                   , somethingMore :: () }

type TagName = Text
type TagValue = Text
type Tag = (TagName, TagValue)
type Tags = [Tag]

main :: IO ()
main = applicationNew (Just "cn.Magicloud.Clipper") [] >>= \case
  Nothing -> return () -- exit
  Just app -> do
    void $ app `on` #activate $ onActivate app
    #run app Nothing >>= \case
      0 -> return () -- exit
      _ec -> return () -- exit

onActivate :: Application -> IO ()
onActivate app = do
  model <- new ListStore []
  stateStore <- newIORef $ State model ()
  clipboards >>= mapM_ (\cb -> cb `on` #ownerChange $ onClipBoardOwnerChange cb stateStore)
  win <- new Window [ #defaultHeight := 480, #defaultWidth := 640 ]
  #addWindow app win
  scroll <- new ScrolledWindow [ #hscrollbarPolicy := PolicyTypeAlways, #vscrollbarPolicy := PolicyTypeAlways ]
  #add win scroll
  lb <- new ListBox []
  #bindModel lb (Just model) (Just lbRowCreator)
  #add scroll lb
  #iconify win
  #showAll win

onClipBoardOwnerChange :: Clipboard -> IORef State -> EventOwnerChange -> IO ()
onClipBoardOwnerChange cb ss e = do
  wn <- get e #owner >>= \case
    Just w -> getWindowName w
    Nothing -> return ""
  now <- pack . show <$> getCurrentTime
  let tags = [ ("time", now), ("source", wn) ]
  tb <- new TextBuffer []
  incaseM cb (flip #waitIsRichTextAvailable tb) $ \targets -> do
    tag <- targets2tag targets
    #requestRichText cb tb $ getRichText ss (tag : tags)
  incaseM cb #waitIsUrisAvailable $ \targets -> do
    tag <- targets2tag targets
    #requestUris cb $ getUris ss (tag : tags)
  incaseM cb #waitIsImageAvailable $ \targets -> do
    tag <- targets2tag targets
    #requestImage cb $ getImage ss (tag : tags)
  incaseM cb #waitIsTextAvailable $ \targets -> do
    tag <- targets2tag targets
    #requestText cb $ getText ss (tag : tags)
  where
    incaseM :: Clipboard -> (Clipboard -> IO Bool) -> ([Atom] -> IO ()) -> IO ()
    incaseM cb' isAvailable f = do
      isAvailable' <- isAvailable cb'
      when isAvailable' $ do
        (targetAvailable, targets) <- #waitForTargets cb'
        when targetAvailable $ f targets
    targets2tag :: [Atom] -> IO Tag
    targets2tag targets = do
      ts <- T.intercalate "," <$> mapM #name targets
      return ("targets", ts)

lbRowCreator :: (TypedObject o, ManagedPtrNewtype o) => o -> IO Widget
lbRowCreator o = do
  listRow <- new HBox [ #heightRequest := 100 ] -- fix as row height. setable
  tagsCol <- new VBox [ #widthRequest := 100]
  #packEnd listRow tagsCol False False 0
  castTo Copied o >>= (\case
    Nothing -> new Label [ #label := "Object is not a Copied" ] >>= #add listRow
    Just copied -> gobjectGetPrivateData copied >>= (\case
      Nothing -> new Label [ #label := "Uninitialized Copied" ] >>= #add listRow
      Just priv -> case cpContent priv of
        CCPixbuf pixbuf -> do
          w <- get pixbuf #width
          h <- get pixbuf #height
          let imageHeight = 100 -- fix as row height. setable
              imageWidth = w `div` h * imageHeight
          scaledPixbuf <- #scaleSimple pixbuf imageWidth imageHeight InterpTypeNearest
          image <- imageNewFromPixbuf scaledPixbuf
          time <- new Label [ #label := getTag "time" priv ]
          source <- new Label [ #label := getTag "source" priv ]
          #packStart listRow image True True 0
          #packStart tagsCol time False False 0
          #packEnd tagsCol source False False 0
        CCRichText _ mtext _ -> do -- what is this like?
          mainContent <- new Label [ #label := fromMaybe "" mtext, #wrap := True ]
          time <- new Label [ #label := getTag "time" priv ]
          source <- new Label [ #label := getTag "source" priv ]
          #packStart listRow mainContent True True 0
          #packStart tagsCol time False False 0
          #packEnd tagsCol source False False 0
        CCText mtext -> do -- `head`
          mainContent <- new Label [ #label := fromMaybe "" mtext, #wrap := True ]
          time <- new Label [ #label := getTag "time" priv ]
          source <- new Label [ #label := getTag "source" priv ]
          #packStart listRow mainContent True True 0
          #packStart tagsCol time False False 0
          #packEnd tagsCol source False False 0
        CCUris uris ->  do -- what is this like?
          mainContent <- new Label [ #label := T.intercalate "," uris, #wrap := True ]
          time <- new Label [ #label := getTag "time" priv ]
          source <- new Label [ #label := getTag "source" priv ]
          #packStart listRow mainContent True True 0
          #packStart tagsCol time False False 0
          #packEnd tagsCol source False False 0
      )
    )
  #showAll listRow
  toWidget listRow
  where
    getTag n cp = fromMaybe "" $ lookup n $ cpTags cp

clipboardNames :: [Text] -- from conf
clipboardNames = ["PRIMARY", "CLIPBOARD"]
clipboards :: IO [Clipboard]
clipboards = mapM (atomInternStaticString >=> clipboardGet) clipboardNames

getImage :: IORef State -> Tags -> Clipboard -> Pixbuf -> IO ()
getImage ss tags _cb pixbuf = do
  state <- readIORef ss
  copied <- new Copied []
  gobjectModifyPrivateData copied $ const $ Just $ CopiedPrivate (CCPixbuf pixbuf) tags
  #append (listStore state) copied

getRichText :: IORef State -> Tags -> Clipboard -> Atom -> Maybe Text -> Word64 -> IO ()
getRichText ss tags _cb format mtext len = do
  state <- readIORef ss
  copied <- new Copied []
  gobjectModifyPrivateData copied $ const $ Just $ CopiedPrivate (CCRichText format mtext len) tags
  #append (listStore state) copied

getText :: IORef State -> Tags -> Clipboard -> Maybe Text -> IO ()
getText ss tags _cb mtext = do
  state <- readIORef ss
  copied <- new Copied []
  gobjectModifyPrivateData copied $ const $ Just $ CopiedPrivate (CCText mtext) tags
  #append (listStore state) copied

getUris :: IORef State -> Tags -> Clipboard -> [Text] -> IO ()
getUris ss tags _cb uris = do
  state <- readIORef ss
  copied <- new Copied []
  gobjectModifyPrivateData copied $ const $ Just $ CopiedPrivate (CCUris uris) tags
  #append (listStore state) copied
