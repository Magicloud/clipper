{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
-- {-# LANGUAGE UndecidableInstances    #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
module Copied where

-- import Control.Monad.IO.Class
import Data.GI.Base
import Data.GI.Base.GObject
import Data.GI.Base.Overloading
import Data.Text
-- import Data.Time.Clock
import Data.Word
import GI.GObject
import GI.Gdk
import GI.GdkPixbuf

newtype Copied = Copied (ManagedPtr Copied)

instance TypedObject Copied where
  glibType = registerGType Copied

instance GObject Copied

data CopiedContent = CCPixbuf Pixbuf
                   | CCRichText Atom (Maybe Text) Word64
                   | CCText (Maybe Text)
                   | CCUris [Text]

data CopiedPrivate = CopiedPrivate { cpContent :: CopiedContent
                                   , cpTags :: [(Text, Text)] }

instance HasParentTypes Copied

type instance ParentTypes Copied = '[Object]

-- class (GObject o, IsDescendantOf Copied o) => IsCopied o
-- instance (GObject o, IsDescendantOf Copied o) => IsCopied o

instance DerivedGObject Copied where
  type GObjectParentType Copied = Object
  type GObjectPrivateData Copied = Maybe CopiedPrivate
  objectTypeName = "Magicloud-Clipper-Copied"
  objectClassInit = copiedClassInit
  objectInstanceInit = copiedInstanceInit

copiedClassInit :: GObjectClass -> IO ()
copiedClassInit _klass = return ()

copiedInstanceInit :: GObjectClass -> Copied -> IO (Maybe CopiedPrivate)
copiedInstanceInit _klass _c = return Nothing

-- toCopied :: (MonadIO m, IsCopied o) => o -> m Copied
-- toCopied = liftIO . unsafeCastTo Copied
