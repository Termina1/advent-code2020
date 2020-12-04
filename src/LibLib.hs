{-# LANGUAGE FlexibleContexts #-}

module LibLib
  ( splitOnSeq
  ) where

import qualified Data.Sequences as Seq
import Data.MonoTraversable
import Data.List
import Data.Conduit
import Control.Monad

splitOnSeq :: (Monad m, Seq.IsSequence seq, Eq (Element seq)) => seq -> ConduitT seq seq m ()
splitOnSeq f =
    start
  where
    start = await >>= maybe (return ()) (loop id)

    loop bldr t =
        if onull y
            then do
                mt <- await
                case mt of
                    Nothing -> let finalChunk = mconcat $ bldr [t]
                               in  unless (onull finalChunk) $ yield finalChunk
                    Just t' -> loop (bldr . (t:)) t'
            else yield (mconcat $ bldr [x]) >> loop id y
      where
        (x, y) = let (x : z) = Seq.splitSeq f t in
          (x, mconcat $ intersperse f z)