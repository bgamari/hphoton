{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module HtmlLog ( LogPosn, HtmlLogT, tellLog, runHtmlLogT, writeHtmlLogT) where

import           Control.Monad.Writer
import           Data.Function
import           Data.List

import qualified Clay
import qualified Data.ByteString.Lazy as BS
import           LogStyle
import           Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Blaze.Renderer.Utf8

type LogPosn = Int

newtype HtmlLogT m a = HtmlLogT (WriterT [(LogPosn, Html)] m a)
                     deriving (Monad, MonadTrans, MonadIO)

tellLog :: Monad m => Int -> Html -> HtmlLogT m ()
tellLog pos content = HtmlLogT $ tell [(pos, content)]

runHtmlLogT :: Monad m => HtmlLogT m a -> m (a, BS.ByteString)
runHtmlLogT (HtmlLogT action) = do
    (a, logEntries) <- runWriterT action
    let log = renderMarkup $ do
                  docTypeHtml $ do
                      H.head $ do
                          style $ preEscapedToHtml $ Clay.render styleSheet
                          title "Log"
                      body $ H.div H.! HA.class_ "container" $ do
                          mapM_ snd $ sortBy (compare `on` fst) logEntries
    return (a, log)

writeHtmlLogT :: MonadIO m => FilePath -> HtmlLogT m a -> m a
writeHtmlLogT fname action = do
    (a, log) <- runHtmlLogT action
    liftIO $ BS.writeFile fname log
    return a
