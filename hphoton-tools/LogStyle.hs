{-# LANGUAGE OverloadedStrings #-}
module LogStyle where

import Clay
import Prelude hiding (div)

styleSheet :: Css
styleSheet = do
    body ? do
        backgroundColor "#eee"
    section ? do
        borderRadius (em 0.5)
        paddingLeft (em 2)
        paddingRight (em 2)
        marginLeft (em 1)
        marginRight (em 1)
    div # ".container" ? do
        marginLeft (px 100)
