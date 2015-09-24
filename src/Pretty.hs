{- |
Module      : Pretty
Description : A class to do pretty printing.
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Pretty where

class Pretty a where
    pretty :: a -> String
