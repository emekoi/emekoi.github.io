module Site.Config
    ( module Site.Config
    ) where

author, email, github :: String
author = "Emeka Nkurumeh"
email = "e.nk@caltech.edu"
github = "https://github.com/emekoi"

siteLang, siteTitle, siteSource, siteURL :: String
siteLang = "en"
siteTitle = author ++ "'s Blog"
siteSource = "https://github.com/emekoi/emekoi.github.io"
siteURL = "https://emekoi.github.io"

defaultCSLFile :: String
defaultCSLFile = "bib/ieee.csl"
