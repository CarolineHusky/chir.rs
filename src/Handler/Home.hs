module Handler.Home (getHomeR) where

import Components.Image (image)
import Config (widgetFile)
import Config.Images (img_2023_06_02_vintagecoyote_prideicon)
import Foundation (App, DummyMessage (..), Route (HomeR), translationUnescaped)
import Yesod (HandlerFor, Html, Yesod (defaultLayout))

getHomeR :: HandlerFor App Html
getHomeR = do
  defaultLayout $ do
    figcaption <- translationUnescaped MsgImgVintagecoyotePrideiconFigcaption
    selfIntro1 <- translationUnescaped MsgSelfIntroPart1
    selfIntro2 <- translationUnescaped MsgSelfIntroPart2
    selfIntro3 <- translationUnescaped MsgSelfIntroPart3
    selfIntro4 <- translationUnescaped MsgSelfIntroPart4
    $(widgetFile "homepage")
