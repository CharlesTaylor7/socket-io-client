module Component.FileDownload where

import Reflex
import Js.Types


fileDownload :: Widget t m => Dynamic t Url -> m ()
fileDownload srcDyn = elDynAttr
    "iframe"
    (srcDyn <&> \(Url src) -> baseAttrs & at "src" ?~ src)
    blank
    where
      baseAttrs = "style" =: "display:none"
