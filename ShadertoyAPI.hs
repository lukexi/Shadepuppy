{-# LANGUAGE OverloadedStrings #-}
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens 
import Data.Aeson 
import Data.Monoid
import Data.Text

shadertoyKey = "NdrKWr"

shadertoyRoot = "https://www.shadertoy.com"

search string = shadertoyRoot 
    ++ "/api/v1/shaders/query/" ++ string 
    ++ "?key=" ++ shadertoyKey

shaderWithID shaderID = shadertoyRoot 
    <> "/api/v1/shaders/" <> shaderID
    <> "?key=" <> shadertoyKey

getVRShaders = do
    r <- get (search "vr")
    return (r ^.. responseBody . key "Results" . values . _String)

main = do
    vrShaders <- getVRShaders
    r <- get (shaderWithID (unpack (vrShaders !! 4)))
    print $ r ^? responseBody
    
    