{-# LANGUAGE RecordWildCards #-}

module Shader where
import Graphics.GL.Pal
import qualified Data.Text.IO as Text
import Data.Monoid

data ShadepuppyShader = ShadepuppyShader 
    { shadepuppyProgram :: GLProgram
    , iGlobalTime :: UniformLocation
    , iResolution :: UniformLocation
    , iMouse      :: UniformLocation
    , iChannel0   :: UniformLocation
    , unCorners   :: UniformLocation
    , unViewport  :: UniformLocation
    }

buildShader :: FilePath -> FilePath -> IO GLProgram
buildShader footerFile shaderName = do
    let fragFile = (shaderName ++ ".frag")
    fragSource <- Text.readFile fragFile

    -- Add the needed uniforms and main function from the header and footer
    fragHeader <- Text.readFile "vrHeader.frag"
    fragFooter <- Text.readFile footerFile
    let fullFragSource = fragHeader <> fragSource <> fragFooter

    -- We use a standard vert shader to position the full screen quad
    vertSource <- Text.readFile "shadepuppy.vert"
    createShaderProgramFromSources "shadepuppy.vert" vertSource fragFile fullFragSource

assembleShaderWithFooter :: FilePath -> FilePath -> IO ShadepuppyShader
assembleShaderWithFooter footerFile shaderName = do
    shadepuppyProgram <- buildShader footerFile shaderName

    iGlobalTime     <- getShaderUniform shadepuppyProgram "iGlobalTime"
    iResolution     <- getShaderUniform shadepuppyProgram "iResolution"
    unCorners       <- getShaderUniform shadepuppyProgram "unCorners"
    unViewport      <- getShaderUniform shadepuppyProgram "unViewport"
    iChannel0       <- getShaderUniform shadepuppyProgram "iChannel0"
    iMouse          <- getShaderUniform shadepuppyProgram "iMouse"

    return ShadepuppyShader{..}