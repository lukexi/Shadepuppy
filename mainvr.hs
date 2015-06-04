{-# LANGUAGE RecordWildCards #-}
import Prelude hiding (init) 
import Control.Monad
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Data.Time
import Quad
import Foreign.Ptr
import Foreign
import Graphics.Oculus
import Linear
import Control.Lens
import qualified Data.Text.IO as Text
import Data.Monoid
import Texture
-- import Data.Maybe

shaderName :: String
-- shaderName = "RaymarchingPrimitives"
-- shaderName = "shadepuppy"
-- shaderName = "MengerSponge"
shaderName = "Sierpinski"

assembleShader :: IO GLProgram
assembleShader = do
    let fragFile = (shaderName ++ ".frag")
    fragSource <- Text.readFile fragFile

    -- Add the needed uniforms and main function from the header and footer
    fragHeader <- Text.readFile "vrHeader.frag"
    fragFooter <- Text.readFile "vrFooter.frag"
    let fullFragSource = fragHeader <> fragSource <> fragFooter

    -- We use a standard vert shader to position the full screen quad
    vertSource <- Text.readFile "shadepuppy.vert"
    createShaderProgramFromSources "shadepuppy.vert" vertSource fragFile fullFragSource

-- Initialization to set up window
main :: IO ()
main = do
  -- Initialize GLFW
  hmd <- createHMD
  (resX, resY) <- getHMDResolution hmd
  
  (window, events) <- createWindow "ShadePuppy" resX resY
  -- Compensate for retina framebuffers on Mac
  (frameW, frameH) <- getFramebufferSize window
  when (frameW > resX && frameH > resY) $
      setWindowSize window (resX `div` 2) (resY `div` 2)

  renderHMD <- configureHMDRendering hmd "ShadePuppy"
  dismissHSWDisplay hmd
  recenterPose hmd

  glClearColor 0.0 0.1 0.1 0
  glEnable GL_DEPTH_TEST
  shaderProg      <- assembleShader
  iGlobalTime     <- getShaderUniform shaderProg "iGlobalTime"
  iResolution     <- getShaderUniform shaderProg "iResolution"
  unCorners       <- getShaderUniform shaderProg "unCorners"
  unViewport      <- getShaderUniform shaderProg "unViewport"
  
  quad            <- makeQuad shaderProg
  glBindVertexArray (unVertexArrayObject (meshVAO quad))
  glUseProgram (unGLProgram shaderProg)
  -- Begin rendering
  whileWindow window $ do
    processEvents events (closeOnEscape window)    
    renderHMDFrame renderHMD $ \eyePoses -> do

      glClear ( GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT )

      -- Send along the current framenumber as a uniform
      glUniform1f (unUniformLocation iGlobalTime) =<< realToFrac . utctDayTime <$> getCurrentTime
      glUniform2f (unUniformLocation iResolution) (fromIntegral resX/2) (fromIntegral resY)
      

      forM_ (renEyes renderHMD) $ \eye -> do
        let (x,y,w,h) = eyeViewport eye
        glViewport x y w h

        let FOVPort{..} = eyeFOV eye
            corA0 = point $ V3 (-fovpLeftTan)  (-fovpDownTan) (-1)
            corB0 = point $ V3 ( fovpRightTan) (-fovpDownTan) (-1)
            corC0 = point $ V3 ( fovpRightTan) ( fovpUpTan)   (-1)
            corD0 = point $ V3 (-fovpLeftTan)  ( fovpUpTan)   (-1)
            apex0 = point $ V3 0 0 0

        (eyeOrientation, eyePosition) <- getPoses_OrientationAndPositionForEye eyePoses (eyeIndex eye)

        -- now <- realToFrac . utctDayTime <$> getCurrentTime
        -- let eyeOrientation = axisAngle (V3 0 1 0) ((sin nowww + )1 / 2)
        --     eyePosition = V3 0 (-4) (3)

        let eyeCameraMat = mkTransformation eyeOrientation eyePosition
            eyeInvMat    = eyeCameraMat
            corA         = eyeInvMat !* corA0
            corB         = eyeInvMat !* corB0
            corC         = eyeInvMat !* corC0
            corD         = eyeInvMat !* corD0
            apex         = eyeInvMat !* apex0

        let corners = [ corA ^. _x, corA ^. _y, corA ^. _z, 
                        corB ^. _x, corB ^. _y, corB ^. _z, 
                        corC ^. _x, corC ^. _y, corC ^. _z, 
                        corD ^. _x, corD ^. _y, corD ^. _z,
                        apex ^. _x, apex ^. _y, apex ^. _z ]

        withArray corners (glUniform3fv (unUniformLocation unCorners) 5)

        -- Pin x to 0 to fix zoomed-in right eye
        let viewport = map fromIntegral [x,y,w,h]
        withArray viewport (glUniform4fv (unUniformLocation unViewport) 1)

        glDrawElements GL_TRIANGLES (meshIndexCount quad) GL_UNSIGNED_INT nullPtr
        
