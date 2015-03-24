import Prelude hiding (init) 
import Control.Applicative

import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Foreign.Storable (sizeOf)

data SPShader = SPShader { spsProgram      :: Program
                         , spsPositionA    :: AttribLocation
                         , spsFrameNumberU :: UniformLocation
                         }

data Resources = Resources { rscVertexBuffer  :: BufferObject
                           , rscElementBuffer :: BufferObject
                           , rscShader        :: SPShader
                           }

-- Vertices for a fullscreen quad
vertexBufferData :: [ GLfloat ]
vertexBufferData =  [ -1 , -1 
                    , 1  , -1 
                    , -1 , 1 
                    , 1  , 1 ]

-- Indices to render a fullscreen quad as a TriangleStrip
elementBufferData :: [ GLuint ]
elementBufferData =  [ 0..3   ]

initShader :: IO SPShader
initShader = do 
  vs <- loadShader VertexShader   "spacepuppy.vert"
  fs <- loadShader FragmentShader "spacepuppy.frag"
  p  <- linkShaderProgram [vs, fs]
  SPShader p
    <$> get (attribLocation p "position")
    <*> get (uniformLocation p "frameNumber")

applyWindowHints :: IO ()
applyWindowHints = do
  windowHint $ WindowHint'ClientAPI ClientAPI'OpenGL
  windowHint $ WindowHint'OpenGLForwardCompat True
  windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
  windowHint $ WindowHint'ContextVersionMajor 3
  windowHint $ WindowHint'ContextVersionMinor 2

makeResources :: IO Resources
makeResources =  Resources
             <$> makeBuffer ArrayBuffer vertexBufferData
             <*> makeBuffer ElementArrayBuffer elementBufferData
             <*> initShader

-- Initialization to set up window --
main :: IO ()
main = do
  _ <- init
  applyWindowHints
  
  maybeWindow <- createWindow 640 480 "SpacePuppy" Nothing Nothing
  
  case maybeWindow of
    Nothing     -> print "Couldn't create a window :*(" 
    Just window -> do
      -- Create a VAO, as required by OpenGL Core Context
      vao <- makeVAO (return ())
      bindVertexArrayObject $= Just vao

      resources <- makeResources
      swapInterval 1 -- Enable VSync
      makeContextCurrent ( Just window )
      mainLoop window resources 0

-- Actually Starting once window is up --
mainLoop :: Window -> Resources -> GLclampf -> IO a
mainLoop window resources frameNumber = do
  -- Clear the frame
  clearColor $= Color4 0.1 0.2 0.1 0 
  clear [ ColorBuffer ]
  
  -- Make sure our viewport matches the latest window size
  (width, height) <- getFramebufferSize window
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  
  -- Bind our shader
  currentProgram $= Just (spsProgram (rscShader resources))
  -- Send along the current framenumber as a uniform
  uniform (spsFrameNumberU (rscShader resources)) $= Index1 frameNumber
  
  -- Bind our fullscreen quad geometry
  bindGeometry resources
  -- Draw the fullscreens quad
  drawElements TriangleStrip (fromIntegral (length elementBufferData)) UnsignedInt offset0

  -- Swap buffers, poll events, and start rendering the next frame
  swapBuffers window
  pollEvents
  mainLoop window resources ( frameNumber + 1 )
 

bindGeometry :: Resources -> IO ()
bindGeometry resources = do
  let numComponents = 2 -- We only use X & Y since this is a fullscreen quad
      stride = fromIntegral $ sizeOf (undefined::GLfloat) * fromIntegral numComponents
      vad = VertexArrayDescriptor numComponents Float stride offset0
      positionA = spsPositionA (rscShader resources)
  -- Bind our vertices
  bindBuffer ArrayBuffer        $= Just (rscVertexBuffer resources)
  vertexAttribArray positionA   $= Enabled
  vertexAttribPointer positionA $= (ToFloat, vad)
  -- Bind the indices to render the quad as a triangle strip
  bindBuffer ElementArrayBuffer $= Just (rscElementBuffer resources)
