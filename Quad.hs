module Quad where

import Graphics.GL.Pal

import Graphics.GL
import Foreign

----------------------------------------------------------
-- Make Quad
----------------------------------------------------------


-- makeQuad :: GLProgram -> IO Mesh
makeQuad program (x1,y1) (x2, y2) = do

    aPosition <- getShaderAttribute program "aPosition"
    aUV       <- getShaderAttribute program "aUV"

    -- Setup a VAO
    vaoQuad <- overPtr (glGenVertexArrays 1)

    glBindVertexArray vaoQuad


    -----------------
    -- Quad Positions
    -----------------
    
    -- Buffer the quad vertices
    let quadVertices = 
            [ x1 , y1  
            , x2 , y1
            , x2 , y2
            , x1 , y2 ] :: [GLfloat]

    vaoQuadVertices <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vaoQuadVertices

    let quadVerticesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length quadVertices)

    withArray quadVertices $ 
        \quadVerticesPtr ->
            glBufferData GL_ARRAY_BUFFER quadVerticesSize (castPtr quadVerticesPtr) GL_STATIC_DRAW 

    -- Describe our vertices array to OpenGL
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation aPosition))

    glVertexAttribPointer
        (fromIntegral (unAttributeLocation aPosition)) -- attribute
        2                 -- number of elements per vertex, here (x,y)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element

    ---------------
    -- Quad UVs
    ---------------
    -- Buffer the quad vertices
    let quadUVs = 
            [ 0 , 0  
            , 1 , 0
            , 1 , 1
            , 0 , 1 ] :: [GLfloat]

    vaoQuadUVs <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vaoQuadUVs

    let quadUVsSize = fromIntegral (sizeOf (undefined :: GLfloat) * length quadUVs)

    withArray quadUVs $ 
        \quadUVsPtr ->
            glBufferData GL_ARRAY_BUFFER quadUVsSize (castPtr quadUVsPtr) GL_STATIC_DRAW 

    -- Describe our vertices array to OpenGL
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation aUV))

    glVertexAttribPointer
        (fromIntegral (unAttributeLocation aUV)) -- attribute
        2                 -- number of elements per vertex, here (u,v)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element

    ----------------
    -- Quad Indicies
    ----------------

    -- Buffer the quad indices
    let quadIndices = 
            [ 0, 1, 2
            , 2, 3, 0 ] :: [GLuint]
    
    iboQuadElements <- overPtr (glGenBuffers 1)
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER iboQuadElements

    let quadElementsSize = fromIntegral (sizeOf (undefined :: GLuint) * length quadIndices)
    
    withArray quadIndices $ 
        \quadIndicesPtr ->
            glBufferData GL_ELEMENT_ARRAY_BUFFER quadElementsSize (castPtr quadIndicesPtr) GL_STATIC_DRAW
    
    glBindVertexArray 0

    return $ Mesh 
        { meshVAO        = VertexArrayObject vaoQuad
        , meshIndexCount = fromIntegral (length quadIndices)
        } 
