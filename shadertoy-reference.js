// Eye rendering
vrData = GetData();
for( var i=0; i<2; i++ )
{
    var ei = (i==0) ? vrData.mLeftEye : vrData.mRightEye;

    var vp = [ i*xres/2, 0, xres/2, yres ];
    //vp = ei.mVP;
    gl.viewport( vp[0], vp[1], vp[2], vp[3] );

    var fov = ei.mProjection;
    var corA = [ -fov[2], -fov[1], -1.0 ];
    var corB = [  fov[3], -fov[1], -1.0 ];
    var corC = [  fov[3],  fov[0], -1.0 ];
    var corD = [ -fov[2],  fov[0], -1.0 ];
    var apex = [ 0.0, 0.0, 0.0 ];

    var ma = invertFast( ei.mCamera );
    corA = matMulpoint( ma, corA ); 
    corB = matMulpoint( ma, corB ); 
    corC = matMulpoint( ma, corC ); 
    corD = matMulpoint( ma, corD ); 
    apex = matMulpoint( ma, apex ); 

    var corners = [ corA[0], corA[1], corA[2], 
                    corB[0], corB[1], corB[2], 
                    corC[0], corC[1], corC[2], 
                    corD[0], corD[1], corD[2],
                    apex[0], apex[1], apex[2] ];
    gl.uniform3fv( gl.getUniformLocation( prog, "unCorners" ), corners );
    gl.uniform4fv( gl.getUniformLocation( prog, "unViewport" ), vp );

    gl.drawArrays(gl.TRIANGLES, 0, 6);
}

-- | Creates a translation matrix from a V3
setTranslation :: V3 -> M44

-- | Creates a rotation matrix from a Quaternion
setFromQuaternion :: Quaternion -> M44

WebVR.prototype.GetData = function( id )
{
    var ss   = this.mHMDSensor.getState();
    var fovL = this.mHMD.getEyeParameters( "left" );
    var fovR = this.mHMD.getEyeParameters( "right" );

    // camera info
    var cPos = vec3(-ss.position.x, -ss.position.y, -ss.position.z);
    var cRot = setFromQuaternion( vec4(ss.orientation.x, ss.orientation.y, ss.orientation.z, ss.orientation.w) );
    var cTra = setTranslation( cPos );
    var cMat = matMul( invertFast(cRot), cTra);

    // per eye info
    //var lTra = setTranslation( add(cPos, vec3(-fovL.eyeTranslation.x, -fovL.eyeTranslation.y, -fovL.eyeTranslation.z)) );
    var lTra = setTranslation( vec3(-fovL.eyeTranslation.x, -fovL.eyeTranslation.y, -fovL.eyeTranslation.z) );
    var lMat = matMul( lTra, cMat );
    var lPrj = [ Math.tan( fovL.recommendedFieldOfView.upDegrees * Math.PI/180.0),
                 Math.tan( fovL.recommendedFieldOfView.downDegrees * Math.PI/180.0),
                 Math.tan( fovL.recommendedFieldOfView.leftDegrees * Math.PI/180.0),
                 Math.tan( fovL.recommendedFieldOfView.rightDegrees * Math.PI/180.0) ];

    //var rTra = setTranslation( add(cPos, vec3(-fovR.eyeTranslation.x, -fovR.eyeTranslation.y, -fovR.eyeTranslation.z)) );
    var rTra = setTranslation( vec3(-fovR.eyeTranslation.x, -fovR.eyeTranslation.y, -fovR.eyeTranslation.z) );
    var rMat = matMul( rTra, cMat );
    var rPrj = [ Math.tan( fovR.recommendedFieldOfView.upDegrees * Math.PI/180.0),
                 Math.tan( fovR.recommendedFieldOfView.downDegrees * Math.PI/180.0),
                 Math.tan( fovR.recommendedFieldOfView.leftDegrees * Math.PI/180.0),
                 Math.tan( fovR.recommendedFieldOfView.rightDegrees * Math.PI/180.0) ];

    return { mCamera   : { mCamera:cMat },
             mLeftEye  : { mVP:[fovL.renderRect.x,fovL.renderRect.y,fovL.renderRect.width,fovL.renderRect.height], mProjection:lPrj, mCamera:lMat },
             mRightEye : { mVP:[fovR.renderRect.x,fovR.renderRect.y,fovR.renderRect.width,fovR.renderRect.height], mProjection:rPrj, mCamera:rMat } };
}
