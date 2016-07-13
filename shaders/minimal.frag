
void mainVR( out vec4 fragColor, in vec2 fragCoord, in vec3 fragRayOri, in vec3 fragRayDir )
{
    // vec3 col = vec3(0.0,1.0,0.0);
    vec3 col = fragRayDir;

    fragColor = vec4( col, 1.0 );
}
