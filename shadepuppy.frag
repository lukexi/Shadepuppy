#version 330

in vec2 fragCoord;
uniform float iGlobalTime;
uniform vec2  iResolution;
uniform vec3  iEyePosition;
uniform mat3  iEyeOrientation;
out vec4 fragColor;


void main() {
    vec2 p = -1.0 + 2.0 * fragCoord.xy / iResolution.xy;

    vec3 fragRayDir = iEyeOrientation * normalize( vec3(p.xy,-1.5));
    
    vec3 fragRayOri = iEyePosition;


    fragColor = vec4(fragRayOri + fragRayDir * (0.5+0.5*sin(iGlobalTime)), 1.0);
    // fragColor = vec4(1,1,0,1);
}
