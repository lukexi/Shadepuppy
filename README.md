VR Edition of Shadepup

`cabal run shadepuppy-vr`

Edit `mainvr.hs` to choose which shader to run.

Until I push them to hackage, you'll need to clone & install:

https://github.com/lukexi/oculus-mini

https://github.com/lukexi/gl-pal

https://github.com/lukexi/glfw-pal

I'll implement a shader munger soon, but 'til then 
Shadertoy shaders will need to be manually bracketed with
something like:
```
#version 330

in vec2 fragCoord;
uniform float iGlobalTime;
uniform vec2  iResolution;
uniform vec3  iEyePosition;
uniform mat3  iEyeOrientation;
out vec4 fragColor;
```
and
```
void main() {
    vec2 p = -1.0 + 2.0 * fragCoord.xy / iResolution.xy;

    vec3 fragRayDir = iEyeOrientation * normalize( vec3(p.xy,-1.5));
    
    vec3 fragRayOri = iEyePosition;

    mainVR(fragColor, fragCoord, fragRayOri, fragRayDir);
}
```
See `MengerSponge.frag` for an example.

