#version 330

in vec2 fragCoord;
uniform float iGlobalTime;
out vec4 fragColor;

// https://www.shadertoy.com/view/XsXXDn
// minus use of iResolution which we don't have yet

// http://www.pouet.net/prod.php?which=57245

#define t iGlobalTime

void main() {
    vec3 c;
    float l,z=t;
    for(int i=0;i<3;i++) {
        vec2 uv,p=fragCoord;
        uv=p;
        p-=.5;
        z+=.07;
        l=length(p);
        uv+=p/l*(sin(z)+1.)*abs(sin(l*9.-z*2.));
        c[i]=.01/length(abs(mod(uv,1.)-.5));
    }
    fragColor=vec4(c/l,t);
}