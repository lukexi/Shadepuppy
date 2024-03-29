#version 330

in vec2 fragCoord;
uniform float iGlobalTime;
uniform vec3  iResolution;
out vec4 fragColor;

void main() {
    vec2 uv = fragCoord.xy / iResolution.xy;
    fragColor = vec4(uv, 0.5+0.5*sin(iGlobalTime), 1.0);
}
