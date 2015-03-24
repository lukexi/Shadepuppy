#version 330

in vec2 fragCoord;
uniform float iGlobalTime;
out vec4 fragColor;

void main() {
    fragColor = vec4(fragCoord, 0.5+0.5*sin(iGlobalTime), 1.0);
}