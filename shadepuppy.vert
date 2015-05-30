#version 330

in vec2 aPosition;
uniform float iGlobalTime;
uniform vec2  iResolution;
out vec2 fragCoord;

void main() {
    fragCoord = ((aPosition + 1) / 2) * iResolution.xy;
    gl_Position = vec4(aPosition, 0.0, 1.0);
}

