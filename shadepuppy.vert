#version 330

in vec2 aPosition;
in vec2 aUV;
uniform vec2  iResolution;
out vec2 fragCoord;

void main() {
    fragCoord = aUV * iResolution.xy;
    gl_Position = vec4(aPosition, -0.5, 1.0);
}