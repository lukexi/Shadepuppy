#version 330

in vec2 fragXY;
uniform float frameNumber;
out vec4 fragColor;

void main() {
    fragColor = vec4(fragXY.x, fragXY.y, sin(frameNumber/100), 1.0);
}
