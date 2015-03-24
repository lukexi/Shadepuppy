#version 330

in vec2 fragXY;
uniform float frameNumber;
out vec4 fragColor;

void main() {
    fragColor = vec4(fragXY.x, fragXY.y, 0.5+0.5*sin(frameNumber/100.0), 1.0);
}
