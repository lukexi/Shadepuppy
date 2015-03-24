#version 330

in vec2 position;
uniform float frameNumber;
out vec2 fragXY;

void main() {
    fragXY = (position + 1) / 2;
    gl_Position = vec4(position, 0.0, 1.0);
}
