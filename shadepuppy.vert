#version 330

in vec2 vertexPosition;
uniform float iGlobalTime;
out vec2 fragCoord;

void main() {
    fragCoord = (vertexPosition + 1) / 2;
    gl_Position = vec4(vertexPosition, 0.0, 1.0);
}

