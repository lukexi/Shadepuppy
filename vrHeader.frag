#version 330

uniform float iGlobalTime;
uniform vec2  iResolution;
uniform vec3  iMouse;

// VR Uniforms
uniform vec4  unViewport;
uniform vec3  unCorners[5];

in vec2 fragCoord;

out vec4 fragColor;
