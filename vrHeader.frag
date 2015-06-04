#version 330

uniform float iGlobalTime;
uniform vec2  iResolution;
uniform vec3  iMouse;

uniform sampler2D iChannel0;
uniform vec3      iChannelResolution[4];
uniform float     iChannelTime[4];
uniform vec4      iDate;
uniform float     iSampleRate;

// VR Uniforms
uniform vec4  unViewport;
uniform vec3  unCorners[5];

in vec2 fragCoord;

out vec4 fragColor;

// Add this back in since it's been taken out...
// Maybe we should use a different #version instead?
vec4 texture2D(sampler2D s, vec2 coord) {
    return texture(s, coord);
}
