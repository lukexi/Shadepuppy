
void main( void )
{
    vec4 color = vec4(0.0,0.0,0.0,1.0);

    vec3 ro = unCorners[4];
    vec2 uv = (gl_FragCoord.xy - unViewport.xy) / unViewport.zw;
    vec3 rd = normalize( mix( mix( unCorners[0], unCorners[1], uv.x ),
                              mix( unCorners[3], unCorners[2], uv.x ), uv.y ) - ro);

    mainVR( color, gl_FragCoord.xy - unViewport.xy, ro, rd );
    color.w = 1.0;
    fragColor = color;
}
