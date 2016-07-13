// various functions from:
// http://mercury.sexy/hg_sdf/
// https://www.shadertoy.com/view/Xds3zN

#define PI 3.14159265

float fScene(in vec3 p);

vec3 calcNormal( in vec3 pos )
{
    vec3 eps = vec3( 0.001, 0.0, 0.0 );
    vec3 nor = vec3(
        fScene(pos+eps.xyy) - fScene(pos-eps.xyy),
        fScene(pos+eps.yxy) - fScene(pos-eps.yxy),
        fScene(pos+eps.yyx) - fScene(pos-eps.yyx) );
    return normalize(nor);
}

mat3 persp(in vec3 ro, in vec3 ta, float cr )
{
    vec3 cw = normalize(ta-ro);
    vec3 cp = vec3(sin(cr), cos(cr),0.0);
    vec3 cu = normalize( cross(cw,cp) );
    vec3 cv = normalize( cross(cu,cw) );
    return mat3( cu, cv, cw );
}


float clamp01(float value)
{
    return clamp(value, 0.0, 1.0);
}

float fSphere(in vec3 p, float radius)
{
    return length(p) - radius;
}

// Repeat in three dimensions
vec3 pMod3(inout vec3 p, vec3 size) {
    vec3 c = floor((p + size*0.5)/size);
    p = mod(p + size*0.5, size) - size*0.5;
    return c;
}

float fScene(in vec3 p)
{
    p.z += iGlobalTime;
    pMod3(p, vec3(3.0));
    return fSphere(p, 0.5);
}

vec4 render(in vec3 ro, in vec3 rd)
{
    const float tMax = 100.0;
    const float epsilon = 0.001;

    float t = 1.0;

    const vec3 bg = vec3(0);
    vec3 col = bg;

    vec3 lightP = ro;

    for(int n = 0;n < 100; ++n)
    {
        vec3 p = ro + rd * t;

        float d = fScene(p);

        if (d > tMax) break;
        if (d < epsilon)
        {
            vec3 light = normalize(lightP - p);
            vec3 normal = calcNormal(p);

            // intensity of diffuse light
            float diff = clamp01(dot(light, normal));

            // intensity of specular - phong
            float shininess = 72.0;
            vec3 reflection = reflect(light, normal);
            float spec = pow(max(dot(rd, reflection), 0.0), shininess);

            float lightPower = 1.0 / (0.5 + length(lightP - p)*0.125);

            col = vec3(0,.1,0);
            col += diff * vec3(0,0.8,0.0) * lightPower + spec * vec3(1,1,1) * lightPower;
            break;
        }

        t += d;
    }

    float fogStart = 10.0;
    float fogDistance = length(rd * (fogStart - max(fogStart, t)));
    float fogDensity = 0.18;
    float fogAmount = 1.0 - exp( -fogDistance*fogDensity );
    vec3  fogColor = vec3(0.);
    col = mix( col, fogColor, fogAmount );

    col = pow(col, vec3(0.4545));
    return vec4(col, 1.0);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 uv = -1.0 + 2.0 * fragCoord.xy / iResolution.xy;
    uv.x *= iResolution.x/iResolution.y;

    // ray origin
    vec3 ro = vec3(0.0,1.0,0.0);

    // mouse look
    vec2 m = iMouse.xy/iResolution.xy;
    vec3 look = 100.0*normalize(vec3(sin(-m.x*PI), 0.7*m.y, cos(-m.x*PI)));

    mat3 ca = persp(ro,look,0.0);

    // ray direction
    vec3 rd = ca * normalize( vec3(uv.xy,2.0) );

    fragColor = render(ro, rd);
}


void mainVR( out vec4 fragColor, in vec2 fragCoord, in vec3 fragRayOri, in vec3 fragRayDir )
{
    fragColor = render( fragRayOri, fragRayDir );
}
