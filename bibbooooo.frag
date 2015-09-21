// ALL TAKEN FROM IQs AMAZING SITE / TUTORIALS / SHADERS:
// http://www.iquilezles.org/www/index.htm
// https://www.shadertoy.com/user/iq

// Specific shaders stolen from
// ROT FUNCTIONS  : https://www.shadertoy.com/view/XsSSzG


const float MAX_TRACE_DISTANCE = 20.0;           // max trace distance
const float INTERSECTION_PRECISION = .01;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 80;

vec3 startPoints[ 25 ];
vec3 endPoints[ 25 ];

//----
// Camera Stuffs
//----
mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in float roll )
{
   vec3 ww = normalize( ta - ro );
   vec3 uu = normalize( cross(ww,vec3(sin(roll),cos(roll),0.0) ) );
   vec3 vv = normalize( cross(uu,ww));
   return mat3( uu, vv, ww );
}

void doCamera( out vec3 camPos, out vec3 camTar, in float time, in float mouseX )
{
   float an = 0.3 + 10.0*mouseX;
camPos = vec3(3.5*sin(an),1.0,3.5*cos(an));
   camTar = vec3(0.0,0.0,0.0);
}



// exponential smooth min (k = 32);
float smin( float a, float b, float k ){
   
   float res = exp( -k*a ) + exp( -k*b );
   return -log( res )/k;
   
}


// and makes the y of the vec2 be the proper id
vec2 opU( vec2 d1, vec2 d2 ){
   
return (d1.x<d2.x) ? d1 : d2;
   
}

float opBlend( vec2 d1, vec2 d2 ){

   return smin( d1.x , d2.x , 8.);
   
}

float sdCapsule( vec3 p, vec3 a, vec3 b, float r )
{
   vec3 pa = p - a, ba = b - a;
   float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
   return length( pa - ba*h ) - r;
}

float sdPlane( vec3 p )
{

   //float f = fbm( p.xy );
   //f *= fbm( p.yz );
   //f = 5. * smoothstep( abs(f) , 0.4 , 0.8 );
return p.y;// + sin( p.x * 1. * p.z ) * .002; //- (f * .4); //(abs( f) * .3)/ max( 1. , pow( length( p ), 1.));

}


//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){  
   
  vec2 res = vec2( sdPlane( pos - vec3( 0. , -1. , 0. )), 0.0 );
   
   for( int i = 0; i < 25; i++ ){
    float intersection = sdCapsule( pos , startPoints[i] , endPoints[i] , .05 );
    vec2 res2 = vec2( intersection ,  float( i ) + 1.);
    res.x = opBlend( res ,  res2 );
   }
    