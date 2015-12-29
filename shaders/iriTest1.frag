// ALL TAKEN FROM IQs AMAZING SITE / TUTORIALS / SHADERS:
// http://www.iquilezles.org/www/index.htm
// https://www.shadertoy.com/user/iq


const float MAX_TRACE_DISTANCE = 10.0;           // max trace distance
const float INTERSECTION_PRECISION = 0.001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 200;


vec3 hsv(float h, float s, float v)
{
  return mix( vec3( 1.0 ), clamp( ( abs( fract(
    h + vec3( 3.0, 2.0, 1.0 ) / 3.0 ) * 6.0 - 3.0 ) - 1.0 ), 0.0, 1.0 ), s ) * v;
}


// q is point
// n is normal
// p is point on plane
vec3 projOnPlane( vec3 q, vec3 p , vec3 n){
    
    vec3 v = q - dot(q - p, n) * n;
    return v;
}

// Taken from https://www.shadertoy.com/view/4ts3z2
float tri(in float x){return abs(fract(x)-.5);}
vec3 tri3(in vec3 p){return vec3( tri(p.z+tri(p.y*1.)), tri(p.z+tri(p.x*1.)), tri(p.y+tri(p.x*1.)));}
                                 

// Taken from https://www.shadertoy.com/view/4ts3z2
float triNoise3D(in vec3 p, in float spd)
{
    float z=1.4;
    float rz = 0.;
    vec3 bp = p;
    for (float i=0.; i<=3.; i++ )
    {
        vec3 dg = tri3(bp*2.);
        p += (dg+iGlobalTime*.1*spd);

        bp *= 1.8;
        z *= 1.5;
        p *= 1.2;
        //p.xz*= m2;
        
        rz+= (tri(p.z+tri(p.x+tri(p.y))))/z;
        bp += 0.14;
    }
    return rz;
}

float posToFloat( vec3 p ){
 
    float f = triNoise3D( p * .2, .1 );
    return f;
    
}

void buildBasis( in vec3 dir , in vec3 up , out vec3 x , out vec3 y , out vec3 z ){
    

 //vec3( 0. , 1. , 0. );
  //vec3  upVector = normalize( centerOfCircle );// vec3( 0. , 1. , 0. );
  float upVectorProj = dot( up , dir );
  vec3  upVectorPara = upVectorProj * dir;
  vec3  upVectorPerp = up - upVectorPara;

  vec3 basisX = normalize( upVectorPerp );
  vec3 basisY = cross( dir , basisX );
    
    
  x = basisX;
  y = basisY;
  z = dir;
    
}


float udBox( vec3 p, vec3 b )
{
  return length(max(abs(p)-b,0.0));
}


float udRoundBox( vec3 p, vec3 b, float r )
{
  return length(max(abs(p)-b,0.0))-r;
}


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
    camPos = vec3(3.5*sin(an),.0,3.5*cos(an));
    camTar = vec3(0.0,0.0,0.0);
}

// ROTATION FUNCTIONS TAKEN FROM
//https://www.shadertoy.com/view/XsSSzG
mat3 xrotate(float t) {
    return mat3(1.0, 0.0, 0.0,
                0.0, cos(t), -sin(t),
                0.0, sin(t), cos(t));
}

mat3 yrotate(float t) {
    return mat3(cos(t), 0.0, -sin(t),
                0.0, 1.0, 0.0,
                sin(t), 0.0, cos(t));
}

mat3 zrotate(float t) {
    return mat3(cos(t), -sin(t), 0.0,
                sin(t), cos(t), 0.0,
                0.0, 0.0, 1.0);
}


mat3 fullRotate( vec3 r ){
 
   return xrotate( r.x ) * yrotate( r.y ) * zrotate( r.z );
    
}

float rotatedBox( vec3 p , vec3 rot , vec3 size , float rad ){
    
    vec3 q = fullRotate( rot ) * p;
    return udRoundBox( q , size , rad );
    
    
}



// checks to see which intersection is closer
// and makes the y of the vec2 be the proper id
vec2 opU( vec2 d1, vec2 d2 ){
    
    return (d1.x<d2.x) ? d1 : d2;
    
}

//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){  
   
    // using super thin cube as plane
    vec3 size = vec3( .1  , .1 , .001 );
    //vec3 rot = vec3( iGlobalTime * .1 , iGlobalTime * .4 , -iGlobalTime * .3 );
    vec3 rot = vec3( 0.,0.,0. );
    vec2 res = vec2( rotatedBox( pos , rot , size , .001 ) , 1.0 );

    return res;
    
}

mat3 matInverse( mat3 m ){
    
  
    vec3 a = vec3(
      
        m[1][1] * m[2][2] - m[2][1] * m[1][2],
        m[0][2] * m[2][1] - m[2][2] * m[0][1],
        m[0][1] * m[1][2] - m[1][1] * m[0][2]
        
    );
    
    vec3 b = vec3(
      
        m[1][2] * m[2][0] - m[2][2] * m[1][0],
        m[0][0] * m[2][2] - m[2][0] * m[0][2],
        m[0][2] * m[1][0] - m[1][2] * m[0][0]
        
    );
    
     vec3 c = vec3(
      
        m[1][0] * m[2][1] - m[2][0] * m[1][1],
        m[0][1] * m[2][0] - m[2][1] * m[0][0],
        m[0][0] * m[1][1] - m[1][0] * m[0][1]
        
    );
    
    
    return mat3( 
        
       a.x , a.y , a.z ,
       b.x , b.y , b.z ,
       c.x , c.y , c.z
        
    );
    
 
  
    
}

vec2 calcIntersection( in vec3 ro, in vec3 rd ){

    
    float h =  INTERSECTION_PRECISION*2.0;
    float t = 0.0;
    float res = -1.0;
    float id = -1.;
    
    for( int i=0; i< NUM_OF_TRACE_STEPS ; i++ ){
        
        if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE ) break;
        vec2 m = map( ro+rd*t );
        h = m.x;
        t += h;
        id = m.y;
        
    }

    if( t < MAX_TRACE_DISTANCE ) res = t;
    if( t > MAX_TRACE_DISTANCE ) id =-1.0;
    
    return vec2( res , id );
    
}


#define STEPS 5
vec3 fogCube( vec3 ro , vec3 rd , vec3 n , mat3 iMat){
 
    float lum = 1.;
    
    vec3 col = vec3( 0. );
    for( int i = 0; i < STEPS; i++ ){
        vec3 p = ro + rd * .03  * float( i );

        lum += abs(sin( p.x * 100. ) + sin( p.y * 100. ));// + sin( p.y * 3. ) + sin( p.z * 5.);
    
        col += hsv( lum / 10. + sin( iGlobalTime * .1 ) , 1. , 1. ) / lum;
    }
    
    return col/float(STEPS);
    
    
}

// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
    vec3 eps = vec3( 0.001, 0.0, 0.0 );
    vec3 nor = vec3(
        map(pos+eps.xyy).x - map(pos-eps.xyy).x,
        map(pos+eps.yxy).x - map(pos-eps.yxy).x,
        map(pos+eps.yyx).x - map(pos-eps.yyx).x );
    return normalize(nor);
}


vec3 render( in vec3 ro , in vec3 rd ){

    vec2 res = calcIntersection( ro , rd  );


    vec3 col = vec3( 0. );
    
    mat3 basis = mat3(
     
        1. , 0. , 0. ,
        0. , 1. , 0. ,
        0. , 0. , 1.
       
    );
        
    mat3 iMat = matInverse( basis );
    
    if( res.y > -.5 ){
        
        vec3 pos = ro + rd * res.x;
        vec3 norm = calcNormal( pos );
        
        vec3 depthColor = fogCube( pos , rd , norm , iMat );
 
        col += depthColor;//lum  * vec3( 1. , .6 , 0.2);
        
        
    }

    return col;

}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = iMouse.xy/iResolution.xy;

    //-----------------------------------------------------
    // camera
    //-----------------------------------------------------
    
    // camera movement
    vec3 ro, ta;
    doCamera( ro, ta, iGlobalTime, m.x );

    // camera matrix
    mat3 camMat = calcLookAtMatrix( ro, ta, 0.0 );  // 0.0 is the camera roll
    
    // create view ray
    vec3 rd = normalize( camMat * vec3(p.xy,2.0) ); // 2.0 is the lens length
    
    vec3 col = render( ro , rd );
    fragColor = vec4( col , 1. );

}

void mainVR( out vec4 fragColor, in vec2 fragCoord, in vec3 fragRayOri, in vec3 fragRayDir )
{
    vec3 col = render( fragRayOri + vec3(0.0,0.0,.5), fragRayDir );
    
    fragColor = vec4( col, 1.0 );
}