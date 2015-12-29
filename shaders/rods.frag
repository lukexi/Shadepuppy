// ALL TAKEN FROM IQs AMAZING SITE / TUTORIALS / SHADERS:
// http://www.iquilezles.org/www/index.htm
// https://www.shadertoy.com/user/iq

// Specific shaders stolen from
// ROT FUNCTIONS  : https://www.shadertoy.com/view/XsSSzG


const float MAX_TRACE_DISTANCE = 20.0;           // max trace distance
const float INTERSECTION_PRECISION = .01;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 80;
    
const int numCapsules = 10;
vec3 startPoints[ numCapsules ];
vec3 endPoints[ numCapsules ];

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
    
    for( int i = 0; i < numCapsules; i++ ){
        float intersection = sdCapsule( pos , startPoints[i] , endPoints[i] , .05 );
        vec2 res2 = vec2( intersection ,  float( i ) + 1.);
        res.x = opBlend( res ,  res2 );
    }
    
    /*vec2 res = vec2( rotatedPlane( pos - vec3( 0. , -2. , 0. ) , vec3(.9 , 0. , 0)) , 0.);
    
    res.x = opBlend( res , vec2( rotatedPlane( pos - vec3( 0. , -2. , 0. ) , vec3(-.7 , 0. , 0)) , 0.));
    res.x = opBlend( res , vec2( rotatedPlane( pos - vec3( 0. , -2. , 0. ) , vec3(0. , 0. , 1.)) , 0.));
    //vec2 res = vec2( sdSphere( pos , 8. ) , 0.); , 1.
   
    for( int i = 0; i < 8; i++ ){
        

        vec3 rot = sphereRot[i];
        vec3 p = pos- spherePos[i].xyz;
        vec3 s = vec3( spherePos[i].w , spherePos[i].w / 100. , spherePos[i].w / 100.);
        float r = spherePos[i].w / 4.;
        
        float intersection = rotatedBox( p , rot , s , r );
        vec2 res2 = vec2( intersection , float(i) + 1.);
        res.x = opBlend( res ,  res2 );
        
    }*/
    

    return res;
    
}

// Calculates our intersection by stepping through the
// map function, and returning both the object hit and the distance
// it is hit at
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


float softshadow( in vec3 ro, in vec3 rd, in float mint, in float tmax )
{
    float res = 1.0;
    float t = mint;
    for( int i=0; i<50; i++ )
    {
        float h = map( ro + rd*t ).x;
        res = min( res, 20.*h/t );
        t += clamp( h, 0.02, 0.10 );
        if( h<0.001 || t>tmax ) break;
    }
    return clamp( res, 0.0, 1.0 );

}


float calcAO( in vec3 pos, in vec3 nor )
{
    float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<3; i++ )
    {
        float hr = 0.01 + 0.612*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).x;
        occ += -(dd-hr)*sca;
        sca *= 0.5;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
}


vec3 render(vec3 ro , vec3 rd ){

 vec2 res = calcIntersection( ro , rd  );
    

  vec3 col = vec3( 1. );
      
  if( res.y > -.5 ){
  
      
      vec3 pos = ro + rd * res.x;
      vec3 nor = calcNormal( pos );
      
      float AO = calcAO( pos , normalize(nor) );
      
      col = vec3(1.) * AO; //nor * .5 + .5;
  }
  
  
  return col;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord ){
    
    vec2 uv = fragCoord.xy / iResolution.xy;
    
    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = iMouse.xy/iResolution.xy;
    //vec2 m = vec2( 1.1 +( sin( iGlobalTime * 0.4 ) + sin( iGlobalTime * 0.01 )) * .04 , 0. );
    
    for( int i = 0; i < numCapsules; i++ ){
        startPoints[i].x = sin( float(i) * iGlobalTime * .03  ); 
        startPoints[i].y = sin( float(i) * iGlobalTime * .03 * .2 );   
        startPoints[i].z = sin( float(i)* iGlobalTime * .03  * .5 );
        
        endPoints[i].x = sin( float(i)* iGlobalTime * .03  * 3. ); 
        endPoints[i].y = cos( float(i)* iGlobalTime * .03  * 1.2 );   
        endPoints[i].z = cos( float(i)* iGlobalTime * .03  * .5 );

        
    }
    
    //-----------------------------------------------------
    // camera
    //-----------------------------------------------------
    
    // camera movement
    vec3 ro, ta;
    doCamera( ro, ta, iGlobalTime, m.x );

    // camera matrix
    mat3 camMat = calcLookAtMatrix( ro, ta, 0.0 );  // 0.0 is the camera roll
    
    // create view ray
    vec3 rd = normalize( camMat * vec3(p.xy,1.5 + abs(sin( iGlobalTime * .01 ) + sin( iGlobalTime * .0315 ))) ); // 2.0 is the lens length

    vec3 col = render( ro , rd );

   fragColor = vec4( col, 1.0 );
    
}



void mainVR( out vec4 fragColor, in vec2 fragCoord, in vec3 fragRayOri, in vec3 fragRayDir )
{

      for( int i = 0; i < numCapsules; i++ ){
        startPoints[i].x = sin( float(i) * iGlobalTime * .03  ); 
        startPoints[i].y = sin( float(i) * iGlobalTime * .03 * .2 );   
        startPoints[i].z = sin( float(i)* iGlobalTime * .03  * .5 );
        
        endPoints[i].x = sin( float(i)* iGlobalTime * .03  * 3. ); 
        endPoints[i].y = cos( float(i)* iGlobalTime * .03  * 1.2 );   
        endPoints[i].z = cos( float(i)* iGlobalTime * .03  * .5 );

        
    }
    
    vec3 col = render( fragRayOri + vec3(0.0,-0.1,0.0), fragRayDir );
    
    fragColor = vec4( col, 1.0 );
}
