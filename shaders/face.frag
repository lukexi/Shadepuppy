
#define time iGlobalTime

const float INTERSECTION_PRECISION = .01;
const float MAX_TRACE_DISTANCE     = 10.;
const int NUM_TRACE_STEPS          = 80;

const vec3 lightPos = vec3( 3. , 0.  , 0. );

vec3 lookPos;


float smin_2_3(float a, float b, float k) {
  float h = clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0);
  return mix(b, a, h) - k * h * (1.0 - h);
}



// checks to see which intersection is closer
// and makes the y of the vec2 be the proper id
vec2 opU( vec2 d1, vec2 d2 ){
    
    return (d1.x<d2.x) ? d1 : d2;
    
}

float opU( float d1, float d2 )
{
    return min(d1,d2);
}


void doCamera( out vec3 camPos , out vec3 camTar , in float time ){

  camPos = vec3( 0. , 0. , 1.5 );
  camTar = vec3( 0. );

}

mat3 calcLookAtMatrix( vec3 camPos , vec3 camTar , float roll ){

  vec3 up = vec3( sin( roll ) ,cos( roll ) , 0. );
  vec3 ww = normalize( camTar - camPos );
  vec3 uu = normalize( cross( ww , up ) );
  vec3 vv = normalize( cross( uu , ww ) );

  return mat3( uu , vv , ww );

}
float sdPlane( vec3 pos ){
 return pos.y;   
}
float sdSphere( vec3 p, float s )
{
  return length(p)-s;
}

float sdCappedCone( in vec3 p, in vec3 c )
{
    vec2 q = vec2( length(p.xy), -p.z - c.z );
    vec2 v = vec2( c.z*c.y/c.x, -c.z );

    vec2 w = v - q;

    vec2 vv = vec2( dot(v,v), v.x*v.x );
    vec2 qv = vec2( dot(v,w), v.x*w.x );

    vec2 d = max(qv,0.0)*qv/vv;

    return sqrt( dot(w,w) - max(d.x,d.y) )* sign(max(q.y*v.x-q.x*v.y,w.y));
}
float sdTorus( vec3 p, vec2 t )
{
  vec2 q = vec2(length(p.xy)-t.x,p.z);
  return length(q)-t.y;
}

float udBox( vec3 p, vec3 b )
{
  return length(max(abs(p)-b,0.0));
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

float face( vec3 pos , vec3 p , mat3 rot ){
    
    mat3 iRot = matInverse( rot );
    
    vec3 headPos  = iRot * ( pos - p );
    vec3 nosePos  = iRot * ( pos - p );
    vec3 lePos    = iRot * ( pos - p + rot * vec3( -.1 , -.1 , 0.));
    vec3 rePos    = iRot * ( pos - p + rot * vec3( .1 , -.1 , 0.));
    vec3 mouthPos = iRot * ( pos - p + rot * vec3( 0. , .12 , 0.));

    
    float nose = sdCappedCone( nosePos , vec3( .1 , .04 , .1 ) );
    float head = udBox( headPos , vec3( .2 , .2 , .01 ));
    
    float re = sdSphere( rePos , .03 );
    float le = sdSphere( lePos , .03 );
    
    float mouth = sdTorus( mouthPos , vec2( .04 , .02 ));

    float f = head;
    
    f = smin_2_3( f , nose  , .04 );
    f = smin_2_3( f , le    , .04 );
    f = smin_2_3( f , re    , .04 );
    f = smin_2_3( f , mouth , .04 );
                       
    return f;
    
}

float fire( vec3 pos ){

    vec3 p = vec3( 0. , -.3 , 0.5 );
    float f = sdSphere( pos - p  , .1 ); 
    
    
    for( int i  = 0; i < 4; i++ ){
        
       float h = mod( time * (abs( sin( float( i + 5 ))) +.3) * .05 , .3 );
       p  = vec3( 
         sin( ( h + .2) * float( i + 5 ) ) * .8 * h,
         h * 2. - .3,
         cos(( h +.3) * float( i + 5 ) ) * .8 * h + .5
       );   
       
       float r = abs( sin( p.x * time * .01 )  + sin( p.y * float( i +4 ) * time * .001 ));
       float v = sdSphere( pos - p  , (.3 - h ) * .2 ); 
       f = smin_2_3( f , v  , .04 );
       
        
    }
    
    
    return f;
    
}



vec2 map( vec3 pos ){


  vec2 res = vec2( 10000. , -1. );//vec2( sdSphere( pos - lookPos , .01 ) ,1. );
  vec2 res2;
  for( int i = 0; i < 1; i ++ ){
   
  
    vec3 p = vec3( (((float( i )+.5) / 1. ) - .5 ) * 2., 0.,0.);
      
    mat3 rot = calcLookAtMatrix( p , lookPos , 0. );
      
    res2 = vec2(face( pos , p , rot) , float( i ) ); 

    res = opU( res , res2 );

  }  
    
  res2 = vec2( fire( pos ) , 10. );
    
  res = opU( res , res2 );
    
  res2 = vec2( sdPlane( pos  - vec3( 0. , -.5 , 0. ) ), 100. );
  res = opU( res , res2 );
    

  return res;



}


// res = result;
vec2 calcIntersection( in vec3 ro , in vec3 rd ){

  float h     = INTERSECTION_PRECISION * 2.;
  float t     = 0.;
  float res   = -1.;
  float id    = -1.;

  for( int i = 0; i < NUM_TRACE_STEPS; i++ ){
      
    if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE ) break;
    
    vec2 m = map( ro + rd * t );
  
    h  = m.x;
    t += h;
    id = m.y;

  }

  if( t < MAX_TRACE_DISTANCE ) res = t;
  if( t > MAX_TRACE_DISTANCE ) id = -1.;

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
    for( int i=0; i<2; i++ )
    {
        float hr = 0.01 + 0.612*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).x;
        occ += -(dd-hr)*sca;
        sca *= 0.5;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
}


vec3 render( vec3 ro , vec3 rd ){
   
  vec2 res = calcIntersection( ro , rd );
  vec3 col = vec3( 1. );

  if( res.x > 0. ){

    vec3 pos = ro + rd * res.x;
    vec3 nor = calcNormal( pos );

      float AO = calcAO( pos , nor );

      float fix = pow( AO , 1.4 );
      
      vec3 coloa = vec3( .6 , .2 , .1 );
      
      // fire
      if( res.y > 9.9 ){
          coloa = vec3( .6 , .4 , .2 ); 
      }
      
      if( res.y > 99.9 ){
        coloa = vec3( .2 , .2 , .5 );    
      }
      
      coloa *= nor * .5 + .5;
      col = mix( vec3( 1. ) , coloa , 1. - fix );

      if( res.y < 10. ){

        col = nor * .5 + .5;
      }

  }
    
  return col;
    
}


void mainImage( out vec4 fragColor , in vec2 fragCoord){

  vec2 p = ( -iResolution.xy + 2.0 * fragCoord.xy ) / iResolution.y;
  
  // Getting mouse position!
  vec2 m =  ( -iResolution.xy + 2.0 * iMouse.xy ) / iResolution.y;;
    
  vec3 ro , ta;
  
  doCamera( ro , ta , time  );

  mat3 camMat = calcLookAtMatrix( ro , ta , 0. ); 
 
  // z = lens length 
  vec3 rd = normalize( camMat * vec3( p.xy , 2. ) ); 
    
  // Getting our mouse ray direction
  vec3 md = normalize( camMat * vec3( m.xy , 2. ) ); 
    
  lookPos = ro + md * 1.;
    
  //lookPos = vec3( 0. , -.3 , .5 );
 
  vec3 col = render( ro , rd );

  fragColor = vec4( col , 1. );

}


void mainVR( out vec4 fragColor, in vec2 fragCoord, in vec3 fragRayOri, in vec3 fragRayDir )
{

    lookPos = fragRayOri + vec3(0.0,0.0,1.);
    vec3 col = render( fragRayOri + vec3(0.0,0.0,1.), fragRayDir );
    
    fragColor = vec4( col, 1.0 );
}