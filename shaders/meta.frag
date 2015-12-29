// ALL TAKEN FROM IQs AMAZING SITE / TUTORIALS / SHADERS:
// http://www.iquilezles.org/www/index.htm
// https://www.shadertoy.com/user/iq

const float MAX_TRACE_DISTANCE = 1000.0;           // max trace distance
const float INTERSECTION_PRECISION = .001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
	
const float speed =.0005;

vec3 lookPos = vec3( 0. , 0. , 1. );

vec4 texCube( sampler2D sam, in vec3 p, in vec3 n, in float k )
{
	vec4 x = texture2D( sam, p.yz );
	vec4 y = texture2D( sam, p.zx );
	vec4 z = texture2D( sam, p.xy );
    vec3 w = pow( abs(n), vec3(k) );
	return (x*w.x + y*w.y + z*w.z) / (w.x+w.y+w.z);
}




float udRoundBox( vec3 p, vec3 b, float r )
{
  return length(max(abs(p)-b,0.0))-r;
}

float repBox( vec3 p, vec3 c , vec3 b , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    return  udRoundBox( q , b,  r);
}


float sdPlane( vec3 p )
{
    
    float f = sin( p.z * 5. ) * sin( p.x * 5. );
    //f = 5. * smoothstep( abs(f) , 0.4 , 0.8 );
	return p.y - (abs( f) * .3)/ max( 1. , pow( length( p ), 1.));

}



float sdSphere( vec3 p, float s )
{
  return length(p)-s;
}

float sdCone( in vec3 p, in vec3 c )
{
    vec2 q = vec2( length(p.xz), p.y );

    float d1 = -p.y-c.z;
    float d2 = max( dot(q,c.xy), p.y);
    return length(max(vec2(d1,d2),0.0)) + min(max(d1,d2), 0.);

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
	camPos = vec3(3.5*sin(an),1.0,3.5*cos(an));
    camTar = vec3(0.0,0.0,0.0);
}




// checks to see which intersection is closer
// and makes the y of the vec2 be the proper id
vec2 opU( vec2 d1, vec2 d2 ){
    
	return (d1.x<d2.x) ? d1 : d2;
    
}

// exponential smooth min (k = 32);
float smin( float a, float b, float k )
{
    float res = exp( -k*a ) + exp( -k*b );
    return -log( res )/k;
}

float opBlend( vec2 d1, vec2 d2 )
{

    return smin( d1.x , d2.x , 8.);
}
//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){  
    
   	//vec2 res = vec2( 10000. , 0.);// vec2( sdPlane( pos - vec3( 0. , -1. , 0. )), 0.0 );
    vec2 res = vec2( sdSphere( pos - lookPos , 1.2 ) , 1.);
    
    vec2 res2 = vec2( repBox( pos , vec3( .7 , .7 , .7 ) , vec3( .1 , .1 , .1 ) , .05 ) , 10. );

    res.x = opBlend( res,res2 );

    res2 = vec2( sdSphere( pos  , 6.2 ) , 1.);
  
    res.x = opBlend( res,res2 );
     for( int i = 0; i < 4; i++ ){
   		
        float x = 1. * cos(iGlobalTime *.13 * (float( i )+10.));
        float y = 1. * sin(iGlobalTime * .075 * (float( i )+40.));
        float z = 1. * sin(iGlobalTime * .1 * (float( i )+31.3));
        float r = .1 * ( sin( iGlobalTime * .1  *( float( i) +13.))+2.);
        vec4 sPos = vec4( x ,  y ,  z , r  );
        sPos.xyz += lookPos;
        
        res2 = vec2( sdSphere( (pos - sPos.xyz) , .345 ) , float(i) + 1.);
        //vec2 res2 = vec2( udRoundBox( (pos - spherePos[i].xyz) , vec3( spherePos[i].w  ) ,spherePos[i].w * .2 ) , float(i) + 1.);
   		res.x = opBlend( res ,  res2 );
        
   	}

    
   	return res;
    
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

vec2 res = calcIntersection( ro , rd  );
    
    
    vec3 col = vec3( 1. );//vec3( .8 , .8 , .8 ); 
   
    // If we have hit something lets get real!
    if( res.y > -.5 ){

        vec3 pos = ro + rd * res.x;
        vec3 nor = calcNormal( pos );
        
        float AO = calcAO( pos , nor );
        
        vec3 lightDir = normalize( pos - lookPos ); 
        float match = -dot( nor , lightDir );
        

       // float c = pow(( 1.-AO) , 5.)*  10.;
        float c = 0.;
        c += pow((1.- match),4.) * 1.;
        c += pow(( 1.-AO) , 5.)*  2.;
        col =vec3(AO) * ( nor * .5 + .5 );

        //Fog
        col += (res.x /10.) * (res.x /10.) * vec3( 1. );

        col =vec3( 1.) - vec3( AO );
        
       /* if( res.y < .5 ){
            
            float f = sin( pos.z * 5. ) * sin( pos.x * 5. );
            //col *= 5. * smoothstep( abs(f) , 0.4 , 0.8 );
            col *= abs( f );
            col /= pow( length( pos ), 4.);
        }*/
    }
    // apply gamma correction
    col = pow( col, vec3(0.4545) );

    return col;
}
void mainImage( out vec4 fragColor, in vec2 fragCoord ){
    

   /*for( int i =0; i < 5; i++ ){
        
        float x = 1. * cos(iGlobalTime *.13 * (float( i )+2.));
        float y = 1. * sin(iGlobalTime * .075 * (float( i )+4.));
        float z = 1. * sin(iGlobalTime * .1 * (float( i )+3.3));
        float r = .1 * ( sin( iGlobalTime * .1  *( float( i) +1.))+2.);
    	spherePos[i] = vec4( x ,  y ,  z - iGlobalTime * 2. , r  );
        
        
    }*/

    
    
    
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
    
    lookPos = ro + rd * 3.5;
    vec3 col = render( ro , rd );

    fragColor = vec4( col , 1. );
    
}


void mainVR( out vec4 fragColor, in vec2 fragCoord, in vec3 fragRayOri, in vec3 fragRayDir )
{

    vec3 lookRay = normalize( mix( mix( unCorners[0], unCorners[1],.5),
                              mix( unCorners[3], unCorners[2], .5 ), .5 ) - fragRayOri); 
    vec3 newCenter = fragRayOri + vec3(0.0,0.0,2.5 -  iGlobalTime * speed);
    lookPos = newCenter + lookRay * 3.;
    vec3 col = render( newCenter , fragRayDir );
    fragColor = vec4( col, 1.0 );
}