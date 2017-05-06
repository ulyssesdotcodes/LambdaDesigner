// Created by Ulysses Popple, inspired by inigo quilez - iq/2013
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

uniform float uTime;
uniform float uScale;

out vec4 fragColor;

vec2 hash2( vec2 p )
{
  // procedural white noise	
	return fract(sin(vec2(dot(p,vec2(127.1,311.7)),dot(p,vec2(269.5,183.3))))*43758.5453);
}

vec3 voronoi( in vec2 x )
{
    vec2 n = floor(x);
    vec2 f = fract(x);

    //----------------------------------
    // first pass: regular voronoi
    //----------------------------------
	vec2 mg, mr;

    float md = 8.0;
    for( int j=-1; j<=1; j++ )
    for( int i=-1; i<=1; i++ )
    {
        vec2 g = vec2(float(i),float(j));
		vec2 o = hash2( n + g );
        o = 0.5 + 0.5*sin( uTime + 6.2831*o );
        vec2 r = g + o - f;
        float d = dot(r,r);

        if( d<md )
        {
            md = d;
            mr = r;
            mg = g;
        }
    }
    md = 8.0;
    for( int j=-2; j<=2; j++ )
    for( int i=-2; i<=2; i++ )
    {
        vec2 g = mg + vec2(float(i),float(j));
		vec2 o = hash2( n + g );
        o = 0.5 + 0.5*sin( uTime + 6.2831*o );
        vec2 r = g + o - f;

        if( dot(mr-r,mr-r)>0.00001 )
        md = min( md, dot( 0.5*(mr+r), normalize(r-mr) ) );
    }

    return vec3( md, mr );
}


void main() {
  if(uScale < 0.2) {
    discard;
  }

  vec2 pos = vUV.st;
  vec3 c = voronoi( uScale*pos );
  vec4 tex = texture2D(sTD2DInputs[0], c.yz/uScale + pos);
  vec3 col = tex.xyz;
  col = mix( vec3(0.0), col, smoothstep( 0.04, 0.07, c.x ) );

	fragColor = vec4(col,tex.w);
}
