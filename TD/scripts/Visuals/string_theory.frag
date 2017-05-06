//Taken from shadertoy.com
//String Theory by nimitz (twitter: @stormoid)

uniform float i_angle;
uniform float i_angle_delta;
uniform float i_xoff;
uniform float i_time;

out vec4 fragColor;

mat2 mm2(float a){
	float c = cos(a);
	float s = sin(a); 
	return mat2(c,-s,s,c);
}

float aspect = 16.f/9.f;
float featureSize = 60./(1920.f*aspect+1080.f);

float f(vec2 p)
{
	p.x = sin(p.x*1.+i_time*1.2)*sin(i_time+p.x*0.1)*3.;	
    p += sin(p.x*1.5)*.1;
    return smoothstep(-0.0,featureSize,abs(p.y));
}

void main()
{
	vec2 p = vUV.xy*6.5 - 3.25;
	p.y = abs(p.y);
	
	vec3 col = vec3(0);
	for(float i=0.;i<26.;i++)
	{
		vec3 col2 = (sin(vec3(3.7,2.5,2.2)+i*0.15)*0.5+0.54)*(1.-f(p));
		col = max(col,col2);
		
        p.x -= (i_xoff + 0.7);
        p.y -= sin(i_time*0.11+1.5)*1.5+1.5;
		p*= mm2(i*(i_angle_delta + 0.02)+(i_angle + 3.5));
		
        vec2 pa = vec2(abs(p.x-.9),abs(p.y));
        vec2 pb = vec2(p.x,abs(p.y));
        
        p = mix(pa,pb,smoothstep(-.07,.07,sin(i_time*0.24)+.1));
	}

	fragColor = vec4(col,sign(col.x + col.y + col.z));
}
