uniform float i_time;

out vec4 fragColor;


float noise(vec2 p)
{
	float s = texture(sTDNoiseMap,vec2(1.,2.*cos(i_time))*i_time*8. + p*1.).x;
	s *= s;
	return s;
}

float onOff(float a, float b, float c)
{
	return step(c, sin(i_time + a*cos(i_time*b)));
}

float ramp(float y, float start, float end)
{
	float inside = step(start,y) - step(end,y);
	float fact = (y-start)/(end-start)*inside;
	return (1.-fact) * inside;
	
}

float stripes(vec2 uv)
{
	
	float noi = noise(uv*vec2(0.5,1.) + vec2(1.,3.));
	return ramp(mod(uv.y*4. + i_time/2.+sin(i_time + sin(i_time*0.63)),1.),0.5,0.6)*noi;
}

vec3 getVideo(vec2 uv)
{
	vec2 look = uv;
	float window = 1./(1.+20.*(look.y-mod(i_time/4.,1.))*(look.y-mod(i_time/4.,1.)));
	look.x = look.x + sin(look.y*10. + i_time)/50.*onOff(4.,4.,.3)*(1.+cos(i_time*80.))*window;
	float vShift = 0.4*onOff(2.,3.,.9)*(sin(i_time)*sin(i_time*20.) + 
										 (0.5 + 0.1*sin(i_time*200.)*cos(i_time)));
	look.y = mod(look.y + vShift, 1.);
	vec3 video = vec3(texture(sTD2DInputs[0],look));
	return video;
}

vec2 screenDistort(vec2 uv)
{
	uv -= vec2(.5,.5);
	uv = uv*1.2*(1./1.2+2.*uv.x*uv.x*uv.y*uv.y);
	uv += vec2(.5,.5);
	return uv;
}

void main()
{
	vec2 uv = vUV.st;
	uv = screenDistort(uv);
	vec3 video = getVideo(uv);
	float vigAmt = 3.+.3*sin(i_time + 5.*cos(i_time*5.));
	float vignette = (1.-vigAmt*(uv.y-.5)*(uv.y-.5))*(1.-vigAmt*(uv.x-.5)*(uv.x-.5));
	
	video += stripes(uv);
	video += noise(uv*2.)/2.;
	video *= vignette;
	video *= (12.+mod(uv.y*30.+i_time,1.))/13.;
	
	fragColor = vec4(video,1.0);
}