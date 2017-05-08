#define EdgeColor vec4(0, 0, 0, 1.0)
#define BackgroundColor vec4(1)
#define NoiseAmount 0.01
#define ErrorPeriod 30.0
#define ErrorRange 0.003

uniform float uTime;
uniform float uDisplacement;

out vec4 fragColor;

float triangle(float x)
{
	return abs(1.0 - mod(abs(x), 2.0)) * 2.0 - 1.0;
}

float rand(float x)
{
    return fract(sin(x) * 43758.5453);
}

void main()
{
    float time = floor(uTime * 16.0) / 16.0;
    vec2 uv = vUV.st;
    // uv += vec2(triangle(uv.y * rand(time) * 1.0) * rand(time * 1.9) * 0.005,
		// 	triangle(uv.x * rand(time * 3.4) * 1.0) * rand(time * 2.1) * 0.005);
    
    float noise1 = 0;
    float noise2 = (texture2D(sTDNoiseMap, vec2(sin(uTime * 0.7), cos(uTime * 0.5))).r - 0.5) * 0.02 * uDisplacement * rand(uTime);
    float noise3 = (texture2D(sTDNoiseMap, vec2(sin(uTime * 0.7), cos(uTime * 0.5))).r - 0.5) * 0.02 * uDisplacement * rand(uTime * 0.4);
    vec2 uvs[3];
    uvs[0] = uv;
    uvs[1] = uv + vec2(ErrorRange * sin(ErrorPeriod * uv.y + 1.047) + noise2, ErrorRange * cos(ErrorPeriod * uv.x + 3.142) + noise2);
    uvs[2] = uv + vec2(ErrorRange * cos(ErrorPeriod * uv.x + 1.571) + noise3, -(ErrorRange * sin(ErrorPeriod * uv.y + 2.094) + noise3));
    vec4 edge = texture2D(sTD2DInputs[0], uvs[0]) + texture2D(sTD2DInputs[0], uvs[1]) * 0.5 + texture2D(sTD2DInputs[0], uvs[2]) * 0.5;

	fragColor = edge;
}
