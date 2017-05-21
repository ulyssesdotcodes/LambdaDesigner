uniform float u_delta;
uniform float u_cohesion;
uniform float u_separation;
uniform float u_alignment;
uniform float u_speed;

#define VELOCITY 0
#define POSITION 1
#define PI_2 6.28318

out vec4 fragColor;
void main()
{
	vec3 velocity = texture(sTD2DInputs[VELOCITY], vUV.st).xyz;
	vec3 position = texture(sTD2DInputs[POSITION], vUV.st).xyz;


	fragColor = vec4(velocity, 1.0);
}