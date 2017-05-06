#define PI 3.14159265359
#define TWO_PI 6.28318530718

uniform float i_width;
uniform float i_spacing;

out vec4 fragColor;

void main() {
  vec2 uv = vUV.st;

  vec3 color = vec3(0, 0, 0);

  uv.x += (1 + i_spacing) / i_spacing;

  float d = round(sin(uv.x * TWO_PI * (1/i_spacing)) * 0.5 + (0.5 * (i_width / i_spacing)));

  color = vec3(d);
  fragColor = vec4(color, 1.0);
}
