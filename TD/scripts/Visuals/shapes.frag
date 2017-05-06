#define PI 3.14159265359
#define TWO_PI 6.28318530718

uniform vec2 i_resolution;
uniform float i_sides;
uniform float i_width;
uniform float i_size;

out vec4 fragColor;

void main() {
  vec2 uv = (vUV.st - vec2(0.5)) * 2 * i_resolution.xy / i_resolution.y;

  float a = atan(uv.x, uv.y) + PI;
  float r = TWO_PI/max(1, round(i_sides));

  float l = length(uv);
  float d = 1.0 - smoothstep(i_size, i_size + 0.001, cos(floor(.5+a/r)*r - a) * l);
  float e = 1.0 - smoothstep(i_size + i_width, i_size + i_width + 0.001, cos(floor(.5+a/r)*r - a) * l);

  fragColor = vec4(e - d);
}
