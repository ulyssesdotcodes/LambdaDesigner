out vec4 fragColor;

uniform float uSeconds;

vec2 cart2Polar(vec2 v) {
  return vec2(atan(v.y, v.x), sqrt(v.x * v.x + v.y * v.y));
}

vec2 polar2Cart(vec2 p) {
  return vec2(p.y * cos(p.x), p.y * sin(p.x));
}

void main() {
  vec2 pos = vUV.st;
  pos = (pos - vec2(0.5)) * 2;
  vec2 posp = cart2Polar(pos);
  posp.x += 3.1415 * 0.75 * sin((1 - posp.y) + uSeconds); // Make seam at bottom
  fragColor = texture(sTD2DInputs[0], (polar2Cart(posp) * 0.5) + vec2(0.5));
}