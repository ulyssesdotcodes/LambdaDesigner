out vec4 fragColor;

vec2 cart2Polar(vec2 v) {
  return vec2(atan(v.y, v.x), sqrt(v.x * v.x + v.y * v.y));
}

vec2 polar2Cart(vec2 p) {
  return vec2(p.y * cos(p.x), p.y * sin(p.x));
}

void main() {
  vec2 pos = vUV.st;
  //pos.y = 1 - pos.y;
  pos = (pos - vec2(0.5)) * 2 * uTD2DInfos[0].res.zw / uTD2DInfos[0].res.w;
  vec2 posp = cart2Polar(pos);
  posp.x += 3.1415 * 0.5; // Make seam at bottom
  //if (pos.x < 0 && pos.y > 0) posp.x += 3.1415;
  if (pos.x < 0 && pos.y < 0) posp.x += 6.28318;
  // if (pos.x > 0 && pos.y < 0) posp.x += 6.28318;
  posp.x /= 6.28318;
  fragColor = texture(sTD2DInputs[0], posp);
}
