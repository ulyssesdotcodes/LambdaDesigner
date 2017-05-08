out vec4 fragColor;

vec3 hsv2rgb(vec3 c) {
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

vec3 rgb2hsv(vec3 c) {
  vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
  vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
  vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

  float d = q.x - min(q.w, q.y);
  float e = 1.0e-10;
  return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

void main() {

  vec2 i;
  vec2 pos = (vUV.st - vec2(0.5)) * 2 * uTD2DInfos[0].res.zw / uTD2DInfos[0].res.w;
  vec2 posp = floor(pos * 20 + vec2(0.5)) * 0.05;
  vec4 b = texture(sTD2DInputs[0], posp * uTD2DInfos[0].res.w * 0.5 / uTD2DInfos[0].res.zw + vec2(0.5));
  vec3 hsv = rgb2hsv(b.xyz);

  vec2 sub = abs(posp - pos);
  float alpha = smoothstep(0.6, 0.5, length(sub) * 20 / hsv.z);

  vec3 lumidot = hsv2rgb(vec3(hsv.xy, alpha));

  fragColor = vec4(lumidot, 1);
}
