uniform float i_repeat;

out vec4 fragColor;

void main() {
  vec2 ipos = floor((vUV.st - 0.5) * i_repeat + 0.5);
  float i = (ipos.x * 2 + ipos.y) * 1;
  vec2 pos = mod((vUV.st - vec2(0.5)) * i_repeat + vec2(0.5), 1);
  vec4 b = texture(sTD2DInputs[0], pos);
  vec4 colorMap = texture(sTD2DInputs[1], vec2(mod(i, 5) * 0.2, 0.5));

  fragColor = b * colorMap;
}
