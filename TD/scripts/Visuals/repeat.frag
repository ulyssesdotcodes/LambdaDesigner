uniform float i_repeat;

out vec4 fragColor;

void main() {
  vec2 i;
  vec2 pos = mod((vUV.st - vec2(0.5)) * i_repeat + vec2(0.5), 1);
  vec4 b = texture(sTD2DInputs[0], pos);

  fragColor = b;
}
