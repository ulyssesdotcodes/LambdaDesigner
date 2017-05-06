uniform vec2 uTranslate;
uniform vec2 uScale;
uniform int uClones;

out vec4 fragColor;

void main()
{
  vec2 uv = vUV.st;
  vec4 color = texture2D(sTD2DInputs[0], vUV.st);

  for(int i = 0; i < uClones; ++i) {
    uv = (uv - vec2(0.5)) * (1 - uScale) + vec2(0.5) + uTranslate;
    color += (1 - color.w) * texture2D(sTD2DInputs[0], uv);
  }

  fragColor = color;
}
