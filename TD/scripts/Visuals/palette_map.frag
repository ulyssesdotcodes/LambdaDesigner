out vec4 fragColor;

uniform float uOffset;

void main()
{
  vec4 tex = texture(sTD2DInputs[0], vUV.st);

  vec4 col = vec4(texture(sTD2DInputs[1], vec2(mod((tex.r + tex.g + tex.b) * 0.33 + uOffset, 1), 0.5)).xyz * tex.w, tex.w);

  fragColor = TDOutputSwizzle(col);
}
