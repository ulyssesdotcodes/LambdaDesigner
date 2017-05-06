out vec4 fragColor;

void main()
{
  vec4 mask = texture(sTD2DInputs[0], vUV.st);
  vec4 A = texture(sTD2DInputs[1], vUV.st);
  vec4 B = texture(sTD2DInputs[2], vUV.st);
  fragColor = mask.r * A + (1 - mask.r) * B;
}