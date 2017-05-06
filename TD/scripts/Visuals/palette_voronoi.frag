out vec4 fragColor;

uniform float uOffset;

void main()
{
  vec4 tex = texture(sTD2DInputs[0], vUV.st);

  vec3 col = texture(sTD2DInputs[1], vec2(mod((tex.r + tex.g + tex.b) * 0.33 + uOffset, 1), 0.5)).xyz;

  fragColor = vec4( col * floor(min(1, tex.w * 100)), tex.w );
}
