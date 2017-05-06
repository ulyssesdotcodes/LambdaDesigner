uniform float uOpacity;

out vec4 fragColor;
void main()
{
    vec4 color = texture(sTD2DInputs[0], vUV.st);
    color += uOpacity * (1 - color.w) * texture(sTD2DInputs[1], vUV.st);
    fragColor = TDOutputSwizzle(color);
}
