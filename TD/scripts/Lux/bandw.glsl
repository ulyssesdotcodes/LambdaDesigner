
out vec4 fragColor;

void main()
{
    // vec4 color = texture(sTD2DInputs[0], vUV.st);
    //vec4 color = vec4(1.0);
    //fragColor = TDOutputSwizzle(color);
    vec4 Color = texture2D( sTD2DInputs[0], vUV.st );
    vec3 lum = vec3(0.299, 0.587, 0.114);
    fragColor = vec4( vec3(dot( Color.rgb, lum)), Color.a);
}