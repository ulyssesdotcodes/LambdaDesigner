uniform float i_volume;

out vec4 fragColor;

void main() {
	vec2 audio = texture(sTD2DInputs[0], vec2(vUV.x, 0.25)).xy;

	float audX = audio.y * i_volume + 0.5;

	float clamped = clamp(20 * (0.05 - abs(vUV.y - audX)), 0, 1);

	fragColor = vec4(clamped);
}
