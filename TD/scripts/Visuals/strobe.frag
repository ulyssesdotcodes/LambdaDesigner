uniform float uTime;
uniform float uSpeed;

out vec4 fragColor;

void main() {
  fragColor = texture(sTD2DInputs[0], vUV.st) * (step(0.5, 1 - fract(uTime * uSpeed)));
}
