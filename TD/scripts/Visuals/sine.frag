uniform float i_time;
uniform float i_scale;
uniform float i_amplitude;

out vec4 fragColor;

void main() {
  float scale = i_scale + 1.0f; 
  float amplitude = i_amplitude + 1.0f; 
  vec2 pos = 2 * (vUV.st - vec2(0.5));

  pos = pos * scale;

  float y = sin(pos.x + i_time * 3.1415 * 2.0) * amplitude;

  float color = max(0, (0.1 - (y - pos.y) * (y - pos.y)) * 10);

  fragColor = vec4(color);
}
