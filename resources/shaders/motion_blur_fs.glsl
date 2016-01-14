#version 330

uniform sampler2D uInputTex;
uniform sampler2D uDepth;

uniform mat4 u_projViewInv;
uniform mat4 u_projView;
uniform mat4 u_proj;
uniform mat4 u_projView_previous;

in vec2 v_texCoord;

out vec3 fResult;

// TODO Common uniforms
const float NEAR = 1.0f;
const float FAR = 100.0f;

const int NUM_SAMPLES = 16;

float nonlinearDepth(float depth)
{
  float a = 2.0 * NEAR * FAR;
  float b = FAR + NEAR;
  float c = FAR - NEAR;
  depth += 0.00000001;
  return (b * depth - a) / (c * depth);
}

void main() {
  // Get the depth buffer value at this pixel.
  float zOverW = nonlinearDepth(texture(uDepth, v_texCoord).r);

  // h is the viewport position at this pixel in the range -1 to 1.
  vec4 h = vec4(v_texCoord.xy * 2 - 1, zOverW, 1);

  // Transform by the view-projection inverse.
  vec4 d = u_projViewInv * h;
  // Divide by w to get the world position.
  vec4 worldPos = d; // / d.w;

  // Use the world position, and transform by the previous view-projection matrix.
  vec4 previousPos = u_projView_previous * worldPos;
  // Convert to nonhomogeneous points [-1,1] by dividing by w.
  previousPos /= previousPos.w;
  // Use this frame's position and last frame's to compute the pixel velocity.
  vec4 currentPos = h; // Current viewport position
  vec2 velocity = (currentPos.xy - previousPos.xy) / float(NUM_SAMPLES);

  vec3 color = vec3(0);
  vec2 texCoord = v_texCoord;
  int samplesTaken = 1;
  for(int i = 0; i < NUM_SAMPLES; i++, texCoord += velocity, samplesTaken++) {
    if (texCoord.x >= 0 && texCoord.x <= 1 && texCoord.y >= 0 && texCoord.y <= 1) {
      color += texture(uInputTex, texCoord).rgb;
    } else {
      break;
    }
  }
  fResult = color / samplesTaken;
}
