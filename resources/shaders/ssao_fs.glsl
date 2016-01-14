#version 330

in vec3 v_normal;
in vec3 v_position;

uniform ivec4 u_viewport;
uniform mat4 u_proj;
uniform mat4 u_projViewInv;
uniform mat4 u_view;
uniform sampler2D rotations;
uniform sampler2D u_shadowMap;
uniform vec4 kernel[16];
uniform vec4 uRadius;

const int KERNEL_SIZE = 16;

const float NEAR = 1.0f;
const float FAR = 100.0f;

float nonlinearDepth(float depth)
{
  float a = 2.0 * NEAR * FAR;
  float b = FAR + NEAR;
  float c = FAR - NEAR;
  depth += 0.00000001;
  return (b * depth - a) / (c * depth);
}

vec3 viewPosition(vec2 texCoord, float depth) {
  // TODO Stupid multiplications here
  vec4 d = u_view * u_projViewInv * vec4(texCoord.xy * 2 - 1, nonlinearDepth(depth), 1);
  return d.xyz / d.w;
}

void main() {
  // TODO Hardcoding scaling factor. We really should be able to calculate this in the vertex shader
  vec2 texCoord = gl_FragCoord.xy / u_viewport.zw;
  vec3 origin = v_position.xyz;
  vec3 normal = normalize(v_normal);
  vec3 rvec = texture(rotations, texCoord * vec2(1024 / 4)).rgb;
  vec3 tangent = normalize(rvec - normal * dot(rvec, normal));
  vec3 bitangent = cross(normal, tangent);
  mat3 tbn = mat3(tangent, bitangent, normal);

  float occlusion = 0.0;
  float sampleDepth;
  vec4 offset;

  // TODO This should not require a texture lookup
  float depthHere = texture2D(u_shadowMap, texCoord.xy).x;
  float delta;

  // TODO The units here are arbitrary. Instead, this should correspond to units.
  // Problem is when we calculate rangeCheck we need to know the world (or view space)
  // position of origin and the end of our sampling ray's *result*. This will need reconstruction
  // of position.
  float r = 3.0f;

  for (int i = 0; i < KERNEL_SIZE; i++) {
    // TODO What is going on with this mod operation?
    vec3 sampleRay = tbn * kernel[int(mod(i,16))].xyz;
    sampleRay = origin + sampleRay * r;

    offset = vec4(sampleRay.xyz, 1.0);
    offset = u_proj * offset;
    offset.xy /= offset.w;
    offset.xy = offset.xy * 0.5 + 0.5;

    sampleDepth = texture2D(u_shadowMap, offset.xy).r;
    vec3 viewPos = viewPosition(offset.xy, sampleDepth);

    float rangeCheck = length(origin - viewPos) < r ? 1.0 : 0.0;
    occlusion += (sampleDepth <= depthHere ? 1.0 : 0.0) * rangeCheck;
  }

  occlusion = 1.0f - (occlusion / float(KERNEL_SIZE));

  gl_FragColor = vec4(vec3(occlusion), 1.0f);
}
