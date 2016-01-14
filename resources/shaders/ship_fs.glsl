#version 330

in vec2 v_uv;

uniform sampler2D ssao;
uniform sampler2D diffuseMap;
uniform ivec4 u_viewport;

void main()
{
  // TODO Hardcoding scaling factor. We really should be able to calculate this in the vertex shader
  vec2 texCoord = gl_FragCoord.xy * vec2(1.0f / u_viewport.z, 1.0f / u_viewport.w);
  float occlusion = texture(ssao, texCoord).r;
  vec3 diffuse = texture(diffuseMap, v_uv).rgb;
  gl_FragColor = vec4(diffuse * occlusion, 1);
}
