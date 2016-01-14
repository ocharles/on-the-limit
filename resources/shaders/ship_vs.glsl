#version 330

in vec3 a_position;
in vec3 a_normal;
in vec2 a_uv;

out vec2 v_uv;

uniform mat4 u_projViewModel;

void main() {
    gl_Position = u_projViewModel * vec4(a_position, 1);
    v_uv = a_uv;
}
