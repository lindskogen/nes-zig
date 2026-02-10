#version 330 core
in vec2 TexCoord;
out vec4 FragColor;

uniform sampler2D nesTexture;
uniform vec2 u_resolution;
uniform vec2 u_texture_size;

// --- Barrel Distortion ---
const float DISTORTION = 0.06;

vec2 barrelDistort(vec2 uv) {
    vec2 centered = uv - 0.5;
    float r2 = dot(centered, centered);
    centered *= 1.0 + DISTORTION * r2;
    return centered + 0.5;
}

// --- CRT Effects ---
const float SCANLINE_STRENGTH = 0.18;
const float CHROMATIC_OFFSET = 0.0008;
const float GLOW_STRENGTH = 0.04;
const float VIGNETTE_STRENGTH = 0.3;
const float MASK_STRENGTH = 0.08;

void main() {
    vec2 uv = barrelDistort(TexCoord);

    // Discard pixels outside [0,1] after distortion
    if (uv.x < 0.0 || uv.x > 1.0 || uv.y < 0.0 || uv.y > 1.0) {
        FragColor = vec4(0.0, 0.0, 0.0, 1.0);
        return;
    }

    // --- Chromatic Aberration ---
    float r = texture(nesTexture, vec2(uv.x + CHROMATIC_OFFSET, uv.y)).r;
    float g = texture(nesTexture, uv).g;
    float b = texture(nesTexture, vec2(uv.x - CHROMATIC_OFFSET, uv.y)).b;
    vec3 color = vec3(r, g, b);

    // --- Phosphor Glow (simple 4-tap box blur blended in) ---
    vec2 texel = 1.0 / u_texture_size;
    vec3 glow = texture(nesTexture, uv + vec2( texel.x, 0.0)).rgb
              + texture(nesTexture, uv + vec2(-texel.x, 0.0)).rgb
              + texture(nesTexture, uv + vec2(0.0,  texel.y)).rgb
              + texture(nesTexture, uv + vec2(0.0, -texel.y)).rgb;
    glow *= 0.25;
    color = mix(color, glow, GLOW_STRENGTH);

    // --- Scanlines ---
    float scanline = sin(uv.y * u_texture_size.y * 3.14159265) * 0.5 + 0.5;
    color *= 1.0 - SCANLINE_STRENGTH * (1.0 - scanline);

    // --- Shadow Mask (RGB phosphor pattern) ---
    int px = int(gl_FragCoord.x) % 3;
    vec3 mask = vec3(1.0);
    if (px == 0) mask = vec3(1.0, 1.0 - MASK_STRENGTH, 1.0 - MASK_STRENGTH);
    else if (px == 1) mask = vec3(1.0 - MASK_STRENGTH, 1.0, 1.0 - MASK_STRENGTH);
    else mask = vec3(1.0 - MASK_STRENGTH, 1.0 - MASK_STRENGTH, 1.0);
    color *= mask;

    // --- Vignette ---
    vec2 vigUV = TexCoord - 0.5;
    float vignette = 1.0 - dot(vigUV, vigUV) * VIGNETTE_STRENGTH * 4.0;
    color *= clamp(vignette, 0.0, 1.0);

    FragColor = vec4(color, 1.0);
}
