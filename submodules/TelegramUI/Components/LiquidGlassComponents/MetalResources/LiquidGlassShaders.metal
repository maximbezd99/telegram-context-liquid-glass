#include <metal_stdlib>
using namespace metal;

struct VertexOut {
    float4 position [[position]];
    float2 texCoord;
};

vertex VertexOut defaultVertex(
    uint vertexID [[vertex_id]],
    constant float4 *vertices [[buffer(0)]]
) {
    VertexOut out;
    float4 v = vertices[vertexID];
    out.position = float4(v.xy, 0.0, 1.0);
    out.texCoord = v.zw;
    return out;
}

float sdf(float2 p, float2 b, float r) {
    float2 d = abs(p) - b + float2(r);
    return min(max(d.x, d.y), 0.0) + length(max(d, 0.0)) - r;
}

half3 rgbToHsv(half3 c) {
    half4 K = half4(0.0, -1.0/3.0, 2.0/3.0, -1.0);
    half4 p = mix(half4(c.bg, K.wz), half4(c.gb, K.xy), step(c.b, c.g));
    half4 q = mix(half4(p.xyw, c.r), half4(c.r, p.yzx), step(p.x, c.r));
    half d = q.x - min(q.w, q.y);
    half e = 1.0e-10;
    return half3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

half3 hsvToRgb(half3 c) {
    half4 K = half4(1.0, 2.0/3.0, 1.0/3.0, 3.0);
    half3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

half3 shiftHueToward(half3 color, half alpha, half targetHue, float amount) {
    if (alpha == 0)
        return color;

    half3 hsv = rgbToHsv(color);

    // Only shift if there's saturation
    if (hsv.y > 0.01) {
        // Shortest path around the hue wheel
        half hueDiff = targetHue - hsv.x;
        if (hueDiff > 0.5) hueDiff -= 1.0;
        if (hueDiff < -0.5) hueDiff += 1.0;

        hsv.x += hueDiff * amount;
        hsv.x = fract(hsv.x);  // wrap 0-1
    }

    return hsvToRgb(hsv);
}

half3 shiftHSV(half3 color,
               half alpha,
               half3 targetHSV,      // (hue, saturation, value)
               float3 amounts) {     // (hueAmt, satAmt, valAmt)
    // Unpremultiply and clamp
    half3 rgb = alpha > 0.001h ? saturate(color / alpha) : half3(0.0h);
    half3 hsv = rgbToHsv(rgb);

    // Hue (with wrap, only if saturated)
    if (hsv.y > 0.01) {
        half hueDiff = targetHSV.x - hsv.x;
        if (hueDiff > 0.5) hueDiff -= 1.0;
        if (hueDiff < -0.5) hueDiff += 1.0;
        hsv.x = fract(hsv.x + hueDiff * amounts.x);
    }

    // Saturation
    hsv.y = mix(hsv.y, targetHSV.y, half(amounts.y));

    // Value
    hsv.z = mix(hsv.z, targetHSV.z, half(amounts.z));

    // Re-premultiply
    return hsvToRgb(hsv) * alpha;
}

struct MorphingUniforms {
    float shapeCount;
    float smoothness;
    float edgeWidth;
    float time;
    float tintOpacity;
    float highlightOpacity;
    float renderMask;
    float padding;
};

float smoothMin(float a, float b, float k) {
    if (k <= 0.0) return min(a, b);
    float h = max(k - abs(a - b), 0.0) / k;
    return min(a, b) - h * h * h * k * (1.0 / 6.0);
}

float morphingHighlightEffect(float dist, float shapeSize, float waveTime, float intensity) {
    if (intensity < 0.001) return 0.0;

    float maxRadius = shapeSize * 2;
    float waveRadius = maxRadius * waveTime;

    float edgeSoftness = shapeSize * 0.4;
    float fillAmount = 1.0 - smoothstep(waveRadius - edgeSoftness, waveRadius + edgeSoftness, dist);

    float centerFalloff = exp(-dist * dist / (shapeSize * shapeSize * 2.0));

    float result = fillAmount * (0.7 + 0.3 * centerFalloff);

    return result * intensity;
}

fragment float4 morphingFragment(
    VertexOut in [[stage_in]],
    constant MorphingUniforms &uniforms [[buffer(0)]],
    constant float *shapeData [[buffer(1)]],
    constant float2 &viewSize [[buffer(2)]]
) {
    int count = int(uniforms.shapeCount);
    if (count <= 0) return float4(0);

    float2 pos = in.texCoord * viewSize;
    float smoothness = uniforms.smoothness;
    float edgeWidth = uniforms.edgeWidth;

    float combined = 1e10;
    for (int i = 0; i < count && i < 64; i++) {
        int idx = i * 12;
        float2 center = float2(shapeData[idx], shapeData[idx + 1]);
        float2 halfSize = float2(shapeData[idx + 2], shapeData[idx + 3]);
        float radius = shapeData[idx + 4];
        float d = sdf(pos - center, halfSize, radius);
        combined = smoothMin(combined, d, smoothness);
    }

    if (combined > edgeWidth) return float4(0);

    float4 blended = float4(0);
    float totalW = 0.0;
    float highlight = 0.0;

    for (int i = 0; i < count && i < 64; i++) {
        int idx = i * 12;
        float2 center = float2(shapeData[idx], shapeData[idx + 1]);
        float2 halfSize = float2(shapeData[idx + 2], shapeData[idx + 3]);
        float radius = shapeData[idx + 4];
        float3 color = float3(shapeData[idx + 5], shapeData[idx + 6], shapeData[idx + 7]);
        float2 hlPos = float2(shapeData[idx + 8], shapeData[idx + 9]);
        float hlIntensity = shapeData[idx + 10];
        float waveTime = shapeData[idx + 11];

        float d = sdf(pos - center, halfSize, radius);
        float w = exp(-max(d, 0.0) / max(smoothness * 0.5, 0.001));
        blended.rgb += color * w;
        totalW += w;

        if (hlIntensity > 0.001) {
            float distHL = length(pos - hlPos);
            float shapeSize = halfSize.x + halfSize.y;
            float influence = exp(-max(d, 0.0) / max(smoothness * 0.3, 0.001));
            highlight += morphingHighlightEffect(distHL, shapeSize, waveTime, hlIntensity) * influence;
        }
    }

    blended.rgb /= totalW;

    float alpha = 1.0 - smoothstep(-edgeWidth, edgeWidth, combined);

    if (uniforms.renderMask > 0.5) {
        return float4(1.0, 1.0, 1.0, alpha);
    }

    float rimWidth = 3.0;
    float edgeDist = abs(combined);
    float edgeHighlight = exp(-edgeDist / rimWidth);

    float eps = 1.0;
    float2 gradientDir = float2(0.0);
    float gradientWeight = 0.0;

    for (int i = 0; i < count && i < 64; i++) {
        int idx = i * 12;
        float2 center = float2(shapeData[idx], shapeData[idx + 1]);
        float2 halfSize = float2(shapeData[idx + 2], shapeData[idx + 3]);
        float radius = shapeData[idx + 4];

        float d = sdf(pos - center, halfSize, radius);
        float w = exp(-max(d, 0.0) / max(smoothness * 0.3, 0.001));

        float dx = sdf(pos + float2(eps, 0) - center, halfSize, radius)
                 - sdf(pos - float2(eps, 0) - center, halfSize, radius);
        float dy = sdf(pos + float2(0, eps) - center, halfSize, radius)
                 - sdf(pos - float2(0, eps) - center, halfSize, radius);
        float2 grad = float2(dx, dy);
        float gradLen = length(grad);
        if (gradLen > 0.001) {
            gradientDir += (grad / gradLen) * w;
            gradientWeight += w;
        }
    }

    if (gradientWeight > 0.001) {
        gradientDir = normalize(gradientDir / gradientWeight);
    }

    float2 lightDirTL = normalize(float2(-1.0, -1.0));
    float2 lightDirBR = normalize(float2(1.0, 1.0));

    float facingTL = max(dot(gradientDir, lightDirTL), 0.0);
    float facingBR = max(dot(gradientDir, lightDirBR), 0.0);

    float cornerHighlight = pow(facingTL, 1.5) + pow(facingBR, 1.5);

    float rimAmount = edgeHighlight * (0.4 + cornerHighlight * 0.6);

    float tintOpacity = uniforms.tintOpacity;
    float highlightOpacity = uniforms.highlightOpacity;
    float hlAmount = min(highlight, 1.0);

    float3 tintColor = blended.rgb;
    float3 rimColor = float3(1.0);
    float3 highlightColor = mix(tintColor, float3(1.0), 0.6);

    float tintAlpha = tintOpacity;
    float rimFade = 1.0 - hlAmount * 0.5;
    float rimAlpha = rimAmount * 0.8 * rimFade;
    float touchAlpha = hlAmount * highlightOpacity;

    float3 finalColor = float3(0.0);
    finalColor += tintColor * tintAlpha;
    finalColor += rimColor * rimAlpha;
    finalColor += highlightColor * touchAlpha;

    float finalAlpha = min(tintAlpha + rimAlpha + touchAlpha, 1.0);

    finalAlpha *= alpha;
    finalColor *= alpha;

    return float4(finalColor, finalAlpha);
}

struct LensUniforms {
    float2 lensCenter;
    float2 lensRadii;
    float2 viewSize;
    float progress;
    float velocityPenalty;
    float minification;
    float magnification;
    float distortionEdgeV;
    float distortionEdgeH;
    float aberration;
    float blurIntensity;
    float4 tintedColorSource;
    float4 tintedColorTarget;
};

half4 tintedLayerSample(half4 sampledColor, float4 sourceTint, float4 targetTint) {
//    float tolerance = 0.002;
//    float3 diff = abs(float3(sampledColor.rgb) - sourceTint.rgb);
//
//    if (all(diff < float3(tolerance))) {
//        half a = sampledColor.a;
//        half3 tintRGB = half3(targetTint.rgb);
//        return half4(tintRGB * a, a);
//    }
//    return sampledColor;
    return sampledColor;
}

// MARK: - Blur

half4 sampleBlurred(texture2d<half> tex, sampler s, float2 coord, float radius, float2 texSize) {
    if (radius < 0.5) {
        return tex.sample(s, coord);
    }

    half4 color = half4(0);
    float totalWeight = 0;
    int kernelSize = min(int(ceil(radius)) + 1, 6);
    float sigma = max(radius, 0.01);
    float sigmaSquared2 = 2.0 * sigma * sigma;

    for (int x = -kernelSize; x <= kernelSize; x++) {
        for (int y = -kernelSize; y <= kernelSize; y++) {
            float2 offset = float2(x, y);
            float weight = exp(-dot(offset, offset) / sigmaSquared2);
            float2 sampleUV = coord + offset / texSize;
            color += tex.sample(s, sampleUV) * weight;
            totalWeight += weight;
        }
    }

    return color / totalWeight;
}

fragment half4 lensDistortionFragment(VertexOut in [[stage_in]],
                                       constant LensUniforms &u [[buffer(0)]],
                                       texture2d<half> snapshotTex [[texture(0)]]) {
    constexpr sampler texSampler(filter::linear, address::clamp_to_edge);

    float2 texSize = float2(snapshotTex.get_width(), snapshotTex.get_height());
    float2 uv = in.texCoord;
    float2 pos = uv * u.viewSize;
    float2 fromCenter = pos - u.lensCenter;
    float cornerRadius = u.lensRadii.y;
    float lensSize = min(u.lensRadii.x, u.lensRadii.y) * 2.0;

    if (lensSize < 0.001) return half4(0, 0, 0, 0);

    float sdfValue = sdf(fromCenter, u.lensRadii, cornerRadius);
    float inverseSDF = -sdfValue / lensSize;

    if (inverseSDF < 0.0) return half4(0, 0, 0, 0);

    // Gradient-based direction (more accurate edge normals)
    float eps = 1.0;
    float2 grad = float2(
        sdf(fromCenter + float2(eps, 0), u.lensRadii, cornerRadius) -
        sdf(fromCenter - float2(eps, 0), u.lensRadii, cornerRadius),
        sdf(fromCenter + float2(0, eps), u.lensRadii, cornerRadius) -
        sdf(fromCenter - float2(0, eps), u.lensRadii, cornerRadius)
    );
    float2 normalizedDir = length(grad) > 0.001 ? normalize(grad) : float2(1.0, 0.0);

    // Angle-based distortion edge
    float angle = atan2(fromCenter.y, fromCenter.x);
    float horizontalness = pow(cos(angle), 2.0);
    float distortionEdge = mix(u.distortionEdgeV, u.distortionEdgeH, horizontalness);

    // Edge distortion - spherical curve
    float distFromCenter = 1.0 - clamp(inverseSDF / max(distortionEdge, 0.001), 0.0, 1.0);
    float distortion = 1.0 - sqrt(1.0 - pow(distFromCenter, 2.0));

    // Calculate offset
    float2 distortOffset = distortion * normalizedDir * lensSize * 0.5;

    // Minification
    float minifyStrength = (1.0 - inverseSDF) * u.minification * u.progress;
    float2 minifyOffset = fromCenter * minifyStrength;

    // Magnification
    float magnifyStrength = u.magnification * u.progress;
    float2 magnifyOffset = fromCenter * magnifyStrength;

    // Final sample position
    float2 samplePos = pos - distortOffset + minifyOffset - magnifyOffset;

    // Blur
    float blurIntensity = mix(u.blurIntensity, 1.0, pow(max(u.progress, 0.001), 2.0));
    float blurEdge = mix(1.0, distortion, pow(max(u.progress, 0.001), 5.0));
    float blurRadius = blurIntensity * blurEdge;

    // Convert to UV and sample
    float2 sampleUV = clamp(samplePos / u.viewSize, float2(0.001), float2(0.999));
    half4 glassColor = sampleBlurred(snapshotTex, texSampler, sampleUV, blurRadius, texSize);

    // Apply tint
    glassColor = tintedLayerSample(glassColor, u.tintedColorSource, u.tintedColorTarget);

    // White glow at edge
    float glowWidth = 0.02;
    float glowIntensity = 0.03;
    float glowFactor = smoothstep(glowWidth, 0.0, inverseSDF);
    glowFactor = pow(glowFactor, 3.0);
    glassColor.rgb += half3(1.0) * half(glowFactor * glowIntensity);

    // Hue shift at edges (cyan/yellow pattern)
    float edgeFactor = smoothstep(0.15, 0.0, inverseSDF) * 0.5;
    float2 dir = fromCenter / u.lensRadii;
    float pattern = sin(dir.x * M_PI_F) * sin(dir.y * M_PI_F);
    half cyan = 0.5h;
    half yellow = 0.16h;
    float t = pattern * 0.7 + 0.5;
    half targetHue = mix(yellow, cyan, half(t));
    glassColor.rgb = shiftHueToward(glassColor.rgb, glassColor.a, targetHue, edgeFactor);

    // Top shadow
    float topOnlySDF = max(sdfValue, -fromCenter.y);
    float softTopOnlySDF = 1.0 - smoothstep(u.lensRadii.y, 0.0, topOnlySDF);
    half shadowAlpha = half(softTopOnlySDF * 0.08 * u.progress);
    glassColor.rgb *= (1.0h - shadowAlpha);
    glassColor.a = glassColor.a + shadowAlpha * (1.0h - glassColor.a);


    float rimWidth = 4.0;
    float edgeDist = abs(sdfValue);
    float rimHighlight = exp(-edgeDist / rimWidth);

    // Dual corner highlights (top-left and bottom-right facing)
    float2 lightDirTL = normalize(float2(-1.0, -1.0));
    float2 lightDirBR = normalize(float2(1.0, 1.0));
    float facingTL = max(dot(normalizedDir, lightDirTL), 0.0);
    float facingBR = max(dot(normalizedDir, lightDirBR), 0.0);
    float cornerHighlight = pow(facingTL, 1.5) + pow(facingBR, 1.5);

    // Rim amount with corner emphasis
    float rimAmount = rimHighlight * (0.05 + cornerHighlight * 0.15);

    // Add white rim
    half3 rimColor = half3(1.0);
    half rimAlpha = half(rimAmount * 0.8 * u.progress);
    glassColor.rgb += rimColor * rimAlpha;
    glassColor.a = saturate(glassColor.a + rimAlpha * 0.5h);

    // Purple tint highlight (top-left edge)
    float2 lightDir = normalize(float2(1.0, 1.0));
    float edgeHighlight = dot(normalizedDir, lightDir);
    edgeHighlight = saturate(-edgeHighlight);
    float edgeMask = smoothstep(0.1, 0.0, inverseSDF);
    float highlightStrength = pow(edgeHighlight * edgeMask, 2.0);
    half3 purpleTint = half3(0.5h, 0.2h, 0.4h);
    half highlightIntensity = half(highlightStrength * 0.3 * u.progress);
    glassColor.rgb = mix(glassColor.rgb, glassColor.rgb * purpleTint, highlightIntensity);
    glassColor.a = saturate(glassColor.a + highlightIntensity * 0.3h);

    return glassColor;
}
