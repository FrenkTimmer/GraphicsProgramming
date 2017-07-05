//***************************************************************************************
// color.fx by Frank Luna (C) 2011 All Rights Reserved.
//
// Transforms and colors geometry.
//***************************************************************************************
#define iResolution float2(400,300)

cbuffer cbPerObject
{
	float gMichaelTime;
	float4x4 gWorldViewProj; 
};

struct VertexIn
{
	float3 Pos   : POSITION;
	float4 Color : COLOR;
};

struct VertexOut
{
	float4 PosH  : SV_POSITION;
    float4 Color : COLOR;
};

VertexOut VS(VertexIn vin)
{
	VertexOut vout;
	
	// Transform to homogeneous clip space.
	vout.PosH = mul(float4(vin.Pos, 1.0f), gWorldViewProj);
	
	// Just pass vertex color into the pixel shader.
    vout.Color = vin.Color;
    
    return vout;
}

#define saw(x) (acos(cos(x))/PI)
#define PI 3.14159265359
#define E 2.7182818284
#define GR 1.61803398875
#define EPS (2.0/max(iResolution.x, iResolution.y))

#define time ((saw(float(__LINE__))+.5)*(gMichaelTime/PI+12345.12345))
#define saw(x) (acos(cos(x))/PI)
#define sphereN(uv) (normalize(float3((uv).xy, sqrt(clamp(1.0-length((uv)), 0.0, 1.0)))))
#define rotatePoint(p,n,theta) (p*cos(theta)+cross(n,p)*sin(theta)+n*dot(p,n) *(1.0-cos(theta)))

float map(float3 p)
{
	float3 q = frac(p * 0.5) * 2.0 - 1.0;
	q.y = q.x * 0.5;

	return length(q) - 0.3;
}

float trace(float3 origin, float3 ray)
{
	float t = 0.0;
	for (int i = 0; i < 18; i++) {
		float3 p = origin + ray * t;
		float d = map(p);
		t += d * 0.5;
	}
	return t;
}

float4 PS(float4 fragCoord:sv_position) : SV_Target
{
	float2 uv = fragCoord.xy / iResolution.xy;
	uv = uv * 2.0 - 1.0;
	uv.x = 1.0 - uv.x;
	uv.y = 1.0 - uv.y;

	// Aspect ratio.
	uv.x *= iResolution.x / iResolution.y;

	// RGB
	float3 c;

	float s1 = sin(time * 0.5);

	// Compute RGB separately.
	for (int i = 0; i < 3; i++) {

		// Move origin.
		float3 origin = float3(0.0, 0.0, time);

		// Some kind of chromatic aberration.
		uv.x *= 0.97;
		uv.y *= 0.97;

		float3 ray = normalize(float3(uv, 0.5));

		// Spiral rotation (XY).
		float the = time + length(uv) * s1;
		ray.xy = mul(ray.xy, float2x2(cos(the), -sin(the), sin(the), cos(the)));

		// Normal rotation (XZ).
		the = time * 0.1;
		ray.xz = mul(ray.xz, float2x2(cos(the), -sin(the), sin(the), cos(the)));

		float t = trace(origin, ray);

		// Visualize depth.
		c[i] = 1.0 / (1.0 + t * t * 0.07);
	}

	return float4 (c, 1.0);
}

technique11 ColorTech
{
    pass P0
    {
        SetVertexShader( CompileShader( vs_5_0, VS() ) );
		SetGeometryShader( NULL );
        SetPixelShader( CompileShader( ps_5_0, PS() ) );
    }
}

