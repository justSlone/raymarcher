Shader "Hidden/NewImageEffectShader"
{
	Properties
	{
		_MainTex ("Texture", 2D) = "white" {}
	}
	SubShader
	{
		// No culling or depth
		Cull Off ZWrite Off ZTest Always

		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			
			#include "UnityCG.cginc"

			// Provided by our script
			uniform float4x4 _FrustumCornersES;
			uniform sampler2D _MainTex;
			uniform float4 _MainTex_TexelSize;
			uniform float4x4 _CameraInvViewMatrix;
			uniform float3 _CameraWS;
			uniform float3 _LightDir;
			uniform float4x4 _MatTorus_InvModel;
			uniform sampler2D _ColorRamp;


			// Input to vertex shader
			struct appdata
			{
				// Remember, the z value here contains the index of _FrustumCornersES to use
				float4 vertex : POSITION;
				float2 uv : TEXCOORD0;
			};

			// Output of vertex shader / input to fragment shader
			struct v2f
			{
				float4 pos : SV_POSITION;
				float2 uv : TEXCOORD0;
				float3 ray : TEXCOORD1;
			};

			v2f vert(appdata v)
			{
				v2f o;

				// Index passed via custom blit function in RaymarchGeneric.cs
				half index = v.vertex.z;
				v.vertex.z = 0.1;

				o.pos = mul(UNITY_MATRIX_MVP, v.vertex);
				o.uv = v.uv.xy;

#if UNITY_UV_STARTS_AT_TOP
				if (_MainTex_TexelSize.y < 0)
					o.uv.y = 1 - o.uv.y;
#endif

				// Get the eyespace view ray (normalized)
				o.ray = _FrustumCornersES[(int)index].xyz;

				// Transform the ray from eyespace to worldspace
				// Note: _CameraInvViewMatrix was provided by the script
				o.ray = mul(_CameraInvViewMatrix, o.ray);
				return o;
			}

			// Torus
			// t.x: diameter
			// t.y: thickness
			// Adapted from: http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
			float sdTorus(float3 p, float2 t)
			{
				float2 q = float2(length(p.xz) - t.x, p.y);
				return length(q) - t.y;
			}

			// This is the distance field function.  The distance field represents the closest distance to the surface
			// of any object we put in the scene.  If the given point (point p) is inside of an object, we return a
			// negative answer.
			//float map(float3 p) {
			//	return sdTorus(p, float2(1, 0.2));
			//}

			// Animated torus
			//float map(float3 p) {
			//	float4 q = mul(_MatTorus_InvModel, float4(p, 1));

			//	return sdTorus(q.xyz, float2(1, 0.2));
			//}

			// Box
			// b: size of box in x/y/z
			// Adapted from: http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
			float sdBox(float3 p, float3 b)
			{
				float3 d = abs(p) - b;
				return min(max(d.x, max(d.y, d.z)), 0.0) +
					length(max(d, 0.0));
			}

			float sdSphere(float3 p, float s)
			{
				return length(p) - s;
			}

			// Union
			// Adapted from: http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
			float opU(float d1, float d2)
			{
				return min(d1, d2);
			}

			// Subtraction
			// Adapted from: http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
			float opS(float d1, float d2)
			{
				return max(-d1, d2);
			}

			// Intersection
			// Adapted from: http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
			float opI(float d1, float d2)
			{
				return max(d1, d2);
			}

			// Union (with material data)
			float2 opU(float2 d1, float2 d2)
			{
				return (d1.x < d2.x) ? d1 : d2;
			}

			// Notice how map() now returns a float2
			// \return.x: Distance field value
			// \return.y: Color of closest object (0 - 1)
			float2 map(float3 p) {
				float2 d_torus = float2(sdTorus(p, float2(1, 0.2)), 0.5);
				float2 d_box = float2(sdBox(p - float3(-3, 0, 0), float3(0.75, 0.5, 0.5)), 0.25);
				float2 d_sphere = float2(sdSphere(p - float3(3, 0, 0), 1), 0.75);

				float2 ret = opU(d_torus, d_box);
				ret = opU(ret, d_sphere);

				return ret;
			}

			/*float map(float3 p) {
				float union_box = opU(
					sdBox(p - float3(-4.5, 0.5, 0), float3(1, 1, 1)),
					sdBox(p - float3(-3.5, -0.5, 0), float3(1, 1, 1))
				);
				float subtr_box = opS(
					sdBox(p - float3(-0.5, 0.5, 0), float3(1, 1, 1.01)),
					sdBox(p - float3(0.5, -0.5, 0), float3(1, 1, 1))
				);
				float insec_box = opI(
					sdBox(p - float3(3.5, 0.5, 0), float3(1, 1, 1)),
					sdBox(p - float3(4.5, -0.5, 0), float3(1, 1, 1))
				);

				float ret = opU(union_box, subtr_box);
				ret = opU(ret, insec_box);

				return ret;
			}*/

			float3 calcNormal(in float3 pos)
			{
				// epsilon - used to approximate dx when taking the derivative
				const float2 eps = float2(0.001, 0.0);

				// The idea here is to find the "gradient" of the distance field at pos
				// Remember, the distance field is not boolean - even if you are inside an object
				// the number is negative, so this calculation still works.
				// Essentially you are approximating the derivative of the distance field at this point.
				float3 nor = float3(
					map(pos + eps.xyy).x - map(pos - eps.xyy).x,
					map(pos + eps.yxy).x - map(pos - eps.yxy).x,
					map(pos + eps.yyx).x - map(pos - eps.yyx).x);
				return normalize(nor);
			}



			// Raymarch along given ray
			// ro: ray origin
			// rd: ray direction
			// s: unity depth buffer
			fixed4 raymarch(float3 ro, float3 rd, float s) {
				fixed4 ret = fixed4(0, 0, 0, 0);

				const int maxstep = 64;
				const float drawdist = 40; // draw distance in unity units

				float t = 0; // current distance traveled along ray
				for (int i = 0; i < maxstep; ++i) {
					// If we run past the depth buffer, stop and return nothing (transparent pixel)
					// this way raymarched objects and traditional meshes can coexist.
					if (t >= s || t > drawdist) { // check draw distance in additon to depth
						ret = fixed4(0, 0, 0, 0);
						break;
					}

					float3 p = ro + rd * t; // World space position of sample
					float2 d = map(p);      // Sample of distance field (see map())
											// d.x: distance field output
											// d.y: material data

					// Old lighting
					//if (d < 0.001) {
					//	// Lambertian Lighting
					//	float3 n = calcNormal(p);
					//	ret = fixed4(dot(-_LightDir.xyz, n).rrr, 1);
					//	break;
					//}

					// If the sample <= 0, we have hit something (see map()).
					if (d.x < 0.001) {
						float3 n = calcNormal(p);
						float light = dot(-_LightDir.xyz, n);
						// Use y value given by map() to choose a color from our Color Ramp
						ret = fixed4(tex2D(_ColorRamp, float2(d.y, 0)).xyz * light, 1);
						break;
					}



					// If the sample > 0, we haven't hit anything yet so we should march forward
					// We step forward by distance d, because d is the minimum distance possible to intersect
					// an object (see map()).
					t += d;
				}
				return ret;
			}

			//fixed4 raymarch(float3 ro, float3 rd, float s) {
			//	const int maxstep = 64;
			//	const float drawdist = 40; // draw distance in unity units

			//	float t = 0; // current distance traveled along ray

			//	for (int i = 0; i < maxstep; ++i) {
			//		float3 p = ro + rd * t; // World space position of sample
			//		float2 d = map(p);      // Sample of distance field (see map())

			//								// If the sample <= 0, we have hit something (see map()).
			//		if (d.x < 0.001 || t > drawdist) {
			//			// Simply return the number of steps taken, mapped to a color ramp.
			//			float perf = (float)i / maxstep;
			//			return fixed4(tex2D(_ColorRamp, float2(perf, 0)).xyz, 1);
			//		}

			//		t += d;
			//	}

			//	// By this point the loop guard (i < maxstep) is false.  Therefore
			//	// we have reached maxstep steps.
			//	return fixed4(tex2D(_ColorRamp, float2(1, 0)).xyz, 1);
			//}

			uniform sampler2D _CameraDepthTexture;


			fixed4 frag(v2f i) : SV_Target
			{

				// ray direction
				float3 rd = normalize(i.ray.xyz);
				// ray origin (camera position)
				float3 ro = _CameraWS;

				float2 duv = i.uv;
				#if UNITY_UV_STARTS_AT_TOP
					if (_MainTex_TexelSize.y < 0)
						duv.y = 1 - duv.y;
				#endif
				
				// Convert from depth buffer (eye space) to true distance from camera
				// This is done by multiplying the eyespace depth by the length of the "z-normalized"
				// ray (see vert()).  Think of similar triangles: the view-space z-distance between a point
				// and the camera is proportional to the absolute distance.
				float depth = LinearEyeDepth(tex2D(_CameraDepthTexture, duv).r);
				depth *= length(i.ray.xyz);

				// Used to debug depth buffer
				//return depth /= 255;

				fixed3 col = tex2D(_MainTex, i.uv);
				fixed4 add = raymarch(ro, rd, depth);

				// Returns final color using alpha blending
			    return fixed4(col*(1.0 - add.w) + add.xyz * add.w,1.0);

				// Used to see basic rays
				//fixed4 col = fixed4(i.ray, 1);
				//return col;
			}
			ENDCG
		}
	}
}
