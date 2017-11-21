#include <malloc.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <cpuid.h>
#include <string.h>
#include <mmintrin.h>
#include <emmintrin.h>
#include <pmmintrin.h>
#include <immintrin.h>

//https://software.intel.com/sites/landingpage/IntrinsicsGuide

void arrays_equal(int* a, int* b, int n){     
     for(int i = 0; i<n; i++){
	  if(a[i] != b[i]) {
	       printf("%d != %d at %d\n ", a[i], b[i], i);
	       return;
	  }	  
     }
     printf("arrays equal\n");	 
}

void task1(){
     printf("TASK 1\n");
     
     int eax, ebx, ecx, edx;
     char vendor[13];
     __cpuid (0, eax, ebx, ecx, edx);
     memcpy(vendor    , &ebx, 4);
     memcpy(vendor + 4, &edx, 4);
     memcpy(vendor + 8, &ecx, 4);
     vendor[12] = '\0';
     printf("Vendor  = %s\n", vendor);

     __cpuid (1, eax, ebx, ecx, edx);

     printf("HYPER   = %d\n", (edx >> 28) & 1);

     printf("MMX     = %d\n", (edx >> 23) & 1);
     
     printf("SSE     = %d\n", (edx >> 25) & 1);
     printf("SSE2    = %d\n", (edx >> 26) & 1);
     printf("SSE3    = %d\n", (ecx >> 0) & 1);
     printf("SSSE3   = %d\n", (ecx >> 9) & 1);
     printf("SSE4.1  = %d\n", (ecx >> 19) & 1);
     printf("SSE4.2  = %d\n", (ecx >> 20) & 1);         

     printf("AVX_1   = %d\n", (ecx >> 26) & 1);
     printf("AVX_2   = %d\n", (ecx >> 27) & 1);
     printf("AVX_3   = %d\n", (ecx >> 28) & 1);

     __cpuid (0x80000000, eax, ebx, ecx, edx);
     
     printf("3DNow_1 = %d\n", (edx >> 31) & 1);
     printf("3DNow_2 = %d\n", (edx >> 30) & 1);

     __cpuid (0x80000001, eax, ebx, ecx, edx);
     
     printf("SSE4A   = %d\n", (ecx >> 6) & 1);
     printf("SSE5    = %d\n", (ecx >> 11) & 1);
     
     char full[48];
     for(int i = 0; i < 3; i++){
	  __cpuid (0x80000002 + i, eax, ebx, ecx, edx);
	  memcpy(full + 16 * i,      &eax, 4);
	  memcpy(full + 16 * i + 4,  &ebx, 4);
	  memcpy(full + 16 * i + 8,  &ecx, 4);
	  memcpy(full + 16 * i + 12, &edx, 4);
     }     
     full[47] = '\0';
     printf("Full    = %s\n", full);
}

void sum_mmx_8(
     int8_t* a,
     int8_t* b,
     int8_t* c,
     size_t s){
     __m64* a_mmx = (__m64*)a;
     __m64* b_mmx = (__m64*)b;
     __m64* c_mmx = (__m64*)c;    
     
     for (int i = 0; i < s / sizeof(__m64); i++){
	  c_mmx[i] = _mm_add_pi8(a_mmx[i], b_mmx[i]);
     }
     c = (int8_t*)c_mmx;
}

void task2_i8(int8_t* a, int8_t* b, int8_t* c, int n){
     for(int i = 0; i < n; i++)
	  c[i] = abs(a[i]) + abs(b[i]);
}

void task2_i8_avx(int8_t* a, int8_t* b, int8_t* c, int n){
     __m256i* pa = (__m256i*) a;
     __m256i* pb = (__m256i*) b;
     __m256i* pc = (__m256i*) c;

     for(int i = 0; i < n / 32; i++)
	  pc[i] = _mm256_add_epi8(_mm256_abs_epi8(pa[i]), _mm256_abs_epi8(pb[i]));
     
}

void task2_i16(int16_t* a, int16_t* b, int16_t* c, int n){
     for(int i = 0; i < n; i++)
	  c[i] = abs(a[i]) + abs(b[i]);
}

void task2_i16_avx(int16_t* a, int16_t* b, int16_t* c, int n){
     __m256i* pa = (__m256i*) a;
     __m256i* pb = (__m256i*) b;
     __m256i* pc = (__m256i*) c;
     for(int i = 0; i < n / 16; i++)
	  pc[i] = _mm256_add_epi16(_mm256_abs_epi16(pa[i]), _mm256_abs_epi16(pb[i]));
}

void task2_i32(int32_t* a, int32_t* b, int32_t* c, int n){
     for(int i = 0; i < n; i++)
	  c[i] = abs(a[i]) + abs(b[i]);
}

void task2_i32_avx(int32_t* a, int32_t* b, int32_t* c, int n){
     __m256i* pa = (__m256i*) a;
     __m256i* pb = (__m256i*) b;
     __m256i* pc = (__m256i*) c;
     for(int i = 0; i < n / 8; i++)
	  pc[i] = _mm256_add_epi32(_mm256_abs_epi32(pa[i]), _mm256_abs_epi32(pb[i]));
}

void task2_i64(int64_t* a, int64_t* b, int64_t* c, int n){
     for(int i = 0; i < n; i++)
	  c[i] = abs(a[i]) + abs(b[i]);
}
/*
void task2_i64_avx(int64_t* a, int64_t* b, int64_t* c, int n){
     __m256i* pa = (__m256i*) a;
     __m256i* pb = (__m256i*) b;
     __m256i* pc = (__m256i*) c;
     for(int i = 0; i < n / 4; i++)
	  pc[i] = _mm256_add_epi64(pa[i], pb[i]);
}
*/

inline __m128 _mm_abs_ps(__m128 x) {
    const __m128 mask = _mm_set1_ps(-0.f); // -0.f = 1 << 31
    return _mm_andnot_ps(mask, x);
}

void task2_f(float* a, float* b, float* c, int n){
     for(int i = 0; i < n; i++)
	  c[i] = fabsf(a[i]) + fabsf(b[i]);
}

void task2_f_sse(float* a, float* b, float* c, int n){
     __m128 *pa = (__m128*) a;
     __m128 *pb = (__m128*) b;
     __m128 *pc = (__m128*) c;

     for (int i = 0; i < n / 4; i++){
	  pc[i] = _mm_add_ps(_mm_abs_ps(pa[i]), _mm_abs_ps(pb[i]));
     }
}

inline __m128d _mm_abs_pd(__m128d x) {
    const __m128d mask = _mm_set1_pd(-0.); // -0. = 1 << 63
    return _mm_andnot_pd(mask, x); // !mask & x
}


void task2_d(double* a, double* b, double* c, int n){
     for(int i = 0; i < n; i++)
	  c[i] = fabs(a[i]) + fabs(b[i]);
}

void task2_d_sse(double* a, double* b, double* c, int n){
     __m128d *pa = (__m128d*) a;
     __m128d *pb = (__m128d*) b;
     __m128d *pc = (__m128d*) c;

     for (int i = 0; i < n / 2; i++){
	  pc[i] = _mm_add_pd(_mm_abs_pd(pa[i]), _mm_abs_pd(pb[i]));
     }
}


void task2(){
     printf("TASK 2\n");
     int n = 4096 * 4096;
     struct timespec start, end;

     // 8i
     int8_t* a8 = aligned_alloc(32,n*sizeof(int8_t));
     int8_t* b8 = aligned_alloc(32,n*sizeof(int8_t));
     int8_t* c8 = aligned_alloc(32,n*sizeof(int8_t));

     for(int i = 0; i < n; i++){
	  a8[i] = rand();
	  b8[i] = rand();
     }     
     
     clock_gettime(CLOCK_REALTIME, &start);
     task2_i8(a8, b8, c8, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("8 bit in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     clock_gettime(CLOCK_REALTIME, &start);
     task2_i8_avx(a8, b8, c8, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("8 bit avx in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
     
     free(a8);free(b8);free(c8);

     // 16i
     int16_t* a16 = aligned_alloc(32,n*sizeof(int16_t));
     int16_t* b16 = aligned_alloc(32,n*sizeof(int16_t));
     int16_t* c16 = aligned_alloc(32,n*sizeof(int16_t));

     for(int i = 0; i < n; i++){
	  a16[i] = rand();
	  b16[i] = rand();
     }
     
     clock_gettime(CLOCK_REALTIME, &start);
     task2_i16(a16, b16, c16, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("16 bit in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     clock_gettime(CLOCK_REALTIME, &start);
     task2_i16_avx(a16, b16, c16, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("16 bit avx in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
     
     free(a16);free(b16);free(c16);

     // 32i
     int32_t* a32 = aligned_alloc(32,n*sizeof(int32_t));
     int32_t* b32 = aligned_alloc(32,n*sizeof(int32_t));
     int32_t* c32 = aligned_alloc(32,n*sizeof(int32_t));

     for(int i = 0; i < n; i++){
	  a32[i] = rand();
	  b32[i] = rand();
     }
     
     clock_gettime(CLOCK_REALTIME, &start);
     task2_i32(a32, b32, c32, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("32 bit in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     clock_gettime(CLOCK_REALTIME, &start);
     task2_i32_avx(a32, b32, c32, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("32 bit avx in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
     
     
     free(a32);free(b32);free(c32);

     // 64i
     int64_t* a64 = aligned_alloc(32,n*sizeof(int64_t));
     int64_t* b64 = aligned_alloc(32,n*sizeof(int64_t));
     int64_t* c64 = aligned_alloc(32,n*sizeof(int64_t));

     for(int i = 0; i < n; i++){
	  a64[i] = rand();
	  b64[i] = rand();
     }
     
     clock_gettime(CLOCK_REALTIME, &start);
     task2_i64(a64, b64, c64, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("64 bit in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
     /*
     clock_gettime(CLOCK_REALTIME, &start);
     task2_i64_avx(a64, b64, c64, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("64 bit avx in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
     */
     
     free(a64);free(b64);free(c64);

     // float
     float* af = aligned_alloc(32,n*sizeof(float));
     float* bf = aligned_alloc(32,n*sizeof(float));
     float* cf = aligned_alloc(32,n*sizeof(float));

     float alpha_f = -100000.0;
     float beta_f = 100000.0;

     for(int i = 0; i < n; i++){
	  af[i] = alpha_f + ((float)rand()/(float)RAND_MAX)*(beta_f-alpha_f);
	  bf[i] = alpha_f + ((float)rand()/(float)RAND_MAX)*(beta_f-alpha_f);
     }
     
     clock_gettime(CLOCK_REALTIME, &start);
     task2_f(af, bf, cf, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("float in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     clock_gettime(CLOCK_REALTIME, &start);
     task2_f_sse(af, bf, cf, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("float sse in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
     
     free(af);free(bf);free(cf);

     // double
     double* ad = aligned_alloc(32,n*sizeof(double));
     double* bd = aligned_alloc(32,n*sizeof(double));
     double* cd = aligned_alloc(32,n*sizeof(double));

     double alpha_d = -100000.0;
     double beta_d = 100000.0;

     for(int i = 0; i < n; i++){
	  ad[i] = alpha_d + ((double)rand()/(double)RAND_MAX)*(beta_d-alpha_d);
	  bd[i] = alpha_d + ((double)rand()/(double)RAND_MAX)*(beta_d-alpha_d);	  
     }
     
     clock_gettime(CLOCK_REALTIME, &start);
     task2_d(ad, bd, cd, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("double in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     clock_gettime(CLOCK_REALTIME, &start);
     task2_d_sse(ad, bd, cd, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("double sse in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
     
     
     free(ad);free(bd);free(cd);
}

// task 4
// 4096 * 4096
// +--------+--------------+
// |  type  |     time     |
// +--------+--------------+
// |  int8  |   0.047516s  |
// +--------+--------------+
// |  int16 |   0.053025s  |
// +--------+--------------+
// |  int32 |   0.065553s  |
// +--------+--------------+
// |  int64 |   0.120089s  |
// +--------+--------------+
// |  float |   0.061225s  |
// +--------+--------------+
// | double |   0.115424s  |
// +--------+--------------+

void print_array_float(float* a, int n){
     for(int i = 0; i<n; i++){
	  printf("%f ", a[i]);
     }
     printf("\n");	 
}

void arrays_equal_float(float* a, float* b, int n){     
     for(int i = 0; i<n; i++){
	  if(a[i] != b[i]) {
	       printf("%f != %f at %d\n ", a[i], b[i], i);
	       return;
	  }	  
     }
     printf("arrays equal\n");	 
}

void sqrt_float(
     float* x,
     float* y,
     int n){
     for (int i = 0; i < n; i++){
	  y[i] = sqrtf(x[i]);
     }
}

void sqrt_float_sse(
     float* x,
     float* y,
     int n){
     __m128* x_sse = (__m128*)x;
     __m128* y_sse = (__m128*)y;
     int r = sizeof(__m128)/sizeof(float);
     for (int i = 0; i < (n/r); i++){
	  y_sse[i] = _mm_sqrt_ps(x_sse[i]);
     }
     for(int i = n - (n % r); i < n; i++){
	  y[i] = sqrt(x[i]);
     }	  
     
     y = (float*)y_sse;
}

void sqrt_float_avx(
     float* x,
     float* y,
     int n){
     __m256* x_avx = (__m256*)x;
     __m256* y_avx = (__m256*)y;

     int r = sizeof(__m256)/sizeof(float);
     for (int i = 0; i < (n/r); i++){
	  y_avx[i] = _mm256_sqrt_ps(x_avx[i]);
     }
     for(int i = n - (n % r); i < n; i++){
	  y[i] = sqrt(x[i]);
     }
     y = (float*)y_avx;
}

void sqrt_double(
     double* x,
     double* y,
     int n){
     for (int i = 0; i < n; i++){
	  y[i] = sqrt(x[i]);
     }
}

void sqrt_double_sse2(
     double* x,
     double* y,
     int n){
     __m128d* x_sse2 = (__m128d*)x;
     __m128d* y_sse2 = (__m128d*)y;
     int r = sizeof(__m128d)/sizeof(double);
     
     for (int i = 0; i < (n/r); i++){
	  y_sse2[i] = _mm_sqrt_pd(x_sse2[i]);
     }
     for(int i = n - (n % r); i < n; i++){
	  y[i] = sqrt(x[i]);
     }	  
     
     y = (double*)y_sse2;
}

void sqrt_double_avx(
     double* x,
     double* y,
     int n){
     __m256d* x_avx = (__m256d*)x;
     __m256d* y_avx = (__m256d*)y;
     int r = sizeof(__m256d)/sizeof(double);
     
     for (int i = 0; i < (n/r); i++){
	  y_avx[i] = _mm256_sqrt_pd(x_avx[i]);
     }
     for(int i = n - (n % r); i < n; i++){
	  y[i] = sqrt(x[i]);
     }	  
     
     y = (double*)y_avx;
}

void task6(){
     printf("TASK 6\n");
     int n = 4096 * 4096;
     struct timespec start, end;

     // float
     float* x_f = aligned_alloc(32,n*sizeof(float));
     float* y_f = aligned_alloc(32,n*sizeof(float));

     float alpha_f = 1.0;
     float beta_f = 1000000.0;

     for(int i = 0; i < n; i++){
	  x_f[i] = alpha_f + ((float)rand()/(float)RAND_MAX)*(beta_f-alpha_f);
     }
     
     clock_gettime(CLOCK_REALTIME, &start);
     sqrt_float(x_f, y_f, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("float no simd in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     clock_gettime(CLOCK_REALTIME, &start);
     sqrt_float_sse(x_f, y_f, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("float sse in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
     
     clock_gettime(CLOCK_REALTIME, &start);     
     sqrt_float_avx(x_f, y_f, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("float avx in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     // double
     double* x_d = aligned_alloc(32,n*sizeof(double));
     double* y_d = aligned_alloc(32,n*sizeof(double));

     double alpha_d = 1.0;
     double beta_d = 1000000.0;

     for(int i = 0; i < n; i++){
	  x_d[i] = alpha_d + ((double)rand()/(double)RAND_MAX)*(beta_d-alpha_d);
     }
     
     clock_gettime(CLOCK_REALTIME, &start);
     sqrt_double(x_d, y_d, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("double no simd in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     clock_gettime(CLOCK_REALTIME, &start);
     sqrt_double_sse2(x_d, y_d, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("double sse2 in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     clock_gettime(CLOCK_REALTIME, &start);
     sqrt_double_avx(x_d, y_d, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("double avx in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

}

// task 7
// 4096 * 4096
// +--------------+--------------+
// |      type    |     time     |
// +--------------+--------------+
// |      float   |  0.322759s   |
// +--------------+--------------+
// |   sse float  |  0.031860s   |
// +--------------+--------------+
// |     double   |  0.366414s   |
// +--------------+--------------+
// | sse2 double  |  0.072700s   |
// +--------------+--------------+

typedef struct {
     float re;
     float im;
} complex;

void mult_C(complex* x, complex* y, complex *z, int n)
{
     for(int i = 0; i < n; i++){
	z[i].re = (x[i].re*y[i].re) - (x[i].im*y[i].im);
	(z[i]).im = (x[i].im*y[i].re) + (y[i].im*x[i].re);
     }
}

void print128_num(__m128 var)
{
    float *val = (float*) &var;
    printf("[D] %2.2f %2.2f %2.2f %2.2f\n", 
           val[0], val[1], val[2], val[3]);
}

void complex_arrays_eq(complex* x, complex* y, int n){
     for(int i = 0; i < n; i++){
	  if(x[i].re != y[i].re || x[i].im != y[i].im ){
	       printf("NO\n");
	       return;
	  }
     }
     printf("YES\n");
}

void mult_SSE(complex* x, complex* y, complex *z, int n)
{
     __m128 num1, num2;
	
     for(int i = 0; i < n; i++){
	  num1 = _mm_setr_ps(x[i].re, x[i].im, x[i].im, y[i].im);
	  num2 = _mm_setr_ps(y[i].re, y[i].re, y[i].im, x[i].re);
	  
	  num1 = _mm_mul_ps(num1, num2);
	  
	  num2 = _mm_shuffle_ps(num1, num1, _MM_SHUFFLE(0,1,3,2));
	  
	  num2 = _mm_addsub_ps(num1, num2);
	  
	  _mm_storel_pi((__m64*)(z + i), num2);	
	}	
}

void task8(){
     printf("TASK 8\n");
     int n = 4096 * 4096;
     struct timespec start, end;

     complex* x = calloc(n, sizeof(complex));
     complex* y = calloc(n, sizeof(complex));
     complex* z = calloc(n, sizeof(complex));

     float alpha = -10000.0;
     float beta = 10000.0;

     for(int i = 0; i < n; i++){
	  x[i].re = alpha + ((float)rand()/(float)RAND_MAX)*(beta-alpha);
	  x[i].im = alpha + ((float)rand()/(float)RAND_MAX)*(beta-alpha);
	  y[i].re = alpha + ((float)rand()/(float)RAND_MAX)*(beta-alpha);
	  y[i].im = alpha + ((float)rand()/(float)RAND_MAX)*(beta-alpha);
     }

     clock_gettime(CLOCK_REALTIME, &start);
     mult_C(x, y, z, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("complex no simd in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     clock_gettime(CLOCK_REALTIME, &start);
     mult_SSE(x, y, z, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("complex sse in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
}

// task 8
// 4096 * 4096
// +--------------+--------------+
// |      type    |     time     |
// +--------------+--------------+
// |      C       |  0.126849s   |
// +--------------+--------------+
// |     SSE      |  0.085923s   |
// +--------------+--------------+

void shift(int32_t* a, int32_t* b, int size){
  int32_t bit = 0;
  for (int i = size - 1; i >= 0; i--){
    bit = (a[i] >> 31) & 1;
    b[i] = (a[i] << 1) | bit;
  }
}

void shiftSSE(int32_t* a, int32_t* b, int size){
  __m128i* pa = (__m128i*) a;
  __m128i* pb = (__m128i*) b;
  
  __m128i bits[4];
  __m128i one = _mm_set1_epi32(1);
  

  for (int i = 0; i < size / 4; i++){
    bits[i] = _mm_srli_epi32(pa[i], 31);
    
    pb[i] = _mm_slli_epi32(pa[i], 1);
    pb[i] = _mm_or_si128(pb[i], bits[i]);
  }
}


void task9(){
  printf("TASK 2\n");
  int n = 4e7;
  struct timespec start, end;

  int32_t* a32 = aligned_alloc(32,n*sizeof(int32_t));
  int32_t* c32 = aligned_alloc(32,n*sizeof(int32_t));
  int32_t* c32_ = aligned_alloc(32,n*sizeof(int32_t));
  
  for(int i = 0; i < n; i++){
    a32[i] = rand();
  }
  
  clock_gettime(CLOCK_REALTIME, &start);
  shift(a32, c32, n);
  clock_gettime(CLOCK_REALTIME, &end);     
  printf("no sse shift in %fs\n", (end.tv_sec - start.tv_sec)
	 + (end.tv_nsec - start.tv_nsec) / 1e9);

  clock_gettime(CLOCK_REALTIME, &start);
  shiftSSE(a32, c32_, n);
  clock_gettime(CLOCK_REALTIME, &end);     
  printf("sse shift in %fs\n", (end.tv_sec - start.tv_sec)
	 + (end.tv_nsec - start.tv_nsec) / 1e9);
  
}

// L1 = 32K, L2 = 256K, L3 = 3MB
// gcc -msse3 -msse2 -msse -mavx -mavx2 -lm  main.c -o main && ./main
int main (int argc, char *argv[]) {
     srand(time(NULL));
     printf("BEGIN\n\n");
     task1();
     task2();
     task6();
     task8();
     task9();
     printf("\nEND");
}
