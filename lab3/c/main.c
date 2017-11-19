#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <cpuid.h>
#include <string.h>
#include <mmintrin.h>
#include <emmintrin.h> 

//https://software.intel.com/sites/landingpage/IntrinsicsGuide

//TODO:
// more simd for sqrt
// task 8 (complex multiplication)

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

void task2_i16(int16_t* a, int16_t* b, int16_t* c, int n){
     for(int i = 0; i < n; i++)
	  c[i] = abs(a[i]) + abs(b[i]);
}

void task2_i32(int32_t* a, int32_t* b, int32_t* c, int n){
     for(int i = 0; i < n; i++)
	  c[i] = abs(a[i]) + abs(b[i]);
}

void task2_i64(int64_t* a, int64_t* b, int64_t* c, int n){
     for(int i = 0; i < n; i++)
	  c[i] = abs(a[i]) + abs(b[i]);
}

void task2_f(float* a, float* b, float* c, int n){
     for(int i = 0; i < n; i++)
	  c[i] = fabsf(a[i]) + fabsf(b[i]);
}

void task2_d(double* a, double* b, double* c, int n){
     for(int i = 0; i < n; i++)
	  c[i] = fabs(a[i]) + fabs(b[i]);
}

void task2(){
     printf("TASK 2\n");
     int n = 4096 * 4096;
     struct timespec start, end;

     // 8i
     int8_t* a8 = malloc(n*sizeof(int8_t));
     int8_t* b8 = malloc(n*sizeof(int8_t));
     int8_t* c8 = malloc(n*sizeof(int8_t));

     for(int i = 0; i < n; i++){
	  a8[i] = rand();
	  b8[i] = rand();
     }     
     
     clock_gettime(CLOCK_REALTIME, &start);
     task2_i8(a8, b8, c8, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("8 bit in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     free(a8);free(b8);free(c8);

     // 16i
     int16_t* a16 = malloc(n*sizeof(int16_t));
     int16_t* b16 = malloc(n*sizeof(int16_t));
     int16_t* c16 = malloc(n*sizeof(int16_t));

     for(int i = 0; i < n; i++){
	  a16[i] = rand();
	  b16[i] = rand();
     }
     
     clock_gettime(CLOCK_REALTIME, &start);
     task2_i16(a16, b16, c16, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("16 bit in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     free(a16);free(b16);free(c16);

     // 32i
     int32_t* a32 = malloc(n*sizeof(int32_t));
     int32_t* b32 = malloc(n*sizeof(int32_t));
     int32_t* c32 = malloc(n*sizeof(int32_t));

     for(int i = 0; i < n; i++){
	  a32[i] = rand();
	  b32[i] = rand();
     }
     
     clock_gettime(CLOCK_REALTIME, &start);
     task2_i32(a32, b32, c32, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("32 bit in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     free(a32);free(b32);free(c32);

     // 64i
     int64_t* a64 = malloc(n*sizeof(int64_t));
     int64_t* b64 = malloc(n*sizeof(int64_t));
     int64_t* c64 = malloc(n*sizeof(int64_t));

     for(int i = 0; i < n; i++){
	  a64[i] = rand();
	  b64[i] = rand();
     }
     
     clock_gettime(CLOCK_REALTIME, &start);
     task2_i64(a64, b64, c64, n);
     clock_gettime(CLOCK_REALTIME, &end);     
     printf("64 bit in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     free(a64);free(b64);free(c64);

     // float
     float* af = malloc(n*sizeof(float));
     float* bf = malloc(n*sizeof(float));
     float* cf = malloc(n*sizeof(float));

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

     free(af);free(bf);free(cf);

     // double
     double* ad = malloc(n*sizeof(double));
     double* bd = malloc(n*sizeof(double));
     double* cd = malloc(n*sizeof(double));

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

void task6(){
     printf("TASK 6\n");
     int n = 4096 * 4096;
     struct timespec start, end;

     // float
     float* x_f = malloc(n*sizeof(float));
     float* y_f = malloc(n*sizeof(float));

     float alpha_f = -1000000.0;
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

     // double
     double* x_d = malloc(n*sizeof(double));
     double* y_d = malloc(n*sizeof(double));

     double alpha_d = -1000000.0;
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


int main (int argc, char *argv[]) {
     srand(time(NULL));
     printf("BEGIN\n");
     task1();
     task2();
     task6();
     printf("\nEND");
}
