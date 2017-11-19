#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <cpuid.h>
#include <string.h>
#include "mmintrin.h"

//https://software.intel.com/sites/landingpage/IntrinsicsGuide

void task1(){
     printf("\nTASK 1\n\n");
     
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
     int n = 1024;
     struct timespec start, end;

     // 8
     int8_t* a = malloc(n*sizeof(int8_t));
     int8_t* b = malloc(n*sizeof(int8_t));
     int8_t* c = malloc(n*sizeof(int8_t));

     for(int i = 0; i < n; i++){
	  a[i] = rand();
	  b[i] = rand();
     }
     
     clock_gettime(CLOCK_REALTIME, &start);
     printf("8 bit: %30.32f\n",
	    integrate(fun1, 0.0, 1.0, 1.0/(double) n));
     clock_gettime(CLOCK_REALTIME, &end);
     
     printf("in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
}


int main (int argc, char *argv[]) {
     srand(time(NULL));
     printf("BEGIN\n");
     //task1();

     
     int8_t* a = malloc(n*sizeof(int8_t));
     int8_t* b = malloc(n*sizeof(int8_t));
     int8_t* c = malloc(n*sizeof(int8_t));

     for(int i = 0; i < n; i++){
	  a[i] = rand();
	  b[i] = rand();
     }
     task2_i8(a, b, c, n);
     for(int i = 0; i < n; i++){
	  printf("%i ", c[i]);
     }
     
     printf("\nEND");
}
