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

int main (int argc, char *argv[]) {
     printf("BEGIN\n");
     //task1();
     
     int8_t a[1024] = {[0 ... 1023] = 40};
     int8_t b[1024] = {[0 ... 1023] = 12};
     int8_t c[1024];
     
     sum_mmx_8(a, b, c, sizeof(a));
     for(int i = 0; i < 1024; i++){
	  printf("%i ", c[i]);
     }
     
     printf("\nEND");
}
