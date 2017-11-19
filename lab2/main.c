#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>
#include <math.h>


//-----------------------TASKS------------------------------

// n = 4e8
// 13,309,709      branch-misses:u 
// 5.189873888 seconds time elapsed
int task1_bad(int* a, int n){
     int res = 0;
     for(int i=0;i<n;i++){
	  if(a[i] > 0) res++;
     }
     return res;
}

// n = 4e8
// 13,321,596      branch-misses:u
// 5.068166082 seconds time elapsed
int task1_good(int* a, int n){
     int res = 0;
     for(int i=0;i<n;i++){
	  res += a[i] >> 31;
     }
     return n + res;
}

// n = 1e5
// 1,438,820,195      branch-misses:u
// 14.954686597 seconds time elapsed
void task2_bad(int* a, int n){
     int t;
     for(int i = 0; i < n; i++){
	  for(int j = 0; j < n - i + 1; j++){
	       if(a[j] > a[j+1]){
		    t = a[j];
		    a[j] = a[j+1];
		    a[j+1] = t; 
	       }
		    
	  }
     }
}

// n = 1e5
// 218,618      branch-misses:u
// 7.039969125 seconds time elapsed
void task2_good(int* a, int n){
     int t;
     for(int i = 0; i < n; i++){
	  for(int j = i; j >= 0; j--){
	       if(a[j] > a[j+1]){
		    t = a[j];
		    a[j] = a[j+1];
		    a[j+1] = t; 
	       }
		    
	  }
     }
}

// n = 1e6; m = 2e3
// 1,127,087,108      branch-misses:u
// 8.269032493 seconds time elapsed
int* task3_bad(int* a, int* b, int a_n, int b_n){
     int* c = malloc((a_n + b_n - 1) * sizeof(int));
     
     for(int i = 0; i < a_n; i++){
	  for(int j = 0; j < b_n; j++){
	       switch(b[j]){
	       case 1:
		    c[i+j] += a[i];
		    break;
	       case -1:
		    c[i+j] -= a[i];
		    break;
	       }
	  }
     }
     return c;
}

// n = 1e6; m = 2e3
// 1,041,106      branch-misses:u
// 2.691250433 seconds time elapsed
int* task3_good(int* a, int* b, int a_n, int b_n){
     int* c = malloc((a_n + b_n - 1) * sizeof(int));
     
     for(int i = 0; i < a_n; i++){
	  for(int j = 0; j < b_n; j++){
	       c[i+j] += a[i] * b[j];
	  }
     }
     return c;
}

// n = 10e7; f in [-10K..10K]
// 3,442,115      branch-misses:u
// 2.232059735 seconds time elapsed
int* task4_good_float (float* a, int n){
     int* res = malloc(n * sizeof(int));
     double t;
     for(int i = 0; i < n; i++){
	  t = a[i] + 6755399441055744.0;	  
	  res[i] = *((int *)(&t));
     }
     return res;
}

// n = 10e7; f in [-10K..10K]
// 3,555,239      branch-misses:u
// 2.530510866 seconds time elapsed
int* task4_bad_float (float* a, int n){
     int* res = malloc(n * sizeof(int));
     for(int i = 0; i < n; i++){
	  res[i] = (int)roundf(a[i]);
          //res[i] = (int)(a[i]<0 ? a[i]-0.5 : a[i]+0.5);
     }
     return res;
}

// n = 10e7; f in [-10K..10K]
// 3,538,403      branch-misses:u
// 2.833280569 seconds time elapsed
int* task4_good_double (double* a, int n){
     int* res = malloc(n * sizeof(int));
     double t;
     for(int i = 0; i < n; i++){
	  t = a[i] + 6755399441055744.0;
	  res[i] = *((int *)(&t));
     }
     return res;
}

// n = 10e7; f in [-10K..10K]
// 4,555,869      branch-misses:u
// 3.199942895 seconds time elapsed
int* task4_bad_double (double* a, int n){
     int* res = malloc(n * sizeof(int));
     for(int i = 0; i < n; i++){
	  res[i] = (int)round(a[i]);
     }
     return res;
}

//-----------------------UTILS--------------------------------

void print_array(int* a, int n){
     for(int i = 0; i<n; i++){
	  printf("%d ", a[i]);
     }
     printf("\n");	 
}

void print_array_float(float* a, int n){
     for(int i = 0; i<n; i++){
	  printf("%f ", a[i]);
     }
     printf("\n");	 
}

void arrays_equal(int* a, int* b, int n){     
     for(int i = 0; i<n; i++){
	  if(a[i] != b[i]) {
	       printf("%d != %d at %d\n ", a[i], b[i], i);
	       return;
	  }	  
     }
     printf("arrays equal\n");	 
}

void print_bits(int x){
     for(int i = 0; i < 32; i++){
	  printf("%d|", (x >> i)&1);
     }
     printf("\n");
}

int* randoms_int (int n){
     int* a = malloc(n*sizeof(int));
     for(int i = 0; i < n; i++){
	  a[i] = rand()*2;
     }
     return a;
}

int* randoms_coeff (int n){
     int* a = malloc(n*sizeof(int));
     for(int i = 0; i < n; i++){
	  a[i] = rand() % 3 - 1;
     }
     return a;
}

float* randoms_float (int n, float a, float b){
     float* f = malloc(n * sizeof(float));
     for(int i = 0; i < n; i++){
	  f[i] = a + ((float)rand() / (float)RAND_MAX) * (b - a);
     }
     return f;
}

double* randoms_double (int n, double a, double b){
     double* d = malloc(n * sizeof(double));
     for(int i = 0; i < n; i++){
	  d[i] = a + ((double)rand() / (double)RAND_MAX) * (b - a);
     }
     return d;
}

//-----------------------MAIN---------------------------------

// usage: ./main a b where a b are:
// 1 0 - positives with if
// 1 1 - positives with >>
// 2 0 - naive bubblesort
// 2 1 - bubblesort with reversed 2 cycle
// 3 0 - polynomials with switch
// 3 1 - simple multiplication os polynomials
// 4 0 - round floats with roundf
// 4 1 - round floats with magic number
// 5 0 - round doubles with round
// 5 1 - round doubles with magic number

//nix-env -iA nixos.linuxPackages.perf
//gcc -Wall main.c -o main && perf stat -e branch-misses ./main 1 1
int main (int argc, char *argv[]){
     srand(time(NULL));
     
     int t = argv[1][0] - '0';
     int good = argv[2][0] - '0';

     int n;
     int m;
     int* a;
     int* b;
     float* f;
     double* d;

     switch(t){
     case 1:
	  n = 4e8;
	  a = randoms_int(n);
	  if (good) task1_good(a, n);
	  else task1_bad(a, n);
	  break;
     case 2:
	  n = 1e5;
	  a = randoms_int(n);
	  if (good) task2_good(a, n);
	  else task2_bad(a, n);
	  break;
     case 3:
	  n = 1e6;
	  m = 2e3;
	  a = randoms_int(n);
	  b = randoms_coeff(m);
	  if (good) task3_good(a,b,n,m);
	  else task3_bad(a,b,n,m);
	  break;
     case 4:
	  n = 10e7;
	  f = randoms_float(n, -10000.0, 10000.0);
	  //arrays_equal(task4_good_float(f,f_n),task4_bad_float(f,f_n),f_n);
	  if (good) task4_good_float(f, n);
	  else task4_bad_float(f, n);
	  break;
     case 5:
	  n = 10e7;
	  d = randoms_double(n, -10000.0, 10000.0);
	  if (good) task4_good_double(d, n);
	  else task4_bad_double(d, n);
	  break;
     }
}
