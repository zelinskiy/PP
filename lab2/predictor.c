#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

typedef enum { false, true } bool;

typedef enum { lo, hi } bit;

typedef enum Counter {
     OO,
     OI,
     IO,
     II
} counter;

int incCounter(int c, int m){
     return c + (c != m);
}

int decCounter(int c){
     return c - (c != 0);
}

int bitsToInt(bit arr[], int count)
{
    int ret = 0;
    int tmp;
    for (int i = 0; i < count; i++) {
        tmp = arr[i];
        ret |= tmp << (count - i - 1);
    }
    return ret;
}

char* string_array(bit arr[], int n){
     char* res = calloc(n+1, sizeof(char));
     for(int i = 0; i < n; i++){
	  res[i] = arr[i] ? '1' : '0';
     }
     res[n] = '\0';
     return res;
}

void print_array(int*a, int n){
     for(int i = 0; i < n; i++){
	  printf("%d ", a[i]);
     }
     printf("\n");
}


int main (int argc, char *argv[]){
     printf("BEGIN\n\n");
     srand(time(NULL));
     
     const int n = 2;
     const int k = 1;
     const int s = 50;
     const int t_prob = 20;
     
     bit* story = calloc(s, sizeof(bit));
     for(int i = 0; i < s; i++){
	  if((rand() % 100) <= t_prob) story[i] = 1;
	  else story[i] = 0;
	  printf("%d ",story[i]);
     }
     printf("\n");
     
     bit* table = calloc(pow(n,2), sizeof(bit));
     bit* prev  = calloc(n, sizeof(bit));
     int exp, real, prev_i;
     int good = 0;
     int bad = 0;

     for(int i = 0; i < s; i++){
	  prev_i = bitsToInt(prev, n);
	  exp = table[prev_i] < k/2;
	  real = story[i];	  	  
	  if(exp == real){
	       table[prev_i] = decCounter(table[prev_i]);
	       printf("[O] exp = %d, real = %d, dec %s(%d)\n",
		      exp, real, string_array(prev, n), prev_i);
	       good++;
	  }
	  else {
	       table[prev_i] = incCounter(table[prev_i], k);
	       printf("[E] exp = %d, real = %d, inc %s(%d)\n",
		      exp, real, string_array(prev, n), prev_i);
	       bad++;
	  }
	  for(int i = 1; i < n; i++){
	       prev[i-1] = prev[i];
	  }
	  prev[n-1] = real;
	  
	  printf("table = ");
	  print_array((int*)table, pow(n,2));	  	  
     }     
     printf("predicted: %d, missed: %d\n", good, bad);
     printf("\n\nEND");
}
