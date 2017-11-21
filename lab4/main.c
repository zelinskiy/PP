#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <pthread.h>
#include <time.h>
#include <math.h>
#include <unistd.h>

// TODO:
// find n of iterations s.t. pi(n+1) == pi(n)

#define PI 3.14159265358979323846

void task1(){
#ifdef _OPENMP
     printf ("_OPENMP Defined\n");
#else
     printf ("_OPENMP UnDefined\n");
#endif
}

void task2(){
     int n = 0;
#pragma omp parallel reduction (+:n)
     {
	  n++;
     }
     printf("(1 way) ncores = %d\n", n);
     printf("(2 way) ncores = %ld\n", sysconf(_SC_NPROCESSORS_ONLN));
}

double integrate(double (*f)(double), double a, double b, double step){
     double res = 0.0;
     for(double x = a; x <= b; x += step){
	  res += f(x)*step;
     }
     return res;
}

double fun1(double x){
     return 4.0 / (1.0 + x*x);
}

double fun2(double x){
     return 4.0 * sqrt(1.0 - x*x);
}

double leubniz(unsigned int n){
     double res = 0.0;
     for(unsigned int i = 0; i < n; i++){       
	  res += 1.0 / (2 * i + 1) * (i % 2 ? -1 : 1);  
     }
     return res * 4.0;
}

//n = 1e9
//integral(4/(1+x^2))   in 5.784784s
//integral(sqrt(1-x^2)) in 8.009406s
//leubniz               in 6.290465s
void task3(unsigned int n){
     struct timespec start, end;

     clock_gettime(CLOCK_REALTIME, &start);
     double r1 = integrate(fun1, 0.0, 1.0, 1.0/(double) n);
     clock_gettime(CLOCK_REALTIME, &end);
     printf("integral(4/(1+x^2)): %30.32f (d=%30.32f)\n", r1, PI - r1);
     
     printf("in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);

     clock_gettime(CLOCK_REALTIME, &start);
     double r2 = integrate(fun2, 0.0, 1.0, 1.0/(double) n);
     clock_gettime(CLOCK_REALTIME, &end);
     printf("integral(sqrt(1-x^2)): %30.32f (d=%30.32f)\n", r2, PI - r2);
     
     printf("in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
     
     clock_gettime(CLOCK_REALTIME, &start);
     double r3 = leubniz(n);
     clock_gettime(CLOCK_REALTIME, &end);
     printf("leubniz: %30.32f (d=%30.32f)\n", r3, PI - r3);
     
     printf("in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);     
}

struct pi_struct{
     double start;
     double end;
     double step;
     double res;
};

void task6_old (unsigned int n) {
     struct timespec start, end;
     clock_gettime(CLOCK_REALTIME, &start);

     void* func(void* d){
	  struct pi_struct* p = (struct pi_struct*) d;     
	  for(double x = (*p).start; x < (*p).end; x += (*p).step){
	       (*p).res += (*p).step / (1.0 + x*x);
	  }
	  return d;
     }
     
     const int t = 4.0;
     double step = 1.0/(double)n;
     struct pi_struct p[t];
     pthread_t h[t];
     for(int i = 0; i < t; i++){
	  p[i].start = i/(double)t;
	  p[i].end = (i+1)/(double)t;
	  p[i].step = step;
	  p[i].res = 0.0;
	  //printf("[%30.32f] %f - %f\n", p[i].step, p[i].start, p[i].end);
	  pthread_create(&h[i],NULL,func,&p[i]);
     }

     for(int i = 0; i < t; i++) pthread_join(h[i], NULL);
     
     double res = 0.0;
     for(int i = 0; i < t; i++){
	  res += p[i].res;
     }     
     printf("posix integral(4/(1+x^2)): %30.32f (d=%30.32f)\n", 4.0*res, PI-4.0*res);
     
     clock_gettime(CLOCK_REALTIME, &end);
     printf("in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
}

void task8_old (unsigned int n) {
     struct timespec start, end;
     clock_gettime(CLOCK_REALTIME, &start);
     
     double res = 0.0;
     double step = 1.0/(double)n;
#pragma omp parallel for reduction(+:res) num_threads(4)
     for(unsigned int i = 0; i < n; i++){
	  double x = step * i;
	  res += step / (1.0 + x * x);  
     }
     printf("OMP integral(4/(1+x^2)): %30.32f (d=%30.32f)\n", 4.0 * res, PI - 4.0 * res);
     
     clock_gettime(CLOCK_REALTIME, &end);
     printf("in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
}

void task6 (unsigned int n) {
     struct timespec start, end;
     clock_gettime(CLOCK_REALTIME, &start);

     void* func(void* d){
	  struct pi_struct* p = (struct pi_struct*) d;     
	  for(unsigned int i = (*p).start; i < (*p).end; i++){       
	       (*p).res += 1.0 / (2 * i + 1) * (i % 2 ? -1 : 1);  
	  }
	  return d;
     }
     
     const int t = 4;     
     struct pi_struct p[t];
     pthread_t h[t];     
     for(int i = 0; i < t; i++){
	  p[i].start = i*n/t;
	  p[i].end = (i+1)*n/t;
	  p[i].res = 0.0;
	  //printf("%u - %u\n", p[i].start, p[i].end);
	  pthread_create(&h[i],NULL,func,&p[i]);
     }

     for(int i = 0; i < t; i++) pthread_join(h[i], NULL);
     
     double res = 0.0;
     for(int i = 0; i < t; i++){
	  res += p[i].res;
     }     
     printf("posix leubniz: %30.32f (d=%30.32f)\n", res*4.0, PI - res*4.0);
     
     clock_gettime(CLOCK_REALTIME, &end);
     printf("in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
}

void task8 (unsigned int n) {
     struct timespec start, end;
     clock_gettime(CLOCK_REALTIME, &start);
     
     double res = 0.0;
#pragma omp parallel for reduction(+:res) num_threads(4)
     for(unsigned int i = 0; i < n; i++){

	  res += 1.0 / (2 * i + 1) * (i % 2 ? -1 : 1);  
     }
     printf("OMP leubniz: %30.32f (d=%2.32f)\n", res*4.0, PI - res*4.0);
     
     clock_gettime(CLOCK_REALTIME, &end);
     printf("in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
}


//$ gcc -g -Wall -fopenmp -pthread -lm main.c -lpthread -o main && ./main
int main (int argc, char *argv[]) {
     printf("BEGIN\n");
     const unsigned int n = 1e9;
     task1();
     task2();
     task3(n);
     task6(n);
     task8(n);
     task6_old(n);
     task8_old(n);
     printf("END");
}
/*
Порядок виконання лабораторної роботи

1. Включити режим Open MP і перевірити успішність його включення.

2. Визначити кількість потоків першим та другим способом. 
Порівняти отримані значення.

3. Реалізувати послідовні функції, які обчислюють значення π 
(3 різні функції) з заданою точністю.

4. Визначити найкращу функцію з боку обчислювальної складності.

5. Визначити кількість ітерацій для отримання максимально можливої 
точності в разі використання даних типу double.

6. Реалізувати паралельно найкращу з обраних функцій. Створити 
за допомогою функції Windows потоки. Обрати кількість ітерацій 
як для послідовного варанту.

7. Зробити висновки по ефективності використання Windows потоків 
по часу та точності.

8. Забезпечити паралельне виконання за допомогою директив Open MP. 
Порівняти результати з попередніми. Зробити висновки по 
ефективності використання OpenMP потоків.

9. Визначте значення прискорення для паралельних виконань програми. 
Порівняйте ці значення для обох варіантів паралельного виконання. 

*/

// Контрольні запитання і завдання

// 1. Як включається режим використання Open MP?
// gcc ... -fopenmp ... main.c ...

// 2. Як виконуються директиви Open MP, якщо режим Open MP не вмикнуно?
// Послідовно один раз

// 3. Як визначається кількість ядер процесору?
// 2 способами - через системні змінні sysconf(_SC_NPROCESSORS_ONLN)
// або через OpenMP

// 4. Що таке прискорення для паралельного виконання програми?
// Лк 3 слайд 10
// Прискорення Sp(n) для паралельного алгоритму
// розмірністю n визначається відношенням часової
// складності послідовного T1(n) та паралельного Tp(n)
// алгоритмів для p процесорів
// Sp(n) = T1(n)/Tp(n).
