#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <pthread.h>
#include <time.h>
#include <math.h>
#include <unistd.h>

// TODO:
// 2 more methods
// find n of iterations s.t. pi(n+1) == pi(n)

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

double bellard(unsigned int n)
{
    double res = 0.0;
    double sum = 0.0;
 
    for(unsigned int i = 0; i < n; i++)
    {
        long double prod_a = pow(-1, i) / pow(2, 10*i);
        long double prod_b =
                -32.0/(4.0*i+1) 
                -1.0/(4.0*i+3.0) 
                +256.0/(10.0*i+1.0) 
                -64.0/(10.0*i+3.0)
                -4.0/(10.0*i+5.0) 
                -4.0/(10.0*i+7.0)
                +1.0/(10.0*i+9.0);
        sum += prod_a * prod_b;
 
        res = sum / 64;
    }
    return res;
}

double leubniz(unsigned int n){
     double res = 0.0;
     for(unsigned int i = 0; i < n; i++){       
	  res += 1.0 / (2 * i + 1) * (i % 2 ? -1 : 1);  
     }
     return res * 4.0;
}

void task3(unsigned int n){
     struct timespec start, end;
     
     clock_gettime(CLOCK_REALTIME, &start);
     printf("leubniz: %30.32f\n", leubniz(n));
     clock_gettime(CLOCK_REALTIME, &end);
     
     printf("in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
     
     clock_gettime(CLOCK_REALTIME, &start);
     printf("bellard: %30.32f\n", bellard(n/40.0));
     clock_gettime(CLOCK_REALTIME, &end);

     printf("in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
}

struct pi_struct{
     unsigned int start;
     unsigned int end;
     double res;
};



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
     printf("posix leubniz: %30.32f\n", res*4.0);
     
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
     printf("OMP leubniz: %30.32f\n", res*4.0);
     
     clock_gettime(CLOCK_REALTIME, &end);
     printf("in %fs\n", (end.tv_sec - start.tv_sec)
	    + (end.tv_nsec - start.tv_nsec) / 1e9);
}


//$ gcc -g -Wall -fopenmp -pthread -lm main.c -lpthread -o main && ./main
int main (int argc, char *argv[]) {
     printf("BEGIN\n");
     const unsigned int n = 2e9;
     task1();
     task2();
     task3(n);
     task6(n);
     task8(n);
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
