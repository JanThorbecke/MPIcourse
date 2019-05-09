#include <stdio.h> 
#include <stdlib.h> 
int main(int argc, char *argv[]){ 
    int i,N; 
    double *array; 
    double sum; 
    N=100; 
    array=(double *)calloc(sizeof(double),N); 
    for(i=0;i<N;i++){ 
        array[i]=1.0; 
    } 
    sum=0; 
    for(i=0;i<N;i++){ 
        sum+=array[i]; 
    } 
    printf("Sum is %g\n",sum); 
}
