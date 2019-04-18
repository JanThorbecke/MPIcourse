#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <mpi.h>

int main(int argc,char *argv[]){
    int i, myid, nproc;
    int *buf;
    int bufsize;
    
    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);
    MPI_Comm_size(MPI_COMM_WORLD,&nproc);

    bufsize = 100 / nproc;
    assert(bufsize * nproc == 100);
    buf = (int *) malloc(bufsize * sizeof(int));
    for (int i=0; i < bufsize; i++)
      buf[i] = myid*bufsize + i;

    FILE *fp;
    char filename[20];
    
    /* Open file */
    sprintf(filename,"test_%04d.dat",myid);
    fp=fopen(filename,"w");
    /* Write */
    fwrite(buf,sizeof(int),bufsize,fp);
    /* Close the file */
    fclose(fp);

    MPI_Finalize();

}
