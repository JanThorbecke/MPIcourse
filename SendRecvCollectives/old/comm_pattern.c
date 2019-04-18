#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <math.h>

/*prototypes for functions used to initialize, and check the answers */
int init_array(int *buffer,int size);
int print_errors(char *label,int error,int root);
int check_broadcast(int *buffer,int count,int root);
int check_scatter(int *buffer,int count,int root);
int check_gather(int *buffer,int count,int root);

int broadcast(int *buffer,int count,int root);
int scatter(int *sendbuffer,int count,int *recvbuffer,int root);
int gather(int *sendbuffer,int count,int *recvbuffer,int root);




int broadcast(int *buffer,int count,int root){
    /* Write this function in exercise 5a.  */
  
}



int scatter(int *sendbuffer,int count,int *recvbuffer,int root) {
    /* Write this function in exercise 5b.  */
}


int gather(int *sendbuffer,int count,int *recvbuffer,int root){
    /* Write this function in exercise 5c.  */
   
}


int main(int argc, char *argv[]){
    int rank,nprocs;
    int size,chunk_size;
    int *array,*receive_array;
    int root;
    double t1,t2;
    
    
    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);

    
    //initialize some variables and arrays
    chunk_size=100;
    size=chunk_size*nprocs;
    array=malloc(sizeof(int)*size);
    receive_array=malloc(sizeof(int)*size);

    /*functions should work with any root process*/
    root=nprocs/2;

    // (a), let's try to do the broadcast operation
    
    init_array(array,size);
    broadcast(array,size,root);
    check_broadcast(array,chunk_size,root);

    
    // (b) let's try to do the scatter operation
    init_array(array,size);
    scatter(array,chunk_size,receive_array,root);
    check_scatter(receive_array,chunk_size,root);
    

    // (c) let's try to do the gather operation
    init_array(array,size);
    gather(array,chunk_size,receive_array,root);
    check_gather(receive_array,chunk_size,root);

    MPI_Finalize();

}
int init_array(int *buffer,int size){
    int i,rank,nprocs;
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);   

    for(i=0;i<size;i++)
        buffer[i]=rank+i*nprocs;
}

int check_broadcast(int *buffer,int count,int root){
    int i,rank,nprocs;
    int error;
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);   

    error=0;
    for(i=0;i<count*nprocs;i++)
        error+=buffer[i]!=root+i*nprocs;

    print_errors("broadcast",error,root);
}
int check_scatter(int *buffer,int count,int root){
    int i,rank,nprocs;
    int error;
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);   

    error=0;
    for(i=0;i<count;i++)
        error+=buffer[i]!=root+(rank*count+i)*nprocs;

    print_errors("scatter",error,root);
}

int check_gather(int *buffer,int count,int root){
    int p,i,rank,nprocs;
    int error;
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);   

    error=0;
    if(rank==root)
        for(p=0;p<nprocs;p++)
            for(i=0;i<count;i++)
                error+=buffer[i+p*count]!=p+i*nprocs;

    print_errors("gather",error,root);
}


/*print number of errors in order*/
int print_errors(char *label,int error,int root){    
    int i,rank,nprocs;
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);   
    
    if(rank==0) {
        for(i=0;i<nprocs;i++){
            if(i>0)
                MPI_Recv(&error,1,MPI_INT,i,MPI_ANY_TAG,MPI_COMM_WORLD,MPI_STATUS_IGNORE);

                
            if(error)
                printf("%s: %d errors for rank %d",label,error,i);
            else
                printf("%s: success for rank %d",label,i);            

            if(i==root)
                printf(" (root-process)\n");
            else
                printf("\n");
        }
    }
    else{
        MPI_Send(&error,1,MPI_INT,0,10,MPI_COMM_WORLD);
    }
    return 0;
}
