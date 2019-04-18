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



/*this is a naive implementation of  broadcast */
int broadcast(int *buffer,int count,int root){
    /* Write this function in exercise 5a */
    int i,rank,nprocs;
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);

    if(rank==root){
        for(i=0;i<nprocs;i++){
            if(i!=root)
                MPI_Send(buffer,count,MPI_INT,i,10,MPI_COMM_WORLD);
        }
    }
    else{
        MPI_Recv(buffer,count,MPI_INT,root,10,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
    }
}



int scatter(int *sendbuffer,int count,int *recvbuffer,int root) {
    /* Write this function in exercise 5b */
    int i,rank,nprocs;
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);

    if(rank==root){
        for(i=0;i<nprocs;i++){
            if(i!=root)
                MPI_Send(sendbuffer+i*count,count,MPI_INT,i,11,MPI_COMM_WORLD);
        }

        for(i=0;i<count;i++){
            recvbuffer[i]=sendbuffer[i+count*root];
        }
    }
    else{
        MPI_Recv(recvbuffer,count,MPI_INT,root,11,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
    }
  
    
}


int gather(int *sendbuffer,int count,int *recvbuffer,int root){
    /* Write this function in exercise 5c */
    int i,rank,nprocs;
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    
    if(rank!=root){
        MPI_Send(sendbuffer,count,MPI_INT,root,12,MPI_COMM_WORLD);
    }
    else {
        for(i=0;i<nprocs;i++){
            if(i!=root)
                MPI_Recv(recvbuffer+i*count,count,MPI_INT,i,12,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
            
        }
        for(i=0;i<count;i++){
            recvbuffer[i+count*root]=sendbuffer[i];
        }
    }
}



/* This is a tree based algorithm for broadcast. It's much faster and
  * better scaling than the naive one. */

int broadcast_tree(int *buffer,int count,int root){
    /* Write this function in exercise 5a */
    int i,rank,nprocs;
    int group_size=8;
    int group_master;
    int depth=0;
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    int nprocs_2pot;
    int haveData=0;

/*  Here we compute what power of two is larger, or equal to nprocs */

    /*true if nprocs is a power of two*/
    if((nprocs & (nprocs - 1)) == 0)
        nprocs_2pot=nprocs;
    else
        nprocs_2pot=pow(2,(int)(log(nprocs)/log(2.0))+1);


    /*
      First the root process sends data to rank 0. The tree thereafter starts from
      rank 0. This is just to make the implementation easier, we loose one level of
      the tree in this way...
      If root is 0, then we can start immediately with normal algorithm
    */
    
    if(rank==root) {
        if(root!=0)
            MPI_Send(buffer,count,MPI_INT,0,0,MPI_COMM_WORLD); 
        else{
            haveData=1;
            depth=1;
        }
    }
    while(1){
        if(haveData){
            int dest;
            /*here we use nprocs_2pot as we otherwise miss some processes
              need to check that dest is less than nprocs
            */
            dest=rank+nprocs_2pot/(int)(pow(2.0,depth));
            if(dest==rank)
                break;
            if(dest<nprocs)
                MPI_Send(buffer,count,MPI_INT,dest,depth,MPI_COMM_WORLD); 
        }
        else {
            MPI_Status stat;
            MPI_Recv(buffer,count,MPI_INT,MPI_ANY_SOURCE,MPI_ANY_TAG,MPI_COMM_WORLD,&stat);
            depth=stat.MPI_TAG; //trick, use tag to transfer information (here depth)
            haveData=1;
        }
        depth++;
    }
    
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
    chunk_size=10000;
    size=chunk_size*nprocs;
    array=malloc(sizeof(int)*size);
    receive_array=malloc(sizeof(int)*size);

    root=nprocs/2;

    /* (a), let's try to do the broadcast operation. */
    
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
