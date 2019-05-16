#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define SIZE 3

int main(){
	int rank,size;
	MPI_Init(NULL,NULL);
	MPI_Comm_size(MPI_COMM_WORLD,&size);
	MPI_Comm_rank(MPI_COMM_WORLD,&rank);
	MPI_Status status;
	int i;
	int next_rank, prev_rank;

	int *sendbuf,*recvbuf;
	if ( NULL == (sendbuf = malloc( 10 * sizeof (int) ))) exit(1);		
	if ( NULL == (recvbuf = malloc( 10 * sizeof (int) ))) exit(1);		
	
	for ( i = 0 ; i < SIZE; i++) {
		sendbuf[i] = rank;
		recvbuf[i] = -100;
	}

	next_rank = (rank < size -1) ? rank + 1 : MPI_PROC_NULL;	
	prev_rank = (rank >    0   ) ? rank - 1 : MPI_PROC_NULL;	

	for ( i = 0 ; i < SIZE; i++)
	printf("[Before] Send Rank: %d\tRecv Rank: %d\n",sendbuf[i],recvbuf[i]);
/*
 *        int MPI_Sendrecv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
 *                    int dest, int sendtag, void *recvbuf, int recvcount,
 *                                MPI_Datatype recvtype, int source, int recvtag,
 *                                            MPI_Comm comm, MPI_Status *status)
 *                                            */	
	MPI_Sendrecv( sendbuf, SIZE, MPI_INT, next_rank, 0, recvbuf, SIZE, MPI_INT, prev_rank, MPI_ANY_TAG, MPI_COMM_WORLD, &status );

	for ( i = 0 ; i < SIZE; i++)
	printf("[After] Send Rank: %d\tRecv Rank: %d\n",sendbuf[i],recvbuf[i]);

	
	free(sendbuf);
	free(recvbuf);

	MPI_Finalize();
	return 0;
}
