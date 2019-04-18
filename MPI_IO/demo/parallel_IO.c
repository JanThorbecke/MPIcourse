#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>
#ifdef HDF5
#include <hdf5.h>
#endif

double mpiiowriter(int *buf, long long bufsize,int myid,int nproc);
double mpiiowriter_ind(int *buf, long long bufsize,int myid,int nproc);
double mpiiowriter_interleaved(int *buf, long long bufsize,int myid,int nproc);
double mpiiowriter_coll_interleaved(int *buf, long long bufsize,int myid,int nproc);
double subsetwriter(int *buf, long long bufsize,int myid,int nproc);
double nwriter(int *buf, long long bufsize,int myid,int nproc);
double onewriter(int *buf, long long bufsize,int myid,int nproc);

double mpiiowriter_wfopen(int *buf, long long bufsize,int myid,int nproc);
double subsetwriter_wfopen(int *buf, long long bufsize,int myid,int nproc);
double nwriter_wfopen(int *buf, long long bufsize,int myid,int nproc);
double onewriter_wfopen(int *buf, long long bufsize,int myid,int nproc);

#ifdef HDF5
double hdf5writer_coll(int *buf, long long bufsize,int myid,int nproc);
double hdf5writer_ind(int *buf, long long bufsize,int myid,int nproc);
double hdf5writer_coll_interleaved(int *buf, long long bufsize,int myid,int nproc);
double hdf5writer_ind_interleaved(int *buf, long long bufsize,int myid,int nproc);
double hdf5writer_wfopen(int *buf, long long bufsize,int myid,int nproc);
#endif

int main(int argc,char *argv[]){
    int i,myid,nproc;
    int *buf;
    int algorithm;
    /*
     * long long is an integer with a size of at least 64bits, 
     * required for larger buffer sizes
     */
    long long min_bufsize,max_bufsize,bufsize;
    
    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);
    MPI_Comm_size(MPI_COMM_WORLD,&nproc);

    
    if(myid==0){
        /*
         * Read in the maximum and minimum size of buffer
         * from the command line. The input on the command line is the total amount
         * written to disk in megabytes
         * 
         * min_bufsize and max_bufsize store the size of the local buffers in
         * number of integer elements
         */
        if(argc==4){
            min_bufsize=(long long)1024*1024*atof(argv[1])/(sizeof(int)*nproc);
            max_bufsize=(long long)1024*1024*atof(argv[2])/(sizeof(int)*nproc);
            algorithm=atoi(argv[3]);
           
        }            
        else{
            printf("Usage: %s minimum_filesize maximum_filesize algorithm\n",argv[0]);
            printf("      minimum_filesize     Minimum filesize in MB\n");
            printf("      maximum_filesize     Maximum filesize in MB\n");
            printf("      Algorithm            Which Algorithm to use\n");
            printf("                              1  = One writer\n");
            printf("                              11 = One writer including fopen time\n");
            printf("                              2  = sqrt(N)-writers\n");
            printf("                              21 = sqrt(N)-writers including fopen time\n");
            printf("                              3  = N-writers\n");
            printf("                              3  = N-writers including fopen time\n");
            printf("                              4  = MPI-I/O \n");
            printf("                              41 = MPI-I/O including MPI_File_open time \n");
            printf("                              42 = MPI-I/O interleaved data with custom file view\n");
            printf("                              43 = MPI-I/O with independent I/O\n");
            printf("                              44 = MPI-I/O interleaved data with collective I/O\n");

#ifdef HDF5
            printf("                              5  = HDF5-I/O \n");
            printf("                              51  = HDF5-I/O with independent IO\n");
            printf("                              52  = HDF5-I/O interleaved data\n");
            printf("                              53  = HDF5-I/O interleaved data with independent IO\n");

#endif
            max_bufsize=-1; /*will be used to check if we need to exit*/
        }
        if(min_bufsize<0 || max_bufsize<=0){
            printf("Filesizes should be positive\n");
            max_bufsize=-1; /*will be used to check if we need to exit*/
        }
    }
    
    /*broadcast input parameters from rank 0*/
    MPI_Bcast(&min_bufsize,1,MPI_LONG_LONG_INT,0,MPI_COMM_WORLD);
    MPI_Bcast(&max_bufsize,1,MPI_LONG_LONG_INT,0,MPI_COMM_WORLD);
    MPI_Bcast(&algorithm,1,MPI_INT,0,MPI_COMM_WORLD);

    if(max_bufsize==-1){
        /*Filesize not defined, exit program */
        MPI_Finalize();
        return;
    }

    if(myid==0){
        if(algorithm==1)  printf("#One writer\n");
        if(algorithm==11)  printf("#One writer  including fopen time\n");

        if(algorithm==2)  printf("#Sqrt(N)-writers\n");
        if(algorithm==21)  printf("#Sqrt(N)-writers including fopen time\n");
        
        if(algorithm==3)  printf("#N-writers\n");
        if(algorithm==31)  printf("#N-writers including fopen time\n");
        
        if(algorithm==4)  printf("#MPI-I/O with default file view\n");
        if(algorithm==41)  printf("#MPI-I/O with default file view including fopen time\n");
        if(algorithm==42) printf("#MPI-I/O interleaved data with custome file view\n");
        if(algorithm==43) printf("#MPI-I/O with independent I/O\n");
        if(algorithm==44) printf("#MPI-I/O interleaved data with collective I/O\n");
        if(algorithm==5)  printf("#HDF5 with collective I/O\n");
        if(algorithm==51)  printf("#HDF5 with independent I/O\n");
        if(algorithm==52)  printf("#HDF5 interleaved data with collective I/O\n");
        if(algorithm==53)  printf("#HDF5 interleaved data with independent I/O\n");

        printf("#nprocs  Bytes       time(s)   Bandwidth(bytes/s)\n");
    }

    /*
     * Loop over filesizes. For each iteration the size is doubled
     */
    for(bufsize=min_bufsize;bufsize<=max_bufsize;bufsize*=2){
        double mintime=1.0; /*minimum statistics for each bufsize */
        double time;
        int count;
        int breakloop;
        /*
         * Allocate and initialize local buffer
         */

        buf=malloc(sizeof(int)*bufsize);
        for(i=0;i<bufsize;i++)
            buf[i]=myid;
        
        time=0.0;
        count=0;
        breakloop=0;
        while(!breakloop){
            switch(algorithm){
                case 1:
                    time+=onewriter(buf,bufsize,myid,nproc);
                    break;
                case 11:
                    time+=onewriter_wfopen(buf,bufsize,myid,nproc);
                    break;
                case 2:
                    time+=subsetwriter(buf,bufsize,myid,nproc);
                    break;
                case 21:
                    time+=subsetwriter_wfopen(buf,bufsize,myid,nproc);
                    break;
                case 3:
                    time+=nwriter(buf,bufsize,myid,nproc);
                    break;
                case 31:
                    time+=nwriter_wfopen(buf,bufsize,myid,nproc);
                    break;
                case 4:
                    time+=mpiiowriter(buf,bufsize,myid,nproc);
                    break;
                case 41:
                    time+=mpiiowriter_wfopen(buf,bufsize,myid,nproc);
                    break;
                case 42:
                    time+=mpiiowriter_interleaved(buf,bufsize,myid,nproc);
                    break;
                case 43:
                    time+=mpiiowriter_ind(buf,bufsize,myid,nproc);
                    break;
                case 44:
                    time+=mpiiowriter_coll_interleaved(buf,bufsize,myid,nproc);
                    break;
#ifdef HDF5
                case 5:
                    time+=hdf5writer_coll(buf,bufsize,myid,nproc);
                    break;
                case 51:
                    time+=hdf5writer_ind(buf,bufsize,myid,nproc);
                    break;
                case 52:
                    time+=hdf5writer_coll_interleaved(buf,bufsize,myid,nproc);
                    break;
                case 53:
                    time+=hdf5writer_ind_interleaved(buf,bufsize,myid,nproc);
                    break;

#endif
                default:
                    if(myid==0) printf("Algorithm not valid\n");
                    free(buf);
                    MPI_Finalize();
                    return;
            }
            count++;
            breakloop=(time>mintime)||(count>=20);
            /*rank 0 decides when to stop */
            MPI_Bcast(&breakloop,1,MPI_INT,0,MPI_COMM_WORLD);
        }
        time/=count; /*count is the number of samples, time is now the
                      * time per I/O operation
                      */
        if(myid==0) printf("%04d    %10.4g %10.4g %10.4g\n",
                           nproc,(double)bufsize*sizeof(int)*nproc,
                           time,(double)bufsize*sizeof(int)*nproc/(time));
        free(buf);
    }
    MPI_Finalize();
}

double mpiiowriter_interleaved(int *buf, long long bufsize,int myid,int nproc){
    double t1,t2;
    MPI_File fp;
    MPI_Info info;
    char tmp[20];
    MPI_Datatype filetype;
        
    MPI_Info_create(&info);

    /*
     * Give hint for how many processors should be involved in disk
     * i/o. Here chosen to be sqrt(N)
         
    sprintf(tmp,"%d",ceil(sqrt(nproc)));
    MPI_Info_set(info,"cb_nodes",tmp);
    */
 
    /*Open an MPI I/O file for writing */
    MPI_File_open(MPI_COMM_WORLD,"test.dat",
                  MPI_MODE_CREATE|MPI_MODE_WRONLY,
                  info,&fp);

    MPI_Type_vector(bufsize,1,nproc,MPI_INT,&filetype);
    MPI_Type_commit(&filetype);
    MPI_File_set_view(fp,myid*sizeof(int),MPI_INT,filetype,"native",MPI_INFO_NULL);
    
    
    MPI_Barrier(MPI_COMM_WORLD);
    /* Start to measure I/O time */
    t1=MPI_Wtime();
    
    MPI_File_write(fp,buf,bufsize,MPI_INT,MPI_STATUS_IGNORE);

    /* Stop to measure I/O time. Use barrier to make sure we get the
     * time when all processes are actually ready
     */
    
    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();
    MPI_Type_free(&filetype);
    MPI_File_close(&fp);

   
    return t2-t1;
}

double mpiiowriter_coll_interleaved(int *buf, long long bufsize,int myid,int nproc){
    double t1,t2;
    MPI_File fp;
    MPI_Info info;
    char tmp[20];
    MPI_Datatype filetype;
        
    /*
     * Give hint for how many processors should be involved in disk
     * i/o. Here chosen to be sqrt(N)
     */
    
    MPI_Info_create(&info);
    sprintf(tmp,"%d",ceil(sqrt(nproc)));
    MPI_Info_set(info,"cb_nodes",tmp);
    
 
    /*Open an MPI I/O file for writing */
    MPI_File_open(MPI_COMM_WORLD,"test.dat",
                  MPI_MODE_CREATE|MPI_MODE_WRONLY,
                  info,&fp);

    MPI_Type_vector(bufsize,1,nproc,MPI_INT,&filetype);
    MPI_Type_commit(&filetype);
    MPI_File_set_view(fp,myid*sizeof(int),MPI_INT,filetype,"native",MPI_INFO_NULL);
    
    
    MPI_Barrier(MPI_COMM_WORLD);
    /* Start to measure I/O time */
    t1=MPI_Wtime();
    
    MPI_File_write_all(fp,buf,bufsize,MPI_INT,MPI_STATUS_IGNORE);

    /* Stop to measure I/O time. Use barrier to make sure we get the
     * time when all processes are actually ready
     */
    
    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();
    MPI_Type_free(&filetype);
    MPI_File_close(&fp);

   
    return t2-t1;
}

double mpiiowriter(int *buf, long long bufsize,int myid,int nproc){
    double t1,t2;
    MPI_File fp;
    MPI_Info info;
    char tmp[20];
    
    /*
     * Give hint on how many processors should be involved in disk
     * i/o. Here chosen to be sqrt(N)
     */
    
    MPI_Info_create(&info);
    sprintf(tmp,"%d",ceil(sqrt(nproc)));
    //  MPI_Info_set(info,"cb_nodes",tmp);
//    MPI_Info_set(info,"romio_ds_read","disable");
    //   MPI_Info_set(info,"romio_ds_write","disable");
//    MPI_Info_set(info,"indr_wr_buffer_size","16777216");

    
    /*Open an MPI I/O file for writing */
    MPI_File_open(MPI_COMM_WORLD,"test.dat",
                  MPI_MODE_CREATE|MPI_MODE_WRONLY,
                  info,&fp);

    /*
      Write one integer to file to make sure that the filesystem is actually ready
      to write before we start to measure the transfer time.
    */
    MPI_File_write_at_all(fp,myid*sizeof(int),
                          buf,1,MPI_INT,MPI_STATUS_IGNORE);
    
    MPI_Barrier(MPI_COMM_WORLD);
    /* Start to measure I/O time */
    t1=MPI_Wtime();
    
    /*
      Collective I/O with the default fileview. Each process
      writes out its part bufsize*sizeof(int) bytes apart. Could
      also use MPI_File_seek followed by MPI_File_write_all
    */
    MPI_File_write_at_all(fp,myid*bufsize*sizeof(int),
                          buf,bufsize,MPI_INT,MPI_STATUS_IGNORE);
    
    /* Stop to measure I/O time. Use barrier to make sure we get the
     * time when all processes are actually ready
     */
    
    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();
    
    MPI_File_close(&fp);

   

    return t2-t1;
}

double mpiiowriter_ind(int *buf, long long bufsize,int myid,int nproc){
    double t1,t2;
    MPI_File fp;
    MPI_Info info;
    char tmp[20];
    
    /*
     * Give hint on how many processors should be involved in disk
     * i/o. Here chosen to be sqrt(N)
     */
    
    MPI_Info_create(&info);
    sprintf(tmp,"%d",ceil(sqrt(nproc)));
    //  MPI_Info_set(info,"cb_nodes",tmp);
//    MPI_Info_set(info,"romio_ds_read","disable");
    //   MPI_Info_set(info,"romio_ds_write","disable");
//    MPI_Info_set(info,"indr_wr_buffer_size","16777216");

    
    /*Open an MPI I/O file for writing */
    MPI_File_open(MPI_COMM_WORLD,"test.dat",
                  MPI_MODE_CREATE|MPI_MODE_WRONLY,
                  info,&fp);

    /*
      Write one integer to file to make sure that the filesystem is actually ready
      to write before we start to measure the transfer time.
    */
    MPI_File_write_at(fp,myid*sizeof(int),
                          buf,1,MPI_INT,MPI_STATUS_IGNORE);
    
    MPI_Barrier(MPI_COMM_WORLD);
    /* Start to measure I/O time */
    t1=MPI_Wtime();
    
    /*
      Independent I/O with the default fileview. Each process
      writes out its part bufsize*sizeof(int) bytes apart. Could
      also use MPI_File_seek followed by MPI_File_write_all
    */
    MPI_File_write_at(fp,myid*bufsize*sizeof(int),
                          buf,bufsize,MPI_INT,MPI_STATUS_IGNORE);
    
    /* Stop to measure I/O time. Use barrier to make sure we get the
     * time when all processes are actually ready
     */
    
    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();
    
    MPI_File_close(&fp);

   

    return t2-t1;
}

double mpiiowriter_wfopen(int *buf, long long bufsize,int myid,int nproc){
    double t1,t2;
    MPI_File fp;
    MPI_Info info;
    char tmp[20];

    MPI_Barrier(MPI_COMM_WORLD);
    /* Start to measure I/O time */
    t1=MPI_Wtime();
    
    /*
     * Give hint on how many processors should be involved in disk
     * i/o. Here chosen to be sqrt(N)
     */


    
    MPI_Info_create(&info);
    sprintf(tmp,"%d",ceil(sqrt(nproc)));
    MPI_Info_set(info,"cb_nodes",tmp);
    
    
    /*Open an MPI I/O file for writing */
    MPI_File_open(MPI_COMM_WORLD,"test.dat",
                  MPI_MODE_CREATE|MPI_MODE_WRONLY,
                  info,&fp);
        
  
    
    /*
      Collective I/O with the default fileview. Each process
      writes out its part bufsize*sizeof(int) bytes apart. Could
      also use MPI_File_seek followed by MPI_File_write_all
    */
    MPI_File_write_at_all(fp,myid*bufsize*sizeof(int),
                          buf,bufsize,MPI_INT,MPI_STATUS_IGNORE);

    
    MPI_File_close(&fp);

    /* Stop to measure I/O time. Use barrier to make sure we get the
     * time when all processes are actually ready
     */
    
    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();


    return t2-t1;
}
double nwriter(int *buf, long long bufsize,int myid,int nproc){
    double t1,t2;
    int i;
    FILE *fp;
    char fname[20];

    sprintf(fname,"test_%04d.dat",myid);
    fp=fopen(fname,"w");
   
    /*
      Write one integer to file to make sure that the filesystem is actually ready
      to write before we start to measure the transfer time.
    */
    fwrite(buf,sizeof(int),1,fp); 
    
    MPI_Barrier(MPI_COMM_WORLD);
    t1=MPI_Wtime();

    fwrite(buf,sizeof(int),bufsize,fp);

    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();

    fclose(fp);

    return t2-t1;
}

double nwriter_wfopen(int *buf, long long bufsize,int myid,int nproc){
    double t1,t2;
    int i;
    FILE *fp;
    char fname[20];



    MPI_Barrier(MPI_COMM_WORLD);
    t1=MPI_Wtime();
    
    sprintf(fname,"test_%04d.dat",myid);
    fp=fopen(fname,"w");
    fwrite(buf,sizeof(int),bufsize,fp);
    fclose(fp);

    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();

    return t2-t1;
}


void aggregated_write(int *buf, long long bufsize,int myid,int nproc,MPI_Comm comm, FILE *fp){
    int *writebuf,*recvbuf,*swap,*org_recvbuf;
    int i;
    MPI_Request request;

  
    if(myid==0) {
        writebuf=buf;
        recvbuf=malloc(sizeof(int)*bufsize);
        org_recvbuf=recvbuf; /*store original pointer so that it can easily be freed */
        
        for(i=1;i<nproc;i++){
            /*receive from rank i */
            MPI_Irecv(recvbuf,bufsize,MPI_INT,i,1,comm,&request);
            /*write out buffer */
            fwrite(writebuf,sizeof(int),bufsize,fp);
            /*wait for send from rank i to end */
            MPI_Wait(&request,MPI_STATUS_IGNORE);
            /*swap writebuf and recvbuf*/
            swap=writebuf;
            writebuf=recvbuf;
            recvbuf=swap;
        }
        fwrite(writebuf,sizeof(int),bufsize,fp);
        free(org_recvbuf);
    }
    else{
        double temp;
        /*send data to rank 0, ssend to avoid overloading root process */
        MPI_Ssend(buf,bufsize,MPI_INT,0,1,comm);
    }
    
}
    

double onewriter(int *buf, long long bufsize,int myid,int nproc){
    double t1,t2;
    int i;
    FILE *fp;

    
    if(myid==0) {
        int temp=0;
        fp=fopen("test.dat","w");
        /*
          Write one integer to file to make sure that the filesystem is actually ready
          to write before we start to measure the transfer time.
        */
        fwrite(&temp,sizeof(int),1,fp);
    }
        
    
    MPI_Barrier(MPI_COMM_WORLD);
    t1=MPI_Wtime();
    aggregated_write(buf,bufsize,myid,nproc,MPI_COMM_WORLD,fp);
    
    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();
    
    if(myid==0) {
         fclose(fp);
    }
    
    return t2-t1;
}


double onewriter_wfopen(int *buf, long long bufsize,int myid,int nproc){
    double t1,t2;
    int i;
    FILE *fp;
    
    MPI_Request request;
    
    MPI_Barrier(MPI_COMM_WORLD);
    t1=MPI_Wtime();
    
    if(myid==0) {
        int temp;
        fp=fopen("test.dat","w");
    }

    aggregated_write(buf,bufsize,myid,nproc,MPI_COMM_WORLD,fp);
    
    if(myid==0) {
         fclose(fp);
    }
    
    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();
    
    
    return t2-t1;
}




double subsetwriter(int *buf, long long bufsize,int myid,int nproc){
    double t1,t2;
    int procs_per_writer;
    int i;
    int io_myid,io_nproc;
    MPI_Comm io_comm;
    FILE *fp;
    char filename[20];
    MPI_Request request;
    
    procs_per_writer=nproc/sqrt((double)nproc);

    /*
     * Create new communicator io_comm that will be used to do a
     * subset of the I/O. Each io_comm will do a one writer like I/O
     * operation where rank 0 in io_comm writes out a file and the
     * other ranks in io_comm send their data to it.
     */
    MPI_Comm_split(MPI_COMM_WORLD,myid/procs_per_writer,0,&io_comm);
    MPI_Comm_rank(io_comm,&io_myid);
    MPI_Comm_size(io_comm,&io_nproc);


    /* Open file and initialize buffers */
    if(io_myid==0) {
        int temp;
        sprintf(filename,"test_%04d.dat",myid/procs_per_writer);
        fp=fopen(filename,"w");
        /*
          Write one integer to file to make sure that the filesystem is actually ready
          to write before we start to measure the transfer time.
        */
        fwrite(&temp,sizeof(int),1,fp);
    }
    
    /*start to measure time for I/O */
    MPI_Barrier(MPI_COMM_WORLD);
    t1=MPI_Wtime();

    /*do aggregated I/O with io_comm */
    aggregated_write(buf,bufsize,io_myid,io_nproc,io_comm,fp);
    
    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();
    
     
     if(io_myid==0) {
        fclose(fp);
    }
    
    
    MPI_Comm_free(&io_comm);
    return t2-t1;
}




double subsetwriter_wfopen(int *buf, long long bufsize,int myid,int nproc){
    double t1,t2;
    int procs_per_writer;
    int i;
    int io_myid,io_nproc;
    MPI_Comm io_comm;
    FILE *fp;
    char filename[20];
    MPI_Request request;
    
    procs_per_writer=nproc/sqrt((double)nproc);

    /*
     * Create new communicator io_comm that will be used to do a
     * subset of the I/O. Each io_comm will do a one writer like I/O
     * operation where rank 0 in io_comm writes out a file and the
     * other ranks in io_comm send their data to it.
     */
    MPI_Comm_split(MPI_COMM_WORLD,myid/procs_per_writer,0,&io_comm);
    MPI_Comm_rank(io_comm,&io_myid);
    MPI_Comm_size(io_comm,&io_nproc);

    /*start to measure time for I/O */
    MPI_Barrier(MPI_COMM_WORLD);
    t1=MPI_Wtime();


    /* Open file and initialize buffers */
    if(io_myid==0) {
        int temp;
        sprintf(filename,"test_%04d.dat",myid/procs_per_writer);
        fp=fopen(filename,"w");
    }
    

    /*do aggregated I/O with io_comm */
    aggregated_write(buf,bufsize,io_myid,io_nproc,io_comm,fp);

    if(io_myid==0) {
        fclose(fp);
    }
        
    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();
    
     

    
    MPI_Comm_free(&io_comm);
    return t2-t1;
}

#ifdef HDF5
double hdf5writer_coll(int *buf, long long bufsize,int myid,int nproc)
{
    double t1,t2;
    hid_t file_id, dset_id;         /* file and dataset identifiers */
    hid_t filespace, memspace;      /* file and memory dataspace identifiers */
    hid_t plist_id;                 /* property list identifier */
    herr_t status;

    hsize_t dimsf;
    hsize_t count;
    hsize_t offset;

    dimsf = nproc*bufsize;
    count = bufsize;
    offset = myid*bufsize;
    
    /*
     * Set up file access property list with parallel I/O access
     */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /*
     * Create a new file collectively and release property list identifier.
     */
    file_id = H5Fcreate("test.dat", H5F_ACC_TRUNC, H5P_DEFAULT, plist_id);
    H5Pclose(plist_id);

    /*
     * Create the dataspace for the dataset.
     */
    filespace = H5Screate_simple(1, &dimsf, NULL);

    /*
     * Create the dataset with default properties and close filespace.
     */
    dset_id = H5Dcreate(file_id, "testdata", H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Sclose(filespace);

    memspace = H5Screate_simple(1, &count, NULL);

    /*
     * Select hyperslab in the file.
     */
    filespace = H5Dget_space(dset_id);
    H5Sselect_hyperslab(filespace, H5S_SELECT_SET, &offset, NULL, &count, NULL);

    /*
     * Create property list for collective dataset write.
     */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE);

    MPI_Barrier(MPI_COMM_WORLD);
    /* Start to measure I/O time */
    t1=MPI_Wtime();
    status = H5Dwrite(dset_id, H5T_NATIVE_INT, memspace, filespace,
		      plist_id, buf);

    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();
    /*
     * Close/release resources.
     */
    H5Dclose(dset_id);
    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Pclose(plist_id);
    H5Fclose(file_id);

    return t2-t1;

}

double hdf5writer_coll_interleaved(int *buf, long long bufsize,int myid,int nproc)
{
    double t1,t2;
    hid_t file_id, dset_id;         /* file and dataset identifiers */
    hid_t filespace, memspace;      /* file and memory dataspace identifiers */
    hid_t plist_id;                 /* property list identifier */
    herr_t status;

    hsize_t dimsf;
    hsize_t count;
    hsize_t offset;
    hsize_t stride;

    dimsf = nproc*bufsize;
    count = bufsize;
    offset = myid;
    stride = nproc;

    /*
     * Set up file access property list with parallel I/O access
     */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /*
     * Create a new file collectively and release property list identifier.
     */
    file_id = H5Fcreate("test.dat", H5F_ACC_TRUNC, H5P_DEFAULT, plist_id);
    H5Pclose(plist_id);

    /*
     * Create the dataspace for the dataset.
     */
    filespace = H5Screate_simple(1, &dimsf, NULL);

    /*
     * Create the dataset with default properties and close filespace.
     */
    dset_id = H5Dcreate(file_id, "testdata", H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Sclose(filespace);

    memspace = H5Screate_simple(1, &count, NULL);

    /*
     * Select hyperslab in the file.
     */
    filespace = H5Dget_space(dset_id);
    H5Sselect_hyperslab(filespace, H5S_SELECT_SET, &offset, &stride, &count, NULL);

    /*
     * Create property list for collective dataset write.
     */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE);

    MPI_Barrier(MPI_COMM_WORLD);
    /* Start to measure I/O time */
    t1=MPI_Wtime();
    status = H5Dwrite(dset_id, H5T_NATIVE_INT, memspace, filespace,
		      plist_id, buf);

    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();
    /*
     * Close/release resources.
     */
    H5Dclose(dset_id);
    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Pclose(plist_id);
    H5Fclose(file_id);

    return t2-t1;

}

double hdf5writer_ind(int *buf, long long bufsize,int myid,int nproc)
{
    double t1,t2;
    hid_t file_id, dset_id;         /* file and dataset identifiers */
    hid_t filespace, memspace;      /* file and memory dataspace identifiers */
    hid_t plist_id;                 /* property list identifier */
    herr_t status;

    hsize_t dimsf;
    hsize_t count;
    hsize_t offset;

    dimsf = nproc*bufsize;
    count = bufsize;
    offset = myid*bufsize;
    
    /*
     * Set up file access property list with parallel I/O access
     */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /*
     * Create a new file collectively and release property list identifier.
     */
    file_id = H5Fcreate("test.dat", H5F_ACC_TRUNC, H5P_DEFAULT, plist_id);
    H5Pclose(plist_id);

    /*
     * Create the dataspace for the dataset.
     */
    filespace = H5Screate_simple(1, &dimsf, NULL);

    /*
     * Create the dataset with default properties and close filespace.
     */
    dset_id = H5Dcreate(file_id, "testdata", H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Sclose(filespace);

    memspace = H5Screate_simple(1, &count, NULL);

    /*
     * Select hyperslab in the file.
     */
    filespace = H5Dget_space(dset_id);
    H5Sselect_hyperslab(filespace, H5S_SELECT_SET, &offset, NULL, &count, NULL);

    /*
     * Create property list for collective dataset write.
     */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_INDEPENDENT);

    MPI_Barrier(MPI_COMM_WORLD);
    /* Start to measure I/O time */
    t1=MPI_Wtime();
    status = H5Dwrite(dset_id, H5T_NATIVE_INT, memspace, filespace,
		      plist_id, buf);

    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();
    /*
     * Close/release resources.
     */
    H5Dclose(dset_id);
    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Pclose(plist_id);
    H5Fclose(file_id);

    return t2-t1;

}


double hdf5writer_ind_interleaved(int *buf, long long bufsize,int myid,int nproc)
{
    double t1,t2;
    hid_t file_id, dset_id;         /* file and dataset identifiers */
    hid_t filespace, memspace;      /* file and memory dataspace identifiers */
    hid_t plist_id;                 /* property list identifier */
    herr_t status;

    hsize_t dimsf;
    hsize_t count;
    hsize_t offset;
    hsize_t stride;

    dimsf = nproc*bufsize;
    count = bufsize;
    /*offset = myid*bufsize;*/
    offset = myid;
    stride = nproc;
    
    /*
     * Set up file access property list with parallel I/O access
     */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /*
     * Create a new file collectively and release property list identifier.
     */
    file_id = H5Fcreate("test.dat", H5F_ACC_TRUNC, H5P_DEFAULT, plist_id);
    H5Pclose(plist_id);

    /*
     * Create the dataspace for the dataset.
     */
    filespace = H5Screate_simple(1, &dimsf, NULL);

    /*
     * Create the dataset with default properties and close filespace.
     */
    dset_id = H5Dcreate(file_id, "testdata", H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Sclose(filespace);

    memspace = H5Screate_simple(1, &count, NULL);

    /*
     * Select hyperslab in the file.
     */
    filespace = H5Dget_space(dset_id);
    H5Sselect_hyperslab(filespace, H5S_SELECT_SET, &offset, &stride, &count, NULL);

    /*
     * Create property list for collective dataset write.
     */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_INDEPENDENT);

    MPI_Barrier(MPI_COMM_WORLD);
    /* Start to measure I/O time */
    t1=MPI_Wtime();
    status = H5Dwrite(dset_id, H5T_NATIVE_INT, memspace, filespace,
		      plist_id, buf);

    MPI_Barrier(MPI_COMM_WORLD);
    t2=MPI_Wtime();
    /*
     * Close/release resources.
     */
    H5Dclose(dset_id);
    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Pclose(plist_id);
    H5Fclose(file_id);

    return t2-t1;

}

#endif
