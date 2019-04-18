/* Include Files */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "mpi.h"
/* For this self-contained example, the following macros are defined.
 * They can be defined with different values here. Or the example could
 * be expanded to take any of these as command line arguments. Of course
 * a real application would have its own set of variables and values. */
#define MY_INPUT_FILE "my_input"   /* name of input datafile */
#define MY_RESULTS_FILE "my_results" /* name of output results file */
#define MY_CHECKPOINT_FILE "my_checkpoint" /* name of checkpoint file */
#define LOCAL_SIZE 1000000L
#define STRIPE_COUNT "4"
#define STRIPE_SIZE "1048576"
#define X_DIM 30000
#define Y_DIM 20000
#define Z_DIM 10000
 /* size of local array in ints */
/* must be an ascii string */
     /* must be an ascii string */
/* size of 1st dimension */
/* size of 2nd dimension, if any */
/* size of 3rd dimension, if any */
int create_my_input_file(char *file_name, int local_size, int init_value,
    int my_rank, int comm_size)
{
  MPI_File fh;
  MPI_Info info;
  int *local_array;
  int size;
  int rc;
  int i;
  double t0,t1;
  /* Delete any existing file so striping can be set. Striping cannot
   * be changed on an existing file. */
  rc = MPI_File_delete(file_name, MPI_INFO_NULL);
  /* Create a local array that will contain this process's data.
   * In this case, every local array is the same size, though that
   * is not necessary for collective I/O to work. */
  local_array = (int*)malloc((size_t)(local_size * sizeof(int)));
  if (local_array == NULL) {
return -1; }
  /* Initialize the array with data that will serve as the input data. */
  for (i = 0; i < local_size; i++) {
    local_array[i] = init_value + i;
  }
  /* Set the stripe_count and stripe_size, that is, the striping_factor
   * and striping_unit. Both keys and values for MPI_Info_set must be
   * in the form of ascii strings. */
MPI_Info_create(&info);
  MPI_Info_set(info, "striping_factor", STRIPE_COUNT);
  MPI_Info_set(info, "striping_unit", STRIPE_SIZE);
// MPI_Info_set(info, "romio_cb_write", "disable");
  /* All processes in the application open the file. The info object
   * sets the striping information for the file. */
  MPI_Barrier(MPI_COMM_WORLD);
  t0 = MPI_Wtime();
  rc = MPI_File_open(MPI_COMM_WORLD, file_name,
      MPI_MODE_CREATE | MPI_MODE_RDWR, info, &fh);
  /* The return code will be MPI_SUCCESS if the open was successful.
   * There are a number of options for handling unsuccessful calls.
   * We will return a negative status for the caller to handle. */
  if (rc != MPI_SUCCESS) {
    return -1;
}
  /* Write the file as a collective call, with every process writing
   * a part of the file. */
  rc = MPI_File_set_view(fh, my_rank * (MPI_Offset)local_size * sizeof(int),
      MPI_INT, MPI_INT, "native", info);
  rc = MPI_File_write_all(fh, local_array, local_size, MPI_INT,
      MPI_STATUS_IGNORE);
  if (rc != MPI_SUCCESS) {
    return -1;
}
  /* Close the file. */
  rc = MPI_File_close(&fh);
  MPI_Barrier(MPI_COMM_WORLD);
  t1 = MPI_Wtime();
  /* Print file creation information. */
  if (my_rank == 0) {
    MPI_Offset size = comm_size * (MPI_Offset)LOCAL_SIZE * sizeof(int);
    double time = t1-t0;
    double mibps = (size/time)/1048576.0;
    printf("input data file '%s' created;\n"
        " file_size=%ld create_time=%f6.2 MiB/sec=%f\n",
        MY_INPUT_FILE, size, time, mibps);
}
return 0; }

int main(int argc, char **argv)
{
  MPI_Aint lb, extent;
  MPI_Datatype etype, memtype, filetype, contig;
  MPI_Offset disp;
  MPI_File in_fh;
  MPI_File out_fh;
  MPI_Info info;
  int buf[LOCAL_SIZE];
int my_rank;
int comm_size;
double t0,t1;
int stripe_count;
int init_value;
int rc;
/* MPI Initialization */
MPI_Init(&argc, &argv);
MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
/* A real application would likely already have an input data file
 * but for a self-contained example, we will create one and close it. */
init_value = my_rank * LOCAL_SIZE;
rc = create_my_input_file(MY_INPUT_FILE, LOCAL_SIZE, init_value,
    my_rank, comm_size);
if (rc != 0) {
  fprintf(stderr, "could not create input file\n");
  MPI_Abort(MPI_COMM_WORLD, 1);
}
/* Create an info object for hints. Note that striping can't be
 * changed on an existing file. */
MPI_Info_create(&info);
MPI_Info_set(info, "romio_cb_read", "enable");
/* Open the input data file. */
rc = MPI_File_open(MPI_COMM_WORLD, MY_INPUT_FILE, MPI_MODE_RDONLY,
    info, &in_fh);
if (rc != MPI_SUCCESS) {
  fprintf(stderr, "could not open input file\n");
  MPI_Abort(MPI_COMM_WORLD, 2);
}
/* Construct a datatype for distributing the input data across all
 * processes. */
MPI_Type_contiguous(LOCAL_SIZE, MPI_INT, &contig);
MPI_Type_commit(&contig);
/* Set the file view so that each process gets its portion of the
 * input data. */
disp = my_rank * LOCAL_SIZE * sizeof(int);
rc = MPI_File_set_view(in_fh, disp, contig, contig, "native", info);
if (rc != MPI_SUCCESS) {
  fprintf(stderr, "error setting file view on input file\n");
  MPI_Abort(MPI_COMM_WORLD, 3);
}
/* Read the input data file. Since we created a contiguous datatype
 * the full size of each process's local data, the count is 1. */
rc = MPI_File_read(in_fh, buf, 1, contig, MPI_STATUS_IGNORE);
if (rc != MPI_SUCCESS) {
  fprintf(stderr, "error reading input file\n");
  MPI_Abort(MPI_COMM_WORLD, 3);
}
MPI_File_close(&in_fh);
/* Compute. This is where you do your science. In this simple
 * example, we will just check that the input was read correctly and
 * then multiply by 2. */
/* At some point, or perhaps at multiple time steps, calculated results
 * would be written out. And perhaps checkpoint files would periodically
 * written out. In this simple example, we will write results just
 * once. */
/* Delete the output file if it exists so that striping can be set
 * on the output file. */
rc = MPI_File_delete(MY_RESULTS_FILE, MPI_INFO_NULL);
/* Set the striping */
/* Open the results file. */
rc = MPI_File_open(MPI_COMM_WORLD, MY_RESULTS_FILE, MPI_MODE_WRONLY |
    MPI_MODE_CREATE, MPI_INFO_NULL, &out_fh);
if (rc != MPI_SUCCESS) {
  fprintf(stderr, "could not open results file\n");
  MPI_Abort(MPI_COMM_WORLD, 3);
}
/* Set the file view for the output file. In this example, we will
 * use the same contiguous datatype as we used for reading the data
 * into local memory. A better example would be to write out just
 * part of the data, say 4 contiguous elements followed by a gap of
 * 4 elements, and repeated. */
disp = my_rank * LOCAL_SIZE * sizeof(int);
MPI_File_set_view(out_fh, disp, contig, contig, "native", MPI_INFO_NULL);
if (rc != MPI_SUCCESS) {
  fprintf(stderr, "error setting view on results file\n");
  MPI_Abort(MPI_COMM_WORLD, 4);
}
/* MPI Collective Write */
t0 = MPI_Wtime();
rc = MPI_File_write_all(out_fh, buf, 1, contig, MPI_STATUS_IGNORE);
if (rc != MPI_SUCCESS) {
  fprintf(stderr, "error writing results file\n");
  MPI_Abort(MPI_COMM_WORLD, 5);
}
/* Close Files */
MPI_File_close(&out_fh);
t1 = MPI_Wtime();
/* Print time info. */
if (my_rank == 0) {
  MPI_Offset size = comm_size * (MPI_Offset)LOCAL_SIZE * sizeof(int);
  double time = t1-t0;
  double mibps = (size/time)/1048576.0;
  printf("results file '%s' written;\n"
      " file_size=%ld write_time=%f6.2 MiB/sec=%f\n",
      MY_RESULTS_FILE, size, time, mibps);
}
  /* MPI Finalize */
  MPI_Finalize();
return 0; }
