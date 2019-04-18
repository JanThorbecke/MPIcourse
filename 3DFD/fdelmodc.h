#include<stdlib.h>
#include<stdio.h>
#include<math.h>

typedef struct _compType { /* Receiver Type */
	int vz;
	int vx;
	int p;
	int txx;
	int tzz;
	int txz;
	int pp;
	int ss;
	int ud;
} compType;

typedef struct _receiverPar { /* Receiver Parameters */
	char *file_rcv;
	compType type;
	int n;
	int nt;
	int delay;
	int skipdt;
	int max_nrec;
	int *z;
	int *x;
	int *y;
	float *zr;
	float *xr;
	float *yr;
	int int_p;
	int int_vx;
	int int_vz;
	int scale;
	int sinkdepth;
	int sinkvel;
	float cp;
	float rho;
} recPar;

typedef struct _snapshotPar { /* Snapshot Parameters */
	char *file_snap;
	char *file_beam;
	compType type;
	int nsnap;
	int delay;
	int skipdt;
	int skipdz;
	int skipdx;
	int skipdy;
	int nz;
	int nx;
	int ny;
	int z1;
	int z2;
	int x1;
	int x2;
	int y1;
	int y2;
	int vxvztime;
	int beam;
} snaPar;

typedef struct _modelPar { /* Model Parameters */
	int iorder;
	int ischeme;
	int grid_dir;
	int sh;
	char *file_cp;
	char *file_ro;
	char *file_cs;
	char *file_qp;
	char *file_qs;
	float dz;
	float dx;
	float dy;
	float dt;
	float tmod;
	int nt;
	float z0;
	float x0;
	float y0;
	int nz;
	int nx;
	int ny;
	int naz;
	int nax;
	int nay;
	/* Vx: rox */
	int ioXx;
	int ioXz;
	int ieXx;
	int ieXz;
	/* Vz: roz */
	int ioZx;
	int ioZz;
	int ieZx;
	int ieZz;
	/* P, Txx, Tzz: lam, l2m */
	int ioPx;
	int ioPz;
	int iePx;
	int iePz;
	/* Txz: muu */
	int ioTx;
	int ioTz;
	int ieTx;
	int ieTz;
	float Qp;
	float Qs;
	float fw;
} modPar;

typedef struct _waveletPar { /* Wavelet Parameters */
	char *file_src; /* general source */
	char *file_Fx; /* Horizontal Particle Velocity Source */
	char *file_Fz; /* Vertical   Particle Velocity Source */
	int nt;
	int nx;
	int ny;
	float dt;
	float fmax;
	int random;
	int seed;
	int nst;
	size_t *nsamp;
} wavPar;

typedef struct _sourcePar { /* Source Array Parameters */
	int n;
	int type;
	int orient;
	int *z;
	int *x;
	int *y;
	int single;	
	int plane;
	int circle;
	int array;
	int random;
	float *tbeg;
	float *tend;
	int multiwav;
	float angle;
	float velo;
	float amplitude;
	int distribution;
	int window;
    int injectionrate;
	int sinkdepth;
	int src_at_rcv; /* Indicates that wavefield should be injected at receivers */
} srcPar;

typedef struct _shotPar { /* Shot Parameters */
	int n;
	int *z;
	int *x;
	int *y;
} shotPar;

typedef struct _boundPar { /* Boundary Parameters */
	int top;
	int bot;
	int lef;
	int rig;
	float *tapz;
	float *tapx;
	float *tapy;
	float *tapxz;
	int cfree;
	int ntap;
	int *surface;
    int pml;
    float *pml_Vx;
    float *pml_nzVx;
    float *pml_nxVz;
    float *pml_nzVz;
    float *pml_nxP;
    float *pml_nzP;

} bndPar;


#if __STDC_VERSION__ >= 199901L
  /* "restrict" is a keyword */
#else
#define restrict 
#endif

