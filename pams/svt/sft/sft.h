#include <stdio.h>
#include "sft_am.h"


#define SPT_X  500
#define SPT_Z  500
#define SPT_LOOKUP_NMAX SPT_X*SPT_Z
#define PIX_NMAX SPT_LOOKUP_NMAX
#define SPT_PTR_NMAX   20000
#define VALID 0
#define INVALID -1
#define NO_STEPS 4
#define NO_EVENT 5
#define MIN_STEP_SIZE 0.025


typedef struct spt_st
{
  int     wafer,
          layer;
  long    track,
          ix,
          iz,
          ii,
          off;
  float   de,
          x[3];
  double  rx,
          rz,
          xc,
          zc,
          zc0;
}  SPACEPOINT;



typedef struct spt_ref_st
{
  int   n;        /* number of points mapped at this pixel */
  int   spt[6];   /* list of points mapped at this pixel   */
  long  ii;       /* index in the lookup array */
}  SPT_REF;



typedef struct vf_ntrack_st
{
  int    prev;  /* previous */
  int    next;  /* next element*/
  int    n;     /* number of tracks found at this location */
  double z;     /* location */
}  VF_NTRACK;


int
sft_main (TABLE_HEAD_ST *sft_par_h,   SFT_PAR_ST    *sftpar,
          TABLE_HEAD_ST *spt_h,       SCS_SPT_ST    *staf_spt,
          TABLE_HEAD_ST *sft_vertex,  SFT_VERTEX_ST *vertex);
int
sft_process_event (TABLE_HEAD_ST *sft_par_h,   SFT_PAR_ST    *sftpar,
                   TABLE_HEAD_ST *spt_h,       SCS_SPT_ST    *staf_spt,
                   TABLE_HEAD_ST *sft_vertex,  SFT_VERTEX_ST *vertex);
void     sft_split_all         (void);
float    sft_vf_ntrack         (double   z0);
double   sft_vf_max            (double  *zlow,  double *zhigh);
void     sft_z_scan            (double   z_low, double  z_high);
void     sft_split_print_one   (FILE    *fout,  int     inew,   int ispt);
int      sft_which_octant      (long     ispt);
void     sft_split_one         (FILE    *fout);
void     sft_shift             (double   offset);
void     sft_analyse           (double   z);
void     sft_vf_init           (void);
FILE    *sft_open_event_file   (char    *fn);
double   sft_find_vertex       (double fZlow, double fZhigh);









