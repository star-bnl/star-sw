/*** contains definitions for user buffers */

#include        "eg_event_str_def.h"

/* Event user buffer */

#define         N_KEYS_PER_SUBSYSTEM    8
#define         N_SUBSYSTEMS_UBUF       16

typedef struct ubuf_event_t
{
struct  eg_run_t        eg_run;         /* run information from event gen. */
struct  eg_event_t      eg_event;       /* event information from event gen. */

/* run related stuff */
char    author[80];
char    machine[80];
int     date;
int     time;

float   geant_version;
int     ge_run;
int     ge_rndm_run[2];
int     events_ordered;

/* event related stuff */
int     ge_rndm_evt[2];
int     n_event;

/* geometry information */
int     cav;
int     tpc;
int     svt;
int     pid;
int     ctf;
int     mag;
int     emc;
int     vpd;
int     xtp;
int     mlt;
int     xx1;
int     xx2;

/* event information */
int     n_shtk_evt;
int     mx_shtk_itra;
int     nw_shtk;

/* pointers for event chunking */
int     first_eg_vert_sub;
int     first_eg_track_sub;    

/* test of length of structure (should be last element) */
int     equals_99999;
} ubuf_event_t;


/* Vertex user buffer */

typedef struct ubuf_vert_t
{
int     ge_process;     /* geant mechanism */
int     ge_medium;      /* geant medium */
int     ge_volume;      /* geant volume */
float   ge_tof;         /* geant tof */
float   eg_x;           /* event gen position vector */
float   eg_y;           /* event gen position vector */
float   eg_z;           /* event gen position vector */
float   eg_tof;         /* event gen tof */
int     eg_label;       /* event gen vertex label */
int     eg_process;     /* event gen process */
int     eg_n_parent;    /* event gen number of parent tracks */
int     eg_parent_p;    /* event gen pointer to first parent track */
int     eg_n_final;     /* event gen number of final state tracks */
int     eg_final_p;     /* event gen pointer to first final state track */
int     eg_n_nfinal;    /* event gen number of not-final state tracks */
int     eg_nfinal_p;    /* event gen pointer to first not-final state track */
int     eg_is_itrmd;    /* flags if event generator vertex is intermediate */
/* test of length of structure (should be last element) */
int     equals_99999;
} ubuf_vert_t;

/* Track (kine) user buffer */

typedef struct ubuf_kine_t
{
int     eg_label;                /* event generator label */
int     eg_pid;                  /* event gen pid */
int     eg_start_vert_p;         /* event gen p_start_vertex */
int     eg_stop_vert_p;          /* event gen p_stop_vertex */
int     eg_itrmd_vert_p;         /* event gen p_first_intermediate_vertex */
int     eg_next_parent_p;        /* pointer to next parent of same stop vert */
/* test of length of structure (should be last element) */
int     equals_99999;
} ubuf_kine_t;
