#ifndef  _eg_event_str_included_
#define  _eg_event_str_included_


/************************************************************************/

typedef struct eg_run_t {   /* Event generator run structure */
   long     generator;              /* event generator identification */
   char     eg_name[32];            /* event generator name */
   float    eg_version;             /* version of event generator */
   long     eg_run;                 /* generator run number */
   long     eg_rndm[2];             /* generator random numbers */
   float    sqrts;                  /* center of mass energy */
   long     is_minbias;             /* minimum bias flag */
   float    b_min;                  /* minimum impact parameter */
   float    b_max;                  /* maximum impact parameter */
   long     east_a;                 /* projectile 1 mass number */
   long     east_z;                 /* projectile 1 charge */
   long     west_a;                 /* projectile 2 mass number */
   long     west_z;                 /* projectile 2 charge */
   long     polarization_run[10];   /* to be defined */
} eg_run_t;

/************************************************************************/

typedef struct eg_event_t {   /* Event generator event structure */
   long                  n_event;                  /* eg event number */
   float                 b_impact;                 /* actual impact parameter */
   float                 phi_impact;               /* reaction plane */
   long                  event_type;               /* trigger, minbias bkgd, cosmic, etc. */
   long                  polarization_evt[10];     /* to be defined */
   long                  n_part_prot_east;         /* number of participant protons */
   long                  n_part_neut_east;         /* number of participant neutrons */
   long                  n_part_prot_west;         /* number of participant protons */
   long                  n_part_neut_west;         /* number of participant neutrons */
   long                  n_track;                  /* # tracks */
   long                  n_vertex;                 /* # vertices */
   long                  n_fs_track;               /* # final state tracks */
   long                  n_not_fs_track;           /* # non-final state tracks */
   long                  n_primary_vertex;         /* # primary vertices */
   long                  n_fs_vertex;              /* # non-final state vertices */
   struct eg_vertex_t   *p_first_primary_vertex;   /* pointer to ll of primary vertices */
   struct eg_vertex_t   *p_first_fs_vertex;        /* pointer to ll of final state vert. */
} eg_event_t;

/************************************************************************/

typedef struct eg_track_t {   /* Event generator track structure */
   long                  label;                 /* event generator label */
   long                  eg_pid;                /* event generator id */
   long                  ge_pid;                /* GEANT id */
   float                 p[3];                  /* momentum */
   struct eg_vertex_t   *p_start_vertex;        /* pointer to start vertex */
   struct eg_vertex_t   *p_stop_vertex;         /* pointer to stop vertex */
   struct eg_track_t    *p_parent_track;        /* pointer to parent track */
   struct eg_track_t    *p_next_fs_track;       /* pointer to next final state track */
   struct eg_track_t    *p_prev_fs_track;       /* pointer to previous final state track */
   struct eg_track_t    *p_next_not_fs_track;   /* pointer to next non-fs track */
   struct eg_track_t    *p_prev_not_fs_track;   /* pointer to previous non-fs track */
} eg_track_t;

/************************************************************************/

typedef struct eg_vertex_t {   /* Event generator vertex structure */
   long                  label;                   /* event generator label */
   long                  i_eg_process;            /* event generator production process */
   float                 x[3];                    /* space point */
   float                 t;                       /* time coordinate */
   long                  n_fs_track;              /* # final state daughter tracks */
   long                  n_not_fs_track;          /* # non-final state daughter tracks */
   struct eg_track_t    *p_parent_track;          /* pointer to parent track */
   struct eg_track_t    *p_first_fs_track;        /* pointer to ll of final state tracks */
   struct eg_track_t    *p_first_not_fs_track;    /* pointer to ll of non-fs tracks */
   struct eg_vertex_t   *p_next_primary_vertex;   /* pointer to next primary vertex */
   struct eg_vertex_t   *p_prev_primary_vertex;   /* pointer to previous primary vertex */
   struct eg_vertex_t   *p_next_fs_vertex;        /* pointer to next final state vertex */
   struct eg_vertex_t   *p_prev_fs_vertex;        /* pointer to prev. final state vertex */
} eg_vertex_t;


#endif  /* _eg_event_str_included_ */
