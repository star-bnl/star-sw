/****************************************************************************/
/*
   This file contains the global variables for the g2t program.
   GLOBAL is defined extern in all files except g2t.c

   Author:      dhwi, 9/9/94
   History:     dhwi, 9/9/94            first version

*/
/****************************************************************************/

/* general version control */

/* Some special include files */
#include        "g2t_cfortran.h"
#include        "g2t_detector.h"
#include        "g2t_structures.h"
#include        "dscodes.h"
#include        "dstype.h"
#include        "dsxdr.h"


GLOBAL  int     Lun;                    /* fortran logical unit to read from */

GLOBAL  FILE    *xdro_fp;               /* DSL output file pointer */

GLOBAL  XDR     Xdr_out;                /* XDR output construct */

GLOBAL  struct  detector        STAR;   /* this contains all the names */

GLOBAL  struct  hit_table       Hit_Table[N_TABLES]; /* table info */

GLOBAL  int     Printon;                /* verbose message flag */

GLOBAL  int     N_chunks, I_chunk;      /* take care of the event chunking */

GLOBAL  int     N_tot_show, N_max_show;         /* shower tracks */

GLOBAL  int     N_track_tot, N_vertex_tot;      /* the total for the event */
GLOBAL  int     Book_track, Book_vertex;        /* originally booked */

GLOBAL  char    infname[255], outfname[255];    /* filenames */

/***************************************/
/*  DSL objects                        */
/***************************************/

/* structures that are filled once per run go into this dataset */

GLOBAL  DS_DATASET_T                    *RunDs_p;   /* Run dataset pointer */
#define         RUNDS_LEN       5+1

GLOBAL  G2T_RUN_ST           *Run;           /* pointer to run structure */
        
GLOBAL  G2T_GEPART_ST        *Part_ge;       /* pointer to GEANT particles */

/*GLOBAL  G2t_egpart       *Part_eg; */ /* pointer to generator particles */

GLOBAL  G2T_DECAY_MODE_ST    *Decay_mode;    /* decay modes */

GLOBAL  G2T_DECAY_DAUGHTER_ST *Decay_daughter; /* decay daughters */

/* structures that are filled every event go into this dataset */

GLOBAL  DS_DATASET_T    *EventDs_p;     /* Event dataset pointer */
#define         EVENTDS_LEN     4+N_TABLES+1

GLOBAL  DS_DATASET_T    *TrackDs_p;     /* Track table pointer */
                        
GLOBAL  DS_DATASET_T    *VertexDs_p;    /* Vertex table pointer */
                        
GLOBAL  G2T_SUPEREVENT_ST    *S_Event;       /* pointer to superevent struct. */

GLOBAL  G2T_EVENT_ST      *Event; /* pointer to event struct. */
        
GLOBAL  G2T_VERTEX_ST     *Vertex;/* pointer to vertex struct. */
        
GLOBAL  G2T_TRACK_ST      *Track; /* pointer to track struct. */
        
/* subevent track, vertex pointers */
GLOBAL  G2T_TRACK_ST      *Track_sub_p;
GLOBAL  G2T_VERTEX_ST     *Vertex_sub_p;


/***************************************/
/*  General defines                    */
/***************************************/

#define ipo     if (Printon) printf
