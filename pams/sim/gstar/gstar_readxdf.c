/* FILE:           gstar_readxdf.c
   DESCRIPTION:    Read in Event Generator XDF files in GSTAR
   Copyright 1997, Lawrence Berkeley National Laboratory
*/
#include <stdio.h>
#include <stdlib.h>
#include "dsxdr.h"
#include "dscodes.h"
#include "dstype.h"
   static FILE         *f=NULL; /* input file      */
   static XDR           x;      /* XDR stream      */
   static DS_DATASET_T *d=NULL; /* "root" dataset  */
#include "eg_gener.h"
   static DS_DATASET_T *g=NULL; /* generator table */
   EG_GENER_ST         *gener;	/* data structure  */
   size_t              gok;	/* size of table (# of rows) */
#include "eg_event.h"
   static DS_DATASET_T *e=NULL; /* event table     */
   EG_EVENT_ST         *event;	/* data structure  */
   size_t              eok;	/* size of table (# of rows) */
#include "eg_track.h"
   static DS_DATASET_T *t=NULL; /* track table     */
   EG_TRACK_ST         *track;	/* data structure  */
   size_t              tok;	/* size of table (# of rows) */
#include "eg_vertex.h"
   static DS_DATASET_T *v=NULL;	/* vertex table */
   EG_VERTEX_ST        *vertex;	/* data structure */
   size_t              vok;	/* size of table (# of rows) */
#include "particle.h"
   static DS_DATASET_T *p=NULL;	  /* HEPEVNT table */
   PARTICLE_ST         *particle; /* data structure */
   size_t              pok;	  /* size of table (# of rows) */
/*generic io*/
   static DS_DATASET_T *ptr;
   unsigned int        *pp;
   size_t               nn;

#define GET(P,T,O,F)  (dsFindEntry(&P,d,T)                && \
                       dsTableDataAddress((char**)(&O),P) && \
                       dsTableRowCount(&F,P))
#define GETT(P,S,O,F) (dsFindTab(&P,d,S)                  && \
                       dsTableDataAddress((char**)(&O),P) && \
                       dsTableRowCount(&F,P))
typedef struct cparticle_st {
      long  isthep;   /* status code of the entry */
      long  idhep;    /* particle identity, accordingly to the PDG standard*/
      long  jmohep[2];/* pointer(s) to position where the mother(s) stored */
      long  jdahep[2];/* pointers to position of the first/last daughter   */
      double phep[5]; /* p4 and mass (GeV) */
      double vhep[4]; /* production vertex (mm) and time (mm/c) */
} CPARTICLE_ST;

 void rebank_(char *,void*,void *,void*,void*,int);
 void ucopy_ (void*,void*,int*);
 void apdg2gea_(void*,void*); 
 void agsvert_ (void*,void*,void*,void*,void*,void*); 
 void agskine_ (void*,void*,void*,void*,void*,void*); 
 int dsFindTab (DS_DATASET_T **ppEntry, DS_DATASET_T* pDataset, char* spec);

/*----------------------------------------------------------------------------*/
void gstar_readxdf_()     { printf("  Gstar xdf format readout activated \n"); }
/*----------------------------------------------------------------------------*/
void xdf_skip_(igate)
int  *igate;
{  
   if ( !xdr_dataset(&x,&d) )  
      { dsPerror("xdf_read: end of file "); *igate=-1; return; }
}
/*----------------------------------------------------------------------------*/

int dummy_xdr(void)
{
    return 0;
}







