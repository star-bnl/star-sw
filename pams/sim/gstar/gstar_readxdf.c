/* FILE:           gstar_readxdf.c
   DESCRIPTION:    Read in Event Generator XDF files in GSTAR
   Copyright 1997, Lawrence Berkeley National Laboratory
*/
#include <stdio.h>
#include <stdlib.h>
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
   uint                *pp;
   size_t               nn;

#define GET(P,T,O,F) (dsFindEntry(&P,d,T)                && \
                      dsTableDataAddress((char**)(&O),P) && \
                      dsTableRowCount(&F,P))
/*----------------------------------------------------------------------------*/
void gstar_readxdf_()     { printf("  Gstar xdf format readout activated \n"); }
/*----------------------------------------------------------------------------*/
void xdf_read_(igate)
int  *igate;
{  int    i,nv,nt,iv,jv,id; 
   float  z[4] = {0,0,0,0}; 
   extern int gcflag_,gcbank_; 
   jv=-(*igate); nt=0; nv=0;

   if ( !xdr_dataset(&x,&d) )  
      { dsPerror("xdf_read: end of file "); *igate=-1; return; }
   /* if (particle) free(particle); */

   if ( GET(p,"particle",particle,pok) )
      { if (gcflag_) printf("  xdf_read: HEPEVNT table Npart=%d \n",pok);
        if (pok) 
        { int num[3]={1,1,1};  int L=15*pok;  int Link;
          rebank_("/EVNT/GENE/GENT*",num,&L,&Link,z,16);
          ucopy_ (particle, &gcbank_+29+Link+1, &L);
	                  *(&gcbank_+29+Link-5) = -pok;  
        }
        for (i=0; i<pok; i++)
        { if (particle[i].isthep==1)
          { apdg2gea_(&particle[i].idhep, &id );
	    if (id<=0) 
            { if (gcflag_) printf(" xdf_read: unknown particle %d %d %d \n",i,
                                    particle[i].isthep,particle[i].idhep);
              id = 1000000+particle[i].idhep;
            }
            agsvert_ ( particle[i].vhep,   z,  &jv,  z, z, &nv); 
            agskine_ ( particle[i].phep,  &id, &nv, &i, z, &nt); 
        } }
        return; 
      }   
     
   if ( !GET(g,"gener",gener,gok) || !GET(e,"event",event,eok) ||
        !GET(t,"track",track,tok) || !GET(v,"vertex",vertex,vok) )
      { dsPerror("xdf_read: format error"); *igate=-1; return; }

   for (i=0; i<gok; i++)
   {  if (gcflag_) printf(" xdf_read: GENER: %s %g %g %g %d %d %d %d \n",
	 gener[i].eg_name,gener[i].eg_version,gener[i].sqrts,gener[i].b_max,
         gener[i].east_a,gener[i].east_z,gener[i].west_a,gener[i].west_z);
   }
   for (i=0; i<eok; i++)
   {  if (gcflag_) printf(" xdf_read: EVENT: %d %g %g %d %d \n",
	 event[i].n_event,event[i].b_impact,event[i].phi_impact,
         event[i].n_track,event[i].n_vertex);
   }
   for (i=0; i<vok; i++)    
   {  iv=-(i+1);  agsvert_( vertex[i].x, &iv, &jv, z, z, &nv); }
   for (i=0; i<tok; i++)
   {  iv=-track[i].ivertex; agsvert_( z, &iv, &jv, z, z, &nv); 
      agskine_( track[i].p, &track[i].ge_pid, &nv, z, z, &nt); 
   }
}
/****************************************************************************/
void xdf_open_(file,n)
char *file;   int n;
{  char fname[256]; int i;
   if (n<1 || n>255) { printf(" error in xdf_open n= %d \n",n); return; }
   strncpy(fname,file,n); fname[n]='\0'; 

   /* trim the string as COMIS put some spaces at the end */
   for (i=0; i<=n; i++) { if (fname[i]==' ') fname[i]='\0'; } 

   if ((f=fopen(fname,"rb"))==NULL || !dsNewDataset(&d,"gstar"))
      {  dsPerror("XDF_FORMAT: error opening file");   return; }
   xdrstdio_create(&x, f, XDR_DECODE);
}
/*----------------------------------------------------------------------------*/
void xdf_next_record_(ier)
int *ier;
{
*ier=0; if (!xdr_dataset(&x,&d)) {dsPerror("xdf_read: end of file"); *ier=-1;}
}
/*----------------------------------------------------------------------------*/
void xdf_get_struct_(name,n)
char *name; int  n;
{  char sname[80];  int i;  

   if (n<1 || n>79) { printf(" xdf_get_struct error: n= %d \n",n); return; }
   strncpy(sname,name,n); sname[n]='\0'; 

   /* trim the string as COMIS put some spaces at the end */
   for (i=0; i<=n; i++) { if (sname[i]==' ') sname[i]='\0'; } 

   printf("trying to get %s \n",sname);
   if (pp)  free(pp);    /*   ???   */
   if (ptr) free(ptr);   /*   ???   */
   if (GET(ptr,sname,pp,nn)) printf("xdf_geta ok %d %d %d\n",ptr,pp,nn);
}








