/*:>--------------------------------------------------------------------
**: FILE:       tte_hit_match.c.
**: HISTORY:
**:             00jan93-v000a-hpl- Created by stic Version
**:  Id: idl.y,v 1.11 1998/06/23 15:52:43 ward Exp  
**:<------------------------------------------------------------------*/
#include "tte_hit_match.h"
#include <math.h>


long type_of_call tte_hit_match_(
  TABLE_HEAD_ST        *g2t_hit_h,    G2T_TPC_HIT_ST          *g2t_hit ,
  TABLE_HEAD_ST      *tpc_index_h,    TCL_TPC_INDEX_ST        *tpc_index ,
  TABLE_HEAD_ST *tpc_index_type_h,    TCL_TPC_INDEX_TYPE_ST   *tpc_index_type ,
  TABLE_HEAD_ST          *tphit_h,    TCL_TPHIT_ST            *tphit )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    tte_hit_match_
**: DESCRIPTION: This module matches generated and reconstructed hits 
**:              based on proximity in space.
**: AUTHOR:     is Iwona Sakrejda, isakrejda2lbl.gov
**: ARGUMENTS:
**:       IN:
**:            g2t_hit    - geant tpc hits
**:           g2t_hit_h   - header Structure for g2t_hit
**:     tpc_index_type    - types of possible matches
**:    tpc_index_type_h   - header Structure for tpc_index_type
**:              tphit    - reconstructed tpc hits
**:             tphit_h   - header Structure for tphit
**:    INOUT:
**:      OUT:
**:          tpc_index    - matching between geant and reconstructed hits
**:         tpc_index_h   - header Structure for tpc_index
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
#define tls_qsort_herb_i_ F77_NAME(tls_qsort_herb_i,TLS_QSORT_HERB_I)
extern void type_of_call tls_qsort_herb_i_(long *, long *,long *,long *);

#define tls_quick_sort_r_ F77_NAME(tls_quick_sort_r,TLS_QUICK_SORT_R)
extern void type_of_call tls_quick_sort_r_(long *, float *,float *,long *);

#define vzero_ F77_NAME (vzero,VZERO)
extern void type_of_call vzero_(long *,long *);

#define MAX_ROW    45          /* Number of TPC rows*/
#define MAX_SECTOR 24          /* Number of TPC sectors*/
#define LIMIT      4.0         /* Maximum alowable difference in z position*/
#define DIST_MAX   20.0        /* Maximum allowable distance sqared*/

long total_len=MAX_ROW*MAX_SECTOR;
long count_hit_rec[MAX_ROW][MAX_SECTOR]; /*count of reconstructed hits per row/sector*/
long count_hit_g2t[MAX_ROW][MAX_SECTOR]; /*count of generated hits per row/sector*/
long current_sector_row;                 /* 100*sector+row currently being processed*/

/*loop iterators*/
long k;
long l;
long i;
long j;

long start_p_g2t; /* Beginning of mc hits in g2t_hit structure for current sector/row */
long start_p_rec; /* Beginning of reconstructed hits in tphit structure for current sector/row*/
long row_id;
long sector_id;
long id_current; /* id of a reconstructed hit currently closest to the generated one*/
long g2t_ent; /*counts geant hits on real padrows*/

/* coordinates of current geant hit*/
float xg;
float yg;
float zg;
/* coordinates of current reconstructed hit*/
float xr;
float yr;
float zr;

float dist2; /* sqared distance between the reconstructed and generated hit*/
float dist_current; /* current sqared smallest distance between the geant and reconstructed hit*/


   
/* Sort all the reconstructed hits according to sector/row*/
    tls_qsort_herb_i_(&tphit_h[0].nok, &tphit[0].row,
		      &tphit[1].row,&tphit[0].cluster);

/* Sort all the geant hits according to sector/row*/
    tls_qsort_herb_i_(&g2t_hit_h[0].nok, &g2t_hit[0].volume_id,
		      &g2t_hit[1].volume_id,&g2t_hit[0].id);

/* Clear counter table */
    vzero_(&count_hit_rec[0][0],&total_len);
    vzero_(&count_hit_g2t[0][0],&total_len);

/* Setup pointers to rows and sectors and count number of hits
   for generated */

    current_sector_row=0;

    for (k=0; k<g2t_hit_h[0].nok; k++){

      row_id=g2t_hit[k].volume_id%100-1;
      sector_id=g2t_hit[k].volume_id/100-1;
      if(sector_id>MAX_SECTOR-1) break;

      if(current_sector_row==g2t_hit[k].volume_id)
	count_hit_g2t[row_id][sector_id]++;
      else{
	current_sector_row=g2t_hit[k].volume_id;
	count_hit_g2t[row_id][sector_id]=1;
      }

    }

    /* and reconstructed hits */
    current_sector_row=0;

    for (k=0; k<tphit_h[0].nok; k++){

      row_id   =tphit[k].row%100-1;
      sector_id=tphit[k].row/100-1;

      if(current_sector_row==tphit[k].row)
	count_hit_rec[row_id][sector_id]++;
      else{
	current_sector_row=tphit[k].row;
	count_hit_rec[row_id][sector_id]=1;
      }

    }
    /*sort each row in z, for both generated*/

    /*    start_p_g2t=0;
    for (k=0; k<MAX_SECTOR; k++){
      for (l=0; l<MAX_ROW; l++){
        if(count_hit_g2t[l][k]>1){
	tls_quick_sort_r_(&count_hit_g2t[l][k],&g2t_hit[start_p_g2t].x[2],
			  &g2t_hit[start_p_g2t+1].x[2],&g2t_hit[start_p_g2t].id);
	}
	start_p_g2t += count_hit_g2t[l][k];
      }
    }
    */

    /* and reconstructed hits*/

    start_p_g2t=0;
    start_p_rec=0;
    g2t_ent=0;
    for (k=0; k<MAX_SECTOR; k++){
      for (l=0; l<MAX_ROW; l++){
	/*        if(count_hit_rec[l][k]>1){
	tls_quick_sort_r_(&count_hit_rec[l][k],&tphit[start_p_rec].z,
			  &tphit[start_p_rec+1].z,&tphit[start_p_rec].cluster);
			  }*/
	/* now that hits are sorted, loop over both the generated and reconstructed hits and findthe closest one */
	for(i=0;i<count_hit_g2t[l][k];i++){
	  /*for each generated hit loop over all the reconstructed hits
(within some limits!) and find the closest one*/
	  xg=g2t_hit[start_p_g2t+i].x[0];
          yg=g2t_hit[start_p_g2t+i].x[1];
          zg=g2t_hit[start_p_g2t+i].x[2];
          dist_current=DIST_MAX;
	  id_current=0;
	  for(j=0;j<count_hit_rec[l][k];j++){
	    /*calculate the distance*/
            xr=tphit[start_p_rec+j].x;
            yr=tphit[start_p_rec+j].y;
            zr=tphit[start_p_rec+j].z;
	    if(fabs(zg-zr)>LIMIT){
	      continue;
	    }
	    else{
	      dist2=(xg-xr)*(xg-xr)+(yg-yr)*(yg-yr)+(zg-zr)*(zg-zr);
	      if(dist2<dist_current){
	        dist_current=dist2;
	        id_current=tphit[start_p_rec+j].id;
	      }
	    }
	  } /* end of loop over reconstructed hits, if i_current>-1, there was 
a match for id_current, now we fill the index table*/
	  tpc_index[g2t_ent].type=tpc_index_type[0].tphit_mhitstpc;
          tpc_index[g2t_ent].key1=g2t_hit[start_p_g2t+i].id;
	  tpc_index[g2t_ent].key2=id_current;
          g2t_ent++;
	}
	start_p_rec += count_hit_rec[l][k];
        start_p_g2t += count_hit_g2t[l][k];
      }
    }
   tpc_index_h[0].nok=g2t_ent;
   return STAFCV_OK;
}
