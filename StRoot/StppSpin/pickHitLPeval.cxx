//*-- Author : Jan Balewski
//  
// $Id: pickHitLPeval.cxx,v 1.1 2001/04/12 15:19:10 balewski Exp $
// $Log: pickHitLPeval.cxx,v $
// Revision 1.1  2001/04/12 15:19:10  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                    
//  selects hits for the given track. One routine for generated tracks
// and one for the reconstructed ones.  
//                                                                    
//////////////////////////////////////////////////////////////////////////

#include <assert.h>
#include <math.h>
#include <strings.h>

#include "StppLPevalMaker.h"
#include "tables/St_g2t_tpc_hit_Table.h" 
#include "tables/St_g2t_track_Table.h"
#include "tables/St_tcl_tphit_Table.h" 
#include "tables/St_dst_track_Table.h"

#include "tables/St_jdata1_Table.h"

//_____________________________________________________________
//_____________________________________________________________
//_____________________________________________________________
St_jdata1 *StppLPevalMaker::pickHit( g2t_track_st *tr, St_g2t_tpc_hit *gtph)
{// select G2T TPC hits belonging to  the trID geant track, from the table g2t
  assert(gtph); 
  assert(tr);

  int trID=tr->id;
  //printf("pickGTpcHit(), trID=%d, NGtphit=%d\n",trID,(int)gtph->GetNRows()); 

  char tt[20]; sprintf(tt,"uGTpcHit-%d",trID);
  St_jdata1 * jt=new St_jdata1(tt);  AddData(jt);
  // assume size of the output j-table (wild guess of size) 
  jt->ReAllocate(tr->n_tpc_hit);

  // find G2T TPC-HITS for this track
  int i;
  g2t_tpc_hit_st *GTPH=gtph->GetTable(); assert(GTPH);
  int nhitok=0;
  int n_tot_hit_ok=0;
  for(i=0; i<gtph->GetNRows(); i++,GTPH++) {
    if(trID !=GTPH->track_p) continue;
    nhitok++;
    struct jdata1_st item;
    item.id=trID;
    item.x[0]=GTPH->x[0];
    item.x[1]=GTPH->x[1];
    item.x[2]=GTPH->x[2];
    jt->AddAt(&item,n_tot_hit_ok++); // update my table with new record
  }
  jt->Purge();
    
  printf("   END pickGTpcHit() trID=%d Npicked_Gtphit=%d ( of %d all G-hits) \n",trID, (int)jt->GetNRows(), (int)gtph->GetNRows());

return jt;
}


//_____________________________________________________________
//_____________________________________________________________
//_____________________________________________________________
St_jdata1 * StppLPevalMaker::pickHit(St_dst_track * DstTr, St_tcl_tphit *TclH, int flag)
{// select TCL clusters belonging to the dst-tracks
  // INPUT flag=1 : use TPT_trID, =2 : use DST_trID

  assert(DstTr);
  assert(TclH);

  // printf("start pickRhit() NdstTR=%d, NtpcCL=%d\n",(int)DstTr->GetNRows(),(int)TclH->GetNRows());


  int ntr0=DstTr->GetNRows();
  dst_track_st *pDstTr=DstTr->GetTable(); assert(pDstTr);

  char tt[20]; sprintf(tt,"uTclClust-%d",flag);
  St_jdata1 * jt=new St_jdata1(tt);
  // change size of the output j-table (wild guess of size) 
  jt->ReAllocate(ntr0*45);// starting max.# of clusters

  int noktr=0;
  int ntrval=0;
  int i,nokcl;

  //printf("dsttr-id, iflag  nfit  chisq[0]  invp  phi0 psi r0 tanl z0\n");
  for(i=0,nokcl=0; i<ntr0; i++,pDstTr++)
    {
      if(pDstTr->iflag<=0) continue; // not a valid track according to:
      //www.star.bnl.gov/STAR/html/all_l/html/dst_track_flags.html
      // e.g. 301=TPC+Vertex, good track
      //float reta=asinh(pDstTr->tanl);// rec eta

      ntrval++;
      int rtr_id=pDstTr->id;

#if 0
      printf("%2d  %2d  %2d   %10f %f %f %f %f %f  %f \n",
	     pDstTr->id,  pDstTr->iflag  , pDstTr->n_fit_point,  pDstTr->chisq[0]  ,
	     pDstTr->invpt  , pDstTr->phi0 , pDstTr->psi ,pDstTr->r0,pDstTr->tanl,pDstTr->z0);

#endif

      noktr++;
      int j;
      tcl_tphit_st *H=TclH->GetTable(); assert(H);
      for(j=0; j<TclH->GetNRows(); j++,H++)
	{
	  int id=-1;
	  if(flag==1)
	    id=H->track/1000; // use TPT track ID for this hit
	  else if(flag==2)
	    id=H->id_globtrk; // use global track ID
	  if(id!=rtr_id) continue;
	  jdata1_st item;
	  item.id=rtr_id;
	  item.x[0]=H->x;
	  item.x[1]=H->y;
	  item.x[2]=H->z;
	  jt->AddAt(&item,nokcl++); // update my table with new record
	  //printf("TCL hit id=%d picked by GlobTrID=%d\n",H->id,id);
	}
    }
  
  jt->Purge();
  printf("   END pickTpcCluster(), nOKtr=%d, of valid NRtra=%d, nOKcl=%d of TB=%d\n",noktr,ntrval, nokcl, (int)TclH->GetNRows());
  

  return jt; 

}


