#include <assert.h>
#include <math.h>
#include <strings.h>

#include "StppLPfindMaker.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_tpt_track_Table.h" 


//_____________________________________________________________
//_____________________________________________________________
//_____________________________________________________________
void StppLPfindMaker::copyTpcTr2Dst(St_tpt_track * TpcTr,St_dst_track * DstTr)
{// copy TPC track to dst-format

  assert(TpcTr);
  assert(DstTr);
    
  Int_t numRowTptrack = TpcTr->GetNRows();
  
  tpt_track_st *tptrackT = TpcTr->GetTable();
  
  Int_t counter = 0;
  for( Int_t i=0; i<numRowTptrack; i++) {
    if( tptrackT[i].flag < 0 ) continue;
    dst_track_st globtpcRow;
    globtpcRow.r0          = tptrackT[i].r0;
    globtpcRow.phi0        = tptrackT[i].phi0;
    globtpcRow.z0          = tptrackT[i].z0;
    globtpcRow.psi         = tptrackT[i].psi; //  to get track direction X/Y
    globtpcRow.tanl        = tptrackT[i].tanl;
    globtpcRow.invpt       = tptrackT[i].invp;
    globtpcRow.length      = tptrackT[i].length;
    globtpcRow.id          = tptrackT[i].id;
    globtpcRow.iflag       = 666;//tptrackT[i].flag;
    globtpcRow.det_id      = 777;
    globtpcRow.n_point     = tptrackT[i].nrec;
    globtpcRow.n_fit_point = tptrackT[i].nfit;
    globtpcRow.icharge     = tptrackT[i].q;
    memset(globtpcRow.covar,776,sizeof(globtpcRow.covar));
    memset(globtpcRow.chisq,775,sizeof(globtpcRow.chisq));
    // NOT ALL INFO IS COPPIED
    // NOT ALL INFO IS FILLED IN

    //printf(" TPT tr id=%d  n_point=%d pt=%f\n",globtpcRow.id ,globtpcRow.n_point,1./globtpcRow.invpt);
    DstTr->AddAt(&globtpcRow, counter);
    counter++;
  }
  
  // printf("copyTpc2Dst()  NtpcTR=%d NdstTR=%d, counter=%d\n",(int)TpcTr->GetNRows(),(int)DstTr->GetNRows(),counter);

  return ; 

}

