// $Id: StPreVertexMaker.cxx,v 1.8 2000/05/17 21:25:36 wdeng Exp $
// $Log: StPreVertexMaker.cxx,v $
// Revision 1.8  2000/05/17 21:25:36  wdeng
// Copy nfit. evr needs it now.
//
// Revision 1.7  2000/05/09 19:54:31  wdeng
// Copy more entries from tpt_track to local globtpc table. It is needed by lmv according to Akio.
//
// Revision 1.6  2000/05/08 20:20:42  wdeng
// Install a switch to call lmv if the tracks are less than 15. Flag pre-vertex the same numbers as that in evr_am if lmv get called.
//
// Revision 1.5  2000/03/16 21:31:21  wdeng
// Change the name of evr_evrpar to pre_evr_evrpar.
//
// Revision 1.4  2000/02/16 16:18:20  genevb
// Fixed typo in previous check-in
//
// Revision 1.3  2000/02/16 16:13:08  genevb
// Correction to not call evr_am with <1 tracks
//
// Revision 1.2  2000/02/02 19:53:55  wdeng
// Assigned -1 to covariance entry of evrpar.
//
// Revision 1.1  2000/02/01 17:12:18  wdeng
// Initial version. This maker reads in tptrack and produces a preliminary primary vertex.
//

#include <iostream.h>
#include <stdlib.h>

#include "StPreVertexMaker.h"

#include "StDetectorId.h"
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"
#include "global/St_evr_am_Module.h"
#include "St_db_Maker/St_db_Maker.h"

long lmv(St_dst_track *track, St_dst_vertex *vertex, Int_t mdate);

ClassImp(StPreVertexMaker)
  
//_____________________________________________________________________________
StPreVertexMaker::StPreVertexMaker(const char *name):StMaker(name),
  m_pre_evrpar(0) { 
}

//_____________________________________________________________________________
StPreVertexMaker::~StPreVertexMaker(){
}

//_____________________________________________________________________________
Int_t StPreVertexMaker::Init(){
  m_pre_evrpar = new St_evr_evrpar("pre_evr_evrpar",1);
  {
    evr_evrpar_st row;

    memset(&row,0,sizeof(row));
    row.fitoption  = 0;
    row.covariance = -1;   //
    row.vcut	   = 3.;  // distance below where track is marked as default primary
    row.cut2	   = 2.;  // select tracks for 2nd vertex fit
    row.cut3	   = 0.5; // select tracks for 3rd vertex fit
    row.cutxy	   = 1.;  // select tracks for vertex fitting
    row.cutz	   = 10.; // select tracks for vertex fitting
    row.ptmin	   = 0.;  // minimum pt of individual tracks
    m_pre_evrpar->AddAt(&row,0);
  }
  AddRunCont(m_pre_evrpar);
  
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StPreVertexMaker::Make(){
  PrintInfo();  

  St_dst_vertex *preVertex = new St_dst_vertex("preVertex",4); 
  AddData(preVertex);   
  
  St_DataSet    *tpctracks = GetInputDS("tpc_tracks");
  St_tpt_track  *tptrack   = 0;
  if( tpctracks ) {
    St_DataSetIter tpctracksI(tpctracks); 
    tptrack   = (St_tpt_track  *) tpctracksI("tptrack");
  }

  if( !tptrack ) {
    gMessMgr->Warning() << "no tptrack. Exit from StPreVertexMaker!" << endm;
    return kStWarn;    
  } else if ( !(tptrack->GetNRows()) ) {
    gMessMgr->Warning() << "zero rows in tptrack. Exit from StPreVertexMaker!" << endm;
    return kStWarn;    
  } else {
    Int_t numRowTptrack = tptrack->GetNRows();
    St_dst_track globtpc("globtpc", numRowTptrack);
  
    tpt_track_st *tptrackT = tptrack->GetTable();
    dst_track_st globtpcRow;

    Int_t counter = 0;
    for( Int_t i=0; i<numRowTptrack; i++) {
      if( tptrackT[i].flag < 0 ) continue;
      globtpcRow.r0          = tptrackT[i].r0;
      globtpcRow.phi0        = tptrackT[i].phi0;
      globtpcRow.z0          = tptrackT[i].z0;
      globtpcRow.psi         = tptrackT[i].psi;
      globtpcRow.tanl        = tptrackT[i].tanl;
      globtpcRow.invpt       = tptrackT[i].invp;
      globtpcRow.length      = tptrackT[i].length;
      globtpcRow.id          = counter+1;
      globtpcRow.iflag       = tptrackT[i].flag;
      globtpcRow.det_id      = kTpcId;
      globtpcRow.n_point     = tptrackT[i].nrec; 
      globtpcRow.n_fit_point = tptrackT[i].nfit; 
      globtpcRow.icharge     = tptrackT[i].q;

      globtpc.AddAt(&globtpcRow, counter);
      counter++;
    }

    Int_t iRes = kStOK;
    if( globtpc.GetNRows() >= 15 ) {
      iRes = evr_am(m_pre_evrpar,&globtpc,preVertex);
    } else {
      St_db_Maker *db = ( St_db_Maker *)GetMaker("db");
      Int_t mdate = db->GetDateTime().GetDate();
      iRes = lmv(&globtpc,preVertex,mdate);
      
      // use the same flag convention as that in evr_am 
      Int_t flagArray[] = {-103, -102, -101, 101};
      dst_vertex_st* preVertexT = preVertex->GetTable();
      for( Int_t i=0; i< preVertex->GetNRows(); i++)
	(preVertexT+i)->iflag = flagArray[i];
    }
    if (iRes !=kSTAFCV_OK) return kStWarn;
  }
  
  return kStOK;
}
