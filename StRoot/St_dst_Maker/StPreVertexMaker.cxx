// $Id: StPreVertexMaker.cxx,v 1.2 2000/02/02 19:53:55 wdeng Exp $
// $Log: StPreVertexMaker.cxx,v $
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
  m_pre_evrpar = new St_evr_evrpar("evr_evrpar",1);
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

  // Set up in order to call evr_am
  St_dst_vertex *preVertex = new St_dst_vertex("preVertex",4); 
  AddData(preVertex);   
  
  St_DataSet    *tpctracks = GetInputDS("tpc_tracks");
  St_tpt_track  *tptrack   = 0;
  if( tpctracks ) {
    St_DataSetIter tpctracksI(tpctracks); 
    tptrack   = (St_tpt_track  *) tpctracksI("tptrack");
  }

  // If tptrack exists, we call evr_am
  if( !tptrack ) {
    gMessMgr->Warning() << "StPreVertexMaker: no tptrack. Exit without doing anything!" << endm;
    return kStWarn;    
  } else {
    Int_t numRowTptrack = tptrack->GetNRows();
    St_dst_track globtpc("globtpc", numRowTptrack);
  
    tpt_track_st *tptrackTable = tptrack->GetTable();
    dst_track_st globtpcRow;

    // Copy tptrack to globtpc if good quality
    Int_t counter = 0;
    for( Int_t i=0; i<numRowTptrack; i++) {
      if( tptrackTable[i].flag < 0 ) continue;
      globtpcRow.r0      = tptrackTable[i].r0;
      globtpcRow.phi0    = tptrackTable[i].phi0;
      globtpcRow.z0      = tptrackTable[i].z0;
      globtpcRow.psi     = tptrackTable[i].psi;
      globtpcRow.tanl    = tptrackTable[i].tanl;
      globtpcRow.invpt   = tptrackTable[i].invp;
      globtpcRow.iflag   = tptrackTable[i].flag;
      globtpcRow.det_id  = kTpcId;
      globtpcRow.icharge = tptrackTable[i].q;

      globtpc.AddAt(&globtpcRow, counter);
      counter++;
    }

    Int_t iRes = evr_am(m_pre_evrpar,&globtpc,preVertex);
    if (iRes !=kSTAFCV_OK) return kStWarn;
  }
 
  return kStOK;
}
