
#include "StSvtVertexFinderMaker.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StMessMgr.h"
#include "TH1.h"


#include "tables/St_dst_vertex_Table.h"
#include "tables/St_scs_spt_Table.h"
#include "tables/St_g2t_vertex_Table.h"
long sft_main( St_scs_spt *scs_spt, St_dst_vertex *vertex);

ClassImp(StSvtVertexFinderMaker)
//___________________________________________________________________________
StSvtVertexFinderMaker::StSvtVertexFinderMaker(const char *name) : StMaker(name)
{

}

//_____________________________________________________________________________
StSvtVertexFinderMaker::~StSvtVertexFinderMaker(){

}

//_____________________________________________________________________________
Int_t StSvtVertexFinderMaker::Init()
{

  if (Debug()) gMessMgr->Debug() << "In StSvtVertexFinderMaker::Init() ..."  << endm;

  mVtxZ     = new TH1F("StkSvtVert"        ,"Z SVT - Z TPC Primary vertex resolution"        ,100,-0.1,0.1);
  mVtxZGe     = new TH1F("StkSvtVertGe"        ,"Z SVT - Z Geant Primary vertex resolution"        ,100,-0.1,0.1);
  
  return  StMaker::Init();
  
}

//___________________________________________________________________________

Int_t StSvtVertexFinderMaker::Make()
{
  if (Debug()) gMessMgr->Debug() << "In StSvtvertexFinderMaker::Make() ..."  << endm;
  
  //
  St_scs_spt    *ScsSpt      = (St_scs_spt *)GetDataSet("svt_hits/.data/scs_spt");
  if (!ScsSpt) {
    gMessMgr->Warning() << "StSvtVertexMaker:: No SVT space points" << endm;
    
    return kStWarn;
  }
  
  
  //pointer to preVertex dataset
  St_DataSet *preVertex = GetDataSet("preVertex"); 
  
  //iterator
  St_DataSetIter preVertexI(preVertex);
  
  //pointer to preVertex
  St_dst_vertex  *preVtx  = (St_dst_vertex *)preVertexI("preVertex");
  int numRowPreVtx = 0;
  if( preVtx){
    numRowPreVtx = preVtx->GetNRows();
    preVtx->ReAllocate(numRowPreVtx+1);
    
    
    // Find SVT vertex
    Int_t Res_sft= sft_main(ScsSpt,preVtx);
    
    if (Res_sft !=  kSTAFCV_OK) 
    gMessMgr->Warning() << " Problem on return from SFT" << endm;
    
    MakeHistograms(preVtx); // histograms
  }
  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtVertexFinderMaker::Finish(){
  
  if (Debug()) gMessMgr->Debug() << "In StSvtVertexFinderMaker::Finish() ..." 
				 <<  GetName() << endm;
  
  return kStOK;
}


//_____________________________________________________________________________
void StSvtVertexFinderMaker::MakeHistograms( St_dst_vertex*  vtx){
  
  if (vtx) {
    float z_svt=999.;
    float z_tpc=-999.;
    dst_vertex_st *preVtxPtr = vtx->GetTable();
    
    for (Int_t i = 0; i <vtx->GetNRows();i++,preVtxPtr++) {
      
      if (preVtxPtr->iflag == 201) {
	z_svt = preVtxPtr->z;
      }
      else if(preVtxPtr->iflag == 101) {
	z_tpc = preVtxPtr->z;
      }
    }
    mVtxZ->Fill(z_tpc-z_svt);
    
    // Compare to Geant
    // Geantiterator
    St_DataSetIter Geant(GetInputDS("geant"));
    St_g2t_vertex *GeantVert = (St_g2t_vertex *) Geant("g2t_vertex");
    
    if( GeantVert){
      g2t_vertex_st *VtxGe = GeantVert->GetTable();
      
      mVtxZGe->Fill(VtxGe->ge_x[2]-z_svt);
      
    }
  }
  
}
