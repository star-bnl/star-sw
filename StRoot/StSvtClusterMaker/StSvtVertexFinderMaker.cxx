
#include "StSvtVertexFinderMaker.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StMessMgr.h"
#include "SvtVertFind.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"


#include "tables/St_dst_vertex_Table.h"
#include "tables/St_scs_spt_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "StEvent.h"
#include "StSvtHitCollection.h"
#include "StPrimaryVertex.h"

float sft_main( St_scs_spt *scs_spt, St_dst_vertex *vertex, float x=0, float y=0, TH1F *hist=0);
float sft_main2(  StSvtHitCollection* rSvtHitColl, StEvent* event, float x=0, float y=0, TH1F *hist=0);
long event_numb=0;

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

  char  title1[30],title2[30];
  char *title3;

  mVtxZDiff2     = new TH1F("StkSvtVertDiff2","Z SVT - Z TPC Primary vertex resolution",100,-30,30);
  mVtxZTpc     = new TH1F("StkTpcVert" ,"Z TPC Primary vertex",101,-50,50);
  mVtxZSvt     = new TH1F("StkSvtVert" ,"Z SVT Primary vertex",101,-50,50);
  if (Debug()) {
    gMessMgr->Debug() << "In StSvtVertexFinderMaker::Init() ..."  << endm;
    
    hfile2  = new TFile("myrootfile2.root","RECREATE","Demo ROOT file");
    
    mVtxZDiff1     = new TH1F("StkSvtVertDiff1","Z SVT - Z Geant Primary vertex resolution",100,-15,15);
    mVtxZGe      = new TH1F("StkGeaVert" ,"Z Geant Primary vertex",101,-50,50);

    mtemp = new TH1F*[150];

    for (int i=1; i<151; i++){
      sprintf(title1,"ntracks_");
      sprintf(title2,"%d", i);
      title3 = strcat(title1,title2);
      mtemp[i]     = new TH1F(title3 ,"SVT number of tracks for each z for event i",1001,-50,50);
    }
    
    ntuple2 = new TNtuple("ntuple2","SVT primary Vertex ntuple","xtpc:ytpc:ztpc:zsvt:diff1:ntracks:event");
    ntuple3 = new TNtuple("ntuple3","SVT ","event:x:y:z:id_wafer:ix:iz:r");
    
  }
  
  return  StMaker::Init();
}

//___________________________________________________________________________

Int_t StSvtVertexFinderMaker::Make()
{
  if (Debug()) gMessMgr->Debug() << "In StSvtvertexFinderMaker::Make() ..."  << endm;
  
  St_scs_spt    *ScsSpt      = (St_scs_spt *)GetDataSet("svt_hits/.data/scs_spt");
  

  StEvent* event=0;
  StSvtHitCollection* rSvtHitColl=0;
  event = (StEvent *) GetInputDS("StEvent"); 
   
  
  if( event){ 
    rSvtHitColl = event->svtHitCollection();
  }
    
  if (!ScsSpt && !rSvtHitColl) {
    
    gMessMgr->Warning() << "StSvtVertexMaker:: No SVT space points" << endm;
    return kStWarn;
  }
  

  int numRowPreVtx = 0;
  float xtpc=0;
  float ytpc=0;
  float ztpc=0;
  St_DataSet* preVertex;
  St_dst_vertex  *preVtx;
  preVtx = 0;
  preVertex= 0;
  if( ScsSpt){

    //pointer to preVertex dataset
    preVertex = GetDataSet("preVertex"); 
    if (!preVertex) {
      preVtx  = new St_dst_vertex("preVertex",4); 
      AddData(preVtx);
    }
    else{
      //iterator
      St_DataSetIter preVertexI(preVertex);
      //pointer to preVertex
      preVtx  = (St_dst_vertex *)preVertexI("preVertex");
    }
    if( preVtx){
      numRowPreVtx = preVtx->GetNRows();
      preVtx->ReAllocate(numRowPreVtx+1);
      
      // Finds TPC x,y,z
      dst_vertex_st *preVtxPtr = preVtx->GetTable();    
      for (Int_t i = 0; i < numRowPreVtx;i++,preVtxPtr++) {
	if(preVtxPtr->iflag == 101) {
	  xtpc = preVtxPtr->x;
	  ytpc = preVtxPtr->y;
	  ztpc = preVtxPtr->z;
	}
      }
    }
  }
  else if( event){
    
    for( int i=0; i<event->numberOfPrimaryVertices(); i++){
      
      if( event->primaryVertex(i)->flag() == 1){
	xtpc = event->primaryVertex(i)->position().x();
	ytpc = event->primaryVertex(i)->position().y();
	ztpc = event->primaryVertex(i)->position().z();
	break;
      }
    } 
  }
  
  
  // Find SVT vertex
  event_numb++;
  float zsvt;
  if (Debug()) { 
    if( ScsSpt)
      zsvt= sft_main(ScsSpt,preVtx,xtpc,ytpc,mtemp[event_numb]);
    if(rSvtHitColl)
      zsvt= sft_main2(rSvtHitColl,event,xtpc,ytpc,mtemp[event_numb]);
  }
  else {
    if( ScsSpt) zsvt= sft_main(ScsSpt,preVtx,xtpc,ytpc);
    if(rSvtHitColl) zsvt= sft_main2(rSvtHitColl,event,xtpc,ytpc); 
  }
  
  
  if( ScsSpt){
    scs_spt_st *s_spt = ScsSpt->GetTable();  
    long NSpt = ScsSpt->GetNRows();
    
    //
    //Fills ntuple3
    if (Debug()) {
      for (int i=0;i<NSpt;i++){
	if (s_spt[i].flag<2){
	  float my_x = s_spt[i].x[0]-xtpc;
	  float my_y = s_spt[i].x[1]-ytpc;
	  float my_z = s_spt[i].x[2];
	  float my_r = sqrt(my_x*my_x+my_y*my_y);
	  float ix = atan2(my_y,my_x);
	  float my_z0 = (float)ztpc;
	  float iz = ((11*my_z)/my_r)+((11*my_z0)/my_r)-my_z0;
	  ntuple3->Fill(event_numb,s_spt[i].x[0],s_spt[i].x[1],s_spt[i].x[2],
			s_spt[i].id_wafer,ix,iz,my_r);
	}
      }
    }
 
    MakeHistograms(preVtx,event_numb); // histograms
  }
  
  
  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtVertexFinderMaker::Finish(){
  
  if (Debug()) {
    gMessMgr->Debug() << "In StSvtVertexFinderMaker::Finish() ..." 
				 <<  GetName() << endm;

    gMessMgr->Warning() << "Closing hfile2"  << endm;
    hfile2->Write();
    hfile2->Close();
  }
  return kStOK;
}


//_____________________________________________________________________________
void StSvtVertexFinderMaker::MakeHistograms( St_dst_vertex*  vtx, long event ){
  
  if (vtx) {
    float z_svt=999.;
    float n_svt=0.;
    float x_tpc=-999.;
    float y_tpc=-999.;
    float z_tpc=-999.;
    dst_vertex_st *preVtxPtr = vtx->GetTable();
    
    for (Int_t i = 0; i <vtx->GetNRows();i++,preVtxPtr++) {
            
      if (preVtxPtr->iflag == 201) {
	z_svt = preVtxPtr->z;	
	n_svt = preVtxPtr->x;
	gMessMgr->Warning() << " fill with SVT PreVtx z=" << preVtxPtr->z  <<endm;
      }
      else if(preVtxPtr->iflag == 101) {
	x_tpc = preVtxPtr->x;
	y_tpc = preVtxPtr->y;
	z_tpc = preVtxPtr->z;
	gMessMgr->Warning() << " fill with TPC PreVtx z=" << preVtxPtr->z  <<endm;
      }
    }
    mVtxZDiff2->Fill(z_tpc-z_svt);
    mVtxZTpc->Fill(z_tpc);
    mVtxZSvt->Fill(z_svt);
    if (Debug()) {
      ntuple2->Fill((float)x_tpc,(float)y_tpc,(float)z_tpc,(float)z_svt,(float)(z_tpc-z_svt),(float)n_svt,(float)event);
    }

    // Compare to Geant
    // Geantiterator
    St_DataSetIter Geant(GetInputDS("geant"));
    St_g2t_vertex *GeantVert = (St_g2t_vertex *) Geant("g2t_vertex");
    if( GeantVert){
      g2t_vertex_st *VtxGe = GeantVert->GetTable();
      if (Debug()) {
	mVtxZDiff1->Fill(VtxGe->ge_x[2]-z_svt);
	mVtxZGe->Fill(VtxGe->ge_x[2]);
      }  
    }

  }  
}
