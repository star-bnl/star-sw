
#include "Cut.h"

#include "Stiostream.h"
#include <string.h>

ClassImp(Cut)

//
// initialize event and track cuts to broad values
//
//EVENT CUTS:
Float_t Cut::mVertexZ[2] = {-200,200};
Int_t Cut::mFlowCent[2] = {0,9};	//MinBias
Float_t Cut::mZDCSum[2] = {0,1000};	//MinBias

//TRACK CUTS:
Float_t Cut::mEta[2] = {-1.,1.};
Int_t   Cut::mFitPts[2] = {0,50};
Float_t Cut::mSDcaGl[2] = {-3,3};
Float_t Cut::mDcaXYGl[2] = {-3,3};
Float_t Cut::mCross[2] = {-20,20};
Int_t   Cut::mFirstPadrow[2] = {0,99};
Bool_t  Cut::mSameSector = false;
Bool_t  Cut::mCrossCM = true;   //Meaning, its okay to cross CM by default

void
Cut::ShowCuts()
{

  cout << "******************************************************" << endl;
  cout << "Cut::ShowCuts()" << endl;
  cout << "event cuts:" << endl;
  
  cout << "\tflow cent   : " << mFlowCent[0] << " -- " << mFlowCent[1] << endl;
  cout << "\tvertex z    : " << mVertexZ[0] << " -- " << mVertexZ[1] << endl;
  cout << "\tZDCSum      : " << mZDCSum[0] << " -- " << mZDCSum[1] << endl;
  
  cout << "track cuts:" << endl
       << "\teta         : " << mEta[0] << " -- " << mEta[1] << endl
       << "\tfit pts     : " << mFitPts[0] << " -- " << mFitPts[1] << endl
       << "\tsdca        : " << mSDcaGl[0] << " -- " << mSDcaGl[1] << endl
       << "\tdcaxy       : " << mDcaXYGl[0] << " -- " << mDcaXYGl[1] << endl
       << "\tcross       : " << mCross[0] << " -- " << mCross[1] << endl
       << "\tfirst row   : " << mFirstPadrow[0] << " -- " << mFirstPadrow[1] << endl
       << "\tSameSector? : " << (Int_t)mSameSector << endl
       << "\tCrossCM?    : " << (Int_t)mCrossCM << endl
       << endl;
  cout << "******************************************************" << endl;

}

//The output of this method will eventually be used to make a conversion table for cuts on plots
void
Cut::DecodeCutList(const char *cutList)
{

  cout << "******************************************************" << endl;
  cout << "Cut::DecodeCutList("<< cutList << ")" << endl;
  cout << "Event Cuts:" << endl;
  
  cout << "\tFlow centrality	: " << mFlowCent[0] << " -- " << mFlowCent[1] << endl;
  cout << "\tZ vertex		: " << mVertexZ[0] << " -- " << mVertexZ[1] << endl;
  cout << "\tZDCSum		: " << mZDCSum[0] << " -- " << mZDCSum[1] << endl;
  
  cout << "Track Cuts:" << endl
       << "\tEta		: " << mEta[0] << " -- " << mEta[1] << endl
       << "\tFitpts 		: " << mFitPts[0] << " -- " << mFitPts[1] << endl
       << "\t3dDca		: " << mSDcaGl[0] << " -- " << mSDcaGl[1] << endl
       << "\tDcaXY		: " << mDcaXYGl[0] << " -- " << mDcaXYGl[1] << endl
       << "\tCrossing Angle	: " << mCross[0] << " -- " << mCross[1] << endl
       << "\tFirst Row		: " << mFirstPadrow[0] << " -- " << mFirstPadrow[1] << endl
       << "\tSameSector?        : " << (Int_t)mSameSector << endl
       << "\tCrossCM?           : " << (Int_t)mCrossCM
       << endl
       << endl;
  cout << "******************************************************" << endl;

}

const char* Cut::SetCuts(const char* cutList) {

  char cut[7];

  if (strlen(cutList) != 7) { 
    cerr << "<W> Error, cutList has " << strlen(cutList) << " members" << endl;
    cerr << "<W> Expecting 7 cuts.  Reverting to wide open cuts!" << endl << endl;
    cutList = "ZZZZZZZ";
  } 

  sscanf(cutList,"%c%c%c%c%c%c%c",&cut[0],&cut[1],
		&cut[2],&cut[3],&cut[4],&cut[5],&cut[6]);

  cout << "Setting cuts:" << endl
       << "\tcentType = " << cut[0] << endl
       << "\tvertex = " << cut[1] << endl
       << "\teta = " << cut[2] << endl
       << "\tfitPts = " << cut[3] << endl
       << "\t2dDCA = " << cut[4] << endl    
       << "\tSameSector ?= " << cut[5] << endl
       << "\tCrossCM ?= " << cut[6] << endl << endl;

  SetCentCut(cut[0]);
  SetZVertexCut(cut[1]);
  SetEtaCut(cut[2]);
  SetFitPtsCut(cut[3]);
  SetDCACut(cut[4]);
  SetSameSectorCut(cut[5]);
  SetCrossCMCut(cut[6]);

  return cutList;

}  

void Cut::SetCrossCMCut(const char crossCMCut) {
  
  switch(crossCMCut) {  
  case 'y': //
   mCrossCM = true; break;
  case 'n': //
   mCrossCM = false; break;
  default:
    cerr << "<W> Unknown CrossCM Cut Type: " << crossCMCut << endl;
    cerr << "<W> Reverting to NO cut: CrossCM = true" << endl << endl; break;
  }    
}

void Cut::SetSameSectorCut(const char sameSectorCut) {

  switch(sameSectorCut) {
  case 'y': //
   mSameSector = true; break;
  case 'n': //
   mSameSector = false; break;
  default:
    cerr << "<W> Unknown SameSector Cut Type: " << sameSectorCut << endl;
    cerr << "<W> Reverting to NO cut: SameSector = false" << endl << endl; break;
  }   
}

void Cut::SetDCACut(const char dcaCut) {

  switch(dcaCut) {
  case '1': //
   mSDcaGl[0] = -1.; mSDcaGl[1] = 1.;
   mDcaXYGl[0] = -3.; mDcaXYGl[1] = 3.; break;
  case '2': //
   mSDcaGl[0] = -0.9; mSDcaGl[1] = 0.9;
   mDcaXYGl[0] = -3.; mDcaXYGl[1] = 3.; break;
  case '3': //
   mSDcaGl[0] = -1.5; mSDcaGl[1] = 1.5;
   mDcaXYGl[0] = -3.; mDcaXYGl[1] = 3.; break;
  case '4': //
   mSDcaGl[0] = -3.; mSDcaGl[1] = 3.;
   mDcaXYGl[0] = -1.; mDcaXYGl[1] = 1.; break;
  case '5': //
   mSDcaGl[0] = -3.; mSDcaGl[1] = 3.;
   mDcaXYGl[0] = -0.9; mDcaXYGl[1] = 0.9; break;
  case '6': //
   mSDcaGl[0] = -3.; mSDcaGl[1] = 3.;
   mDcaXYGl[0] = -1.5; mDcaXYGl[1] = 1.5; break;
  default:
    cerr << "<W> Unknown DCA Cut Type: " << dcaCut << endl;
    cerr << "<W> Reverting to wide open cut: " << endl; 
    cerr << "\t-3. < SDcaGl < 3. " << endl;
    cerr << "\t-3. < DcaXYGl < 3. " << endl << endl;
    break;
  }   
}

void Cut::SetFitPtsCut(const char fitPtsCut) {

  switch(fitPtsCut) {
  case '1': //
   mFitPts[0] = 20; mFitPts[1] = 50; break;
  case '2': //
   mFitPts[0] = 15; mFitPts[1] = 50; break;
  case '3': //
   mFitPts[0] = 10; mFitPts[1] = 50; break;
  case '4': //
   mFitPts[0] = 25; mFitPts[1] = 50; break;
  case '5': //
   mFitPts[0] = 30; mFitPts[1] = 50; break;
  default:
    cerr << "<W> Unknown FitPts Cut Type: " << fitPtsCut << endl;
    cerr << "<W> Reverting to wide open cut: 0 < FitPts < 50" << endl << endl;
    break;
  }   
}

void Cut::SetEtaCut(const char etaCut) {

  switch(etaCut) {
  case '1': //
   mEta[0] = -0.75; mEta[1] = 0.75; break;
  case '2': //
   mEta[0] = -0.5; mEta[1] = -0.5; break;
  case '3': //
   mEta[0] = -1.; mEta[1] = 1.; break;
  default:
    cerr << "<W> Unknown Eta Cut Type: " << etaCut << endl;
    cerr << "<W> Reverting to wide open cut: -1. < Eta < 1." << endl << endl;
    break;
  }   
}

void Cut::SetZVertexCut(const char zVtxCut) {

  switch(zVtxCut) {
  case '1': //
   mVertexZ[0] = -25.; mVertexZ[1] = 25.; break;
  case '2': //
   mVertexZ[0] = 0.; mVertexZ[1] = 25.; break;
  case '3': //
   mVertexZ[0] = -25.; mVertexZ[1] = 0.; break;
  case '4': //
   mVertexZ[0] = -15.; mVertexZ[1] = 15.; break;
  case '5': //
   mVertexZ[0] = -10.; mVertexZ[1] = 10.; break;
  case '6': //
   mVertexZ[0] = -5.; mVertexZ[1] = 5.; break;
  default:
    cerr << "<W> Unknown Vertex Z type: " << zVtxCut << endl;
    cerr << "<W> Reverting to wide open cut: -200 < Zvtx < 200" << endl << endl;
    break;
  }   
}

void Cut::SetCentCut(const char centType) {

  switch(centType){
  case 'C': // triggered central ALL
    mFlowCent[0]=-1; mFlowCent[1]=-1; break;
  case '9': // triggered central 0-5
    mFlowCent[0]=-1; mFlowCent[1]=-1;
    mZDCSum[0]=0; mZDCSum[1]=66; break;	//Determined ZdcCut using zdcCent.C
  case '8': // triggered central 5-10
    mFlowCent[0]=-1; mFlowCent[1]=-1;
    mZDCSum[0]=66; mZDCSum[1]=1000; break;  //Determined ZdcCut using zdcCent.C
  case 'M': // triggered minbias
    mFlowCent[0]=0; mFlowCent[1]=9; break;

  case '6': // 0-5% of minbias
    mFlowCent[0]=9; mFlowCent[1]=9; break;
  case '5': // 5-10% of minbias
    mFlowCent[0]=8; mFlowCent[1]=8; break;
  case '4': // 10-20% of minbias
    mFlowCent[0]=7; mFlowCent[1]=7; break;
  case '3': // 20-30% of minbias
    mFlowCent[0]=6; mFlowCent[1]=6; break;
  case '2': // 30-40% of minbias
    mFlowCent[0]=5; mFlowCent[1]=5; break;
  case '1': // 40-60% of minbias
    mFlowCent[0]=3; mFlowCent[1]=4; break;
  case '0': // 60-80% of minbias
    mFlowCent[0]=1; mFlowCent[1]=2; break;
  case 'a': // 40-80% of minbias
    mFlowCent[0]=1; mFlowCent[1]=4; break;
  default:
    cerr << "<W> Unknown centrality type: " << centType << endl;
    cerr << "<W> Reverting to MinBias. " << endl << endl;
    break;
  }

}
