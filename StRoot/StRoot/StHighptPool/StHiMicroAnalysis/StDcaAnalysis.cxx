/***************************************************************************
 *
 * $Id: StDcaAnalysis.cxx,v 1.3 2002/05/31 21:58:29 jklay Exp $
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  Class to look at DCA information from highpt uDSTs
 *
 *
 ***************************************************************************
 *
 * $Log: StDcaAnalysis.cxx,v $
 * Revision 1.3  2002/05/31 21:58:29  jklay
 * Updated analysis code to use new cut class
 *
 * Revision 1.2  2002/04/03 00:23:27  jklay
 * Fixed private member access bugs in analysis code
 *
 * Revision 1.1  2002/04/02 20:05:18  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 **************************************************************************/
#include "StDcaAnalysis.h"

enum TYPE { MEAN=0,RMS,NUM,SKEW};

//__________________

StDcaAnalysis::StDcaAnalysis(const char* inputDir,
			   const char* outRootName)
  :  StHiBaseAnalysis(inputDir,outRootName)
     
{


}

//__________________

StDcaAnalysis::~StDcaAnalysis()
{
}


//_____________________________

void
StDcaAnalysis::initHistograms()
{
  cout << "StDcaAnalysis::initHistograms()" << endl;
  
  char name[100];

  //***********************
  
  using namespace Bin;

  //***********************

  /*
  Double_t sectorMin, sectorMax;

  if(CutRc::Half() == 'w'){
    sectorMin = sectorWestMin;
    sectorMax = sectorWestMax;
  }
  else{
    sectorMin = sectorEastMin;
    sectorMax = sectorEastMax;
  }
  */
  //  int netabin=6; double etamin=-.5,etamax=.5;

  char* type[] = {"Mean","Rms","Num" };

  for(int i=0; i<3; i++){
    // global pt
    sprintf(name,"h%sPosDcaXYGlPtZFD",type[i]); 
    dca[i].hPosGlPtZFD =
      new TH3D(name,name,
	       nVertexZWideBin,vertexZWideMin,vertexZWideMax,
	       nPhiBin,phiDegMin,phiDegMax,
	       nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax);

    sprintf(name,"h%sNegDcaXYGlPtZFD",type[i]); 
    dca[i].hNegGlPtZFD =
      new TH3D(name,name,
	       nVertexZWideBin,vertexZWideMin,vertexZWideMax,
	       nPhiBin,phiDegMin,phiDegMax,
	       nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax);

    sprintf(name,"h%sPosPlusNegDcaXYGlPtZFD",type[i]); 
    dca[i].hPosPlusNegGlPtZFD =
      new TH3D(name,name,
	       nVertexZWideBin,vertexZWideMin,vertexZWideMax,
	       nPhiBin,phiDegMin,phiDegMax,
	       nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax);

    sprintf(name,"h%sPosMinusNegDcaXYGlPtZFD",type[i]); 
    dca[i].hPosMinusNegGlPtZFD =
      new TH3D(name,name,
	       nVertexZWideBin,vertexZWideMin,vertexZWideMax,
	       nPhiBin,phiDegMin,phiDegMax,
	       nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax);
    // primary pt
    sprintf(name,"h%sPosDcaXYPrPtZFD",type[i]); 
    dca[i].hPosPrPtZFD =
      new TH3D(name,name,
	       nVertexZWideBin,vertexZWideMin,vertexZWideMax,
	       nPhiBin,phiDegMin,phiDegMax,
	       nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax);

    sprintf(name,"h%sNegDcaXYPrPtZFD",type[i]); 
    dca[i].hNegPrPtZFD =
      new TH3D(name,name,
	       nVertexZWideBin,vertexZWideMin,vertexZWideMax,
	       nPhiBin,phiDegMin,phiDegMax,
	       nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax);

    sprintf(name,"h%sPosPlusNegDcaXYPrPtZFD",type[i]); 
    dca[i].hPosPlusNegPrPtZFD =
      new TH3D(name,name,
	       nVertexZWideBin,vertexZWideMin,vertexZWideMax,
	       nPhiBin,phiDegMin,phiDegMax,
	       nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax);

    sprintf(name,"h%sPosMinusNegDcaXYPrPtZFD",type[i]);
    dca[i].hPosMinusNegPrPtZFD =
      new TH3D(name,name,
	       nVertexZWideBin,vertexZWideMin,vertexZWideMax,
	       nPhiBin,phiDegMin,phiDegMax,
	       nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax);
  }

}

//_____________________

void 
StDcaAnalysis::trackLoop()
{
  if(mDebug)
    cout << "StDcaAnalysis::trackLoop()" << endl;

  //
  // event stuff
  // 
  /*
  Float_t zdcSum = mHiMicroEvent->ZDCe() + mHiMicroEvent->ZDCw();
  Float_t ctb    = mHiMicroEvent->CTB();
  Float_t flowCent   = mHiMicroEvent->Centrality();
  NchCentrality zdcCent = centrality(zdcSum,ctb);
  */

  Int_t nTrack = mHiMicroEvent->NTrack();
  StHiMicroTrack* track;
  
  for(Int_t i=0; i<nTrack; i++){
    track =(StHiMicroTrack*) mHiMicroEvent->tracks()->At(i);

    Float_t vertexZ = mHiMicroEvent->VertexZ();
    Float_t dcaXYGl = track->DcaXYGl();
    Int_t charge = track->Charge();
    Float_t phiGlDeg = track->PhiGl()*180./M_PI;
    phiGlDeg = (phiGlDeg<-165) ? (phiGlDeg += 360) : phiGlDeg;
    Float_t eta  = track->EtaPr();

    Float_t z = vertexZ;

    //Float_t exitZ = vtxZ+200*TMath::Tan(track->DipAnglePr());

    if(!CutRc::AcceptTrackVtxZHalf(track,vertexZ)) continue;

    if(!CutRc::AcceptFitPts(track)) continue;
    if(fabs(eta)>.1) continue;

    mNHiPtTrack++;

    if(track->PtGl() >1.7 && track->PtGl()<6){ // glpt
      if(charge>0){ //pos
	dca[MEAN].hPosGlPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl);
	dca[RMS].hPosGlPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl*dcaXYGl);
	dca[NUM].hPosGlPtZFD->Fill(z,phiGlDeg,dcaXYGl);
      }
      else{ //neg
	dca[MEAN].hNegGlPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl);
	dca[RMS].hNegGlPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl*dcaXYGl);
	dca[NUM].hNegGlPtZFD->Fill(z,phiGlDeg,dcaXYGl);
      }
	
      // both
      dca[MEAN].hPosPlusNegGlPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl);
      dca[RMS].hPosPlusNegGlPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl*dcaXYGl);
      dca[NUM].hPosPlusNegGlPtZFD->Fill(z,phiGlDeg,dcaXYGl);
    
      dca[MEAN].hPosMinusNegGlPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl*charge);
      dca[RMS].hPosMinusNegGlPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl*dcaXYGl);
      dca[NUM].hPosMinusNegGlPtZFD->Fill(z,phiGlDeg,dcaXYGl);
    }
    if(track->PtPr()>1.7 && track->PtPr()<6){
      if(charge>0){ //pos
	dca[MEAN].hPosPrPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl);
	dca[RMS].hPosPrPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl*dcaXYGl);
	dca[NUM].hPosPrPtZFD->Fill(z,phiGlDeg,dcaXYGl);
      }
      else{ //neg
	dca[MEAN].hNegPrPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl);
	dca[RMS].hNegPrPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl*dcaXYGl);
	dca[NUM].hNegPrPtZFD->Fill(z,phiGlDeg,dcaXYGl);
      }
	
      // both
      dca[MEAN].hPosPlusNegPrPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl);
      dca[RMS].hPosPlusNegPrPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl*dcaXYGl);
      dca[NUM].hPosPlusNegPrPtZFD->Fill(z,phiGlDeg,dcaXYGl);
    
      dca[MEAN].hPosMinusNegPrPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl*charge);
      dca[RMS].hPosMinusNegPrPtZFD->Fill(z,phiGlDeg,dcaXYGl,dcaXYGl*dcaXYGl);
      dca[NUM].hPosMinusNegPrPtZFD->Fill(z,phiGlDeg,dcaXYGl);

    }

    
  } // tracks

  if(mDebug)
    cout << "\ttracks : " << nTrack << endl;

}


//_______________________

Bool_t
StDcaAnalysis::acceptEvent(StHiMicroEvent* event)
{
  return CutRc::Accept(event);

}



//_______________________

// ew: east is 0, west is 1

Int_t
StDcaAnalysis::findSector(Float_t phi, Char_t ew)
{
  Int_t sector=0;
  Float_t pi = (Float_t) TMath::Pi();
  Float_t sectorWidth = pi/6.;

  Int_t sectorWest[] = {8,7,6,5,4,3,2,1,12,11,10};
  Int_t sectorEast[] = {16,17,18,19,20,21,22,23,24,13,14};
  
  if(phi>=(pi-sectorWidth/2.) || phi < (-pi+sectorWidth/2.)){
    if(ew=='w'){
      sector = 9;
    }
    else if(ew=='e'){
      sector = 15;
    }
    else{
      cerr << "wrong CutRc::Half " << ew << endl;
      exit(1);
    }
  }
  else{
    for(int i=0; i<11; i++){
      if(phi>=(-pi +sectorWidth*(i+1) - sectorWidth/2.) && 
	 phi<(-pi +sectorWidth*(i+1) + sectorWidth/2.)){
	
	if(ew=='w') sector = sectorWest[i];
	else if(ew=='e') sector = sectorEast[i];
	
	break;
      }
    }
  }
  //  if(gRandom->Rndm()<.1)
  //   cout << "phi : " << phi*180./pi << " sector : " << sector << endl;


  if(!sector){
    cerr << "Huh? phi = " << phi << endl; exit(1);
  }

  return sector;
}

//_____________________________________

ClassImp(StDcaAnalysis)
