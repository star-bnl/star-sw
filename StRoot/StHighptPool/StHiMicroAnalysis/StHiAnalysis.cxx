/***************************************************************************
 *
 * $Id: StHiAnalysis.cxx,v 1.1 2002/04/02 20:05:18 jklay Exp $                                    
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  Class to perform highpt Analysis on highpt uDSTs
 *
 *
 ***************************************************************************
 *
 * $Log: StHiAnalysis.cxx,v $
 * Revision 1.1  2002/04/02 20:05:18  jklay
 * Bums analysis tools for highpt uDSTs
 *
 * 
 **************************************************************************/
#include "StHiAnalysis.h"


//__________________

StHiAnalysis::StHiAnalysis(const char* inputDir,
			   const char* outRootName)
  :  StHiBaseAnalysis(inputDir,outRootName)

     
{


}

//__________________

StHiAnalysis::~StHiAnalysis()
{
}

//_____________________________

void
StHiAnalysis::initHistograms()
{
  cout << "StHiAnalysis::initHistograms()" << endl;
  
  //***********************
  
  using namespace Bin;

  //***********************


  //  gStyle->SetPalette(1,0);

  char title[500],name[500];


  h1CentralityCut = new TH1D("h1CentralityCut","centrality cut",
			     nFlowCentBin,flowCentMin,flowCentMax);

  h1Centrality = new TH1D("h1Centrality","centrality",
			     nFlowCentBin,flowCentMin,flowCentMax);
  sprintf(name,"h3VtxXYZ");
  h3VtxXYZ = 
    new TH3D(name,name,
	     nVertexXBin, vertexXMin,vertexXMax,
	     nVertexXBin, vertexXMin,vertexXMax,
	     nVertexZEvtBin,vertexZEvtMin,vertexZEvtMax);
  h3VtxXYZ->SetXTitle("vtx x");
  h3VtxXYZ->SetYTitle("vtx y");
  h3VtxXYZ->SetZTitle("vtx z");

  strcpy(name,"h1VtxZCentCut");
  h1VtxZCentCut =
    new TH1D(name,name,
	     nVertexZEvtThinBin,vertexZEvtMin,vertexZEvtMax);
  h1VtxZCentCut->SetXTitle("vtx z");

  strcpy(name,"h3ZdcHMinusVtxZ");
  h3ZdcHMinusVtxZ =
    new TH3D(name,name,
	     nZdcBin,zdcMin,zdcMax,
	     nHMinusBin,hMinusMin,hMinusMax,
	     nVertexZEvtBin,vertexZEvtMin,vertexZMax);
  h3ZdcHMinusVtxZ->SetXTitle("zdcSum");
  h3ZdcHMinusVtxZ->SetYTitle("hMinus");
  h3ZdcHMinusVtxZ->SetZTitle("vtxZ");
	     

  strcpy(name,"h3ZdcHMinusCtbVtxZCut");
  h3ZdcHMinusCtbVtxZCut =
    new TH3D(name,name,
	     nZdcBin,zdcMin,zdcMax,
	     nHMinusBin,hMinusMin,hMinusMax,
	     30001,-0.5,30000);
  h3ZdcHMinusCtbVtxZCut->SetXTitle("zdcSum");
  h3ZdcHMinusCtbVtxZCut->SetYTitle("hMinus");
  h3ZdcHMinusCtbVtxZCut->SetZTitle("ctb");
  

  //------------------------------------------------------------
  
  

  TString sPM[2] = { "Plus","Minus"};
  TString sEW[2] = { "East","West" };

  //********************************************************
  // both charges

  strcpy(name,"h3ResPtPrGlPtPrDcaXYGl");
  h3ResPtPrGlPtPrDcaXYGl = 
    new TH3D(name,name,
	     nResPtBin,resPtMin,resPtMax,
	     nPtBin,ptMin,ptMax,
	     nDcaXYGlWideBin,dcaXYGlWideMin,dcaXYGlWideMax);
  h3ResPtPrGlPtPrDcaXYGl->SetXTitle("(ptPr-ptGl)/ptPr");
  h3ResPtPrGlPtPrDcaXYGl->SetYTitle("ptPr");
  h3ResPtPrGlPtPrDcaXYGl->SetZTitle("glDcaXY");

  strcpy(name,"h3ResPtPrGlPtGlDcaXYGl");
  h3ResPtPrGlPtGlDcaXYGl = 
    new TH3D(name,name,
	     nResPtBin,resPtMin,resPtMax,
	     nPtBin,ptMin,ptMax,
	     nDcaXYGlWideBin,dcaXYGlWideMin,dcaXYGlWideMax);
  h3ResPtPrGlPtGlDcaXYGl->SetXTitle("(ptPr-ptGl)/ptGl");
  h3ResPtPrGlPtGlDcaXYGl->SetYTitle("ptGl");
  h3ResPtPrGlPtGlDcaXYGl->SetZTitle("glDcaXY");

  //*** init var bin 0
  TArrayD* bins = new TArrayD;
  initPtAry(bins,0);
  

  // plus/minus
  
  for(Int_t i=0; i<2; i++){

    //******** centrality dependence of pt yields
    setName(title,"h2ZDCCentralityPtPr",sPM[i].Data());
    pm[i].h2ZDCCentralityPtPr
      = new TH2D(title,title,
		 nZdcCentBin,zdcCentMin,zdcCentMax,
		 nPtBin,ptMin,ptMax);
    pm[i].h2ZDCCentralityPtPr->SetXTitle("zdc centrality");
    pm[i].h2ZDCCentralityPtPr->SetXTitle("primary pt");

    setName(title,"h2CentralityPtPr",sPM[i].Data());
    pm[i].h2CentralityPtPr
      = new TH2D(title,title,
		 nZdcCentBin,zdcCentMin,zdcCentMax,
		 nPtBin,ptMin,ptMax);
    pm[i].h2CentralityPtPr->SetXTitle("zdc centrality");
    pm[i].h2CentralityPtPr->SetXTitle("primary pt");

    //******** phi and global dca xy
    setName(name,"h3PhiPrDcaXYGlPtPr",sPM[i].Data());
    pm[i].h3PhiPrDcaXYGlPtPr
      = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);
    pm[i].h3PhiPrDcaXYGlPtPr->SetXTitle("phiPr");
    pm[i].h3PhiPrDcaXYGlPtPr->SetYTitle("dcaXYGl");
    pm[i].h3PhiPrDcaXYGlPtPr->SetZTitle("ptPr");

    setName(name,"h3PhiGlDcaXYGlPtGl",sPM[i].Data());
    pm[i].h3PhiGlDcaXYGlPtGl
      = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);
    pm[i].h3PhiGlDcaXYGlPtGl->SetXTitle("phiGl");
    pm[i].h3PhiGlDcaXYGlPtGl->SetYTitle("dcaXYGl");
    pm[i].h3PhiGlDcaXYGlPtGl->SetZTitle("ptGl");
    
    //********** 3d dca, dca xy ,pt
    setName(name,"h3DcaGlDcaXYGlPtPr",sPM[i].Data());
    pm[i].h3DcaGlDcaXYGlPtPr =
      new TH3D(name,name,
	       nDcaBin,dcaMin,dcaMax,
	       nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
	       nPtBin,ptMin,ptMax);
    pm[i].h3DcaGlDcaXYGlPtPr->SetXTitle("dca 3d");
    pm[i].h3DcaGlDcaXYGlPtPr->SetYTitle("dca xy");
    pm[i].h3DcaGlDcaXYGlPtPr->SetZTitle("ptPr");

    setName(name,"h3DcaGlDcaXYGlPtGl",sPM[i].Data());
    pm[i].h3DcaGlDcaXYGlPtGl =
      new TH3D(name,name,
	       nDcaBin,dcaMin,dcaMax,
	       nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
	       nPtBin,ptMin,ptMax);
    pm[i].h3DcaGlDcaXYGlPtGl->SetXTitle("dca 3d");
    pm[i].h3DcaGlDcaXYGlPtGl->SetYTitle("dca xy");
    pm[i].h3DcaGlDcaXYGlPtGl->SetZTitle("ptGl");

    // for backgrounds
    setName(name,"h2SDcaGlPtPrRebin",sPM[i].Data());
    pm[i].h2SDcaGlPtPrRebin =
      new TH2D(name,name,
	       nSDcaBin,sDcaMin,sDcaMax,
	       nPtRebinBin,ptRebinMin,ptRebinMax);
    pm[i].h2SDcaGlPtPrRebin->SetXTitle("dca 3d");
    pm[i].h2SDcaGlPtPrRebin->SetYTitle("ptPr");
    
    setName(name,"h2DcaXYGlPtPrRebin",sPM[i].Data());
    pm[i].h2DcaXYGlPtPrRebin =
      new TH2D(name,name,
	       nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
	       nPtRebinBin,ptRebinMin,ptRebinMax);
    pm[i].h2DcaXYGlPtPrRebin->SetXTitle("dca 3d");
    pm[i].h2DcaXYGlPtPrRebin->SetYTitle("ptPr");

    setName(name,"h2DcaGlPtPrRebin",sPM[i].Data());
    pm[i].h2DcaGlPtPrRebin
      = new TH2D(name,name,
		 nDcaBin,dcaMin,dcaMax,
		 nPtRebinBin,ptRebinMin,ptRebinMax);
    pm[i].h2DcaGlPtPrRebin->SetXTitle("dca");
    pm[i].h2DcaGlPtPrRebin->SetYTitle("ptPr");
    

    //********** phi , fit pts, pt 
    setName(name,"h3PhiPrFitPtsPtPr",sPM[i].Data());
    pm[i].h3PhiPrFitPtsPtPr =
      new TH3D(name,name,
	       nPhiBin,phiDegMin,phiDegMax,
	       nFitPtsBin,fitPtsMin,fitPtsMax,
	       nPtBin,ptMin,ptMax);
    pm[i].h3PhiPrFitPtsPtPr->SetXTitle("phi");
    pm[i].h3PhiPrFitPtsPtPr->SetYTitle("fit pts");
    pm[i].h3PhiPrFitPtsPtPr->SetZTitle("ptPr");


    //********** phi , fit pts, pt 
    setName(name,"h3PhiPrAllPtsPtPr",sPM[i].Data());
    pm[i].h3PhiPrAllPtsPtPr =
      new TH3D(name,name,
	       nPhiBin,phiDegMin,phiDegMax,
	       nFitPtsBin,fitPtsMin,fitPtsMax,
	       nPtBin,ptMin,ptMax);
    pm[i].h3PhiPrAllPtsPtPr->SetXTitle("phi");
    pm[i].h3PhiPrAllPtsPtPr->SetYTitle("all pts");
    pm[i].h3PhiPrAllPtsPtPr->SetZTitle("ptPr");



    //********** vtx z, fit pts, pt
    setName(name,"h3VtxZFitPtsPtPr",sPM[i].Data());
    pm[i].h3VtxZFitPtsPtPr =
      new TH3D(name,name,
	       nVertexZWideBin,vertexZWideMin,vertexZWideMax,
	       nFitPtsBin,fitPtsMin,fitPtsMax,
	       nPtBin,ptMin,ptMax);
    pm[i].h3VtxZFitPtsPtPr->SetXTitle("vtxZ");
    pm[i].h3VtxZFitPtsPtPr->SetYTitle("fitPts");
    pm[i].h3VtxZFitPtsPtPr->SetZTitle("ptPr");

    //********* vtx z, fit pts, eta
    setName(name,"h3VtxZFitPtsEtaPr",sPM[i].Data());
    pm[i].h3VtxZFitPtsEtaPr = 
      new TH3D(name,name,
	       nVertexZWideBin,vertexZWideMin,vertexZWideMax,
	       nFitPtsBin,fitPtsMin,fitPtsMax,
	       nEtaBin,etaMin,etaMax);
    pm[i].h3VtxZFitPtsEtaPr->SetXTitle("vtxZ");
    pm[i].h3VtxZFitPtsEtaPr->SetYTitle("fitPts");
    pm[i].h3VtxZFitPtsEtaPr->SetZTitle("etaPr");

    //********** flow cent, fit pts, pt pr
    setName(name,"h3FlowCentFitPtsPtPr",sPM[i].Data());
     pm[i].h3FlowCentFitPtsPtPr =
      new TH3D(name,name,
	       nFlowCentBin,flowCentMin,flowCentMax,
	       nFitPtsCentBin,fitPtsCentMin,fitPtsCentMax,
	       nPtBin,ptMin,ptMax);
    pm[i].h3FlowCentFitPtsPtPr->SetXTitle("flowCent");
    pm[i].h3FlowCentFitPtsPtPr->SetYTitle("fitPts");
    pm[i].h3FlowCentFitPtsPtPr->SetZTitle("ptPr");


    //********** vtx, eta, pt
    setName(name,"h3VtxZEtaPrPtPr",sPM[i].Data());
    pm[i].h3VtxZEtaPrPtPr
      = new TH3D(name,name,
		 nVertexZBin,vertexZMin,vertexZMax,
		 nEtaSmallBin,etaSmallMin,etaSmallMax,
		 nPtBin,ptMin,ptMax);
    pm[i].h3VtxZEtaPrPtPr->SetXTitle("vertexZ");
    pm[i].h3VtxZEtaPrPtPr->SetYTitle("etaPr");
    pm[i].h3VtxZEtaPrPtPr->SetZTitle("ptPr");

    setName(name,"h3VtxZEtaGlPtGl",sPM[i].Data());
    pm[i].h3VtxZEtaGlPtGl
      = new TH3D(name,name,
		 nVertexZBin,vertexZMin,vertexZMax,
		 nEtaSmallBin,etaSmallMin,etaSmallMax,
		 nPtBin,ptMin,ptMax);
    pm[i].h3VtxZEtaGlPtGl->SetXTitle("vertexZ");
    pm[i].h3VtxZEtaGlPtGl->SetYTitle("etaPr");
    pm[i].h3VtxZEtaGlPtGl->SetZTitle("ptPr");

    // varying bins spectra 
    setName(name,"RawVarBin",sPM[i].Data());
    pm[i].h1RawVarBin 
      = new TH1D(name,name,bins->GetSize()-1,bins->GetArray());
    pm[i].h1RawVarBin->SetXTitle("pT");
    

  }



  
}
//______________________


void 
StHiAnalysis::trackLoop()
{
  if(mDebug)
    cout << "StHiAnalysis::trackLoop()" << endl;

  //
  // event stuff
  // 
  Float_t zdcSum = mHiMicroEvent->mZDCe + mHiMicroEvent->mZDCw;
  Float_t ctb    = mHiMicroEvent->mCTB;
  Float_t flowCent   = mHiMicroEvent->mCentrality;
  NchCentrality zdcCent = centrality(zdcSum,ctb);


  Int_t nTrack = mHiMicroEvent->mNTrack;
  StHiMicroTrack* track;
  
  for(Int_t i=0; i<nTrack; i++){
    track =(StHiMicroTrack*) mHiMicroEvent->tracks()->At(i);

    Int_t iCharge = (track->mCharge>0) ? 0 : 1; //plus is 0
    
    Float_t ptPr  = track->mPtPr;
    Float_t ptGl  = track->mPtGl;
    Float_t resPtPrGlPr = (ptPr-ptGl)/ptPr;
    Float_t resPtPrGlGl = (ptPr-ptGl)/ptGl;
    

    Float_t sDcaGl = (track->mDcaXYGl>0) ? track->mDcaGl : -track->mDcaGl;
    Float_t dcaXYGl = track->mDcaXYGl;
    Float_t dcaGl = track->mDcaGl;
    Float_t etaGl = track->mEtaGl;
    Float_t etaPr  = track->mEtaPr;

    Float_t phiPr = track->mPhiPr;
    Float_t phiGl = track->mPhiGl;
    Float_t phiGlDeg = phiGl*180./TMath::Pi();
    Float_t phiPrDeg = phiPr*180./TMath::Pi();

    Int_t fitPts = track->mFitPts;
    Int_t allPts = track->mAllPts;

    phiGlDeg = (phiGlDeg<-165) ? (phiGlDeg += 360) : phiGlDeg;
    phiPrDeg = (phiPrDeg<-165) ? (phiPrDeg += 360) : phiPrDeg;

    Float_t vertexZ = mHiMicroEvent->mVertexZ;

    //**********************************************************
    
    if(!CutRc::AcceptTrackHalf(track,vertexZ)) continue;

    //************** both signs ********************************
    
    // dca and fit points
    if(CutRc::AcceptNoEta(track)){
      
    }
    

    //************** plus/minus ********************************

    if(CutRc::AcceptFitPts(track) && CutRc::AcceptSDcaGl(track)){
      pm[iCharge].h3VtxZEtaPrPtPr->Fill(vertexZ,etaPr,ptPr);
      pm[iCharge].h3VtxZEtaGlPtGl->Fill(vertexZ,etaGl,ptGl);
    }

    // eta cut!
    //
    if(CutRc::AcceptEta(track)){
      if(CutRc::AcceptFitPts(track)){
	// phi,dcaxy,pt
	pm[iCharge].h3PhiPrDcaXYGlPtPr->Fill(phiPrDeg,dcaXYGl,ptPr);
	pm[iCharge].h3PhiGlDcaXYGlPtGl->Fill(phiGlDeg,dcaXYGl,ptGl);

	// dca3d, dcaxy,pt
	pm[iCharge].h3DcaGlDcaXYGlPtPr->Fill(dcaGl,dcaXYGl,ptPr);
	pm[iCharge].h3DcaGlDcaXYGlPtGl->Fill(dcaGl,dcaXYGl,ptGl);


	// respt, pt,dcaxy
	// no charge difference
	h3ResPtPrGlPtPrDcaXYGl->Fill(resPtPrGlPr,ptPr,dcaXYGl);
	h3ResPtPrGlPtGlDcaXYGl->Fill(resPtPrGlGl,ptGl,dcaXYGl);

	// rebin?
	pm[iCharge].h2SDcaGlPtPrRebin->Fill(sDcaGl,ptPr);
	
	// dcaxy,pt
	pm[iCharge].h2DcaXYGlPtPrRebin->Fill(dcaXYGl,ptPr);
	
	// dca,pt
	pm[iCharge].h2DcaGlPtPrRebin->Fill(dcaGl,ptPr);
      }
      
      //
      // 
      //
      if(CutRc::AcceptSDcaGl(track)){ // eta and dca cut
	// phi,fit pts, pt
	pm[iCharge].h3PhiPrFitPtsPtPr->Fill(phiPrDeg,fitPts,ptPr);

	// phi, all pts, pt
	pm[iCharge].h3PhiPrAllPtsPtPr->Fill(phiPrDeg,allPts,ptPr);
	
	// flow, fit pts, pt
  	pm[iCharge].h3FlowCentFitPtsPtPr->Fill(flowCent,fitPts,ptPr);
      }    
    }
    // dca and tight eta cut
    if(CutRc::AcceptSDcaGl(track) &&
       fabs(etaPr)<.1){
      // vtx z, fit pts, pt
      pm[iCharge].h3VtxZFitPtsPtPr->Fill(vertexZ,fitPts,ptPr);
    }

    // dca and pt
    if(CutRc::AcceptSDcaGl(track) &&
       ptPr>2 && ptPr<4){
      // vtx z, fit pts, eta
      pm[iCharge].h3VtxZFitPtsEtaPr->Fill(vertexZ,fitPts,etaPr);
    }

    // all cuts
    if(CutRc::Accept(track)){
      // count and centrality
      pm[iCharge].h2ZDCCentralityPtPr->Fill(zdcCent,ptPr);
      pm[iCharge].h2CentralityPtPr->Fill(flowCent,ptPr);

      pm[iCharge].h1RawVarBin->Fill(ptPr);
    }

  } // tracks

  if(mDebug)
    cout << "\ttracks : " << nTrack << endl;

}
//_____________________

void
StHiAnalysis::fillEventHistograms()
{
  h1Centrality->Fill(mHiMicroEvent->mCentrality);

  if(CutRc::AcceptVertexZ(mHiMicroEvent))
    h1CentralityCut->Fill(mHiMicroEvent->mCentrality);

  h3VtxXYZ->Fill(mHiMicroEvent->mVertexX,
		 mHiMicroEvent->mVertexY,
		 mHiMicroEvent->mVertexZ);

  if(CutRc::AcceptCent(mHiMicroEvent) && 
     CutRc::AcceptVertexZ(mHiMicroEvent)){
    h1VtxZCentCut->Fill(mHiMicroEvent->mVertexZ);
  }

  Float_t zdcSum = mHiMicroEvent->mZDCe + mHiMicroEvent->mZDCw;
 
  h3ZdcHMinusVtxZ->Fill(zdcSum,mHiMicroEvent->mNUncorrectedNegativePrimaries,
			mHiMicroEvent->mVertexZ);

  float vertexCut=fabs(Cut::mVertexZ[0]);
  if(fabs(mHiMicroEvent->mVertexZ)<vertexCut){
    //h3ZdcHMinusCtbVtxZCut->Fill(zdcSum,mHiMicroEvent->mNUncorrectedNegativePrimaries,mHiMicroEvent->mCTB);
    //h3ZdcHMinusCtbVtxZCut->Fill(0,0,0);
  }
}

//______________________

void
StHiAnalysis::finishHistograms()
{

}
//______________

Bool_t
StHiAnalysis::acceptEvent(StHiMicroEvent* event)
{
  return CutRc::Accept(event);
}
//_____________________________________

ClassImp(StHiAnalysis)

