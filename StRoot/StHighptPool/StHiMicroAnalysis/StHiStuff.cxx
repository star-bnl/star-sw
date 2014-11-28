/***************************************************************************
 *
 * $Id: StHiStuff.cxx,v 1.5 2004/01/26 22:59:41 perev Exp $
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  Class for making highpt Analysis histograms from highpt
 *		 uDST's
 *
 ***************************************************************************
 *
 * $Log: StHiStuff.cxx,v $
 * Revision 1.5  2004/01/26 22:59:41  perev
 * Add init of statick member
 *
 * Revision 1.4  2003/04/30 20:37:30  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.3  2002/05/31 21:58:30  jklay
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
#include "StHiStuff.h"
Float_t StHiStuff::mMinPt = 2.5;

//__________________

StHiStuff::StHiStuff(const char* inputDir,
			   const char* outRootName)
  :  StHiBaseAnalysis(inputDir,outRootName)
     
{


}

//__________________

StHiStuff::~StHiStuff()
{
}

//_____________________________

Int_t 
StHiStuff::initMore()
{
 
  return 0;
 
}


//_____________________________

void
StHiStuff::initHistograms()
{
  cout << "StHiStuff::initHistograms()" << endl;
  
  char name[100];

  //***********************
  
  using namespace Bin;

  //***********************

  TString sEW[3] = {"East","West","FullTPC"};
  TString sPM[3] = {"Plus","Minus","Charged"};

  // positive, negative
  for(int iew=0; iew<3; iew++) {
    for(int ipm=0; ipm<3; ipm++){
    
      setName(name,"h3PhiGlDcaXYGlVertexZ",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiGlDcaXYGlVertexZ
        = new TH3D(name,name,
	  	 nPhiBin,phiDegMin,phiDegMax,
	  	 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
		 nMidZBigBin,midZBigMin,midZBigMax);
      ew[iew].pm[ipm].h3PhiGlDcaXYGlVertexZ->Sumw2();

      setName(name,"h3PhiPrDcaXYGlVertexZ",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiPrDcaXYGlVertexZ
        = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
		 nMidZBigBin,midZBigMin,midZBigMax);
      ew[iew].pm[ipm].h3PhiPrDcaXYGlVertexZ->Sumw2();

      setName(name,"h1PhiGlReality",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h1PhiGlReality
        = new TH1D(name,name,
		 nPhiBin,phiMin,phiMax);
      ew[iew].pm[ipm].h1PhiGlReality->Sumw2();
    
    
      setName(name,"h3PhiPrDcaXYGlPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiPrDcaXYGlPtPr
        = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiPrDcaXYGlPtPr->Sumw2();
    
      setName(name,"h3PhiGlDcaXYGlPtGl",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiGlDcaXYGlPtGl
        = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiGlDcaXYGlPtGl->Sumw2();
        
      setName(name,"h3SectorDcaXYGlPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3SectorDcaXYGlPtPr
        = new TH3D(name,name,
		 nSectorBin,sectorMin,sectorMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3SectorDcaXYGlPtPr->Sumw2();
    
      setName(name,"h3SectorDcaXYGlPtGl",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3SectorDcaXYGlPtGl
        = new TH3D(name,name,
		 nSectorBin,sectorMin,sectorMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3SectorDcaXYGlPtGl->Sumw2();

      setName(name,"h3SectorVertexZPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3SectorVertexZPtPr
        = new TH3D(name,name,
		 nSectorBin,sectorMin,sectorMax,
		 nAbsZBigBin,absZBigMin,absZBigMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3SectorVertexZPtPr->Sumw2();
    
      setName(name,"h3SectorVertexZPtGl",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3SectorVertexZPtGl
        = new TH3D(name,name,
		 nPhiBin,sectorMin,sectorMax,
		 nAbsZBigBin,absZBigMin,absZBigMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3SectorVertexZPtGl->Sumw2();

      setName(name,"h3PhiPrVertexZPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiPrVertexZPtPr
        = new TH3D(name,name,
		 nSectorBin,phiMin,phiMax,
		 nAbsZBigBin,absZBigMin,absZBigMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiPrVertexZPtPr->Sumw2();
    
      setName(name,"h3PhiGlVertexZPtGl",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiGlVertexZPtGl
        = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nAbsZBigBin,absZBigMin,absZBigMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiGlVertexZPtGl->Sumw2();

      setName(name,"h3PhiPrEtaPrMidZ",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiPrEtaPrMidZ
        = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nEtaBigBin,etaBigMin,etaBigMax,
		 nMidZBigBin,midZBigMin,midZBigMax);
      ew[iew].pm[ipm].h3PhiPrEtaPrMidZ->Sumw2();
    
      setName(name,"h3PhiGlEtaPrMidZ",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiGlEtaPrMidZ
        = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nEtaBigBin,etaBigMin,etaBigMax,
		   nMidZBigBin,midZBigMin,midZBigMax);
      ew[iew].pm[ipm].h3PhiGlEtaPrMidZ->Sumw2();
    
      setName(name,"h1PhiPrHighPtCut",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h1PhiPrHighPtCut
        = new TH1D(name,name,nPhiBin,phiMin,phiMax);
      ew[iew].pm[ipm].h1PhiPrHighPtCut->Sumw2();
    
      setName(name,"h1PhiPrLowPtCut",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h1PhiPrLowPtCut
        = new TH1D(name,name,nPhiBin,phiMin,phiMax);
      ew[iew].pm[ipm].h1PhiPrLowPtCut->Sumw2();
    
      setName(name,"h3PhiPrNFitHitPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiPrNFitHitPtPr
        = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nFitHitThinBin,fitHitThinMin,fitHitThinMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiPrNFitHitPtPr->Sumw2();
    
      setName(name,"h3PhiPrNAllHitPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiPrNAllHitPtPr
        = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nFitHitThinBin,fitHitThinMin,fitHitThinMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiPrNAllHitPtPr->Sumw2();

      setName(name,"h3PhiPrFracHitPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiPrFracHitPtPr
        = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nFracHitBin,fracHitMin,fracHitMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiPrFracHitPtPr->Sumw2();

      setName(name,"h3PhiPrSmallNFitHitPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiPrSmallNFitHitPtPr
        = new TH3D(name,name,
		 nPhiSmallBin,phiMin,phiMax,
		 nFitHitThinBin,fitHitThinMin,fitHitThinMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiPrSmallNFitHitPtPr->Sumw2();
    
      setName(name,"h3PhiPrSmallNAllHitPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiPrSmallNAllHitPtPr
        = new TH3D(name,name,
		 nPhiSmallBin,phiMin,phiMax,
		 nFitHitThinBin,fitHitThinMin,fitHitThinMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiPrSmallNAllHitPtPr->Sumw2();

      setName(name,"h3PhiPrSmallFracHitPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiPrSmallFracHitPtPr
        = new TH3D(name,name,
		 nPhiSmallBin,phiMin,phiMax,
		 nFracHitBin,fracHitMin,fracHitMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiPrSmallFracHitPtPr->Sumw2();


      setName(name,"h3PhiGlNFitHitPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiGlNFitHitPtPr
        = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nFitHitThinBin,fitHitThinMin,fitHitThinMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiGlNFitHitPtPr->Sumw2();
    
      setName(name,"h3PhiGlNAllHitPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiGlNAllHitPtPr
        = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nFitHitThinBin,fitHitThinMin,fitHitThinMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiGlNAllHitPtPr->Sumw2();
    
      setName(name,"h3PhiGlFracHitPtPr",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiGlFracHitPtPr
        = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nFracHitBin,fracHitMin,fracHitMax,
		 nPtBin,ptMin,ptMax);
      ew[iew].pm[ipm].h3PhiGlFracHitPtPr->Sumw2();
    
      // analysis cut
      setName(name,"h3VtxZEtaPrPtPrCut",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3VtxZEtaPrPtPrCut
        = new TH3D(name,name,
		 nVertexZBin,vertexZMin,vertexZMax,
		 nEtaSmallBin,etaSmallMin,etaSmallMax,
		 nPtBin,ptMin,ptMax);
    
      setName(name,"h3VtxZEtaGlPtGlCut",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3VtxZEtaGlPtGlCut
        = new TH3D(name,name,
		 nVertexZBin,vertexZMin,vertexZMax,
		 nEtaSmallBin,etaSmallMin,etaSmallMax,
		 nPtBin,ptMin,ptMax);

      setName(name,"h3ResPtDcaXYGlPtGlEastCut",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3ResPtDcaXYGlPtGlEastCut
        = new TH3D(name,name,
		 nResPtBin,resPtMin,resPtMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);

      setName(name,"h3ResPtDcaXYGlPtGlWestCut",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3ResPtDcaXYGlPtGlWestCut
        = new TH3D(name,name,
		 nResPtBin,resPtMin,resPtMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);

      setName(name,"h3ResPtDcaXYGlPtPrEastCut",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3ResPtDcaXYGlPtPrEastCut
        = new TH3D(name,name,
		 nResPtBin,resPtMin,resPtMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);

      setName(name,"h3ResPtDcaXYGlPtPrWestCut",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3ResPtDcaXYGlPtPrWestCut
        = new TH3D(name,name,
		 nResPtBin,resPtMin,resPtMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);

      setName(name,"h3PhiGlPtPrPtGlEastCut",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiGlPtPrPtGlEastCut
        = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nPtBin,ptMin,ptMax,
		 nPtBin,ptMin,ptMax);
    
      setName(name,"h3PhiGlPtPrPtGlWestCut",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h3PhiGlPtPrPtGlWestCut
        = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nPtBin,ptMin,ptMax,
		 nPtBin,ptMin,ptMax);
    
      setName(name,"h2VtxZLastZ",sEW[iew].Data(),sPM[ipm].Data());
      ew[iew].pm[ipm].h2VtxZLastZ
        = new TH2D(name,name,
		 nVertexZBin,vertexZMin,vertexZMax,
		 nVertexZBin,vertexZMin,vertexZMax);
    
		 
    }	 
  }

}

//_____________________

void 
StHiStuff::trackLoop()
{
  if(mDebug)
    cout << "StHiStuff::trackLoop()" << endl;

  //
  // event stuff
  // 
//VPunused  Float_t zdcSum = mHiMicroEvent->ZDCe() + mHiMicroEvent->ZDCw();
//VPunused  Float_t ctb    = mHiMicroEvent->CTB();
//VPunused  Float_t flowCent   = mHiMicroEvent->Centrality();
//  NchCentrality zdcCent = centrality(zdcSum,ctb);


  Int_t nTrack = mHiMicroEvent->NTrack();
  StHiMicroTrack* track;
  
  for(Int_t i=0; i<nTrack; i++){
    track =(StHiMicroTrack*) mHiMicroEvent->tracks()->At(i);

    Float_t vertexZ = mHiMicroEvent->VertexZ();

    Float_t ptPr  = track->PtPr();
    Float_t ptGl  = track->PtGl();
    Float_t etaGl = track->EtaGl();
    Float_t etaPr = track->EtaPr();
    //Float_t curvPr = track->CurvPr();
    //Float_t curvGl = track->CurvGl();
    //Float_t resPt = (ptPr-ptGl)/ptPr;
    //Float_t resCurvPrGl = (curvPr-curvGl)/curvPr;
    //Float_t resCurvGlPr = (curvGl-curvPr)/curvGl;
    Float_t resPtPrGlOverPr = (ptPr-ptGl)/ptPr;
    Float_t resPtPrGl = (ptPr-ptGl)/ptGl;
    Float_t fracPts = (Float_t) track->FitPts()/ (Float_t) track->AllPts();
    Float_t dcaXYGl = track->DcaXYGl();
    // Float_t dPhi = track->PhiPr() - track->PhiGl();
    Float_t phiPr = track->PhiPr();
    Float_t phiGl = track->PhiGl();
    //Float_t midZ = 100*TMath::Tan(track->DipAnglePr()) + vertexZ;
    Int_t   fitPts = track->FitPts();
    Int_t   allPts = track->AllPts();
//VPunused    Int_t   charge = track->Charge();
    Float_t phiGlDeg = phiGl*180./TMath::Pi();
    Float_t phiPrDeg = phiPr*180./TMath::Pi();
    Int_t firstSector = track->FirstSector();

    Int_t sector = findSector(phiGl,firstSector);

    phiGlDeg = (phiGlDeg<-165) ? (phiGlDeg += 360) : phiGlDeg;
    phiPrDeg = (phiPrDeg<-165) ? (phiPrDeg += 360) : phiPrDeg;

    //Float_t exitZ = vtxZ+200*TMath::Tan(track->DipAnglePr());

    Int_t iCharge = (track->Charge()>0) ? 0 : 1; //plus is 0
    //This definition of iSide only requires the track be on
    //one half or the other and doesn't specify vertex position
    //Need to make that cut explicitly using the cutList
    Int_t iSide = (CutRc::AcceptEastSideTrack(track)) ? 0 : 1; //east is 0

    // ** analysis cut
    if(CutRc::AcceptTrackVtxZHalf(track,vertexZ)){
      // fit points
      if(CutRc::AcceptFitPts(track)){
	ew[iSide].pm[iCharge].h2VtxZLastZ->Fill(vertexZ,track->LastZ());
	ew[2].pm[2].h2VtxZLastZ->Fill(vertexZ,track->LastZ());
	ew[2].pm[iCharge].h2VtxZLastZ->Fill(vertexZ,track->LastZ());
	ew[iSide].pm[2].h2VtxZLastZ->Fill(vertexZ,track->LastZ());

	ew[iSide].pm[iCharge].h3VtxZEtaPrPtPrCut->Fill(vertexZ,etaPr,ptPr);
	ew[2].pm[2].h3VtxZEtaPrPtPrCut->Fill(vertexZ,etaPr,ptPr);
	ew[2].pm[iCharge].h3VtxZEtaPrPtPrCut->Fill(vertexZ,etaPr,ptPr);
	ew[iSide].pm[2].h3VtxZEtaPrPtPrCut->Fill(vertexZ,etaPr,ptPr);

	ew[iSide].pm[iCharge].h3VtxZEtaGlPtGlCut->Fill(vertexZ,etaGl,ptGl);
	ew[2].pm[2].h3VtxZEtaGlPtGlCut->Fill(vertexZ,etaGl,ptGl);
	ew[2].pm[iCharge].h3VtxZEtaGlPtGlCut->Fill(vertexZ,etaGl,ptGl);
	ew[iSide].pm[2].h3VtxZEtaGlPtGlCut->Fill(vertexZ,etaGl,ptGl);
	
	if(fabs(track->EtaGl())<0.7){
	  if(track->FirstZ() < 0 ) { //east
	    ew[iSide].pm[iCharge].h3ResPtDcaXYGlPtGlEastCut->Fill(resPtPrGl,dcaXYGl,ptGl);
	    ew[2].pm[2].h3ResPtDcaXYGlPtGlEastCut->Fill(resPtPrGl,dcaXYGl,ptGl);
	    ew[2].pm[iCharge].h3ResPtDcaXYGlPtGlEastCut->Fill(resPtPrGl,dcaXYGl,ptGl);
	    ew[iSide].pm[2].h3ResPtDcaXYGlPtGlEastCut->Fill(resPtPrGl,dcaXYGl,ptGl);

	    ew[iSide].pm[iCharge].h3ResPtDcaXYGlPtPrEastCut->Fill(resPtPrGlOverPr,dcaXYGl,ptPr);
	    ew[2].pm[2].h3ResPtDcaXYGlPtPrEastCut->Fill(resPtPrGlOverPr,dcaXYGl,ptPr);
	    ew[2].pm[iCharge].h3ResPtDcaXYGlPtPrEastCut->Fill(resPtPrGlOverPr,dcaXYGl,ptPr);
	    ew[iSide].pm[2].h3ResPtDcaXYGlPtPrEastCut->Fill(resPtPrGlOverPr,dcaXYGl,ptPr);
	    if(fabs(track->DcaXYGl())<1.2)
	      ew[iSide].pm[iCharge].h3PhiGlPtPrPtGlEastCut->Fill(phiGlDeg,ptPr,ptGl);
	      ew[2].pm[2].h3PhiGlPtPrPtGlEastCut->Fill(phiGlDeg,ptPr,ptGl);
	      ew[2].pm[iCharge].h3PhiGlPtPrPtGlEastCut->Fill(phiGlDeg,ptPr,ptGl);
	      ew[iSide].pm[2].h3PhiGlPtPrPtGlEastCut->Fill(phiGlDeg,ptPr,ptGl);
	  }
	  else{
	    ew[iSide].pm[iCharge].h3ResPtDcaXYGlPtGlWestCut->Fill(resPtPrGl,dcaXYGl,ptGl);
	    ew[2].pm[2].h3ResPtDcaXYGlPtGlWestCut->Fill(resPtPrGl,dcaXYGl,ptGl);
	    ew[2].pm[iCharge].h3ResPtDcaXYGlPtGlWestCut->Fill(resPtPrGl,dcaXYGl,ptGl);
	    ew[iSide].pm[2].h3ResPtDcaXYGlPtGlWestCut->Fill(resPtPrGl,dcaXYGl,ptGl);

	    ew[iSide].pm[iCharge].h3ResPtDcaXYGlPtPrWestCut->Fill(resPtPrGlOverPr,dcaXYGl,ptPr);
	    ew[2].pm[2].h3ResPtDcaXYGlPtPrWestCut->Fill(resPtPrGlOverPr,dcaXYGl,ptPr);
	    ew[2].pm[iCharge].h3ResPtDcaXYGlPtPrWestCut->Fill(resPtPrGlOverPr,dcaXYGl,ptPr);
	    ew[iSide].pm[2].h3ResPtDcaXYGlPtPrWestCut->Fill(resPtPrGlOverPr,dcaXYGl,ptPr);
	    if(fabs(track->DcaXYGl())<1.2)
	      ew[iSide].pm[iCharge].h3PhiGlPtPrPtGlWestCut->Fill(phiGlDeg,ptPr,ptGl);
	      ew[2].pm[2].h3PhiGlPtPrPtGlWestCut->Fill(phiGlDeg,ptPr,ptGl);
	      ew[2].pm[iCharge].h3PhiGlPtPrPtGlWestCut->Fill(phiGlDeg,ptPr,ptGl);
	      ew[iSide].pm[2].h3PhiGlPtPrPtGlWestCut->Fill(phiGlDeg,ptPr,ptGl);
	  }
	}
      }
    }

    //************ plus/minus
    //***eta 
    //
    if(CutRc::AcceptEta(track)){
      
      //
      // reality check
      //
      //if(fabs(etaPr)>.5) cout << "huh?" << endl;

      // eta and fit pts
      //
      if(CutRc::AcceptFitPts(track)){
	ew[iSide].pm[iCharge].h3PhiPrDcaXYGlPtPr->Fill(phiPrDeg,dcaXYGl,ptPr);
	ew[2].pm[2].h3PhiPrDcaXYGlPtPr->Fill(phiPrDeg,dcaXYGl,ptPr);
	ew[2].pm[iCharge].h3PhiPrDcaXYGlPtPr->Fill(phiPrDeg,dcaXYGl,ptPr);
	ew[iSide].pm[2].h3PhiPrDcaXYGlPtPr->Fill(phiPrDeg,dcaXYGl,ptPr);

	ew[iSide].pm[iCharge].h3PhiGlDcaXYGlPtGl->Fill(phiGlDeg,dcaXYGl,ptGl);
	ew[2].pm[2].h3PhiGlDcaXYGlPtGl->Fill(phiGlDeg,dcaXYGl,ptGl);
	ew[2].pm[iCharge].h3PhiGlDcaXYGlPtGl->Fill(phiGlDeg,dcaXYGl,ptGl);
	ew[iSide].pm[2].h3PhiGlDcaXYGlPtGl->Fill(phiGlDeg,dcaXYGl,ptGl);

	ew[iSide].pm[iCharge].h3SectorDcaXYGlPtPr->Fill(sector,dcaXYGl,ptPr);
	ew[2].pm[2].h3SectorDcaXYGlPtPr->Fill(sector,dcaXYGl,ptPr);
	ew[2].pm[iCharge].h3SectorDcaXYGlPtPr->Fill(sector,dcaXYGl,ptPr);
	ew[iSide].pm[2].h3SectorDcaXYGlPtPr->Fill(sector,dcaXYGl,ptPr);

	ew[iSide].pm[iCharge].h3SectorDcaXYGlPtGl->Fill(sector,dcaXYGl,ptGl);
	ew[2].pm[2].h3SectorDcaXYGlPtGl->Fill(sector,dcaXYGl,ptGl);
	ew[2].pm[iCharge].h3SectorDcaXYGlPtGl->Fill(sector,dcaXYGl,ptGl);
	ew[iSide].pm[2].h3SectorDcaXYGlPtGl->Fill(sector,dcaXYGl,ptGl);
	
      }

      // eta, sdca, and fitpts
      //
      if(CutRc::AcceptSDcaGl(track) && CutRc::AcceptFitPts(track)){
	ew[iSide].pm[iCharge].h3PhiPrNFitHitPtPr->Fill(phiPr,fitPts,ptPr);
	ew[2].pm[2].h3PhiPrNFitHitPtPr->Fill(phiPr,fitPts,ptPr);
	ew[2].pm[iCharge].h3PhiPrNFitHitPtPr->Fill(phiPr,fitPts,ptPr);
	ew[iSide].pm[2].h3PhiPrNFitHitPtPr->Fill(phiPr,fitPts,ptPr);

	ew[iSide].pm[iCharge].h3PhiPrSmallNFitHitPtPr->Fill(phiPr,fitPts,ptPr);
	ew[2].pm[2].h3PhiPrSmallNFitHitPtPr->Fill(phiPr,fitPts,ptPr);
	ew[2].pm[iCharge].h3PhiPrSmallNFitHitPtPr->Fill(phiPr,fitPts,ptPr);
	ew[iSide].pm[2].h3PhiPrSmallNFitHitPtPr->Fill(phiPr,fitPts,ptPr);

	ew[iSide].pm[iCharge].h3PhiGlNFitHitPtPr->Fill(phiGl,fitPts,ptPr);
	ew[2].pm[2].h3PhiGlNFitHitPtPr->Fill(phiGl,fitPts,ptPr);
	ew[2].pm[iCharge].h3PhiGlNFitHitPtPr->Fill(phiGl,fitPts,ptPr);
	ew[iSide].pm[2].h3PhiGlNFitHitPtPr->Fill(phiGl,fitPts,ptPr);

	ew[iSide].pm[iCharge].h3PhiPrNAllHitPtPr->Fill(phiPr,allPts,ptPr);
	ew[2].pm[2].h3PhiPrNAllHitPtPr->Fill(phiPr,allPts,ptPr);
	ew[2].pm[iCharge].h3PhiPrNAllHitPtPr->Fill(phiPr,allPts,ptPr);
	ew[iSide].pm[2].h3PhiPrNAllHitPtPr->Fill(phiPr,allPts,ptPr);

	ew[iSide].pm[iCharge].h3PhiPrSmallNAllHitPtPr->Fill(phiPr,allPts,ptPr);
	ew[2].pm[2].h3PhiPrSmallNAllHitPtPr->Fill(phiPr,allPts,ptPr);
	ew[2].pm[iCharge].h3PhiPrSmallNAllHitPtPr->Fill(phiPr,allPts,ptPr);
	ew[iSide].pm[2].h3PhiPrSmallNAllHitPtPr->Fill(phiPr,allPts,ptPr);

	ew[iSide].pm[iCharge].h3PhiGlNAllHitPtPr->Fill(phiGl,allPts,ptPr);
	ew[2].pm[2].h3PhiGlNAllHitPtPr->Fill(phiGl,allPts,ptPr);
	ew[2].pm[iCharge].h3PhiGlNAllHitPtPr->Fill(phiGl,allPts,ptPr);
	ew[iSide].pm[2].h3PhiGlNAllHitPtPr->Fill(phiGl,allPts,ptPr);

	ew[iSide].pm[iCharge].h3PhiPrFracHitPtPr->Fill(phiPr,fracPts,ptPr);
	ew[2].pm[2].h3PhiPrFracHitPtPr->Fill(phiPr,fracPts,ptPr);
	ew[2].pm[iCharge].h3PhiPrFracHitPtPr->Fill(phiPr,fracPts,ptPr);
	ew[iSide].pm[2].h3PhiPrFracHitPtPr->Fill(phiPr,fracPts,ptPr);

	ew[iSide].pm[iCharge].h3PhiPrSmallFracHitPtPr->Fill(phiPr,fracPts,ptPr);
	ew[2].pm[2].h3PhiPrSmallFracHitPtPr->Fill(phiPr,fracPts,ptPr);
	ew[2].pm[iCharge].h3PhiPrSmallFracHitPtPr->Fill(phiPr,fracPts,ptPr);
	ew[iSide].pm[2].h3PhiPrSmallFracHitPtPr->Fill(phiPr,fracPts,ptPr);

	ew[iSide].pm[iCharge].h3PhiGlFracHitPtPr->Fill(phiGl,fracPts,ptPr);
	ew[2].pm[2].h3PhiGlFracHitPtPr->Fill(phiGl,fracPts,ptPr);
	ew[2].pm[iCharge].h3PhiGlFracHitPtPr->Fill(phiGl,fracPts,ptPr);
	ew[iSide].pm[2].h3PhiGlFracHitPtPr->Fill(phiGl,fracPts,ptPr);

	// reality check
	if(ptPr>2 && ptPr<6){
	  ew[iSide].pm[iCharge].h1PhiPrHighPtCut->Fill(phiPr);	  
	  ew[2].pm[2].h1PhiPrHighPtCut->Fill(phiPr);	  
	  ew[2].pm[iCharge].h1PhiPrHighPtCut->Fill(phiPr);	  
	  ew[iSide].pm[2].h1PhiPrHighPtCut->Fill(phiPr);	  
	}
	if(ptPr>1 && ptPr<2){
	  ew[iSide].pm[iCharge].h1PhiPrLowPtCut->Fill(phiPr);
	  ew[2].pm[2].h1PhiPrLowPtCut->Fill(phiPr);
	  ew[2].pm[iCharge].h1PhiPrLowPtCut->Fill(phiPr);
	  ew[iSide].pm[2].h1PhiPrLowPtCut->Fill(phiPr);
	}
      }

    }

    // sdca, fitpts, tight eta
    
    if(CutRc::AcceptSDcaGl(track) && CutRc::AcceptFitPts(track) &&
       fabs(etaPr)<.1){
      ew[iSide].pm[iCharge].h3PhiPrVertexZPtPr->Fill(phiPr,fabs(vertexZ),ptPr);
      ew[2].pm[2].h3PhiPrVertexZPtPr->Fill(phiPr,fabs(vertexZ),ptPr);
      ew[2].pm[iCharge].h3PhiPrVertexZPtPr->Fill(phiPr,fabs(vertexZ),ptPr);
      ew[iSide].pm[2].h3PhiPrVertexZPtPr->Fill(phiPr,fabs(vertexZ),ptPr);

      ew[iSide].pm[iCharge].h3PhiGlVertexZPtGl->Fill(phiGl,fabs(vertexZ),ptGl);
      ew[2].pm[2].h3PhiGlVertexZPtGl->Fill(phiGl,fabs(vertexZ),ptGl);
      ew[2].pm[iCharge].h3PhiGlVertexZPtGl->Fill(phiGl,fabs(vertexZ),ptGl);
      ew[iSide].pm[2].h3PhiGlVertexZPtGl->Fill(phiGl,fabs(vertexZ),ptGl);

	ew[iSide].pm[iCharge].h3SectorVertexZPtPr->Fill(sector,fabs(vertexZ),ptPr);
	ew[2].pm[2].h3SectorVertexZPtPr->Fill(sector,fabs(vertexZ),ptPr);
	ew[2].pm[iCharge].h3SectorVertexZPtPr->Fill(sector,fabs(vertexZ),ptPr);
	ew[iSide].pm[2].h3SectorVertexZPtPr->Fill(sector,fabs(vertexZ),ptPr);

	ew[iSide].pm[iCharge].h3SectorVertexZPtGl->Fill(sector,fabs(vertexZ),ptGl);
	ew[2].pm[2].h3SectorVertexZPtGl->Fill(sector,fabs(vertexZ),ptGl);
	ew[2].pm[iCharge].h3SectorVertexZPtGl->Fill(sector,fabs(vertexZ),ptGl);
	ew[iSide].pm[2].h3SectorVertexZPtGl->Fill(sector,fabs(vertexZ),ptGl);


    }

    // fit pts, tight eta, pt
    if(CutRc::AcceptFitPts(track) && fabs(etaPr)<0.1 &&
       fabs(track->DcaGl())<3){
      if(ptGl>1.5 && ptGl<6)
	ew[iSide].pm[iCharge].h3PhiGlDcaXYGlVertexZ->Fill(phiGlDeg,dcaXYGl,vertexZ);
	ew[2].pm[2].h3PhiGlDcaXYGlVertexZ->Fill(phiGlDeg,dcaXYGl,vertexZ);
	ew[2].pm[iCharge].h3PhiGlDcaXYGlVertexZ->Fill(phiGlDeg,dcaXYGl,vertexZ);
	ew[iSide].pm[2].h3PhiGlDcaXYGlVertexZ->Fill(phiGlDeg,dcaXYGl,vertexZ);
      if(ptPr>1.5 && ptPr<6)
	ew[iSide].pm[iCharge].h3PhiPrDcaXYGlVertexZ->Fill(phiPrDeg,dcaXYGl,vertexZ);
	ew[2].pm[2].h3PhiPrDcaXYGlVertexZ->Fill(phiPrDeg,dcaXYGl,vertexZ);
	ew[2].pm[iCharge].h3PhiPrDcaXYGlVertexZ->Fill(phiPrDeg,dcaXYGl,vertexZ);
	ew[iSide].pm[2].h3PhiPrDcaXYGlVertexZ->Fill(phiPrDeg,dcaXYGl,vertexZ);
    }
    
  } // tracks

  if(mDebug)
    cout << "\ttracks : " << nTrack << endl;

}
//_____________________

void
StHiStuff::fillEventHistograms()
{

}

//______________________

void
StHiStuff::finishHistograms()
{

}

//_______________________

Bool_t
StHiStuff::acceptEvent(StHiMicroEvent* event)
{
  return CutRc::Accept(event);

}



//_______________________

// ew: east is 0, west is 1

Int_t
StHiStuff::findSector(Float_t phi, Int_t firstSector)
{
  Int_t sector=0;
  Float_t pi = (Float_t) TMath::Pi();
  Float_t sectorWidth = pi/6.;

  Int_t sectorWest[] = {8,7,6,5,4,3,2,1,12,11,10};
  Int_t sectorEast[] = {16,17,18,19,20,21,22,23,24,13,14};
  
  if(phi>=(pi-sectorWidth/2.) || phi < (-pi+sectorWidth/2.)){
    if(firstSector <= 12 ){
      sector = 9;
    }
    else if(firstSector >= 13){
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
	
	if(firstSector >= 13) sector = sectorWest[i];
	else if(firstSector <= 12) sector = sectorEast[i];
	
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

ClassImp(StHiStuff)
