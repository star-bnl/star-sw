/***************************************************************************
 *
 * $Id: StHiStuff.cxx,v 1.1 2002/04/02 20:05:18 jklay Exp $
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
 * Revision 1.1  2002/04/02 20:05:18  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 **************************************************************************/
#include "StHiStuff.h"

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

  TString sPM[2] = {"plus","minus"};

  Double_t sectorMin, sectorMax;

  if(CutRc::Half() == 'w'){
    sectorMin = sectorWestMin;
    sectorMax = sectorWestMax;
  }
  else{
    sectorMin = sectorEastMin;
    sectorMax = sectorEastMax;
  }

  // postive + negative.
  
  



  // positive, negative
  for(int ipm=0; ipm<2; ipm++){
    
    setName(name,"h3PhiGlDcaXYGlVertexZ",sPM[ipm].Data());
    pm[ipm].h3PhiGlDcaXYGlVertexZ
      = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
		 nMidZBigBin,midZBigMin,midZBigMax);
    pm[ipm].h3PhiGlDcaXYGlVertexZ->Sumw2();

    setName(name,"h3PhiPrDcaXYGlVertexZ",sPM[ipm].Data());
    pm[ipm].h3PhiPrDcaXYGlVertexZ
      = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
		 nMidZBigBin,midZBigMin,midZBigMax);
    pm[ipm].h3PhiPrDcaXYGlVertexZ->Sumw2();

    setName(name,"h1PhiGlReality",sPM[ipm].Data());
    pm[ipm].h1PhiGlReality
      = new TH1D(name,name,
		 nPhiBin,phiMin,phiMax);
    pm[ipm].h1PhiGlReality->Sumw2();
    
    
    setName(name,"h3PhiPrDcaXYGlPtPr",sPM[ipm].Data());
    pm[ipm].h3PhiPrDcaXYGlPtPr
      = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiPrDcaXYGlPtPr->Sumw2();
    
    setName(name,"h3PhiGlDcaXYGlPtGl",sPM[ipm].Data());
    pm[ipm].h3PhiGlDcaXYGlPtGl
      = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiGlDcaXYGlPtGl->Sumw2();
        
    setName(name,"h3SectorDcaXYGlPtPr",sPM[ipm].Data());
    pm[ipm].h3SectorDcaXYGlPtPr
      = new TH3D(name,name,
		 nSectorBin,sectorMin,sectorMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3SectorDcaXYGlPtPr->Sumw2();
    
    setName(name,"h3SectorDcaXYGlPtGl",sPM[ipm].Data());
    pm[ipm].h3SectorDcaXYGlPtGl
      = new TH3D(name,name,
		 nSectorBin,sectorMin,sectorMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3SectorDcaXYGlPtGl->Sumw2();

    setName(name,"h3SectorVertexZPtPr",sPM[ipm].Data());
    pm[ipm].h3SectorVertexZPtPr
      = new TH3D(name,name,
		 nSectorBin,sectorMin,sectorMax,
		 nAbsZBigBin,absZBigMin,absZBigMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3SectorVertexZPtPr->Sumw2();
    
    setName(name,"h3SectorVertexZPtGl",sPM[ipm].Data());
    pm[ipm].h3SectorVertexZPtGl
      = new TH3D(name,name,
		 nPhiBin,sectorMin,sectorMax,
		 nAbsZBigBin,absZBigMin,absZBigMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3SectorVertexZPtGl->Sumw2();

    setName(name,"h3PhiPrVertexZPtPr",sPM[ipm].Data());
    pm[ipm].h3PhiPrVertexZPtPr
      = new TH3D(name,name,
		 nSectorBin,phiMin,phiMax,
		 nAbsZBigBin,absZBigMin,absZBigMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiPrVertexZPtPr->Sumw2();
    
    setName(name,"h3PhiGlVertexZPtGl",sPM[ipm].Data());
    pm[ipm].h3PhiGlVertexZPtGl
      = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nAbsZBigBin,absZBigMin,absZBigMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiGlVertexZPtGl->Sumw2();

    setName(name,"h3PhiPrEtaPrMidZ",sPM[ipm].Data());
    pm[ipm].h3PhiPrEtaPrMidZ
      = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nEtaBigBin,etaBigMin,etaBigMax,
		 nMidZBigBin,midZBigMin,midZBigMax);
    pm[ipm].h3PhiPrEtaPrMidZ->Sumw2();
    
    setName(name,"h3PhiGlEtaPrMidZ",sPM[ipm].Data());
    pm[ipm].h3PhiGlEtaPrMidZ
      = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nEtaBigBin,etaBigMin,etaBigMax,
		   nMidZBigBin,midZBigMin,midZBigMax);
    pm[ipm].h3PhiGlEtaPrMidZ->Sumw2();
    
    setName(name,"h1PhiPrHighPtCut",sPM[ipm].Data());
    pm[ipm].h1PhiPrHighPtCut
      = new TH1D(name,name,nPhiBin,phiMin,phiMax);
    pm[ipm].h1PhiPrHighPtCut->Sumw2();
    
    setName(name,"h1PhiPrLowPtCut",sPM[ipm].Data());
    pm[ipm].h1PhiPrLowPtCut
      = new TH1D(name,name,nPhiBin,phiMin,phiMax);
    pm[ipm].h1PhiPrLowPtCut->Sumw2();
    
    setName(name,"h3PhiPrNFitHitPtPr",sPM[ipm].Data());
    pm[ipm].h3PhiPrNFitHitPtPr
      = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nFitHitThinBin,fitHitThinMin,fitHitThinMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiPrNFitHitPtPr->Sumw2();
    
    setName(name,"h3PhiPrNAllHitPtPr",sPM[ipm].Data());
    pm[ipm].h3PhiPrNAllHitPtPr
      = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nFitHitThinBin,fitHitThinMin,fitHitThinMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiPrNAllHitPtPr->Sumw2();

    setName(name,"h3PhiPrFracHitPtPr",sPM[ipm].Data());
    pm[ipm].h3PhiPrFracHitPtPr
      = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nFracHitBin,fracHitMin,fracHitMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiPrFracHitPtPr->Sumw2();

    setName(name,"h3PhiPrSmallNFitHitPtPr",sPM[ipm].Data());
    pm[ipm].h3PhiPrSmallNFitHitPtPr
      = new TH3D(name,name,
		 nPhiSmallBin,phiMin,phiMax,
		 nFitHitThinBin,fitHitThinMin,fitHitThinMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiPrSmallNFitHitPtPr->Sumw2();
    
    setName(name,"h3PhiPrSmallNAllHitPtPr",sPM[ipm].Data());
    pm[ipm].h3PhiPrSmallNAllHitPtPr
      = new TH3D(name,name,
		 nPhiSmallBin,phiMin,phiMax,
		 nFitHitThinBin,fitHitThinMin,fitHitThinMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiPrSmallNAllHitPtPr->Sumw2();

    setName(name,"h3PhiPrSmallFracHitPtPr",sPM[ipm].Data());
    pm[ipm].h3PhiPrSmallFracHitPtPr
      = new TH3D(name,name,
		 nPhiSmallBin,phiMin,phiMax,
		 nFracHitBin,fracHitMin,fracHitMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiPrSmallFracHitPtPr->Sumw2();


    setName(name,"h3PhiGlNFitHitPtPr",sPM[ipm].Data());
    pm[ipm].h3PhiGlNFitHitPtPr
      = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nFitHitThinBin,fitHitThinMin,fitHitThinMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiGlNFitHitPtPr->Sumw2();
    
    setName(name,"h3PhiGlNAllHitPtPr",sPM[ipm].Data());
    pm[ipm].h3PhiGlNAllHitPtPr
      = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nFitHitThinBin,fitHitThinMin,fitHitThinMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiGlNAllHitPtPr->Sumw2();
    
    setName(name,"h3PhiGlFracHitPtPr",sPM[ipm].Data());
    pm[ipm].h3PhiGlFracHitPtPr
      = new TH3D(name,name,
		 nPhiBin,phiMin,phiMax,
		 nFracHitBin,fracHitMin,fracHitMax,
		 nPtBin,ptMin,ptMax);
    pm[ipm].h3PhiGlFracHitPtPr->Sumw2();
    
    // analysis cut
    setName(name,"h3VtxZEtaPrPtPrCut",sPM[ipm].Data());
    pm[ipm].h3VtxZEtaPrPtPrCut
      = new TH3D(name,name,
		 nVertexZBin,vertexZMin,vertexZMax,
		 nEtaSmallBin,etaSmallMin,etaSmallMax,
		 nPtBin,ptMin,ptMax);
    
    setName(name,"h3VtxZEtaGlPtGlCut",sPM[ipm].Data());
    pm[ipm].h3VtxZEtaGlPtGlCut
      = new TH3D(name,name,
		 nVertexZBin,vertexZMin,vertexZMax,
		 nEtaSmallBin,etaSmallMin,etaSmallMax,
		 nPtBin,ptMin,ptMax);

    setName(name,"h3ResPtDcaXYGlPtGlEastCut",sPM[ipm].Data());
    pm[ipm].h3ResPtDcaXYGlPtGlEastCut
      = new TH3D(name,name,
		 nResPtBin,resPtMin,resPtMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);

    setName(name,"h3ResPtDcaXYGlPtGlWestCut",sPM[ipm].Data());
    pm[ipm].h3ResPtDcaXYGlPtGlWestCut
      = new TH3D(name,name,
		 nResPtBin,resPtMin,resPtMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);

    setName(name,"h3ResPtDcaXYGlPtPrEastCut",sPM[ipm].Data());
    pm[ipm].h3ResPtDcaXYGlPtPrEastCut
      = new TH3D(name,name,
		 nResPtBin,resPtMin,resPtMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);

    setName(name,"h3ResPtDcaXYGlPtPrWestCut",sPM[ipm].Data());
    pm[ipm].h3ResPtDcaXYGlPtPrWestCut
      = new TH3D(name,name,
		 nResPtBin,resPtMin,resPtMax,
		 nDcaXYGlWideBin,dcaXYGlMin,dcaXYGlMax,
		 nPtBin,ptMin,ptMax);

    setName(name,"h3PhiGlPtPrPtGlEastCut",sPM[ipm].Data());
    pm[ipm].h3PhiGlPtPrPtGlEastCut
      = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nPtBin,ptMin,ptMax,
		 nPtBin,ptMin,ptMax);
    
    setName(name,"h3PhiGlPtPrPtGlWestCut",sPM[ipm].Data());
    pm[ipm].h3PhiGlPtPrPtGlWestCut
      = new TH3D(name,name,
		 nPhiBin,phiDegMin,phiDegMax,
		 nPtBin,ptMin,ptMax,
		 nPtBin,ptMin,ptMax);
    
    setName(name,"h2VtxZLastZ",sPM[ipm].Data());
    pm[ipm].h2VtxZLastZ
      = new TH2D(name,name,
		 nVertexZBin,vertexZMin,vertexZMax,
		 nVertexZBin,vertexZMin,vertexZMax);
    
		 
		 
    


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
  Float_t zdcSum = mHiMicroEvent->mZDCe + mHiMicroEvent->mZDCw;
  Float_t ctb    = mHiMicroEvent->mCTB;
  Float_t flowCent   = mHiMicroEvent->mCentrality;
  NchCentrality zdcCent = centrality(zdcSum,ctb);


  Int_t nTrack = mHiMicroEvent->mNTrack;
  StHiMicroTrack* track;
  
  for(Int_t i=0; i<nTrack; i++){
    track =(StHiMicroTrack*) mHiMicroEvent->tracks()->At(i);

    Float_t vertexZ = mHiMicroEvent->mVertexZ;

    Float_t ptPr  = track->mPtPr;
    Float_t ptGl  = track->mPtGl;
    Float_t etaGl = track->mEtaGl;
    Float_t etaPr = track->mEtaPr;
    //Float_t curvPr = track->mCurvPr;
    //Float_t curvGl = track->mCurvGl;
    //Float_t resPt = (ptPr-ptGl)/ptPr;
    //Float_t resCurvPrGl = (curvPr-curvGl)/curvPr;
    //Float_t resCurvGlPr = (curvGl-curvPr)/curvGl;
    Float_t resPtPrGlOverPr = (ptPr-ptGl)/ptPr;
    Float_t resPtPrGl = (ptPr-ptGl)/ptGl;
    Float_t fracPts = (Float_t) track->mFitPts/ (Float_t) track->mAllPts;
    Float_t dcaXYGl = track->mDcaXYGl;
    // Float_t dPhi = track->mPhiPr - track->mPhiGl;
    Float_t phiPr = track->mPhiPr;
    Float_t phiGl = track->mPhiGl;
    //Float_t midZ = 100*TMath::Tan(track->mDipAnglePr) + vertexZ;
    Int_t   fitPts = track->mFitPts;
    Int_t   allPts = track->mAllPts;
    Int_t   charge = track->mCharge;
    Float_t phiGlDeg = phiGl*180./TMath::Pi();
    Float_t phiPrDeg = phiPr*180./TMath::Pi();

    phiGlDeg = (phiGlDeg<-165) ? (phiGlDeg += 360) : phiGlDeg;
    phiPrDeg = (phiPrDeg<-165) ? (phiPrDeg += 360) : phiPrDeg;

    //Float_t exitZ = vtxZ+200*TMath::Tan(track->mDipAnglePr);

    // east west cut
    if(
       CutRc::Half() && 
       !CutRc::IsSameSide(vertexZ,track->mFirstZ,track->mLastZ)
       ) continue;

    Int_t iCharge = (track->mCharge>0) ? 0 : 1; //plus is 0

    // ** analysis cut
    if(CutRc::AcceptTrackHalf(track,vertexZ)){
      // fit points
      if(CutRc::AcceptFitPts(track)){
	pm[iCharge].h2VtxZLastZ->Fill(vertexZ,track->mLastZ);

	pm[iCharge].h3VtxZEtaPrPtPrCut->Fill(vertexZ,etaPr,ptPr);
	pm[iCharge].h3VtxZEtaGlPtGlCut->Fill(vertexZ,etaGl,ptGl);
	
	if(fabs(track->mEtaGl)<0.7){
	  if(track->mFirstZ < 0 ) { //east
	    pm[iCharge].h3ResPtDcaXYGlPtGlEastCut->Fill(resPtPrGl,dcaXYGl,
							ptGl);
	    pm[iCharge].h3ResPtDcaXYGlPtPrEastCut->Fill(resPtPrGlOverPr,dcaXYGl,
							ptPr);
	    if(fabs(track->mDcaXYGl)<1.2)
	      pm[iCharge].h3PhiGlPtPrPtGlEastCut->Fill(phiGlDeg,ptPr,ptGl);
	  }
	  else{
	    pm[iCharge].h3ResPtDcaXYGlPtGlWestCut->Fill(resPtPrGl,dcaXYGl,ptGl);
	    pm[iCharge].h3ResPtDcaXYGlPtPrWestCut->Fill(resPtPrGlOverPr,dcaXYGl,ptPr);
	    if(fabs(track->mDcaXYGl)<1.2)
	      pm[iCharge].h3PhiGlPtPrPtGlWestCut->Fill(phiGlDeg,ptPr,ptGl);
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
	pm[iCharge].h3PhiPrDcaXYGlPtPr->Fill(phiPrDeg,dcaXYGl,ptPr);
	pm[iCharge].h3PhiGlDcaXYGlPtGl->Fill(phiGlDeg,dcaXYGl,ptGl);

	Int_t sector;
	if(CutRc::Half() == 'w'){
	  sector = findSector(phiGl,'w');
	}
	else if(CutRc::Half() == 'e'){
	  sector = findSector(phiGl,'e');
	}
	pm[iCharge].h3SectorDcaXYGlPtPr->Fill(sector,dcaXYGl,ptPr);
	pm[iCharge].h3SectorDcaXYGlPtGl->Fill(sector,dcaXYGl,ptGl);
	
      }

      // eta, sdca, and fitpts
      //
      if(CutRc::AcceptSDcaGl(track) && CutRc::AcceptFitPts(track)){
	pm[iCharge].h3PhiPrNFitHitPtPr->Fill(phiPr,fitPts,ptPr);
	pm[iCharge].h3PhiPrSmallNFitHitPtPr->Fill(phiPr,fitPts,ptPr);
	pm[iCharge].h3PhiGlNFitHitPtPr->Fill(phiGl,fitPts,ptPr);

	pm[iCharge].h3PhiPrNAllHitPtPr->Fill(phiPr,allPts,ptPr);
	pm[iCharge].h3PhiPrSmallNAllHitPtPr->Fill(phiPr,allPts,ptPr);
	pm[iCharge].h3PhiGlNAllHitPtPr->Fill(phiGl,allPts,ptPr);

	pm[iCharge].h3PhiPrFracHitPtPr->Fill(phiPr,fracPts,ptPr);
	pm[iCharge].h3PhiPrSmallFracHitPtPr->Fill(phiPr,fracPts,ptPr);
	pm[iCharge].h3PhiGlFracHitPtPr->Fill(phiGl,fracPts,ptPr);

	// reality check
	if(ptPr>2 && ptPr<6){
	  pm[iCharge].h1PhiPrHighPtCut->Fill(phiPr);	  
	}
	if(ptPr>1 && ptPr<2){
	  pm[iCharge].h1PhiPrLowPtCut->Fill(phiPr);
	}
      }

    }

    // sdca, fitpts, tight eta
    
    if(CutRc::AcceptSDcaGl(track) && CutRc::AcceptFitPts(track) &&
       fabs(etaPr)<.1){
      pm[iCharge].h3PhiPrVertexZPtPr->Fill(phiPr,fabs(vertexZ),ptPr);
      pm[iCharge].h3PhiGlVertexZPtGl->Fill(phiGl,fabs(vertexZ),ptGl);

      Int_t sector;
	if(CutRc::Half() == 'w'){
	  sector = findSector(phiGl,'w');
	}
	else if(CutRc::Half() == 'e'){
	  sector = findSector(phiGl,'e');
	}
	pm[iCharge].h3SectorVertexZPtPr->Fill(sector,fabs(vertexZ),ptPr);
	pm[iCharge].h3SectorVertexZPtGl->Fill(sector,fabs(vertexZ),ptGl);


    }

    // fit pts, tight eta, pt
    if(CutRc::AcceptFitPts(track) && fabs(etaPr)<0.1 &&
       fabs(track->mDcaGl)<3){
      if(ptGl>1.5 && ptGl<6)
	pm[iCharge].h3PhiGlDcaXYGlVertexZ->Fill(phiGlDeg,dcaXYGl,vertexZ);
      if(ptPr>1.5 && ptPr<6)
	pm[iCharge].h3PhiPrDcaXYGlVertexZ->Fill(phiPrDeg,dcaXYGl,vertexZ);
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
StHiStuff::findSector(Float_t phi, Char_t ew)
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

ClassImp(StHiStuff)
