/***************************************************************************
 *
 * $Id: StBadDcaAnalysis.cxx,v 1.1 2002/04/02 20:05:18 jklay Exp $
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
 * $Log: StBadDcaAnalysis.cxx,v $
 * Revision 1.1  2002/04/02 20:05:18  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 **************************************************************************/
#include "StBadDcaAnalysis.h"


//__________________

StBadDcaAnalysis::StBadDcaAnalysis(const char* inputDir,
			 const char* outRootName)
  : StHiBaseAnalysis(inputDir,outRootName)
     
{

}

//__________________

StBadDcaAnalysis::~StBadDcaAnalysis()
{
}

//__________________
//
// reads in all the files in the directory
// by default, looks for files ending in minimc.root 
//

Int_t
StBadDcaAnalysis::initMore()
{
  return 0;
}


void
StBadDcaAnalysis::initHistograms()
{
  cout << "StBadDcaAnalysis::initHistograms()" << endl;

  //*********************

  using namespace Bin;

  //*********************

  char title[500],name[500];

  char* cPM[2] = {"Plus","Minus"};

  for(int ipm=0; ipm<2; ipm++){
    
    // dca xy, fit pts, pt
    setName(name,"h3DcaXYGlFitPtsPtPr",cPM[ipm]);
    pm[ipm].h3DcaXYGlFitPtsPtPr 
      = new TH3D(name,name,
		 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
		 nFitPtsBin,fitPtsMin,fitPtsMax,
		 nPtTinyBin,ptTinyMin,ptTinyMax);
    
    pm[ipm].h3DcaXYGlFitPtsPtPr->SetXTitle("dcaXYGl");
    pm[ipm].h3DcaXYGlFitPtsPtPr->SetYTitle("fitPts");
    pm[ipm].h3DcaXYGlFitPtsPtPr->SetZTitle("ptPr");
    
    // dcaxy, all pts, pt
    setName(name,"h3DcaXYGlAllPtsPtPr",cPM[ipm]);
    pm[ipm].h3DcaXYGlAllPtsPtPr 
      = new TH3D(name,name,
		 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
		 nFitPtsBin,fitPtsMin,fitPtsMax,
		 nPtTinyBin,ptTinyMin,ptTinyMax);
    
    pm[ipm].h3DcaXYGlAllPtsPtPr->SetXTitle("dcaXYGl");
    pm[ipm].h3DcaXYGlAllPtsPtPr->SetYTitle("allPts");
    pm[ipm].h3DcaXYGlAllPtsPtPr->SetZTitle("ptPr");
    
    // dcaxy, points ratio, pt
    setName(name,"h3DcaXYGlRatioPtsPtPr",cPM[ipm]);
    pm[ipm].h3DcaXYGlRatioPtsPtPr 
      = new TH3D(name,name,
		 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
		 100,0,1,
		 nPtTinyBin,ptTinyMin,ptTinyMax);
    
    pm[ipm].h3DcaXYGlRatioPtsPtPr->SetXTitle("dcaXYGl");
    pm[ipm].h3DcaXYGlRatioPtsPtPr->SetYTitle("ratioPts");
    pm[ipm].h3DcaXYGlRatioPtsPtPr->SetZTitle("ptPr");
    
    // dca xy, first hit, pt
    setName(name,"h3DcaXYGlFirstRowPtPr",cPM[ipm]);
    pm[ipm].h3DcaXYGlFirstRowPtPr 
	= new TH3D(name,name,
		   nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
		   45,0.5,45.5,
		   nPtTinyBin,ptTinyMin,ptTinyMax);
    
    pm[ipm].h3DcaXYGlFirstRowPtPr->SetXTitle("dcaXYGl");
    pm[ipm].h3DcaXYGlFirstRowPtPr->SetYTitle("firstRow");
    pm[ipm].h3DcaXYGlFirstRowPtPr->SetZTitle("ptPr");
    
    // dca xy, last hit, pt
    setName(name,"h3DcaXYGlLastRowPtPr",cPM[ipm]);
    pm[ipm].h3DcaXYGlLastRowPtPr 
      = new TH3D(name,name,
		 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
		 45,0.5,45.5,
		 nPtTinyBin,ptTinyMin,ptTinyMax);
    
    pm[ipm].h3DcaXYGlLastRowPtPr->SetXTitle("dcaXYGl");
    pm[ipm].h3DcaXYGlLastRowPtPr->SetYTitle("firstRow");
    pm[ipm].h3DcaXYGlLastRowPtPr->SetZTitle("ptPr");
    
    // dca xy, local phi, pt
    setName(name,"h3DcaXYGlLocalPhiPtPr",cPM[ipm]);
    pm[ipm].h3DcaXYGlLocalPhiPtPr 
      = new TH3D(name,name,
		 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
		 30,-15,15,
		 nPtTinyBin,ptTinyMin,ptTinyMax);
    
    pm[ipm].h3DcaXYGlLocalPhiPtPr->SetXTitle("dcaXYGl");
    pm[ipm].h3DcaXYGlLocalPhiPtPr->SetYTitle("localPhi");
    pm[ipm].h3DcaXYGlLocalPhiPtPr->SetZTitle("ptPr");
    
    
  }


}

//_____________________

void 
StBadDcaAnalysis::trackLoop()
{
  if(mDebug)
    cout << "StBadDcaAnalysis::spectraTrackLoop()" << endl;

  Int_t nTrack = mHiMicroEvent->mNTrack;
  StHiMicroTrack* track;
  
  Float_t vertexZ = mHiMicroEvent->mVertexZ;

  for(Int_t i=0; i<nTrack; i++){
    track =(StHiMicroTrack*) mHiMicroEvent->tracks()->At(i);

    //***************** spectra  ***********************************
    
    if(!CutRc::AcceptTrackHalf(track,vertexZ)) continue;
    
    //**************************************************************
    Float_t ptPr = track->mPtPr;
    Float_t eta = track->mEtaPr;
    Float_t ptGl = track->mPtGl;
    
     float fitPts = track->mFitPts;
    float allPts = track->mAllPts;
    float ratioPts = float(fitPts/allPts);
    float firstRow = track->mFirstPadrow;
    float lastRow = track->mLastPadrow;

    float dcaXYGl = track->mDcaXYGl;
    float phiGlDeg = track->mPhiGl*180./M_PI;;
    phiGlDeg = (phiGlDeg<-165) ? (phiGlDeg += 360) : phiGlDeg;

    int ic=(track->mCharge>0) ? 0 : 1;

    if(CutRc::AcceptEta(track)){
      // dcaxy, fit pts, pt
      pm[ic].h3DcaXYGlFitPtsPtPr->Fill(dcaXYGl,fitPts,ptPr);

      // dcaxy, all pts, pt
      pm[ic].h3DcaXYGlAllPtsPtPr->Fill(dcaXYGl,allPts,ptPr);

      // dcaxy, ratio pts, pt
      pm[ic].h3DcaXYGlRatioPtsPtPr->Fill(dcaXYGl,ratioPts,ptPr);

      if(CutRc::AcceptFitPts(track)){
	// dcaxy, first row, pt
	pm[ic].h3DcaXYGlFirstRowPtPr->Fill(dcaXYGl,firstRow,ptPr);
	
	// dcaxy, first row, pt
	pm[ic].h3DcaXYGlLastRowPtPr->Fill(dcaXYGl,lastRow,ptPr);


      }
    }
    

  }
}

//_____________________

void
StBadDcaAnalysis::fillEventHistograms()
{

}


//______________________

Bool_t
StBadDcaAnalysis::acceptEvent(StHiMicroEvent* event)
{
  return CutRc::Accept(event);

}


//______________________

void
StBadDcaAnalysis::finishHistograms()
{
  

}



//_____________________________________

ClassImp(StBadDcaAnalysis)
