/***************************************************************************
 *
 * $Id: StHiStuff.h,v 1.3 2004/01/26 23:00:11 perev Exp $                                    
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  Class for making highpt Analysis histograms from highpt
 *               uDST's
 *
 ***************************************************************************
 * 
 * $Log: StHiStuff.h,v $
 * Revision 1.3  2004/01/26 23:00:11  perev
 * Add init of statick member
 *
 * Revision 1.2  2002/05/31 21:58:30  jklay
 * Updated analysis code to use new cut class
 *
 * Revision 1.1  2002/04/02 20:05:19  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 **************************************************************************/
#ifndef StHiStuff_H
#define StHiStuff_H


#include "StHiBaseAnalysis.h"

class StHiStuff : public StHiBaseAnalysis{
 public:
  StHiStuff(const char* inputDir="./", 
	       const char* outRootName="hianalysis.hist.root");
  virtual ~StHiStuff();

 protected:

  void initHistograms();
  Int_t initMore();
  void fillEventHistograms();
  void finishHistograms();

  Bool_t acceptEvent(StHiMicroEvent*);
  
  void trackLoop();

  Int_t findSector(Float_t phi, Int_t firstSector);

  static Float_t mMinPt;

  //###### histograms

  struct PlusMinus{

    TH3D* h3PhiGlDcaXYGlVertexZ;
    TH3D* h3PhiPrDcaXYGlVertexZ;

    // reality check
    
    TH1D* h1PhiGlReality;

    TH3D* h3PhiPrDcaXYGlPtPr;
    TH3D* h3PhiGlDcaXYGlPtGl;
    
    TH3D* h3SectorDcaXYGlPtPr;
    TH3D* h3SectorDcaXYGlPtGl;

    TH3D* h3SectorVertexZPtPr;
    TH3D* h3SectorVertexZPtGl;

    TH3D* h3PhiPrVertexZPtPr; // integrate over pt
    TH3D* h3PhiGlVertexZPtGl;

    TH3D* h3PhiPrEtaPrMidZ;
    TH3D* h3PhiGlEtaPrMidZ;

    TH1D* h1PhiPrHighPtCut;
    TH1D* h1PhiPrLowPtCut;

    // look at fit hits
    TH3D* h3PhiPrNFitHitPtPr;
    TH3D* h3PhiPrNAllHitPtPr;
    TH3D* h3PhiPrFracHitPtPr;
    
    TH3D* h3PhiPrSmallNFitHitPtPr;
    TH3D* h3PhiPrSmallNAllHitPtPr;
    TH3D* h3PhiPrSmallFracHitPtPr;
    
    TH3D* h3PhiGlNFitHitPtPr;
    TH3D* h3PhiGlNAllHitPtPr;
    TH3D* h3PhiGlFracHitPtPr;

    //*** analysis cut
    
    TH3D* h3VtxZEtaPrPtPrCut;
    TH3D* h3VtxZEtaGlPtGlCut;

    TH3D* h3ResPtDcaXYGlPtGlEastCut;
    TH3D* h3ResPtDcaXYGlPtGlWestCut;

    TH3D* h3ResPtDcaXYGlPtPrEastCut;
    TH3D* h3ResPtDcaXYGlPtPrWestCut;

    TH3D* h3PhiGlPtPrPtGlEastCut;
    TH3D* h3PhiGlPtPrPtGlWestCut;

    TH2D* h2VtxZLastZ; 

    
  };

  struct EastWest {
    PlusMinus pm[3]; //! 0 is positive, 1 is negative, 2 is all charged
  };

  EastWest ew[3];  //0 is East, 1 is West, 2 is FullTPC

  ClassDef(StHiStuff,1)
};

#endif
