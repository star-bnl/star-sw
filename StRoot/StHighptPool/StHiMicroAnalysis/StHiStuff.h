/***************************************************************************
 *
 * $Id: StHiStuff.h,v 1.1 2002/04/02 20:05:19 jklay Exp $                                    
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

  Int_t findSector(Float_t phi, Char_t ew);

  static const Float_t mMinPt = 2.5;

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

  PlusMinus pm[2]; //! 0 is positive
  

  ClassDef(StHiStuff,1)
};

#endif
