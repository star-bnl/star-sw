/***************************************************************************
 *
 * $Id: StHiAnalysis.h,v 1.1 2002/04/02 20:05:18 jklay Exp $                                    
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
 * $Log: StHiAnalysis.h,v $
 * Revision 1.1  2002/04/02 20:05:18  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 **************************************************************************/
#ifndef StHiAnalysis_H
#define StHiAnalysis_H

#include "StHiBaseAnalysis.h"

class StHiAnalysis : public StHiBaseAnalysis{
 public:
  StHiAnalysis(const char* inputDir="./", 
	       const char* outRootName="hianalysis.hist.root");
  virtual ~StHiAnalysis();

 private:

  void initHistograms();
  void fillEventHistograms();
  void finishHistograms();

  Bool_t acceptEvent(StHiMicroEvent*);
  
  void trackLoop();
  
  //***************************************************************
  // histograms
  //

  // event histograms
  TH1D* h1Centrality; //! just look at centrality stuff
  TH1D* h1CentralityCut; //!
  TH3D* h3VtxXYZ; //!
  TH1D* h1VtxZCentCut; //!

  // cent
  TH3D* h3ZdcHMinusVtxZ; //!
  TH3D* h3ZdcHMinusCtbVtxZCut; //

  // 0 is plus, 1 is minus
  struct PlusMinus{
    TH3D* h3PhiPrDcaXYGlPtPr; // look at phi counts
    TH3D* h3PhiGlDcaXYGlPtGl;

    TH3D* h3DcaGlDcaXYGlPtPr; // look at dca's for different pt bins
    TH3D* h3DcaGlDcaXYGlPtGl;

    // --backgrounds
    TH2D* h2SDcaGlPtPrRebin; 
    TH2D* h2DcaXYGlPtPrRebin;
    TH2D* h2DcaGlPtPrRebin;
    // --

    TH3D* h3PhiPrFitPtsPtPr; // phi,fit pts,pt
    TH3D* h3VtxZFitPtsPtPr; // vtx z, fit pts, pt
    TH3D* h3VtxZFitPtsEtaPr; // vtx z, fit pts, eta

    TH3D* h3PhiPrAllPtsPtPr; // phi, all pts, pt

    TH3D* h3FlowCentFitPtsPtPr; // flow cent, fit pts, pt

    TH2D* h2ZDCCentralityPtPr; // pt yields and centrality
    TH2D* h2CentralityPtPr;

    TH3D* h3VtxZEtaPrPtPr; // reality check of the eta vs vertex
    TH3D* h3VtxZEtaGlPtGl;

    TH1D* h1RawVarBin; //
  };
  PlusMinus pm[2]; //!
  
  TH3D* h3ResPtPrGlPtPrDcaXYGl; //!
  TH3D* h3ResPtPrGlPtGlDcaXYGl; //!

  //  struct EastWest{
  //   PlusMinus pm[2];
  //};
  //xEastWest ew[2]; //! east is 0.
  

  ClassDef(StHiAnalysis,1)
};

#endif
