/***************************************************************************
 *
 * $Id: StBadDcaAnalysis.h,v 1.1 2002/04/02 20:05:18 jklay Exp $
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
 * $Log: StBadDcaAnalysis.h,v $
 * Revision 1.1  2002/04/02 20:05:18  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 **************************************************************************/
#ifndef StBadDcaAnalysis_H
#define StBadDcaAnalysis_H


#include "StHiBaseAnalysis.h"

class StBadDcaAnalysis : public StHiBaseAnalysis{
 public:
  StBadDcaAnalysis(const char* inputDir="./", 
	      const char* outRootName="hianalysis.hist.root");
  virtual ~StBadDcaAnalysis();

 protected:

  Int_t initMore();
  void initHistograms();
  void fillEventHistograms();
  void finishHistograms();

  void trackLoop();

  

  Bool_t acceptEvent(StHiMicroEvent*);

  
  //***************************************************************
  // histograms
  // 

  struct PlusMinus{
    // dcaxy, fit pts, pt
    TH3D* h3DcaXYGlFitPtsPtPr;
    
    // dcaxy, all pts, pt
    TH3D* h3DcaXYGlAllPtsPtPr;

    // dcaxy, ratio pts, pt
    TH3D* h3DcaXYGlRatioPtsPtPr;

    // dcaxy, first hit row, pt
    TH3D* h3DcaXYGlFirstRowPtPr;

    // dcaxy, last hit row, pt 
    TH3D* h3DcaXYGlLastRowPtPr;

    // dcaxy, local phi, pt
    TH3D* h3DcaXYGlLocalPhiPtPr;

  };

  PlusMinus pm[2];//!
  
  ClassDef(StBadDcaAnalysis,1)
  
};
#endif
