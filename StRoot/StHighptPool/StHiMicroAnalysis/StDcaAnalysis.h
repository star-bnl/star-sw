/***************************************************************************
 *
 * $Id: StDcaAnalysis.h,v 1.1 2002/04/02 20:05:18 jklay Exp $                                    
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
 * $Log: StDcaAnalysis.h,v $
 * Revision 1.1  2002/04/02 20:05:18  jklay
 * Bums analysis tools for highpt uDSTs
 *
 * 
 **************************************************************************/
#ifndef StDcaAnalysis_H
#define StDcaAnalysis_H


#include "StHiBaseAnalysis.h"

class StDcaAnalysis : public StHiBaseAnalysis{
 public:
  StDcaAnalysis(const char* inputDir="./", 
	       const char* outRootName="hianalysis.hist.root");
  virtual ~StDcaAnalysis();

 protected:

  void initHistograms();

  Bool_t acceptEvent(StHiMicroEvent*);
  
  void trackLoop();

  Int_t findSector(Float_t phi, Char_t ew);

  //###### histograms
  
  struct Dca{
    // global pt
    TH3D* hPosGlPtZFD;
    TH3D* hNegGlPtZFD;
    TH3D* hPosPlusNegGlPtZFD;
    TH3D* hPosMinusNegGlPtZFD;
    // primary pt
    TH3D* hPosPrPtZFD;
    TH3D* hNegPrPtZFD;
    TH3D* hPosPlusNegPrPtZFD;
    TH3D* hPosMinusNegPrPtZFD;
  };
  Dca dca[3]; //!


  ClassDef(StDcaAnalysis,1)
};

#endif
