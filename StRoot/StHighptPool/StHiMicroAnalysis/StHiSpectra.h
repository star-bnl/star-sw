/***************************************************************************
 *
 * $Id: StHiSpectra.h,v 1.2 2002/05/31 21:58:29 jklay Exp $                                    
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  Class for making highpt inclusive spectra from highpt
 *               uDST's
 *
 ***************************************************************************
 * 
 * $Log: StHiSpectra.h,v $
 * Revision 1.2  2002/05/31 21:58:29  jklay
 * Updated analysis code to use new cut class
 *
 * Revision 1.1  2002/04/02 20:05:18  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 **************************************************************************/
#ifndef StHiSpectra_H
#define StHiSpectra_H


#include "StHiBaseAnalysis.h"

class StHiSpectra : public StHiBaseAnalysis{
 public:
  StHiSpectra(const char* inputDir="./", 
	      const char* outRootName="hianalysis.hist.root",
	      const char* filename=0);
  virtual ~StHiSpectra();

  void setEfficiencyString(char* a){ mEfficiencyString = a;}
  void setEfficiencyFileName(char* a) { mEfficiencyFileName = a; }
  TString efficiencyFileName() const { return mEfficiencyFileName; }

 protected:
  static const int mNVarBin = 2; //!
  static const int mNWeight = 4; //!

  Int_t initMore();
  void initHistograms();
  void fillEventHistograms();
  void finishHistograms();

  void trackLoop();

  

  Bool_t acceptEvent(StHiMicroEvent*);

  TString            mEfficiencyFileName; //!
  TString            mEfficiencyString;//!
  TF2*               mEfficiencyMap; //!
  
  //***************************************************************
  // histograms
  //

  // event histograms
  TH1D*              h1Centrality; //! just look at centrality stuff
  TH1D*              h1CentralityCut; //!
  TH1D*              h1VertexZ; //!
//  TH1D*              h1VertexZCut; //!
  TH1D*              h1VertexZThin; //!
//  TH1D*              h1VertexRCut; //!

  TH2D*              h2Yield; //!

  TH1D*              h1NEvent; //! # of events for the centrality class
  TH1D*              h1EtaCut; //! just in case, store the eta cut
  
  struct Spectrum{
    TH1D*  h1Raw;
    TH1D*  h1EffCorrected;
    TH1D*  h1Corrected;

  };

  //
  // try different pt bins 
  //

  struct Mean{
    TH1D*              h1OneOverPt; 
    TH1D*              h1WeightedMean; // h1raw/h1OneOverPt
  };
  
  struct VarBin{
    Spectrum           specPM[2]; //!
    Spectrum           spec; //! plus + minus
    TH1D*              h1RawRatio;
    TH1D*              h1CorrectedRatio;

    Mean               meanPM[2]; //!
    Mean               mean; //!

    TArrayD*           ptBinsAry;
    Int_t              nPtBinAry;
  };
  

  VarBin varBin[mNVarBin]; //!

  ClassDef(StHiSpectra,1)
};

#endif
