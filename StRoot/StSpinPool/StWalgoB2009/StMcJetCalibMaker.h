// $Id: StMcJetCalibMaker.h,v 1.2 2014/08/06 11:43:41 jeromel Exp $
//
//*-- Author : Jan Balewski, MIT
// calibrates jets event by event using partonic 2->W/Z-> process

#ifndef STAR_StMcJetCalibMaker
#define STAR_StMcJetCalibMaker

/*!
 *                                                                     
 * \class  StMcJetCalibMaker

 * \author Jan Balewski, MIT
 * \date   August 2009
 * \brief  spin sorting of W's 
 *
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <TString.h>

class St2009WMaker;
class St2009WjjMaker;

class StMcJetCalibMaker : public StMaker {
 private:

  float par_jetPtLow,par_jetPtHigh;
  float par_jetEtaLow,par_jetEtaHigh ; 
  float par_delRmax; // match gen & reco jets


  float par_vertexZ,  par_verZerr; //cm
  int   par_corLevel; // level of jet energy correction
  int   isMC;

  St2009WMaker *wMK; // W-algo maker with all data
  St2009WjjMaker * wjjMK;
  TString core; // name attached to all histos

  // histograms
  TObjArray *HList;
  enum {mxHA=64, mxEta=8}; TH1 * hA[mxHA];

  void initHistos();
  void calibrate();

 public: 
  StMcJetCalibMaker(const char *name="mcJetCalib");
  virtual       ~StMcJetCalibMaker(){};
  virtual Int_t  Init();
  virtual Int_t  InitRun  (int runumber);
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}
  void setCorrection(int x){ par_corLevel=x;}
  void attachWalgoMaker(St2009WMaker *mk) { wMK=mk;}
  void attachWjjMaker(St2009WjjMaker *mk) { wjjMK=mk;}


  virtual Int_t FinishRun(int runumber);


  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StMcJetCalibMaker.h,v 1.2 2014/08/06 11:43:41 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StMcJetCalibMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StMcJetCalibMaker.h,v $
// Revision 1.2  2014/08/06 11:43:41  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2010/05/01 01:31:45  balewski
// added W->JJ code & JES calibration
//
// Revision 1.1  2010/04/16 01:04:43  balewski
// start
//
