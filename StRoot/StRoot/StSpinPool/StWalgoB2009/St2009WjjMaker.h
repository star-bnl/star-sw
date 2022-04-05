// $Id: St2009WjjMaker.h,v 1.3 2014/08/06 11:43:41 jeromel Exp $
//
//*-- Author : Jan Balewski, MIT


#ifndef STAR_St2009WjjMaker
#define STAR_St2009WjjMaker

/*!
 *                                                                     
 * \class  St2009WjjMaker

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
#include <TLorentzVector.h>

class St2009WMaker;
class StSpinDbMaker;

class St2009WjjMaker : public StMaker {
 private:
  int nRun;
  int Tfirst,Tlast;

  float par_jetPtLow,par_jetPtHigh;
  float par_djPtLow, par_djPtHigh , par_djEtaMin , par_djPzLow, par_djPzHigh ; 
  float par_jetEtaLow,par_jetEtaHigh ; 
  float par_etaSumLow, par_etaSumHigh;
  bool  par_spinSort;
  float par_vertexZ;
  int par_corLevel; // level of jet energy correction
  int   isMC;

  St2009WMaker *wMK; // W-algo maker with all data
  StSpinDbMaker *spinDb;
  TString core; // name attached to all histo
  TString  mJEScorrFile;
  enum {mxJESeta=8};
  TH1F *mJEScorrH[mxJESeta];

  // histograms
  TObjArray *HList;
  enum {mxHA=32}; TH1 * hA[mxHA];
  TH1F *hbxIdeal;
  
  void initHistos();
  void bXingSort();  
 public: 
  St2009WjjMaker(const char *name="2009Wjetjet");
  virtual       ~St2009WjjMaker(){};
  virtual Int_t  Init();
  virtual Int_t  InitRun  (int runumber);
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}
  void setSpinSort(bool x){ par_spinSort=x;}
  void setMC(int x){ isMC=x;}
  void setCorrection(char *name){ mJEScorrFile=name; par_corLevel=1;}

  void attachWalgoMaker(St2009WMaker *mk) { wMK=mk;}
  void attachSpinDb(StSpinDbMaker *mk){ spinDb=mk;}
  virtual Int_t FinishRun(int runumber);

  TLorentzVector  trueJet( TLorentzVector recoJet) ;
 
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2009WjjMaker.h,v 1.3 2014/08/06 11:43:41 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(St2009WjjMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2009WjjMaker.h,v $
// Revision 1.3  2014/08/06 11:43:41  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.2  2010/05/01 01:31:44  balewski
// added W->JJ code & JES calibration
//
// Revision 1.1  2010/04/16 01:04:43  balewski
// start
//
