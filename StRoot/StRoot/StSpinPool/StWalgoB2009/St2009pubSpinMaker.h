// $Id: St2009pubSpinMaker.h,v 1.7 2014/08/06 11:43:41 jeromel Exp $
//
//*-- Author : Jan Balewski, MIT


#ifndef STAR_St2009pubSpinMaker
#define STAR_St2009pubSpinMaker

/*!
 *                                                                     
 * \class  St2009pubSpinMaker

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
class StSpinDbMaker;

class St2009pubSpinMaker : public StMaker {
 private:
  int nRun;
  int Tfirst,Tlast;

  float par_QPTlow,par_QPThighET0,par_QPThighET1, par_QPThighA ,par_QPThighB; // cuts to drop questionable reco charge charges
  float par_leptonEta1, par_leptonEta2; // narrow the range
  int par_useNoEEMC;

  St2009WMaker *wMK; // W-algo maker with all data
  StSpinDbMaker *spinDb;
  TString core; // name attached to all histos

  // histograms
  TObjArray *HList;
  enum {mxHA=32}; TH1 * hA[mxHA];
  TH1 *hbxIdeal;
  
  void initHistos();
  void bXingSort();  
 public: 
  St2009pubSpinMaker(const char *name="2009publWana");
  virtual       ~St2009pubSpinMaker(){};
  virtual Int_t  Init();
  virtual Int_t  InitRun  (int runumber);
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}
  void setEta(float x, float y) { par_leptonEta1=x; par_leptonEta2=y;};
  void setQPT(float x){  par_QPTlow=x;}
  void setNoEEMC() {par_useNoEEMC=1;}

  void attachWalgoMaker(St2009WMaker *mk) { wMK=mk;}
  void attachSpinDb(StSpinDbMaker *mk){ spinDb=mk;}
  virtual Int_t FinishRun(int runumber);


  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2009pubSpinMaker.h,v 1.7 2014/08/06 11:43:41 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(St2009pubSpinMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2009pubSpinMaker.h,v $
// Revision 1.7  2014/08/06 11:43:41  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.6  2010/04/14 20:00:08  balewski
// added AL w/o endcap
//
// Revision 1.5  2010/03/20 19:19:05  balewski
// added ability to drop Q/PT cut for spin analysis
//
// Revision 1.4  2010/03/14 22:50:31  balewski
// *** empty log message ***
//
// Revision 1.3  2010/01/28 20:10:05  balewski
// added eta dependent spin sorting
//
// Revision 1.2  2010/01/27 22:12:25  balewski
// spin code matched to x-section code
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
