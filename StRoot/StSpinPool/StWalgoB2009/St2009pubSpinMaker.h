// $Id: St2009pubSpinMaker.h,v 1.2 2010/01/27 22:12:25 balewski Exp $
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

  float par_QPTplus,par_QPTminus; // cuts to drop not sure charges

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

  void attachWalgoMaker(St2009WMaker *mk) { wMK=mk;}
  void attachSpinDb(StSpinDbMaker *mk){ spinDb=mk;}
  virtual Int_t FinishRun(int runumber);


  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2009pubSpinMaker.h,v 1.2 2010/01/27 22:12:25 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(St2009pubSpinMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2009pubSpinMaker.h,v $
// Revision 1.2  2010/01/27 22:12:25  balewski
// spin code matched to x-section code
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
