// $Id: St2009WlumiMaker.h,v 1.1 2009/11/23 23:00:18 balewski Exp $
//
//*-- Author : Ross Corliss, MIT


#ifndef STAR_St2009WlumiMaker
#define STAR_St2009WlumiMaker

/*!
 *                                                                     
 * \class  St2009WlumiMaker
 * \author Jan Balewski, MIT
 * \date   August 2009
 * \brief  gathers all results from  W-analysis, Jan's analysis
 *
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
class StMuDstMaker;
class St2009WMaker;

class St2009WlumiMaker : public StMaker {
 private:
  // variables
  int nActiveTowers; // number of towers in the run that have good status
  bool towerInfoIsCurrent;//whether we've computed the active fraction for this run
  int nBHT3; //number of L2W random accepts, hence the number of BHT3 triggers in general (prescaled)
  int nBx[120];//number of randoms, broken up by bxing.
  // parameters
  float  par_highET; // cut off for W 2x2 cluster ET

  St2009WMaker *wMK; // W-algo maker with all data
  StMuDstMaker *muMK;
  // histograms
  TObjArray *HList;
  enum {mxHA=32}; TH1 * hA[mxHA];
  
  void initHistos();
  void sortTrigger();
  void getActiveTowers();
  void getAbortGapCounts(int* n1,int* n2);
  
 public: 
  St2009WlumiMaker(const char *name="2009publWana");
  virtual       ~St2009WlumiMaker(){};
  virtual Int_t  Init();
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}

  void attachWalgoMaker(St2009WMaker *mk) { wMK=mk;}
  void attachMuMaker(StMuDstMaker *mk) { muMK=mk;}

  virtual Int_t InitRun(int runumber); // Overload empty StMaker::InitRun 
  virtual Int_t FinishRun(int runumber); // Overload empty StMaker::FinishRun 


  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2009WlumiMaker.h,v 1.1 2009/11/23 23:00:18 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(St2009WlumiMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2009WlumiMaker.h,v $
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
