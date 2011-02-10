// $Id: St2011WlumiMaker.h,v 1.1 2011/02/10 20:33:24 balewski Exp $
//
//*-- Author : Ross Corliss, MIT


#ifndef STAR_St2011WlumiMaker
#define STAR_St2011WlumiMaker

/*!
 *                                                                     
 * \class  St2011WlumiMaker
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
class St2011WMaker;

class St2011WlumiMaker : public StMaker {
 private:
  // variables
  int nActiveTowers; // number of towers in the run that have good status
  bool towerInfoIsCurrent;//whether we've computed the active fraction for this run
  int nBHT3_hardware_L0; //number of L2W random accepts that pass the hardware L0 requirement.
  int nBHT3_software_L0; //number of L2W random accepts that pass the software-imposed L0 requirement, hence the number of BHT3 triggers in general (prescaled)
  int nBHT3[16]; //number of L2W random accepts, hence the number of BHT3 triggers in general (prescaled)
  int nBx[16][120];//number of randoms, broken up by bxing.
  // parameters
  float  par_highET; // cut off for W 2x2 cluster ET

  St2011WMaker *wMK; // W-algo maker with all data
  StMuDstMaker *muMK;
  // histograms
  TObjArray *HList;
  enum {mxHA=120}; TH1 * hA[mxHA];
  
  void initHistos();
  void sortTrigger();
  void getActiveTowers();
  void getAbortGapCounts(int angle, int* n1,int* n2);
  
 public: 
  St2011WlumiMaker(const char *name="2011Wlumi");
  virtual       ~St2011WlumiMaker(){};
  virtual Int_t  Init();
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}

  void attachWalgoMaker(St2011WMaker *mk) { wMK=mk;}
  void attachMuMaker(StMuDstMaker *mk) { muMK=mk;}

  virtual Int_t InitRun(int runumber); // Overload empty StMaker::InitRun 
  virtual Int_t FinishRun(int runumber); // Overload empty StMaker::FinishRun 


  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2011WlumiMaker.h,v 1.1 2011/02/10 20:33:24 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(St2011WlumiMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2011WlumiMaker.h,v $
// Revision 1.1  2011/02/10 20:33:24  balewski
// start
//
