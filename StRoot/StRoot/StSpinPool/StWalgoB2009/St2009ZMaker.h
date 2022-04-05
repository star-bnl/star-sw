
// $Id: St2009ZMaker.h,v 1.7 2014/08/06 11:43:41 jeromel Exp $

#ifndef STAR_St2009ZMaker
#define STAR_St2009ZMaker

/*!
 *                                                                     
 * \class  St2009ZMaker
 * \author Ross
 * \date   December 2009
 * \brief  uses tree from W-algo to find Zs
 *
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
class StMuDstMaker;
class St2009WMaker;
class WeventDisplay;
class WeveEleTrack;

class St2009ZMaker : public StMaker {
  friend class St2009pubMcMaker;

 private:
  // parameters
  float par_clustFrac24Z; // ET ratio 2x2/4x4 cluster
  float par_delR3DZ; // cm, dist between projected track and center of cluster
  float par_nearTotEtFracZ;
  float par_clusterEtZ;
  float par_delPhi12;
  float par_minMassZ;
  float par_maxMassZ;

  St2009WMaker *wMK; // W-algo maker with all data
  StMuDstMaker *muMK;
  // histograms
  TObjArray *HList;
  enum {mxHA=64}; TH1 * hA[mxHA];
  
  void initHistos();
  void find_Z_boson(); 
  
 public: 
  St2009ZMaker(const char *name="2009publWana");
  virtual       ~St2009ZMaker(){};
  virtual Int_t  Init();
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;}

  void attachWalgoMaker(St2009WMaker *mk) { wMK=mk;}
  void attachMuMaker(StMuDstMaker *mk) { muMK=mk;}
  void setClusterFrac24(float x) {par_clustFrac24Z=x; return;}
  void setDeltaR(float x) {par_delR3DZ=x; return;}
  void setNearEtFrac(float x) {par_nearTotEtFracZ=x; return;}
  void setClusterMinEt(float x) {par_clusterEtZ=x; return;}
  void setPhi12Min(float x) {par_delPhi12=x; return;}
  void setMinZMass(float x) {par_minMassZ=x; return;}
  void setMaxZMass(float x) {par_maxMassZ=x; return;}
  void printJan(WeveEleTrack *T);
  float calcMass(WeveEleTrack T1, WeveEleTrack T2);
  virtual Int_t InitRun(int runnumber); // Overload empty StMaker::InitRun 
  virtual Int_t FinishRun(int runnumber); // Overload empty StMaker::FinishRun 


  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2009ZMaker.h,v 1.7 2014/08/06 11:43:41 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(St2009ZMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2009ZMaker.h,v $
// Revision 1.7  2014/08/06 11:43:41  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.6  2011/09/14 14:23:21  stevens4
// update used for cross section PRD paper
//
// Revision 1.5  2010/01/06 04:22:15  balewski
// added Q/PT plot for Zs, more cleanup
//
// Revision 1.4  2010/01/05 03:22:55  balewski
// change logic for filling btow status tables, added printout to Z-code
//
// Revision 1.3  2010/01/04 05:12:00  balewski
// added 4x4 cut to Z-algo, cleanup
//
// Revision 1.2  2010/01/03 04:38:24  balewski
// reorganized Z-algo
//
// Revision 1.1  2009/12/07 20:37:56  rcorliss
// Start
//
