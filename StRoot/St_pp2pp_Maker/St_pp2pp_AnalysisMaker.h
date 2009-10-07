// $Id: St_pp2pp_AnalysisMaker.h,v 1.1 2009/10/07 19:59:30 yipkin Exp $

#ifndef STAR_St_pp2pp_AnalysisMaker
#define STAR_St_pp2pp_AnalysisMaker

/*!
 *                                                                     
 * \class  St_pp2pp_AnalysisMaker
 * \author Kin Yip
 * \date   2009/07/15
 * \brief  For pp2pp analysis
 *
 *
 *
 */                                                                      

class TFile;
class TTree;

#include "StRTSBaseMaker.h"
//#include "pp2ppHit_Cluster.h"
#include "St_pp2pp_Maker.h"

class St_pp2pp_AnalysisMaker : public StRTSBaseMaker {

 private:

  enum { MAXClusters = 10 };


  // For making root-tree
  TFile     *fTreeFile ;
  TTree     *fClusterTree;

  struct P2P {
    unsigned short RPWVD2_ADC;
    unsigned short RPWVD1_ADC;
    unsigned short RPWVU2_ADC;
    unsigned short RPWVU1_ADC;
    unsigned short RPEVD2_ADC;
    unsigned short RPEVD1_ADC;
    unsigned short RPEVU2_ADC;
    unsigned short RPEVU1_ADC;
    unsigned short RPWVD2_TAC;
    unsigned short RPWVD1_TAC;
    unsigned short RPWVU2_TAC;
    unsigned short RPWVU1_TAC;
    unsigned short RPEVD2_TAC;
    unsigned short RPEVD1_TAC;
    unsigned short RPEVU2_TAC;
    unsigned short RPEVU1_TAC;
    unsigned short RPWHI2_ADC;
    unsigned short RPWHI1_ADC;
    unsigned short RPWHO2_ADC;
    unsigned short RPWHO1_ADC;
    unsigned short RPEHI2_ADC;
    unsigned short RPEHI1_ADC;
    unsigned short RPEHO2_ADC;
    unsigned short RPEHO1_ADC;
    unsigned short RPWHI2_TAC;
    unsigned short RPWHI1_TAC;
    unsigned short RPWHO2_TAC;
    unsigned short RPWHO1_TAC;
    unsigned short RPEHI2_TAC;
    unsigned short RPEHI1_TAC;
    unsigned short RPEHO2_TAC;
    unsigned short RPEHO1_TAC;
  } P2P ;

  struct bunch_crossing {
    unsigned int xing_lo;
    unsigned int xing_hi;
    unsigned int bunch_number;
    unsigned int b120;
  } xing ;

  struct cluster {
    int nclusters ;
    unsigned char length[MAXClusters] ; // > 0 
    double position[MAXClusters] ;     // 0 - 755
    double energy[MAXClusters] ;       // in ADC
    double x[MAXClusters] ;
    double y[MAXClusters] ;
    double z[MAXClusters] ;
  } allclusters[St_pp2pp_Maker::MAXSEQ][St_pp2pp_Maker::MAXCHAIN] ;


  struct event_info {
    int run_number ;
    int event_number ;
    int seq ; // non-unique the same as what used to be called "seq" in evpreader
    int daqbits ;
    int token ;
  } event_info ;

  unsigned short tcubits ;
  unsigned char silicon_bunch;

  // --- end of root-tree stuff

  Int_t nevt_count ;

 protected:
  // Protected method if any
 public: 
  St_pp2pp_AnalysisMaker(const char *name="PP2PPANALYSIS") ;

  virtual       ~St_pp2pp_AnalysisMaker();
  virtual void  Clear(Option_t *option="");
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t Finish();

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St_pp2pp_AnalysisMaker.h,v 1.1 2009/10/07 19:59:30 yipkin Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  ClassDef(St_pp2pp_AnalysisMaker,0)   //StAF chain virtual base class for Makers
};


#endif


