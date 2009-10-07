// $Id: St_pp2pp_Maker.h,v 1.1 2009/10/07 19:59:30 yipkin Exp $

#ifndef STAR_St_pp2pp_Maker
#define STAR_St_pp2pp_Maker

/*!
 *                                                                     
 * \class  St_pp2pp_Maker
 * \author Kin Yip
 * \date   2009/07/15
 * \brief  For pp2pp analysis
 *
 *
 *
 */                                                                      


#include "StRTSBaseMaker.h"
#include "pp2ppHit_Cluster.h"

class TGenericTable;
class pp2pp_t;

class St_pp2pp_Maker : public StRTSBaseMaker {

 public:
  enum {ErrorCode = -999,
	MAXSEC = 2 ,  // 2 sides
	MAXCHAIN = 4 ,
	MAXSVX = 6 ,
	MAXSEQ = 8 ,
	MAXSTRIP = 128 } ;

 private:
  typedef pair<Int_t, Double_t> HitChannel ; // first -> Position ; second -> Energy

  vector<HitChannel>  validhits[MAXSEQ][MAXCHAIN] ;
  static Bool_t hitcompare (HitChannel A,HitChannel B) { return (A.first<B.first); }


  Double_t  pedave[MAXSEQ][MAXCHAIN][MAXSVX][MAXSTRIP] ;
  Double_t  pedrms[MAXSEQ][MAXCHAIN][MAXSVX][MAXSTRIP] ;
  //  Double_t  *pedave[MAXSEQ][MAXCHAIN][MAXSVX] ;
  //  Double_t  *pedrms[MAXSEQ][MAXCHAIN][MAXSVX] ;

  Int_t fLast_svx;
  Int_t fLast_chain;
  Int_t fLast_seq;

  string pedestal_perchannel_filename ;
  Int_t read_pedestal_perchannel() ;
  //  Int_t nevt_count ;

  Bool_t LDoCluster; // to do clustering or not

 protected:
  // Protected method if any
 public: 

  St_pp2pp_Maker(const char *name="PP2PP") ;

  virtual       ~St_pp2pp_Maker();
  virtual void  Clear(Option_t *option="");
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t Finish();
  void SetPedestalFileName(const char* filename) { pedestal_perchannel_filename = filename ; }
  void DoClusterOrNot(Bool_t todo) { LDoCluster = todo ; }

  Int_t  DoerPp2pp(const pp2pp_t &d,  TGenericTable &hitsTable);
  Int_t  MakeClusters(TGenericTable &clustersTable);
  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St_pp2pp_Maker.h,v 1.1 2009/10/07 19:59:30 yipkin Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  /// obtain the whole list of leading edge hits
  // to obtain the published result use StMaker::GetDataSet("pp2ppRawHits");

  ClassDef(St_pp2pp_Maker,0)   //StAF chain virtual base class for Makers
};


#endif


