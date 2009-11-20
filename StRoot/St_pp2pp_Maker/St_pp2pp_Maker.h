// $Id: St_pp2pp_Maker.h,v 1.8 2009/11/20 19:54:17 yipkin Exp $

#ifndef STAR_St_pp2pp_Maker
#define STAR_St_pp2pp_Maker

/*!
 *                                                                     
 * \class  St_pp2pp_Maker
 * \author Kin Yip
 * \date   2009/11/15
 * \brief  For pp2pp analysis : mainly creating clusters from raw data silicon hits
 *
 *
 *
 */                                                                      


#include "StRTSBaseMaker.h"
#include "pp2ppHit_Cluster.h"

class TGenericTable;
class StEvent;
class StRpsCollection;

class pp2ppOffset_st;
class pp2pp_t;

class St_pp2pp_Maker : public StRTSBaseMaker {

 public:
  enum {ErrorCode = -9999,
	MAXSEC = 2 ,   /// 2 sides
	MAXCHAIN = 4 , /// 4 chains/planes
	MAXSVX = 6 ,   
	MAXSEQ = 8 ,   /// 8 sequencers/roman pots
	MAXSTRIP = 128 } ;

 private:
  StEvent *mEvent ; /// for fetching StEvent

  typedef pair<Int_t, Double_t> HitChannel ; /// first -> Position ; second -> Energy

  vector<HitChannel>  mvalidhits[MAXSEQ][MAXCHAIN] ;
  static Bool_t hitcompare (HitChannel A,HitChannel B) { return (A.first<B.first); }


  Double_t  mpedave[MAXSEQ][MAXCHAIN][MAXSVX][MAXSTRIP] ;
  Double_t  mpedrms[MAXSEQ][MAXCHAIN][MAXSVX][MAXSTRIP] ;

  Int_t mLast_svx;
  Int_t mLast_chain;
  Int_t mLast_seq;

  string mpedestal_perchannel_filename ; /// filename to read in pedestal_per_channel
  Int_t read_pedestal_perchannel() ;
  Int_t read_offset_perplane() ;
  pp2ppOffset_st *moffset_table ;

  //  Int_t nevt_count ;

  Bool_t mLDoCluster; // to do clustering or not

 public: 

  St_pp2pp_Maker(const char *name="PP2PP") ;

  virtual       ~St_pp2pp_Maker();
  virtual void  Clear(Option_t *option="");
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t Finish();

  
  /*!
    Set the filename for the pedestal file if you don't want to use the default filename.
    Default : "pedestal.in.perchannel"
   */
  void SetPedestalFileName(const char* filename) { mpedestal_perchannel_filename = filename ; }

  /*!
    Set the flag not to do clustering 
    Default : "kTRUE"
   */
  void DoClusterOrNot(Bool_t todo) { mLDoCluster = todo ; }

  /*!
    DoerPp2pp(const pp2pp_t &d,  TGenericTable &hitsTable) read all channels from each SVX and make valid hits
    and put into the vector of "mvalidhits".

  */
  Int_t  DoerPp2pp(const pp2pp_t &d,  TGenericTable &hitsTable);

  /*!
    MakeClusters() actually makes the clusters and store into StRpsCollection
  */
  Int_t  MakeClusters();

  virtual Int_t InitRun  (int runumber); /// Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St_pp2pp_Maker.h,v 1.8 2009/11/20 19:54:17 yipkin Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  // obtain the whole list of leading edge hits
  // to obtain the published result use StMaker::GetDataSet("pp2ppRawHits");

  ClassDef(St_pp2pp_Maker,0)   //StAF chain virtual base class for Makers

};


#endif


