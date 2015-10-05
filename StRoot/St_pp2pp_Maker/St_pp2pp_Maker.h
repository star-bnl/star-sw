// $Id: St_pp2pp_Maker.h,v 1.19 2015/10/05 12:09:05 yipkin Exp $

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

class pp2pp_t;
class pp2ppOffset_st;
class pp2ppZ_st;
class pp2ppRPpositions_st;

class St_pp2pp_Maker : public StRTSBaseMaker {

 public:
  enum {ErrorCode = -9999,
	kMAXSEC = 2 ,   /// 2 sides
	kMAXCHAIN = 4 , /// 4 chains/planes
	kMAXSVX = 6 ,   
	kMAXSEQ = 8 ,   /// 8 sequencers/roman pots
	kMAXSTRIP = 128 } ;

 private:
  StEvent *mEvent ; /// pointer *mEvent for fetching StEvent

  typedef pair<Int_t, Double_t> HitChannel ; /// HitChannel : a pair in which first -> Position ; second -> Energy

  vector<HitChannel>  mValidHits[kMAXSEQ][kMAXCHAIN] ; /// mValidHits[][] array to store hits which are above thresholds
  static Bool_t hitcompare (HitChannel A,HitChannel B) { return (A.first<B.first); }

  Double_t  mPedave[kMAXSEQ][kMAXCHAIN][kMAXSVX][kMAXSTRIP] ;
  Double_t  mPedrms[kMAXSEQ][kMAXCHAIN][kMAXSVX][kMAXSTRIP] ;

  unsigned char mRpStatus[kMAXSEQ] ;

  Int_t mLastSvx;
  Int_t mLastChain;
  Int_t mLastSeq;

  string mPedestalPerchannelFilename ; /// filename to read in pedestal_per_channel
  Int_t readPedestalPerchannel() ;
  Int_t readOffsetPerplane() ;
  Int_t readZPerplane() ;
  pp2ppOffset_st *mOffsetTable ;
  pp2ppZ_st *mZTable ;
  pp2ppRPpositions_st *mRPpositionsTable ;

  UChar_t mSiliconBunch ;

  //  Int_t nevt_count ;

  Bool_t mLDoCluster; // to do clustering or not

  Int_t mVersion ; // K. Yip (2015-2-22) : to deal with different (years of) data

 public: 

  St_pp2pp_Maker(const char *name="PP2PP") ;

  virtual       ~St_pp2pp_Maker();
  virtual void  Clear(Option_t *option="");
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

  
  /*!
    Set the filename for the pedestal file if you don't want to use the default filename.
    Default : "pedestal.in.perchannel"
   */
  void SetPedestalFileName(const char* filename) { mPedestalPerchannelFilename = filename ; }

  /*!
    Set the flag not to do clustering 
    Default : "kTRUE"
   */
  void DoClusterOrNot(Bool_t todo) { mLDoCluster = todo ; }

  /*!
    DoerPp2pp(const pp2pp_t &d,  TGenericTable &hitsTable) read all channels from each SVX and make valid hits
    and put into the vector of "mValidHits".

  */
  Int_t  DoerPp2pp(const pp2pp_t &d,  TGenericTable &hitsTable);

  /*!
    MakeClusters() actually makes the clusters and store into StRpsCollection
  */
  Int_t  MakeClusters();

  /*!
    MakeTracks() actually makes the tracks and store into StRpsCollection
  */
  Int_t  MakeTracks(StRpsCollection &RpsColl, float blue_beamenergy, float yellow_beamenergy);

  virtual Int_t InitRun  (int runumber); /// Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St_pp2pp_Maker.h,v 1.19 2015/10/05 12:09:05 yipkin Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  // obtain the whole list of leading edge hits
  // to obtain the published result use StMaker::GetDataSet("pp2ppRawHits");

  ClassDef(St_pp2pp_Maker,0)   //StAF chain virtual base class for Makers

};


#endif


