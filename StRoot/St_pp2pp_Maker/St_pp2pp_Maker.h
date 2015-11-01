// $Id: St_pp2pp_Maker.h,v 1.27 2015/11/01 22:58:51 yipkin Exp $

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
class StRpsRomanPot; // added by Rafal
class StRpsTrack; // added by Rafal
class StRpsTrackPoint; // added by Rafal

class pp2pp_t;
class pp2ppOffset_st;
class pp2ppZ_st;
class pp2ppRPpositions_st;
class pp2ppAcceleratorParameters_st;
class pp2ppPMTSkewConstants_st;

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
  Double_t mLVDT_pos[kMAXSEQ] ;

  UChar_t mSiliconBunch ;

  // K. Yip (2015-10-22) : Adding variables for Accelerator and Skew parameters in the respective database ;
  double mSkew_param[kMAXSEQ][2][4] ; // 4 parameters for each PMT and there are 2 PMT's for each of the 8 RP
  double mXYZ_IP[3]; /* collision coordinates at the IP; 0=X, 1=Y, 2=Z */
  double mThetaXY_tilt[2]; /* tilt angles of the beam at collision; 0=X, 1=Y */
  double mDistanceFromIPtoDX[2]; /* distance from the IP to the DX magnet in the East and West; 0=E, 1=W */
  double mLDX[2];     /* length of DX in the East and West; 0=E, 1=W */
  double mBendingAngle[2];     /* DX bending angles in the East and West; 0=E, 1=W */
  double mConversion_TAC_time ;	/* converting the TAC tick to time (second) */

  // K. Yip (2015-10-22) : Adding methods to read Accelerator and Skew parameters from the respective database ;
  Int_t readAccelerateParameter() ;
  Int_t readSkewParameter() ;

  //  Int_t nevt_count ;

  Bool_t mLDoCluster; // to do clustering or not

  Int_t mVersion ; // K. Yip (2015-2-22) : to deal with different (years of) data

  // K. Yip (2015-10-15) : Make constants for the class so that MakeTrack can use, too.
  //                       All pitches in meter 
  // For A & C planes
  const double kpitch_4svx = 9.74E-5; //! 
  // For E2D.A >= 2015
  const double kpitch_4svx2 = 9.55E-5; //!
  // For B & D planes
  const double kpitch_6svx = 1.050E-4; //! 

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
    static const char cvs[]="Tag $Name:  $ $Id: St_pp2pp_Maker.h,v 1.27 2015/11/01 22:58:51 yipkin Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  // obtain the whole list of leading edge hits
  // to obtain the published result use StMaker::GetDataSet("pp2ppRawHits");


  //BEGIN  ------------------------ Rafal's code ------------------------  
  private:

  enum RP_STATIONS_PER_BRANCH { kRP1, kRP2, kStationsPerBranch };
  enum SILICON_PLANES_PER_COORDINATE { kFirst, kSecond };
  enum COORDINATES { kX, kY, kCoordinates, kZ = kCoordinates };

  // additional constants describing the Roman Pot Phase II* system
  static const int kBranches = 4; // number of branches in RP system (EU, ED, WU and WD)
  static const int kPlanesPerCoordinate = kMAXCHAIN/2; // number of Si planes for one spatial coordinate (assumes kMAXCHAIN is even!)

  // reconstruction parameters
  static const int kMaxClusterLength = 5;
  const int kMaxNumberOfClusterPerPlane = 5;
  const double kMaxPitchesToMatch;
  static const double kEmin[kMAXSEQ][kMaxClusterLength];
  const double kMaxPedestalTAC = 100;

  // handy arrays
  static const int kPlanes[kCoordinates][kPlanesPerCoordinate];
  static const int kRpInBranch[kBranches][kStationsPerBranch];
  const double kPitch[kCoordinates];

  // structures
  struct StRpsHit { // structure representing reconstructed coordinate (one) of track-point
    double mPositionXY;
    double mPositionZ;
    int mClusterId[kPlanesPerCoordinate];
    int mPlanesUsed;
    bool mGolden;
  };
  
  // methods
  void formTracks( vector< StRpsTrack* > *,  const vector< StRpsTrackPoint* > * , const float, const float ) const;
  void formTrackPoints(const StRpsCollection &, vector< StRpsTrackPoint* > * ) const;
  vector<St_pp2pp_Maker::StRpsHit> formHits(const StRpsRomanPot *, const int) const;
  void preselectClusters(const StRpsRomanPot *, const int coordinate, vector<double>*, vector<int>*, vector<int>*, vector<int>*) const;
  Int_t classifyClustersCase(vector<double>*) const;
  Bool_t matchClusters(const int, const int, const vector<double>*, std::vector<int>*) const;
  Bool_t areMatched(const int, const double, const double, double* = nullptr) const;
  Double_t timeFromTAC(const int, const int, const int, const int) const;
  
  //END    ------------------------ Rafal's code ------------------------

  ClassDef(St_pp2pp_Maker,1)   //StAF chain virtual base class for Makers

};

//BEGIN  ------------------------ Rafal's code ------------------------  
inline Double_t St_pp2pp_Maker::timeFromTAC(const int Rp, const int pmt, const int Tac, const int Adc) const{
  return mConversion_TAC_time * (Tac + mSkew_param[Rp][pmt][0]
  + mSkew_param[Rp][pmt][1]*exp(-mSkew_param[Rp][pmt][2]*(Adc-mSkew_param[Rp][pmt][3])) / (Adc-mSkew_param[Rp][pmt][3]) );
}
//END    ------------------------ Rafal's code ------------------------

#endif


