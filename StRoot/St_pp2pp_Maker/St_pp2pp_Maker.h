// $Id: St_pp2pp_Maker.h,v 1.25 2015/10/22 18:51:08 yipkin Exp $

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
  double mskew_param[kMAXSEQ][2][4] ; // 4 parameters for each PMT and there are 2 PMT's for each of the 8 RP
  double mx_IP, my_IP, mz_IP; /* collision coordinates at the IP */
  double mtheta_x_tilt, mtheta_y_tilt ; /* tilt angles of the beam at collision */
  double mdistancefromDX_east, mdistancefromDX_west; /* distance from the IP to the DX magnet in the East and West */
  double mLDX_east, mLDX_west;     /* length of DX in the East and West */
  double mbendingAngle_east, mbendingAngle_west;     /* DX bending angles in the East and West */
  double mconversion_TAC_time ;	/* converting the TAC tick to time (second) */

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
    static const char cvs[]="Tag $Name:  $ $Id: St_pp2pp_Maker.h,v 1.25 2015/10/22 18:51:08 yipkin Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  // obtain the whole list of leading edge hits
  // to obtain the published result use StMaker::GetDataSet("pp2ppRawHits");


  //BEGIN  ------------------------ Rafal's code ------------------------  
  private:

  struct StRpsHit { // structure representing reconstructed coordinate (one) of track-point
    double positionXY;
    double positionZ;
    int clusterId[2];
    bool golden;
  };
    
  enum COORDINATES { X=0, Y=1 };

  const int nBranches = 4; 	// number of branches in RP system (EU, ED, WU and WD)

  // reconstruction parameters
  static const int maxClusterLength = 5;
  static const int maxNumberOfClusterPerPlane = 5;
  const double maxPitchesToMatch;
  
  static const double Emin[8][5];
  static const int Planes[2][2];
  static const int RpInBranch[4][2];
  double Pitch[2];

  // methods
  void formTracks( vector< StRpsTrack* > *,  const vector< StRpsTrackPoint* > * , const float, const float ) const;
  void formTrackPoints(const StRpsCollection &, vector< StRpsTrackPoint* > * ) const;
  vector<St_pp2pp_Maker::StRpsHit> formHits(const StRpsRomanPot *, const int) const;
  void preselectClusters(const StRpsRomanPot *, const int coordinate, vector<double>*, vector<int>*, vector<int>*, vector<int>*) const;
  Int_t classifyClustersCase(vector<double>*) const;
  Bool_t matchClusters(const int, const int, const vector<double>*, int*, double* = nullptr) const;
  Bool_t areMatched(const int, const double, const double, double* = nullptr) const;
  
  
  //END    ------------------------ Rafal's code ------------------------

  ClassDef(St_pp2pp_Maker,1)   //StAF chain virtual base class for Makers

};
#endif


