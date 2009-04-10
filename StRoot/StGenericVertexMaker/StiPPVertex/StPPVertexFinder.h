/*!
 * \class StPPVertexFinder
 * \author Jan Balewski, July 2004
 *
 *  StGenericVertexFinder implementation of PPV
 * $Id: StPPVertexFinder.h,v 1.13 2009/04/10 22:57:14 genevb Exp $
 *
 */
#include "StGenericVertexMaker/StGenericVertexFinder.h"

class StiKalmanTrack;
class TrackData;
class VertexData;
class TGraphErrors;
class StEvent; 
class StiToolkit;
class StEEmcDb;

class EEmcGeomSimple;

class  CtbHitList;
class  BemcHitList;
class  EemcHitList;

class StPPVertexFinder: public StGenericVertexFinder {
 private:
  enum {mxH=32};
  bool examinTrackDca(const StiKalmanTrack*, TrackData &t);
  void matchTrack2CTB(const StiKalmanTrack*, TrackData &t);
  void matchTrack2EEMC(const StiKalmanTrack*, TrackData &t, float z);
  void matchTrack2BEMC(const StiKalmanTrack*, TrackData &t, float rxy);
  bool matchTrack2Membrane(const StiKalmanTrack*, TrackData &t);
  bool isPostCrossingTrack(const StiKalmanTrack* track);
  vector<TrackData>  mTrackData;
  vector<VertexData> mVertexData;
  bool buildLikelihood();
  bool findVertex(VertexData &);
  bool evalVertex(VertexData &);
  void exportVertices(); 

  void saveHisto(TString fname);
  int  mTotEve;
  int  eveID;
  uint  mAlgoSwitches; //binary, assign 1bit per change, use enum below
  enum {kSwitchOneHighPT=1}; 

  // params
  double mMinTrkPt;       //~ pT=0.16(GeV/c) == R=2 (m )in 2001
  double mMaxTrkDcaRxy;   //DCA to nominal beam line for each track
  float  mMaxZradius;     // used in matching: tracks to zVertex
  int    mMinMatchTr;     // for valid vertex
  float  mMaxZrange;      // cut off for tracks Z_DCA
  float  mMinAdcBemc;     // BEMC towers with MIP response
  float  mMinAdcEemc;     // EEMC towers with MIP response
  float  mMinFitPfrac;    // nFit/nPossible
  bool   isMC;            // flag minor differences between Data & M-C
  bool   mUseCtb;         // disable CTB from matching/vetoing of tracks
  bool   mDropPostCrossingTrack;  // enable/disable post crossing tarck rejection
  int    mStoreUnqualifiedVertex; // set the max # of vertices, sorted by rank
  float  mCut_oneTrackPT; // threshold for storing one track vertices

  // beam line
  double          mX0  ;     // starting point of beam parameterization
  double          mY0  ;     // starting point of beam parameterization
  double          mdxdz;     // beam slope
  double          mdydz;     // beam slope

  // util
  StiToolkit     *mToolkit;
  CtbHitList     *ctbList;
  BemcHitList    *bemcList;
  EemcHitList    *eemcList;
  StEEmcDb       *eeDb;
  EEmcGeomSimple *geomE;

  
  void dumpKalmanNodes(const StiKalmanTrack *track);
  //  void plotVertex(VertexData *);
  //  void plotTracksDca();
  void initHisto();
  
public:
  void setMC(bool x=true){isMC=x;}
  void useCTB(bool x=true){mUseCtb=x;}
  void UsePCT(bool x=true){setDropPostCrossingTrack(!x);}
  void setDropPostCrossingTrack(bool x=true){mDropPostCrossingTrack=x;}
  void Finish();

  TH1F *hA[mxH];
  TH1D *hL ;      // likelyhood distribution
  TH1D *hM, *hW ; // cumulative track mult & weight distribution, for better errZ calculation
  TObjArray * HList;
  StPPVertexFinder();

  // mandatory implementations
  virtual  ~StPPVertexFinder();
  int       fit(StEvent*);        
  void      printInfo(ostream& = cout) const;
  void      UseVertexConstraint(double x0, double y0, double dxdz, double dydz, double weight);
 
  // over-written method
  virtual void  Init();
  virtual void  InitRun  (int runumber);
  virtual void  Clear(); 
};

/***************************************************************************
 *
 * $Log: StPPVertexFinder.h,v $
 * Revision 1.13  2009/04/10 22:57:14  genevb
 * Typo correction
 *
 * Revision 1.12  2009/02/05 21:43:59  balewski
 * Oleksandr renamed StEEmcDbMaker to StEEmcDb and requested this set of code corrections
 *
 * Revision 1.11  2008/12/01 22:57:46  balewski
 * Added capability to reco 1 high pT track vertices with positive rank. 2+ match vertices will have rank above 1e6. Sub-prime vertices (for Akio) have negative rank. More details is given at:
 * http://drupal.star.bnl.gov/STAR/comp/reco/vf/ppv-vertex/2009-algo-upgrade-1
 *
 * Revision 1.10  2008/10/23 20:37:32  genevb
 * Add switches for turning on/off use of Post-Crossing Tracks [default:off]
 *
 * Revision 1.9  2008/10/21 19:23:06  balewski
 * store unqualified vertices on Akio's request
 *
 * Revision 1.8  2008/08/21 22:09:31  balewski
 * - In matchTrack2Membrane()
 *   - Cut on hit max R chanegd from 190 to 199cm
 *   - Fixed logic failure of counting possible hits
 *   - Fixed logic failure of crossing CM for certain pattern of hits
 * - Added a new function bool isPostCrossingTrack()
 *   - it returns true if track have 2 or more hits in wrong z
 * - Use isPostCrossingTrack() in fit()
 * - Added switch setDropPostCrossingTrack(bool), defaulted to true
 * All changes tested & implemented by Akio in preparation for 2008 pp production.
 * The key change (removing PostCrossingTrack) is in response to the change of the TPC cluster finder
 * - now we use the on-line version which allows for longer range of TPC time buckets to be used.
 *
 * Revision 1.7  2006/03/12 17:01:01  jeromel
 * Minor change + use ppvNoCtbVertexFinder
 *
 * Revision 1.6  2006/03/11 04:12:50  balewski
 * 2 changes in preparation for 2006 data processing:
 * - CTB matching  ON/OFF switch activated by m_Mode 0x8 or 0x10
 * - vertex enum extension depending on CTB usage - hack in the moment, Jerome needs to proviade actual new enum
 * - BTOW calibration wil change for 2006+ from maxt eT of ~27 --> 60 GeV
 * NOTE : this new code was NOT executed - it is late, I want to get it in CVS
 * Tomorrow I'll do some tests
 * Jan
 *
 * Revision 1.5  2005/08/30 22:08:43  balewski
 * drop '*' from declaration of   mTrackData &  mVertexData
 *
 * Revision 1.4  2005/08/12 18:35:28  balewski
 * more accurate calculation of Z-vertex error
 * by accounting for average weight of tracks contributing to the likelihood,
 *  Now errZ is of 0.5-1.5 mm, was ~2x smaller
 *
 * Revision 1.3  2005/07/20 05:34:16  balewski
 * cleanup
 *
 * Revision 1.2  2005/07/19 22:01:59  perev
 * MultiVertex
 *
 * Revision 1.1  2005/07/11 20:38:12  balewski
 * PPV added for real
 *
 *
 *
 **************************************************************************/

