/*!
 * \class StPPVertexFinder
 * \author Jan Balewski, July 2004
 *
 *  StGenericVertexFinder implementation of PPV
 * $Id: StPPVertexFinder.h,v 1.5 2017/05/10 23:16:42 smirnovd Exp $
 *
 */

#include "StGenericVertexMaker/StGenericVertexFinder.h"

#include "StPhysicalHelixD.hh"
class StEventToolkit;
class StGlobalTrack;
class TGraphErrors;
class StEvent; 
class StEEmcDb;
class EEmcGeomSimple;
class StBTofGeometry; 

namespace StEvPPV {
class VertexData;
class TrackData;
class BtofHitList;  
class CtbHitList;
class BemcHitList;
class EemcHitList;
class Vertex3D;

class StPPVertexFinder: public StGenericVertexFinder {
 private:
  enum {mxH=32};
  bool examinTrackDca(const StGlobalTrack*, TrackData &t);
  void matchTrack2BTOF(const StGlobalTrack*, TrackData &t, StBTofGeometry *geom);
  void matchTrack2CTB(const StGlobalTrack*, TrackData &t);
  void matchTrack2EEMC(const StGlobalTrack*, TrackData &t, float z);
  void matchTrack2BEMC(const StGlobalTrack*, TrackData &t, float rxy);
  bool matchTrack2Membrane(const StGlobalTrack*, TrackData &t);
  bool isPostCrossingTrack(const StGlobalTrack* track);
  vector<TrackData>  mTrackData;
  vector<VertexData> mVertexData;
  Vertex3D *vertex3D; // for stand alone 3D vertex reco
  bool buildLikelihoodZ();
  bool findVertexZ(VertexData &);
  bool evalVertexZ(VertexData &);
  void exportVertices(); 

  void saveHisto(TString fname);
  int  mTotEve;
  int  eveID;
  unsigned int  mAlgoSwitches; //binary, assign 1bit per change, use enum below
  enum {kSwitchOneHighPT=1}; 

  StEventToolkit *mToolkit;
  // params
  double mMinTrkPt;       //~ pT=0.16(GeV/c) == R=2 (m )in 2001
  double mMaxTrkDcaRxy;   //DCA to nominal beam line for each track
  float  mMaxZradius;     // used in matching: tracks to zVertex
  int    mMinMatchTr;     // for valid vertex
  float  mMaxZrange;      // cut off for tracks Z_DCA
  float  mDyBtof;         // BTOF delta y cut
  float  mMinZBtof;       // BTOF local z min cut
  float  mMaxZBtof;       // BTOF local z max cut
  float  mMinAdcBemc;     // BEMC towers with MIP response
  float  mMinAdcEemc;     // EEMC towers with MIP response
  float  mMinFitPfrac;    // nFit/nPossible
  bool   mFitPossWeighting; // Use nFit/nPossible in track weighting (ranking)
  bool   mDropPostCrossingTrack;  // enable/disable post crossing tarck rejection
  int    mStoreUnqualifiedVertex; // set the max # of vertices, sorted by rank
  float  mCut_oneTrackPT; // threshold for storing one track vertices
  int    mBeamLineTracks; // activates writing them out + lot of QA histos, 
                          // use  BFC option: VtxSeedCalG to enable it, expert only

  // util
  BtofHitList    *btofList;
  CtbHitList     *ctbList;
  BemcHitList    *bemcList;
  EemcHitList    *eemcList;
  StBTofGeometry *btofGeom;
  StEEmcDb       *eeDb;
  EEmcGeomSimple *geomE;
  
  void dumpKalmanNodes(const StGlobalTrack *track);
  //  void plotVertex(VertexData *);
  //  void plotTracksDca();
  void initHisto();

  virtual void  UseVertexConstraint() {}
  
public:
  void UsePCT(bool x=true)			{setDropPostCrossingTrack(!x);}
  void setDropPostCrossingTrack(bool x=true)	{mDropPostCrossingTrack=x;}
  void Finish();

  TH1F *hA[mxH];
  TH2F *hACorr;
  TH1D *hL ;      // likelyhood distribution
  TH1D *hM, *hW ; // cumulative track mult & weight distribution, for better errZ calculation
  TObjArray * HList;
  StPPVertexFinder();

  // mandatory implementations
  virtual  ~StPPVertexFinder();
  int       fit(StEvent*);        
  void      printInfo(ostream& = cout) const;
 
  // over-written method
  virtual void  Init();
  virtual void  InitRun  (int runumber);
  virtual void  Clear(); 
};
}// end namespace StEvPPV

/***************************************************************************
 *
 * $Log: StPPVertexFinder.h,v $
 * Revision 1.5  2017/05/10 23:16:42  smirnovd
 * Some minor refactoring changes:
 *
 * See commits 6fb592df..07da3bdf on master branch
 *
 * StPPVertexFinder: Get rid of a temporary variable
 *
 * StPPVertexFinder: Get rid of extra return
 * Zero vertices returned for unqualified event anyway
 *
 * StGenericVertexFinder: Removed deprecated CalibBeamLine()
 *
 * Revision 1.4  2017/02/14 22:00:41  smirnovd
 * Squashed commit of the following clean-up changes:
 *
 * See master branch for details.
 *
 * - Remove commented code for debugging
 * - Removed extra validation; it is done at construction
 * - No need to include header for apple OS
 * - Removed pointless assert
 * - Use standard portable type name
 * - Remove unused header math_constants.h
 * - StMinuitVertexFinder: Remove abandoned member function
 *
 * Revision 1.3  2016/11/07 21:19:28  smirnovd
 * Added and reworded some doxygen and other comments
 *
 * Also cleaned up not-so-useful comments
 *
 * Revision 1.2  2016/08/18 17:46:15  smirnovd
 * Squashed commit of the following refactoring changes:
 *
 * Date:   Wed Jul 27 18:31:18 2016 -0400
 *
 *     Removed unused arguments in UseVertexConstraint()
 *
 *     In StiPPVertexFinder and StvPPVertexFinder this method does nothing
 *
 * Date:   Wed Jul 27 16:47:58 2016 -0400
 *
 *     Make old UseVertexConstraint private virtual and call it from its public replacement in the base class
 *
 *     also mark methods as private explicitly
 *
 * Date:   Wed Jul 27 16:52:02 2016 -0400
 *
 *     Removed unused private data member mWeight
 *
 * Date:   Wed Jul 27 16:50:42 2016 -0400
 *
 *     Prefer base class static beamline parameters rather than this class private members
 *
 * Date:   Wed Jul 27 16:21:49 2016 -0400
 *
 *     StPPVertexFinder: Got rid of unused private beamline parameters
 *
 *     The equivalent measurements are available from the base class
 *     StGenericVertexFinder
 *
 * Date:   Wed Jul 27 16:19:19 2016 -0400
 *
 *     StPPVertexFinder: For beamline position use equivalent static methods from parent class
 *
 * Date:   Wed Jul 27 16:05:50 2016 -0400
 *
 *     StGenericVertexMaker: Assigning once is enough
 *
 * Date:   Mon Aug 15 10:43:49 2016 -0400
 *
 *     StGenericVertexFinder: Print out beamline parameters
 *
 *     Print beamline values as extracted from the database before any modification.
 *
 * Date:   Wed Jul 6 15:33:02 2016 -0400
 *
 *     Stylistic changes and minor refactoring
 *
 *     Whitespace and comments for improved readability
 *     s/track/stiKalmanTrack/
 *
 * Date:   Wed Jul 6 15:28:16 2016 -0400
 *
 *     StPPVertexFinder: Switched to cleaner c++11 range loop syntax
 *
 * Date:   Wed Jul 6 15:22:14 2016 -0400
 *
 *     StPPVertexFinder: Minor c++ refactoring
 *
 *     - Removed unused counter
 *     - c-style array to std::array
 *
 * Date:   Wed Jul 6 15:20:11 2016 -0400
 *
 *     Deleted commented out code
 *
 *     Removed unused #include's StMinuitVertexFinder
 *
 * Revision 1.1  2013/08/16 22:19:56  perev
 * PPV with only StEvent dependency
 *
 * Revision 1.18  2013/04/09 22:37:56  genevb
 * Remove boostEfficiency codes: DB usage implemented
 *
 * Revision 1.17  2013/04/05 21:00:02  jeromel
 * Implemented and merged back to source the boostEfficiency (i.e. change of
 * nFit /nPossible points on the track fract to consider). No DB imp yet.
 *
 * Fixed boostEfficiency()
 *
 * Changed cout to LOG_INFO
 *
 * Revision 1.16  2012/12/12 22:09:58  fisyak
 * add sys/types.h include for APPLE
 *
 * Revision 1.15  2010/09/10 21:08:35  rjreed
 * Added function UseBOTF and bool mUseBtof to switch the use of the TOF on and off in vertex finding.  Default value is off (false).
 * Added functions, and variables necessary to use the TOF in PPV for vertex finding.  Includes matching tracks to the TOF and changing the track weight based on its matched status with the TOF.
 *
 * Revision 1.14  2009/07/09 21:29:03  balewski
 * allow export of prim tracks for 3D beam line fit (use VtxSeedCalG option),
 * oneTrack vertex thresholds was lowered form 15 to 10 GeV/c
 *
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

