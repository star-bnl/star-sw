/*!
 * \class StPPVertexFinder
 * \author Jan Balewski, July 2004
 *
 *  StGenericVertexFinder implementation of PPV
 * $Id: StPPVertexFinder.h,v 1.40 2017/02/15 15:30:19 smirnovd Exp $
 *
 */

#include <vector>

#include "StGenericVertexMaker/StGenericVertexFinder.h"

#include "StPhysicalHelixD.hh"

class TH1F;
class TH2F;
class TH1D;

class StiKalmanTrack;
class TrackData;
class VertexData;
class StEvent; 
class StiToolkit;
class StEEmcDb;

class EEmcGeomSimple;
class StBTofGeometry; 

class BtofHitList;  
class CtbHitList;
class BemcHitList;
class EemcHitList;
class Vertex3D;

class StPPVertexFinder: public StGenericVertexFinder {
 private:

  /// Takes a list of vertex candidates/seeds and updates each vertex position
  /// by fitting tracks pointing to it
  int fitTracksToVertex(VertexData &vertex);

  /// Creates DCA states for selected tracks (mTrackData) and fills the member
  /// container mDCAs
  void createTrackDcas(const VertexData &vertex);

  /// using the ROOT's TSpectrum peak finder applied to the distribution of
  /// track DCAs along the `z` axis
  void findSeeds_TSpectrum();

  /// Searches for vertex candidates and fills private `mVertexData` container
  /// by building a likelihood distribution of track DCAs along the `z` axis
  void findSeeds_PPVLikelihood();

  enum {mxH=32};
  bool examinTrackDca(const StiKalmanTrack*, TrackData &t);
  void matchTrack2BTOF(const StiKalmanTrack*, TrackData &t, StBTofGeometry *geom);
  void matchTrack2CTB(const StiKalmanTrack*, TrackData &t);
  void matchTrack2EEMC(const StiKalmanTrack*, TrackData &t);
  void matchTrack2BEMC(const StiKalmanTrack*, TrackData &t);
  bool matchTrack2Membrane(const StiKalmanTrack*, TrackData &t);
  bool isPostCrossingTrack(const StiKalmanTrack* track);

  /// A container with pre-selected tracks to be used in seed finding
  std::vector<TrackData>  mTrackData;
  std::vector<VertexData> mVertexData;
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

  TH1F *hA[mxH];
  TH2F *hACorr;
  TH1D *hL ;      // likelyhood distribution
  TH1D *hM, *hW ; // cumulative track mult & weight distribution, for better errZ calculation
  TObjArray HList;

  // params
  double mMinTrkPt;            ///< ~ pT=0.16(GeV/c) == R=2 (m )in 2001
  double mMaxTrkDcaRxy;        ///< DCA to nominal beam line for each track
  float  mMaxZradius;          ///<  used in matching: tracks to zVertex
  int    mMinMatchTr;          ///<  for valid vertex
  float  mMaxZrange;           ///<  cut off for tracks Z_DCA
  float  mDyBtof;              ///<  BTOF delta y cut
  float  mMinZBtof;            ///<  BTOF local z min cut
  float  mMaxZBtof;            ///<  BTOF local z max cut
  float  mMinAdcBemc;          ///<  BEMC towers with MIP response
  float  mMinAdcEemc;          ///<  EEMC towers with MIP response
  float  mMinFitPfrac;         ///<  nFit/nPossible
  bool   mFitPossWeighting;    ///< Use nFit/nPossible in track weighting (ranking)
  bool   mDropPostCrossingTrack;  ///< enable/disable post crossing tarck rejection
  int    mStoreUnqualifiedVertex; ///< set the max # of vertices, sorted by rank
  float  mCut_oneTrackPT;         ///< threshold for storing one track vertices
  bool   mStudyBeamLineTracks; ///< activates writing them out + lot of QA histos,
                               ///< use  BFC option: VtxSeedCalG to enable it, expert only

  // util
  StiToolkit     *mToolkit;
  BtofHitList    *btofList;
  CtbHitList     *ctbList;
  BemcHitList    *bemcList;
  EemcHitList    *eemcList;
  StBTofGeometry *btofGeom;
  EEmcGeomSimple *geomE;
  
  void dumpKalmanNodes(const StiKalmanTrack *track);
  void initHisto();

  virtual void  UseVertexConstraint() {}
  
public:
  virtual void UsePCT(bool x=true) { mDropPostCrossingTrack = !x; }
  virtual void Finish();

  StPPVertexFinder(VertexFit_t fitMode=VertexFit_t::Beamline1D);

  // mandatory implementations
  virtual  ~StPPVertexFinder();
  virtual int       fit(StEvent*);        
  void      printInfo(ostream& = cout) const;
 
  // over-written method
  virtual void  Init();
  virtual void  InitRun  (int runumber);
  virtual void  Clear(); 
  virtual void  CalibBeamLine(); // activates saving high quality prim tracks for 3D fit of the beamLine
};
