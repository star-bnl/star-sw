/*!
 * \class StPPVertexFinder
 * \author Jan Balewski, July 2004
 *
 *  StGenericVertexFinder implementation of PPV
 * $Id: StPPVertexFinder.h,v 1.2 2005/07/19 22:01:59 perev Exp $
 *
 */
#include "StGenericVertexMaker/StGenericVertexFinder.h"

class StiKalmanTrack;
class TrackData;
class VertexData;
class TGraphErrors;
class StEvent; 
class StiToolkit;
class StEEmcDbMaker;

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
  vector<TrackData> *mTrackData;
  vector<VertexData> *mVertexData;
  bool buildLikelihood();
  bool findVertex(VertexData &);
  bool evalVertex(VertexData &);
  void exportVertices(); 

  void saveHisto(TString fname);
  int  mTotEve;
  int  eveID;

  // params
  double mMinTrkPt;       //~ pT=0.16(GeV/c) == R=2 (m )in 2001
  double mMaxTrkDcaRxy;   //DCA to nominal beam line for each track
  float  mMaxZradius;  // used in matching: tracks to zVertex
  int    mMinMatchTr; // for valid vertex
  float  mMaxZrange; // cut off for tracks Z_DCA
  float  mMinAdcBemc; // BEMC towers with MIP response
  float  mMinAdcEemc; // EEMC towers with MIP response
  float  mMinFitPfrac; // nFit/nPossible
  bool isMC; // flag minor differences between Data & M-C

  // beam line
  double          mX0  ;     // starting point of beam parameterization
  double          mY0  ;     // starting point of beam parameterization
  double          mdxdz;     // beam slope
  double          mdydz;     // beam slope

  // util
  StiToolkit  *mToolkit;
  CtbHitList  *ctbList;
  BemcHitList  *bemcList;
  EemcHitList  *eemcList;
  StEEmcDbMaker *eeDb;
  EEmcGeomSimple *geomE;

  FILE *fdOut; // record ascii output of vertex finder for Jan's QA

  void dumpKalmanNodes(const StiKalmanTrack *track);
  void plotVertex(VertexData *);
  void plotTracksDca();
  void initHisto();
  
public:
  void setMC(bool x=true){isMC=x;}
  void Finish();

  TH1F *hA[mxH];
  TH1D *hL; // likelyhood distribution
  TObjArray * HList;
  StPPVertexFinder();

  // mandatory implementations
  virtual         ~StPPVertexFinder();
  int             fit(StEvent*);        
  void            printInfo(ostream& = cout) const;
  void            UseVertexConstraint(double x0, double y0, double dxdz, double dydz, double weight);
  //int    NCtbMatches() {return -1;}; //dumm
  //int    NCtbSlats() {return -1;}; //dumm  
 
  // over-written method
  virtual void  Init();
  virtual void  InitRun  (int runumber);
  virtual void  Clear(); 
};

/***************************************************************************
 *
 * $Log: StPPVertexFinder.h,v $
 * Revision 1.2  2005/07/19 22:01:59  perev
 * MultiVertex
 *
 * Revision 1.1  2005/07/11 20:38:12  balewski
 * PPV added for real
 *
 *
 *
 **************************************************************************/

