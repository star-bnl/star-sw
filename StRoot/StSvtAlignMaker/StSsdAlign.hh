// StSsdAlign.hh
// Ludovic Gaudichet

#ifndef  STAR_StSsdAlign_hh
#define  STAR_StSsdAlign_hh

#include "StSsdWafer.hh"
#include "StSsdEvent.hh"

class svg_geom_st;
class StSvtConfig;
class StSvtCoordinateTransform;
class StSsdLadder
{
public :
StSsdLadder(const int nWaf);
~StSsdLadder();
StSsdWafer* wafers[16];
const int numberOfWafers() const ;

protected :
int mWafer;

};

const inline int StSsdLadder::numberOfWafers() const { return mWafer; };
//____________________________________________________________________

class StSsdLayer
{
public :
StSsdLayer(const int nWafer, const int nLadder);
~StSsdLayer();
StSsdLadder* ladders[20];
const int numberOfLadders() const ;
const int numberOfWafers() const ;

protected :
int mWafer;
int mLadder;
};


const inline int StSsdLayer::numberOfLadders() const { return mLadder; };
const inline int StSsdLayer::numberOfWafers() const { return mWafer; };

//____________________________________________________________________


class StSsdAlign
{
public :
  
  StSsdAlign();
  ~StSsdAlign();
  
  StSsdWafer mWafer[536]; 
  int nbofhits[536];
  StSsdLayer* layers[7];
  // To use wafer object  :
  // 1) mWafer[0..535].
  // or
  // 2) layers[0..6]->ladders[]->wafers[]->

  const int layer() const;
  void init( svg_geom_st *geom, StSvtConfig* config, int NumEvents);
  int addNewHit( const double x, const double y, const double z );
  //  int addNewHit( StThreeVector<double> * newHit );
  int recordEventHits();


  int areWafersCloth( int inner, int outer, float distance );
  int areWafersAligned( int inner, int middle, int outer );
  int areWafersAligned( globalPoint *p1, int middle, int outer );
  globalPoint findVertex(int event);
  int tracking();
  void trackingGoodTRacks();
  
  void saveTrackedEvents();
  void loadTrackedEvents(char * fichier);

  int simulUnAlignment(double trans, double rot);
  int simulUnAlignment(double transW, double rotW,double transL, double rotL );
  int simulEvents(int numberOfEventsSoFar,int numberOfTracks ,int level);

  double takeDerivatives(double step);
  void shiftParams(double sh);
  double totalChi2();
  void updateGlobalPoints(int Nevent);
  void makeAlign( int iMax );
  void makeAlign2( int iMax ); 

  void simulCosmics(int numberOfEventssoFar,int numberOfTracks ,int level);
  void cosmicAlign( int iMax );
  void cosmicAlign2( int iMax );
  double totalCosmicChi2();
  double takeCosmicDerivatives(double step);

  StSsdEvent *mEvents[maxNumberOfEvents];

  void FillTrack(int ev, int TrackNumber, int nTrackHit, globalPoint* gP, int* ilp);
  void CreateEvent(int ev);
  void SetTransform( StSvtCoordinateTransform* Tran);

protected :

  int mLayer;
  int mNumberOfEvents;
  
  svg_geom_st *mgeom;
  goodtrack mGoodTracks[maxNumberOfEvents][1]; //[maxNumberOfEvents][2000] in 'primary vertex' private mode
  int mNumberOfGoodTracks[maxNumberOfEvents];
  
  double mDerivatives[536][6];

  StSvtCoordinateTransform* mTransform;

};

const inline int StSsdAlign::layer() const { return mLayer; };
inline void StSsdAlign::SetTransform(StSvtCoordinateTransform* trans){ mTransform = trans;};

#endif
