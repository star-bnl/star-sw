/***************************************************************************
 *
 * $Id: StSsdAlign.hh,v 1.2 2001/05/09 16:33:02 gaudiche Exp $
 *
 * Author: Ludovic Gaudichet
 ***************************************************************************
 *
 * Description: SVT & SSD alignment code
 *
 ***************************************************************************
 *
 * $Log: StSsdAlign.hh,v $
 * Revision 1.2  2001/05/09 16:33:02  gaudiche
 * bug on Solaris fixed - cleanup
 *
 *
 ***************************************************************************/

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
  
  StSsdLayer* layers[7];

  // To use wafer object  :
  // 1) mWafer[0..535].
  // or
  // 2) layers[0..6]->ladders[]->wafers[]->
  StSsdWafer mWafer[536]; 
  int nbofhits[536];
  StSsdEvent *mEvents[maxNumberOfEvents];

  const int layer() const;
  void init( svg_geom_st *geom, StSvtConfig* config, int NumEvents);
  void updateGeom(svg_geom_st* geom, StSvtConfig* config);

  int addNewHit( const double x, const double y, const double z );
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

  void FillTrack(int ev, int TrackNumber, int nTrackHit, globalPoint* gP, int* ilp);
  void CreateEvent(int ev);
  void SetTransform( StSvtCoordinateTransform* Tran);
  void tetaDistri(int &nval, double *teta);
  void nHitsPerTrackDistri(int &nval, int *number);
  void chi2Distri(int &nval, double *chi2);

protected :

  int mLayer;
  int mNumberOfEvents;
  double mDerivatives[536][6];

  svg_geom_st *mgeom;
  StSvtCoordinateTransform* mTransform;
};

const inline int StSsdAlign::layer() const { return mLayer; };
inline void StSsdAlign::SetTransform(StSvtCoordinateTransform* trans){ mTransform = trans;};

#endif
