// StSsdWafer.hh
// Ludovic Gaudichet

#ifndef  STAR_StSsdWafer_hh
#define  STAR_StSsdWafer_hh


//#include "StGlobals.hh"
//#include  <Rtypes.h>
#include "StSsdObjects.hh"


class StSsdWafer
{
public :
  
 
  StSsdWafer();
  ~StSsdWafer();

  void init( double *x, double *d, double *t, double *n,
	     int lay, int lad, int waf, double lX, double lY );
  
  const int isOn()      const;
  const int waferID()   const;
  const int ladderID()  const;
  const int layerID()   const;
  const double dx()     const;
  const double dy()     const;
  const double dz()     const;
  const double alpha()  const;
  const double beta()   const;
  const double gamma()  const;
  const double param(int par) const;
  const double localXsize()  const;
  const double localYsize()  const;

  void switchOff();
  void switchOn();

  void setDx(double dx);
  void setDy(double dy);
  void setDz(double dz);
  void setAlpha(double alpha);
  void setBeta(double beta);
  void setGamma(double gamma);
  void shiftParam(int param, double val);

  const double* center() const;

  int addNewHit( localPoint *local ); // local coordinates
  int addNewHit( globalPoint *global ); // global coordinates

  int global2Local( globalPoint* global ,localPoint *local );
  int local2Global( localPoint *local, globalPoint* global );

  localPoint pointOnWafer( globalPoint* p1, globalPoint* p2 );
  int isTouched( globalPoint *vertex, globalPoint *p2, localPoint *P );

  int recordEvent();
  const globalPoint hit(int event, int nhit);
  void speedHit(int event, int nhit, globalPoint *p);

  const int numberOfHits(int event) const;
  const int numberOfHits() const;

  int clotherHit( int event, globalPoint* p1, globalPoint* p2, float threshold );

  void printState();
  
protected :

private :

  void calculD();
  void calculT();
  void calculN();


  int mIsOn;
  int mWaferID;
  int mLadderID;
  int mLayerID;
  
  int event;
  localPoint **mEventFirstHit[maxNumberOfEvents+1];

  int mCurrentHit;
  int mCurrentEvent;

  int maxNumHits;
  localPoint **mHits;
  
  // wafer size :
  double mLX;
  double mLY;
  
  // alignment parameter :
  double mDx, mDy, mDz, mAlpha, mBeta, mGamma;

  // in svg_geom :
  double mx0[3]; // wafer center
  double md0[3]; // drift direction
  double mt0[3]; // transverse to drift
  double mn0[3]; // normal to wafer



  double mx[3]; // wafer center
  double md[3]; // drift direction
  double mt[3]; // transverse to drift
  double mn[3]; // normal to wafer

};

const inline int StSsdWafer::waferID() const { return mWaferID; }
const inline int StSsdWafer::ladderID() const { return mLadderID; }
const inline int StSsdWafer::layerID() const { return mLayerID; }

inline void StSsdWafer::switchOff() { mIsOn = 0; };
inline void StSsdWafer::switchOn() { mIsOn = 1; };
const inline int StSsdWafer::isOn() const { return mIsOn ; };

const inline int StSsdWafer::numberOfHits(int event) const
{  return ( mEventFirstHit[event+1] - mEventFirstHit[event] ); };

const inline int StSsdWafer::numberOfHits() const
{ return mCurrentHit; };

const inline double* StSsdWafer::center() const { return mx; };

const inline double StSsdWafer::dx() const {return mDx;};
const inline double StSsdWafer::dy() const {return mDy;};
const inline double StSsdWafer::dz() const {return mDz;};
const inline double StSsdWafer::alpha() const {return mAlpha;};
const inline double StSsdWafer::beta() const {return mBeta;};
const inline double StSsdWafer::gamma() const {return mGamma;};

const inline double StSsdWafer::localXsize()  const{return mLX;};
const inline double StSsdWafer::localYsize()  const{return mLY;};

#endif
