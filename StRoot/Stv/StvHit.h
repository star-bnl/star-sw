/*! \class StvHit
  StvHit is a simple class that encapsulates a three dimensional position
  measurement.  The measurement is represented in a frame
  that is 'local' to the detector plane from which it arose.
  <p>
  It is assumed
  that all hits come from a detector that can be composed of discrete planes.
  Each plane is characterized by two values: position and refAngle.  In the
  mid-rapidity region of STAR (SVT, TPC, EMC, etc) the position corresponds to
  the magnitude of a vector pointing from the origin to the center of the plane.
  The refAngle is the azimuthal defined by the aforemention vector and the
  STAR global x-axis.  All hits store the position and refAngle of the detector
  plane from which they came.
  <p>
  Within each detector plane, the hit is characterized by two more numbers:
  y and z.  The y value corresponds to the distance along the plane (e.g.,
  padrow) w.r.t. the center of the plane.  The z value corresponds to the STAR
  global coordinate.
  <p>
  While it only takes two values (y and z) two specify the hit location within
  a plane (once the location of the plane is known) StvHit stores one more
  value: x.  This value of x corresponds to the magnitude of a vector pointing
  from the origin to the detector plane <b>perpendicular to the plane</b>.  So,
  for planes with no tilt w.r.t. the origin (e.g., TPC pad-planes), x will be
  identical to position.  Actually, this is not even quite true for tpc hits, as
  distortion corrections can make x slightly different than position.  However,
  by storing both x and position, we allow for the separation of information
  that depends only on material location (track fitting) from that which
  depends only on hit location (track finding).
  <p>
  StvHit stores information that represents a full error matrix.  For efficiency
  purposes this is stored as a collection of discreet floats instead of a
  matrix.  Because the error matrix is always symmetric, we must store only six
  values.  These are denoted by s_ij, where s_ij corresponds to the (i,j)
  component of the matrix.
  <p>
  StvHit also stores a pointer to an StHit that it corresponds to.
  Additionally, StvHit stores a pointer to the StDetector object from which its
  measurement arose.

  \author M.L. Miller (Yale Software)
 */

#ifndef StvHit_HH
#define StvHit_HH
#include "assert.h"
#include "math.h"
class StHitPlane;
class StvHit 
{
public: 

    ///Default constructor.
    StvHit();
//     StvHit(const StvHit&);
//     const StvHit& operator=(const StvHit& );
    ///Default destructor.
    ~StvHit();

    ///Return the global x, y, z values.
    const float *x()     const {return mGlo;}
          float getRxy() const {return sqrt(mGlo[0]*mGlo[0]+mGlo[1]*mGlo[1]);}
    
    ///Return components of the error matrix.
    virtual const float *errMtx() const   		{return 0;}
    virtual       float *errMtx()   	   		{return 0;}
                  double err2() const;
    ///Return a const pointer to the StHitPlane object from which the hit
    ///arose.
    const StHitPlane* detector() const {return mDetector;}
                  int detectorId() const ;
    ///Return a const pointer to the StHit object corresponding to this StvHit
    ///instance
    //const StHit* stHit() const 
    const void *stHit() const {return msthit;}

    ///Set the global position one function call 
    void set(const StHitPlane* detector) {mDetector = detector;}
    void set(const void *stHit,const float x[3]);
    ///Set the global position and error 
    virtual void set(const float *x,const float *err){assert(0);}
    virtual void addCount()		{assert(0);}
    virtual void setCount(int kount)	{assert(0);}
    virtual  int getCount() const	{assert(0);}
    
    ///Set the position error matrix for the measurement from an float array
    ///object.
    virtual void setError(const float errMx[6]){ assert(0);}

    ///Set the number of times used
    ///Return the number of times this hit was assigned to a track
    virtual int  isUsed() const 	{ return (int)mTimesUsed;}
    virtual void addTimesUsed(int add=1){ mTimesUsed+=add ;}
    virtual void setTimesUsed(int ijk)	{ mTimesUsed=(unsigned char)ijk; }
    virtual void reset();
    void unset(){;}
protected:
    char  mBeg[1];
    unsigned char mTimesUsed;
    float mGlo[3]; 			//global position
    const void *msthit;
    const StHitPlane *mDetector;
    // drift velocities cm/mksec( 0 for non driting )
    char  mEnd[1];
public:
    int mCount;
};

class StvVertex : public StvHit
{
public: 

    ///Default constructor.
    StvVertex(){;}
    ///Default destructor.
    ~StvVertex(){;}
    ///Set the position error matrix for the measurement from an float array
    ///object.
    void reset();
    void set(const float *x,const float *err);
    void set(const StHitPlane* detector
                  ,const void *stHit
		  ,const float *x){assert(0);}
    virtual  int timesUsed() const 	{ return 0;}
    virtual void addTimesUsed(int){;}
    virtual void setTimesUsed(int){;}

    virtual void addCount()		{mKount++ ;}
    virtual void setCount(int kount)	{mKount=kount ;}
    virtual  int getCount() const	{return mKount;}

    const float *errMtx() const   {return mErr;}
          float *errMtx()         {return mErr;}


protected:
    float mErr[6];			//error matrix
    int   mKount;
};
#endif
