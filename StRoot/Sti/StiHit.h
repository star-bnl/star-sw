/*! \class StiHit
  StiHit is a simple class that encapsulates a three dimensional position
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
  a plane (once the location of the plane is known) StiHit stores one more
  value: x.  This value of x corresponds to the magnitude of a vector pointing
  from the origin to the detector plane <b>perpendicular to the plane</b>.  So,
  for planes with no tilt w.r.t. the origin (e.g., TPC pad-planes), x will be
  identical to position.  Actually, this is not even quite true for tpc hits, as
  distortion corrections can make x slightly different than position.  However,
  by storing both x and position, we allow for the separation of information
  that depends only on material location (track fitting) from that which
  depends only on hit location (track finding).
  <p>
  StiHit stores information that represents a full error matrix.  For efficiency
  purposes this is stored as a collection of discreet floats instead of a
  matrix.  Because the error matrix is always symmetric, we must store only six
  values.  These are denoted by s_ij, where s_ij corresponds to the (i,j)
  component of the matrix.
  <p>
  StiHit also stores a pointer to an StHit that it corresponds to.
  Additionally, StiHit stores a pointer to the StDetector object from which its
  measurement arose.

  \author M.L. Miller (Yale Software)
 */

#ifndef StiHit_HH
#define StiHit_HH
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StMeasuredPoint.h"
#include "StThreeVectorF.hh"
#include "StMatrixF.hh"
class StiHit 
{
public: 

  enum StiHitProperty { kR,
			kZ,
			kPseudoRapidity,
			kPhi};
    ///Default constructor.
    StiHit();
    StiHit(const StiHit&);
    const StiHit& operator=(const StiHit& );
    ///Default destructor.
    ~StiHit();

    ///Return the local x, y, z values.
    const float &x() const {return mx;}
    const float &y() const {return my;}
    const float &z() const {return mz;}
    const float  y(float time) const {return my + _vy*time;}
    const float  z(float time) const {return mz + _vz*time;}
    ///Return the global x, y, z values.
    float x_g() const {return _xg;}
    float y_g() const {return _yg;}
    float z_g() const {return _zg;}
    
    ///Return components of the error matrix.
    float sxx() const {return msxx;}
    float syy() const {return msyy;}
    float szz() const {return mszz;}
    float sxy() const {return msxy;}
    float sxz() const {return msxz;}
    float syz() const {return msyz;}
    const float *errMtx() const   		{return &msxx;}
    ///Return the energy deposition associated with this point 
    float getEloss();
    ///Return the refAngle of the detector plane from which the hit arose.
    float refangle() const {return mrefangle;}
    ///Return the position of the detector plane from whcih the hit arose.
    float position() const {return mposition;}
    ///Return a const pointer to the StiDetector object from which the hit
    ///arose.
    const StiDetector* detector() const {return mdetector;}
    ///Test for DCA.  Fake hit for dca calculation
    int isDca() const;
    ///Make fake hit for dca calculation
    void makeDca();
    ///Return a const pointer to the StHit object corresponding to this StiHit
    ///instance
    //const StHit* stHit() const 
    const StMeasuredPoint * stHit() const {return msthit;}

    ///If we are running in simulated mode, return a const pointer to the
    /// StMcHit associated with this StiHit.
    //const StMcHit* stMcHit() const;

    ///Return the number of times this hit was assigned to a track
    unsigned int timesUsed() const { return mTimesUsed;}
    
    ///Return a boolean that marks whether or not this hit is assigned to a
    ///track.
    //bool   isUsed() const;

    ///Return a const reference to a StThreeVectorF that denotes the position
    ///of the hit in global STAR coordinates.
    const StThreeVectorF globalPosition() const;

    void set(float position,  float angle, float y, float z);

    ///Set the local position and error in one function call
    void set(const StiDetector * detector,
	     const StMeasuredPoint * stHit,
	     float energy,
	     float x, float y, float z, 
	     float sxx=1, float sxy=1, float sxz=1, float syy=1, float syz=1, float szz=1);
    ///Set the global position and error in one function call 
    ///A transformation is performed internally from global to local coordinates
    ///according to the detector information.
    void setGlobal(const StiDetector * detector,
		   const StMeasuredPoint * stHit,
		   float x, float y, float z,
		   float energy);
    
    ///Set the position error matrix for the measurement from an StMatrixF
    ///object.
    void setError(const StMatrixF&);
    ///Set the position error matrix for the measurement from an float array
    ///object.
    void setError(const float errMx[6]);
    ///Set the pointer to the StiDetector from which the hit arose.
    void setDetector(const StiDetector*det) {mdetector=det;};
    ///Set the pointer to the corresponding StHit object.
    void setStHit(const StMeasuredPoint*hit){msthit=hit;}
    ///Set the number of times used
    void setTimesUsed(unsigned int);
    void setVz(float vz) {_vz = vz;}
    void setVy(float vy) {_vy = vy;}
    void reset();
    void unset(){;}
    void rotate(double angle);
    double getValue(int key) const;
    double getPseudoRapidity() const;
    float  vz() const {return _vz;}
    float  vy() const {return _vy;}
    friend ostream& operator<<(ostream& os, const StiHit& h);
private:
    char  mBeg[1];
    unsigned char mTimesUsed;
    float mrefangle;
    float mposition;
    float mx;
    float my;
    float mz; 
    float msxx;
    float msxy;
    float msyy;
    float msxz;
    float msyz;
    float mszz;
    // global position
    float _xg,_yg,_zg;
    const StiDetector* mdetector;
    const StMeasuredPoint * msthit;
    float _energy;
    // drift velocities cm/mksec( 0 for non driting )
    float _vy, _vz;
    
    char  mEnd[1];
public:
    int mCount;
};

//Functors for ordering hits
struct StiHitRadiusLessThan
{
  bool operator() (const StiHit*h1, const StiHit*h2) const
  {
    double x1 = h1->x_g();
    double y1 = h1->y_g();
    double r1 = sqrt(x1*x1+y1*y1);
    double x2 = h2->x_g();
    double y2 = h2->y_g();
    double r2 = sqrt(x2*x2+y2*y2);
    return r1<r2;
  }
};

struct StidHitLessThan
{
  bool operator() (const StiHit*lhs, const StiHit*rhs) const
    {
      return (lhs->y() < rhs->y()) ? true : false;
    }
};

struct StizHitLessThan
{
  bool operator() (const StiHit*lhs, const StiHit*rhs) const
  {
    return (lhs->z() < rhs->z()) ? true : false;
  }
};

struct StiHitIsUsed
{
  bool operator() (const StiHit*hit) const
  {
    return (hit->timesUsed()==0);
  }
};


#endif
