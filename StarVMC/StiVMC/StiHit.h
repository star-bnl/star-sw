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
#include "StMeasuredPoint.h"
#include "StThreeVectorF.hh"
#include "StMatrixF.hh"
#include "TGeoMatrix.h"
#include "TRVector.h"
#include "TRSymMatrix.h"
class StiHit {
 public: 
  
  enum StiHitProperty { kR,
			kZ,
			kPseudoRapidity,
			kPhi};
  StiHit() {Reset();}
  ~StiHit() {}
  
  const TRVector &Measurement()  const {return _M;}
  const TRVector  Measurement(Double_t time)  const {
    TRVector m(Measurement()); if (_DriftVelocity.GetSize()) m += time*_DriftVelocity; return m;
  }
  const TRVector &Gxyz()         const {return _M_g;}
  const TRSymMatrix &CovMatrix() const {return _V_g;}
  //!Return the local x, y, z values.
  Double_t &y()             {return _M[0];}
  Double_t &z()             {return _M[1];}
  Double_t y()        const {return Measurement()[0];}
  Double_t z()        const {return Measurement()[1];}
  Double_t y(Double_t time) {return Measurement(time)[0];}
  Double_t z(Double_t time) {return Measurement(time)[1];}
  //!Return the global x, y, z values.
  Double_t x_g() const {return _M_g[0];}
  Double_t y_g() const {return _M_g[1];}
  Double_t z_g() const {return _M_g[2];}
#if 0  
  //Return components of the error matrix.
  Double_t sxx() const {return _V_g(0,0);}
  Double_t syy() const {return _V_g(1,1);}
  Double_t szz() const {return _V_g(2,2);}
  Double_t sxy() const {return _V_g(1,0);}
  Double_t sxz() const {return _V_g(2,0);}
  Double_t syz() const {return _V_g(2,1);}
#endif
  const Double_t *errMtx() const    {return _V_g.GetArray();}
  //!Return a const pointer to the StiDetector object from which the hit
  //!arose.
  const StiDetector* Detector() const {return _detector;}
  //!Test for DCA.  Fake hit for dca calculation
  Int_t IsDca() const;
  //!Make fake hit for dca calculation
  void makeDca();
  //!Return a const pointer to the StHit object corresponding to this StiHit
  //!instance
  //const StHit* stHit() const 
  const StMeasuredPoint * stHit() const {return _sthit;}
  
  //!If we are running in simulated mode, return a const pointer to the
  //! StMcHit associated with this StiHit.
  //const StMcHit* stMcHit() const;
  
  //!Return the number of times this hit was assigned to a track
  UInt_t timesUsed() const { return mTimesUsed;}
  
  //!Return a boolean that marks whether or not this hit is assigned to a
  //!track.
  //bool   isUsed() const;
  
  //!Return a const reference to a StThreeVectorF that denotes the position
  //!of the hit in global STAR coordinates.
  const StThreeVectorF globalPosition() {return StThreeVectorF(x_g(),y_g(),z_g()); }
  void set(const StiDetector * detector, Float_t y, Float_t z);
  //!Set the local position and error in one function call
  void set(const StiDetector * detector,
	   const StMeasuredPoint * stHit,
	   Float_t x, Float_t y, Float_t z, 
	   Float_t sxx=1, Float_t sxy=1, Float_t sxz=1, Float_t syy=1, Float_t syz=1, Float_t szz=1);
  //!Set the global position and error in one function call 
  //!A transformation is performed internally from global to local coordinates
  //!according to the detector information.
  void setGlobal(const StiDetector * detector,
		 const StMeasuredPoint * stHit,
		 Float_t x, Float_t y, Float_t z);
  //!Set the position error matrix for the measurement from an StMatrixF
  //!object.
  void setError(const StMatrixF&);
  //!Set the position error matrix for the measurement from an Float_t array
  //!object.
  void setError(const Float_t errMx[6]) { _V_g = TRSymMatrix(3,errMx);}
  void setError(const Double_t errMx[6]) { _V_g = TRSymMatrix(3,errMx);}
  //!Set the pointer to the StiDetector from which the hit arose.
  void SetDetector(const StiDetector*det) {_detector=det;};
  //!Set the pointer to the corresponding StHit object.
  void setStHit(const StMeasuredPoint*hit){_sthit=hit;}
  //!Set the number of times used
  void setTimesUsed(UInt_t val) { mTimesUsed=(UChar_t)val; }
  void setVyz(Float_t vy = 0, Float_t vz = 0) {_DriftVelocity = TRVector(2,vy,vz);}
  void setVz(Float_t vz) {_DriftVelocity = TRVector(2,0.,vz);}
  void setVy(Float_t vy) {_DriftVelocity = TRVector(2,vy,0.);}
  void Reset();
  void Unset(){;}
  Double_t Value(Int_t key) const;
  Double_t PseudoRapidity() const;
  Float_t  vz() const {return _vz;}
  Float_t  vy() const {return _vy;}
  friend ostream& operator<<(ostream& os, const StiHit& h);
  void Print(Option_t *opt="") const {cout << *this << endl;}
 private:
  char  mBeg[1];
  UChar_t mTimesUsed;
  const StiDetector* _detector;
  const StMeasuredPoint * _sthit;
  Double_t  _vy, _vz; // drift velocities cm/mksec( 0 for non driting )
  Double_t _local[2]; // measurement in local coordinate system
  Double_t _global[3];// measurement in global coordinate system
  Double_t _cov[6]; 
  char  mEnd[1];
  TRVector    _M;    // measurement in local coordinate system
  TRVector    _DriftVelocity; 
  TRVector    _M_g;  // measurement in Global (Master) coordinate system
  TRSymMatrix _V_g;  // measurement covariance matrix in Global coordinate system
  
 public:
  Int_t mCount;
};
//________________________________________________________________________________
//Functors for ordering hits
struct StiHitRadiusLessThan {
  bool operator() (const StiHit*h1, const StiHit*h2) const  {
    const Double_t x1 = h1->x_g();
    const Double_t y1 = h1->y_g();
    const Double_t r1 = sqrt(x1*x1+y1*y1);
    const Double_t x2 = h2->x_g();
    const Double_t y2 = h2->y_g();
    const Double_t r2 = sqrt(x2*x2+y2*y2);
    return r1<r2;
  }
};
//________________________________________________________________________________
struct StidHitLessThan {
  bool operator() (const StiHit*lhs, const StiHit*rhs) const    {
    return (lhs->y() < rhs->y()) ? true : false;
  }
};
//________________________________________________________________________________
struct StizHitLessThan {
  bool operator() (const StiHit*lhs, const StiHit*rhs) const  {
    return (lhs->z() < rhs->z()) ? true : false;
  }
};
//________________________________________________________________________________
struct StiHitIsUsed {
  bool operator() (const StiHit*hit) const  {return (hit->timesUsed()==0); }
};
#endif
