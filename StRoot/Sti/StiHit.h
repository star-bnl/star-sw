//StiHit.h
//M.L. Miller (Yale Software)
//04/01

//Hit class to be used for the ITTF tracker

// Ignore me

/*! \class StiHit
  StiHit is a simple class that encapsulates a three dimensional position
  measurement in the STAR detector.  The measurement is represented in a frame
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
  purposes this is stored as a collection of discreet doubles instead of a
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

class StHit;
class StThreeVectorD;
class StThreeVectorF;
class StMatrixF;
class StiDetector;

class StiHit 
{
public:
    ///Default constructor.
    StiHit();
    
    ///Default destructor.
    ~StiHit();

    //Gets
    ///Return the local x value.
    double x() const;
    ///Return the local y value.
    double y() const;
    ///Return the global z value.
    double z() const;
    ///Return the (x,x) component of the error matrix.
    double sxx() const;
    ///Return the (y,y) component of the error matrix.
    double syy() const;
    ///Return the (z,z) component of the error matrix.
    double szz() const;
    ///Return the (x,y) component of the error matrix.
    double sxy() const;
    ///Return the (x,z) component of the error matrix.
    double sxz() const;
    ///Return the (y,z) component of the error matrix.
    double syz() const;

    double getEloss();

    ///Return the refAngle of the detector plane from which the hit arose.
    double refangle() const;
    ///Return the position of the detector plane from whcih the hit arose.
    double position() const;

    ///Return a const pointer to the StiDetector object from which the hit
    ///arose.
    const StiDetector* detector() const;
    ///Return a pointer to the StiDetector object from which the hit arose.
    StiDetector* detector();

    ///Return a const pointer to the StHit object corresponding to this StiHit
    ///instance
    const StHit* stHit() const;

    ///If we are running in simulated mode, return a const pointer to the
    /// StMcHit associated with this StiHit.
    //const StMcHit* stMcHit() const;

    ///Return a boolean that marks whether or not this hit is assigne to a
    ///track.
    bool   isUsed() const;

    ///Return a const reference to a StThreeVectorF that denotes the position
    ///of the hit in global STAR coordinates.
    const StThreeVectorF& globalPosition() const;

    //Sets

    ///Set the position and error in one function call
    void set(double refAngle, double position,double x, double y, double z, 
	     double sxx, double sxy, double sxz, double syy, double syz,
	     double szz);

    ///Set the local x value.
    void setX(double);
    ///Set the local y value.
    void setY(double);
    ///Set the global z value.
    void setZ(double);
    ///Set the refAngle of the detector plane from the hit arose.
    void setRefangle(double);
    ///Set the position of the detector plane from the hit arose.
    void setPosition(double);

    ///Set the error-matrix components one by one:
    void setSxx(double);
    void setSyy(double);
    void setSzz(double);
    void setSxy(double);
    void setSxz(double);
    void setSyz(double);
    
    ///Set the position error matrix for the measurement from an StMatrixF
    ///object.
    void setError(const StMatrixF&);
    ///Scale the error arbitrarily
    void scaleError(double);
    ///Set the pointer to the StiDetector from which the hit arose.
    void setDetector(StiDetector*);
    ///Set the pointer to the corresponding StHit object.
    void setStHit(StHit*);
    ///Set a boolean that marks whether or not this hit is assigned to a track.
    void setUsed(bool);

    //Operators

    //Action
    void reset();
    
private:
  
    double mrefangle;
    double mposition;
    double mx;
    double my;
    double mz; 
    double msxx;
    double msyy;
    double mszz;
    double msxy;
    double msxz;
    double msyz;
    bool   mused;
    StiDetector* mdetector;
    StHit* msthit;
};

//Inlines-----------------------------------------------------------

inline void StiHit::setSxx(double val)
{
    msxx=val;
}

inline void StiHit::setSyy(double val)
{
    msyy=val;
}

inline void StiHit::setSzz(double val)
{
    mszz=val;
}

inline void StiHit::setSxy(double val)
{
    msxy=val;
}

inline void StiHit::setSxz(double val)
{
    msxz=val;
}

inline void StiHit::setSyz(double val)
{
    msyz=val;
}

inline void StiHit::reset()
{
    mrefangle = mposition = mx = my = mz = msxx = msyy = mszz = msxy = msxz = msyz = 0.;
    mused = false;
    mdetector = 0;
    msthit = 0;
}

inline void StiHit::scaleError(double scale)
{
    msxx*=scale;
    msyy*=scale;
    mszz*=scale;
    msxy*=scale;
    msxz*=scale;
    msyz*=scale;
}

inline double StiHit::x() const {return mx;}

inline double StiHit::y() const {return my;}

inline double StiHit::z() const {return mz;}

inline double StiHit::sxx() const {return msxx;}

inline double StiHit::syy() const {return msyy;}

inline double StiHit::szz() const {return mszz;}

inline double StiHit::sxy() const {return msxy;}

inline double StiHit::sxz() const {return msxz;}

inline double StiHit::syz() const {return msyz;}

inline double StiHit::refangle() const {return mrefangle;}

inline double StiHit::position() const {return mposition;}

inline const StHit* StiHit::stHit() const {return msthit;}

inline const StiDetector* StiHit::detector() const {return mdetector;}

inline StiDetector* StiHit::detector() {return mdetector;}

inline bool   StiHit::isUsed() const { return mused;}

inline void StiHit::set(double refAngle, double position, double x, double y, double z, 
			double sxx, double sxy, double sxz, double syy, double syz, double szz) 
{
    mrefangle = refAngle;
    mposition = position;
    mx = x;
    my = y;
    mz = z;
    msxx = sxx;
    msyy = syy;
    mszz = szz;
    msxy = sxy;
    msxz = sxz;
    msyz = syz;  
    mused = false;
}

inline void StiHit::setX(double val) {mx=val;}

inline void StiHit::setY(double val) {my=val;}

inline void StiHit::setZ(double val) {mz=val;}

inline void StiHit::setRefangle(double val) {mrefangle=val;}

inline void StiHit::setPosition(double val) {mposition=val;}

inline void StiHit::setDetector(StiDetector* det) {mdetector=det;}

inline void StiHit::setStHit(StHit* val) {msthit=val;}

inline void StiHit::setUsed(bool val) { mused = val;}

#endif
