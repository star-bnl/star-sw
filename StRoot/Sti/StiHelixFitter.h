
//StiHelixFitter

/*! \class StiHelixFitter
  StiHelixFitter is a class that performs a fast and approximate
  fit to a collection of StiHit object to return the necessary
  parameters to initialize an StiKalmanTrack.  These parameters are:
  curvature, tanLambda, xCenter, yCenter, and z0.  For more on these
  parameters, please see documentation for StiKalmanTrack::initialize().
  <p>
  StiHelixFitter uses an instance of StFastCircleFitter and StFastLineFitter.
  A circle fit is first performed in the transverse plane, and then a line
  is performed in the two dimensional plane z vs s_xy, where s_xy is the
  pathlength along the circle, starting at the innermost point moving
  outwards.

  \author M.L. Miller (Yale Software)

*/

#ifndef StiHelixFitter_HH
#define StiHelixFitter_HH

//std
#include <iostream.h>
#include <vector>
using std::vector;

#include <math.h>

//SCL
#include "StFastCircleFitter.hh"
#include "StThreeVectorF.hh"

//Sti
#include "StFastLineFitter.h"
#include "StiHit.h"

class StiHelixFitter
{
public:
    ///For internal convenience.
    typedef vector<StiHit*> StiHitVector;

    StiHelixFitter();
    virtual ~StiHelixFitter();

    ///Return a boolean that signifies whether the information is valid.
    bool valid() const;
    
    ///Return the xCenter in global coordinates.
    double xCenter() const;
    
    ///Return the yCenter in global coordinates.
    double yCenter() const;
    
    ///Return the z reference point in global coordinates.
    double z0() const;

    ///Return the signed curvature. (k*h)
    double curvature() const;

    ///Return the value of tanLambda.
    double tanLambda() const;

    //action

    ///Full internal clear.
    void reset();

    bool fit(const StiHitVector&);

private:
    void calculateH(const StiHitVector&);
    
    StFastCircleFitter mCircleFitter;
    StFastLineFitter mLineFitter;
    //Store to represent the (0,0) point, avoid constructor calls
    StThreeVectorF mOrigin;
    //Store the fit result in a 3-vec to avoid constructor calls
    StThreeVectorF mCenter;

    //Store the sign of the curvature;
    double mH; 
    bool mValid;
    
};

//inlines

inline void StiHelixFitter::reset()
{
    mCircleFitter.clear();
    mLineFitter.clear();
    mValid=false;
}

inline double StiHelixFitter::xCenter() const
{
    return (mValid) ? mCircleFitter.xcenter() : DBL_MAX;
}

inline double StiHelixFitter::yCenter() const
{
    return (mValid) ? mCircleFitter.ycenter() : DBL_MAX;
}

inline double StiHelixFitter::z0() const
{
    return (mValid) ? mLineFitter.intercept() : DBL_MAX;
}

inline double StiHelixFitter::curvature() const
{
    return (mValid) ? 1./mCircleFitter.radius() * mH : DBL_MAX;
}

inline double StiHelixFitter::tanLambda() const
{
    return (mValid) ? mLineFitter.slope() : DBL_MAX;
}

inline bool StiHelixFitter::valid() const
{
    return mValid;
}

inline ostream& operator<<(ostream& os, const StiHelixFitter& f)
{
    return os << "valid: "<<f.valid()<<" xC: "<<f.xCenter()
	      <<" yC: "<<f.yCenter()<<" z0: "<<f.z0()
	      <<" curvature: "<<f.curvature()
	      <<" tanLambda: "<<f.tanLambda();
}


/*
  class CircleFitterInserter
  {
  public:
  CircleFitterInserter(StFastCircleFitter& ftr)
  : mFitter(ftr) {};
  CircleFitterInserter(const CircleFitterInserter& rhs)
  : mFitter(rhs.mFitter) {};
  
  void operator()(const StiHit*);
  
  private:
  StFastCircleFitter& mFitter;
  CircleFitterInserter(); //Not implemented    
  };
  

  //Non-members
  inline void CircleFitterInserter::operator()(const StiHit* hit)
  {
  mFitter.addPoint( hit->globalPosition().x(),
  hit->globalPosition().y() );
  }
  
  class LineFitterInserter
  {
  public:
  LineFitterInserter(StFastCircleFitter& cftr, StFastLineFitter& lftr)
  : mCircleFitter(cftr), mLineFitter(lftr), mS2d(0.) {};
  
  LineFitterInserter(const LineFitterInserter& rhs)
  : mCircleFitter(rhs.mCircleFitter),
  mLineFitter(rhs.mLineFitter), mS2d(0.) {};
  
  void reset() {mS2d=0.;}
  void operator()(const StiHit*);
  
  private:
  LineFitterInserter(); //Not implemented
  
  StFastCircleFitter& mCircleFitter;
  StFastLineFitter& mLineFitter;
  double mS2d;
  
  };
  
*/

#endif

