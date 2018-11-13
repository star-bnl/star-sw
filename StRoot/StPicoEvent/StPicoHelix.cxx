//
// StPicoHelix is a helix parametrization that uses ROOT TVector3
//

// C++ headers
#if !defined(ST_NO_NUMERIC_LIMITS)
#    include <limits>
#    if !defined(ST_NO_NAMESPACES)
         using std::numeric_limits;
#    endif
#endif

#define FOR_PICO_HELIX

// C++ headers
#include <float.h>

// PicoDst headers
#include "StPicoHelix.h"
#ifdef _VANILLA_ROOT_
#include "PhysicalConstants.h"
#else
#include "StarClassLibrary/PhysicalConstants.h" 
#endif

ClassImpT(StPicoHelix,double);

const Double_t StPicoHelix::NoSolution = 3.e+33;

//_________________
StPicoHelix::StPicoHelix() : mSingularity(false), mOrigin(0, 0, 0),
			     mDipAngle(0), mCurvature(0), mPhase(0),
			     mH(0), mCosDipAngle(0), mSinDipAngle(0),
			     mCosPhase(0), mSinPhase(0) {
  /*no-op*/
}

//_________________
StPicoHelix::StPicoHelix(Double_t c, Double_t d, Double_t phase,
			 const TVector3& o, Int_t h) {
  setParameters(c, d, phase, o, h);
}

//_________________
StPicoHelix::StPicoHelix(const StPicoHelix &h) {
  mSingularity = h.mSingularity;
  mOrigin = h.mOrigin;
  mDipAngle = h.mDipAngle;
  mCurvature = h.mCurvature;
  mPhase = h.mPhase;
  mH = h.mH;
  mCosDipAngle = h.mCosDipAngle;
  mSinDipAngle = h.mSinDipAngle;
  mCosPhase = h.mCosPhase;
  mSinPhase = h.mSinPhase;
}

//_________________
StPicoHelix::~StPicoHelix() { /* noop */ };

//_________________
void StPicoHelix::setParameters(Double_t c, Double_t dip, Double_t phase,
				const TVector3& o, Int_t h) {
  
  //
  //  The order in which the parameters are set is important
  //  since setCurvature might have to adjust the others.
  //
  mH = (h>=0) ? 1 : -1;    // Default is: positive particle
                           //             positive field
  mOrigin   = o;
  setDipAngle(dip);
  setPhase(phase);

  //
  // Check for singularity and correct for negative curvature.           
  // May change mH and mPhase. Must therefore be set last.
  //
  setCurvature(c);

  //
  // For the case B=0, h is ill defined. In the following we
  // always assume h = +1. Since phase = psi - h * pi/2
  // we have to correct the phase in case h = -1.
  // This assumes that the user uses the same h for phase
  // as the one he passed to the constructor.
  //
  if (mSingularity && mH == -1) {
    mH = +1;
    setPhase(mPhase-M_PI);
  }
}

//_________________
void StPicoHelix::setCurvature(Double_t val) {
  
  if (val < 0) {
    mCurvature = -val;
    mH = -mH;
    setPhase( mPhase+M_PI );
  }
  else {
    mCurvature = val;
  }

#ifndef ST_NO_NUMERIC_LIMITS
  if ( ::fabs(mCurvature) <= numeric_limits<Double_t>::epsilon() ) {
#else
  if ( ::fabs(mCurvature) <= static_cast<Double_t>(0) ) {
#endif
    mSingularity = true;  // straight line
  }
  else {
    mSingularity = false; // curved
  }
}

//_________________
void StPicoHelix::setPhase(Double_t val) {
  
  mPhase       = val;
  mCosPhase    = cos(mPhase);
  mSinPhase    = sin(mPhase);
  if ( ::fabs(mPhase) > M_PI) {
    mPhase = atan2(mSinPhase, mCosPhase);  // force range [-pi,pi]
  }
}

//_________________
void StPicoHelix::setDipAngle(Double_t val) {
  
  mDipAngle    = val;
  mCosDipAngle = cos(mDipAngle);
  mSinDipAngle = sin(mDipAngle);
}

//_________________
Double_t StPicoHelix::xcenter() const {
  
  if (mSingularity) {
    return 0;
  }
  else {
    return mOrigin.x()-mCosPhase/mCurvature;
  }
}

//_________________
Double_t StPicoHelix::ycenter() const {
  if (mSingularity) {
    return 0;
  }
  else {
    return mOrigin.y()-mSinPhase/mCurvature;
  }
}

//_________________
Double_t StPicoHelix::fudgePathLength(const TVector3& p) const {
  
  Double_t s;
  Double_t dx = p.x()-mOrigin.x();
  Double_t dy = p.y()-mOrigin.y();
    
  if (mSingularity) {
    s = (dy*mCosPhase - dx*mSinPhase)/mCosDipAngle;
  }
  else {
    s = atan2(dy*mCosPhase - dx*mSinPhase,
	      1/mCurvature + dx*mCosPhase+dy*mSinPhase)/
      (mH*mCurvature*mCosDipAngle);
  }
  return s;
}

//_________________
Double_t StPicoHelix::distance(const TVector3& p, Bool_t scanPeriods) const {
  return ( this->at( pathLength(p,scanPeriods) )-p ).Mag();
}

//_________________
Double_t StPicoHelix::pathLength(const TVector3& p, Bool_t scanPeriods) const {
  
  //
  //  Returns the path length at the distance of closest 
  //  approach between the helix and point p. 
  //  For the case of B=0 (straight line) the path length
  //  can be calculated analytically. For B>0 there is
  //  unfortunately no easy solution to the problem.
  //  Here we use the Newton method to find the root of the
  //  referring equation. The 'fudgePathLength' serves
  //  as a starting value.
  //
  
  Double_t s;
  Double_t dx = p.x()-mOrigin.x();
  Double_t dy = p.y()-mOrigin.y();
  Double_t dz = p.z()-mOrigin.z();

  if (mSingularity) {
    s = mCosDipAngle*(mCosPhase*dy-mSinPhase*dx) +
      mSinDipAngle*dz;
  }
  else {
#ifndef ST_NO_NUMERIC_LIMITS
    {
      using namespace units;
#endif
      const Double_t MaxPrecisionNeeded = micrometer;
      const Int_t    MaxIterations      = 100;

      //
      // The math is taken from Maple with C(expr,optimized) and
      // some hand-editing. It is not very nice but efficient.
      //
      Double_t t34 = mCurvature*mCosDipAngle*mCosDipAngle;
      Double_t t41 = mSinDipAngle*mSinDipAngle;
      Double_t t6, t7, t11, t12, t19;
      
      //
      // Get a first guess by using the dca in 2D. Since
      // in some extreme cases we might be off by n periods
      // we add (subtract) periods in case we get any closer.
      // 
      s = fudgePathLength(p);

      if (scanPeriods) {
      
	Double_t ds = period();
	Int_t    j;
	Int_t jmin = 0;
	Double_t d;
	Double_t dmin = ( at(s) - p).Mag() ;
      
	for(j=1; j<MaxIterations; j++) {
	  d = ( at(s+j*ds) - p ).Mag();
	  if ( d  < dmin) {
	    dmin = d;
	    jmin = j;
	  }
	  else {
	    break;
	  }
	} //for(j=1; j<MaxIterations; j++)
      
	for(j=-1; -j<MaxIterations; j--) {
	  d = ( at(s+j*ds ) - p ).Mag() ;
	  if ( d < dmin) {
	    dmin = d;
	    jmin = j;
	  }
	  else {
	    break;
	  }
	} //for(j=-1; -j<MaxIterations; j--)
      
	if (jmin) {
	  s += jmin*ds;
	}
      } //if (scanPeriods)
	    
      //
      // Newtons method:
      // Stops after MaxIterations iterations or if the required
      // precision is obtained. Whatever comes first.
      //
      Double_t sOld = s;
      for (Int_t i=0; i<MaxIterations; i++) {
	t6  = mPhase+s*mH*mCurvature*mCosDipAngle;
	t7  = cos(t6);
	t11 = dx-(1/mCurvature)*(t7-mCosPhase);
	t12 = sin(t6);
	t19 = dy-(1/mCurvature)*(t12-mSinPhase);
	s  -= (t11*t12*mH*mCosDipAngle-t19*t7*mH*mCosDipAngle -
	       (dz-s*mSinDipAngle)*mSinDipAngle)/
	  (t12*t12*mCosDipAngle*mCosDipAngle+t11*t7*t34 +
	   t7*t7*mCosDipAngle*mCosDipAngle +
	   t19*t12*t34+t41);
	if (fabs(sOld-s) < MaxPrecisionNeeded) break;
	sOld = s;
      } //for (Int_t i=0; i<MaxIterations; i++)
#ifndef ST_NO_NUMERIC_LIMITS
    }
#endif
  } //else
  return s;
}

//_________________
Double_t StPicoHelix::period() const {
  if (mSingularity) {
#ifndef ST_NO_NUMERIC_LIMITS
    return numeric_limits<Double_t>::max();
#else
    return DBL_MAX;
#endif
  }
  else {
    return fabs(2*M_PI/(mH*mCurvature*mCosDipAngle));
  }
}

//_________________
pair<Double_t, Double_t> StPicoHelix::pathLength(Double_t r) const {
  
  pair<Double_t,Double_t> value;
  pair<Double_t,Double_t> VALUE(999999999.,999999999.);
  //
  // The math is taken from Maple with C(expr,optimized) and
  // some hand-editing. It is not very nice but efficient.
  // 'first' is the smallest of the two solutions (may be negative)
  // 'second' is the other.
  //
  if (mSingularity) {
    Double_t t1 = mCosDipAngle*(mOrigin.x()*mSinPhase-mOrigin.y()*mCosPhase);
    Double_t t12 = mOrigin.y()*mOrigin.y();
    Double_t t13 = mCosPhase*mCosPhase;
    Double_t t15 = r*r;
    Double_t t16 = mOrigin.x()*mOrigin.x();
    Double_t t20 = -mCosDipAngle*mCosDipAngle*(2.0*mOrigin.x()*mSinPhase*mOrigin.y()*mCosPhase +
					       t12-t12*t13-t15+t13*t16);
    if (t20<0.) {
      return VALUE;
    }
    t20 = ::sqrt(t20);
    value.first  = (t1-t20)/(mCosDipAngle*mCosDipAngle);
    value.second = (t1+t20)/(mCosDipAngle*mCosDipAngle);
  }
  else {
    Double_t t1 = mOrigin.y()*mCurvature;
    Double_t t2 = mSinPhase;
    Double_t t3 = mCurvature*mCurvature;
    Double_t t4 = mOrigin.y()*t2;
    Double_t t5 = mCosPhase;
    Double_t t6 = mOrigin.x()*t5;
    Double_t t8 = mOrigin.x()*mOrigin.x();
    Double_t t11 = mOrigin.y()*mOrigin.y();
    Double_t t14 = r*r;
    Double_t t15 = t14*mCurvature;
    Double_t t17 = t8*t8;
    Double_t t19 = t11*t11;
    Double_t t21 = t11*t3;
    Double_t t23 = t5*t5;
    Double_t t32 = t14*t14;
    Double_t t35 = t14*t3;
    Double_t t38 = 8.0*t4*t6 - 4.0*t1*t2*t8 - 4.0*t11*mCurvature*t6 +
      4.0*t15*t6 + t17*t3 + t19*t3 + 2.0*t21*t8 + 4.0*t8*t23 -
      4.0*t8*mOrigin.x()*mCurvature*t5 - 4.0*t11*t23 -
      4.0*t11*mOrigin.y()*mCurvature*t2 + 4.0*t11 - 4.0*t14 +
      t32*t3 + 4.0*t15*t4 - 2.0*t35*t11 - 2.0*t35*t8;
    Double_t t40 = (-t3*t38);
    if (t40<0.) {
      return VALUE;
    }
    t40 = ::sqrt(t40);
	
    Double_t t43 = mOrigin.x()*mCurvature;
    Double_t t45 = 2.0*t5 - t35 + t21 + 2.0 - 2.0*t1*t2 -2.0*t43 - 2.0*t43*t5 + t8*t3;
    Double_t t46 = mH*mCosDipAngle*mCurvature;
	
    value.first = (-mPhase + 2.0*atan((-2.0*t1 + 2.0*t2 + t40)/t45))/t46;
    value.second = -(mPhase + 2.0*atan((2.0*t1 - 2.0*t2 + t40)/t45))/t46;

    //
    //   Solution can be off by +/- one period, select smallest
    //
    Double_t p = period();
    
    if ( ! std::isnan(value.first) ) {
      if ( ::fabs(value.first-p) < ::fabs(value.first) ) {
	value.first = value.first-p;
      }
      else if ( ::fabs(value.first+p) < ::fabs(value.first) ) {
	value.first = value.first+p;
      }
    } //if ( ! std::isnan(value.first) )
    
    if (! std::isnan(value.second)) {
      if ( ::fabs(value.second-p) < ::fabs(value.second) ) {
	value.second = value.second-p;
      }
      else if ( ::fabs(value.second+p) < ::fabs(value.second) ) {
	value.second = value.second+p;
      }
    } //if (! std::isnan(value.second))
  } //else
  
  if (value.first > value.second) {
    swap(value.first,value.second);
  }
  
  return(value);
}

//_________________
pair<Double_t, Double_t> StPicoHelix::pathLength(Double_t r, Double_t x, Double_t y) {
  Double_t x0 = mOrigin.x();
  Double_t y0 = mOrigin.y();
  mOrigin.SetX(x0-x);
  mOrigin.SetY(y0-y);
  pair<Double_t, Double_t> result = this->pathLength(r);
  mOrigin.SetX(x0);
  mOrigin.SetY(y0);
  return result;  
}

//________________
Double_t StPicoHelix::pathLength(const TVector3& r,
				 const TVector3& n) const {
  //
  // Vector 'r' defines the position of the center and
  // vector 'n' the normal vector of the plane.
  // For a straight line there is a simple analytical
  // solution. For curvatures > 0 the root is determined
  // by Newton method. In case no valid s can be found
  // the max. largest value for s is returned.
  //
  Double_t s;

  if (mSingularity) {
    Double_t t = n.z()*mSinDipAngle +
      n.y()*mCosDipAngle*mCosPhase -
      n.x()*mCosDipAngle*mSinPhase;
    if (t == 0) {
      s = NoSolution;
    }
    else {
      s = ((r - mOrigin)*n)/t;
    }
  }
  else {
    const Double_t MaxPrecisionNeeded = micrometer;
    const Int_t    MaxIterations      = 20;
        	
    Double_t A = mCurvature*((mOrigin - r)*n) -
      n.x()*mCosPhase - 
      n.y()*mSinPhase;
    Double_t t = mH*mCurvature*mCosDipAngle;
    Double_t u = n.z()*mCurvature*mSinDipAngle;
    
    Double_t a, f, fp;
    Double_t sOld = s = 0;  
    Double_t shiftOld = 0;
    Double_t shift;
    //  (cos(angMax)-1)/angMax = 0.1
    const Double_t angMax = 0.21;
    Double_t deltas = fabs(angMax/(mCurvature*mCosDipAngle));
    //  dampingFactor = exp(-0.5);
    //	Double_t dampingFactor = 0.60653;
    Int_t i;

    for (i=0; i<MaxIterations; i++) {
      a  = t*s+mPhase;
      Double_t sina = sin(a);
      Double_t cosa = cos(a);
      f = A + n.x()*cosa + n.y()*sina +	u*s;
      fp = -n.x()*sina*t + n.y()*cosa*t + u;
      
      if ( fabs(fp)*deltas <= fabs(f) ) { //too big step
	Int_t sgn = 1;
	if (fp<0.) sgn = -sgn;
	if (f <0.) sgn = -sgn;
	shift = sgn*deltas;
	if (shift<0) shift*=0.9;  // don't get stuck shifting +/-deltas
      }
      else {
	shift = f/fp;
      }
      s -= shift;
      shiftOld = shift;
      if ( ::fabs(sOld-s) < MaxPrecisionNeeded ) {
	break;
      }
      sOld = s;
    } //for (i=0; i<MaxIterations; i++)
    
    if (i == MaxIterations) {
      return NoSolution;
    }
  } //else 
  return s;
}

//_________________
pair<Double_t, Double_t> StPicoHelix::pathLengths(const StPicoHelix& h, Double_t minStepSize,
						  Double_t minRange) const {
  //
  //	Cannot handle case where one is a helix
  //  and the other one is a straight line.
  //
  if (mSingularity != h.mSingularity) {
    return pair<Double_t, Double_t>(NoSolution, NoSolution);
  }
    
  Double_t s1, s2;
    
  if (mSingularity) {
    //
    //  Analytic solution
    //
    TVector3 dv = h.mOrigin - mOrigin;
    TVector3 a(-mCosDipAngle*mSinPhase,
	       mCosDipAngle*mCosPhase,
	       mSinDipAngle);
    TVector3 b(-h.mCosDipAngle*h.mSinPhase,
	       h.mCosDipAngle*h.mCosPhase,
	       h.mSinDipAngle);
    Double_t ab = a*b;
    Double_t g  = dv*a;
    Double_t k  = dv*b;
    s2 = (k-ab*g)/(ab*ab-1.);
    s1 = g+s2*ab;
    return pair<Double_t, Double_t>(s1, s2);
  } //if (mSingularity)
  else {
    //
    //  First step: get dca in the xy-plane as start value
    //
    Double_t dx = h.xcenter() - xcenter();
    Double_t dy = h.ycenter() - ycenter();
    Double_t dd = ::sqrt(dx*dx + dy*dy);
    Double_t r1 = 1/curvature();
    Double_t r2 = 1/h.curvature();
        
    Double_t cosAlpha = (r1*r1 + dd*dd - r2*r2)/(2*r1*dd);
        
    Double_t s;
    Double_t x, y;
    if ( ::fabs(cosAlpha) < 1) {           // two solutions
      Double_t sinAlpha = sin(acos(cosAlpha));
      x = xcenter() + r1*(cosAlpha*dx - sinAlpha*dy)/dd;
      y = ycenter() + r1*(sinAlpha*dx + cosAlpha*dy)/dd;
      s = pathLength(x, y);
      x = xcenter() + r1*(cosAlpha*dx + sinAlpha*dy)/dd;
      y = ycenter() + r1*(cosAlpha*dy - sinAlpha*dx)/dd;
      Double_t a = pathLength(x, y);
      if ( h.distance(at(a)) < h.distance(at(s)) ) {
	s = a;
      }
    } //if ( ::fabs(cosAlpha) < 1)
    else {                              // no intersection (or exactly one)
      Int_t rsign = ((r2-r1) > dd ? -1 : 1); // set -1 when *this* helix is
      // completely contained in the other
      x = xcenter() + rsign*r1*dx/dd;
      y = ycenter() + rsign*r1*dy/dd;
      s = pathLength(x, y);
    } //else
        
    //
    //   Second step: scan in decreasing intervals around seed 's'
    //   minRange and minStepSize are passed as arguments to the method.
    //   They have default values defined in the header file.
    //
    Double_t dmin              = h.distance(at(s));
    Double_t range             = max(2*dmin, minRange);
    Double_t ds                = range/10;
    Double_t slast=-999999, ss, d;
    s1 = s - range/2.;
    s2 = s + range/2.;
    
    while (ds > minStepSize) {
      
      for ( ss=s1; ss<s2+ds; ss+=ds ) {
	d = h.distance(at(ss));
	if (d < dmin) {
	  dmin = d;
	  s = ss;
	}
	slast = ss;
      } //for ( ss=s1; ss<s2+ds; ss+=ds )
      
      //
      //  In the rare cases where the minimum is at the
      //  the border of the current range we shift the range
      //  and start all over, i.e we do not decrease 'ds'.
      //  Else we decrease the search intervall around the
      //  current minimum and redo the scan in smaller steps.
      //
      if (s == s1) {
	d = 0.8*(s2-s1);
	s1 -= d;
	s2 -= d;
      }
      else if (s == slast) {
	d = 0.8*(s2-s1);
	s1 += d;
	s2 += d;
      }
      else {           
	s1 = s-ds;
	s2 = s+ds;
	ds /= 10;
      }
    } //while (ds > minStepSize)
    return pair<Double_t, Double_t>(s, h.pathLength(at(s)));
  } //else
}

//_________________
void StPicoHelix::moveOrigin(Double_t s) {
  
  if (mSingularity) {
    mOrigin = at(s);
  }
  else {
    TVector3 newOrigin = at(s);
    Double_t newPhase = atan2(newOrigin.y() - ycenter(),
			      newOrigin.x() - xcenter());
    mOrigin = newOrigin;
    setPhase(newPhase);	        
  }
}

//_________________
Int_t operator== (const StPicoHelix& a, const StPicoHelix& b) {
  
  //
  // Checks for numerical identity only !
  //
  return ( a.origin()    == b.origin()    &&
	   a.dipAngle()  == b.dipAngle()  &&
	   a.curvature() == b.curvature() &&
	   a.phase()     == b.phase()     &&
	   a.h()         == b.h() );
}

//_________________
Int_t operator!= (const StPicoHelix& a, const StPicoHelix& b) {
  return !(a == b);
}

//_________________
std::ostream& operator<<(std::ostream& os, const StPicoHelix& h) {
  return os << '('
	    << "curvature = "  << h.curvature() << ", " 
	    << "dip angle = "  << h.dipAngle()  << ", "
	    << "phase = "      << h.phase()     << ", "  
	    << "h = "          << h.h()         << ", "    
	    << "origin = "     << h.origin().X()
	    << " , " << h.origin().Y() << " , " << h.origin().Z() << " "
	    << ')';
}



