/***************************************************************************
 *
 * $Id: StHelixD.hh,v 1.1 1999/01/30 03:59:02 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 * Remarks:   This is a 'handmade' specialisation of StHelix
 *            with StThreeVector<T> replaced by StThreeVectorD
 *            and pair<T, T> replaced by pairD.
 *            This code contains no templates.
 *
 ***************************************************************************
 *
 * $Log: StHelixD.hh,v $
 * Revision 1.1  1999/01/30 03:59:02  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.2  1999/03/02 19:47:43  ullrich
 * Added method to find dca between two helices
 *
 * Revision 1.1  1999/01/30 03:59:02  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.2  1999/01/25 12:44:41  ullrich
 * Minor changes in pairD to cope with class browser.
 *
 * Revision 1.1  1999/01/23 00:27:51  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_HELIX_D_HH
#define ST_HELIX_D_HH

#include <math.h>
#include "StThreeVectorD.hh"

struct pairD 
{
    double first;
    double second;
    pairD(double, double);
    pairD();
};

inline pairD::pairD(double a, double b) : first(a), second(b) {}
inline pairD::pairD() : first(0), second(0) {}

class StHelixD 
#ifdef __ROOT__
 : public TObject 
#endif
{
public:
    // curvature, dip angle, phase, origin, h
    StHelixD(double c, double dip, double phase,
	     const StThreeVectorD& o, int h=-1);
    
    StHelixD();   
    virtual ~StHelixD();
    // StHelixD(const StHelixD&);		// use default
    // StHelixD& operator=(const StHelixD&);	// use default

    double       dipAngle()   const;           
    double       curvature()  const;	// 1/R in xy-plane
    double       phase()      const;	// aziumth in xy-plane measured from ring center
    double       xcenter()    const;	// x-center of circle in xy-plane
    double       ycenter()    const;	// y-center of circle in xy-plane
    int          h()          const;	// -sign(q*B);
    
    const StThreeVectorD& origin() const;	// starting point

    void setParameters(double c, double dip, double phase, const StThreeVectorD& o, int h);
    
    double       x(double s)  const;
    double       y(double s)  const;
    double       z(double s)  const;

    StThreeVectorD  at(double s) const;

    // returns period length of helix
    pairD pathLength(double r)   const;
    
    // path length at given r (cylindrical r)
    pairD        pathLength(double r)   const;
    
    // path length at distance of closest approach to a given point
    double       pathLength(const StThreeVectorD& p) const;
    
    // path length at intersection with plane
    double       pathLength(const StThreeVectorD& r,
			    const StThreeVectorD& n) const;

    // path lengths at dca between two helices 
    pairD        pathLengths(const StHelixD&) const;
    
    // minimal distance between point and helix
    int         valid() const;
    
    // checks for valid parametrization
    int          valid() const;
    
    // move the origin along the helix to s which becomes then s=0
    virtual void moveOrigin(double s);
    
protected:
    void setCurvature(double);	// performs also various checks   
    void setPhase(double);	        
    void setDipAngle(double);
    
    // value of S where distance in x-y plane is minimal
    double fudgePathLength(const StThreeVectorD&) const;
    
protected:
    int                    mSingularity;	// true for straight line case (B=0)
    StThreeVectorD         mOrigin;
    double                 mDipAngle;
    double                 mCurvature;
    double                 mPhase;
    int                    mH;			// -sign(q*B);

    double                 mCosDipAngle;
    double                 mSinDipAngle;
    double                 mCosPhase;
    double                 mSinPhase;
#ifdef __ROOT__
    ClassDef(StHelixD,1)
#endif
};

//
//     Non-member functions
//
int operator== (const StHelixD&, const StHelixD&);
int operator!= (const StHelixD&, const StHelixD&);
ostream& operator<<(ostream&, const StHelixD&);

//
//     Inline functions
//
inline int StHelixD::h() const {return mH;}

inline double StHelixD::dipAngle() const {return mDipAngle;}

inline double StHelixD::curvature() const {return mCurvature;}

inline double StHelixD::phase() const {return mPhase;}

inline double StHelixD::x(double s) const
{
    if (mSingularity)
	return mOrigin.x() - s*mCosDipAngle*mSinPhase;
    else
	return mOrigin.x() + (cos(mPhase + s*mH*mCurvature*mCosDipAngle)-mCosPhase)/mCurvature;
}
 
inline double StHelixD::y(double s) const
{
    if (mSingularity)
	return mOrigin.y() + s*mCosDipAngle*mCosPhase;
    else
	return mOrigin.y() + (sin(mPhase + s*mH*mCurvature*mCosDipAngle)-mSinPhase)/mCurvature;
}

inline double StHelixD::z(double s) const
{
    return mOrigin.z() + s*mSinDipAngle;
}

inline const StThreeVectorD& StHelixD::origin() const {return mOrigin;}

inline StThreeVectorD StHelixD::at(double s) const
{
    return StThreeVectorD(x(s), y(s), z(s));
}

inline double StHelixD::pathLength(double x, double y) const
{
    return fudgePathLength(StThreeVectorD(x, y, 0));
}

#endif
