#ifndef StiKalmanTrackNode_H
#define StiKalmanTrackNode_H 1
#include <iostream.h>
#include <stdlib.h>
#include <stdexcept>
#include <math.h>
///////#include "Exception.h"
#include "StiTrackNode.h"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StiKalmanTrackFinderParameters.h"

class StiHit;
class StiDetector;
class StiMaterial;
class StiPlanarShape;
class StiCylindricalShape;

/*! \class StiKalmanTrackNode
  Work class used to handle Kalman filter information while
  constructing track nodes.  A node may or may not own a hit
  depending whether it lies on a measurement layer where a hit
  was found. A node can have 0, 1, or many children. The canonical
  ordering of the nodes is outside-in. Children node should be
  at smaller radii.

  <p>  
  \author Claude A Pruneau
*/
class StiKalmanTrackNode : public StiTrackNode 
{
    
public:
    
    /// Resets the node to a "null" un-used state
    void reset();
    /// Sets the various attributes of this node based on the argument list.
    void set(
	     StiHit * hit,
	     const double alpha,
	     const double xRef,
	     const double xx[5], 
	     const double cc[15], 
	     const double dEdx,
	     const double chi2);
    /// Sets the Kalman state of this node equal to that of the given node. 
    void setState(const StiKalmanTrackNode * node);
    /// Extract state information from this node.
    void get(double& alpha,
	     double& xRef,
	     double x[5], 
	     double cc[15], 
	     double& dEdx,
	     double& chi2);

    /// Get the charge (sign) of the track at this node
    double getCharge() const;
    
    /// Convenience Method that returns the track momentum at this node
    StThreeVectorF getMomentumF() const;
    
    /// Convenience Method that returns the track momentum at this node
    /// in global coordinates.
    StThreeVectorF getGlobalMomentumF() const;

    StThreeVector<double> getMomentum() const;
    StThreeVector<double> getGlobalMomentum() const;

    /// Calculates and returns the momentum and error of the track at this node. The momentum is 
    /// in the local reference frame of this node.
    void getMomentum(double p[3], double e[6]=0) const;
    /// Calculates and returns the tangent of the track pitch angle at this node.
    double getCurvature() const;
    double getDipAngle() const;
    double getTanL() const;
    /// Calculates and returns the momentum of the track at this node.
    double getP() const;
    /// Calculates and returns the transverse momentum of the track at this node.
    double getPt() const;
    /// Calculates and returns the momentum and error of the track at this node in global coordinates.
    void getGlobalMomentum(double p[3], double e[6]=0) const;
    /// Set the attributes of this node as a copy of the given node.
    void setAsCopyOf(const StiKalmanTrackNode * node);

    /// Propagates a track encapsulated by the given node "p" to the given detector "tDet".
    int  propagate(StiKalmanTrackNode *p, const StiDetector * tDet);	//throw (Exception);

    /// Propagates a track encapsulated by the given node "p" to the given vertex
    void  propagate(const StiKalmanTrackNode *p, const StiHit * vertex);

    /// Evaluates, stores and returns the dedx associated with this node.
    /// Possible returned values are:
    /// > 0 : value of dedx
    /// -1  : pathlength was invalid or less than "0"
    /// -2  : no hit is associated with the node.
    /// -3  : invalid eloss data for this node.
    double  evaluateDedx();

    void propagate(double x);
    void propagateCylinder(double x);
    void propagateError();
		void propagateMCS(double density, double radThickness, double massHypo);

    /// Extrapolate the track parameters to radial position "x"  and return a point global coordinates along
    /// the track at that point.
    StThreeVector<double> getPointAt(double xk) const;

    double evaluateChi2(); 
    void updateNode(); //throw (Exception);
    void extendToVertex(); //throw (Exception);
    void rotate(double alpha); //throw ( Exception);
    void add(StiKalmanTrackNode * newChild);
    //void setTargetDet(const StiDetector * targetDet);
    //const StiDetector * getTargetDet();
  
    double getWindowY() const;
    double getWindowZ() const;

    double pitchAngle() const;
    double crossAngle() const;

    /// Return center of helix circle in global coordinates
    StThreeVector<double> getHelixCenter() const;

    void setError(pair<double, double> p);

    // static methods
    

    static void   setParameters(StiKalmanTrackFinderParameters *parameters);

    friend ostream& operator<<(ostream& os, const StiKalmanTrackNode& n);

    /// rotation angle of local coordinates wrt global coordinates
    double fAlpha;
    /// local X-coordinate of this track (reference plane)
    double fX;   
    /// local Y-coordinate of this track (reference plane)           
    double fP0; 
    /// local Z-coordinate of this track (reference plane)
    double fP1;
    /// (signed curvature)*(local X-coordinate of helix axis)
    double fP2;
    /// signed curvature [sign = sign(-qB)]
    double fP3;  
    /// tangent of the track momentum dip angle
    double fP4;
    
      
    /// covariance matrix of the track parameters
    double fC00;                       
    double fC10, fC11;                 
    double fC20, fC21, fC22;           
    double fC30, fC31, fC32, fC33;     
    double fC40, fC41, fC42, fC43, fC44;
    double fChi2;
    float  fdEdx;           // dE/dx 
    float  pathLength;


    // local error assigned to the y and z position of the hit.
    float  eyy,ezz;

    int hitCount;
    int nullCount;
    int contiguousHitCount;
    int contiguousNullCount;
    
    static double  getFieldConstant()         { return pars->field;}; 
    
 protected:   
    static bool  recurse;
    
    static StiKalmanTrackFinderParameters * pars;
    
    //const StiDetector * targetDet; // not persistent
    static int   shapeCode;
    static const StiDetector * det;
    static const StiPlanarShape * planarShape;
    static const StiCylindricalShape * cylinderShape;
    static StiMaterial * gas;
    static StiMaterial * prevGas;
    static StiMaterial * mat;
    static StiMaterial * prevMat;
    static double x1,x2,y1,z1,dx,r1,r2,c1,c2,c1sq,c2sq,x0,y0;
    static double radThickness, density;
    static double gasDensity,matDensity,gasRL,matRL;
    static bool   useCalculatedHitError;
};

inline double StiKalmanTrackNode::getCurvature() const
{
  return fP3;
}

inline double StiKalmanTrackNode::getDipAngle() const
{
  return atan(fP4);
}

inline StThreeVector<double> StiKalmanTrackNode::getMomentum() const
{
  double pt, sinPhi;
  pt = getPt();
  sinPhi = fP3*fX-fP2;
  double ss = sinPhi*sinPhi;
  if (ss>1.)
    {
      throw runtime_error("StiKalmanTrackNode::getMomentum() - ERROR - sinPhi*sinPhi>1.");
    }  
  return StThreeVector<double>(pt*sqrt(1-ss),pt*sinPhi,pt*fP4);
}

inline StThreeVectorF StiKalmanTrackNode::getMomentumF() const
{
  double pt, sinPhi;
  pt = getPt();
  sinPhi = fP3*fX-fP2;
  double ss = sinPhi*sinPhi;
  if (ss>1.)
    {
      throw runtime_error("StiKalmanTrackNode::getMomentumF() - ERROR - sinPhi*sinPhi>1.");
    }  
  return StThreeVectorF(pt*sqrt(1-ss),pt*sinPhi,pt*fP4);
}

inline StThreeVector<double> StiKalmanTrackNode::getGlobalMomentum() const
{
  StThreeVector<double> p = getMomentum();
  p.rotateZ(fAlpha);
  return StThreeVector<double>(p);
}

inline StThreeVectorF StiKalmanTrackNode::getGlobalMomentumF() const
{
  StThreeVectorF p = getMomentumF();
  p.rotateZ(fAlpha);
  return StThreeVectorF(p);
}


inline double StiKalmanTrackNode::getCharge() const
{
  return (pars->field*fP3 > 0) ? -1. : 1.;
}



inline double StiKalmanTrackNode::getTanL() const
{
  return fP4;
}

inline double StiKalmanTrackNode::pitchAngle() const
{
  return atan(fP4);
}

inline double StiKalmanTrackNode::crossAngle() const
{
  return asin(fP3*fX-fP2);
}



inline void StiKalmanTrackNode::setError(pair<double, double> p)
{
  eyy = p.first*p.first;
  ezz = p.second*p.second;
}

/*! Calculate/return the track transverse momentum
  <p>
  Calculate the track transverse momentum in GeV/c based on this node's track parameters.
  <p>
  The momentum is calculated based on the track curvature held by this node. A minimum
  curvature of 1e-12 is allowed. 
*/
inline double StiKalmanTrackNode::getPt() const
{
  double c;
  c = fabs(fP3);
  if (c<1e-12) 
    return 0.003e12*pars->field;
  else
    return 0.003*pars->field/c;
}

/*! Calculate/return the track momentum
  <p>
  Calculate the track  momentum in GeV/c based on this node's track parameters.
  <p>
  The momentum is calculated based on the track curvature held by this node. A minimum
  curvature of 1e-12 is allowed. 
*/
inline double StiKalmanTrackNode::getP() const
{
  double c;
  c = fabs(fP3);
  if (c<1e-12) 
    return 0.003e12*pars->field;
  else
    return 0.003*pars->field*(1.+fP4*fP4)/c;
}


//stl helper functor

struct StiKTNXLessThan
{
    bool operator()(const StiKalmanTrackNode& lhs, const StiKalmanTrackNode& rhs) const;
};

struct StreamX {
    void operator()(const StiKalmanTrackNode& node) {
	cout <<node.fX<<endl;
    }
};

#endif

