#ifndef StiKalmanTrackNode_H
#define StiKalmanTrackNode_H 1
#include <iostream.h>
#include <stdlib.h>
#include <stdexcept>
#include <math.h>
#include "StiTrackNode.h"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StiKalmanTrackFinderParameters.h"
#include "StiShape.h"
#include "StiPlanarShape.h"
#include "StiCylindricalShape.h"
#include "StiPlacement.h"
#include "StiHit.h"
#include "StiMaterial.h"
class StiMaker;
class StiDetector;
class StiMaterial;
class Messenger;
class StiElossCalculator;
  
typedef enum {
  kFailed = -1,         // could not find intersection
  kHit,                                
  kEdgePhiPlus, kEdgeZminus, kEdgePhiMinus, kEdgeZplus, 
  kMissPhiPlus, kMissZminus, kMissPhiMinus, kMissZplus
} StiIntersection;
  

/*! \class StiKalmanTrackNode
  Work class used to handle Kalman filter information while
  constructing track nodes.  A node may or may not own a hit
  depending whether it lies on a measurement layer where a hit
  was found. A node can have 0, 1, or many children. 
  Nodes are nominally sequenced outside-in i.e. with decreasing 
  radius (or independent variable). The order can however be reversed.
  In anycase, the order should always be monotonically increasing 
  or decreasing.
  \author Claude A Pruneau
*/
class StiKalmanTrackNode : public StiTrackNode 
{
public:
  //StiKalmanTrackNode();
  //~StiKalmanTrackNode();
  const StiKalmanTrackNode& operator=(const StiKalmanTrackNode&node);  
  
  double mcs2(double relRadThickness, double beta2, double p2);
  /// Resets the node to a "null" un-used state
  void reset();
  /// Initialize this node with the given hit information
  void initialize(StiHit*h,double alpha, double eta, double curvature, double tanl);
  
  // Sets the various attributes of this node based on the argument list.
  //void set(StiHit * hit, const double alpha,const double xRef,const double xx[5], const double cc[15], const double dEdx, const double chi2);
  /// Sets the Kalman state of this node equal to that of the given node. 
  void setState(const StiKalmanTrackNode * node);
  // Extract state information from this node.
  void get(double& alpha, double& xRef, double x[5], double cc[15], double& chi2);
  /// Get the charge (sign) of the track at this node
  double getCharge() const;
  /// Convenience Method that returns the track momentum at this node
  StThreeVectorF getMomentumF() const;
  /// Convenience Method that returns the track momentum at this node
  /// in global coordinates.
  StThreeVectorF getGlobalMomentumF() const;
  StThreeVector<double> getMomentum() const;
  StThreeVector<double> getGlobalMomentum() const;
  void setDetector(const StiDetector * detector);
  const StiDetector * getDetector() const;
  /// Calculates and returns the momentum and error of the track at this node. The momentum is 
  /// in the local reference frame of this node.
  void getMomentum(double p[3], double e[6]=0) const;
  /// Calculates and returns the tangent of the track pitch angle at this node.
  double getCurvature() const;
  void setCurvature(double curvature);
  double getDipAngle() const;
  double getTanL() const;
  /// Calculates and returns the momentum of the track at this node.
  double getP() const;
  /// Calculates and returns the transverse momentum of the track at this node.
  double getPt() const;
  
  double getRefPosition() const 
    {
      return _refX;
    }
  
  double getRefAngle() const 
    {
      return _alpha;
    }
  
  double getX() const
    {
      return _x;
    }
  double getY() const
    {
      return _p0;
    }
  
  double getZ() const
    {
      return _p1;
    }
  
  double getEta() const
    {
      return _p2;
    }
  double getChi2() const
    {
      return _chi2;
    }
  void setChi2(double chi2)
    {
      _chi2 = chi2;
    }
  StThreeVector<double>getPoint() const;
  StThreeVector<double>getGlobalPoint() const;
  /// Calculates and returns the momentum and error of the track at this node in global coordinates.
  void getGlobalMomentum(double p[3], double e[6]=0) const;
  /// Set the attributes of this node as a copy of the given node.
  void setAsCopyOf(const StiKalmanTrackNode * node);
  
  /// Propagates a track encapsulated by the given node "p" to the given detector "tDet".
  int  propagate(StiKalmanTrackNode *p, const StiDetector * tDet);	//throw (Exception);
  
  /// Propagates a track encapsulated by the given node "p" to the given vertex
  bool propagate(const StiKalmanTrackNode *p, StiHit * vertex);
  
  /// Evaluates, stores and returns the dedx associated with this node.
  /// Possible returned values are:
  /// > 0 : value of dedx
  /// -1  : pathlength was invalid or less than "0"
  /// -2  : no hit is associated with the node.
  /// -3  : invalid eloss data for this node.
  double  evaluateDedx();
  
  int  locate(StiPlacement*place,StiShape*sh);
  int  propagate(double x,int option);
  void propagateError();
  void propagateMCS(StiKalmanTrackNode * previousNode, const StiDetector * tDet);
  
  /// Extrapolate the track parameters to radial position "x"  and return a point global coordinates along
  /// the track at that point.
  StThreeVector<double> getPointAt(double xk) const;
  
  void nudge();
  double evaluateChi2(const StiHit *hit); 
  void updateNode(); 
  void rotate(double alpha); 
  void add(StiKalmanTrackNode * newChild);
  double getField()  const;
  int    getHelicity()  const;
  double getPhase()   const;
  double getWindowY() const;
  double getWindowZ() const;
  double pitchAngle() const;
  double crossAngle() const;
  double sinCrossAngle() const;
  double pathlength() const;
  double pathLToNode(const StiKalmanTrackNode * const oNode);
  StThreeVectorD* getLengths(StiKalmanTrackNode *nextNode);

  double length(const StThreeVector<double>& delta, double curv);
  double getDedx() const;
  double nice(double angle) const;
  /// Return center of helix circle in global coordinates
  StThreeVector<double> getHelixCenter() const;
  void setError(pair<double, double> p);
  static void   setParameters(StiKalmanTrackFinderParameters *parameters);
  friend ostream& operator<<(ostream& os, const StiKalmanTrackNode& n);

  double getX0() const;
  double getGasX0() const;
  double getDensity() const;
  double getGasDensity() const;

  /// rotation angle of local coordinates wrt global coordinates



  
  double _alpha;
  double _cosAlpha;
  double _sinAlpha;
  /// sine and cosine of cross angle
  double _sinCA;
  double _cosCA;
  /// local X-coordinate of this track (reference plane)
  double _refX;
  double _x;   
  /// local Y-coordinate of this track (reference plane)           
  double _p0; 
  /// local Z-coordinate of this track (reference plane)
  double _p1;
  /// (signed curvature)*(local X-coordinate of helix axis)
  double _p2;
  /// signed curvature [sign = sign(-qB)]
  double _p3;  
  /// tangent of the track momentum dip angle
  double _p4;
  /// covariance matrix of the track parameters
  double _c00;                       
  double _c10, _c11;                 
  double _c20, _c21, _c22;           
  double _c30, _c31, _c32, _c33;     
  double _c40, _c41, _c42, _c43, _c44;
  double _chi2;
  float  eyy,ezz;
  short int hitCount;
  short int nullCount;
  short int contiguousHitCount;
  short int contiguousNullCount;

  static StiMaker * maker;
  
 protected:   
  static const StiElossCalculator * _elossCalculator;
  const StiDetector * _detector;
  static Messenger &  _messenger;

  static bool  recurse;
  static StiKalmanTrackFinderParameters * pars;
  static int   shapeCode;
  static const StiDetector * det;
  static const StiPlanarShape * planarShape;
  static const StiCylindricalShape * cylinderShape;
  static StiMaterial * gas;
  static StiMaterial * prevGas;
  static StiMaterial * mat;
  static StiMaterial * prevMat;
  static double x0,y0, dx;
  static double x1,y1,z1,cosCA1,sinCA1;
  static double x2,y2,z2,cosCA2,sinCA2;
  static double sumSin, sinCA1plusCA2, sumCos;
  static double radThickness, density;
  static double gasDensity,matDensity,gasRL,matRL;
  static bool   useCalculatedHitError;
};

inline void StiKalmanTrackNode::reset()
{ 
  StiTrackNode::reset();
  _cosAlpha = 1.;
  _alpha=_sinAlpha=_sinCA=_cosCA=_refX=_x=_p0=_p1=_p2=_p3=_p4=0.;
  // diagonal error set to 1
  _c00=_c11=_c22=_c33=_c44=1.;
  // off diagonal set to zero
  _c10=_c20=_c21=_c30=_c31=_c32=_c40=_c41=_c42=_c43=0.;
  _chi2=eyy=ezz=0.;
  hitCount=nullCount=contiguousHitCount=contiguousNullCount = 0;
	_detector = 0;
}

inline double StiKalmanTrackNode::nice(double angle) const
{ 
  if (angle < -M_PI) angle += 2*M_PI;
  if (angle >= M_PI) angle -= 2*M_PI;
  return angle;
}

inline double StiKalmanTrackNode::getCurvature() const
{
  return _p3;
}

inline double StiKalmanTrackNode::getDipAngle() const
{
  return atan(_p4);
}

inline StThreeVector<double> StiKalmanTrackNode::getMomentum() const
{
  double pt = getPt();
  return StThreeVector<double>(pt*_cosCA,pt*_sinCA,pt*_p4);
}

inline StThreeVectorF StiKalmanTrackNode::getMomentumF() const
{
  double pt = getPt();
  return StThreeVectorF(pt*_cosCA,pt*_sinCA,pt*_p4);
}

inline StThreeVector<double> StiKalmanTrackNode::getGlobalMomentum() const
{
  StThreeVector<double> p = getMomentum();
  p.rotateZ(_alpha);
  return StThreeVector<double>(p);
}

inline StThreeVectorF StiKalmanTrackNode::getGlobalMomentumF() const
{
  StThreeVectorF p = getMomentumF();
  p.rotateZ(_alpha);
  return StThreeVectorF(p);
}

inline double StiKalmanTrackNode::getCharge() const
{
  return (pars->field*_p3 > 0) ? -1. : 1.;
}

inline double StiKalmanTrackNode::getTanL() const
{
  return _p4;
}

inline int StiKalmanTrackNode::getHelicity()  const
{
  return (_p3 < 0) ? 1 : -1;
}


inline double StiKalmanTrackNode::pitchAngle() const
{
  return atan(_p4);
}

inline double StiKalmanTrackNode::sinCrossAngle() const
{
  return _sinCA;
}

inline double StiKalmanTrackNode::crossAngle() const
{
  return asin(_sinCA);
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
  double curvature;
  curvature = fabs(_p3);
  if (curvature<1e-12) 
    return 0.003e12*pars->field;
  else
    return 0.003*pars->field/curvature;
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
  return (getPt()*sqrt(1.+_p4*_p4));
}

inline double StiKalmanTrackNode::mcs2(double relRadThickness, double beta2, double p2)
{
  return 14.1*14.1*relRadThickness/(beta2*p2*1e6);
}

//stl helper functor

struct StiKTNXLessThan
{
    bool operator()(const StiKalmanTrackNode& lhs, const StiKalmanTrackNode& rhs) const;
};

struct StreamX 
{
  void operator()(const StiKalmanTrackNode& node) 
  {
    cout <<node._x<<endl;
  }
};

inline int StiKalmanTrackNode::locate(StiPlacement*place,StiShape*sh)
{
  int position;
  double yOff, yAbsOff, detHW, detHD,edge,innerY, outerY, innerZ, outerZ, zOff, zAbsOff;
  
  yOff = _p0 - place->getNormalYoffset();
  yAbsOff = fabs(yOff);
  zOff = _p1 - place->getZcenter();
  zAbsOff = fabs(zOff);
  switch (sh->getShapeCode())
    {
    case kPlanar:
      {
	planarShape = static_cast<StiPlanarShape *>(sh);
	detHW = planarShape->getHalfWidth();
	detHW = planarShape->getHalfWidth();
	detHD = planarShape->getHalfDepth();
	edge  = 4.;//shape->getEdgeHalfWidth();
	break;
      }
    case kCylindrical:
      {
	StiCylindricalShape * cylinderShape = static_cast<StiCylindricalShape *>(sh);
	detHW = 100.; // will never be outside
	detHD = cylinderShape->getHalfDepth();
	edge  = 4.;//shape->getEdgeHalfWidth();
	break;
      }
    default:
      {
	throw logic_error("SKTN::locate() - ERROR - Invalid detector shape code");
      }
    }
  innerY = detHW - edge;
  outerY = innerY + 2*edge;
  innerZ = detHD - edge;
  outerZ = innerZ + 2*edge;
  if (yAbsOff<innerY && zAbsOff<innerZ)
    position = kHit; 
  else if (yAbsOff>outerY && (yAbsOff-outerY)>(zAbsOff-outerZ))
    // outside detector to positive or negative y (phi)
    position = yOff>0 ? kMissPhiPlus : kMissPhiMinus;
  else if (zAbsOff>outerZ && (zAbsOff-outerZ)>(yAbsOff-outerY))
    // outside detector to positive or negative z (west or east)
    position = zOff>0 ? kMissZplus : kMissZminus;
  else if ((yAbsOff-innerY)>(zAbsOff-innerZ))
    // positive or negative phi edge
    position = yOff>0 ? kEdgePhiPlus : kEdgePhiMinus;
  else
    // positive or negative z edge
    position = zOff>0 ? kEdgeZplus : kEdgeZminus;
  return position;
}

inline StThreeVector<double> StiKalmanTrackNode::getPoint() const
{
  return StThreeVector<double>(_x,_p0,_p1);
}

inline StThreeVector<double> StiKalmanTrackNode::getGlobalPoint() const
{
  return StThreeVector<double>(_cosAlpha*_x-_sinAlpha*_p0, _sinAlpha*_x+_cosAlpha*_p0, _p1);
}

inline double StiKalmanTrackNode::pathlength() const
{

  //returns pathlength within detector volume
    const StiDetector * det = getDetector();
    if (!det) return -1.; 
    double thickness = det->getShape()->getThickness();
    return (thickness*sqrt(1.+_p4*_p4)) / _cosCA;
}

///Return the radiation length (in cm) of the 
///the detector volume at this node.
inline double StiKalmanTrackNode::getX0() const
{
  const StiDetector * det = getDetector();
  if (!det)
    return 0.;
  return det->getMaterial()->getX0();
}

///Return the radiation length (in cm) of the gas
///surrounding the detector volume at this node.
inline double StiKalmanTrackNode::getGasX0() const
{
  const StiDetector * det = getDetector();
  if (!det)
    return 0.;
  return det->getGas()->getX0();
}

inline double StiKalmanTrackNode::getDensity() const
{
  const StiDetector * det = getDetector();
  if (!det)
    return 0.;
  return det->getMaterial()->getDensity();
}

inline double StiKalmanTrackNode::getGasDensity() const
{
  const StiDetector * det = getDetector();
  if (!det)
    return 0.;
  return det->getGas()->getDensity();
}


inline StThreeVectorD* StiKalmanTrackNode::getLengths(StiKalmanTrackNode* nextNode)
{
  double x1=pathlength()/2.;
  double x3=nextNode->pathlength()/2.;
  double x2=pathLToNode(nextNode);
  if (x2> (x1+x3)) x2=x2-x1-x3;
  else x2=0;

  return new StThreeVectorD(x1/getX0(),
			    x2/getDetector()->getMaterial()->getX0(), 
			    x3/nextNode->getX0());
}

inline double StiKalmanTrackNode::getDedx() const
{
  double de=_hit->getEloss();
  double dx=pathlength();
  if(dx>0 && de>0) return de/dx;
  return -1;
}

inline const StiDetector * StiKalmanTrackNode::getDetector() const 
{
  if (_hit)
    return _hit->detector();
  else 
    return _detector;
}

inline void StiKalmanTrackNode::setDetector(const StiDetector * detector)
{
  _detector = detector;
}

inline void StiKalmanTrackNode::setCurvature(double curvature)
{
  _p3=curvature;
}

inline  void StiKalmanTrackNode::initialize(StiHit*h,double alpha, double eta, double curvature, double tanl)
{
  //cout << "StiKalmanTrackNode::initialize(...) -I- Started"<<endl;
  reset();
  _hit     = h;
  _refX    = h->detector()->getPlacement()->getNormalRadius();
  _x       = h->x();
  _alpha   = alpha;
  _cosAlpha = cos(alpha);
  _sinAlpha = sin(alpha);
  _p0      = h->y();
  _p1      = h->z();
  _p2      = eta;
  _p3      = curvature;
  _p4      = tanl;
  _sinCA   = _p3*_x-_p2;
  if (fabs(_sinCA)>1.) 
    {
      cout << "SKTN::initialize() -E- fabs(_sinCA)>1."<<endl;
      throw runtime_error("SKTN::initialize() - ERROR - fabs(_sinCA)>1.");
    }
  _cosCA   = sqrt(1.-_sinCA*_sinCA);
  //cout << "StiKalmanTrackNode::initialize(...) -I- Done"<<endl;
};


inline const StiKalmanTrackNode& StiKalmanTrackNode::operator=(const StiKalmanTrackNode & n)
{
  children.clear();
  parent     = n.parent;
  _detector  = n._detector;
  _hit       = n._hit;
  _alpha     = n._alpha;
  _cosAlpha = n._cosAlpha;
  _sinAlpha = n._sinAlpha;
  _sinCA = n._sinCA;
  _cosCA = n._cosCA;
  _refX = n._refX;
  _x    = n._x;   
  _p0   = n._p0; 
  _p1   = n._p1;
  _p2   = n._p2;
  _p3   = n._p3;  
  _p4   = n._p4;
  _c00  = n._c00;                       
  _c10  = n._c10;
  _c11  = n._c11;                 
  _c20  = n._c20;
  _c21  = n._c21;
  _c22  = n._c22;           
  _c30  = n._c30;
  _c31  = n._c31;
  _c32  = n._c32;
  _c33  = n._c33; 
  _c40  = n._c40;
  _c41  = n._c41;
  _c42  = n._c42;
  _c43  = n._c43;
  _c44  = n._c44;
  _chi2 = n._chi2;
  eyy   = n.eyy;
  ezz   = n.ezz;
  hitCount = n.hitCount;
  nullCount = n.nullCount;
  contiguousHitCount = n.contiguousHitCount;
  contiguousNullCount = n.contiguousNullCount;
  _detector = n._detector;  
  return *this;
}

#endif

