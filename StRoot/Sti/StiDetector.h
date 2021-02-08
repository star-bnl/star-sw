#ifndef STI_DETECTOR_H
#define STI_DETECTOR_H
#include <vector>
#include <string>
#include <math.h>
#include "Stiostream.h"
using std::string;
#include "Sti/Base/Named.h"
#include "Sti/StiPlacement.h"
#include "StiIsActiveFunctor.h"
#include "StDetectorDbMaker/StiTrackingParameters.h"

class StiMaterial;
class StiShape;
template<class T> class StiCompositeTreeNode;
class StiHitErrorCalculator;
class StiElossCalculator;
class StiDetector;

typedef std::vector<StiDetector*> StiDetVect;



/*!
   StiDetector represents a detector for the purposes of ITTF tracking.
   It contains all information about the geometry of the detector and
   the necessary physical properties for incorporating it in tracking.
 */
class StiDetector : public Named
{
    
public:

  friend class StiHit;

    // con/destructor
    StiDetector();
    virtual ~StiDetector();
    void reset();
    void unset(){;}
    int  splitIt(StiDetVect &vect,double thick=0.5,int nMax=20);
    
    // accessors
    bool isOn() const 			{ return on;}
    inline bool isActive(double dYlocal, double dZlocal) const {return (*isActiveFunctor)(dYlocal, dZlocal);}
    inline bool isActive()                               const {return isActiveFunctor->isActive();}
    bool isContinuousMedium() const 	{ return continuousMedium; }
    bool isDiscreteScatterer() const 	{ return discreteScatterer; }

    StiMaterial* getGas() const 	{ return gas; }
    StiMaterial* getMaterial() const 	{ return material; }

    StiShape* getShape() const { return shape; }
    StiPlacement* getPlacement() const 	{ return placement; }

    StiIsActiveFunctor* getIsActiveFunctor() {return isActiveFunctor;}

    // mutators
    void setIsOn(bool val) {on = val;}
    void setIsActive(StiIsActiveFunctor *val){ isActiveFunctor = val; }
    void setIsContinuousMedium(bool val) {continuousMedium = val;}
    void setIsDiscreteScatterer(bool val) {discreteScatterer = val;}

    void setGas(StiMaterial *val){ gas = val; }
    void setMaterial(StiMaterial *val){ material = val; }

    void setShape(StiShape *val){ shape = val; }
    void setPlacement(StiPlacement *val)
      { 
	placement = val; 
	_cos = cos(val->getNormalRefAngle());
	_sin = sin(val->getNormalRefAngle());
      }

    //action
    virtual void build(){}  //for now, build from SCL parsable ascii file

    virtual void copy(StiDetector &detector);
    
    //This is a bit of a hack, but we leave ourselves a reverse connection between
    // a detector and the tree node that it's stored on.
    void setTreeNode( StiCompositeTreeNode<StiDetector> * val) {mNode=val;}
    StiCompositeTreeNode<StiDetector> * getTreeNode() const {return mNode;}
    
    void setHitErrorCalculator(const StiHitErrorCalculator * calculator) {_hitErrorCalculator = calculator;}
    const StiHitErrorCalculator * getHitErrorCalculator() const {return _hitErrorCalculator;}

    void setGroupId(int id) {  _groupId = id;}
    int  getGroupId() const {return _groupId;}

    void setTrackingParameters(const StiTrackingParameters * pars) {_pars = pars;}
    const StiTrackingParameters * getTrackingParameters() const {return _pars;}

    friend ostream& operator<<(ostream&os, const StiDetector & det);

    void setKey(int index,int value)
      {
	switch (index)
	  {
	  case 1: _key1 = value; break;
	  case 2: _key2 = value; break;
	  }
      }

    int getKey(int index) const 
      {
	switch (index)
	  {
	  case 1: return _key1;
	  case 2: return _key2;
	  }
	return -1;
      }

    void setElossCalculator(StiElossCalculator * calculator)
      {
	_elossCalculator = calculator;
      }

    StiElossCalculator * getElossCalculator() const
      {
	return _elossCalculator;
      }

 protected:
    
    char mBeg[1];

    /// Toggle switch determining whether this detector is to be added to the detector tree.
    /// The detector is added if the switch is "true"
    bool on;    
    /// Functor used to calculate whether the posistion reached by a track is 
    /// to be considered within the active area of the detector, and
    /// is thus susceptible of providing hit information.
    StiIsActiveFunctor *isActiveFunctor; 
    /// Toggle switch determining whether this detector contains a continuous 
    /// medium (e.g. gas). If true, scatterer information is provided 
    /// by the gas material.
    bool continuousMedium;  
    /// Toggle switch determining whether the detector contains a discrete
    /// thin scatterer (e.g. a Si wafer). If true, scatter information provided
    /// by the material.
    bool discreteScatterer;   // is this a discrete scatterer?    (yes => scatterer given by "material" below)

    /// Hit Error Calculator for this detector
    const StiHitErrorCalculator * _hitErrorCalculator;
    
    /// Continuous scatter attributes.
    StiMaterial *gas;         
    /// Discrete scatterer attributes
    StiMaterial *material;   
    
    /// Physical Shape attribute of this detector or voloume
    StiShape     *shape;
    /// Physical position and orientation of this detector or volume.
    StiPlacement *placement;
    /// Pointer to the parent detector node.
    StiCompositeTreeNode<StiDetector>  * mNode;

    /// Convenience storage of cos(refAngle) 
    double _cos;
    /// Convenience storage of sin(refAngle)
    double _sin;
    /// Detector group identifier.
    int _groupId;
    const StiTrackingParameters * _pars;
    int _key1, _key2;

    StiElossCalculator * _elossCalculator;
    char mEnd[1];
};
#endif
