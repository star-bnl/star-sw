#ifndef STI_DETECTOR_H
#define STI_DETECTOR_H
#include <vector>
#include <string>
#include <math.h>
#include <iostream>
using std::string;
#include "Sti/Base/Named.h"
#include "Sti/StiPlacement.h"
#include "StiIsActiveFunctor.h"

class StiMaterial;
class StiShape;
template<class T> class StiCompositeTreeNode;
class StiHitErrorCalculator;

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
    
    // accessors
    bool isOn() const {return on;}
    inline bool isActive(double dYlocal, double dZlocal) const;
    inline bool isActive() const;
    bool isContinuousMedium() const { return continuousMedium; }
    bool isDiscreteScatterer() const { return discreteScatterer; }

    StiMaterial* getGas() const { return gas; }
    StiMaterial* getMaterial() const { return material; }

    StiShape* getShape() const { return shape; }
    StiPlacement* getPlacement() const { return placement; }

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
    
    void setHitErrorCalculator(const StiHitErrorCalculator * calculator);
    const StiHitErrorCalculator * getHitErrorCalculator() const;

    void setGroupId(int id);
    int  getGroupId() const;

    friend ostream& operator<<(ostream&os, const StiDetector & det);
 protected:
    
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

};

inline void StiDetector::setHitErrorCalculator(const StiHitErrorCalculator * calculator)
{
  _hitErrorCalculator = calculator;
}

inline const StiHitErrorCalculator * StiDetector::getHitErrorCalculator() const
{
  return _hitErrorCalculator;
}


inline void StiDetector::setGroupId(int id)
{
  _groupId = id;
}

inline int  StiDetector::getGroupId() const
{
  return _groupId;
}

inline bool StiDetector::isActive(double dYlocal, double dZlocal) const
{
  return (*isActiveFunctor)(dYlocal, dZlocal);
} // isActive

inline bool StiDetector::isActive() const 
{
  return isActive(placement->getNormalYoffset(), placement->getZcenter());
} // isActive



#endif
