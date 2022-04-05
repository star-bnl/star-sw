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
#include "TString.h"

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
    int  splitIt(StiDetVect &vect,double thick=0.2,int nMax=20);
    
    // accessors
    bool isOn() const 				{ return true;}
    inline bool isActive(double dYlocal, double dZlocal) const {return (*isActiveFunctor)(dYlocal, dZlocal);}
    inline bool isActive()    const 		{ return isActiveFunctor->isActive();}
    bool isDiscreteScatterer() const 		{ return 0; }
     int insideL(const double xl[3],int mode=1,double fakt=1) const ;
     int insideG(const double xl[3],int mode=1,double fakt=1) const ;

//		Gas is under detector
    StiMaterial* getGas() const 		{ assert(gas); return gas; }
    StiMaterial* getMaterial() const 		{ assert(material) ;return material; }
    double getCos() const 			{ return _cos; }
    double getSin() const 			{ return _sin; }

    StiShape* getShape() const 			{ return shape; }
    StiPlacement* getPlacement() const 		{ return placement; }

    StiIsActiveFunctor* getIsActiveFunctor() {return isActiveFunctor;}

    // mutators
    void setIsOn(bool ) {;}
    void setIsActive(StiIsActiveFunctor *val)	{ isActiveFunctor = val; }
    void setIsContinuousMedium(bool ) 		{;}
    void setIsDiscreteScatterer(bool) 	        {;}

    void setGas(StiMaterial *val)		{ gas = val; }
    void setMaterial(StiMaterial *val)		{ material = val; }

    void setShape(StiShape *val)		{ shape = val; }
    void setPlacement(StiPlacement *val);
    void setProperties(std::string name, StiIsActiveFunctor* activeFunctor, StiShape* shape, StiPlacement* placement,
            StiMaterial* gas, StiMaterial* material);

    //action
    virtual void build(){}  //for now, build from SCL parsable ascii file

    virtual void copy(StiDetector &detector);
    
    //This is a bit of a hack, but we leave ourselves a reverse connection between
    // a detector and the tree node that it's stored on.
    void setTreeNode( StiCompositeTreeNode<StiDetector> * val) {mNode=val;}
    StiCompositeTreeNode<StiDetector> * getTreeNode() const {return mNode;}
    
    void setHitErrorCalculator(const StiHitErrorCalculator * calculator) {_hitErrorCalculator = calculator;}
    const StiHitErrorCalculator * getHitErrorCalculator() const {return _hitErrorCalculator;}

    void setGroupId(int id) { _groupId = _groupId < 0 ? id : _groupId; }
    int  getGroupId() const {return _groupId;}

    void setTrackingParameters(const StiTrackingParameters * pars) {_pars = pars;}
    const StiTrackingParameters * getTrackingParameters() const {return _pars;}

    friend ostream& operator<<(ostream&os, const StiDetector & det);

    void setKey(int index,int value);
    int getKey(int index) const; 

    double getVolume() const;
    double getWeight() const;
    void   getDetPlane(double plane[4]) const;

 static int    mgIndex;
 static double mgValue[2];

 protected:
    
    char mBeg[1];
   
    /// Functor used to calculate whether the posistion reached by a track is 
    /// to be considered within the active area of the detector, and
    /// is thus susceptible of providing hit information.
 StiIsActiveFunctor *isActiveFunctor; 

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
    int _groupId = -1;
    const StiTrackingParameters * _pars;
    int _key1, _key2;

    char mEnd[1];
};


inline void StiDetector::setPlacement(StiPlacement *val)
{ 
  placement = val; 
  _cos = cos(val->getNormalRefAngle());
  _sin = sin(val->getNormalRefAngle());
}


inline void StiDetector::setKey(int index,int value)
{
switch (index)
  {
  case 1: _key1 = value; break;
  case 2: _key2 = value; break;
  }
}


inline int StiDetector::getKey(int index) const 
{
switch (index)
  {
  case 1: return _key1;
  case 2: return _key2;
  }
return -1;
}

#endif
