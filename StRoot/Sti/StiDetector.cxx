#include <math.h>
#include <iostream>
#include <string>
#include <map>
#include "StiMaterial.h"
#include "StiShape.h"
#include "StiPlacement.h"
#include "StiDetectorContainer.h"
#include "StiDetector.h"
#include "StiMapUtilities.h"


StiDetector::StiDetector() : gas(0), material(0), shape(0), placement(0), mNode(0), _cos(0), _sin(0)
{ }

StiDetector::~StiDetector()
{}

void StiDetector::copy(StiDetector &detector){

  on = detector.isOn();
  isActiveFunctor = detector.isActiveFunctor;
  continuousMedium = detector.isContinuousMedium();
  discreteScatterer = detector.isDiscreteScatterer();

  gas = detector.getGas();
  material = detector.getMaterial();
  shape = detector.getShape();
  placement = detector.getPlacement();
  _cos  = detector._cos;
  _sin  = detector._sin;
  setName(detector.getName());
}
 
ostream& operator<<(ostream& os, const StiDetector& d){

    os << d.getName()
       <<"\tR:"<<d.getPlacement()->getCenterRadius()<<"cm\tA:"
       <<d.getPlacement()->getCenterRefAngle()<< " radians";
    return os;
    
} // operator<<


