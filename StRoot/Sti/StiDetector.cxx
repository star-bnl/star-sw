#include <math.h>
#include "Stiostream.h"
#include <string>
#include <map>
#include "StiMaterial.h"
#include "StiShape.h"
#include "StiPlacement.h"
#include "StiDetectorContainer.h"
#include "StiDetector.h"
#include "StiMapUtilities.h"


StiDetector::StiDetector()
{
  reset();
}
void StiDetector::reset()
{
  setName("");
  memset(mBeg,0,mEnd-mBeg+1);
  _key1 = _key2 = -1;
}

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
 
ostream& operator<<(ostream& os, const StiDetector& d)
{
    os << "StiDetector:" << endl
       << d.getName()
       <<"\tR:"<<d.getPlacement()->getNormalRadius()<<"cm\tA:"
       <<d.getPlacement()->getNormalRefAngle()<< " radians" << endl;

    if (d.material)
       os << *d.material;

    if (d.shape)
       os << *d.shape;

    if (d.placement)
       os << *d.placement;

    return os;
}
