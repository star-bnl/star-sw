#include <math.h>
#include "Stiostream.h"
#include <string>
#include <map>
#include "StiMaterial.h"
#include "StiShape.h"
#include "StiPlanarShape.h"
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
  _elossCalculator = detector.getElossCalculator();
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
//______________________________________________________________________________
int StiDetector::splitIt(StiDetVect &vect,double dXdY,int nMax)
{
  vect.resize(1);
  vect[0]=this;
  assert(shape);
  if (shape->getShapeCode()!=kPlanar) return 1;

  float deltaX = shape->getThickness();
  float halfZ  = shape->getHalfDepth();
  float halfY  = shape->getHalfWidth();
  int ny = deltaX/(halfY*2*dXdY)+0.5;
  int nz = deltaX/(halfZ*2*dXdY)+0.5;
  int nSplit = (ny>nz)? ny:nz;
  if (nSplit<=1) return 1;
  if (nSplit>nMax) nSplit=nMax;

//		OK, we must split it.

   vect.clear();
   float dX = deltaX/nSplit;
   int N = -1;
   for (float xc = -deltaX/2 +dX/2; xc<deltaX/2;xc+=dX) {
     N++;
//		Create small part of  detector
     StiDetector *det = new StiDetector;
     det->copy(*this);
     TString ts(getName());
     if (N) { ts+="_"; ts+=N;}
     det->setName(ts.Data());
//		Create shape
     ts = shape->getName();
     if (N) { ts+="_"; ts+=N;}
     StiShape *myShape = new StiPlanarShape(ts.Data(),halfZ,dX,halfY);
     det->setShape(myShape);
//		Create placement

     StiPlacement *place = new StiPlacement;
     *place = *placement;
     float myRadius = placement->getNormalRadius()+xc;
     place->setNormalRep(placement->getNormalRefAngle()
                        ,myRadius
                        ,placement->getNormalYoffset());
     place->setLayerRadius(myRadius);
     det->setPlacement(place);
     vect.push_back(det);
   }
   this->copy(*vect[0]); 
   this->setName(vect[0]->getName());
   delete vect[0]; vect[0] = this;
   if (vect.size()>1) {
     printf("StiDetector::splitIt %s is split into %d peices\n",getName().c_str(),vect.size());}
   return vect.size();
}
