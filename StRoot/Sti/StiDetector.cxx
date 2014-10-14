#include <math.h>
#include "Stiostream.h"
#include <string>
#include <map>
#include "TError.h"
#include "StiMaterial.h"
#include "StiShape.h"
#include "StiPlanarShape.h"
#include "StiCylindricalShape.h"
#include "StiPlacement.h"
#include "StiDetectorContainer.h"
#include "StiDetector.h"
#include "Sti/StiToolkit.h"
#include "StiMapUtilities.h"


//______________________________________________________________________________
StiDetector::StiDetector()
{
  reset();
}
//______________________________________________________________________________
void StiDetector::reset()
{
  setName("");
  memset(mBeg,0,mEnd-mBeg+1);
  _key1 = _key2 = -1;
}

//______________________________________________________________________________
StiDetector::~StiDetector()
{}

//______________________________________________________________________________
void StiDetector::copy(StiDetector &detector){

  isActiveFunctor = detector.isActiveFunctor;

  gas = detector.getGas();
  material = detector.getMaterial();
  shape = detector.getShape();
  placement = detector.getPlacement();
  _cos  = detector._cos;
  _sin  = detector._sin;
  setName(detector.getName());
}
 
//______________________________________________________________________________
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
static int nCall=0; nCall++;


  double startWeight = getWeight();
  vect.resize(1);
  vect[0]=this;
  assert(shape);
  int iShape = shape->getShapeCode();
  float deltaX = shape->getThickness();
  float halfZ  = shape->getHalfDepth(); 
  float halfY  = shape->getHalfWidth(); 
  float angle  = shape->getOpeningAngle(); 
  float nRadius = placement->getNormalRadius();
  if (iShape == kCylindrical)  nRadius = shape->getOuterRadius()-deltaX/2;

  if (nRadius < deltaX/2) {		// non splitable
    Warning("splitIt","%s Non splitable Rnormal < thickness/2 %g %g\n"
           ,getName().c_str(),nRadius,deltaX/2);
    return 1;
  }
  int ny = deltaX/(halfY*2*dXdY)+0.5;
  int nz = deltaX/(halfZ*2*dXdY)+0.5;
  int nSplit = (ny>nz)? ny:nz;
  if (nSplit<=1) return 1;
  if (nSplit>nMax) nSplit=nMax;

//		OK, we mast split it.

   vect.clear();
   float dX = deltaX/nSplit;
   double sumWeight = 0;
   for (int iSplit=0; iSplit<nSplit; iSplit++) 
   {
     float xc = -deltaX/2 +dX/2+iSplit*dX;  
//		Create small part of  detector
     StiDetector *det = StiToolkit::instance()->getDetectorFactory()->getInstance();
     det->copy(*this);
     TString ts(getName());
     if (iSplit) { ts+="_"; ts+=iSplit;} 
     det->setName(ts.Data());
//		Create shape
     ts = shape->getName();
     if (iSplit) { ts+="_"; ts+=iSplit;} 
     StiShape *myShape =0;
     float myRadius = nRadius+xc;
assert(myRadius>1e-2 && myRadius < 1e3);
     if (iShape==kPlanar) 	{//Planar shape
       myShape = new StiPlanarShape(ts.Data(),halfZ,dX,halfY);

     } else if (iShape==kCylindrical) {//Cylinder shape
       myShape = new StiCylindricalShape(ts.Data(),halfZ,dX,myRadius+dX/2,angle);

     } else { assert(0 && "Wrong shape type");}

//		Create placement
     StiPlacement *place = new StiPlacement;
     *place = *placement;
     place->setNormalRep(placement->getNormalRefAngle(),myRadius,placement->getNormalYoffset());
     det->setShape(myShape);
     place->setLayerRadius(myRadius);
     det->setPlacement(place);
     sumWeight += det->getWeight();
     vect.push_back(det);
   }
   this->copy(*vect[0]); 
   this->setName(vect[0]->getName());
//   delete vect[0];
    StiToolkit::instance()->getDetectorFactory()->free(vect[0]);
    vect[0] = this;
//    if (vect.size()>1) {
//      printf("StiDetector::splitIt %s is splitted into %d peaces\n",getName().c_str(),vect.size());}
//    assert(fabs(startWeight-sumWeight)<1e-3*startWeight);


   return vect.size();
}
//______________________________________________________________________________
double StiDetector::getVolume() const
{
return shape->getVolume();
}
//______________________________________________________________________________
    double StiDetector::getWeight() const
{
return shape->getVolume()*material->getDensity();
}
//______________________________________________________________________________
int StiDetector::insideL(const double xl[3]) const 
{
double rN = placement->getNormalRadius();
double thick = shape->getThickness();
if (shape->getShapeCode()==1) { //Planar
  if (fabs(xl[0]-rN)>thick/2) 			return 0;
  double y = xl[1]-placement->getNormalYoffset();
  if (fabs(y)>shape->getHalfWidth()) 		return 0;
 } else {
  double rxy = sqrt(xl[0]*xl[0]+xl[1]*xl[1]);
  if (fabs(rxy-rN)>thick/2) 			return 0;
    double ang = atan2(xl[1],xl[0]);
    if (fabs(ang)>shape->getOpeningAngle()/2)	return 0;
  } 
  double z = xl[2]-placement->getZcenter();  
  if (fabs(z)>shape->getHalfDepth())		return 0;
  return 1;
}
