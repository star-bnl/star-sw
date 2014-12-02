#include <math.h>
#include <stdio.h>
#include "StiPlacement.h"

using namespace std;


//______________________________________________________________________________
StiPlacement::StiPlacement(){
    normalRefAngle=0; 
    normalRadius=0;   
    normalYoffset=0;
    centerRefAngle=0; 
    centerRadius=0;   
    centerOrientation=0;  
    layerRadius=0;
    zCenter=0;
  setNormalRep(0., 0., 0.);
}
//______________________________________________________________________________
StiPlacement::StiPlacement(float  normRefAngle,float  normRadius,float normYOffset,float centralZ) 
{
  setNormalRep(normRefAngle,normRadius,normYOffset);
  zCenter = centralZ;

}
//______________________________________________________________________________
void StiPlacement::setNormalRep(float refAngle_, float radius_, float yOffset_)
{

  if (radius_ < 0) {
    radius_ = -radius_;
    refAngle_ += M_PI;
  }
  while(refAngle_ <  -M_PI){ refAngle_ += 2.*M_PI; }
  while(refAngle_ >=  M_PI){ refAngle_ -= 2.*M_PI; }
  normalRefAngle = refAngle_;

  normalRadius = radius_; 
  normalYoffset = yOffset_;

  // the checking above makes these values within bounds, also
  centerRadius = ::sqrt(normalRadius*normalRadius + normalYoffset*normalYoffset);
  centerOrientation = atan2(normalYoffset,normalRadius);
  centerRefAngle = normalRefAngle + centerOrientation;
  while(centerRefAngle <  -M_PI){ centerRefAngle += 2.*M_PI; }
  while(centerRefAngle >=  M_PI){ centerRefAngle -= 2.*M_PI; }
  setLayerRadius(centerRadius);
  layerAngle = centerRefAngle;
  mRegion = kMidRapidity;

}// setNormalRep()

//______________________________________________________________________________
void StiPlacement::setLayerAngle(float layerAng) 
{
  layerAngle = layerAng;
  while (layerAngle< -M_PI) {layerAngle+=2*M_PI;}	
  while (layerAngle>  M_PI) {layerAngle-=2*M_PI;}	
}
	
	
//______________________________________________________________________________
ostream& operator<<(ostream& os, const StiPlacement& p)
{
   os << "StiPlacement:" << endl
      << "normalRefAngle: " << p.normalRefAngle << " rad, "
      << "normalRadius: " << p.normalRadius << " cm, "
      << "normalYoffset: " << p.normalYoffset << " cm" << endl
      << "centerRefAngle: " << p.centerRefAngle << " rad, "
      << "centerRadius: "   << p.centerRadius << " cm, "
      << "centerOrientation: "  << p.centerOrientation << " rad" << endl
      << "zCenter: " << p.zCenter << " cm, "
      << "layerRadius: " << p.layerRadius << " cm, "
      << "layerAngle: " << p.layerAngle << " rad" << endl;

   return os;
}
//______________________________________________________________________________
void StiPlacement::setLayerRadius(float radius_)
{
static const double kMinRad=0.1,kMaxRad=200;
static const double kLogStep=(log(kMaxRad)-log(kMinRad))/1000;
   int nStp = int(log(radius_+kMinRad)/kLogStep);
   layerRadius = exp(nStp*kLogStep)-kMinRad; 
}
