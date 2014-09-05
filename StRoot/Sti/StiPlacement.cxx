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
void StiPlacement::setNormalRep(float refAngle_, float radius_, float yOffset_)
{

  while(refAngle_ <  -M_PI){ refAngle_ += 2.*M_PI; }
  while(refAngle_ >=  M_PI){ refAngle_ -= 2.*M_PI; }
  normalRefAngle = refAngle_;

  if(radius_ >= 0.){ normalRadius = radius_; }
  normalYoffset = yOffset_;

  // the checking above makes these values within bounds, also
  centerRadius = ::sqrt(normalRadius*normalRadius + normalYoffset*normalYoffset);
  centerOrientation = atan2(normalYoffset,normalRadius);
  centerRefAngle = normalRefAngle + centerOrientation;
  while(centerRefAngle <  -M_PI){ centerRefAngle += 2.*M_PI; }
  while(centerRefAngle >=  M_PI){ centerRefAngle -= 2.*M_PI; }

}// setNormalRep()

//______________________________________________________________________________
void StiPlacement::setLayerAngle(float layerAngle) 
{
  _layerAngle = layerAngle;
  if (_layerAngle< -M_PI)  _layerAngle+=2*M_PI;	
  if (_layerAngle>  M_PI)  _layerAngle-=2*M_PI;	
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
      << "_layerAngle: " << p._layerAngle << " rad" << endl;

   return os;
}
//______________________________________________________________________________
void StiPlacement::setLayerRadius(float radius_)
{
static const double kMinRad=0.1,kMaxRad=200;
static const double kLogStep=(log(kMaxRad)-log(kMinRad))/1000;
   int nStp = int(log(radius_+kMinRad)/kLogStep);
   layerRadius = exp(nStp*kLogStep); 
}
