#include <math.h>
#include <stdio.h>
#include "StiPlacement.h"

StiPlacement::StiPlacement(){
    normalRefAngle=0; 
    normalRadius=0;   
    normalYoffset=0;
    centerRefAngle=0; 
    centerRadius=0;   
    centerOrientation=0;  
    layerRadius=0;
    zCenter=0;
  setCenterRep(0., 0., 0.);
}// StiPlacement()

void StiPlacement::setCenterRep(float refAngle_, float radius_, 
                                float orientation_){

  while(refAngle_ <  -M_PI){ refAngle_ += 2.*M_PI; }
  while(refAngle_ >=  M_PI){ refAngle_ -= 2.*M_PI; }
  centerRefAngle = refAngle_;


  if(radius_ >= 0.){ centerRadius = radius_; }
  
  while(orientation_ < -M_PI/2.){ orientation_ += M_PI; }
  while(orientation_ >= M_PI/2.){ orientation_ -= M_PI; }
  centerOrientation = orientation_;

  normalRefAngle = centerRefAngle + centerOrientation;
  while(normalRefAngle <  -M_PI){ normalRefAngle += 2.*M_PI; }
  while(normalRefAngle >=  M_PI){ normalRefAngle -= 2.*M_PI; }
  normalRadius = centerRadius*cos(centerOrientation);
  normalYoffset = centerRadius*sin(centerOrientation);
  if (!radius_) return;
  double trig[4];
  trig[0] = cos(normalRefAngle);
  trig[1] = sin(normalRefAngle);
  trig[2] = cos(centerRefAngle);
  trig[3] = sin(centerRefAngle);
#if 0
  double dif = fabs(trig[0]-trig[2])+fabs(trig[1]-trig[3]);
  if (dif >1e-3) {
    printf("**** centerRefAngle=%g normalAngle=%g diff=%g ****\n"
          ,centerRefAngle,normalRefAngle,dif);
  }
#endif
}// setCenterRep()

void StiPlacement::setNormalRep(float refAngle_, float radius_, 
                                float yOffset_){

  while(refAngle_ <  -M_PI){ refAngle_ += 2.*M_PI; }
  while(refAngle_ >=  M_PI){ refAngle_ -= 2.*M_PI; }
  normalRefAngle = refAngle_;

  if(radius_ >= 0.){ normalRadius = radius_; }
  normalYoffset = yOffset_;

  // the checking above makes these values within bounds, also
  centerRadius = ::sqrt(normalRadius*normalRadius + normalYoffset*normalYoffset);
  centerOrientation = atan2(normalYoffset,normalRadius);
  centerRefAngle = normalRefAngle - centerOrientation;
  while(centerRefAngle <  -M_PI){ centerRefAngle += 2.*M_PI; }
  while(centerRefAngle >=  M_PI){ centerRefAngle -= 2.*M_PI; }

  double trig[4];
  trig[0] = cos(normalRefAngle);
  trig[1] = sin(normalRefAngle);
  trig[2] = cos(centerRefAngle);
  trig[3] = sin(centerRefAngle);
#if 0
  double dif = fabs(trig[0]-trig[2])+fabs(trig[1]-trig[3]);
  if (dif >1e-3) {
    printf("**** centerRefAngle=%g normalAngle=%g diff=%g ****\n"
          ,centerRefAngle,normalRefAngle,dif);
  }
#endif  
  
  
//  cout << "normal(" << normalRefAngle << ", " << normalRadius
//       << ", " << normalXoffset << ") == center(" << centerRefAngle
//       << ", " << centerRadius << ", " << centerOrientation << ")" << endl;
}// setNormalRep()

void StiPlacement::setLayerAngle(float layerAngle) 
{
  _layerAngle = layerAngle;
  if (_layerAngle< -M_PI)  _layerAngle+=2*M_PI;	
  if (_layerAngle>  M_PI)  _layerAngle-=2*M_PI;	
}
	
	
