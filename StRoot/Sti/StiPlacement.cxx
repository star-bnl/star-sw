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
  setLayerRadius(centerRadius);
  layerAngle = centerRefAngle;
  mRegion = kMidRapidity;

}


/**
 * Creates a new StiPlacement for an Sti detector by transforming its center
 * coordinates to the global coordinate system defined by transMatrix. The
 * center of the Sti detector can be placed at arbitrary localCenter in the
 * _local_ coordinate system.
 *
 * By default the normal vector is collinear with the "thickness" of the Sti
 * detector, i.e. normal = {x, y, z} = {0, 1, 0}. But it can be arbitrary
 * redefined to, say, {1, 0, 0} in order to swap the Sti detector's thickness
 * and width. It is assumed that the normal vector starts from the origin.
 */
StiPlacement::StiPlacement(const TGeoMatrix& transMatrix, const TVector3& localCenter, const TVector3& normal)
{
   double centerXyzLocal[3]  = {localCenter.X(), localCenter.Y(), localCenter.Z()};
   double centerXyzGlobal[3] = {};

   double normalXyzLocal[3]  = {normal.X() + localCenter.X(), normal.Y() + localCenter.Y(), normal.Z() + localCenter.Z()};
   double normalXyzGlobal[3] = {};

   // Translate the local coordinates to the global coordinate system
   transMatrix.LocalToMaster(centerXyzLocal, centerXyzGlobal);
   transMatrix.LocalToMaster(normalXyzLocal, normalXyzGlobal);

   TVector3 centralVec(centerXyzGlobal);
   TVector3 normalVec(normalXyzGlobal);

   // We actually need the normal vector from the center rather than from the
   // origin
   normalVec -= centralVec;

   if (normalVec.Dot(centralVec) < 0) normalVec *= -1;

   double deltaPhi = centralVec.DeltaPhi(normalVec);
   double normalVecMag = fabs(centralVec.Perp() * cos(deltaPhi));

   setNormalRep(normalVec.Phi(), normalVecMag, centralVec.Perp()*sin(deltaPhi));
   zCenter = centralVec.Z();
   setLayerRadius(centerRadius);
   layerAngle = centerRefAngle;
   mRegion = kMidRapidity;
}


//______________________________________________________________________________
void StiPlacement::setNormalRep(float refAngle_, float radius_, float yOffset_)
{

  if (radius_ < 0) {
    radius_   *= -1;
    refAngle_ += M_PI;
    yOffset_  *=-1.;
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
static const double kLogStep=(log(kMaxRad)-log(kMinRad))/10000;
   int nStp = int(log(radius_+kMinRad)/kLogStep);
   layerRadius = exp(nStp*kLogStep)-kMinRad; 
}
