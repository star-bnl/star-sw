//StiDetector.cxx

//STD
#include <math.h>
#include <iostream>
#include <string>
#include <map>
//SCL
#include "StGetConfigValue.hh"
//Sti
#include "StiMaterial.h"
#include "StiDetectorContainer.h"
#include "StiDetector.h"
#include "StiMapUtilities.h"

StiDetector::StiDetector() : gas(0), material(0)
{
}

StiDetector::~StiDetector()
{
}

void StiDetector::setCenterRep(double cRadius, double cRefAngle, double oAngle, double hWidth)
{
    centerRadius = cRadius;
    centerRefAngle = cRefAngle;
    orientationAngle = oAngle;
    halfWidth = hWidth;
    updateNormalRep();
}


void StiDetector::setNormalRep(double nRadius, double nRefAngle,double minY, double maxY)
{
    normalRadius = nRadius;
    normalRefAngle = nRefAngle;
    yMin = minY;
    yMax = maxY;
    updateCenterRep();
}

void StiDetector::updateNormalRep()
{
    if(shapeCode == kPlanar){
        normalRadius = centerRadius*sin(orientationAngle);
        normalRefAngle = centerRefAngle - orientationAngle;
        yMin = centerRadius*cos(orientationAngle);
        yMax = yMin + 2.*halfWidth;
    }
    else if(shapeCode == kCylindrical){
        normalRadius = centerRadius;
        normalRefAngle = centerRefAngle;
        yMin = -halfWidth;
        yMax = halfWidth;
    }
}

void StiDetector::updateCenterRep()
{
    if(shapeCode == kPlanar){
        halfWidth = (yMax - yMin)/2.;
        orientationAngle = atan( (yMin + yMax)/2./normalRadius );
        centerRefAngle = normalRefAngle - orientationAngle;
        centerRadius = normalRadius/cos(orientationAngle);
    }
    else if(shapeCode == kCylindrical){
        halfWidth = yMax;
        orientationAngle = 0;
        centerRefAngle = normalRefAngle;
        centerRadius = normalRadius;
    }

    cout << "yMax = " << yMax << endl
         << "yMin = " << yMin << endl
         << "normalRefAngle = " << normalRefAngle << endl
         << "normalRadius = " << normalRadius << endl
         << "halfWidth = " << halfWidth << endl
         << "orientationAngle = " << orientationAngle << endl
         << "centerRefAngle = " << centerRefAngle << endl
         << "centerRadius = " << centerRadius << endl;
}

void StiDetector::build(const char* buildfile)
{
    StiDetectorContainer* store = StiDetectorContainer::instance();
    
    StGetConfigValue(buildfile, "on", on);
    StGetConfigValue(buildfile, "active", active);
    StGetConfigValue(buildfile, "continuousMedium", continuousMedium);
    StGetConfigValue(buildfile, "discreteScatterer", discreteScatterer);
    
    //materialmap materialMap =  StiDetectorContainer::instance()->getMaterialMap();
    
    string gasName;
    StGetConfigValue(buildfile, "gas", gasName);
    if(gasName != "(null)"){
      gas = store->material( MaterialMapKey(gasName.c_str()) );
    }
    else {
	gas = 0;
    }
    string materialName;
    StGetConfigValue(buildfile, "material", materialName);
    if(materialName != "(null)"){
      material = store->material( MaterialMapKey(materialName.c_str()) );
      //material = materialMap[MaterialMapKey(materialName.c_str())];
    }
    else {
	material=0;
    }
    int code;
    StGetConfigValue(buildfile, "shapeCode", code);
    shapeCode = static_cast<StiShapeCode>(code);

    // we only store & read one representation of the geometry
    StGetConfigValue(buildfile, "centerRadius", centerRadius);
    StGetConfigValue(buildfile, "centerRefAngle", centerRefAngle);
    
    //Temporary Fix of ordering in phi
    //if (centerRefAngle<0) centerRefAngle+=2.*M_PI;
    
    StGetConfigValue(buildfile, "orientationAngle", orientationAngle);
    StGetConfigValue(buildfile, "halfWidth", halfWidth);
    updateNormalRep();

    StGetConfigValue(buildfile,"activePosition",activePosition);
    StGetConfigValue(buildfile,"zCenter",zCenter);
    StGetConfigValue(buildfile,"halfDepth",halfDepth);
    StGetConfigValue(buildfile, "thickness", thickness);

    StGetConfigValue(buildfile,"sector",sector);
    StGetConfigValue(buildfile,"padrow",padrow);
    StGetConfigValue(buildfile,"name",name);

    return;
}  // build()
 
void StiDetector::write(const char *szFileName){
  
  ofstream os(szFileName);

  os << "on:\t\t\t" << static_cast<int>(on) << endl;
  os << "active:\t\t\t" << static_cast<int>(active) << endl;
  os << "continuousMedium:\t" << static_cast<int>(continuousMedium) << endl;
  os << "discreteScatterer:\t" << static_cast<int>(discreteScatterer) << endl;
  
  os << "gas:\t\t\t" << (gas ? gas->getName() : "(null)") << endl;
  os << "material:\t\t" << (material ? material->getName() : "(null)") << endl;

  os << "shapeCode:\t\t" << static_cast<int>(shapeCode) << endl;

  // only store one representation of geometry
  os << "centerRadius:\t\t" << centerRadius << endl;
  os << "centerRefAngle:\t\t" << centerRefAngle << endl;
  os << "orientationAngle:\t" << orientationAngle << endl;
  os << "halfWidth:\t\t" << halfWidth << endl;

  os << "activePosition:\t\t" << activePosition << endl;
  os << "zCenter:\t\t" << zCenter << endl;
  os << "halfDepth:\t\t" << halfDepth << endl;
  os << "thickness:\t\t" << thickness << endl;

  os << "sector:\t\t\t" << sector << endl;
  os << "padrow:\t\t\t" << padrow << endl;
  os << "name:\t\t\t" << name << endl;

} // write()

ostream& operator<<(ostream& os, const StiDetector& d)
{
    os <<d.isOn()<<" "<<d.isActive()<<" "<<d.isContinuousMedium()<<" "<<d.isDiscreteScatterer()<<" ";
    if (!d.getGas()) {
	cout <<"No Gas ";
    }
    else {
	os <<*(d.getGas())<<" ";
    }
    if (!d.getMaterial()) {
	cout <<"No Material ";
    }
    else {
	os <<*(d.getMaterial())<<" ";
    }
    os <<d.getShapeCode()<<" "<<d.getCenterRadius()<<" "<<d.getCenterRefAngle()<<" ";
    os <<d.getOrientationAngle()<<" "<<d.getHalfWidth()<<" ";
    os <<d.getNormalRadius()<<" "<<d.getNormalRefAngle()<<" ";
    os <<d.getYmin()<<" "<<d.getYmax()<<" ";
    os <<d.getActivePosition()<<" "<<d.getZCenter()<<" ";
    os <<d.getHalfDepth()<<" "<<d.getThickness()<<" ";
    os <<d.getSector()<<" "<<d.getPadrow()<<" "<<d.getName();;
    return os;
}
