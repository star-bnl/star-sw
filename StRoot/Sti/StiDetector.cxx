//StiDetector.cxx

//STD
#include <iostream>
#include <string>
//SCL
#include "StGetConfigValue.hh"
//Sti
#include "StiDetector.h"

StiDetector::StiDetector()
{
}

StiDetector::~StiDetector()
{
}

void StiDetector::build(const char* buildfile)
{
    StGetConfigValue(buildfile, "on", on);
    StGetConfigValue(buildfile, "active", active);
    StGetConfigValue(buildfile, "continuousMedium", continuousMedium);
    StGetConfigValue(buildfile, "discreteScatterer", discreteScatterer);
    
    string materialName;
    StGetConfigValue(buildfile, "gas", materialName);
    if(materialName != "(null)"){
      //TODO: set gas by name
    }
    StGetConfigValue(buildfile, "material", materialName);
    if(materialName != "(null)"){
      //TODO: set gas by name
    }
    int code;
    StGetConfigValue(buildfile, "shapeCode", code);
    shapeCode = static_cast<StiShapeCode>(code);

    // we only store & read one representation of the geometry
    StGetConfigValue(buildfile, "centerRadius", centerRadius);
    StGetConfigValue(buildfile, "centerRefAngle", centerRefAngle);
    StGetConfigValue(buildfile, "orientationAngle", orientationAngle);
    StGetConfigValue(buildfile, "halfWidth", halfWidth);
    updateCenterRep();

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
  return os <<d.isOn()<<" "<<d.isActive()<<" "<<d.isContinuousMedium()<<" "
            <<d.isDiscreteScatterer()<<" "
            <<*(d.getGas())<<" "<<*(d.getMaterial())<<" "
            <<d.getShapeCode()<<" "
            <<d.getCenterRadius()<<" "<<d.getCenterRefAngle()<<" "
            <<d.getOrientationAngle()<<" "<<d.getHalfWidth()<<" "
            <<d.getNormalRadius()<<" "<<d.getNormalRefAngle()<<" "
            <<d.getYmin()<<" "<<d.getYmax()<<" "
	    <<d.getActivePosition()<<" "<<d.getZCenter()<<" "
            <<d.getHalfDepth()<<" "<<d.getThickness()<<" "
            <<d.getSector()<<" "<<d.getPadrow()<<" "<<d.getName();
} // <<
