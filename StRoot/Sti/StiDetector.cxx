//StiDetector.cxx

//STD
#include <iostream>
//SCL
#include "StGetConfigValue.hh"
//Sti
#include "StiDetector.h"

StiDetector::StiDetector() : name(0)
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
    gas = StiMaterial::findMaterial(materialName.c_str());
    StGetConfigValue(buildfile, "material", materialName);
    material = StiMaterial::findMaterial(materialName.c_str());

    int code;
    StGetConfigValue(buildfile, "shapeCode", code);
    shapeCode = code;

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
}

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
}
