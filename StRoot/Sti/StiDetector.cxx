//StiDetector.cxx

//STD
#include <iostream>
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
    StGetConfigValue(buildfile, "density",density);
    StGetConfigValue(buildfile,"thickness",thickness);
    StGetConfigValue(buildfile,"halfWidth",halfWidth);
    StGetConfigValue(buildfile,"zcenter",zcenter);
    StGetConfigValue(buildfile,"halfDepth",halfDepth);
    StGetConfigValue(buildfile,"radLength",radLength);
    StGetConfigValue(buildfile,"position",position);
    StGetConfigValue(buildfile,"refAngle",refAngle);
    int dummy_sc=-1;
    StGetConfigValue(buildfile,"shapeCode",dummy_sc);
    if (dummy_sc==1) shapeCode=kPlanar;
    if (dummy_sc==2) shapeCode=kCircular;
    
    return;
}

ostream& operator<<(ostream& os, const StiDetector& d)
{
    return os <<d.isActive()<<" "<<d.isContinuousMedium()<<" "<<d.isDiscreteScatterer()<<" "
	      <<d.getDensity()<<" "<<d.getThickness()<<" "<<d.getHalfWidth()<<" "<<d.getHalfDepth()<<" "
	      <<d.getZCenter()<<" "<<d.getMaterialRadLength()<<" "<<d.getRadLengthThickness()<<" "
	      <<d.getPosition()<<" "<<d.getRefAngle()<<" "<<d.getShapeCode();
}
