#ifndef StiFtpcDetectorBuilder_H
#define StiFtpcDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"

class StiFtpcDetectorBuilder : public StiDetectorBuilder
{
public:
    StiFtpcDetectorBuilder(bool active, const string & inputFile);
    virtual ~StiFtpcDetectorBuilder(); 
    virtual void buildDetectors(StMaker&source);
};

#endif 
