#ifndef StiFtpcDetectorBuilder_H
#define StiFtpcDetectorBuilder_H

#include "Sti/StiDetectorBuilder.h"

class StiFtpcDetectorBuilder : public StiDetectorBuilder
{

public:
    // constructors
    StiFtpcDetectorBuilder();
    virtual ~StiFtpcDetectorBuilder(); 
    virtual void loadDb();
    virtual void buildMaterials();
    virtual void buildShapes();
    virtual void buildDetectors();
};

#endif 
