#ifndef StiSsdDetectorBuilder_H
#define StiSsdDetectorBuilder_H

#include "Sti/StiDetectorBuilder.h"

class StiSsdDetectorBuilder : public StiDetectorBuilder
{

public:
    // constructors
    StiSsdDetectorBuilder(bool active);
    virtual ~StiSsdDetectorBuilder(); 
    virtual void buildDetectors(StMaker&s);
};

#endif 
