#ifndef StiSsdDetectorBuilder_H
#define StiSsdDetectorBuilder_H

#include "Sti/StiDetectorBuilder.h"

class StiSsdDetectorBuilder : public StiDetectorBuilder
{

public:
    StiSsdDetectorBuilder(bool active, const string & inputFile);
    virtual ~StiSsdDetectorBuilder(); 
    virtual void buildDetectors(StMaker&s);
};

#endif 
