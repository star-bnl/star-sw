#ifndef StiTpcDetectorBuilder_H
#define StiTpcDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"

class StiTpcDetectorBuilder : public StiDetectorBuilder
{

public:
    StiTpcDetectorBuilder(bool active);
    virtual ~StiTpcDetectorBuilder(); 	
    virtual void buildDetectors(StMaker&s);
    /// returns the azimuthal angle [-pi, pi) for tpc sector [1-24]
    void         useVMCGeometry();		
int isActive(int sector,int row) const;

static int   sector(double ang,int east=0);
static double angle(int sec);
 protected:
    int _noDead;
    StiMaterial * _fcMaterial;    
};

#endif 
