//------------------------------------------------------------------
// gl3Cylinder: Algorithm to trigger on tracks that cross a 
//              cylindrical volume around the beampipe.
//
// This algorithm triggers when at least GI1 tracks were found in 
// cylindrical volume ranging from z=GF1 to z=GF2 with a radius=GF3.
//
//------------------------------------------------------------------
#ifndef GL3CYLINDER
#define GL3CYLINDER  

#include "Stl3Util/gl3/gl3Algorithm.h"


class gl3Cylinder: public gl3Algorithm {

 public:
    gl3Cylinder();
    virtual int decide();
    virtual int setParameters(int, int, int, int, int,
			      float, float, float, float, float);

    virtual const int   getAlgorithmID() 
	{ return L3_ALGORITHM_CYLINDER; }

    virtual const char *getAlgorithmName() 
	{ return L3_ALGORITHM_CYLINDER_NAME; }
 
 private:
    //static const char algoName[20];

    int minNoOfHitsOnTrack;

};
#endif
