//AnaCuts.h

#ifndef AnaCuts_HH
#define AnaCuts_HH

#include "TObject.h"
// cuts on single track kinematics
class AnaCuts
{
public:

    //Single track cuts
    double ptCut;    // momentum (not pt)
    double triggerPtCut; //pt, gev
    double pseudoRapidityCutOff;

    // track quality cuts
    unsigned int minNumberOfFitPoints;
    unsigned int minNumberOfPoints;

    // general cuts
    double l3MomentumCut;    // keep only tracks with higher momentum (not pt);}
    bool verbose;    //  controls the amount of printout
    
private:
    ClassDef(AnaCuts,2)
};

#endif
