//AnaCuts.h

#ifndef AnaCuts_HH
#define AnaCuts_HH

#include "TObject.h"
// cuts on single track kinematics
class AnaCuts
{
public:

    //Single track cuts
    double electronMomentumCut;    // momentum (not pt)
    double pseudoRapidityCutOff;

    // track quality cuts
    unsigned int minNumberOfFitPoints;
    unsigned int minNumberOfPoints;

    // pair cuts
    double lowerInvariantMassCut;
    double upperInvariantMassCut;
    
    // general cuts
    double l3MomentumCut;    // keep only tracks with higher momentum (not pt);}
    Bool_t verbose;    //  controls the amount of printout
    
private:
    ClassDef(AnaCuts,1)
};

#endif
