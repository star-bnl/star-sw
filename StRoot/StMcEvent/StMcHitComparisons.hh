/**********************************************
 *
 * $Id: StMcHitComparisons.hh,v 2.2 2000/06/09 19:52:07 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez
 ***********************************************
 * Description: Define the comparisons to be used
 *              in the multimaps
 *              & sorting of containers
 *
 ***********************************************
 * $Log: StMcHitComparisons.hh,v $
 * Revision 2.2  2000/06/09 19:52:07  calderon
 * No longer use 2 different functions for SVT and TPC that do the same thing,
 * just use one function for the base class
 *
 * Revision 2.1  2000/03/07 15:09:54  calderon
 * Initial Revision.
 * Comparisons used for sorting the hit containers, and
 * for ordering the hits in the multimaps.
 * 
 * 
 **********************************************/
#ifndef StMcHitComparisons_HH
#define StMcHitComparisons_HH
class StHit;
class StTpcHit;
class StSvtHit;
class StFtpcHit;
class StRichHit;

class StMcHit;
class StMcTpcHit;
class StMcSvtHit;
class StMcFtpcHit;
class StMcRichHit;
class StThreeVectorF;
#if (defined __SUNPRO_CC && (__SUNPRO_CC < 0x500))
// bool is defined in utility for SUNPRO_CC 4.2
#include <utility> 
#endif
struct compHit{
    bool operator()(const StHit*, const StHit*) const;
};

struct compMcHit{
    bool operator()(const StMcHit*, const StMcHit*) const;
};

// struct compTpcHit{
//     bool operator()(const StTpcHit* h1,const StTpcHit* h2) const;
// };

// struct compMcTpcHit{
//     bool operator()(const StMcTpcHit* h1,const StMcTpcHit* h2) const;
// };

// struct compSvtHit{
//     bool operator()(const StSvtHit* h1,const StSvtHit* h2) const;
// };

// struct compMcSvtHit{
//     bool operator()(const StMcSvtHit* h1,const StMcSvtHit* h2) const;
// };

struct compRPhi{
    bool operator()(const StThreeVectorF&, const StThreeVectorF&) const;
};
struct compFtpcHit{
    bool operator()(const StFtpcHit*,const StFtpcHit*) const;
};
struct compMcFtpcHit{
    bool operator()(const StMcFtpcHit*,const StMcFtpcHit*) const;
};

#endif
