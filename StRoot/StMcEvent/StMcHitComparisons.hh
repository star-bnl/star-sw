/**********************************************
 *
 * $Id: StMcHitComparisons.hh,v 2.1 2000/03/07 15:09:54 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez
 ***********************************************
 * Description: Define the comparisons to be used
 *              in the multimaps
 *              & sorting of containers
 *
 ***********************************************
 * $Log: StMcHitComparisons.hh,v $
 * Revision 2.1  2000/03/07 15:09:54  calderon
 * Initial Revision.
 * Comparisons used for sorting the hit containers, and
 * for ordering the hits in the multimaps.
 * 
 * 
 **********************************************/
#ifndef StMcHitComparisons_HH
#define StMcHitComparisons_HH

class StTpcHit;
class StSvtHit;
class StFtpcHit;
class StRichHit;

class StMcTpcHit;
class StMcSvtHit;
class StMcFtpcHit;
class StMcRichHit;

#if (defined __SUNPRO_CC && (__SUNPRO_CC < 0x500))
// bool is defined in utility for SUNPRO_CC 4.2
#include <utility> 
#endif

struct compTpcHit{
    bool operator()(const StTpcHit*,const StTpcHit*) const;
};

struct compMcTpcHit{
    bool operator()(const StMcTpcHit*,const StMcTpcHit*) const;
};

struct compSvtHit{
    bool operator()(const StSvtHit*,const StSvtHit*) const;
};

struct compMcSvtHit{
    bool operator()(const StMcSvtHit*,const StMcSvtHit*) const;
};

struct compFtpcHit{
    bool operator()(const StFtpcHit*,const StFtpcHit*) const;
};

struct compMcFtpcHit{
    bool operator()(const StMcFtpcHit*,const StMcFtpcHit*) const;
};

#endif
