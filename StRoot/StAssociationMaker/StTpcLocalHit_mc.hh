/***************************************************************************
 *
 * $Id: StTpcLocalHit_mc.hh,v 1.3 1999/09/23 21:25:23 calderon Exp $
 * Generated local hits in the TPC - MALisa 9jun99
 * $Log: StTpcLocalHit_mc.hh,v $
 * Revision 1.3  1999/09/23 21:25:23  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 *
 **************************************************************************/

#ifndef StTpcLocalHit_mc_HH
#define StTpcLocalHit_mc_HH

#include "StLocalHit.hh"
#include <iostream.h>

class StMcTpcHit;

class StTpcLocalHit_mc : public StLocalHit {

public:
    StTpcLocalHit_mc(const StMcTpcHit*, float, float);
    ~StTpcLocalHit_mc();

    const StMcTpcHit* globalHitPtr(){return mHit;};

private:
    const StMcTpcHit* mHit;   // points to the hit in the StMcTpcHit collection
};
ostream& operator<<(ostream&, const StTpcLocalHit_mc&);

#endif
