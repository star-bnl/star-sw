//Functors.cxx
//M.L. Miller (MIT)

#include "StJetMaker/StEmcHitMakers/EmcHit.h"
#include "StJetMaker/StFourPMakers/Functors.h"

bool EmcHitLT::operator()(const EmcHit* lhs, const EmcHit* rhs) const
{
    if (lhs->correctedPosition().pseudoRapidity() < rhs->correctedPosition().pseudoRapidity()) {
	return true;
    }
    else if (lhs->correctedPosition().pseudoRapidity() > rhs->correctedPosition().pseudoRapidity()) {
	return false;
    }
    else {
	return (lhs->correctedPosition().phi() < rhs->correctedPosition().phi() );	
    }
}
