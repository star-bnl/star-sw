/***************************************************************************
 *
 * $Id: StTpcLocalHit_recon.hh,v 1.4 1999/10/01 14:08:59 calderon Exp $
 * Reconstructed local hits in the TPC - MALisa 9jun99 
 * $Log: StTpcLocalHit_recon.hh,v $
 * Revision 1.4  1999/10/01 14:08:59  calderon
 * Added Local Hit resolution Histogram. It is made by default
 * without any requirement of association, to serve
 * as a diagnostic.
 * Before building track multimap, check the size of the
 * tpc hit map.  If it is too small, print out a warning
 * and exit.
 *
 * Revision 1.3  1999/09/23 21:25:23  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 **************************************************************************/ 
#ifndef StTpcLocalHit_recon_HH
#define StTpcLocalHit_recon_HH

#include "StLocalHit.hh"
#include <iostream.h>

class StTpcHit;

class StTpcLocalHit_recon : public StLocalHit {

public:
  StTpcLocalHit_recon(const StTpcHit*, float, float);
  ~StTpcLocalHit_recon();

  const StTpcHit* globalHitPtr(){return mHit;};


private:
  const StTpcHit* mHit;   // points to the hit in the StTpcHit collection
};
ostream& operator<<(ostream&, const StTpcLocalHit_recon&);


#endif
