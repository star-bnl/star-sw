/* Reconstructed local hits in the TPC - MALisa 9jun99 */

#ifndef StTpcLocalHit_recon_HH
#define StTpcLocalHit_recon_HH

#include "StLocalHit.hh"
#include <iostream.h>

class StTpcHit;

class StTpcLocalHit_recon : public StLocalHit {

public:
  StTpcLocalHit_recon(const StTpcHit* hit, float xLocal, float zGlobal);
  ~StTpcLocalHit_recon();

  const StTpcHit* globalHitPtr(){return mHit;};


private:
  const StTpcHit* mHit;   // points to the hit in the StTpcHit collection
};
ostream& operator<<(ostream&, const StTpcLocalHit_recon&);


#endif
