#include "StGammaTower.h"

ClassImp(StGammaTower);


// Mapping between endcap index and sector, subsector, etabin is defined
// in StEEmcA2EMaker::index(...)

//////////////////////////////////////////////////
//   Calculate sector, subsector, etabin, and   //
//   phibin for EEMC towers.  The mapping is    //
//      defined in StEEmcA2EMaker::index()      //
//////////////////////////////////////////////////

Int_t StGammaTower::sector()
{
    // Require an endcap tower
    if( ((Int_t)layer) < 4) return phibin() / 5;
    else return -1;
}

Int_t StGammaTower::subsector()
{
  /// Require an endcap tower
  if( ((Int_t)layer) < 4) return phibin() % 5;
  else return -1;
}

Int_t StGammaTower::etabin()
{
    // Require an endcap tower
    if( ((Int_t)layer) < 4) return id % 12;
    else return -1;
}

Int_t StGammaTower::phibin()
{
    // Require an endcap tower
    if( ((Int_t)layer) < 4) return id / 12;
    else return -1;
}





