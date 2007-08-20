#include "StGammaTower.h"

ClassImp(StGammaTower);

StGammaTower::StGammaTower(){ /* nada */ };


// Mapping between endcap index and sector, subsector, etabin is defined
// in StEEmcA2EMaker::index(...)

Int_t StGammaTower::sector()
{
  // endcap tower
  if ( ((Int_t)layer) < 4 )
    {
      return phibin() / 5;
    }
  else
    return -1;

}

Int_t StGammaTower::subsector()
{

  // endcap tower
  if ( ((Int_t)layer) < 4 )
    {
      return phibin() % 5;
    }
  else
    return -1;

}

Int_t StGammaTower::etabin()
{
  // endcap tower
  if ( ((Int_t)layer) < 4 ) 
    {
      return id % 12;
    }
  else
    return -1;
}

Int_t StGammaTower::phibin()
{
  // endcap tower
  if ( ((Int_t)layer) < 4 ) 
    {
      return id / 12;
    }
  else 
    return -1;
}





