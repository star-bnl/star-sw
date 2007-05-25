#include "StGammaCandidate.h"

ClassImp(StGammaCandidate);


// ------------------------------------------------------- class constructor --
StGammaCandidate::StGammaCandidate()
{
  SetId(0);
  SetTowerId(0);
  SetTowerClusterId(0);
  SetSmduClusterId(0);
  SetSmdvClusterId(0);
  SetDetectorId(0);

  SetMomentum( TVector3(0.,0.,0.) );
  SetPosition( TVector3(0.,0.,0.) );
  SetEnergy(0.);
  SetSeedEnergy(0.);
  SetPre1Energy(0.);
  SetPre2Energy(0.);
  SetPostEnergy(0.);
  SetSmduEnergy(0.);
  SetSmdvEnergy(0.);

}

// -------------------------------------------------------- class destructor --
StGammaCandidate::~StGammaCandidate()
{

  
}
