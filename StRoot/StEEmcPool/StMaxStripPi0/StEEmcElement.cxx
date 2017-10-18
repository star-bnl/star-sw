#include "StEEmcElement.h"

ClassImp(StEEmcElement);

// ----------------------------------------------------------------------------
StEEmcElement::StEEmcElement()
{
  stemc(0);
}

// ----------------------------------------------------------------------------
void StEEmcElement::Clear(Option_t *opts)
{
  raw(0.);
  adc(0.);
  energy(0.);
  stat(0);
  fail(0); 
  stemc(0);
}
