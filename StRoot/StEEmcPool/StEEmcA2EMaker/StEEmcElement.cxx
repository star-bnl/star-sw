/**
 * \class StEEmcElement
 * \brief Base class for EEMC detectors
 *
 * This class provides the common functionality for EEMC detectors.
 * Each detector will have a raw adc value, ped-subtracted adc value,
 * gain-corrected adc value (aka energy) as well as status bits (fail
 * and stat) and a pointer to the corresponding StEmcRawHit when 
 * processing StEvent files.
 *
 * \author Jason C. Webb
 * $Date: 2005/08/23 16:55:31 $
 * $Revision: 1.1 $
 *
 */

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
