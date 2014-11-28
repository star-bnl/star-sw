/**
 * \function eeSinglePeak
 * \brief Function which describes the SMD response to an EM shower
 *
 * The transverse profile of an EM shower is often parameterized as
 * a double-gaussian with constrained means and constrained widths.
 * This function implements that line-shape, and is useful for driving
 * fits in ROOT.
 *
 */

#include "eeSinglePeak.h"
#include <TMath.h>
#include <iostream>

Double_t eeSinglePeak ( Double_t *X, Double_t *P ) 
{
  // Shower-shape at shower-max: two gaussians with constrained means
  // and widths.

  Double_t x = X[0];
  Double_t mean = P[1];

  // Coefficient of first gaussian
  Double_t a = 1.0-P[3];

  // Coefficient of second gaussian
  Double_t b = P[3];

  // Width of first gaussian
  Double_t w1 = P[2];
  
  // Width of second gaussian
  Double_t w2 = P[2]*P[4];

  // Sum of two (normalized) gaussians
  Double_t g =
    a * TMath::Gaus( x, mean, w1, kTRUE ) + 
    b * TMath::Gaus( x, mean, w2, kTRUE );
          
  return P[0] * g;
  
};

Double_t eeDoublePeak ( Double_t *X, Double_t *P )
{
  
  Double_t P1[] = { P[0], P[1], P[2], P[3], P[4] };
  Double_t P2[] = { P[5], P[6], P[7], P[8], P[9] };

  return eeSinglePeak( X, P1 ) + eeSinglePeak( X, P2 );

}
