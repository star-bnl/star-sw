#ifndef __eeTowerFunction_h__
#define __eeTowerFunction_h__

/**
 * A functional form describing energy sharing between EEMC towers
 * based on position of the hit photon/electron.
 *
 * x[0]=xphi position of tower center
 * x[1]=xeta position of tower center
 *
 * p[0]=xphi position of gamma/e hit
 * p[1]=xeta position of gamma/e hit
 * p[3]=fit energy response
 *
 */

#include "TSpline.h"
Double_t eeTowerFunction ( Double_t *x, Double_t *p );
Double_t eeTower2Function ( Double_t *x, Double_t *p );
#endif
