
#ifndef G_GARFIELD_CONSTANTS_H
#define G_GARFIELD_CONSTANTS_H

namespace Garfield {

static const int DxcTypeElectron = -1;
static const int DxcTypePhoton   =  1;

// Status codes
static const int StatusLeftDriftArea        =  -1; 
static const int StatusCalculationAbandoned =  -3;
static const int StatusLeftDriftMedium      =  -5;
static const int StatusAttached             =  -7;
static const int StatusBelowTransportCut    = -16;
static const int StatusOutsideTimeWindow    = -17;
static const double Small = 1.e-20;

static const double BoundaryDistance = 1.e-8;

}

#endif
