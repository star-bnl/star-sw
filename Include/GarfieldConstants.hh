
#ifndef G_GARFIELD_CONSTANTS_H
#define G_GARFIELD_CONSTANTS_H

namespace Garfield {

static const int DxcTypeElectron = -1;
static const int DxcTypePhoton   =  1;

// Collision types
static const int ElectronCollisionTypeElastic      = 0;
static const int ElectronCollisionTypeIonisation   = 1;
static const int ElectronCollisionTypeAttachment   = 2;
static const int ElectronCollisionTypeInelastic    = 3;
static const int ElectronCollisionTypeExcitation   = 4;
static const int ElectronCollisionTypeSuperelastic = 5; 

static const int PhotonCollisionTypeElastic    = 0;
static const int PhotonCollisionTypeIonisation = 1;
static const int PhotonCollisionTypeInelastic  = 2;
static const int PhotonCollisionTypeExcitation = 3;

// Status codes for drift lines
static const int StatusLeftDriftArea        =  -1; 
static const int StatusCalculationAbandoned =  -3;
static const int StatusLeftDriftMedium      =  -5;
static const int StatusAttached             =  -7;
static const int StatusBelowTransportCut    = -16;
static const int StatusOutsideTimeWindow    = -17;
static const double Small = 1.e-20;

static const double BoundaryDistance = 1.e-8;

// Conversion from Tesla to internal magnetic field units 
// compatible with cm and ns.
static const double Tesla2Internal = 1.e5;

}

#endif
