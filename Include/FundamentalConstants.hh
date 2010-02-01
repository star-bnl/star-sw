// Fundamental mathematical and physical constants
// Source: C. Amsler et al., The Review of Particle Physics, 
//         Physics Letters B667, 1 (2008)

#ifndef G_FUNDAMENTAL_CONSTANTS_H
#define G_FUNDAMENTAL_CONSTANTS_H

namespace Garfield {

static const double     Pi = 3.1415926535897932384626433832795;
static const double  TwoPi = 2. * Pi;
static const double HalfPi = 0.5 * Pi;
static const double    Pi2 = Pi * Pi;

// Elementary particle masses [eV / c2]
static const double ElectronMass =   0.510998910e6;
static const double MuonMass     = 105.658367e6;
static const double ProtonMass   = 938.27203e6;
static const double NeutronMass  = 939.56536e6;

static const double FineStructureConstant   =   7.2973525376e-3;
static const double HbarC                   = 197.3269631e-7;     // eV cm
static const double ElectronMassGramme      =   9.10938215e-28;   // g
static const double AtomicMassUnit          =   1.660538782e-24;  // g
static const double BohrRadius              =   0.52917720859e-8; // cm
static const double RydbergEnergy           =  13.60569193;       // eV
static const double SpeedOfLight            =  29.9792458;        // cm / ns
static const double ClassicalElectronRadius =   2.8179402894e-13; // cm
static const double ElementaryCharge        =   1.602176487e-19;  // C
static const double BoltzmannConstant       =   8.617343e-5;      // eV / K
static const double VacuumPermittivity      =   8.854187817e-14;  // F / cm
static const double LoschmidtNumber         =   2.6867774e19;     // cm-3
static const double AvogadroConstant        =   6.02214179e23;    // mol-1
static const double AtmosphericPressure     = 760.;               // Torr
static const double FourPiEpsilon0          = 4. * Pi * VacuumPermittivity;
static const double TwoPiEpsilon0           = TwoPi * VacuumPermittivity;

static const double Small = 1.e-20;

}

#endif
