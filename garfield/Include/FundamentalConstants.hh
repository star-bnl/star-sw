// Fundamental mathematical and physical constants
// Source: K. Nakamura et al., The Review of Particle Physics,
//         Journal of Physics G 37 075021 (2010)

#ifndef G_FUNDAMENTAL_CONSTANTS_H
#define G_FUNDAMENTAL_CONSTANTS_H

namespace Garfield {

static const double Pi = 3.1415926535897932384626433832795;
static const double TwoPi = 2. * Pi;
static const double HalfPi = 0.5 * Pi;
static const double Pi2 = Pi * Pi;

// Euler-Mascheroni constant
static const double Gamma = 0.577215664901532861;

static const double CLog2 = 0.693147180559945309417;
static const double Sqrt2 = 1.4142135623730950488016887242097;

// Elementary particle masses [eV / c2]
static const double ElectronMass = 0.510998910e6;
static const double MuonMass = 105.658367e6;
static const double ProtonMass = 938.272013e6;
static const double NeutronMass = 939.56536e6;

// Fine structure constant
static const double FineStructureConstant = 7.2973525376e-3;
// Reduced Planck constant [eV ns]
static const double Hbar = 6.58211899e-7;
// Conversion constant [eV cm]
static const double HbarC = 197.3269631e-7;
// Electron mass [g]
static const double ElectronMassGramme = 9.10938215e-28;
// Unified atomic mass unit (mass 12C atom / 12) [g]
static const double AtomicMassUnit = 1.660538782e-24;
static const double AtomicMassUnitElectronVolt = 931.494028e6;
// Bohr radius [cm]
static const double BohrRadius = 0.52917720859e-8;
// Rydberg energy [eV]
static const double RydbergEnergy = 13.60569193;
// Speed of light in vacuum [cm / ns]
static const double SpeedOfLight = 29.9792458;
// Classical electron radius [cm]
static const double ClassicalElectronRadius = 2.8179402894e-13;
// Electron charge magnitude [fC]
static const double ElementaryCharge = 1.602176487e-4;
// Electron cyclotron frequency / field [rad / (ns * 10-5 T)]
static const double OmegaCyclotronOverB = 1.758820150e-3;
// Boltzmann constant [eV / K]
static const double BoltzmannConstant = 8.617343e-5;
// Permittivity of free space [fF / cm]
static const double VacuumPermittivity = 88.54187817;
static const double FourPiEpsilon0 = 4. * Pi * VacuumPermittivity;
static const double TwoPiEpsilon0 = TwoPi * VacuumPermittivity;
// Loschmidt number [cm-3]
static const double LoschmidtNumber = 2.6867774e19;
// AvogadroConstant [mol-1]
static const double AvogadroConstant = 6.02214179e23;
// Atmospheric pressure [Torr]
static const double AtmosphericPressure = 760.;
// Zero degree Celsius [K]
static const double ZeroCelsius = 273.15;
}

#endif
