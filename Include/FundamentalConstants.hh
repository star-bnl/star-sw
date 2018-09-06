// Fundamental mathematical and physical constants
// Source: K. Nakamura et al., The Review of Particle Physics,
//         Journal of Physics G 37 075021 (2010)

#ifndef G_FUNDAMENTAL_CONSTANTS_H
#define G_FUNDAMENTAL_CONSTANTS_H

namespace Garfield {

static constexpr double Pi = 3.1415926535897932384626433832795;
static constexpr double TwoPi = 2. * Pi;
static constexpr double HalfPi = 0.5 * Pi;
static constexpr double Pi2 = Pi * Pi;

static constexpr double DegreeToRad = Pi / 180.;
static constexpr double RadToDegree = 180. / Pi;

// Euler-Mascheroni constant
static constexpr double Gamma = 0.577215664901532861;

static constexpr double CLog2 = 0.693147180559945309417;
static constexpr double Sqrt2 = 1.4142135623730950488016887242097;

// Elementary particle masses [eV / c2]
static constexpr double ElectronMass = 0.510998910e6;
static constexpr double MuonMass = 105.658367e6;
static constexpr double ProtonMass = 938.272013e6;
static constexpr double NeutronMass = 939.56536e6;

// Fine structure constant
static constexpr double FineStructureConstant = 7.2973525376e-3;
// Reduced Planck constant [eV ns]
static constexpr double Hbar = 6.58211899e-7;
// Conversion constant [eV cm]
static constexpr double HbarC = 197.3269631e-7;
// Electron mass [g]
static constexpr double ElectronMassGramme = 9.10938215e-28;
// Unified atomic mass unit (mass 12C atom / 12) [g]
static constexpr double AtomicMassUnit = 1.660538782e-24;
static constexpr double AtomicMassUnitElectronVolt = 931.494028e6;
// Bohr radius [cm]
static constexpr double BohrRadius = 0.52917720859e-8;
// Rydberg energy [eV]
static constexpr double RydbergEnergy = 13.60569193;
// Speed of light in vacuum [cm / ns]
static constexpr double SpeedOfLight = 29.9792458;
// Classical electron radius [cm]
static constexpr double ClassicalElectronRadius = 2.8179402894e-13;
// Electron charge magnitude [fC]
static constexpr double ElementaryCharge = 1.602176487e-4;
// Electron cyclotron frequency / field [rad / (ns * 10-5 T)]
static constexpr double OmegaCyclotronOverB = 1.758820150e-3;
// Boltzmann constant [eV / K]
static constexpr double BoltzmannConstant = 8.617343e-5;
// Permittivity of free space [fF / cm]
static constexpr double VacuumPermittivity = 88.54187817;
static constexpr double FourPiEpsilon0 = 4. * Pi * VacuumPermittivity;
static constexpr double TwoPiEpsilon0 = TwoPi * VacuumPermittivity;
// Loschmidt number [cm-3]
static constexpr double LoschmidtNumber = 2.6867774e19;
// AvogadroConstant [mol-1]
static constexpr double AvogadroConstant = 6.02214179e23;
// Atmospheric pressure [Torr]
static constexpr double AtmosphericPressure = 760.;
// Zero degree Celsius [K]
static constexpr double ZeroCelsius = 273.15;
}

#endif
