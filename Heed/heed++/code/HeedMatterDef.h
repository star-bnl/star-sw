#ifndef HEEDMATTERDEF_H
#define HEEDMATTERDEF_H

#include <vector>

#include "wcpplib/matter/MatterDef.h"
#include "wcpplib/matter/GasDef.h"
#include "heed++/code/EnergyMesh.h"
#include "heed++/code/PhotoAbsCS.h"

namespace Heed {

/// Definition of matter parameters necessary for HEED.
/// This is photoabsorption cross section, dielectric constant
/// and other parameters related to these.
/// All the parameters depending on energy are kept in arrays
/// associated with specific energy mesh, which should be defined.
///
/// The principle is ordinary: definition of just a class.
/// To the contrary with wcpplib/matter, there is no any global "database"
/// and no formal ban to duplicate these definitions
/// (although there would not be sense in duplication).
///
// 2003, I. Smirnov

class HeedMatterDef {
 public:
  MatterDef* matter = nullptr;
  std::vector<const AtomPhotoAbsCS*> apacs;
  // Each element of this array corresponds to component of matter
  double eldens_cm_3 = 0.; ///< Electron density cm**-3
  double eldens = 0.;      ///< Electron density MeV**3
  double xeldens = 0.;     ///< Long. electron density MeV**2/cm (for x=1 cm).
  double wpla = 0.;        ///< Squared plasma energy;
  double radiation_length = 0.;  ///< Radiation Length.
  double Rutherford_const = 0.;  ///< Const for Rutherford cross section (1/cm3).
  double W = 0.;           ///< Mean work per pair production, MeV
  double F = 0.;           ///< Fano factor
  EnergyMesh* energy_mesh = nullptr;

  // The physical definition of two previous arrays of values:
  // mean values of cross sections for each energy interval.
  std::vector<double> ACS;  ///< Photoabsorption cross section per one atom(Mb).
  std::vector<double> ICS;  ///< Photoionization cross section per one atom(Mb).
  /// Some plasma dielectric constant (not used, but just initialized for print)
  // In order to take into account bounds,
  // one has to multiply this by some integral.
  std::vector<double> epsip;
  /// Real part of dielectric constant (e_1 - 1).
  std::vector<double> epsi1;
  /// Imaginary part of dielectric constant.
  std::vector<double> epsi2;
  /// Minimum ionization potential.
  /// It is used only for switching off the Cherenkov radiation below it.
  double min_ioniz_pot = 0.;

  /// Default constructor.
  HeedMatterDef() = default;
  /// Constructor.
  /// If fW == 0.0, the program takes mean W from
  /// molecules for gas or from atoms for matters.
  /// If fF is input as 0.0, it is assigned to be mean for gas.
  /// For matters this is the terminating error.
  HeedMatterDef(EnergyMesh* fenergy_mesh, MatterDef* amatter,
                const std::vector<AtomPhotoAbsCS*>& faapacs,
                double fW = 0.0, double fF = standard_factor_Fano);
  // Gas consists of molecules, molecules of atoms
  // The order in which molecules appear in fampacs should correspond
  // to that of agas.
  // The order in which atoms appear in fampacs[n] should correspond to that
  // of molecules in gas.
  HeedMatterDef(EnergyMesh* fenergy_mesh, GasDef* agas,
                const std::vector<MolecPhotoAbsCS*>& fampacs,
                double fW = 0.0, double fF = standard_factor_Fano);
  HeedMatterDef(EnergyMesh* fenergy_mesh, const std::string& gas_notation,
                const std::vector<MolecPhotoAbsCS*>& fampacs,
                double fW = 0.0, double fF = standard_factor_Fano);
  // Replace permeability (epsi1 and epsi2) by the numbers
  // calculated by another program and written to a file (only for debug)
  void replace_epsi12(const std::string& file_name);
  void print(std::ostream& file, int l) const;
  HeedMatterDef* copy() const { return new HeedMatterDef(*this); }

  /// Flag affecting mixtures of atoms with different ionization potentials.
  /// If 1, all energy transfers what is absorbed even with the energy less than
  /// the potential of ionization of single atom, but more than the minimal
  /// potential of ionization of the mixture, should ionize.
  /// This is emulation of Jesse effect in extreme case.
  /// It is likely not realistic. So the recommended value is 0.
  static constexpr int s_use_mixture_thresholds = 0;

 private:
  // Initialization after assignment of matter and apacs
  void inite_HeedMatterDef();
};
}

#endif
