#ifndef HEEDDELTAELECTRONCS_H
#define HEEDDELTAELECTRONCS_H

#include <vector>

#include "heed++/code/HeedMatterDef.h"
#include "heed++/code/ElElasticScat.h"
#include "heed++/code/PairProd.h"
#include "wcpplib/random/PointsRan.h"

#define USE_MEAN_COEF  // new variant, means that used mean(1-cos(theta))
// for low angle scattering. It seems to be more straightforwad and
// may be very slightly more precise than the old variant
// that is use of sqrt( mean ( square (1-cos(theta)) ) )

namespace Heed {

/// Cross sections and various parameters for delta-electron transport.
/// 2003, I. Smirnov

class HeedDeltaElectronCS {
 public:
  /// Default constructor
  HeedDeltaElectronCS();
  /// Constructor
  HeedDeltaElectronCS(HeedMatterDef* fhmd, ElElasticScat* fees,
                      ElElasticScatLowSigma* feesls, PairProd* fpairprod,
                      int fsruth = 2, double fmlambda = 0.001 * 4.0e-3,
                      double fmthetac = 0.1);

  double get_sigma(double energy, double nscat) const;
  // copy of similar thing from ElElasticScatLowSigma

  void print(std::ostream& file, int l) const;
  HeedDeltaElectronCS* copy() const { return new HeedDeltaElectronCS(*this); }

  static constexpr long q_angular_mesh = 50;
  static constexpr double low_cut_angle_deg = 20.;

  HeedMatterDef* hmd = nullptr;
  ElElasticScat* ees = nullptr;
  ElElasticScatLowSigma* eesls = nullptr;
  PairProd* pairprod = nullptr;  // in eV
  /// Table of velocities
  std::vector<double> beta;
  /// Table of momenta [MeV/c]
  std::vector<double> momentum;

  /// Energy losses [MeV/cm] according to very advanced formula with
  /// Bethe-Bloch and density effect as in GEANT3,
  /// corresponding to centers of intervals of the common mesh.
  std::vector<double> eLoss;

  /// Mminimum mean length of range, multiplied by density.
  /// sm*gr/sm**3 = gr/sm**2
  double mlambda;
  /// Path length [cm] at the energy values of the mesh.
  /// For sruth == 2 mean free path.
  std::vector<double> lambda;
  /// Path length for low angle scatterings.
  /// This is without multiplication used for coef_low_sigma (cm).
  std::vector<double> low_lambda;

  /// Formula to use.
  ///  0 - usual multiple scattering formula
  ///  1 - Rutherford cross-section
  ///  2 - precise theory?
  int sruth;
  /// Minimum threshold turn angle.
  /// For Rutherford: the interactions with less angle will not be taken
  /// into account. The actual threshold angle can be larger.
  /// The second restriction is going from restriction of atomic shell.
  /// The third one is from mlamBdel.
  /// For usual multiple scattering:
  /// Assuming that sigma = mTetacBdel the path lengt is calculated.
  /// If mlamBdel/density is less then the last is used.
  double mthetac;

  /// Angular mesh, centers, angles in degrees.
  std::vector<double> angular_mesh_c;

  // index - energy mesh, angles in degrees
  std::vector<PointsRan> angular_points_ran;
  // index - energy mesh
  std::vector<PointsRan> low_angular_points_ran;

// introduce new name mean_... in order to avoid errors at replacement
#ifdef USE_MEAN_COEF
  std::vector<double> mean_coef_low_sigma;
#else
  std::vector<double> coef_low_sigma;
// index - energy, but (attention!) for mesh defined in ElElasticScat
// this is sigma of cos(theta) - 1.0, supposing that center is 1.0
#endif
};
}

#endif
