#ifndef ENERGYMESH_H
#define ENERGYMESH_H

#include <vector>

namespace Heed {

/// Energy mesh (in MeV, as everywhere in HEED, unless otherwise specified).
/// Internal calculations in HEED are conveniently performed
/// with some standard energy mesh. This mesh should be logarithmic or similar,
/// with dense spacing at small energies and sparse spacing at large energies.
/// The class below determines such mesh and some standard functions, namely
/// getting center of energy interval get_ec(long n), left edge of interval
/// get_e(long n) (right edge is left one for the next bin),
/// and some other simple functions.
/// There may be many meshes in program (but currently only one mesh
/// is used in a single program, different meshes may be used for totally
/// different matters and cross sections). The pointer
/// to mesh should be given as parameter of class constructor,
/// when a class depends on mesh.
///
/// The class keeps the left sides of bins and their centers in arrays.
/// Since the right side of interval is the left of the next one,
/// the array keeping the left sides should be by one larger then
/// the dimension of the mesh.
///
/// For reasons of speed the internal arrays keep right sides of intervals
/// and their centers are defined as simple fixed-size arrays.
///
/// 2003, I. Smirnov

class EnergyMesh {
 public:
  /// Default constructor.
  EnergyMesh() = default;
  /// Constructor from min./max energy and number of bins.
  EnergyMesh(double femin, double femax, long fq);
  /// Constructor from a list of energies.
  EnergyMesh(const std::vector<double>& fec);

  /// Return number of bins.
  inline long get_q() const { return q; }
  /// Return left side of the first bin.
  inline double get_emin() const { return emin; }
  /// Return right side of the last bin.
  inline double get_emax() const { return emax; }
  /// Return left side of a given bin.
  inline double get_e(long n) const { return e[n]; }
  /// Return center of a given bin.
  inline double get_ec(long n) const { return ec[n]; }
  /// Return all left sides.
  inline const double* get_ae(void) const { return e; }
  /// Return all interval centres.
  inline const double* get_aec(void) const { return ec; }

  long get_interval_number(const double ener) const;
  long get_interval_number_between_centers(const double ener) const;  // left
  friend std::ostream& operator<<(std::ostream& file, EnergyMesh& f);

  EnergyMesh* copy() const { return new EnergyMesh(*this); }
  void print(std::ostream& file, int l) const;

 private:
  /// qener-1 is maximal possible number of bins
  static const int pqener = 1001;
  /// Number of intervals
  long q = 0;
  /// Left side of the first interval
  double emin = 0.;
  /// Right side of the last interval
  double emax = 0.;
  /// Left side of interval, q + 1 numbers
  double e[pqener];
  /// Center of interval, q numbers
  double ec[pqener - 1];
};
}

#endif
