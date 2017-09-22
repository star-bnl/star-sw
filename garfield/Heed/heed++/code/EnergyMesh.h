#ifndef ENERGYMESH_H
#define ENERGYMESH_H

#include <vector>
#include "wcpplib/safetl/AbsPtr.h"

namespace Heed {

/// Energy mesh (in MeV, as everywhere in HEED, unless otherwise specified).
/// Internal calculations in HEED are conveniently performed
/// with some standard energy mesh. This mesh should be logarithmic
// or similar, with dense points at small energies and sparse at large energies.
/// The class below determines such mesh and some standard functions,
/// namely getting center of energy interval get_ec(long n),
/// left edge of interval get_e(long n) (right edge is left one for the next
/// bin),
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
/// For speed the internal arrays keep right sides of intervals
/// and their centers are defined as simple arrays with dimension
/// const int pqener = 1001.
/// The actual size of mesh can be 1000 or less.
/// The violation of boundaries triggers error and termination.
///
/// 2003, I. Smirnov

class EnergyMesh : public RegPassivePtr {
 public:
  EnergyMesh() : q(0), emin(0.0), emax(0.0) {}
  EnergyMesh(double femin, double femax, long fq);
  EnergyMesh(const std::vector<double>& fec);
  virtual ~EnergyMesh() {}

  inline long get_q() const { return q; }
  inline double get_emin() const { return emin; }
  inline double get_emax() const { return emax; }
  /// left side of interval
  inline double get_e(long n) const { return e[n]; }
  /// center of interval
  inline double get_ec(long n) const { return ec[n]; }
  /// left sides
  inline const double* get_ae(void) const { return e; }
  inline const double* get_aec(void) const { return ec; }

  long get_interval_number(const double ener) const;
  long get_interval_number_between_centers(const double ener) const;  // left
  friend std::ostream& operator<<(std::ostream& file, EnergyMesh& f);

  virtual EnergyMesh* copy() const { return new EnergyMesh(*this); }
  virtual void print(std::ostream& file, int l) const;

 private:
  /// qener-1 is maximal possible number of bins
  static const int pqener = 1001;
  /// number of intervals
  long q;
  /// left side of the first interval
  double emin;
  /// right side of the last interval
  double emax;
  /// left side of interval, q + 1 numbers
  double e[pqener];
  /// center of interval, q numbers
  double ec[pqener - 1];
};

}

#endif
