#ifndef ENERGYMESH_H
#define ENERGYMESH_H

#include "wcpplib/safetl/AbsArr.h"

/*
Internal calculations in HEED is convenient to perform
with some standard energy mesh. This mesh should be logarithmic
or similar, with dense points at little energies and sparse at large energies.
THe class below determines such mesh and some standard functions,
namely getting center of energy interval get_ec(long n),
left edge of interval get_e(long n) (right edge is left one for the next bin),
and some other simple functions.
There may be many meshes in program (but currently only one mesh
is used in a single program, different mashes may be used for totally
different matters and cross sections). The pointer
to mesh should be given as tha parameter of class constructor,
when a class depends on mesh.

The class keeps the left sides of bins and their centers in arrays.
Since the right side of interval is the left of the next one,
the array keeping the left sides should be by one larger then
the dimension of the mesh.

For speed the internal arrays keep right sides of intervals
and their centers are defined as simple arrays with dimension
const int pqener = 1001.
The actual size of mesh can be 1000 or less.
The violation of boundaries triggers error and termination.

2003, I. Smirnov

*/

// Energies in MeV, as everywhere in HEED (unless otherwise specified)

namespace Heed {

const int pqener = 1001;
// qener-1 is maximal possible quantity of bins
// In principle it is not clear whether it is better to use DynLinArr instead
// of simple arrays and maximal value

class EnergyMesh : public RegPassivePtr {
 public:
  EnergyMesh(void) : q(0), emin(0.0), emax(0.0) {}
  EnergyMesh(double femin, double femax, long fq);
  //EnergyMesh(int fq, double fec[pqener]);
  EnergyMesh(DynLinArr<double> fec);
  //EnergyMesh(const EnergyMesh& fem);
  macro_copy_total(EnergyMesh);
  virtual ~EnergyMesh() {}

  inline long get_q() const { return q; }
  inline double get_emin() const { return emin; }
  inline double get_emax() const { return emax; }
  /// left side of interval
  inline double get_e(long n) const { return e[n]; }  
  // center of interval
  inline double get_ec(long n) const { return ec[n]; }  
  // left sides
  inline const double* get_ae(void) const { return e; }  
  inline const double* get_aec(void) const { return ec; }

  long get_interval_number(double ener);
  long get_interval_number_between_centers(double ener);  // left
  friend std::ostream& operator<<(std::ostream& file, EnergyMesh& f);
  virtual void print(std::ostream& file, int l) const;

 private:
  // number of intervals
  long q;
  // left side of the first interval
  double emin;
  // right side of the last interval
  double emax;
  // left side of interval, q + 1 numbers
  double e[pqener];
  // center of interval, q numbers
  double ec[pqener - 1];

};

DynLinArr<double> make_log_mesh_ec(double emin, double emax, long q);

}

#endif
