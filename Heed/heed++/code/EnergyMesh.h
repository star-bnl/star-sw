#ifndef ENERGYMESH_H
#define ENERGYMESH_H

#include "wcpplib/safetl/AbsArr.h"
//#include "wcpplib/math/tline.h"

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

//const int pqener = 201;  
const int pqener = 1001;  
// qener-1 is maximal possible quantity of bins
// In principle it is not clear whether it is better to use DynLinArr instead
// of simple arrays and maximal value



class EnergyMesh: public RegPassivePtr
{public:
  EnergyMesh(void):q(0), emin(0.0), emax(0.0) {}
  EnergyMesh(double femin, double femax, long fq);
  //EnergyMesh(int fq, double fec[pqener]);
  EnergyMesh(DynLinArr< double > fec);
  //EnergyMesh(const EnergyMesh& fem);
  inline long get_q() const {return q;}
  inline double get_emin() const {return emin;}
  inline double get_emax() const {return emax;}
  inline double get_e(long n) const {return e[n];}   // left side of interval
  inline double get_ec(long n) const {return ec[n];} // center of interval
  inline const double* get_ae(void) const {return e;} // left sides
  inline const double* get_aec(void) const {return ec;} 
  // array of left sides of intervals
  long get_interval_number(double ener);
  long get_interval_number_between_centers(double ener);  // left
  friend ostream& operator<<(ostream& file, EnergyMesh& f );
  virtual void print(ostream& file, int l) const ;

  // For two folowing things we need to define copying
  //PointCoorMesh< double, double[pqener] > pcm_e;
  //PointCoorMesh< double, double[pqener-1] > pcm_ec;
  //EnergyMesh& operator=(const EnergyMesh& fem); 
  macro_copy_total(EnergyMesh);
  virtual ~EnergyMesh(){};
private:
  long q;     // number of intervals
  double emin;  // left side of the first interval
  double emax;  // right side of the last interval
  double e[pqener];  // left side of interval, q+1 numbers
  double ec[pqener-1]; // center of interval, q numners

};

DynLinArr< double > make_log_mesh_ec(double emin, double emax, long q);

#endif
