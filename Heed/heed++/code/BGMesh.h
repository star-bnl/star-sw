#ifndef BGMESH_H
#define BGMESH_H

#include "wcpplib/safetl/AbsArr.h"

// Here q is the total number of points (not interval).
// Minimum one interval and two points

class BGMesh: public RegPassivePtr
{public:
  BGMesh(void): xmin(0.0), xmax(0.0), q(0), x(0) {}
  BGMesh(double fxmin, double fxmax, long fq);
  double xmin;
  double xmax;
  long q;
  DynLinArr< double > x;
  virtual void print(ostream& file, int l) const ;
  macro_copy_total(BGMesh);

};

ostream& operator<<(ostream& file, const BGMesh& bgm);

DynLinArr< double > make_log_mesh(double fxmin, double fxmax, long fq);

#endif
