#ifndef BGMESH_H
#define BGMESH_H

#include <vector>

#include "wcpplib/safetl/AbsPtr.h"

namespace Heed {

class BGMesh : public RegPassivePtr {
 public:
  BGMesh() : xmin(0.0), xmax(0.0), q(0), x(0) {}
  BGMesh(double fxmin, double fxmax, long fq);
  double xmin;
  double xmax;
  /// Total number of points (not number of intervals).
  long q;
  std::vector<double> x;
  virtual void print(std::ostream& file, int l) const;
   virtual BGMesh* copy() const { return new BGMesh(*this); }
};
std::ostream& operator<<(std::ostream& file, const BGMesh& bgm);

}

#endif
