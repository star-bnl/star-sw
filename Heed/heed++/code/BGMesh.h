#ifndef BGMESH_H
#define BGMESH_H

#include <vector>

namespace Heed {

/// Mesh of \f$\beta\gamma\f$ values.

class BGMesh {
 public:
  BGMesh() = default;
  BGMesh(double fxmin, double fxmax, long fq);
  double xmin = 0.;
  double xmax = 0.;
  /// Total number of points (not number of intervals).
  long q = 0;
  std::vector<double> x;
  void print(std::ostream& file, int l) const;
  BGMesh* copy() const { return new BGMesh(*this); }
};
std::ostream& operator<<(std::ostream& file, const BGMesh& bgm);
}

#endif
