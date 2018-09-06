#include <cmath>

#include "wcpplib/util/FunNameStack.h"
#include "heed++/code/BGMesh.h"

namespace Heed {

BGMesh::BGMesh(double fxmin, double fxmax, long fq)
    : xmin(fxmin), xmax(fxmax), q(fq) {

  mfunname("BGMesh::BGMesh(double fxmin, double fxmax, long fq)");
  // The minimum is one interval and two points.
  check_econd11(fq, <= 1, mcerr);
  const double rk = pow(fxmax / fxmin, 1. / double(fq - 1));
  x.resize(fq);
  x[0] = fxmin;
  x[fq - 1] = fxmax;
  double xr = fxmin;
  for (long n = 1; n < fq - 1; n++) {
    xr *= rk;
    x[n] = xr;
  }
}

void BGMesh::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "BGMesh (l=" << l << "): \n";
  indn.n += 2;
  Ifile << "xmin=" << xmin << " xmax=" << xmax << " quantity of intervals=" << q
        << '\n';
  if (l > 1) {
    for (long k = 1; k < q; ++k) Iprintn(mcout, x[k]);
  }
  indn.n -= 2;
}

std::ostream& operator<<(std::ostream& file, const BGMesh& bgm) {
  Ifile << "operator<<(std::ostream& file, const BGMesh& bgm):\n";
  bgm.print(file, 2);
  return file;
}
}
