#include <cmath>
#include <iomanip>
#include "wcpplib/util/FunNameStack.h"
#include "heed++/code/EnergyMesh.h"

namespace Heed {

EnergyMesh::EnergyMesh(double femin, double femax, long fq)
    : q(fq), emin(femin), emax(femax) {
  mfunname("EnergyMesh::EnergyMesh(double femin, double femax, long fq)");
  check_econd21(q, < 0 ||, > pqener - 1, mcerr);

  const double rk = pow(emax / emin, (1.0 / double(q)));
  double er = emin;
  e[0] = er;
  for (long n = 1; n < q + 1; n++) {
    e[n] = er * rk;
    ec[n - 1] = (e[n - 1] + e[n]) * 0.5;
    er = e[n];
  }
}

EnergyMesh::EnergyMesh(const std::vector<double>& fec) : q(fec.size()) {
  mfunname("std::vector< double > fec");
  check_econd21(q, < 0 ||, > pqener - 1, mcerr);
  check_econd11(q, != 1, mcerr);  // otherwise problems with emin/emax
  if (q <= 0) {
    emin = 0.0;
    emax = 0.0;
    return;
  }
  emin = fec[0] - (fec[1] - fec[0]) / 2.0;
  emax = fec[q - 1] + (fec[q - 1] - fec[q - 2]) / 2.0;
  e[0] = emin;
  e[q] = emax;

  for (long n = 0; n < q; n++) {
    ec[n] = fec[n];
  }
  for (long n = 1; n < q; n++) {
    e[n] = 0.5 * (fec[n - 1] + fec[n]);
  }
}

long EnergyMesh::get_interval_number(const double ener) const {
  if (ener < emin) return -1;
  if (ener > emax) return q;

  long n1 = 0;
  long n2 = q;  // right side of last
  while (n2 - n1 > 1) {
    const long n3 = n1 + ((n2 - n1) >> 1);
    if (ener < e[n3]) {
      n2 = n3;
    } else {
      n1 = n3;
    }
  }
  return n1;
}

long EnergyMesh::get_interval_number_between_centers(const double ener) const {
  if (ener < ec[0]) return -1;
  if (ener > ec[q - 1]) return q;

  long n1 = 0;
  long n2 = q - 1;  // right side of last
  while (n2 - n1 > 1) {
    const long n3 = n1 + ((n2 - n1) >> 1);
    if (ener < ec[n3]) {
      n2 = n3;
    } else {
      n1 = n3;
    }
  }
  return n1;
}

std::ostream& operator<<(std::ostream& file, EnergyMesh& f) {
  Ifile << "EnergyMesh: \n";
  indn.n += 2;
  Ifile << "emin=" << f.emin << " emax=" << f.emax
        << " number of intervals=" << f.q << '\n'
        << " maximal number of intervals=" << EnergyMesh::pqener << '\n';
  Ifile << " bin  left side        center       right side       width\n";
  for (int n = 0; n < f.q; n++) {
    Ifile << std::setw(5) << n << std::setw(15) << f.e[n] << std::setw(15)
          << f.ec[n] << std::setw(15) << f.e[n + 1] << std::setw(15)
          << (f.e[n + 1] - f.e[n]) << '\n';
  }
  indn.n -= 2;
  return file;
}

void EnergyMesh::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "EnergyMesh (l=" << l << "): \n";
  indn.n += 2;
  Ifile << "emin=" << emin << " emax=" << emax << " quantity of intervals=" << q
        << '\n' << " maximal possible quantity of intervals=" << pqener << '\n';
  if (l > 1) {
    Ifile << " number  left side        center       right side       widht\n";
    for (int n = 0; n < q; n++) {
      Ifile << std::setw(5) << n << std::setw(15) << e[n] << std::setw(15)
            << ec[n] << std::setw(15) << e[n + 1] << std::setw(15)
            << (e[n + 1] - e[n]) << '\n';
    }
  }
  indn.n -= 2;
}
}
