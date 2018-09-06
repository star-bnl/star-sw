#include <iomanip>
#include "wcpplib/matter/AtomDef.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/util/FunNameStack.h"

// 1998-2004, I. Smirnov.

namespace Heed {

using CLHEP::gram;
using CLHEP::mole;
using CLHEP::Avogadro;

void AtomDef::print(std::ostream& file, int l) const {
  if (l > 0) file << (*this);
}

void AtomDef::printall(std::ostream& file) {
  Ifile << "AtomDef::printall:\n";
  for (auto atom : AtomDef::get_logbook()) file << atom;
}

AtomDef::AtomDef() {
  AtomDef::get_logbook().push_back(this);
}

AtomDef::AtomDef(const std::string& fnameh, const std::string& fnotationh,
                 int fZh, double fAh)
    : nameh(fnameh), notationh(fnotationh), Zh(fZh), Ah(fAh) {
  mfunname("AtomDef::AtomDef(...)");
  check_econd21(fZh, < 1 ||, > max_poss_atom_z, mcerr);
  verify();
  AtomDef::get_logbook().push_back(this);
}

double AtomDef::get_A(int fZ) {
  mfunnamep("double AtomDef::get_A(int fZ)");
  for (auto atom : AtomDef::get_logbook()) {
    if (atom->Z() == fZ) return atom->A();
  }
  funnw.ehdr(mcerr);
  mcerr << "Atom is not found, Z=" << fZ << '\n';
  spexit(mcerr);
  return 0.0;
}

AtomDef* AtomDef::get_AtomDef(int fZ) {
  mfunnamep("AtomDef* AtomDef::get_AtomDef(int fZ)");
  for (auto atom : AtomDef::get_logbook()) {
    if (atom->Z() == fZ) return atom;
  }
  funnw.ehdr(mcerr);
  mcerr << "Atom is not found, Z=" << fZ << '\n';
  spexit(mcerr);
  return nullptr;
}

void AtomDef::verify() {
  mfunnamep("void AtomDef::verify()");
  if (nameh == "none" && notationh == "none") return;
  for (auto atom : AtomDef::get_logbook()) {
    if (atom->nameh != nameh && atom->notationh != notationh) continue;
    funnw.ehdr(mcerr);
    mcerr << "cannot initialize two atoms with the same name or notation\n";
    mcerr << "name=" << nameh << " notation=" << notationh << '\n';
    spexit(mcerr);
  }
}

std::ostream& operator<<(std::ostream& file, const AtomDef& f) {
  Ifile << "AtomDef: name=" << std::setw(10) << f.name()
        << " notation=" << std::setw(3) << f.notation();
  Ifile << " Z()=" << std::setw(3) << f.Z()
        << " A()/(gram/mole)=" << f.A() / (gram / mole) << '\n';
  return file;
}

std::list<AtomDef*>& AtomDef::get_logbook() {
  static std::list<AtomDef*> logbook;
  return logbook;
}

const std::list<AtomDef*>& AtomDef::get_const_logbook() {
  return AtomDef::get_logbook();
}

AtomDef* AtomDef::get_AtomDef(const std::string& fnotation) {
  for (auto atom : AtomDef::get_logbook()) {
    if (atom->notation() == fnotation) return atom;
  }
  return nullptr;
}

AtomDef::~AtomDef() { AtomDef::get_logbook().remove(this); }

AtomMixDef::AtomMixDef(unsigned long fqatom,
                       const std::vector<std::string>& fatom_not,
                       const std::vector<double>& fweight_quan)
    : qatomh(fqatom),
      atomh(fqatom, nullptr),
      weight_quanh(fqatom, 0.0),
      weight_massh(fqatom, 0.0) {
  mfunnamep("AtomMixDef::AtomMixDef(...)");
  check_econd11(fqatom, <= 0, mcerr);
  check_econd12(fqatom, >, fatom_not.size(), mcerr);
  check_econd12(fqatom, >, fweight_quan.size(), mcerr);

  for (long n = 0; n < qatomh; ++n) {
    AtomDef* ad = AtomDef::get_AtomDef(fatom_not[n]);
    if (!ad) {
      funnw.ehdr(mcerr);
      mcerr << "cannot find atom with notation " << fatom_not[n]
            << "\nIn particular, check the sequence of initialization\n";
      spexit(mcerr);
    }
    atomh[n] = ad;
  }
  double s = 0.0;
  for (long n = 0; n < qatomh; n++) {
    weight_quanh[n] = fweight_quan[n];
    check_econd11(weight_quanh[n], <= 0, mcerr);
    s += weight_quanh[n];
  }
  check_econd11(s, <= 0, mcerr);
  if (s != 1.0) {
    for (long n = 0; n < qatomh; n++) {
      weight_quanh[n] /= s;
    }
  }
  for (long n = 0; n < qatomh; n++) {
    weight_massh[n] = weight_quanh[n] * atomh[n]->A();
  }
  s = 0.0;
  for (long n = 0; n < qatomh; n++) {
    s += weight_massh[n];
  }
  check_econd11(s, <= 0, mcerr);
  if (s != 1.0) {
    for (long n = 0; n < qatomh; n++) {
      weight_massh[n] /= s;
    }
  }
  for (long n = 0; n < qatomh; n++) {
    Z_meanh += atomh[n]->Z() * weight_quanh[n];
    A_meanh += atomh[n]->A() * weight_quanh[n];
    inv_A_meanh += (1.0 / atomh[n]->A()) * weight_quanh[n];
  }
  mean_ratio_Z_to_Ah = Z_meanh / A_meanh;
  NumberOfElectronsInGramh = mean_ratio_Z_to_Ah * (gram / mole) * Avogadro;
}

AtomMixDef::AtomMixDef(unsigned long fqatom,
                       const std::vector<std::string>& fatom_not,
                       const std::vector<long>& fweight_quan)
    : qatomh(fqatom),
      atomh(fqatom, nullptr),
      weight_quanh(fqatom, 0.0),
      weight_massh(fqatom, 0.0) {
  mfunnamep("AtomMixDef::AtomMixDef(...)");
  check_econd11(fqatom, <= 0, mcerr);
  check_econd12(fqatom, >, fatom_not.size(), mcerr);
  check_econd12(fqatom, >, fweight_quan.size(), mcerr);

  for (long n = 0; n < qatomh; ++n) {
    AtomDef* ad = AtomDef::get_AtomDef(fatom_not[n]);
    if (!ad) {
      funnw.ehdr(mcerr);
      mcerr << "cannot find atom with notation " << fatom_not[n]
            << "\nIn particular, check the sequence of initialization\n";
      spexit(mcerr);
    }
    atomh[n] = ad;
  }
  double s = 0.0;
  for (long n = 0; n < qatomh; n++) {
    weight_quanh[n] = fweight_quan[n];
    check_econd11(weight_quanh[n], <= 0, mcerr);
    s += weight_quanh[n];
  }
  check_econd11(s, <= 0, mcerr);
  if (s != 1.0) {
    for (long n = 0; n < qatomh; n++) {
      weight_quanh[n] /= s;
    }
  }
  for (long n = 0; n < qatomh; n++) {
    weight_massh[n] = weight_quanh[n] * atomh[n]->A();
  }
  s = 0.0;
  for (long n = 0; n < qatomh; n++) {
    s += weight_massh[n];
  }
  check_econd11(s, <= 0, mcerr);
  if (s != 1.0) {
    for (long n = 0; n < qatomh; n++) {
      weight_massh[n] /= s;
    }
  }
  for (long n = 0; n < qatomh; n++) {
    Z_meanh += atomh[n]->Z() * weight_quanh[n];
    A_meanh += atomh[n]->A() * weight_quanh[n];
    inv_A_meanh += (1.0 / atomh[n]->A()) * weight_quanh[n];
  }
  mean_ratio_Z_to_Ah = Z_meanh / A_meanh;
  NumberOfElectronsInGramh = mean_ratio_Z_to_Ah * (gram / mole) * Avogadro;
}

// one atom in mixture
AtomMixDef::AtomMixDef(const std::string& fatom_not)
    : qatomh(1),
      atomh(1, nullptr),
      weight_quanh(1, 1.),
      weight_massh(1, 1.) {
  mfunnamep("AtomMixDef::AtomMixDef(...)");
  AtomDef* ad = AtomDef::get_AtomDef(fatom_not);
  if (!ad) {
    funnw.ehdr(mcerr);
    mcerr << "cannot find atom with notation " << fatom_not
          << "\nIn particular, check the sequence of initialization\n";
    spexit(mcerr);
  }
  atomh[0] = ad;

  weight_quanh[0] = 1.0;
  weight_massh[0] = 1.0;

  Z_meanh += atomh[0]->Z();
  A_meanh += atomh[0]->A();
  inv_A_meanh += (1.0 / atomh[0]->A());
  mean_ratio_Z_to_Ah = Z_meanh / A_meanh;
  NumberOfElectronsInGramh = mean_ratio_Z_to_Ah * (gram / mole) * Avogadro;
}

// two atoms
AtomMixDef::AtomMixDef(const std::string& fatom_not1, double fweight_quan1,
                       const std::string& fatom_not2, double fweight_quan2)
    : qatomh(2),
      atomh(2, nullptr),
      weight_quanh(2),
      weight_massh(2) {
  mfunnamep("AtomMixDef::AtomMixDef(...)");
  std::vector<std::string> fatom_not(2);
  fatom_not[0] = fatom_not1;
  fatom_not[1] = fatom_not2;

  for (long n = 0; n < qatomh; ++n) {
    AtomDef* ad = AtomDef::get_AtomDef(fatom_not[n]);
    if (!ad) {
      funnw.ehdr(mcerr);
      mcerr << "cannot find atom with notation " << fatom_not[n]
            << "\nIn particular, check the sequence of initialization\n";
      spexit(mcerr);
    }
    atomh[n] = ad;
  }
  weight_quanh[0] = fweight_quan1;
  weight_quanh[1] = fweight_quan2;
  double s = 0.0;
  for (long n = 0; n < qatomh; n++) {
    check_econd11(weight_quanh[n], <= 0, mcerr);
    s += weight_quanh[n];
  }
  check_econd11(s, <= 0, mcerr);
  if (s != 1.0) {
    for (long n = 0; n < qatomh; n++) {
      weight_quanh[n] /= s;
    }
  }
  for (long n = 0; n < qatomh; n++) {
    weight_massh[n] = weight_quanh[n] * atomh[n]->A();
  }
  s = 0.0;
  for (long n = 0; n < qatomh; n++) {
    s += weight_massh[n];
  }
  check_econd11(s, <= 0, mcerr);
  if (s != 1.0) {
    for (long n = 0; n < qatomh; n++) {
      weight_massh[n] /= s;
    }
  }
  for (long n = 0; n < qatomh; n++) {
    Z_meanh += atomh[n]->Z() * weight_quanh[n];
    A_meanh += atomh[n]->A() * weight_quanh[n];
    inv_A_meanh += (1.0 / atomh[n]->A()) * weight_quanh[n];
  }
  mean_ratio_Z_to_Ah = Z_meanh / A_meanh;
  NumberOfElectronsInGramh = mean_ratio_Z_to_Ah * (gram / mole) * Avogadro;
}

// three atoms
AtomMixDef::AtomMixDef(const std::string& fatom_not1, double fweight_quan1,
                       const std::string& fatom_not2, double fweight_quan2,
                       const std::string& fatom_not3, double fweight_quan3)
    : qatomh(3),
      atomh(3, nullptr),
      weight_quanh(3),
      weight_massh(3) {

  mfunnamep("AtomMixDef::AtomMixDef(...)");
  std::vector<std::string> fatom_not(3);
  fatom_not[0] = fatom_not1;
  fatom_not[1] = fatom_not2;
  fatom_not[2] = fatom_not3;

  for (long n = 0; n < qatomh; ++n) {
    AtomDef* ad = AtomDef::get_AtomDef(fatom_not[n]);
    if (!ad) {
      funnw.ehdr(mcerr);
      mcerr << "cannot find atom with notation " << fatom_not[n]
            << "\nIn particular, check the sequence of initialization\n";
      spexit(mcerr);
    }
    atomh[n] = ad;
  }
  weight_quanh[0] = fweight_quan1;
  weight_quanh[1] = fweight_quan2;
  weight_quanh[2] = fweight_quan3;
  double s = 0.0;
  for (long n = 0; n < qatomh; n++) {
    check_econd11(weight_quanh[n], <= 0, mcerr);
    s += weight_quanh[n];
  }
  check_econd11(s, <= 0, mcerr);
  if (s != 1.0) {
    for (long n = 0; n < qatomh; n++) {
      weight_quanh[n] /= s;
    }
  }
  for (long n = 0; n < qatomh; n++) {
    weight_massh[n] = weight_quanh[n] * atomh[n]->A();
  }
  s = 0.0;
  for (long n = 0; n < qatomh; n++) {
    s += weight_massh[n];
  }
  check_econd11(s, <= 0, mcerr);
  if (s != 1.0) {
    for (long n = 0; n < qatomh; n++) {
      weight_massh[n] /= s;
    }
  }
  for (long n = 0; n < qatomh; n++) {
    Z_meanh += atomh[n]->Z() * weight_quanh[n];
    A_meanh += atomh[n]->A() * weight_quanh[n];
    inv_A_meanh += (1.0 / atomh[n]->A()) * weight_quanh[n];
  }
  mean_ratio_Z_to_Ah = Z_meanh / A_meanh;
  NumberOfElectronsInGramh = mean_ratio_Z_to_Ah * (gram / mole) * Avogadro;
}

// four atoms
AtomMixDef::AtomMixDef(const std::string& fatom_not1, double fweight_quan1,
                       const std::string& fatom_not2, double fweight_quan2,
                       const std::string& fatom_not3, double fweight_quan3,
                       const std::string& fatom_not4, double fweight_quan4)
    : qatomh(4),
      atomh(4, nullptr),
      weight_quanh(4, 0.),
      weight_massh(4, 0.) {
  mfunnamep("AtomMixDef::AtomMixDef(...)");
  std::vector<std::string> fatom_not(4);
  fatom_not[0] = fatom_not1;
  fatom_not[1] = fatom_not2;
  fatom_not[2] = fatom_not3;
  fatom_not[3] = fatom_not4;

  for (long k = 0; k < qatomh; k++) {
    AtomDef* ad = AtomDef::get_AtomDef(fatom_not[k]);
    if (!ad) {
      funnw.ehdr(mcerr);
      mcerr << "cannot find atom with notation " << fatom_not[k]
            << "\nIn particular, check the sequence of initialization\n";
      spexit(mcerr);
    }
    atomh[k] = ad;
  }
  weight_quanh[0] = fweight_quan1;
  weight_quanh[1] = fweight_quan2;
  weight_quanh[2] = fweight_quan3;
  weight_quanh[3] = fweight_quan4;
  double s = 0.0;
  for (long n = 0; n < qatomh; n++) {
    check_econd11(weight_quanh[n], <= 0, mcerr);
    s += weight_quanh[n];
  }
  check_econd11(s, <= 0, mcerr);
  if (s != 1.0) {
    for (long n = 0; n < qatomh; n++) {
      weight_quanh[n] /= s;
    }
  }
  for (long n = 0; n < qatomh; n++) {
    weight_massh[n] = weight_quanh[n] * atomh[n]->A();
  }
  s = 0.0;
  for (long n = 0; n < qatomh; n++) {
    s += weight_massh[n];
  }
  check_econd11(s, <= 0, mcerr);
  if (s != 1.0) {
    for (long n = 0; n < qatomh; n++) {
      weight_massh[n] /= s;
    }
  }
  for (long n = 0; n < qatomh; n++) {
    Z_meanh += atomh[n]->Z() * weight_quanh[n];
    A_meanh += atomh[n]->A() * weight_quanh[n];
    inv_A_meanh += (1.0 / atomh[n]->A()) * weight_quanh[n];
  }
  mean_ratio_Z_to_Ah = Z_meanh / A_meanh;
  NumberOfElectronsInGramh = mean_ratio_Z_to_Ah * (gram / mole) * Avogadro;
}

void AtomMixDef::print(std::ostream& file, int l) const {
  if (l > 0) file << (*this);
}

std::ostream& operator<<(std::ostream& file, const AtomMixDef& f) {
  mfunname("std::ostream& operator << (std::ostream&, const AtomMixDef&)");
  Ifile << "AtomMixDef\n";
  indn.n += 2;
  constexpr double gpm = gram / mole;
  Ifile << "Z_mean()=" << std::setw(3) << f.Z_mean()
        << " A_mean()/(gram/mole)=" << f.A_mean() / gpm << '\n';
  Ifile << "inv_A_mean()*(gram/mole)=" << f.inv_A_mean() * gpm << '\n';
  Ifile << "mean_ratio_Z_to_A()*(gram/mole)=" 
        << f.mean_ratio_Z_to_A() * gpm << '\n';
  Ifile << "NumberOfElectronsInGram()=" << f.NumberOfElectronsInGram() << '\n';
  // Here above the mass unit is defined,
  // therefore there is no need to divide by gram.
  Iprintn(file, f.qatom());
  indn.n += 2;
  for (long n = 0; n < f.qatom(); n++) {
    Ifile << "n=" << n << " atom(n)->notation=" << f.atom(n)->notation()
          << "\n";
    indn.n += 2;
    Ifile << " weight_quan(n)=" << f.weight_quan(n)
          << " weight_mass(n)=" << f.weight_mass(n) << '\n';
    indn.n -= 2;
  }
  indn.n -= 2;
  indn.n -= 2;
  return file;
}
}
