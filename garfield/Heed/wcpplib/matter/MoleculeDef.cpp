#include <iomanip>
#include "wcpplib/matter/MoleculeDef.h"
#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/math/cubic.h"

// 1998-2004 I. Smirnov

namespace Heed {

using CLHEP::k_Boltzmann;
using CLHEP::Avogadro;
using CLHEP::cm3;
using CLHEP::gram;
using CLHEP::mole;

VanDerWaals::VanDerWaals(double fPk, double fTk) : Pkh(fPk), Tkh(fTk) {
  // Rydberg constant
  const double R = k_Boltzmann * Avogadro;

  Vkh = R * 3.0 / 8.0 * Tkh / Pkh;
  ah = 3 * Pkh * Vkh * Vkh;
  bh = 1.0 / 3.0 * Vkh;
}

double VanDerWaals::volume_of_mole(double T, double p, int& s_not_single) {
  mfunname("VanDerWaals::volume_of_mole(...)");

  double Tr = T / Tkh;
  double Pr = p / Pkh;
  Iprint2n(mcout, Tr, Pr);
  Cubic cb(Pr, -1.0 / 3.0 * (Pr + 8 * Tr), 3, -1);
  double r[3];
  int q = cb.find_real_zero(r);
  check_econd11(q, <= 0, mcerr);
  double x = r[q - 1];   // this is the relative volume taken by one mole
  double res = x * Vkh;  // this is the absolute volume taken by one mole
  Iprint2n(mcout, x, res);
  s_not_single = q == 2 ? 1 : 0;
  return res;
}

VanDerWaals* VanDerWaals::copy() const { return new VanDerWaals(*this); }

std::ostream& operator<<(std::ostream& file, const VanDerWaals& f) {
  mfunname(
      "std::ostream& operator << (std::ostream& file, const VanDerWaals& f)");
  Ifile << "VanDerWaals:\n";
  indn.n += 2;
  Iprintn(file, f.Pk() / (CLHEP::atmosphere));
  Iprintn(file, f.Tk() / (CLHEP::kelvin));
  Iprintn(file, f.Vk() / (cm3));
  Ifile << "For comparison, the volume of a mole of ideal gas\n";
  Ifile << "at the same conditions takes\n";
  Iprintn(file, (k_Boltzmann * Avogadro * f.Tk() / f.Pk()) / (cm3 * mole));
  Iprintn(file, f.a() / (CLHEP::atmosphere * cm3 * cm3));
  Iprintn(file, f.b() / (cm3));
  indn.n -= 2;
  return file;
}

MoleculeDef::MoleculeDef() {
  MoleculeDef::get_logbook().push_back(this);
}

MoleculeDef::MoleculeDef(const std::string& fname, const std::string& fnotation,
                         long fqatom, const std::vector<std::string>& fatom_not,
                         const std::vector<long>& fqatom_ps,
                         std::shared_ptr<VanDerWaals> fvdw)
    : AtomMixDef(fqatom, fatom_not, fqatom_ps),
      nameh(fname),
      notationh(fnotation),
      qatom_psh(fqatom_ps) {
  mfunname("MoleculeDef::MoleculeDef(...)");
  m_vdw = std::move(fvdw);
  for (long n = 0; n < qatom(); n++) {
    Z_totalh += qatom_psh[n] * atom(n)->Z();
    A_totalh += qatom_psh[n] * atom(n)->A();
    tqatomh += qatom_psh[n];
    check_econd11(qatom_psh[n], <= 0, mcerr);
  }
  verify();
  MoleculeDef::get_logbook().push_back(this);
}

// one atom in molecule
MoleculeDef::MoleculeDef(const std::string& fname, const std::string& fnotation,
                         const std::string& fatom_not, long fqatom_ps,
                         std::shared_ptr<VanDerWaals> fvdw)
    : AtomMixDef(fatom_not),
      nameh(fname),
      notationh(fnotation),
      qatom_psh(1, fqatom_ps),
      tqatomh(fqatom_ps) {
  mfunname("MoleculeDef::MoleculeDef(...)");
  m_vdw = std::move(fvdw);
  Z_totalh = atom(0)->Z() * fqatom_ps;
  A_totalh = atom(0)->A() * fqatom_ps;
  verify();
  MoleculeDef::get_logbook().push_back(this);
}

// two atoms
MoleculeDef::MoleculeDef(const std::string& fname, const std::string& fnotation,
                         const std::string& fatom_not1, long fqatom_ps1,
                         const std::string& fatom_not2, long fqatom_ps2,
                         std::shared_ptr<VanDerWaals> fvdw)
    : AtomMixDef(fatom_not1, fqatom_ps1, fatom_not2, fqatom_ps2),
      nameh(fname),
      notationh(fnotation),
      qatom_psh(2) {
  mfunname("MoleculeDef::MoleculeDef(...)");
  m_vdw = std::move(fvdw);
  qatom_psh[0] = fqatom_ps1;
  qatom_psh[1] = fqatom_ps2;
  for (long n = 0; n < qatom(); n++) {
    check_econd11(qatom_psh[n], <= 0, mcerr);
    Z_totalh += qatom_psh[n] * atom(n)->Z();
    A_totalh += qatom_psh[n] * atom(n)->A();
    tqatomh += qatom_psh[n];
  }
  verify();
  MoleculeDef::get_logbook().push_back(this);
}

// three atoms
MoleculeDef::MoleculeDef(const std::string& fname, const std::string& fnotation,
                         const std::string& fatom_not1, long fqatom_ps1,
                         const std::string& fatom_not2, long fqatom_ps2,
                         const std::string& fatom_not3, long fqatom_ps3,
                         std::shared_ptr<VanDerWaals> fvdw)
    : AtomMixDef(fatom_not1, fqatom_ps1, fatom_not2, fqatom_ps2, fatom_not3,
                 fqatom_ps3),
      nameh(fname),
      notationh(fnotation),
      qatom_psh(3) {
  mfunname("MoleculeDef::MoleculeDef(...)");
  m_vdw = std::move(fvdw);
  qatom_psh[0] = fqatom_ps1;
  qatom_psh[1] = fqatom_ps2;
  qatom_psh[2] = fqatom_ps3;
  for (long n = 0; n < qatom(); n++) {
    check_econd11(qatom_psh[n], <= 0, mcerr);
    Z_totalh += qatom_psh[n] * atom(n)->Z();
    A_totalh += qatom_psh[n] * atom(n)->A();
    tqatomh += qatom_psh[n];
  }
  verify();
  MoleculeDef::get_logbook().push_back(this);
}

void MoleculeDef::print(std::ostream& file, int l) const {
  if (l > 0) file << (*this);
}

void MoleculeDef::printall(std::ostream& file) {
  Ifile << "MoleculeDef::printall:\n";
  for (auto molecule : MoleculeDef::get_logbook()) {
    file << molecule;
  }
}

void MoleculeDef::verify() {
  mfunnamep("void MoleculeDef::verify()");
  if (nameh == "none" && notationh == "none") return;
  for (auto molecule : MoleculeDef::get_logbook()) {
    if (molecule->nameh != nameh && molecule->notationh != notationh) continue;
    funnw.ehdr(mcerr);
    mcerr << "cannot initialize two molecules with the same name or notation\n";
    mcerr << "name=" << nameh << " notation=" << notationh << '\n';
    spexit(mcerr);
  }
}

std::list<MoleculeDef*>& MoleculeDef::get_logbook() {
  static std::list<MoleculeDef*> logbook;
  return logbook;
}

const std::list<MoleculeDef*>& MoleculeDef::get_const_logbook() {
  return MoleculeDef::get_logbook();
}

MoleculeDef* MoleculeDef::get_MoleculeDef(const std::string& fnotation) {
  for (auto molecule : MoleculeDef::get_logbook()) {
    if (molecule->notation() == fnotation) return molecule;
  }
  return nullptr;
}

std::ostream& operator<<(std::ostream& file, const MoleculeDef& f) {
  mfunnamep("std::ostream& operator << (std::ostream&, const MoleculeDef&)");
  constexpr double gpm = gram / mole;
  Ifile << "MoleculeDef: name=" << std::setw(10) << f.name()
        << " notation=" << std::setw(3) << f.notation() << '\n';
  indn.n += 2;
  Ifile << "Z_total()=" << std::setw(3) << f.Z_total()
        << " A_total()/(gram/mole)=" << f.A_total() / gpm 
        << " tqatom()=" << f.tqatom() << '\n';
  Iprintn(file, f.qatom());
  indn.n += 2;
  for (long n = 0; n < f.qatom(); n++) {
    Ifile << "n=" << n << " atom(n)->notation=" << f.atom(n)->notation()
          << " qatom_ps(n)=" << f.qatom_ps(n) << '\n';
  }
  indn.n -= 2;
  f.AtomMixDef::print(file, 1);
  VanDerWaals* at = f.vdw().get();
  if (at) {
    Ifile << "Density at the crucial conditions for ideal gas (for debug):\n";
    double rydberg = k_Boltzmann * Avogadro;  // more precise
    // mcout<<"rydberg/(joule/(kelvin*mole)) ="
    //     << rydberg/(joule/(kelvin*mole))<<'\n';
    // double sa = f.A_total();
    Iprintn(mcout,
            f.A_total() * at->Pk() / (rydberg * at->Tk()) / (gram / cm3));
    Ifile << "For the Waals:\n";
    Iprintn(mcout, f.A_total() / at->Vk() / (gram / cm3));
  }
  indn.n -= 2;
  return file;
}

// TODO
MoleculeDef::~MoleculeDef() { MoleculeDef::get_logbook().remove(this); }
}
