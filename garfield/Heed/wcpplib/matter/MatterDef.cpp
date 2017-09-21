#include <iomanip>
#include "wcpplib/matter/MatterDef.h"
#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"

// 1998-2004 I. Smirnov

namespace Heed {

void MatterDef::calc_I_eff(void) { I_effh = Z_mean() * 12.0 * eV; }

MatterDef::MatterDef(void) : nameh("none"), notationh("none") {
  MatterDef::get_logbook().push_back(this);
}

MatterDef::MatterDef(const std::string& fname, const std::string& fnotation,
                     long fqatom, const std::vector<std::string>& fatom_not,
                     const std::vector<double>& fweight_quan, double fdensity,
                     double ftemperature)
    : AtomMixDef(fqatom, fatom_not, fweight_quan),
      nameh(fname),
      notationh(fnotation),
      temperatureh(ftemperature),
      densityh(fdensity) {
  mfunname("MatterDef::MatterDef(...many atoms...)");
  calc_I_eff();
  verify();
  MatterDef::get_logbook().push_back(this);
}

MatterDef::MatterDef(const std::string& fname, const std::string& fnotation,
                     const std::string& fatom_not, double fdensity,
                     double ftemperature)
    : AtomMixDef(fatom_not),
      nameh(fname),
      notationh(fnotation),
      temperatureh(ftemperature),
      densityh(fdensity) {
  mfunname("MatterDef::MatterDef(...1 atom...)");
  calc_I_eff();
  verify();
  MatterDef::get_logbook().push_back(this);
}

MatterDef::MatterDef(const std::string& fname, const std::string& fnotation,
                     const std::string& fatom_not1, double fweight_quan1,
                     const std::string& fatom_not2, double fweight_quan2,
                     double fdensity, double ftemperature)
    : AtomMixDef(fatom_not1, fweight_quan1, fatom_not2, fweight_quan2),
      nameh(fname),
      notationh(fnotation),
      temperatureh(ftemperature),
      densityh(fdensity) {
  mfunname("MatterDef::MatterDef(...2 atoms...)");
  calc_I_eff();
  verify();
  MatterDef::get_logbook().push_back(this);
}

MatterDef::MatterDef(const std::string& fname, const std::string& fnotation,
                     const std::string& fatom_not1, double fweight_quan1,
                     const std::string& fatom_not2, double fweight_quan2,
                     const std::string& fatom_not3, double fweight_quan3,
                     double fdensity, double ftemperature)
    : AtomMixDef(fatom_not1, fweight_quan1, fatom_not2, fweight_quan2,
                 fatom_not3, fweight_quan3),
      nameh(fname),
      notationh(fnotation),
      temperatureh(ftemperature),
      densityh(fdensity) {
  mfunname("MatterDef::MatterDef(...2 atoms...)");
  calc_I_eff();
  verify();
  MatterDef::get_logbook().push_back(this);
}

void MatterDef::verify(void) {
  mfunnamep("void MatterDef::verify(void)");
  if (nameh == "none" && notationh == "none") return;
  std::list<MatterDef*>& logbook = MatterDef::get_logbook();
  std::list<MatterDef*>::const_iterator it;
  std::list<MatterDef*>::const_iterator end = logbook.end();
  for (it = logbook.begin(); it != end; ++it) {
    if ((*it)->nameh == nameh || (*it)->notationh == notationh) {
      funnw.ehdr(mcerr);
      mcerr << "cannot initialize two matters "
            << "with the same name or notation\n";
      mcerr << "name=" << nameh << " notation=" << notationh << '\n';
      spexit(mcerr);
    }
  }
}

void MatterDef::verify(const std::string& fname, const std::string& fnotation) {
  mfunnamep(
      "void MatterDef::verify(const std::string& fname, const std::string& "
      "fnotation)");
  std::list<MatterDef*>& logbook = MatterDef::get_logbook();
  std::list<MatterDef*>::const_iterator it;
  std::list<MatterDef*>::const_iterator end = logbook.end();
  for (it = logbook.begin(); it != end; ++it) {
    if ((*it)->nameh == fname || (*it)->notationh == fnotation) {
      funnw.ehdr(mcerr);
      mcerr << "cannot initialize two matters "
            << "with the same name or notation\n";
      mcerr << "name=" << fname << " notation=" << fnotation << '\n';
      spexit(mcerr);
    }
  }
}

void MatterDef::print(std::ostream& file, int l) const {
  if (l > 0) file << (*this);
}

void MatterDef::printall(std::ostream& file) {
  Ifile << "MatterDef::printall:\n";
  std::list<MatterDef*>& logbook = MatterDef::get_logbook();
  std::list<MatterDef*>::const_iterator it;
  std::list<MatterDef*>::const_iterator end = logbook.end();
  for (it = logbook.begin(); it != end; ++it) {
    (*it)->print(file, 1);
  }
}

std::list<MatterDef*>& MatterDef::get_logbook(void) {
  static std::list<MatterDef*> logbook;
  return logbook;
}

const std::list<MatterDef*>& MatterDef::get_const_logbook(void) {
  return MatterDef::get_logbook();
}

MatterDef* MatterDef::get_MatterDef(const std::string& fnotation) {
  std::list<MatterDef*>& logbook = MatterDef::get_logbook();
  std::list<MatterDef*>::const_iterator it;
  std::list<MatterDef*>::const_iterator end = logbook.end();
  for (it = logbook.begin(); it != end; ++it) {
    if ((*it)->notation() == fnotation) return *it;
  }
  return NULL;
}

std::ostream& operator<<(std::ostream& file, const MatterDef& f) {
  mfunname(
      "std::ostream& operator << (std::ostream& file, const MatterDef& f)");
  Ifile << "MatterDef: name=" << std::setw(10) << f.name()
        << " notation=" << std::setw(3) << f.notation() << '\n';
  indn.n += 2;
  Ifile << "density/(gram/cm3)=" << f.density() / (gram / cm3)
        << " temperature/kelvin=" << f.temperature() / kelvin
        << " I_eff/eV=" << f.I_eff() / eV << '\n';
  f.AtomMixDef::print(file, 1);
  indn.n -= 2;
  return file;
}

std::ostream& operator<<(std::ostream& file, const MatterType& f) {
  mfunname(
      "std::ostream& operator << (std::ostream& file, const MatterType& f)");
  if (f.matdef.get() == NULL) {
    Ifile << "MatterType: type is not initialized\n";
  } else {
    Ifile << "MatterType: notation=" << f.matdef->notation() << '\n';
  }
  return file;
}

MatterDef::~MatterDef() { MatterDef::get_logbook().remove(this); }
}
