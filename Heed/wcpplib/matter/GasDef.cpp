#include <iomanip>
#include "wcpplib/matter/GasDef.h"
#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"

namespace Heed {

using CLHEP::Avogadro;
using CLHEP::k_Boltzmann;

GasDef::GasDef() : MatterDef() {}

GasDef::GasDef(const std::string& fname, const std::string& fnotation,
               long fqmolec, const std::vector<std::string>& fmolec_not,
               const std::vector<double>& fweight_quan_molec, double fpressure,
               double ftemperature, double fdensity)
    : pressureh(fpressure),
      qmolech(fqmolec),
      molech(fqmolec, nullptr),
      weight_quan_molech(fqmolec),
      weight_mass_molech(fqmolec) {
  mfunname("GasDef::GasDef(...many molecules...)");

  // Finding pointers to all molec. by notations
  for (long k = 0; k < fqmolec; ++k) {
    MoleculeDef* amd = MoleculeDef::get_MoleculeDef(fmolec_not[k]);
    check_econd11a(amd, == NULL,
                   "No molecule with such notation: " << fmolec_not[k] << '\n',
                   mcerr)
    if (!amd) {
      mcerr << "cannot find molecule with notation " << fmolec_not[k]
            << "\nIn particular, check the sequence of initialization\n";
      spexit(mcerr);
    }
    molech[k] = amd;
  }
  double s = 0.0;
  for (long n = 0; n < fqmolec; ++n) {
    weight_quan_molech[n] = fweight_quan_molec[n];
    check_econd11(weight_quan_molech[n], <= 0, mcerr);
    s += weight_quan_molech[n];
  }
  check_econd11(s, <= 0, mcerr);
  if (s != 1.0) {
    for (long n = 0; n < fqmolec; ++n) {
      weight_quan_molech[n] /= s;
    }
  }
  for (long n = 0; n < fqmolec; ++n) {
    weight_mass_molech[n] = weight_quan_molech[n] * molech[n]->A_total();
  }
  s = 0.0;
  for (long n = 0; n < fqmolec; ++n) {
    s += weight_mass_molech[n];
  }
  check_econd11(s, <= 0, mcerr);
  if (s != 1.0) {
    for (long n = 0; n < fqmolec; ++n) {
      weight_mass_molech[n] /= s;
    }
  }

  long qat = 0;
  std::vector<std::string> fatom_not(1000);
  std::vector<double> weight_qa(1000, 0.0);
  for (long k = 0; k < fqmolec; ++k) {
    for (long n = 0; n < molech[k]->qatom(); ++n) {
      /*
      This is originally designed to avoid duplication of an atom
      in the list if it presents twice in different moleculas.
      But it appears that the same atom in different moleculas
      can have different features related to position and sensitivity
      of external shell. In particular it affects on ionization.
      This difference can be used in inherited and
      related classes. Therefore such reduction of the list can produce
      problems. Therefore this is excluded by commenting off this passage,
      and also by commenting off mark2.
      for (i = 0; i < qat; i++) {
        if (molech[k]->atom(n)->notation() == fatom_not[i]) {
          weight_qa[i] += fweight_quan_molec[k] * molech[k]->weight_quan(n);
          goto mark2;
        }
      }
      */
      fatom_not[qat] = molech[k]->atom(n)->notation();
      weight_qa[qat] = fweight_quan_molec[k] * molech[k]->qatom_ps(n);
      // mcout << "qat=" << qat << " fatom_not[qat]=" << fatom_not[qat]
      //      << " weight_qa[qat]=" << weight_qa[qat] << '\n';
      ++qat;
      // mark2: ;
    }
  }
  if (fdensity < 0.0) {
    double sw = 0.0;
    double sa = 0.0;
    for (long n = 0; n < qmolech; ++n) {
      sa += weight_quan_molech[n] * molech[n]->A_total();
      sw += weight_quan_molech[n];
    }
    const double rydberg = k_Boltzmann * Avogadro;
    fdensity = sa * fpressure / (rydberg * ftemperature * sw);
  }
  verify(fname, fnotation);
  {
    *((MatterDef*)this) = MatterDef(fname, fnotation, qat, fatom_not, weight_qa,
                                    fdensity, ftemperature);
  }
}

GasDef::GasDef(const std::string& fname, const std::string& fnotation,
               long fqmolec, const std::vector<std::string>& fmolec_not,
               const std::vector<double>& fweight_volume_molec,
               double fpressure, double ftemperature, int /*s1*/, int /*s2*/) {
  // s1 and s2 are to distinguish the constructor
  mfunname("GasDef::GasDef(...many molecules... Waals)");
  std::vector<MoleculeDef*> amolec(fqmolec);
  for (long n = 0; n < fqmolec; ++n) {
    amolec[n] = MoleculeDef::get_MoleculeDef(fmolec_not[n]);
    check_econd11a(amolec[n], == NULL,
                   "No molecule with such notation: " << fmolec_not[n] << '\n',
                   mcerr)
    // Van der Waals correction currently not used.
    // VanDerWaals* aw = amolec[n]->vdw().get();
  }
  // first normalize volumes to total unity
  std::vector<double> fw(fqmolec);
  // normalized volume weights
  double s = 0.0;
  for (long n = 0; n < fqmolec; ++n) {
    s += fweight_volume_molec[n];
  }
  check_econd11(s, <= 0, mcerr);
  for (long n = 0; n < fqmolec; ++n) {
    fw[n] = fweight_volume_molec[n] / s;
  }

  // calculate number of molecules or moles and mass of each component
  std::vector<double> fweight_quan_molec(fqmolec);
  double mass_t = 0.0;
  constexpr double rydberg = k_Boltzmann * Avogadro;
  for (long n = 0; n < fqmolec; ++n) {
    VanDerWaals* aw = amolec[n]->vdw().get();
    if (!aw) {
      // ideal gas case
      fweight_quan_molec[n] = fw[n] * fpressure / (rydberg * ftemperature);
      double ms = fweight_quan_molec[n] * amolec[n]->A_total();
      // Iprint2n(mcout, fweight_quan_molec[n], ms/gram);
      mass_t += ms;
    } else {
      // van der Waals gas case
      int s_not_single;
      double number_of_moles =
          fw[n] * 1.0 / aw->volume_of_mole(ftemperature,  // relative to T_k
                                           fpressure, s_not_single);
      check_econd11(s_not_single, == 1, mcerr);
      fweight_quan_molec[n] = number_of_moles;
      double ms = fweight_quan_molec[n] * amolec[n]->A_total();
      // Iprint2n(mcout, fweight_quan_molec[n], ms/gram);
      mass_t += ms;
    }
  }
  double density_t = mass_t;
  *this = GasDef(fname, fnotation, fqmolec, fmolec_not, fweight_quan_molec,
                 fpressure, ftemperature, density_t);
}

GasDef::GasDef(const std::string& fname, const std::string& fnotation,
               const std::string& fmolec_not, double fpressure,
               double ftemperature, double fdensity) {
  mfunname("GasDef::GasDef(...1 molecule...)");
  std::vector<std::string> fmolec_noth(1, fmolec_not);
  std::vector<double> fweight_quan_molec(1, 1.0);
  {
    *this = GasDef(fname, fnotation, 1, fmolec_noth, fweight_quan_molec,
                   fpressure, ftemperature, fdensity);
  }
}

GasDef::GasDef(const std::string& fname, const std::string& fnotation,
               const std::string& fmolec_not, double fpressure,
               double ftemperature, int s1, int s2) {
  mfunname("GasDef::GasDef(...1 molecule...)");
  std::vector<std::string> fmolec_noth(1, fmolec_not);
  std::vector<double> fweight_volume_molec(1, 1.0);
  {
    *this = GasDef(fname, fnotation, 1, fmolec_noth, fweight_volume_molec,
                   fpressure, ftemperature, s1, s2);
  }
}

GasDef::GasDef(const std::string& fname, const std::string& fnotation,
               const std::string& fmolec_not1, double fweight_quan_molec1,
               const std::string& fmolec_not2, double fweight_quan_molec2,
               double fpressure, double ftemperature, double fdensity) {
  mfunname("GasDef::GasDef(...2 molecules...)");
  std::vector<std::string> fmolec_noth(2);
  std::vector<double> fweight_quan_molec(2, 0.0);
  fmolec_noth[0] = fmolec_not1;
  fmolec_noth[1] = fmolec_not2;
  fweight_quan_molec[0] = fweight_quan_molec1;
  fweight_quan_molec[1] = fweight_quan_molec2;
  {
    *this = GasDef(fname, fnotation, 2, fmolec_noth, fweight_quan_molec,
                   fpressure, ftemperature, fdensity);
  }
}

GasDef::GasDef(const std::string& fname, const std::string& fnotation,
               const std::string& fmolec_not1, double fweight_volume_molec1,
               const std::string& fmolec_not2, double fweight_volume_molec2,
               double fpressure, double ftemperature, int s1, int s2) {
  mfunname("GasDef::GasDef(...2 molecules...)");
  std::vector<std::string> fmolec_noth(2);
  std::vector<double> fweight_volume_molec(2, 0.0);
  fmolec_noth[0] = fmolec_not1;
  fmolec_noth[1] = fmolec_not2;
  fweight_volume_molec[0] = fweight_volume_molec1;
  fweight_volume_molec[1] = fweight_volume_molec2;
  {
    *this = GasDef(fname, fnotation, 2, fmolec_noth, fweight_volume_molec,
                   fpressure, ftemperature, s1, s2);
  }
}

GasDef::GasDef(const std::string& fname, const std::string& fnotation,
               const std::string& fmolec_not1, double fweight_quan_molec1,
               const std::string& fmolec_not2, double fweight_quan_molec2,
               const std::string& fmolec_not3, double fweight_quan_molec3,
               double fpressure, double ftemperature, double fdensity) {
  mfunname("GasDef::GasDef(...3 molecules...)");
  std::vector<std::string> fmolec_noth(3);
  std::vector<double> fweight_quan_molec(3, 0.0);
  fmolec_noth[0] = fmolec_not1;
  fmolec_noth[1] = fmolec_not2;
  fmolec_noth[2] = fmolec_not3;
  fweight_quan_molec[0] = fweight_quan_molec1;
  fweight_quan_molec[1] = fweight_quan_molec2;
  fweight_quan_molec[2] = fweight_quan_molec3;
  {
    *this = GasDef(fname, fnotation, 3, fmolec_noth, fweight_quan_molec,
                   fpressure, ftemperature, fdensity);
  }
}

GasDef::GasDef(const std::string& fname, const std::string& fnotation,
               const std::string& fmolec_not1, double fweight_volume_molec1,
               const std::string& fmolec_not2, double fweight_volume_molec2,
               const std::string& fmolec_not3, double fweight_volume_molec3,
               double fpressure, double ftemperature, int s1, int s2) {
  mfunname("GasDef::GasDef(...3 molecules...)");
  std::vector<std::string> fmolec_noth(3);
  std::vector<double> fweight_volume_molec(3, 0.0);
  fmolec_noth[0] = fmolec_not1;
  fmolec_noth[1] = fmolec_not2;
  fmolec_noth[2] = fmolec_not3;
  fweight_volume_molec[0] = fweight_volume_molec1;
  fweight_volume_molec[1] = fweight_volume_molec2;
  fweight_volume_molec[2] = fweight_volume_molec3;
  {
    *this = GasDef(fname, fnotation, 3, fmolec_noth, fweight_volume_molec,
                   fpressure, ftemperature, s1, s2);
  }
}

GasDef::GasDef(const std::string& fname, const std::string& fnotation,
               const GasDef& gd, double fpressure, double ftemperature,
               double fdensity) {
  mfunname("GasDef::GasDef( another GasDef with different pres)");
  long fqmolec = gd.qmolec();
  std::vector<std::string> fmolec_not(fqmolec);
  std::vector<double> fweight_quan_molec(fqmolec);
  for (long n = 0; n < fqmolec; ++n) {
    fmolec_not[n] = gd.molec(n)->notation();
    fweight_quan_molec[n] = gd.weight_quan_molec(n);
  }
  *this = GasDef(fname, fnotation, fqmolec, fmolec_not, fweight_quan_molec,
                 fpressure, ftemperature, fdensity);
}

// mean charge of molecule
double GasDef::Z_mean_molec(void) const {
  mfunname("double GasDef::Z_mean_molec(void) const ");
  double s = 0.0;
  for (long n = 0; n < qmolech; ++n) {
    s += molech[n]->Z_total() * weight_quan_molech[n];
  }
  return s;
}

void GasDef::print(std::ostream& file, int l) const {
  if (l > 0) file << (*this);
}

std::ostream& operator<<(std::ostream& file, const GasDef& f) {
  mfunname("std::ostream& operator << (std::ostream& file, const GasDef& f)");
  Ifile << "GasDef: \n";
  indn.n += 2;
  indn.n += 2;
  file << ((MatterDef&)f);
  indn.n -= 2;
  constexpr double mm_rt_st_in_atmosphere = 760.;
  // This corresponds to 133.322 pascal in one mm
  //( 101325 pascal in one atmosphere )
  const double patm = f.pressure() / CLHEP::atmosphere;
  Ifile << "pressure/atmosphere=" << patm
        << " pressure/atmosphere * mm_rt_st_in_atmosphere = "
        << patm * mm_rt_st_in_atmosphere << '\n';
  Ifile << "Z_mean_molec=" << f.Z_mean_molec() << '\n';

  file << "qmolec()=" << f.qmolec() << '\n';
  indn.n += 2;
  for (long n = 0; n < f.qmolec(); ++n) {
    Ifile << "n=" << n << " molec(n)->notation=" << f.molec(n)->notation()
          << '\n';
    indn.n += 2;
    Ifile << "weight_quan_molec(n)=" << f.weight_quan_molec(n)
          << " weight_mass_molec(n)=" << f.weight_mass_molec(n) << '\n';
    Ifile << "Z_total=" << f.molec(n)->Z_total()
          << " A_total/(gram/mole)=" << f.molec(n)->A_total() / (CLHEP::gram / CLHEP::mole)
          << '\n';
    indn.n -= 2;
  }
  indn.n -= 2;
  indn.n -= 2;
  return file;
}

}
