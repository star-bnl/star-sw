#include "wcpplib/particle/particle_def.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/util/FunNameStack.h"

// 1998 - 2004,   I. Smirnov

namespace Heed {

using CLHEP::electron_mass_c2;
using CLHEP::proton_mass_c2;
using CLHEP::neutron_mass_c2;
using CLHEP::c_squared;
using CLHEP::electron_charge;
using CLHEP::eplus;
using CLHEP::MeV;
using CLHEP::GeV;

spin_def::spin_def(float ftotal, float fprojection)
    : total(ftotal), projection(fprojection) {
  mfunname("spin_def::spin_def(float ftotal, float fprojection)");
  check_econd11(total, < 0, mcerr);
  check_econd12(total, <, projection, mcerr);
}

std::ostream& operator<<(std::ostream& file, const spin_def& f) {
  Ifile << "spin_def: total=" << f.total << " projection=" << f.projection;
  return file;
}

particle_def electron_def("electron", "e-", electron_mass_c2 / c_squared,
                          electron_charge, 1, 0, 0.5, spin_def(0.0, 0.0));
particle_def positron_def("positron", "e+", electron_def);

particle_def muon_minus_def("muon_minus", "mu-", 105.658367 * MeV / c_squared,
                            electron_charge, 1, 0, 0.5, spin_def(0.0, 0.0));
particle_def muon_plus_def("muon_plus", "mu+", muon_minus_def);

particle_def proton_def("proton", "p+", proton_mass_c2 / c_squared, eplus, 0, 1,
                        0.5, spin_def(0.5, 0.5));
particle_def anti_proton_def("", "p-", proton_def);
particle_def neutron_def("neutron", "n", neutron_mass_c2 / c_squared, 0, 0, 1,
                         0.5, spin_def(0.5, -0.5));
particle_def anti_neutron_def("", "", neutron_def);

particle_def P11_def("P11", "P11", 1440.0 * MeV / c_squared, 1 * eplus, 0, 1,
                     0.5, spin_def(0.5, 0.5));
particle_def D13_def("D13", "D13", 1520.0 * MeV / c_squared, 1 * eplus, 0, 1,
                     1.5, spin_def(0.5, 0.5));
particle_def S11_def("S11", "S11", 1535.0 * MeV / c_squared, 1 * eplus, 0, 1,
                     0.5, spin_def(0.5, 0.5));

// light unflavored mesons
particle_def pi_plus_meson_def("pi_plus_meson", "pi+",
                               139.56755 * MeV / c_squared, eplus, 0, 0, 0.0,
                               spin_def(1.0, 1.0));
particle_def pi_minus_meson_def("pi_minus_meson", "pi-",
                                139.56755 * MeV / c_squared, -eplus, 0, 0, 0.0,
                                spin_def(1.0, -1.0));
particle_def pi_0_meson_def("pi_0_meson", "pi0", 134.9734 * MeV / c_squared, 0,
                            0, 0, 0.0, spin_def(1.0, 0.0));
particle_def eta_meson_def("eta_meson_def", "eta", 548.8 * MeV / c_squared, 0,
                           0, 0, 1.0, spin_def(0.0, 0.0));
particle_def K_plus_meson_def("K_plus_meson_def", "K+",
                              493.677 * MeV / c_squared, 1, 0, 0, 0.0,
                              spin_def(0.5, -0.5));
particle_def K_minus_meson_def("K_minus_meson_def", "K-", K_plus_meson_def);

particle_def deuteron_def("deuteron", "dtr", 1875.613 * MeV / c_squared, eplus,
                          0, 2, 0.0, spin_def(0.0, 0.0));
particle_def alpha_particle_def("alpha_particle", "alpha",
                                3727.417 * MeV / c_squared, 2 * eplus, 0, 4,
                                0.0, spin_def(0.0, 0.0));

particle_def user_particle_def("user_particle", "X",
                               139.56755 * MeV / c_squared, eplus, 0, 0, 0.0,
                               spin_def(0.0, 0.0));

particle_def::particle_def(const std::string& fname,
                           const std::string& fnotation, double fmass,
                           double fcharge, int flepton_n, int fbaryon_n,
                           float fspin, const spin_def& fisospin) {
  name = fname;
  notation = fnotation;
  mass = fmass;
  charge = fcharge;
  baryon_n = fbaryon_n;
  lepton_n = flepton_n;
  spin = fspin;
  isospin = fisospin;
  verify();
  particle_def::get_logbook().push_back(this);
}

particle_def::particle_def(const std::string& fname,
                           const std::string& fnotation, particle_def& p) {
  // creates anti-particle through the call of anti_particle(p)
  *this = anti_particle(p);
  // if(strlen(fname) > 0)
  // strcpy(name,fname);
  if (!(fname == "" || fname == " ")) name = fname;
  if (!(fnotation == "" || fnotation == " ")) notation = fnotation;
  verify();
  particle_def::get_logbook().push_back(this);
}

particle_def particle_def::anti_particle(const particle_def& p) {
  std::string aname = "anti-" + p.name;
  std::string anot = "anti-" + p.notation;
  return particle_def(aname, anot, p.mass, -p.charge, -p.lepton_n, -p.baryon_n,
                      -p.spin, p.isospin);
}
std::list<particle_def*>& particle_def::get_logbook() {
  static std::list<particle_def*> logbook;
  return logbook;
}

const std::list<particle_def*>& particle_def::get_const_logbook() {
  return particle_def::get_logbook();
}

particle_def* particle_def::get_particle_def(const std::string& fnotation) {
  for (auto particle : particle_def::get_logbook()) {
    if (!particle) continue;
    if (particle->notation == fnotation) return particle;
  }
  return nullptr;
}

void particle_def::set_mass(const double m) { mass = m * MeV / c_squared; }

void particle_def::set_charge(const double z) { charge = z * eplus; }

void particle_def::print(std::ostream& file, int l) const {
  if (l > 0) file << (*this);
}
void particle_def::printall(std::ostream& file) {
  Ifile << "particle_def::printall:\n";
  for (auto particle : particle_def::get_logbook()) {
    if (particle) file << particle;
  }
}

std::ostream& operator<<(std::ostream& file, const particle_def& f) {
  Ifile << "particle_def: name=" << f.name << " notation=" << f.notation
        << '\n';
  Ifile << "mass=" << f.mass
        << " mass/(GeV/c_squared)=" << f.mass / (GeV / c_squared)
        << " charge=" << f.charge << " charge/eplus=" << f.charge / eplus
        << '\n';
  Ifile << "lepton_n=" << f.lepton_n << " baryon_n=" << f.baryon_n << '\n';
  Ifile << "spin=" << f.spin << " isospin=" << f.isospin << '\n';
  return file;
}

particle_type::particle_type(const char* name, int s) {
  mfunname("particle_type::particle_type(const char* name, int s)");
  for (auto particle : particle_def::get_logbook()) {
    if (name == particle->notation) {
      pardef = particle;
      return;
    }
  }
  for (auto particle : particle_def::get_logbook()) {
    if (name == particle->name) {
      pardef = particle;
      return;
    }
  }
  if (s == 0) {
    mcerr << "this type of particle is absent, name=" << name << '\n';
    spexit(mcerr);
  }
  pardef = nullptr;
}

void particle_type::print_notation(std::ostream& file) const {
  if (!pardef) {
    file << "none";
  } else {
    file << pardef->notation;
  }
}

std::ostream& operator<<(std::ostream& file, const particle_type& f) {
  if (!f.pardef) {
    file << "type is not initialized";
  } else {
    file << (f.pardef->name);
  }
  return file;
}
}
