#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/random/ranluxint.h"
#include "wcpplib/random/chisran.h"
#include "wcpplib/random/pois.h"
#include "heed++/code/HeedDeltaElectron.h"
#include "heed++/code/HeedDeltaElectronCS.h"
#include "heed++/code/EnTransfCS.h"
#include "heed++/code/ParticleBank.h"
#include "heed++/code/HeedPhoton.h"
                                            /*
2003, I. Smirnov
*/

namespace Heed {

// long HeedPhoton::last_particle_number = 0;

HeedPhoton::HeedPhoton(manip_absvol* primvol, const point& pt, const vec& vel,
                       vfloat time, long fparent_particle_number,
                       double fenergy, int fs_print_listing)
    : gparticle(primvol, pt, vel, time),
      particle_number(last_particle_number++),
      parent_particle_number(fparent_particle_number),
      s_print_listing(fs_print_listing),
      energy(fenergy),
      s_photon_absorbed(0),
#ifdef SFER_PHOTOEL
      s_sfer_photoel(0),
#endif
      s_delta_generated(0) {
  mfunname("HeedPhoton::HeedPhoton(...)");
  double length_vel = length(vel);
  check_econd11(fabs(length_vel - c_light) / (length_vel + c_light), > 1.0e-10,
                mcerr);
}

void HeedPhoton::physics(void) {
  mfunname("void HeedPhoton::physics(void)");
  if (s_print_listing == 1) {
    mcout << "void HeedPhoton::physics(void) is starting\n";
  }
  // Stop here if the photon has already been absorbed.
  if (s_photon_absorbed != 0) return;
  if (nextpos.prange <= 0.0) return;
  // Get least address of volume
  const absvol* av = currpos.G_lavol();  
  HeedMatterDef* hmd = NULL;
  const EnTransfCSType* etcst = dynamic_cast<const EnTransfCSType*>(av);
  if (etcst) {
    hmd = etcst->etcs->hmd.getver();
  } else {
    const HeedDeltaElectronCSType* hmecst =
        dynamic_cast<const HeedDeltaElectronCSType*>(av);
    if (hmecst) hmd = hmecst->hdecs->hmd.get();
  }
  // Stop here if we couldn't retrieve the material definition.
  if (!hmd) return;
  // Calculate the cross section.
  // First count the shells.
  long qst = 0;
  const long qa = hmd->matter->qatom();
  for (long na = 0; na < qa; na++) {
    const long qs = hmd->apacs[na]->get_qshell();
    qst += qs;
  }
  // Sum up the cross-sections.
  DynLinArr<double> cs(qst);
  DynLinArr<long> nat(qst);
  DynLinArr<long> nsh(qst);
  double s = 0.0;
  long nst = 0;
  for (long na = 0; na < qa; na++) {
    const long qs = hmd->apacs[na]->get_qshell();
    double at_weight_quan = hmd->matter->weight_quan(na);
    for (long ns = 0; ns < qs; ns++) {
      cs[nst] = hmd->apacs[na]->get_ICS(ns, energy) * at_weight_quan;
      // threshold is taken into account in apacs[na]->get_ACS(ns,..
      nat[nst] = na;
      nsh[nst] = ns;
      s += cs[nst];
      nst++;
    }
  }
  if (s_print_listing == 1) Iprintn(mcout, s);
  // Calculate the path length.
  //s=s * hmd->eldens / hmd->matter->Z_mean() * C1_MEV_CM;
  s = s * 1.0e-18 * AVOGADRO / (hmd->matter->A_mean() / (gram / mole)) *
      hmd->matter->density() / (gram / cm3);
  if (s_print_listing == 1) Iprintn(mcout, s);
  const double path_length = 1.0 / s;  // cm
  if (s_print_listing == 1) Iprint2n(mcout, energy, path_length);
  // Draw a random step length.
  double xleng = -path_length * log(1.0 - SRANLUX());
  if (s_print_listing == 1) {
    Iprintn(mcout, xleng);
    Iprintn(mcout, nextpos.prange / cm);
  }
  if (xleng * cm < nextpos.prange) {
    s_photon_absorbed = 1;
#ifdef SFER_PHOTOEL
    s_sfer_photoel = 1;  // assumes that virtual photons are already
                         // absorbed and s_sfer_photoel is  0 for them
#endif
    // Sample the shell.
    chispre(cs);
    double r = chisran(SRANLUX(), cs);
    //Iprintn(mcout, r);
    long n = long(r);
    if (n < 0) n = 0;
    if (n > nst - 1) n = nst - 1;
    if (s_print_listing == 1) Iprintn(mcout, n);
    na_absorbing = nat[n];
    ns_absorbing = nsh[n];
    nextpos.prange = xleng * cm;
    nextpos.pt = currpos.pt + nextpos.prange * currpos.dir;
    nextpos.ptloc = nextpos.pt;
    nextpos.tid.up_absref(&nextpos.ptloc);
  }
}

void HeedPhoton::physics_after_new_speed(void) {
  mfunname("void HeedPhoton::physics_after_new_speed(void)");
  if (s_print_listing == 1) {
    mcout << "HeedPhoton::physics_after_new_speed is started\n";
  }
  // Stop if the photon has not been absorbed.
  if (s_photon_absorbed != 1) return;
  // Stop if the delta electrons have already been generated.
  if (s_delta_generated != 0) return;
  // Get least address of volume
  const absvol* av = currpos.G_lavol();  
  HeedMatterDef* hmd = NULL;
  const EnTransfCSType* etcst = dynamic_cast<const EnTransfCSType*>(av);
  if (etcst) {
    hmd = etcst->etcs->hmd.getver();
  } else {
    const HeedDeltaElectronCSType* hmecst =
        dynamic_cast<const HeedDeltaElectronCSType*>(av);
    if (hmecst) {
      hmd = hmecst->hdecs->hmd.get();
    }
  }
  // Stop here if we couldn't retrieve the material definition.
  if (!hmd) return;
  // Generate delta-electrons
  DynLinArr<double> el_energy;
  DynLinArr<double> ph_energy;
  hmd->apacs[na_absorbing]->get_escape_particles(ns_absorbing, energy, 
                                                 el_energy, ph_energy);
  if (s_print_listing == 1) {
    mcout << "The condition:\n";
    Iprint2n(mcout, na_absorbing, ns_absorbing);
    mcout << "The decay products:\n";
    Iprintn(mcout, el_energy);
    Iprintn(mcout, ph_energy);
  }
  const long qel = el_energy.get_qel();
  for (long nel = 0; nel < qel; nel++) {
    vec vel;
    if (nel == 0) {  // assumed it is photoelectron
#ifdef SFER_PHOTOEL
      if (s_sfer_photoel == 1) {
        vel.random_sfer_vec();
      } else {
        vel = currpos.dir;
      }
#else
      vel = currpos.dir;  // direction is OK
#endif
    } else {
      vel.random_sfer_vec();
    }
    const double gam_1 = el_energy[nel] / ELMAS;
    const double inv = 1.0 / (gam_1 + 1.0);
    const double beta = sqrt(1.0 - inv * inv);
    const double mod_v = beta * c_light;
    vel = vel * mod_v;
    if (s_print_listing == 1) {
      mcout << "Initializing delta electron\n";
      Iprint4n(mcout, el_energy[nel], gam_1, beta, mod_v);
    }
    ActivePtr<gparticle> ac;
    ac.pass(
        new HeedDeltaElectron(currpos.tid.eid[0].amvol.getver(), currpos.pt,
                              vel, currpos.time, particle_number));
    // particle_bank.insert_after(particle_bank.get_last_node(), ac);
    particle_bank.push_back(ac);
    // particle_bank.push_back(HeedDeltaElectron(currpos.tid.eid[0].amvol.getver(), currpos.pt,
    //                         vel, currpos.time, particle_number));
  }
  const long qph = ph_energy.get_qel();
  for (long nph = 0; nph < qph; nph++) {
    vec vel;
    vel.random_sfer_vec();
    vel *= c_light;
    if (s_print_listing == 1) {
      mcout << "Initializing photon\n";
      Iprint2n(mcout, el_energy[nph], vel);
    }
    ActivePtr<gparticle> ac;
    ac.pass(new HeedPhoton(currpos.tid.eid[0].amvol.getver(), currpos.pt, vel,
                           currpos.time, particle_number, ph_energy[nph]));
    // particle_bank.insert_after(particle_bank.get_last_node(), ac);
    particle_bank.push_back(ac);
    // particle_bank.push_back(HeedPhoton(currpos.tid.eid[0].amvol.getver(), currpos.pt, vel,
    //                                    currpos.time, particle_number, ph_energy[nph]));
  }
  s_delta_generated = 1;
  s_life = 0;
  if (s_print_listing == 1) {
    mcout << "HeedPhoton::physics_after_new_speed is exited\n";
  }
}

void HeedPhoton::print(std::ostream& file, int l) const {
  if (l < 0) return;
  Ifile << "HeedPhoton (l=" << l << "): particle_number=" << particle_number
        << " energy=" << energy << "MeV\n";
  if (l == 1) return;
  indn.n += 2;
  Ifile << "s_photon_absorbed=" << s_photon_absorbed
        << " na_absorbing=" << na_absorbing
        << " ns_absorbing=" << ns_absorbing
        << " s_delta_generated=" << s_delta_generated
#ifdef SFER_PHOTOEL
        << " s_sfer_photoel=" << s_sfer_photoel
#endif
        << " parent_particle_number=" << parent_particle_number
        << " s_print_listing=" << s_print_listing << '\n';
  gparticle::print(file, l - 1);
  indn.n -= 2;
}

}
