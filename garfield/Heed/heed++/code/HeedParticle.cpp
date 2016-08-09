#include <iomanip>
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/random/ranluxint.h"
#include "wcpplib/random/pois.h"
#include "wcpplib/math/kinem.h"
#include "wcpplib/math/tline.h"
#include "heed++/code/HeedParticle.h"
#include "heed++/code/HeedCluster.h"
#include "heed++/code/HeedDeltaElectron.h"
#include "heed++/code/HeedPhoton.h"
#include "heed++/code/EnTransfCS.h"
/*
2003-2008, I. Smirnov
*/

namespace Heed {

HeedParticle::HeedParticle(manip_absvol* primvol, const point& pt,
                           const vec& vel, vfloat time, particle_def* fpardef,
                           std::list<ActivePtr<gparticle> >& particleBank,
                           int fs_loss_only, int fs_print_listing)
    : eparticle(primvol, pt, vel, time, fpardef),
      s_print_listing(fs_print_listing),
      particle_number(last_particle_number++),
      transferred_energy_in_step(0.0),
      qtransfer(0),
      s_loss_only(fs_loss_only),
      m_particleBank(&particleBank) {

  mfunname("HeedParticle::HeedParticle(...)");
  transferred_energy.reserve(100);
  natom.reserve(100);
  nshell.reserve(100);
  m_clusterBank.reserve(100);
}

void HeedParticle::physics(void) {
  mfunname("void HeedParticle::physics(void)");
  if (s_print_listing == 1) {
    mcout << "HeedParticle::physics is started\n";
    Iprintn(mcout, currpos.prange);
  }
  transferred_energy_in_step = 0.0;
  qtransfer = 0;
  transferred_energy.clear();
  natom.clear();
  nshell.clear();
  if (currpos.prange <= 0.0) return;
  // Get least address of volume
  const absvol* av = currpos.G_lavol();
  const EnTransfCSType* etcst = dynamic_cast<const EnTransfCSType*>(av);
  // Check if dynamic cast was successful.
  if (!etcst) return;

  EnTransfCS* aetcs = etcst->etcs.getver();
  HeedMatterDef* ahmd = aetcs->hmd.getver();
  MatterDef* amatter = ahmd->matter.getver();
  EnergyMesh* a_energy_mesh = ahmd->energy_mesh.getver();
  const double* aetemp = ahmd->energy_mesh->get_ae();
  PointCoorMesh<double, const double*> pcm_e(a_energy_mesh->get_q() + 1,
                                             &(aetemp));
  const long qa = amatter->qatom();
  if (s_print_listing == 1) Iprintn(mcout, qa);
  basis tempbas(currpos.dir, "tempbas");
  for (long na = 0; na < qa; ++na) {
    if (s_print_listing == 1) Iprintn(mcout, na);
    const long qs = ahmd->apacs[na]->get_qshell();
    for (long ns = 0; ns < qs; ++ns) {
      if (s_print_listing == 1) Iprintn(mcout, ns);
      long qt = 0;
#ifdef SINGLE_TRANSFER
      if (aetcs == aetcs_single_transf && na == na_single_transf &&
          ns == ns_single_transf) {
        qt = 1;
      }
#else
      if (aetcs->quan[na][ns] > 0.0) {
        int ierror;
        qt = pois(aetcs->quan[na][ns] * currpos.prange / cm, ierror);
        check_econd11a(ierror, == 1,
            " aetcs->quan[na][ns]=" << aetcs->quan[na][ns]
                                    << " currpos.prange/cm=" << currpos.prange /
                                                                    cm << '\n',
            mcerr);
      }
#endif
      if (s_print_listing == 1) Iprintn(mcout, qt);
      if (qt > 0) {
        point curpt = prevpos.pt;
        vec dir = unit_vec(currpos.pt - prevpos.pt);
        double range = length(currpos.pt - prevpos.pt);
        if (s_print_listing == 1) {
          Iprint(mcout, curpt);
          Iprint(mcout, dir);
          Iprintn(mcout, range);
        }
        for (long nt = 0; nt < qt; ++nt) {
#ifdef SINGLE_TRANSFER
          transferred_energy.push_back(ener_single_transf);
#else
          double rn = SRANLUX();
          if (s_print_listing == 1) {
            Iprintn(mcout, rn);
            Iprintn(mcout, aetcs);
            Iprintn(mcout, aetcs->fadda[na][ns][1]);
          }
          double r = t_hisran_step_ar<double, DynLinArr<double>,
                                      PointCoorMesh<double, const double*> >(
              pcm_e, aetcs->fadda[na][ns], rn);

          // Convert to internal units.
          transferred_energy.push_back(r * MeV);
#endif
          if (s_print_listing == 1) {
            Iprint2n(mcout, nt, transferred_energy[qtransfer]);
          }
          transferred_energy_in_step += transferred_energy[qtransfer];
          natom.push_back(na);
          nshell.push_back(ns);
#ifdef SINGLE_TRANSFER
          double arange = 0.5 * range;
#else
          double arange = SRANLUX() * range;
#endif
          point pt = curpt + dir * arange;
          point ptloc = pt;
          prevpos.tid.up_absref(&ptloc);
          qtransfer++;
          if (s_loss_only != 0) continue;
          if (s_print_listing == 1) mcout << "generating new cluster\n";
          m_clusterBank.push_back(HeedCluster(transferred_energy[qtransfer - 1],
                                              0, pt, ptloc, prevpos.tid, na, ns));

          double Ep0 = mass * c_squared + curr_kin_energy;
          double Ep1 = Ep0 - transferred_energy[qtransfer - 1];
          double Mp = mass;
	  if (Ep1 <= Mp * c_squared) continue; // ??
          double Mt = electron_def.mass;
          double theta_p, theta_t;
          theta_two_part(Ep0, Ep1, Mp, Mt, theta_p, theta_t);
          vec vel;
          vel.random_conic_vec(fabs(theta_t));
          vel.down(&tempbas);  // direction is OK
          vel *= c_light;
          // HS
          double speed = length(vel);
          double time = arange / speed;
          if (s_print_listing == 1) {
            mcout << "generating new virtual photon\n";
          }
          HeedPhoton hp(currpos.tid.eid[0].amvol.getver(), pt, vel, time,
                        particle_number, transferred_energy[qtransfer - 1],
                        *m_particleBank, 0);
          hp.s_photon_absorbed = 1;
          hp.s_delta_generated = 0;
          hp.na_absorbing = na;
          hp.ns_absorbing = ns;
          ActivePtr<gparticle> ac;
          ac.put(&hp);
          m_particleBank->push_back(ac);
        }
      }
    }
  }
  if (s_print_listing == 1) {
    Iprint2n(mcout, qtransfer, transferred_energy_in_step);
    mcout << "Exiting HeedParticle::physics\n";
  }
}

void HeedParticle::print(std::ostream& file, int l) const {
  if (l >= 0) {
    Ifile << "HeedParticle (l=" << l << "): particle_number=" << particle_number
          << " type=";
    print_notation(file);
    file << std::endl;
    if (l == 1) return;
    mparticle::print(file, l - 1);
    Iprintn(mcout, transferred_energy_in_step);
    Iprintn(mcout, qtransfer);
    if (l >= 5) {
      Ifile << "   nt  natom nshell transferred_energy\n";
      for (long nt = 0; nt < qtransfer; nt++) {
        Ifile << std::setw(3) << nt << ' ' << std::setw(3) << natom[nt] << ' '
              << std::setw(3) << nshell[nt] << ' ' << std::setw(12)
              << transferred_energy[nt] << '\n';
      }
    }
  }

}

}
