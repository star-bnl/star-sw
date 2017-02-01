#include <iomanip>
#include <iostream>
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/random/ranluxint.h"
#include "wcpplib/random/pois.h"
#include "wcpplib/math/kinem.h"
#include "wcpplib/math/tline.h"
#include "heed++/code/HeedParticle_BGM.h"
#include "heed++/code/HeedCluster.h"
#include "heed++/code/HeedPhoton.h"
#include "heed++/code/EnTransfCS_BGM.h"

/*
2003-2008, I. Smirnov
*/

namespace Heed {

HeedParticle_BGM::HeedParticle_BGM(manip_absvol* primvol, const point& pt,
                                   const vec& vel, vfloat time,
                                   particle_def* fpardef, 
                                   std::list<ActivePtr<gparticle> >& particleBank,
                                   HeedFieldMap* fieldmap,
                                   int fs_loss_only,
                                   int fs_print_listing)
    : eparticle(primvol, pt, vel, time, fpardef, fieldmap),
      s_print_listing(fs_print_listing),
      particle_number(last_particle_number++),
      transferred_energy_in_step(0.0),
      qtransfer(0),
      s_loss_only(fs_loss_only),
      m_particleBank(&particleBank) {
  mfunname("HeedParticle_BGM::HeedParticle_BGM(...)");
  transferred_energy.reserve(100);
  natom.reserve(100);
  nshell.reserve(100);
  m_clusterBank.reserve(100);
}

void HeedParticle_BGM::physics(void) {
  mfunname("void HeedParticle_BGM::physics(void)");
  if (s_print_listing == 1) {
    mcout << "HeedParticle_BGM::physics is started\n";
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
  const EnTransfCS_BGM_Type* etcs_bgm_t =
      dynamic_cast<const EnTransfCS_BGM_Type*>(av);
  // Check if dynamic cast was successful.
  if (!etcs_bgm_t) return;
  // All these objects are expected to exist till the end of this function,
  // if they exist now:
  EnTransfCS_BGM* aetcs_bgm = etcs_bgm_t->etcs_bgm.getver();
  HeedMatterDef* ahmd = aetcs_bgm->hmd.getver();
  MatterDef* amatter = ahmd->matter.getver();
  EnergyMesh* a_energy_mesh = ahmd->energy_mesh.getver();
  const double* aetemp = ahmd->energy_mesh->get_ae();
  PointCoorMesh<double, const double*> pcm_e(a_energy_mesh->get_q() + 1,
                                               &(aetemp));
  const long qa = amatter->qatom();
  if (s_print_listing == 1) Iprintn(mcout, qa);
  // long qe = a_energy_mesh->get_q();
  basis tempbas(currpos.dir, "tempbas");
  for (long na = 0; na < qa; ++na) {
    if (s_print_listing == 1) Iprintn(mcout, na);
    long qs = ahmd->apacs[na]->get_qshell();
    for (long ns = 0; ns < qs; ++ns) {
      if (s_print_listing == 1) Iprintn(mcout, ns);
      long qt = 0;
#ifdef SINGLE_TRANSFER
      if (aetcs == aetcs_single_transf && na == na_single_transf &&
          ns == ns_single_transf) {
        qt = 1;
      }
#else
      double bg = sqrt(curr_gamma_1 * (curr_gamma_1 + 2.0));

      PointCoorMesh<double, DynLinArr<double> > pcm(aetcs_bgm->mesh->q,
                                                    &(aetcs_bgm->mesh->x));

      long n1;
      long n2;
      double b1;
      double b2;
      int s_ret = pcm.get_interval(bg, n1, b1, n2, b2);
      if (s_ret != 1) {
        mcerr << "ERROR in void HeedParticle_BGM::physics(void)\n";
        mcerr << "betta*gamma is outside range of Cross Section table\n";

        std::streamsize old_prec = mcerr.precision(15);
        Iprint2n(mcerr, curr_gamma_1, bg);
        mcerr.precision(old_prec);
        Iprint2n(mcerr, n1, n2);
        Iprint2n(mcerr, b1, b2);
        Iprintn(mcerr, aetcs_bgm->mesh);
        mcerr << "This particle is:\n";
        print(mcerr, 2);
        mcerr << "This volume is:\n";
        av->print(mcerr, 2);
        // funnw.ehdr(mcerr);
        spexit(mcerr);
      }

      double y1 = aetcs_bgm->etcs_bgm[n1].quan[na][ns];
      double y2 = aetcs_bgm->etcs_bgm[n2].quan[na][ns];

      double mean_pois = y1 + (bg - b1) * (y2 - y1) / (b2 - b1);

      if (mean_pois > 0.0) {
        int ierror;
        qt = pois(mean_pois * currpos.prange / cm, ierror);
        check_econd11a(ierror, == 1,
                       " mean_pois=" << mean_pois << " currpos.prange/cm="
                                     << currpos.prange / cm << '\n',
                       mcerr);
      }
#endif
      if (s_print_listing == 1) Iprintn(mcout, qt);

      if (qt > 0) {
        point curpt = prevpos.pt;
        vec dir = unit_vec(currpos.pt - prevpos.pt);
        // this approximation ignores curvature
        double range = length(currpos.pt - prevpos.pt);
        if (s_print_listing == 1) {
          Iprint(mcout, curpt);
          Iprint(mcout, dir);
          Iprintn(mcout, range);
        }
        for (long nt = 0; nt < qt; nt++) {
#ifdef SINGLE_TRANSFER
          transferred_energy.append(ener_single_transf);
#else
          double rn = SRANLUX();
          double r1 = t_hisran_step_ar<double, DynLinArr<double>,
                                       PointCoorMesh<double, const double*> >(
              pcm_e, aetcs_bgm->etcs_bgm[n1].fadda[na][ns], rn);
          double r2 = t_hisran_step_ar<double, DynLinArr<double>,
                                       PointCoorMesh<double, const double*> >(
              pcm_e, aetcs_bgm->etcs_bgm[n2].fadda[na][ns], rn);
          double r = r1 + (bg - b1) * (r2 - r1) / (b2 - b1);
          if (s_print_listing == 1) {
            Iprintn(mcout, rn);
            Iprint3n(mcout, r1, r2, r);
          }
          transferred_energy.push_back(r * MeV);  // internal units
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
          m_clusterBank.push_back(
              HeedCluster(transferred_energy[qtransfer - 1], 0, pt, ptloc,
                          prevpos.tid, na, ns));
          vec vel;
          const double Ep0 = mass * c_squared + curr_kin_energy;
          const double Ep1 = Ep0 - transferred_energy[qtransfer - 1];
          const double Mp = mass * c_squared;
          const double Mt = electron_def.mass * c_squared;
          double theta_p, theta_t;
          theta_two_part(Ep0, Ep1, Mp, Mt, theta_p, theta_t);
          vel.random_conic_vec(fabs(theta_t));
          vel.down(&tempbas);  // direction is OK
          vel *= c_light;
          if (s_print_listing == 1) mcout << "generating new virtual photon\n";
          HeedPhoton hp(currpos.tid.eid[0].amvol.getver(),
                        pt, vel, currpos.time, particle_number,
                        transferred_energy[qtransfer - 1], 
                        *m_particleBank, m_fieldMap, 0);
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
    mcout << "Exiting HeedParticle_BGM::physics\n";
  }
}

void HeedParticle_BGM::print(std::ostream& file, int l) const {
  if (l >= 0) {
    Ifile << "HeedParticle_BGM (l=" << l
          << "): particle_number=" << particle_number << " type=";
    print_notation(file);
    file << std::endl;
    if (l == 1) return;
    //file<<'\n';
    mparticle::print(file, l - 1);
    Iprintn(mcout, transferred_energy_in_step);
    Iprintn(mcout, qtransfer);
    if (l >= 5) {
      long nt;
      Ifile << "   nt  natom nshell transferred_energy\n";

      for (nt = 0; nt < qtransfer; nt++) {
        Ifile << std::setw(3) << nt << ' ' << std::setw(3) << natom[nt] << ' ' << std::setw(3)
              << nshell[nt] << ' ' << std::setw(12) << transferred_energy[nt]
              << '\n';
      }
    }
  }

}

}
