#include <iomanip>
#include <numeric>
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/random/ranluxint.h"
#include "wcpplib/random/pois.h"
#include "wcpplib/math/kinem.h"
#include "wcpplib/math/tline.h"
#include "heed++/code/HeedParticle.h"
#include "heed++/code/HeedCluster.h"
#include "heed++/code/HeedPhoton.h"
#include "heed++/code/EnTransfCS.h"

// 2003-2008, I. Smirnov

namespace Heed {

HeedParticle::HeedParticle(manip_absvol* primvol, const point& pt,
                           const vec& vel, vfloat time, particle_def* fpardef,
                           std::list<ActivePtr<gparticle> >& particleBank,
                           HeedFieldMap* fieldmap, const bool fs_loss_only,
                           const bool fs_print_listing)
    : eparticle(primvol, pt, vel, time, fpardef, fieldmap),
      s_print_listing(fs_print_listing),
      particle_number(last_particle_number++),
      s_loss_only(fs_loss_only),
      m_particleBank(&particleBank) {

  mfunname("HeedParticle::HeedParticle(...)");
  etransf.reserve(100);
  natom.reserve(100);
  nshell.reserve(100);
  m_clusterBank.reserve(100);
}

void HeedParticle::physics() {
  mfunname("void HeedParticle::physics(void)");
  if (s_print_listing) {
    mcout << "HeedParticle::physics is started\n";
    Iprintn(mcout, currpos.prange);
  }
  etransf.clear();
  natom.clear();
  nshell.clear();
  if (currpos.prange <= 0.0) return;
  // Get least address of volume
  const absvol* av = currpos.G_lavol();
  const EnTransfCSType* etcst = dynamic_cast<const EnTransfCSType*>(av);
  // Check if dynamic cast was successful.
  if (!etcst) return;

  EnTransfCS* etcs = etcst->etcs.getver();
  HeedMatterDef* hmd = etcs->hmd.getver();
  MatterDef* amatter = hmd->matter.getver();
  EnergyMesh* emesh = hmd->energy_mesh.getver();
  const double* aetemp = hmd->energy_mesh->get_ae();
  PointCoorMesh<double, const double*> pcm(emesh->get_q() + 1, &(aetemp));
  const long qa = amatter->qatom();
  if (s_print_listing) Iprintn(mcout, qa);
  basis tempbas(currpos.dir, "tempbas");
  for (long na = 0; na < qa; ++na) {
    if (s_print_listing) Iprintn(mcout, na);
    const long qs = hmd->apacs[na]->get_qshell();
    for (long ns = 0; ns < qs; ++ns) {
      if (s_print_listing) Iprintn(mcout, ns);
      if (etcs->quan[na][ns] <= 0.0) continue;
      int ierror = 0;
      const long qt = pois(etcs->quan[na][ns] * currpos.prange / cm, ierror);
      check_econd11a(ierror, == 1,
                     " etcs->quan[na][ns]=" << etcs->quan[na][ns]
                                             << " currpos.prange/cm="
                                             << currpos.prange / cm << '\n',
                     mcerr);
      if (s_print_listing) Iprintn(mcout, qt);
      if (qt <= 0) continue;
      point curpt = prevpos.pt;
      vec dir = unit_vec(currpos.pt - prevpos.pt);
      const double range = length(currpos.pt - prevpos.pt);
      if (s_print_listing) {
        Iprint(mcout, curpt);
        Iprint(mcout, dir);
        Iprintn(mcout, range);
      }
      for (long nt = 0; nt < qt; ++nt) {
        const double rn = SRANLUX();
        if (s_print_listing) {
          Iprintn(mcout, rn);
          Iprintn(mcout, etcs);
          Iprintn(mcout, etcs->fadda[na][ns][1]);
        }
        const double r = t_hisran_step_ar<double, std::vector<double>,
                                    PointCoorMesh<double, const double*> >(
            pcm, etcs->fadda[na][ns], rn);

        // Convert to internal units.
        etransf.push_back(r * MeV);
        if (s_print_listing) Iprint2n(mcout, nt, etransf.back());
        natom.push_back(na);
        nshell.push_back(ns);
        const double arange = SRANLUX() * range;
        point pt = curpt + dir * arange;
        point ptloc = pt;
        prevpos.tid.up_absref(&ptloc);
        if (s_loss_only) continue;
        if (s_print_listing) mcout << "generating new cluster\n";
        m_clusterBank.push_back(HeedCluster(etransf.back(), 0, 
                                            pt, ptloc, prevpos.tid, na, ns));
        const double Ep0 = mass * c_squared + curr_kin_energy;
        const double Ep1 = Ep0 - etransf.back();
        const double Mp = mass * c_squared;
        const double Mt = electron_def.mass * c_squared;
        double theta_p, theta_t;
        theta_two_part(Ep0, Ep1, Mp, Mt, theta_p, theta_t);
        vec vel;
        vel.random_conic_vec(fabs(theta_t));
        vel.down(&tempbas);  // direction is OK
        vel *= c_light;
        // HS
        double speed = length(vel);
        double time = arange / speed;
        if (s_print_listing) mcout << "generating new virtual photon\n";
        HeedPhoton hp(currpos.tid.eid[0].amvol.getver(), pt, vel, time,
                      particle_number, etransf.back(),
                      *m_particleBank, m_fieldMap);
        hp.s_photon_absorbed = true;
        hp.s_delta_generated = false;
        hp.na_absorbing = na;
        hp.ns_absorbing = ns;
        ActivePtr<gparticle> ac;
        ac.put(&hp);
        m_particleBank->push_back(ac);
      }
    }
  }
  if (s_print_listing) {
    const double sum = std::accumulate(etransf.begin(), etransf.end(), 0.); 
    Iprint2n(mcout, etransf.size(), sum);
    mcout << "Exiting HeedParticle::physics\n";
  }
}

void HeedParticle::print(std::ostream& file, int l) const {
  if (l < 0) return;
  Ifile << "HeedParticle (l=" << l << "): particle_number=" << particle_number
        << " type=";
  print_notation(file);
  file << std::endl;
  if (l == 1) return;
  mparticle::print(file, l - 1);
  const double sum = std::accumulate(etransf.begin(), etransf.end(), 0.);
  Iprintn(mcout, sum);
  Iprintn(mcout, etransf.size());
  if (l >= 5) {
    Ifile << "   nt  natom nshell transferred energy\n";
    const long qt = etransf.size();
    for (long nt = 0; nt < qt; nt++) {
      Ifile << std::setw(3) << nt << ' ' << std::setw(3) << natom[nt] << ' '
            << std::setw(3) << nshell[nt] << ' ' << std::setw(12)
            << etransf[nt] << '\n';
    }
  }
}
}
