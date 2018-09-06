#include <iomanip>
#include <numeric>
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/random/ranluxint.h"
#include "wcpplib/random/pois.h"
#include "wcpplib/math/kinem.h"
#include "wcpplib/math/tline.h"
#include "heed++/code/HeedParticle_BGM.h"
#include "heed++/code/HeedCluster.h"
#include "heed++/code/HeedPhoton.h"
#include "heed++/code/EnTransfCS_BGM.h"

// 2003-2008, I. Smirnov

namespace Heed {

using CLHEP::c_light;
using CLHEP::c_squared;
using CLHEP::cm;
using CLHEP::MeV;

HeedParticle_BGM::HeedParticle_BGM(manip_absvol* primvol, const point& pt,
                                   const vec& vel, vfloat time,
                                   particle_def* fpardef,
                                   HeedFieldMap* fieldmap,
                                   const bool fs_loss_only,
                                   const bool fs_print_listing)
    : eparticle(primvol, pt, vel, time, fpardef, fieldmap),
      s_print_listing(fs_print_listing),
      particle_number(last_particle_number++),
      s_loss_only(fs_loss_only) {
  mfunname("HeedParticle_BGM::HeedParticle_BGM(...)");
  etransf.reserve(100);
  natom.reserve(100);
  nshell.reserve(100);
  m_clusterBank.reserve(100);
}

void HeedParticle_BGM::physics(std::vector<gparticle*>& secondaries) {
  mfunname("void HeedParticle_BGM::physics()");
  if (s_print_listing) {
    mcout << "HeedParticle_BGM::physics is started\n";
    Iprintn(mcout, currpos.prange);
  }
  etransf.clear();
  natom.clear();
  nshell.clear();
  if (currpos.prange <= 0.0) return;
  // Get least address of volume
  const absvol* av = currpos.tid.G_lavol();
  const EnTransfCS_BGM* etcs = dynamic_cast<const EnTransfCS_BGM*>(av);
  // Check if dynamic cast was successful.
  if (!etcs) return;
  HeedMatterDef* hmd = etcs->hmd;
  MatterDef* matter = hmd->matter;
  EnergyMesh* emesh = hmd->energy_mesh;
  const double* aetemp = hmd->energy_mesh->get_ae();
  PointCoorMesh<double, const double*> pcm_e(emesh->get_q() + 1, &(aetemp));

  const double bg = sqrt(curr_gamma_1 * (curr_gamma_1 + 2.0));
  PointCoorMesh<double, std::vector<double> > pcm(etcs->mesh->q,
                                                  &(etcs->mesh->x));
  long n1, n2;
  double b1, b2;
  int s_ret = pcm.get_interval(bg, n1, b1, n2, b2);
  if (s_ret != 1) {
    mcerr << "ERROR in void HeedParticle_BGM::physics()\n";
    mcerr << "beta*gamma is outside range of cross-section table\n";
    std::streamsize old_prec = mcerr.precision(15);
    Iprint2n(mcerr, curr_gamma_1, bg);
    mcerr.precision(old_prec);
    Iprint2n(mcerr, n1, n2);
    Iprint2n(mcerr, b1, b2);
    Iprintn(mcerr, etcs->mesh);
    mcerr << "This particle is:\n";
    print(mcerr, 2);
    mcerr << "This volume is:\n";
    av->print(mcerr, 2);
    spexit(mcerr);
    return;
  }

  const long qa = matter->qatom();
  if (s_print_listing) Iprintn(mcout, qa);
  basis tempbas(currpos.dir, "tempbas");
  for (long na = 0; na < qa; ++na) {
    if (s_print_listing) Iprintn(mcout, na);
    long qs = hmd->apacs[na]->get_qshell();
    for (long ns = 0; ns < qs; ++ns) {
      if (s_print_listing) Iprintn(mcout, ns);
      const double y1 = etcs->etcs_bgm[n1].quan[na][ns];
      const double y2 = etcs->etcs_bgm[n2].quan[na][ns];
      const double mean_pois = y1 + (bg - b1) * (y2 - y1) / (b2 - b1);
      if (mean_pois <= 0.) continue;
      int ierror;
      long qt = pois(mean_pois * currpos.prange / cm, ierror);
      check_econd11a(ierror, == 1, " mean_pois=" << mean_pois
                                                 << " currpos.prange/cm="
                                                 << currpos.prange / cm << '\n',
                     mcerr);
      if (s_print_listing) Iprintn(mcout, qt);
      if (qt <= 0) continue;
      point curpt = prevpos.pt;
      vec dir = unit_vec(currpos.pt - prevpos.pt);
      // This approximation ignores curvature
      const double range = (currpos.pt - prevpos.pt).length();
      if (s_print_listing) Iprint3n(mcout, curpt, dir, range);
      for (long nt = 0; nt < qt; nt++) {
        // Sample the energy transfer in this collision.
        double rn = SRANLUX();
        const double r1 = t_hisran_step_ar<
            double, std::vector<double>, PointCoorMesh<double, const double*> >(
            pcm_e, etcs->etcs_bgm[n1].fadda[na][ns], rn);
        const double r2 = t_hisran_step_ar<
            double, std::vector<double>, PointCoorMesh<double, const double*> >(
            pcm_e, etcs->etcs_bgm[n2].fadda[na][ns], rn);
        const double r = r1 + (bg - b1) * (r2 - r1) / (b2 - b1);
        if (s_print_listing) {
          Iprintn(mcout, rn);
          Iprint3n(mcout, r1, r2, r);
        }
        // Convert to internal units.
        const double et = r * MeV;
        etransf.push_back(et);
        natom.push_back(na);
        nshell.push_back(ns);
        if (s_print_listing) Iprint2n(mcout, nt, et);
        // Sample the position of the collision.
        const double arange = SRANLUX() * range;
        point pt = curpt + dir * arange;
        point ptloc = pt;
        prevpos.tid.up_absref(&ptloc);
        if (s_loss_only) continue;
        if (s_print_listing) mcout << "generating new cluster\n";
        m_clusterBank.push_back(
            HeedCluster(et, 0, pt, ptloc, prevpos.tid, na, ns));
        // Generate a virtual photon.
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
        if (s_print_listing) mcout << "generating new virtual photon\n";
        HeedPhoton* hp =
            new HeedPhoton(currpos.tid.eid[0], pt, vel, currpos.time,
                           particle_number, et, m_fieldMap);
        hp->s_photon_absorbed = true;
        hp->s_delta_generated = false;
        hp->na_absorbing = na;
        hp->ns_absorbing = ns;
        secondaries.push_back(hp);
      }
    }
  }
  if (s_print_listing) {
    const double sum = std::accumulate(etransf.begin(), etransf.end(), 0.);
    Iprint2n(mcout, etransf.size(), sum);
    mcout << "Exiting HeedParticle_BGM::physics\n";
  }
}

void HeedParticle_BGM::print(std::ostream& file, int l) const {
  if (l < 0) return;
  Ifile << "HeedParticle_BGM (l=" << l
        << "): particle_number=" << particle_number << " type=";
  print_notation(file);
  file << std::endl;
  if (l == 1) return;
  mparticle::print(file, l - 1);
  const double sum = std::accumulate(etransf.begin(), etransf.end(), 0.);
  Iprintn(mcout, sum);
  Iprintn(mcout, etransf.size());
  if (l >= 5) {
    Ifile << "   nt  natom nshell etransf\n";
    const long qt = etransf.size();
    for (long nt = 0; nt < qt; nt++) {
      Ifile << std::setw(3) << nt << ' ' << std::setw(3) << natom[nt] << ' '
            << std::setw(3) << nshell[nt] << ' ' << std::setw(12) << etransf[nt]
            << '\n';
    }
  }
}
}
