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

using CLHEP::c_light;
using CLHEP::c_squared;
using CLHEP::cm;
using CLHEP::MeV;

HeedParticle::HeedParticle(manip_absvol* primvol, const point& pt,
                           const vec& vel, vfloat ftime, particle_def* fpardef,
                           HeedFieldMap* fieldmap, const bool floss_only,
                           const bool fprint_listing)
    : eparticle(primvol, pt, vel, ftime, fpardef, fieldmap),
      m_print_listing(fprint_listing),
      m_particle_number(last_particle_number++),
      m_loss_only(floss_only) {

  mfunname("HeedParticle::HeedParticle(...)");
  m_etransf.reserve(100);
  m_natom.reserve(100);
  m_nshell.reserve(100);
}

void HeedParticle::physics(std::vector<gparticle*>& secondaries) {
  mfunname("void HeedParticle::physics()");
  if (m_print_listing) {
    mcout << "HeedParticle::physics is started\n";
    Iprintn(mcout, m_currpos.prange);
  }
  m_etransf.clear();
  m_natom.clear();
  m_nshell.clear();
  if (m_currpos.prange <= 0.0) return;
  // Get local volume.
  const absvol* av = m_currpos.tid.G_lavol();
  auto etcs = dynamic_cast<const EnTransfCS*>(av);
  if (!etcs) return;
  HeedMatterDef* hmd = etcs->hmd;
  MatterDef* matter = hmd->matter;
  EnergyMesh* emesh = hmd->energy_mesh;
  const double* aetemp = emesh->get_ae();
  PointCoorMesh<double, const double*> pcm(emesh->get_q() + 1, &(aetemp));
  const long qa = matter->qatom();
  if (m_print_listing) Iprintn(mcout, qa);
  basis tempbas(m_currpos.dir, "tempbas");
  for (long na = 0; na < qa; ++na) {
    if (m_print_listing) Iprintn(mcout, na);
    const long qs = hmd->apacs[na]->get_qshell();
    for (long ns = 0; ns < qs; ++ns) {
      if (m_print_listing) Iprintn(mcout, ns);
      if (etcs->quan[na][ns] <= 0.0) continue;
      // Sample the number of collisions for this shell.
      int ierror = 0;
      const long qt = pois(etcs->quan[na][ns] * m_currpos.prange / cm, ierror);
      check_econd11a(ierror, == 1,
                     " etcs->quan[na][ns]=" << etcs->quan[na][ns]
                                            << " currpos.prange/cm="
                                            << m_currpos.prange / cm << '\n',
                     mcerr);
      if (m_print_listing) Iprintn(mcout, qt);
      if (qt <= 0) continue;
      point curpt = m_prevpos.pt;
      vec dir = unit_vec(m_currpos.pt - m_prevpos.pt);
      const double range = (m_currpos.pt - m_prevpos.pt).length();
      if (m_print_listing) Iprint3n(mcout, curpt, dir, range);
      for (long nt = 0; nt < qt; ++nt) {
        // Sample the energy transfer in this collision.
        const double rn = SRANLUX();
        if (m_print_listing) Iprint3n(mcout, rn, etcs, etcs->fadda[na][ns][1]);
        const double r = t_hisran_step_ar<
            double, std::vector<double>, PointCoorMesh<double, const double*> >(
            pcm, etcs->fadda[na][ns], rn);

        // Convert to internal units.
        const double et = r * MeV;
        m_etransf.push_back(et);
        m_natom.push_back(na);
        m_nshell.push_back(ns);
        if (m_print_listing) Iprint2n(mcout, nt, et);
        // Sample the position of the collision.
        const double arange = SRANLUX() * range;
        point pt = curpt + dir * arange;
        point ptloc = pt;
        m_prevpos.tid.up_absref(&ptloc);
        if (m_loss_only) continue;
        if (m_print_listing) mcout << "generating new cluster\n";
        if (m_store_clusters) {
          m_clusterBank.emplace_back(
              HeedCluster(et, 0, pt, ptloc, m_prevpos.tid, na, ns));
        }
        // Generate a virtual photon.
        const double Ep0 = m_mass * c_squared + m_curr_ekin;
        const double Ep1 = Ep0 - m_etransf.back();
        const double Mp = m_mass * c_squared;
        const double Mt = electron_def.mass * c_squared;
        double theta_p, theta_t;
        theta_two_part(Ep0, Ep1, Mp, Mt, theta_p, theta_t);
        vec vel;
        vel.random_conic_vec(fabs(theta_t));
        vel.down(&tempbas);  // direction is OK
        vel *= c_light;
        const double t = m_prevpos.time + arange / m_prevpos.speed;
        if (m_print_listing) mcout << "generating new virtual photon\n";
        HeedPhoton* hp = new HeedPhoton(m_currpos.tid.eid[0], pt, vel, t,
                                        m_particle_number, et, m_fieldMap);
        hp->m_photon_absorbed = true;
        hp->m_delta_generated = false;
        hp->m_na_absorbing = na;
        hp->m_ns_absorbing = ns;
        secondaries.push_back(hp);
      }
    }
  }
  if (m_print_listing) {
    const double sum = std::accumulate(m_etransf.begin(), m_etransf.end(), 0.);
    Iprint2n(mcout, m_etransf.size(), sum);
    mcout << "Exiting HeedParticle::physics\n";
  }
}

void HeedParticle::print(std::ostream& file, int l) const {
  if (l < 0) return;
  Ifile << "HeedParticle (l=" << l << "): particle_number=" << m_particle_number
        << " type=";
  print_notation(file);
  file << std::endl;
  if (l == 1) return;
  mparticle::print(file, l - 1);
  const double sum = std::accumulate(m_etransf.begin(), m_etransf.end(), 0.);
  Iprintn(mcout, sum);
  Iprintn(mcout, m_etransf.size());
  if (l >= 5) {
    Ifile << "   nt  natom nshell transferred energy\n";
    const long qt = m_etransf.size();
    for (long nt = 0; nt < qt; nt++) {
      Ifile << std::setw(3) << nt << ' ' << std::setw(3) << m_natom[nt] << ' '
            << std::setw(3) << m_nshell[nt] << ' ' 
            << std::setw(12) << m_etransf[nt] << '\n';
    }
  }
}
}
