#include <iomanip>
#include <iostream>
#include "heed++/code/ParticleBank.h"
#include "heed++/code/HeedParticle_BGM.h"
#include "heed++/code/HeedCluster.h"
#include "heed++/code/HeedDeltaElectron.h"
#include "heed++/code/HeedPhoton.h"
#include "heed++/code/EnTransfCS_BGM.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/random/ranluxint.h"
#include "wcpplib/random/pois.h"
#include "wcpplib/math/kinem.h"
#include "wcpplib/math/tline.h"
/*
2003-2008, I. Smirnov
*/

namespace Heed {

long HeedParticle_BGM::last_particle_number = 0;

HeedParticle_BGM::HeedParticle_BGM(manip_absvol* primvol, const point& pt,
                                   const vec& vel, vfloat time,
                                   particle_def* fpardef, int fs_loss_only,
                                   int fs_print_listing)
    : eparticle(primvol, pt, vel, time, fpardef),
      transferred_energy_in_step(0.0),  // tnpi_in_step(0),
      qtransfer(0),
      s_loss_only(fs_loss_only),
      s_print_listing(fs_print_listing),
      particle_number(last_particle_number++) {
  mfunname("HeedParticle_BGM::HeedParticle_BGM(...)");
  //set_count_references();
  transferred_energy.allocate_block(100);
  natom.allocate_block(100);
  nshell.allocate_block(100);
}

void HeedParticle_BGM::physics(void) {
  mfunname("void HeedParticle_BGM::physics(void)");
  if (s_print_listing == 1) {
    mcout << "HeedParticle_BGM::physics is started\n";
    Iprintn(mcout, currpos.prange);
  }
  transferred_energy_in_step = 0.0;
  //tnpi_in_step = 0;
  qtransfer = 0;
  transferred_energy.allocate_block(100);
  natom.allocate_block(100);
  nshell.allocate_block(100);
  if (currpos.prange <= 0.0) return;
  const absvol* av = currpos.G_lavol();  // get least address of volume
  const EnTransfCS_BGM_Type* etcs_bgm_t =
      dynamic_cast<const EnTransfCS_BGM_Type*>(av);
  if (etcs_bgm_t == NULL) {
    //mcout<<"HeedParticle_BGM::physics: "
    //<<"dynamic_cast is not successful, return 0\n";
    //av->chname(name);
    //mcout<<"name="<<name<<'\n';
    return;
  } else {
    //mcout<<"HeedParticle_BGM::physics: proceeding well\n";
    // All these objects are expected to exist till the end of this function,
    // if they exist now:
    EnTransfCS_BGM* aetcs_bgm = etcs_bgm_t->etcs_bgm.getver();
    HeedMatterDef* ahmd = aetcs_bgm->hmd.getver();
    MatterDef* amatter = ahmd->matter.getver();
    EnergyMesh* a_energy_mesh = ahmd->energy_mesh.getver();
    const double* aetemp = ahmd->energy_mesh->get_ae();
    PointCoorMesh<double, const double*> pcm_e(a_energy_mesh->get_q() + 1,
                                               &(aetemp));
    //double emin = hmd->energy_mesh->get_emin();
    //double emax = hmd->energy_mesh->get_emax();

    //transferred_energy.put_qel(10, 0.0);
    //natom.put_qel(100, 0);
    //nshell.put_qel(100, 0);
    long qa = amatter->qatom();
    //long qa = etcst->etcs->hmd->matter->qatom();
    if (s_print_listing == 1) {
      Iprintn(mcout, qa);
    }
    long na;
    long qe = a_energy_mesh->get_q();
    //long qe = ahmd->energy_mesh->get_q();
    //long qe = etcst->etcs->hmd->energy_mesh->get_q();
    basis tempbas(currpos.dir, "tempbas");
    for (na = 0; na < qa; na++) {
      if (s_print_listing == 1) {
        Iprintn(mcout, na);
      }
      long qs = ahmd->apacs[na]->get_qshell();
      //long qs = etcst->etcs->hmd->apacs[na]->get_qshell();
      long ns;
      for (ns = 0; ns < qs; ns++) {
        if (s_print_listing == 1) {
          Iprintn(mcout, ns);
        }
        long qt = 0;

#ifdef SINGLE_TRANSFER
        if (aetcs == aetcs_single_transf && na == na_single_transf &&
            ns == ns_single_transf) {
          qt = 1;
        }
#else
        double bg = sqrt(curr_gamma_1 * (curr_gamma_1 + 2.0));

        //streamsize old_prec = mcout.precision(15);
        //Iprint2n(mcout, curr_gamma_1, bg);
        //mcout.precision(old_prec);

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
          funnw.ehdr(mcerr);
          spexit(mcerr);
        }

        //check_econd11a(s_ret , != 1 ,
        //	       "bg="<<bg<<" n1="<<n1<<" n2="<<n2<<" b1="<<b1<<" b2="<<b2<<"
        //aetcs_bgm->mesh="<<aetcs_bgm->mesh<<'\n',
        //	       mcerr);
        double y1 = aetcs_bgm->etcs_bgm[n1].quan[na][ns];
        double y2 = aetcs_bgm->etcs_bgm[n2].quan[na][ns];

        double mean_pois = y1 + (bg - b1) * (y2 - y1) / (b2 - b1);

        if (mean_pois > 0.0)
            //if(etcst->etcs->quan[na][ns] > 0.0)
            {
          int ierror;
          //Iprintn(mcout, etcst->etcs->quan[na][ns]);
          qt = pois(mean_pois * currpos.prange / cm, ierror);
          //qt = pois( etcst->etcs->quan[na][ns] * currpos.prange/cm, ierror);
          //Iprintn(mcout, qt);
          check_econd11a(ierror, == 1,
                         " mean_pois=" << mean_pois << " currpos.prange/cm="
                                       << currpos.prange / cm << '\n',
                         mcerr);
        }
#endif
        if (s_print_listing == 1) {
          Iprintn(mcout, qt);
        }

        if (qt > 0) {
          point curpt = prevpos.pt;
          //Iprint(mcout, curpt);
          vec dir = unit_vec(currpos.pt - prevpos.pt);
          //Iprint(mcout, dir);
          // this approximation ignores curvature
          double range = length(currpos.pt - prevpos.pt);
          //Iprintn(mcout, range);
          if (s_print_listing == 1) {
            Iprint(mcout, curpt);
            Iprint(mcout, dir);
            Iprintn(mcout, range);
          }
          long nt;
          for (nt = 0; nt < qt; nt++) {
//if(s_print_listing == 1)
//{
//  Iprintn(mcout, nt);
//}
//if(qtransfer == transferred_energy.get_qel())
//{
//  transferred_energy.put_qel(2 * qtransfer);
//  natom.put_qel(2 * qtransfer);
//  nshell.put_qel(2 * qtransfer);
//}
#ifdef SINGLE_TRANSFER
            transferred_energy.append(ener_single_transf);
#else
            double rn = SRANLUX();
            /*
       	    double r = chisran(rn, aetcs->fadda[na][ns]);
       	    //double r = chisran(rn, etcst->etcs->fadda[na][ns]);
       	    long nr = left_round(r);
       	    check_econd21( nr , < 0 || , > qe , mcout);
       	    double e1 = a_energy_mesh->get_e(nr);
       	    double e2 = a_energy_mesh->get_e(nr+1);
       	    //double e1 = etcst->etcs->hmd->energy_mesh->get_e(nr);
       	    //double e2 = etcst->etcs->hmd->energy_mesh->get_e(nr+1);
       	    double dr = r - nr;
       	    transferred_energy.append( (e1 + (e2 - e1) * dr) * MeV );
       	    //transferred_energy[qtransfer] = (e1 + (e2 - e1) * dr) * MeV;
       	    if(s_print_listing == 1)
       	    {
       	      Iprint2n(mcout, r, nr);
       	      Iprint2n(mcout, e1, e2);
       	      Iprintn(mcout, dr);
       	    }
       	    */

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
              //Iprintn(mcout, aetcs->fadda[na][ns][1]);
            }
            transferred_energy.append(r * MeV);  // and passing to
                                                 // internal units
#endif
            if (s_print_listing == 1) {
              Iprint2n(mcout, nt, transferred_energy[qtransfer]);
            }
            transferred_energy_in_step += transferred_energy[qtransfer];
            natom.append(na);
            nshell.append(ns);
//natom[qtransfer] = na;
//nshell[qtransfer] = ns;
#ifdef SINGLE_TRANSFER
            double arange = 0.5 * range;
#else
            double arange = SRANLUX() * range;
#endif
            point pt = curpt + dir * arange;
            //Iprint(mcout, pt);
            point ptloc = pt;
            prevpos.tid.up_absref(&ptloc);
            qtransfer++;
            if (s_loss_only == 0) {
              if (s_print_listing == 1) {
                mcout << "generating new cluster\n";
              }
              cluster_bank.append(
                  HeedCluster(transferred_energy[qtransfer - 1], 0, pt, ptloc,
                              prevpos.tid, na, ns));
              /*
       		cluster_bank.insert_after
       		( cluster_bank.get_last_node(),
       		HeedCluster( transferred_energy[qtransfer-1],
       		0,
       		pt,
       		ptloc,
       		prevpos.tid,	
       		na,
       		ns) );
       	      */
              vec vel;
              double Ep0 = mass * c_squared + curr_kin_energy;
              double Ep1 = Ep0 - transferred_energy[qtransfer - 1];
              double Mp = mass;
              double Mt = electron_def.mass;
              double theta_p, theta_t;
              //mcout<<"Ep0/MeV="<<Ep0/MeV<<" Ep1/MeV="<<Ep1/MeV
              //     <<" (Ep0 - Ep1)/MeV="<<(Ep0 - Ep1)/MeV
              //     <<" Mp*c_squared/MeV="<<Mp*c_squared/MeV
              //     <<" Mt*c_squared/MeV="<<Mt*c_squared/MeV<<endl;
              theta_two_part(Ep0, Ep1, Mp, Mt, theta_p, theta_t);
              //mcout<<"theta_p/M_PI * 180.0="<<theta_p/M_PI * 180.0
              //     <<" theta_t/M_PI * 180.0="<<theta_t/M_PI * 180.0<<endl;
              vel.random_conic_vec(fabs(theta_t));
              vel.down(&tempbas);  // direction is OK
              vel *= c_light;
              if (s_print_listing == 1) {
                mcout << "generating new virtual photon\n";
              }
              HeedPhoton hp(currpos.tid.eid[0].amvol.getver(),
                            //currpos.tid.eid[0].amvol.get(),
                            pt, vel, currpos.time, particle_number,
                            //PassivePtr< gparticle > (this),
                            transferred_energy[qtransfer - 1], 0);
              hp.s_photon_absorbed = 1;
              hp.s_delta_generated = 0;
              hp.na_absorbing = na;
              hp.ns_absorbing = ns;
              ActivePtr<gparticle> ac;
              ac.put(&hp);

              particle_bank.insert_after(particle_bank.get_last_node(), ac);
            }
          }
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
