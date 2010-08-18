#include <iomanip>
#include "heed++/code/ParticleBank.h"
#include "heed++/code/HeedParticle.h"
#include "heed++/code/HeedCluster.h"
#include "heed++/code/HeedDeltaElectron.h"
#include "heed++/code/HeedPhoton.h"
#include "heed++/code/EnTransfCS.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/random/ranluxint.h"
//#include "wcpplib/random/chisran.h"
#include "wcpplib/random/pois.h"
#include "wcpplib/math/kinem.h"
#include "wcpplib/math/tline.h"
/*
2003-2008, I. Smirnov
*/


HeedParticle::HeedParticle
(manip_absvol* primvol, const point& pt, 
 const vec& vel, vfloat time, particle_def* fpardef, 
 int fs_loss_only, int fs_print_listing):
  eparticle(primvol, pt, vel, time, fpardef),
  transferred_energy_in_step(0.0), // tnpi_in_step(0),
  qtransfer(0),
  s_loss_only(fs_loss_only),
  s_print_listing(fs_print_listing)
{
  mfunname("HeedParticle::HeedParticle(...)");
  particle_number = last_particle_number;
  last_particle_number++;
  //set_count_references();
  transferred_energy.allocate_block(100);
  natom.allocate_block(100);
  nshell.allocate_block(100);
}

void HeedParticle::physics(void)
{
  mfunname("void HeedParticle::physics(void)");
  if(s_print_listing == 1)
  {
    mcout<<"HeedParticle::physics is started\n";
    Iprintn(mcout, currpos.prange);
  }
  transferred_energy_in_step = 0.0;
  //tnpi_in_step = 0;
  qtransfer = 0;
  transferred_energy.allocate_block(100);
  natom.allocate_block(100);
  nshell.allocate_block(100);
  if(currpos.prange <= 0.0) return;
  const absvol* av = currpos.G_lavol(); // get least address of volume
  const EnTransfCSType* etcst = dynamic_cast< const EnTransfCSType*  >(av);
  if(etcst == NULL) 
  {
    //mcout<<"HeedParticle::physics: "
    //<<"dynamic_cast is not successful, return 0\n";
    //av->chname(name);
    //mcout<<"name="<<name<<'\n';
    return;
  }
  else
  {
    //mcout<<"HeedParticle::physics: proceeding well\n";
    // All these objects are expected to exist till the end of this function,
    // if they exist now:
    EnTransfCS* aetcs = etcst->etcs.getver();
    HeedMatterDef* ahmd = aetcs->hmd.getver();
    MatterDef* amatter = ahmd->matter.getver();
    EnergyMesh* a_energy_mesh = ahmd->energy_mesh.getver();
    const double* aetemp = ahmd->energy_mesh->get_ae();
    PointCoorMesh< double, const double* > pcm_e
      ( a_energy_mesh->get_q() + 1 , &(aetemp) );
    //double emin = hmd->energy_mesh->get_emin();
    //double emax = hmd->energy_mesh->get_emax();

    //transferred_energy.put_qel(10, 0.0);
    //natom.put_qel(100, 0);
    //nshell.put_qel(100, 0);
    long qa = amatter->qatom();
    //long qa = etcst->etcs->hmd->matter->qatom();
    if(s_print_listing == 1)
    {
      Iprintn(mcout, qa);
    }
    long na;
    long qe = a_energy_mesh->get_q();
    //long qe = ahmd->energy_mesh->get_q();
    //long qe = etcst->etcs->hmd->energy_mesh->get_q();
    basis tempbas(currpos.dir, "tempbas");
    for(na=0; na<qa; na++)
    {
      if(s_print_listing == 1)
      {
	Iprintn(mcout, na);
      }
      long qs = ahmd->apacs[na]->get_qshell();
      //long qs = etcst->etcs->hmd->apacs[na]->get_qshell();
      long ns;
      for(ns=0; ns<qs; ns++)
      {
	if(s_print_listing == 1)
	{
	  Iprintn(mcout, ns);
	}
	long qt=0;
 
#ifdef SINGLE_TRANSFER 
	if(aetcs == aetcs_single_transf &&
	   na == na_single_transf &&
	   ns == ns_single_transf)
	{
	  qt = 1;
	}
#else
	if(aetcs->quan[na][ns] > 0.0)
	  //if(etcst->etcs->quan[na][ns] > 0.0)
	{
	  int ierror;
	  //Iprintn(mcout, etcst->etcs->quan[na][ns]);
	  qt = pois( aetcs->quan[na][ns] * currpos.prange/cm, ierror);
	  //qt = pois( etcst->etcs->quan[na][ns] * currpos.prange/cm, ierror);
	  //Iprintn(mcout, qt);
	  check_econd11a(ierror , == 1 ,
			 " aetcs->quan[na][ns]="<<aetcs->quan[na][ns]
			 <<" currpos.prange/cm="<<currpos.prange/cm<<'\n' ,
			 mcerr);
	}
#endif
	if(s_print_listing == 1)
	{
	  Iprintn(mcout, qt);
	}

	if(qt > 0)
	{
	  point curpt = prevpos.pt;
	  //Iprint(mcout, curpt);
	  vec dir = unit_vec(currpos.pt - prevpos.pt);   
	  //Iprint(mcout, dir);
	  // this approximation ignores curvature
	  double range = length(currpos.pt - prevpos.pt);
	  //Iprintn(mcout, range);
	  if(s_print_listing == 1)
	  {
	    Iprint(mcout, curpt);
	    Iprint(mcout, dir);
	    Iprintn(mcout, range);
	  }
	  long nt;
	  for(nt=0; nt<qt; nt++)
	  {
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
	    transferred_energy.append( ener_single_transf );
#else
	    double rn = SRANLUX();
	    if(s_print_listing == 1)
	    {
	      Iprintn(mcout, rn);
	      Iprintn(mcout, aetcs);
	      Iprintn(mcout, aetcs->fadda[na][ns][1]);
	    }
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
	    double r = t_hisran_step_ar< double, DynLinArr < double >,
	      PointCoorMesh< double, const double* > >
	      (pcm_e, aetcs->fadda[na][ns], rn);
	    transferred_energy.append( r * MeV );  // and passing to 
	                                           // internal units
#endif
	    if(s_print_listing == 1)
	    {
	      Iprint2n(mcout, nt, transferred_energy[qtransfer]);
	    }
	    transferred_energy_in_step += transferred_energy[qtransfer];
	    natom.append( na ); 
	    nshell.append( ns );
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
	    if(s_loss_only == 0)
	    {
	      if(s_print_listing == 1)
	      {
		mcout<<"generating new cluster\n";
	      }
	      cluster_bank.append
		( HeedCluster( transferred_energy[qtransfer-1],
			       0,
			       pt,
			       ptloc,
			       prevpos.tid,	
			       na,
			       ns));
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
	      double Ep0 = mass * 
		mparticle::speed_of_light * mparticle::speed_of_light
		+ curr_kin_energy;
	      double Ep1 = Ep0 - transferred_energy[qtransfer-1];
	      double Mp = mass;
	      double Mt = electron_def.mass;
	      double theta_p, theta_t;
	      //mcout<<"Ep0/MeV="<<Ep0/MeV<<" Ep1/MeV="<<Ep1/MeV
	      //     <<" (Ep0 - Ep1)/MeV="<<(Ep0 - Ep1)/MeV
	      //     <<" Mp*c_squared/MeV="<<Mp*c_squared/MeV
	      //     <<" Mt*c_squared/MeV="<<Mt*c_squared/MeV<<endl;
	      theta_two_part(Ep0, Ep1, Mp, Mt, 
			     mparticle::speed_of_light,
			     theta_p, theta_t);
	      //mcout<<"theta_p/M_PI * 180.0="<<theta_p/M_PI * 180.0
	      //     <<" theta_t/M_PI * 180.0="<<theta_t/M_PI * 180.0<<endl;
	      vel.random_conic_vec(fabs(theta_t));  
	      vel.down(&tempbas);   // direction is OK
	      vel *= mparticle::speed_of_light;
	      if(s_print_listing == 1)
	      {
		mcout<<"generating new virtual photon\n";
	      }
	      HeedPhoton hp(
			    currpos.tid.eid[0].amvol.getver(), 
			    //currpos.tid.eid[0].amvol.get(), 
			    pt,
			    vel,
			    currpos.time,
			    particle_number,
			    //PassivePtr< gparticle > (this),
			    transferred_energy[qtransfer-1],
			    0);
	      hp.s_photon_absorbed = 1;
	      hp.s_delta_generated = 0;
	      hp.na_absorbing = na;
	      hp.ns_absorbing = ns;
	      ActivePtr< gparticle > ac;
	      ac.put(&hp);
	      
	      particle_bank.insert_after( particle_bank.get_last_node(), ac);
	    }
	  }
	}
      }
    }
  } 
  if(s_print_listing == 1)
  {
    Iprint2n(mcout, qtransfer, transferred_energy_in_step);
    mcout<<"Exiting HeedParticle::physics\n";
  }
}

void HeedParticle::print(ostream& file, int l) const 
{
  if(l >=0 )
  {  
    Ifile<<"HeedParticle (l="<<l<<"): particle_number="<<particle_number
	 <<" type=";
    print_notation(file);
    file<<endl;
    if(l == 1) return;
    //file<<'\n';
    mparticle::print(file,l-1);
    Iprintn(mcout, transferred_energy_in_step);
    Iprintn(mcout, qtransfer);
    if(l >=5 )
    {
      long nt;
      Ifile<<"   nt  natom nshell transferred_energy\n";   
      
      for(nt=0; nt<qtransfer; nt++)
      {
	Ifile<<setw(3)<<nt<<' '<<setw(3)<<natom[nt]<<' '<<setw(3)<<nshell[nt]<<' '
	     <<setw(12)<<transferred_energy[nt]<<'\n';
      } 
    }
  }

}
  
