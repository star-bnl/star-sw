#include "wcpplib/particle/eparticle.h"
//#include "wcpplib/math/lorforce.h"
//#include "CLHEP/Units/PhysicalConstants.h"
#include "wcpplib/util/emul_new_stand.h"
/*
1998 - 2004,   I. Smirnov
*/

void field_map(const point& pt, vec& Efield, vec& Hfield, vfloat& mrange);
// defined anywhere outside
eparticle::eparticle(manip_absvol* primvol, const point& pt, 
		     const vec& vel, vfloat time, particle_def* fpardef):
  mparticle(), 
  particle_type(fpardef) 
  //pardef(fpardef) 
{
  //mcout<<"fpardef="<<fpardef<<'\n';
  //mcout<<"pardef="<<pardef<<'\n';
  //mcout<<"eparticle::eparticle: print_notation:";
  //print_notation(mcout);
  //mcout<<'\n';
  gparticle gp(primvol, pt, vel, time);
  statcast(mparticle& , *this)=mparticle(gp, fpardef->mass);
  //mcout<<"eparticle::eparticle: print_notation:";
  //print_notation(mcout);
  //mcout<<'\n';
  //static_cast<mparticle&>(*this)=mparticle(gp, fpardef->mass);
  ;
}

eparticle::eparticle(manip_absvol* primvol, const point& pt, 
		     const vec& vel, vfloat time, particle_def* fpardef,
		     double gamma_1):
  mparticle(primvol, pt, vel, time, fpardef->mass, gamma_1), 
  particle_type(fpardef) 
  //pardef(fpardef) 
{
  //mcout<<"fpardef="<<fpardef<<'\n';
  //mcout<<"pardef="<<pardef<<'\n';
  //mcout<<"eparticle::eparticle: print_notation:";
  //print_notation(mcout);
  //mcout<<'\n';
  //gparticle gp(primvol, pt, vel, time);
  //statcast(mparticle& , *this)=mparticle(gp, fpardef->mass, gamma_1);
  //mcout<<"eparticle::eparticle: print_notation:";
  //print_notation(mcout);
  //mcout<<'\n';
  //static_cast<mparticle&>(*this)=mparticle(gp, fpardef->mass);
  ;
}
//  mparticle(gparticle(primvol, pt, vel, time), fpardef->mass), 

int eparticle::force(const point& pt, vec& f,  vec& f_perp, vfloat& mrange)
{
  vec Efield; vec Hfield;
  field_map(pt, Efield, Hfield, mrange);
  //mcout<<"eparticle::force: print_notation:";
  //print_notation(mcout);
  //mcout<<'\n';
  f = pardef->charge * Efield;
  f_perp = pardef->charge * Hfield;
  //f_perp = pardef->charge * currpos.speed * (currpos.dir||Hfield);
  //f=lorforce(pardef->charge, currpos.dir*currpos.speed, 
  //	     Efield, Hfield);
  return 1;
}

void eparticle::print(ostream& file, int l) const 
{
  if(l >=0 )
  {  
    Ifile<<"eparticle: particle is ";
    print_notation(file);
    file<<'\n';
    mparticle::print(file,l);
  }
}
