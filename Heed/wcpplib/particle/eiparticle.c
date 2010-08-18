#include <iomanip>
#include "wcpplib/particle/eiparticle.h"
#include "wcpplib/ioniz/bethe_bloch.h"
#include "wcpplib/math/lorgamma.h"
#include "wcpplib/matter/MatterDef.h"
#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
/*
1998 - 2002,   I. Smirnov
*/

double eiparticle::Bethe_Bloch_en_loss(void)
{
  mfunname("double eiparticle::Bethe_Bloch_energy_loss(void)");
  const absvol* av = currpos.G_lavol(); // get least address of volume
  const MatterType* amt = dynamic_cast< const MatterType*  >(av);
  //PassivePtr< MatterDef >* amt = dynamic_cast< PassivePtr< MatterDef >*  >(av);
  //char name[1000];
  if(amt == NULL) 
  {
    //mcout<<"eiparticle::Bethe_Bloch_en_loss: dynamic_cast is not successful, return 0\n";
    //av->chname(name);
    //mcout<<"name="<<name<<'\n';
    return 0.0;
  }
  else
  {
    //mcout<<"eiparticle::Bethe_Bloch_en_loss: dynamic_cast is successful\n";
    //av->chname(name);
    //mcout<<"name="<<name<<'\n';
    MatterDef* amd = amt->matdef.get();
    double gamma_1 = curr_gamma_1;
    double betta = lorbetta(gamma_1);
    double Eloss = Bethe_Bloch_energy_loss(amd->Z_mean() / amd->A_mean(),
					   amd->I_eff(), 
					   betta, 
					   pardef->charge/eplus);
    Eloss *= amd->density();
    //mcout<<"Eloss/(keV / cm  )="<<Eloss/(keV / cm)<<'\n';;
    return Eloss;
  }
}
/*
void eiparticle::step(void)
{          // make step to nextpos and calculate new step to border
  mfunname("void eiparticle::step(void)");
  prevpos=currpos;
  currpos=nextpos;
  nstep++;
  if(currpos.prange==0)
  {
    n_zero_step++;
    check_econd12a(n_zero_step , > , max_q_zero_step, 
		  "too many zero steps, possible infinite loop\n"; 
		   print(mcout,10);, mcerr);
  }
  else
    n_zero_step=0;
  new_speed();
  ionization_loss();
  if(prevpos.tid != currpos.tid)
    //prevpos.namvol != currpos.namvol ||
    //prevpos.amvol[prevpos.namvol-1] != currpos.amvol[currpos.namvol-1])
    change_vol();  // possible correction ( reflection..)
  nextpos=calc_step_to_bord();
}
*/
//void eiparticle::ionization_loss(void)
//{
//  mfunname("void eiparticle::ionization_loss(void)");
void eiparticle::physics_after_new_speed(void)
{
  mfunname("void eiparticle::physics_after_new_speed(void)");
  double Eloss=Bethe_Bloch_en_loss();
  Eloss *= currpos.prange;
  total_Eloss += Eloss;
  //mcout<<"total_Eloss/keV="<<total_Eloss/keV<<'\n';
  if(s_add_loss == 0)
  {
    curr_kin_energy -= Eloss;
    if(curr_kin_energy <= 0.0)
    {
      curr_kin_energy = 0.0;
      curr_gamma_1 = 0.0;
      currpos.speed = 0.0;
      s_life = 0;
    }
    else
    {
      double resten=mass*pow(speed_of_light, 2);
      curr_gamma_1=curr_kin_energy / resten;
      currpos.speed=speed_of_light * lorbetta( curr_gamma_1 );
    }
  }
  else
  {
    curr_kin_energy += Eloss;
    double resten=mass*pow(speed_of_light, 2);
    curr_gamma_1=curr_kin_energy / resten;
    currpos.speed=speed_of_light * lorbetta( curr_gamma_1 );
  }
}
void eiparticle::print(ostream& file, int l) const 
{
  if(l >=0 )
  {  
    Ifile<<"eiparticle: s_add_loss="<<s_add_loss
	 <<" total_Eloss/keV="<<total_Eloss/keV<<'\n';
    eparticle::print(file,l);
  }
}
