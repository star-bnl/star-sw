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
  if (amt == NULL) return 0.;
  MatterDef* amd = amt->matdef.get();
  const double beta = lorbetta(curr_gamma_1);
  double Eloss = Bethe_Bloch_energy_loss(amd->Z_mean() / amd->A_mean(),
                                         amd->I_eff(), beta, pardef->charge/eplus);
  Eloss *= amd->density();
  return Eloss;
}

void eiparticle::physics_after_new_speed(void)
{
  mfunname("void eiparticle::physics_after_new_speed(void)");
  double Eloss=Bethe_Bloch_en_loss();
  Eloss *= currpos.prange;
  total_Eloss += Eloss;
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
void eiparticle::print(std::ostream& file, int l) const 
{
  if(l >=0 )
  {  
    Ifile<<"eiparticle: s_add_loss="<<s_add_loss
	 <<" total_Eloss/keV="<<total_Eloss/keV<<'\n';
    eparticle::print(file,l);
  }
}
