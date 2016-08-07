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

namespace Heed {

double eiparticle::Bethe_Bloch_en_loss() {
  mfunname("double eiparticle::Bethe_Bloch_energy_loss()");
  // Get least address of volume
  const absvol* av = currpos.G_lavol();
  const MatterType* amt = dynamic_cast<const MatterType*>(av);
  if (!amt) return 0.;
  MatterDef* amd = amt->matdef.get();
  const double beta = lorbeta(curr_gamma_1);
  const double loss =
      Bethe_Bloch_energy_loss(amd->Z_mean() / amd->A_mean(), amd->I_eff(), beta,
                              pardef->charge / eplus);
  return loss * amd->density();
}

void eiparticle::physics_after_new_speed() {
  mfunname("void eiparticle::physics_after_new_speed(void)");
  const double loss = Bethe_Bloch_en_loss() * currpos.prange;
  total_loss += loss;
  if (s_add_loss == 0) {
    curr_kin_energy -= loss;
    if (curr_kin_energy <= 0.0) {
      curr_kin_energy = 0.0;
      curr_gamma_1 = 0.0;
      currpos.speed = 0.0;
      s_life = 0;
    } else {
      const double resten = mass * c_squared;
      curr_gamma_1 = curr_kin_energy / resten;
      currpos.speed = c_light * lorbeta(curr_gamma_1);
    }
  } else {
    curr_kin_energy += loss;
    const double resten = mass * c_squared;
    curr_gamma_1 = curr_kin_energy / resten;
    currpos.speed = c_light * lorbeta(curr_gamma_1);
  }
}

void eiparticle::print(std::ostream& file, int l) const {
  if (l < 0) return;
  Ifile << "eiparticle: s_add_loss=" << s_add_loss
        << " total_loss/keV=" << total_loss / keV << '\n';
  eparticle::print(file, l);
}

}
