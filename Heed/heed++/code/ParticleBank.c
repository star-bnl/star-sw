#include "heed++/code/ParticleBank.h"

namespace Heed {

void treat_particle_bank(int s_erase) {
  mfunname("void treat_particle_bank(int s_erase)");
  AbsListNode<ActivePtr<gparticle> >* aln;
  AbsListNode<ActivePtr<gparticle> >* aln1;

  aln = particle_bank.get_first_node();
  while (aln != NULL) {
    aln->el->fly();
    aln1 = aln->get_next_node();
    if (s_erase == 1) particle_bank.erase(aln);
    aln = aln1;
  }
}

}
