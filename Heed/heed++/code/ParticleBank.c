
#include "heed++/code/ParticleBank.h"

void treat_particle_bank(int s_erase)
{
  mfunname("void treat_particle_bank(int s_erase)");
  AbsListNode< ActivePtr< gparticle > >* aln;
  AbsListNode< ActivePtr< gparticle > >* aln1;

  aln = particle_bank.get_first_node();
  while(aln != NULL)
  {
    /*
    // For debug:
    mcout<<"treat_particle_bank: flying the particle:\n";
    aln->el->print(mcout, 2);
    HeedDeltaElectron* hde = dynamic_cast< HeedDeltaElectron* >(aln->el.get());
    if(hde != NULL)
    {
      //if(hde->particle_number == 36634)
      if(hde->particle_number == 37908)
      {
	mcout<<"Initialized full print\n";
	hde->s_print_listing = 1;
      }
    }
    */
    //mcout<<"treat_particle_bank: now treat node \n";
    //aln->el.print(mcout, 2);
    aln->el->fly();
    aln1 = aln->get_next_node();
    //mcout<<"now erase \n";
    //aln->el.print(mcout, 2);
    //RegPassivePtr::s_allow_delete_with_references = 1;
    if(s_erase == 1)
      particle_bank.erase(aln);
    //RegPassivePtr::s_allow_delete_with_references = 0;
    aln = aln1;
  }
}
  
