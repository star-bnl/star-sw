#ifndef HEEDPARTICLE_H
#define HEEDPARTICLE_H


#include "wcpplib/particle/eparticle.h"
#include "wcpplib/safetl/AbsList.h"
#include "wcpplib/safetl/BlkArr.h"
/*
Definition of the particle which can be traced through the
geometry. Also the definition of cluster (energy transfer),
and particle bank.

2003, I. Smirnov
*/

//#define SINGLE_TRANSFER          // for debug
#ifdef SINGLE_TRANSFER 
#include "heed++/code/EnTransfCS.h"
extern EnTransfCS* aetcs_single_transf;
extern long na_single_transf;
extern long ns_single_transf;
extern double ener_single_transf;
#endif


extern long last_particle_number;  // for debug print
// Each particle is identified by particle_number.
// It is assigned by current last_particle_number which is then incremented

class HeedParticle: public eparticle
{public:
  int s_print_listing; 
  //PassivePtr< EnTransfCS > encs;
  long particle_number;
  HeedParticle(manip_absvol* primvol, const point& pt, 
	       const vec& vel, vfloat time, particle_def* fpardef, 
	       int fs_loss_only = 0,
	       int fs_print_listing = 0);  
  // if fs_loss_only == 1 - only transfer energy and 
  // no othet physics: no deposition of clusters, 
  // no generation of virtual photons.
  // Thus it is just a PAI without even clusters 

	    //EnTransfCS* encs);
  virtual void physics(void);
  HeedParticle(void): eparticle() { ; }
  virtual void print(ostream& file, int l) const ;
  macro_copy_total(HeedParticle);
  virtual ~HeedParticle() {;}
  double transferred_energy_in_step;  // internal units

  //long tnpi_in_step;  // total number of initial ionization
  // that is delta-electrons emitted in collision of
  // the incident particle with atoms. 
  // No, it cannot be done here. This class knows only
  // the number of transfers

  long qtransfer;
  int s_loss_only; 
  BlkArr< double > transferred_energy;  // internal units
  BlkArr< long > natom;
  BlkArr< long > nshell;
  //DynLinArr< double > transferred_energy;  // internal units
  //DynLinArr< long > natom;
  //DynLinArr< long > nshell;
  
};

#endif
