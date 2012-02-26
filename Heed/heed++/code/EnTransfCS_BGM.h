#ifndef ENTRANFCS_BGM_H
#define ENTRANFCS_BGM_H

#include "heed++/code/BGMesh.h"
#include "heed++/code/EnTransfCS.h"

// Energy Transfer Cross Section
class EnTransfCS_BGM: public RegPassivePtr  {
public:
  // All data from EnTransfCS that do not depend on speed.
  double particle_mass;     // MeV
  // Particle charge in terms of electron charges.
  // It is squared, therefore the sign does not matter.  
  long particle_charge;
  // Sign that the primary particle is an electron
  int s_primary_electron;  

  PassivePtr< HeedMatterDef > hmd;
  PassivePtr< BGMesh > mesh;
  DynLinArr< EnTransfCS > etcs_bgm;

  EnTransfCS_BGM(void);
  EnTransfCS_BGM(double fparticle_mass, 
                 PassivePtr< BGMesh > fmesh,
                 int fs_primary_electron, 
                 HeedMatterDef* fhmd, long fparticle_charge=1);
  macro_copy_total(EnTransfCS_BGM);

  virtual void print(std::ostream& file, int l) const;
};


class EnTransfCS_BGM_Type {
public:
  PassivePtr<EnTransfCS_BGM> etcs_bgm;
  EnTransfCS_BGM_Type(void): etcs_bgm() {;}
  EnTransfCS_BGM_Type(EnTransfCS_BGM* md): etcs_bgm(md) {;}
};
std::ostream& operator << (std::ostream& file, const EnTransfCS_BGM_Type& f);

#endif
