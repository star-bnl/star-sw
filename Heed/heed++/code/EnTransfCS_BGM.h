#ifndef ENTRANFCS_BGM_H
#define ENTRANFCS_BGM_H

#include "heed++/code/BGMesh.h"
#include "heed++/code/EnTransfCS.h"


class EnTransfCS_BGM: public RegPassivePtr  // Energy Transfer Cross Section
{public:

  // All data from EnTransfCS that do not depend on speed.

  double particle_mass;     // MeV
  long particle_charge;     // in the charge of electron units.
  // it is squared, therefore the sign does not matter.
  int s_primary_electron;  // sign that the primary particle is the electron
  PassivePtr< HeedMatterDef > hmd;

  PassivePtr< BGMesh > mesh;

  DynLinArr< EnTransfCS > etcs_bgm;

  EnTransfCS_BGM(void);
  EnTransfCS_BGM(double fparticle_mass, 
		 PassivePtr< BGMesh > fmesh,
		 int fs_primary_electron, 
		 HeedMatterDef* fhmd, long fparticle_charge=1);
  macro_copy_total(EnTransfCS_BGM);

  virtual void print(ostream& file, int l) const ;
};


class EnTransfCS_BGM_Type
{public:
  PassivePtr<EnTransfCS_BGM> etcs_bgm;
  EnTransfCS_BGM_Type(void): etcs_bgm() {;}
  EnTransfCS_BGM_Type(EnTransfCS_BGM* md): etcs_bgm(md) {;}
};
ostream & operator << (ostream & file, const EnTransfCS_BGM_Type & f);

#endif
