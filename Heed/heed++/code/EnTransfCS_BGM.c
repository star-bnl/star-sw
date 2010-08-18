#include "heed++/code/EnTransfCS_BGM.h"

EnTransfCS_BGM::EnTransfCS_BGM(double fparticle_mass, 
			       PassivePtr< BGMesh > fmesh,
			       int fs_primary_electron, 
			       HeedMatterDef* fhmd, long fparticle_charge):
  particle_mass(fparticle_mass),
  s_primary_electron(fs_primary_electron),
  particle_charge(fparticle_charge),
  hmd(fhmd),
  mesh(fmesh)
{
  mfunnamep("EnTransfCS_BGM::EnTransfCS_BGM(...)");

  long n;
  long q = mesh->q;
  etcs_bgm = DynLinArr< EnTransfCS >(q);
  for(n=0; n<q; n++)
  {
    double bg = mesh->x[n];
    double gamma_1 = sqrt(1.0 + (bg * bg)) - 1.0; // gamma - 1
    etcs_bgm[n] = EnTransfCS(fparticle_mass, gamma_1, 
			     fs_primary_electron, fhmd,  fparticle_charge);
  }
  
}


void EnTransfCS_BGM::print(ostream& file, int l) const 
{
  if(l <= 0)return;
  Ifile<<"EnTransfCS_BGM(l="<<l<<"):\n";
  indn.n+=2;
  Ifile<<"particle_mass="<<particle_mass
       <<" particle_charge="<<particle_charge
       <<endl;
  Ifile<<"s_primary_electron="<<s_primary_electron<<endl;
  Ifile<<"hmd:\n";
  hmd->print(file, 1);
  Ifile<<"mesh:\n";
  mesh->print(file, 1);

  Ifile<<"Array of Cross Section:\n";
  Ifile<<"Number of elements = "<<etcs_bgm.get_qel()<<'\n';

  if(l >= 2)
  {
    long q = mesh->q;
    long n;
    for(n=0; n<q; n++)
    {
      Ifile<<"n="<<setw(5)<<n<<" bg="<<setw(14)<<mesh->x[n]<<" quan="<<setw(14)<<etcs_bgm[n].quanC;
#ifndef EXCLUDE_MEAN
      file<<" mean="<<setw(14)<<etcs_bgm[n].meanC;
#endif
      file<<'\n';
    }
  }
  indn.n-=2;
}

ostream & operator << (ostream & file, const EnTransfCS_BGM_Type& f)
{
  mfunname("ostream & operator << (ostream & file, const EnTransfCS_BGM_Type& f)");
  if(f.etcs_bgm.get() == NULL)
    Ifile<<"EnTransfCS_BGM_Type: type is not initialized\n";
  else
  {
    Ifile<<"EnTransfCS_BGM_Type: =";
    f.etcs_bgm->print(file, 1);
  }
  return file;
}
