#ifndef StKFParticleInterface_H
#define StKFParticleInterface_H

#include "KFParticle.h"

#include "TObject.h"

#include <vector>

class KFParticleTopoReconstructor;
class KFTopoPerformance;
class KFVertex;

class StKFParticleInterface: public TObject
{
 public:
   
  StKFParticleInterface();
  ~StKFParticleInterface();
  
  void InitParticles();
  void ReconstructParticles();
  void ReconstructTopology();

  void SetParticles(std::vector<KFParticle>& particles)
  {
    fParticles = particles;
    if(fParticlesPdg.size() != fParticles.size())
    {
      fParticlesPdg.clear();
      fParticlesPdg.resize(fParticles.size(), -1);
    }
  }
  void SetParticlesPdg(std::vector<int>& pdg) { fParticlesPdg = pdg;}
  
  void SetField(float field);
  void SetBeamLine(KFParticle& p);
  
  void AddPV(const KFVertex &pv, const std::vector<int> &tracks);
  void AddPV(const KFVertex &pv);
  void AddParticle(const KFParticle& p);
  
  const KFParticleTopoReconstructor* GetTopoReconstructor() const { return fKFParticleTopoReconstructor; }
  
 private:
   
  KFParticleTopoReconstructor* fKFParticleTopoReconstructor;
  std::vector<KFParticle> fParticles;
  std::vector<int> fParticlesPdg;
  
  ClassDef(StKFParticleInterface,1)
};

#endif //#ifndef StKFParticleInterface_H
