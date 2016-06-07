#ifndef StKFParticleInterface_H
#define StKFParticleInterface_H
#include <vector>

#include "KFParticle.h"
#include "TObject.h"


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
  
  const std::vector<KFParticle> &GetParticles() const;
  const std::vector<KFParticle>* GetSecondaryCandidates() const;                      // Get secondary particles with the mass constraint
  const std::vector<KFParticle>& GetSecondaryK0()         const;
  const std::vector<KFParticle>& GetSecondaryLambda()     const;
  const std::vector<KFParticle>& GetSecondaryAntiLambda() const;
  const std::vector<KFParticle>& GetSecondaryGamma()      const;
  const std::vector<KFParticle>& GetSecondaryPi0()        const;
  const std::vector< std::vector<KFParticle> >* GetPrimaryCandidates() const;         // Get primary particles with the mass constraint
  const std::vector< std::vector<KFParticle> >* GetPrimaryTopoCandidates() const;     // Get primary particles with the topologigal constraint
  const std::vector< std::vector<KFParticle> >* GetPrimaryTopoMassCandidates() const; // Get primary particles with the topologigal and mass constraint
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
  void AddCandidate(const KFParticle& candidate, int iPV = -1);
  
  const KFParticleTopoReconstructor* GetTopoReconstructor() const { return fKFParticleTopoReconstructor; }
  
 private:
   
  KFParticleTopoReconstructor* fKFParticleTopoReconstructor;
  std::vector<KFParticle> fParticles;
  std::vector<int> fParticlesPdg;
  
  ClassDef(StKFParticleInterface,1)
};

#endif //#ifndef StKFParticleInterface_H
