#ifndef StKFParticleInterface_H
#define StKFParticleInterface_H
#include <vector>

#include "KFParticle/KFParticle.h"
#include "TObject.h"


class KFParticleTopoReconstructor;
class KFTopoPerformance;
class KFVertex;
class StPicoDst;
class StMuDst;
class StDcaGeometry;
class KFPTrack;

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
  void SetHftHits(std::vector<int>& nHftHits) { fNHftHits = nHftHits; }
  
  void SetField(float field);
  void SetBeamLine(KFParticle& p);
  
  void CleanPV();
  void AddPV(const KFVertex &pv, const std::vector<int> &tracks);
  void AddPV(const KFVertex &pv);
  void AddParticle(const KFParticle& p);
  void AddCandidate(const KFParticle& candidate, int iPV = -1);
  
  const KFParticleTopoReconstructor* GetTopoReconstructor() { return fKFParticleTopoReconstructor; }
  
  void SetPrimaryProbCut(float prob);
  
  bool ProcessEvent(StPicoDst* picoDst, std::vector<int>& goodTracks);
//   bool ProcessEvent(StMuDst* muDst);
  bool OpenCharmTrigger();
  void OpenCharmTriggerCompression(int nTracksTriggered, int nTracksInEvent, bool triggerDMesons);
  
  
  //PID cuts 
  void SetStrictTofPidMode() { fStrictTofPID = true;  }
  void SetSoftTofPidMode()   { fStrictTofPID = false; }
  void SetSoftKaonPIDMode()  { fCleanKaonsWitTof = false; }
  void UseCorrecteddEdX()    { fdEdXMode = 2; }
  void SetTriggerMode()      { fTriggerMode = true; }
  //KF Particle Finder cuts
  void SetChiPrimaryCut(float cut)  { fChiPrimaryCut = cut; }
  void SetChiPrimaryMaxCut(float cut)  { fChiPrimaryMaxCut = cut; }
  
 private:
  
  double InversedChi2Prob(double p, int ndf) const;
  bool IsGoodPV(const KFVertex& pv);
  bool GetTrack(const StDcaGeometry& dcaG, KFPTrack& track, int q, int index);
  std::vector<int> GetTofPID(double m2, double p, int q);
  std::vector<int> GetPID(double m2, double p, int q, float dEdXPull[7], bool isTofm2);
  void AddTrackToParticleList(const KFPTrack& track, int nHftHitsInTrack, int index, const std::vector<int>& totalPDG, KFVertex& pv, std::vector<int>& primaryTrackList,
                              std::vector<int>& nHftHits, std::vector<int>& particlesPdg, std::vector<KFParticle>& particles, int& nPartSaved);
  
  KFParticleTopoReconstructor* fKFParticleTopoReconstructor;
  std::vector<KFParticle> fParticles;
  std::vector<int> fParticlesPdg;
  std::vector<int> fNHftHits;
  
  //PID cuts
  bool fStrictTofPID;
  bool fCleanKaonsWitTof;
  int fdEdXMode;
  //trigger cuts
  bool fTriggerMode;
  //KF Particle Finder cuts
  float fChiPrimaryCut;
  float fChiPrimaryMaxCut;
  
  ClassDef(StKFParticleInterface,1)
};

#endif //#ifndef StKFParticleInterface_H
