#ifndef StKFParticleInterface_H
#define StKFParticleInterface_H
#include <vector>
#include <map>

#include "KFParticle.h"
#include "TObject.h"

class KFParticleTopoReconstructor;
class KFParticleFinder;
class KFTopoPerformance;
class KFVertex;
class StPicoDst;
class StPicoTrack;
class KFMCTrack;
class StMuDst;
class StDcaGeometry;
class KFPTrack;
class TH1F;
class TH2F;

class StKFParticleInterface: public TObject
{
 public:
   
  StKFParticleInterface();
  ~StKFParticleInterface();
  
  void InitParticles();
  void ReconstructParticles();
  void ReconstructTopology();
  
  const std::vector<KFParticle> &GetParticles() const;
  void RemoveParticle(const int iParticle);
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
  void AddDecayToReconstructionList(Int_t pdg);
  const KFParticleTopoReconstructor* GetTopoReconstructor() { return fKFParticleTopoReconstructor; }
  
  void SetPrimaryProbCut(float prob);
  
  bool ProcessEvent(StPicoDst* picoDst, std::vector<int>& goodTracks);
  bool ProcessEvent(StMuDst* muDst, std::vector<KFMCTrack>& mcTracks, std::vector<int>& mcIndices, bool processSignal);
  bool OpenCharmTrigger();
  void OpenCharmTriggerCompression(int nTracksTriggered, int nTracksInEvent, bool triggerDMesons);
  
  //Histograms
  void CollectTrackHistograms();
  void CollectPIDHistograms();
  
  //PID hypothesis, should be resized from outside
  void ResizeTrackPidVectors(const int nTracks);
  const float GetdEdXNSigmaPion(const int trackId)   const { return fTrackPidTpc[0][trackId]; }
  const float GetdEdXNSigmaKaon(const int trackId)   const { return fTrackPidTpc[1][trackId]; }
  const float GetdEdXNSigmaProton(const int trackId) const { return fTrackPidTpc[2][trackId]; }
  const float GetTofNSigmaPion(const int trackId)    const { return fTrackPidTof[0][trackId]; }
  const float GetTofNSigmaKaon(const int trackId)    const { return fTrackPidTof[1][trackId]; }
  const float GetTofNSigmaProton(const int trackId)  const { return fTrackPidTof[2][trackId]; }

  //PID cuts 
  void SetStrictTofPidMode() { fStrictTofPID = true;  }
  void SetSoftTofPidMode()   { fStrictTofPID = false; }
  void SetSoftKaonPIDMode()  { fCleanKaonsWitTof = false; }
  void UseCorrecteddEdX()    { fdEdXMode = 2; }
  void SetTriggerMode()      { fTriggerMode = true; }
  //KF Particle Finder cuts
  void SetChiPrimaryCut(float cut)  { fChiPrimaryCut = cut; }
  void SetChiPrimaryMaxCut(float cut)  { fChiPrimaryMaxCut = cut; }
  //Event cuts
  void CleanLowPVTrackEvents() { fCleanLowPVTrackEvents = true; }
  void UseHFTTracksOnly()      { fUseHFTTracksOnly = true; }
  
  KFParticleFinder* GetKFParticleFinder();
  //KF Particle Finder cuts
  void SetMaxDistanceBetweenParticlesCut(float cut);
  void SetLCut(float cut);
  void SetChiPrimaryCut2D(float cut);
  void SetChi2Cut2D(float cut);
  void SetLdLCut2D(float cut);
  void SetLdLCutXiOmega(float cut);
  void SetChi2TopoCutXiOmega(float cut);
  void SetChi2CutXiOmega(float cut);
  void SetChi2TopoCutResonances(float cut);
  void SetChi2CutResonances(float cut);
  void SetPtCutLMVM(float cut);
  void SetPCutLMVM(float cut);
  void SetPtCutJPsi(float cut);
  void SetPtCutCharm(float cut);
  void SetChiPrimaryCutCharm(float cut);
  void SetLdLCutCharmManybodyDecays(float cut);
  void SetChi2TopoCutCharmManybodyDecays(float cut);
  void SetChi2CutCharmManybodyDecays(float cut);
  void SetLdLCutCharm2D(float cut);
  void SetChi2TopoCutCharm2D(float cut);
  void SetChi2CutCharm2D(float cut);
  static StKFParticleInterface *instance() {return fgStKFParticleInterface;}
 private:
  
  double InversedChi2Prob(double p, int ndf) const;
  bool IsGoodPV(const KFVertex& pv);
  bool GetTrack(const StDcaGeometry& dcaG, KFPTrack& track, int q, int index);
  std::vector<int> GetTofPID(double m2, double p, int q, const int trackId);
  std::vector<int> GetPID(double m2, double p, int q, double dEdX, double dEdXPull[7], bool isTofm2, const int trackId);
  void AddTrackToParticleList(const KFPTrack& track, int nHftHitsInTrack, int index, const std::vector<int>& totalPDG, KFVertex& pv, std::vector<int>& primaryTrackList,
                              std::vector<int>& nHftHits, std::vector<int>& particlesPdg, std::vector<KFParticle>& particles, int& nPartSaved);
  void FillPIDHistograms(StPicoTrack *gTrack, const std::vector<int>& pdgVector, const bool isTofm2, float m2tof);
  
  KFParticleTopoReconstructor* fKFParticleTopoReconstructor;
  std::vector<KFParticle> fParticles;
  std::vector<int> fParticlesPdg;
  std::vector<int> fNHftHits;
  
  //histograms
  bool fCollectTrackHistograms;
  bool fCollectPIDHistograms;
  //0 - N HFT hits in track, 1 - PV error distribution
  TH1F* fTrackHistograms[2];
  // 0 - dEdX, 1 - dEdX positive tracks, 2 - dEdX negative tracks, 3 - dEdX tracks with ToF, 4 - ToF PID, 5 - PV errors vs N tracks, 6 - PV errors vs N PV tracks
  TH2F* fTrackHistograms2D[7];
  //PID histograms
  static const int NTrackHistoFolders = 18;
  TH2F* fHistodEdXTracks[NTrackHistoFolders];
  TH2F* fHistodEdXwithToFTracks[NTrackHistoFolders];
  TH2F* fHistoTofPIDTracks[NTrackHistoFolders];
  TH1F* fHistoMomentumTracks[NTrackHistoFolders];
  TH2F* fHistodEdXPull[NTrackHistoFolders];
  TH2F* fHistodEdXZ[NTrackHistoFolders];
  std::map<int, int> fTrackPdgToHistoIndex;
  
  //PID information with respect to the trackID
  std::vector<float> fTrackPidTof[3];
  std::vector<float> fTrackPidTpc[3];
  
  //PID cuts
  bool fStrictTofPID;
  bool fCleanKaonsWitTof;
  int fdEdXMode;
  //trigger cuts
  bool fTriggerMode;
  //KF Particle Finder cuts
  float fChiPrimaryCut;
  float fChiPrimaryMaxCut;
  static StKFParticleInterface* fgStKFParticleInterface;
  //Event cuts
  bool fCleanLowPVTrackEvents;
  bool fUseHFTTracksOnly;
  ClassDef(StKFParticleInterface,1)
};

#endif //#ifndef StKFParticleInterface_H
