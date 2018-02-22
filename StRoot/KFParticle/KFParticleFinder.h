//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________


#ifndef KFParticleFinder_h
#define KFParticleFinder_h

#include "KFParticle.h"
#include "KFParticleSIMD.h"

#include "KFPTrackVector.h"
#include "KFPEmcCluster.h"

#include <vector>
#include <map>

class KFParticleFinder
{
 public:

  KFParticleFinder();
  ~KFParticleFinder() {};
  
  void Init(int nPV);
  void SetNThreads(short int n) { fNThreads = n;}

  /// Find particles with 2-body decay channel from input tracks vRTracks with primary vertex PrimVtx:
  /// 1. K0s->pi+ pi-
  /// 2. Lambda->p pi-
  /// All particles are put into the Particles array. 3 cuts for each particle are required.
  /// First index in the cuts array sets a particle number (see table above), second index - a cut number:
  /// cut[0][0] - chi to a primary vertex of a track (sqare root from a normalized on a total error of 
  ///             the track and the vertex distance between the track and the primary vertex), only
  ///             element cut[0][0] is used to select tracks, all other elements cut[*][0] are not used;
  /// cut[*][1] - chi2/ndf of the reconstructed particle;
  /// cut[*][2] - z coordinate of the reconstructed particle.
  /// cut[*][3] - chi2/ndf of the reconstructed particle fitted to the PV;

  void FindParticles(KFPTrackVector* vRTracks, kfvector_float* ChiToPrimVtx,
                     std::vector<KFParticle>& Particles, std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx, int nPV);

  void ExtrapolateToPV(std::vector<KFParticle>& vParticles, KFParticleSIMD& PrimVtx);
  float_v GetChi2BetweenParticles(KFParticleSIMD &p1, KFParticleSIMD &p2);
     
  inline void ConstructV0(KFPTrackVector* vTracks,
                    int iTrTypePos,
                    int iTrTypeNeg,
                    uint_v& idPosDaughters,
                    uint_v& idNegDaughters,
                    int_v& daughterPosPDG,
                    int_v& daughterNegPDG,
                    KFParticleSIMD& mother,
                    KFParticle& mother_temp,
                    const unsigned short NTracks,
                    kfvector_floatv& l,
                    kfvector_floatv& dl,
                    std::vector<KFParticle>& Particles,
                    std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                    const float* cuts,
                    const int_v& pvIndex,
                    const float* secCuts,
                    const float_v& massMotherPDG,
                    const float_v& massMotherPDGSigma,
                    KFParticleSIMD& motherPrimSecCand,
                    int& nPrimSecCand,
                    std::vector< std::vector<KFParticle> >* vMotherPrim = 0,
                    std::vector<KFParticle>* vMotherSec = 0
                  ) __attribute__((always_inline));
  
  void SaveV0PrimSecCand(KFParticleSIMD& mother,
                          int& NParticles,
                          KFParticle& mother_temp,
                          std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                          const float* secCuts,
                          std::vector< std::vector<KFParticle> >* vMotherPrim,
                          std::vector<KFParticle>* vMotherSec);
  
  void ConstructTrackV0Cand(KFPTrackVector& vTracks,
                              uint_v& idTracks,
                              int_v& trackPDG,
                              KFParticle* vV0[],
                              KFParticleSIMD& mother,
                              std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& motherTopo,
                              KFParticle& mother_temp,
                              const unsigned short nElements,
                              kfvector_floatv& l,
                              kfvector_floatv& dl,
                              std::vector<KFParticle>& Particles,
                              std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                              const float_v* cuts,
                              const int_v& pvIndex,
                              const float_v& massMotherPDG,
                              const float_v& massMotherPDGSigma,
                              std::vector< std::vector<KFParticle> >* vMotherPrim,
                              std::vector<KFParticle>* vMotherSec);

  void Find2DaughterDecay(KFPTrackVector* vTracks, kfvector_float* ChiToPrimVtx,
                          std::vector<KFParticle>& Particles,
                          std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                          const float* cuts,
                          const float* secCuts,
                          std::vector< std::vector<KFParticle> >* vMotherPrim,
                          std::vector<KFParticle>* vMotherSec );
  
  void ConstructPrimaryBG(KFPTrackVector* vTracks,
                          std::vector<KFParticle>& Particles,
                          std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                          const float* cuts,
                          const float* secCuts,
                          std::vector< std::vector<KFParticle> >* vMotherPrim,
                          std::vector<KFParticle>* vMotherSec );
  
  void NeutralDaughterDecay(KFPTrackVector* vTracks, std::vector<KFParticle>& Particles);

  void FindTrackV0Decay(std::vector<KFParticle>& vV0,
                        const int V0PDG,
                        KFPTrackVector& vTracks,
                        const int q,
                        const int firstTrack,
                        const int lastTrack,
                        std::vector<KFParticle>& Particles,    
                        std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                        int v0PVIndex = -1,
                        kfvector_float* ChiToPrimVtx = 0,
                        std::vector< std::vector<KFParticle> >* vMotherPrim = 0,
                        std::vector<KFParticle>* vMotherSec = 0);

  void SelectParticles(std::vector<KFParticle>& Particles,
                       std::vector<KFParticle>& vCandidates,
                       std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                       const float& cutChi2Topo,
                       const float& cutLdL,
                       const float& mass,
                       const float& massErr,
                       const float& massCut);
  
  void CombinePartPart(std::vector<KFParticle>& particles1,
                       std::vector<KFParticle>& particles2,
                       std::vector<KFParticle>& Particles,
                       std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                       const float* cuts,
                       int iPV,
                       const int MotherPDG,
                       bool isSameInputPart = 0,
                       bool saveOnlyPrimary = 1,
                       std::vector< std::vector<KFParticle> >* vMotherPrim = 0,
                       std::vector<KFParticle>* vMotherSec = 0,
                       float massMotherPDG = 0.f,
                       float massMotherPDGSigma = 0.f);
  
  //set cuts
  void Set2DCuts(const float chi2Prim = 3.f, const float chi2 = 3.f, const float ldl = 5.f) {
    fCuts2D[0] = chi2Prim; 
//     fCuts2D[1] = chi2;      
//     fCuts2D[2] = ldl;
  } 
  void SetSecondaryCuts(const float sigmaMass = 3.f, const float chi2Topo = 5.f, const float ldl = 10.f) {
    fSecCuts[0] = sigmaMass;
    fSecCuts[1] = chi2Topo;
    fSecCuts[2] = ldl;
  }
  
  //Set Emc clusters containing gammas
  void SetEmcClusters(KFPEmcCluster* clusters) { fEmcClusters = clusters; }
  
  // Mixed Event Analysis
  void SetMixedEventAnalysis() { fMixedEventAnalysis = 1; }
  
  //Get secondary particles with the mass constraint
  static int GetNSecondarySets()  { return fNSecCandidatesSets; }
  const std::vector<KFParticle>* GetSecondaryCandidates() const { return fSecCandidates;    }
  const std::vector<KFParticle>& GetSecondaryK0()         const { return fSecCandidates[0]; }
  const std::vector<KFParticle>& GetSecondaryLambda()     const { return fSecCandidates[1]; }
  const std::vector<KFParticle>& GetSecondaryAntiLambda() const { return fSecCandidates[2]; }
  const std::vector<KFParticle>& GetSecondaryGamma()      const { return fSecCandidates[3]; }
  const std::vector<KFParticle>& GetSecondaryPi0()        const { return fSecCandidates[4]; }
  
  //Get primary particles with the mass constraint
  static int GetNPrimarySets() { return fNPrimCandidatesTopoSets; }
  const std::vector< std::vector<KFParticle> >* GetPrimaryCandidates() const { return fPrimCandidates;    }
  const std::vector< std::vector<KFParticle> >& GetPrimaryK0()         const { return fPrimCandidates[0]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryLambda()     const { return fPrimCandidates[1]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryAntiLambda() const { return fPrimCandidates[2]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryGamma()      const { return fPrimCandidates[3]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryPi0()        const { return fPrimCandidates[4]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryXi()         const { return fPrimCandidates[5]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryAntiXi()     const { return fPrimCandidates[6]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryOmega()      const { return fPrimCandidates[7]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryAntiOmega()  const { return fPrimCandidates[8]; }
  
  //Get primary particles with the topologigal constraint
  const std::vector< std::vector<KFParticle> >* GetPrimaryTopoCandidates() const { return fPrimCandidatesTopo;    }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoK0()         const { return fPrimCandidatesTopo[0]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoLambda()     const { return fPrimCandidatesTopo[1]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoAntiLambda() const { return fPrimCandidatesTopo[2]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoGamma()      const { return fPrimCandidatesTopo[3]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoPi0()        const { return fPrimCandidatesTopo[4]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoXi()         const { return fPrimCandidatesTopo[5]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoAntiXi()     const { return fPrimCandidatesTopo[6]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoOmega()      const { return fPrimCandidatesTopo[7]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoAntiOmega()  const { return fPrimCandidatesTopo[8]; }

  //Get primary particles with the topologigal and mass constraint
  const std::vector< std::vector<KFParticle> >* GetPrimaryTopoMassCandidates() const { return fPrimCandidatesTopoMass;    }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassK0()         const { return fPrimCandidatesTopoMass[0]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassLambda()     const { return fPrimCandidatesTopoMass[1]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassAntiLambda() const { return fPrimCandidatesTopoMass[2]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassGamma()      const { return fPrimCandidatesTopoMass[3]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassPi0()        const { return fPrimCandidatesTopoMass[4]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassXi()         const { return fPrimCandidatesTopoMass[5]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassAntiXi()     const { return fPrimCandidatesTopoMass[6]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassOmega()      const { return fPrimCandidatesTopoMass[7]; }
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassAntiOmega()  const { return fPrimCandidatesTopoMass[8]; }
  
  void AddCandidate(const KFParticle& candidate, int iPV = -1);
  void SetNPV(int nPV);
  
  //Functionality to change cuts, all cuts have default values set in the constructor
  void SetMaxDistanceBetweenParticlesCut(float cut) { fDistanceCut = cut; }
  void SetLCut(float cut) { fLCut = cut; }
  
  void SetChiPrimaryCut2D(float cut) { fCuts2D[0] = cut; }
  void SetChi2Cut2D(float cut)       { fCuts2D[1] = cut; }
  void SetLdLCut2D(float cut)        { fCuts2D[2] = cut; }
  
  void SetLdLCutXiOmega(float cut)      { fCutsTrackV0[0][0] = cut; }
  void SetChi2TopoCutXiOmega(float cut) { fCutsTrackV0[0][1] = cut; }
  void SetChi2CutXiOmega(float cut)     { fCutsTrackV0[0][2] = cut; }

  void SetChi2TopoCutResonances(float cut) { fCutsTrackV0[2][1] = cut; }
  void SetChi2CutResonances(float cut)     { fCutsTrackV0[2][2] = cut; }

  void SetPtCutLMVM(float cut) { fCutLVMPt = cut; }
  void SetPCutLMVM(float cut)  { fCutLVMP = cut; }
  void SetPtCutJPsi(float cut) { fCutJPsiPt = cut; }
  
  void SetPtCutCharm(float cut)         { fCutCharmPt = cut; }
  void SetChiPrimaryCutCharm(float cut) { fCutCharmChiPrim = cut; }
  void SetLdLCutCharmManybodyDecays(float cut)      { fCutsTrackV0[1][0] = cut; }
  void SetChi2TopoCutCharmManybodyDecays(float cut) { fCutsTrackV0[1][1] = cut; }
  void SetChi2CutCharmManybodyDecays(float cut)     { fCutsTrackV0[1][2] = cut; }

  void SetLdLCutCharm2D(float cut)      { fCutsCharm[1] = cut; }
  void SetChi2TopoCutCharm2D(float cut) { fCutsCharm[2] = cut; }
  void SetChi2CutCharm2D(float cut)     { fCutsCharm[0] = cut; }
  
  void AddDecayToReconstructionList(int pdg) { fDecayReconstructionList[pdg] = true; }
    
 private:

  short int fNPV;
  short int fNThreads;
  
  float fDistanceCut;
  float fLCut;

  float fCuts2D[3]; //chi2_prim, chi2_geo, l/dl
  float fSecCuts[3]; //mass, chi2_topo, l/dl
  float fCutsTrackV0[3][3]; //ldl, chi2_topo, chi2_geo
  float fCutsPartPart[2][3]; //ldl, chi2_topo, chi2_geo
  
  //cuts on open charm particles with 2 daughters
  float fCutCharmPt, fCutCharmChiPrim; //cuts on tracks
  float fCutsCharm[3]; //cuts on reconstructed charm candidates: chi2, l/dl, chi2_topo
  
  //cuts on LVM
  float fCutLVMPt, fCutLVMP;
  
  //cuts on J/Psi
  float fCutJPsiPt;
  
  //vectors with temporary particles for charm reconstruction
  std::vector<KFParticle> fD0;
  std::vector<KFParticle> fD0bar;
  std::vector<KFParticle> fD04;
  std::vector<KFParticle> fD04bar;
  std::vector<KFParticle> fD0KK;
  std::vector<KFParticle> fD0pipi;
  std::vector<KFParticle> fDPlus;
  std::vector<KFParticle> fDMinus;
  std::vector<KFParticle> fDPlus3Pi;
  std::vector<KFParticle> fDMinus3Pi;
  std::vector<KFParticle> fDsPlusK2Pi;
  std::vector<KFParticle> fDsMinusK2Pi;
  std::vector<KFParticle> fLcPlusP2Pi;
  std::vector<KFParticle> fLcMinusP2Pi;
  
  //vectors with temporary particles for H0
  std::vector<KFParticle> fLPi; //Lambda Pi+ combination
  std::vector<int> fLPiPIndex; //index of the proton in Labmda
  std::vector<KFParticle> fHe3Pi; //He3+ Pi- combination
  std::vector<KFParticle> fHe3PiBar; //He3- Pi+ combination
  std::vector<KFParticle> fHe4Pi; //He4+ Pi- combination
  std::vector<KFParticle> fHe4PiBar; //He4- Pi+ combination
  std::vector<KFParticle> fHe4L;
  std::vector<KFParticle> fHe5L;
  std::vector<KFParticle> fLLn;
  std::vector<KFParticle> fH5LL;
  
  //vectors of candidates with the mass constraint
  static const int fNSecCandidatesSets = 5;
  std::vector<KFParticle> fSecCandidates[fNSecCandidatesSets]; //Ks, Lambda, LambdaBar, gamma, pi0
  static const int fNPrimCandidatesSets = 11;
  std::vector< std::vector<KFParticle> > fPrimCandidates[fNPrimCandidatesSets]; //0 Ks, 1 Lambda,2 LambdaBar, 3 gamma, 4 pi0, 5 Xi, 6 XiBar, 7 Omega, 8 OmegaBar, 9 XiStar, 10 XiStarBar
  static const int fNPrimCandidatesTopoSets = 9;
  std::vector< std::vector<KFParticle> > fPrimCandidatesTopo[fNPrimCandidatesTopoSets];
  std::vector< std::vector<KFParticle> > fPrimCandidatesTopoMass[fNPrimCandidatesTopoSets];
  
  //pointer to the gamma EMC clusters
  KFPEmcCluster* fEmcClusters;

  bool fMixedEventAnalysis;
  
  std::map<int,bool> fDecayReconstructionList;
  
  KFParticleFinder(const KFParticleFinder&);
  KFParticleFinder& operator=(const KFParticleFinder&);
};

#endif /* !KFParticleFinder_h */

