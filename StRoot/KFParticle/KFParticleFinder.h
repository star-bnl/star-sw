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

class KFParticleFinder
{
 public:

  KFParticleFinder();
  ~KFParticleFinder() {};
  
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
    fCuts2D[1] = chi2;      
    fCuts2D[2] = ldl;
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
  
 private:

  short int fNPV;
  short int fNThreads;
  
  float fCuts2D[3]; //chi2_prim, chi2_geo, l/dl
  float fSecCuts[3]; //mass, chi2_topo, l/dl
  float fCutsTrackV0[3][3];
  float fCutsPartPart[2][3];
  
  //cuts on charm particles
  float fCutCharmPt, fCutCharmChiPrim; //cuts on tracks
  float fCutsCharm[3]; //cuts on reconstructed charm candidates
  
  //cuts on LVM
  float fCutLVMPt, fCutLVMP;
  
  //cuts on J/Psi
  float fCutJPsiPt;
  
  //vectors with temporary particles for charm reconstruction
  std::vector<KFParticle> fD0;
  std::vector<KFParticle> fD0bar;
  std::vector<KFParticle> fD04;
  std::vector<KFParticle> fD04bar;
  std::vector<KFParticle> fDPlus;
  std::vector<KFParticle> fDMinus;
  //vectors with temporary particles for H0
  std::vector<KFParticle> fLPi; //Lambda Pi+ combination
  std::vector<int> fLPiPIndex; //index of the proton in Labmda
  std::vector<KFParticle> fHe3Pi; //He3+ Pi- combination
  std::vector<KFParticle> fHe3PiBar; //He3- Pi+ combination
  std::vector<KFParticle> fHe4Pi; //He4+ Pi- combination
  std::vector<KFParticle> fHe4PiBar; //He4- Pi+ combination
  
  std::vector<KFParticle> fK0PiPlus;
  std::vector<int> fK0PiMinusIndex;
  std::vector<KFParticle> fK0PiPi;
  
  //pointer to the gamma EMC clusters
  KFPEmcCluster* fEmcClusters;

  bool fMixedEventAnalysis;
  
  KFParticleFinder(const KFParticleFinder&);
  KFParticleFinder& operator=(const KFParticleFinder&);

};

#endif /* !KFParticleFinder_h */

