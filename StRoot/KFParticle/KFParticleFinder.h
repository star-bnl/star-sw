/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#ifndef KFParticleFinder_h
#define KFParticleFinder_h

#include "KFParticle.h"
#include "KFParticleSIMD.h"
#include "KFPTrackVector.h"

#include <vector>
#include <map>

class KFPEmcCluster;

/** @class KFParticleFinder
 ** @brief Class for reconstruction short-lived particles.
 ** @author  I.Kisel, M.Zyzak
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class reconstructs short-lived particles by combining long-lived
 ** and already reconstructed short-lived particles. As an input it requires 
 ** tracks and primary vertices. All short-lived particles are stored to one
 ** array. The default values of cuts are initialised in the constructor.
 **/

class KFParticleFinder
{
 public:

  KFParticleFinder();
  virtual ~KFParticleFinder() {};
  
  void Init(int nPV);
  void SetNThreads(short int n) { fNThreads = n;} ///< Sets the number of threads to by run in parallel. Currently not used.

  void FindParticles(KFPTrackVector* vRTracks, kfvector_float* ChiToPrimVtx,
                     std::vector<KFParticle>& Particles, std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx, int nPV);

  void ExtrapolateToPV(std::vector<KFParticle>& vParticles, KFParticleSIMD& PrimVtx);
     
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

  //Set Emc clusters containing gammas
  void SetEmcClusters(KFPEmcCluster* clusters) { fEmcClusters = clusters; } ///< Set a pointer to the gamma-clusters from the electromagnetic calorimeter.
  
  // Mixed Event Analysis
  void SetMixedEventAnalysis() { fMixedEventAnalysis = 1; } ///< Switch KFParticleFinder to the mixed event mode.
  
  //Get secondary particles with the mass constraint
  /** Returns number of sets of vectors with secondary candidates for different decays. */
  static int GetNSecondarySets()  { return fNSecCandidatesSets; }
  /** Returns a pointer to array with sets of vectors with secondary candidates for different decays. */
  const std::vector<KFParticle>* GetSecondaryCandidates() const { return fSecCandidates;    }
  /** Returns a constant reference to the vector with secondary candidates for \f$K_s^0\rightarrow\pi^+\pi^-\f$. **/
  const std::vector<KFParticle>& GetSecondaryK0()         const { return fSecCandidates[0]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\Lambda\rightarrow p\pi^-\f$. **/
  const std::vector<KFParticle>& GetSecondaryLambda()     const { return fSecCandidates[1]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\overline{\Lambda}\rightarrow \overline{p}\pi^+\f$. **/
  const std::vector<KFParticle>& GetSecondaryAntiLambda() const { return fSecCandidates[2]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\gamma\rightarrow e^+e^-\f$ conversion. **/
  const std::vector<KFParticle>& GetSecondaryGamma()      const { return fSecCandidates[3]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\pi^0\rightarrow \gamma\gamma\f$. **/
  const std::vector<KFParticle>& GetSecondaryPi0()        const { return fSecCandidates[4]; }
  
  //Get primary particles with the mass constraint
  /** Returns number of sets of vectors with primary candidates for different decays. */
  static int GetNPrimarySets() { return fNPrimCandidatesTopoSets; }
  /** Returns a pointer to array with sets of vectors with primary candidates for different decays. */
  const std::vector< std::vector<KFParticle> >* GetPrimaryCandidates() const { return fPrimCandidates;    }
  /** Returns a constant reference to the vector with primary candidates for \f$K_s^0\rightarrow\pi^+\pi^-\f$. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryK0()         const { return fPrimCandidates[0]; }
  /** Returns a constant reference to the vector with primary candidates for \f$\Lambda\rightarrow p\pi^-\f$. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryLambda()     const { return fPrimCandidates[1]; }
  /** Returns a constant reference to the vector with primary candidates for \f$\overline{\Lambda}\rightarrow \overline{p}\pi^+\f$. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryAntiLambda() const { return fPrimCandidates[2]; }
  /** Returns a constant reference to the vector with primary candidates for \f$\gamma\rightarrow e^+e^-\f$ conversion. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryGamma()      const { return fPrimCandidates[3]; }
  /** Returns a constant reference to the vector with primary candidates for \f$\pi^0\rightarrow \gamma\gamma\f$. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryPi0()        const { return fPrimCandidates[4]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\Xi^-\rightarrow \Lambda\pi^-\f$. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryXi()         const { return fPrimCandidates[5]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\overline{\Xi}^-\rightarrow \overline{\Lambda}\pi^+\f$. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryAntiXi()     const { return fPrimCandidates[6]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\Omega^-\rightarrow \Lambda K^-\f$. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryOmega()      const { return fPrimCandidates[7]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\overline{\Omega}^-\rightarrow \overline{\Lambda} K^+\f$. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryAntiOmega()  const { return fPrimCandidates[8]; }
  
  //Get primary particles with the topologigal constraint
  /** Returns a pointer to array with sets of vectors with primary candidates for different decays with topological constraint set on. */
  const std::vector< std::vector<KFParticle> >* GetPrimaryTopoCandidates() const { return fPrimCandidatesTopo;    }
  /** Returns a constant reference to the vector with primary candidates for \f$K_s^0\rightarrow\pi^+\pi^-\f$ 
   ** with topological constraint set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoK0()         const { return fPrimCandidatesTopo[0]; }
  /** Returns a constant reference to the vector with primary candidates for \f$\Lambda\rightarrow p\pi^-\f$
   ** with topological constraint set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoLambda()     const { return fPrimCandidatesTopo[1]; }
  /** Returns a constant reference to the vector with primary candidates for \f$\overline{\Lambda}\rightarrow \overline{p}\pi^+\f$
   ** with topological constraint set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoAntiLambda() const { return fPrimCandidatesTopo[2]; }
  /** Returns a constant reference to the vector with primary candidates for \f$\gamma\rightarrow e^+e^-\f$ conversion
   ** with topological constraint set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoGamma()      const { return fPrimCandidatesTopo[3]; }
  /** Returns a constant reference to the vector with primary candidates for \f$\pi^0\rightarrow \gamma\gamma\f$
   **  with topological constraint set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoPi0()        const { return fPrimCandidatesTopo[4]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\Xi^-\rightarrow \Lambda\pi^-\f$
   ** with topological constraint set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoXi()         const { return fPrimCandidatesTopo[5]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\overline{\Xi}^-\rightarrow \overline{\Lambda}\pi^+\f$
   ** with topological constraint set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoAntiXi()     const { return fPrimCandidatesTopo[6]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\Omega^-\rightarrow \Lambda K^-\f$
   ** with topological constraint set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoOmega()      const { return fPrimCandidatesTopo[7]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\overline{\Omega}^-\rightarrow \overline{\Lambda} K^+\f$
   ** with topological constraint set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoAntiOmega()  const { return fPrimCandidatesTopo[8]; }

  //Get primary particles with the topologigal and mass constraint
  /** Returns a pointer to array with sets of vectors with primary candidates for different decays with topological and mass constraints set on. */
  const std::vector< std::vector<KFParticle> >* GetPrimaryTopoMassCandidates() const { return fPrimCandidatesTopoMass;    }
  /** Returns a constant reference to the vector with primary candidates for \f$K_s^0\rightarrow\pi^+\pi^-\f$ 
   ** with topological and mass constraints set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassK0()         const { return fPrimCandidatesTopoMass[0]; }
  /** Returns a constant reference to the vector with primary candidates for \f$\Lambda\rightarrow p\pi^-\f$
   ** with topological and mass constraints set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassLambda()     const { return fPrimCandidatesTopoMass[1]; }
  /** Returns a constant reference to the vector with primary candidates for \f$\overline{\Lambda}\rightarrow \overline{p}\pi^+\f$
   ** with topological and mass constraints set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassAntiLambda() const { return fPrimCandidatesTopoMass[2]; }
  /** Returns a constant reference to the vector with primary candidates for \f$\gamma\rightarrow e^+e^-\f$ conversion
   ** with topological and mass constraints set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassGamma()      const { return fPrimCandidatesTopoMass[3]; }
  /** Returns a constant reference to the vector with primary candidates for \f$\pi^0\rightarrow \gamma\gamma\f$
   ** with topological and mass constraints set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassPi0()        const { return fPrimCandidatesTopoMass[4]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\Xi^-\rightarrow \Lambda\pi^-\f$
   ** with topological and mass constraints set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassXi()         const { return fPrimCandidatesTopoMass[5]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\overline{\Xi}^-\rightarrow \overline{\Lambda}\pi^+\f$
   ** with topological and mass constraints set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassAntiXi()     const { return fPrimCandidatesTopoMass[6]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\Omega^-\rightarrow \Lambda K^-\f$
   ** with topological and mass constraints set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassOmega()      const { return fPrimCandidatesTopoMass[7]; }
  /** Returns a constant reference to the vector with secondary candidates for \f$\overline{\Omega}^-\rightarrow \overline{\Lambda} K^+\f$
   ** with topological and mass constraints set on. **/
  const std::vector< std::vector<KFParticle> >& GetPrimaryTopoMassAntiOmega()  const { return fPrimCandidatesTopoMass[8]; }
  
  void AddCandidate(const KFParticle& candidate, int iPV = -1);
  void SetNPV(int nPV);
  
  //Functionality to change cuts, all cuts have default values set in the constructor
  void SetMaxDistanceBetweenParticlesCut(float cut) { fDistanceCut = cut; } ///< Sets cut on the distance between secondary tracks at the DCA point.
  void SetLCut(float cut) { fLCut = cut; } ///< Sets cut on the distance to the primary vertex from the decay vertex.
  
  void SetChiPrimaryCut2D(float cut) { fCuts2D[0] = cut; } ///< Sets cut on \f$\chi^2_{prim}\f$ of each track for 2-daughter decays.
  void SetChi2Cut2D(float cut)       { fCuts2D[1] = cut; } ///< Sets cut on \f$\chi^2_{geo}\f$ for 2-daughter decays.
  void SetLdLCut2D(float cut)        { fCuts2D[2] = cut; } ///< Sets cut on \f$l/\Delta l\f$ for 2-daughter decays.
  
  /** \brief Sets cuts on selection of secondary and primary candidates: \f$\sigma_{M}\f$, \f$\chi^2_{topo}\f$, \f$l/\Delta l\f$. */
  void SetSecondaryCuts(const float sigmaMass = 3.f, const float chi2Topo = 5.f, const float ldl = 10.f) {
    fSecCuts[0] = sigmaMass;
    fSecCuts[1] = chi2Topo;
    fSecCuts[2] = ldl;
  }
  
  void SetLdLCutXiOmega(float cut)      { fCutsTrackV0[0][0] = cut; } ///< Sets \f$l/\Delta l\f$ cut for \f$\Xi\f$ and \f$\Omega\f$.
  void SetChi2TopoCutXiOmega(float cut) { fCutsTrackV0[0][1] = cut; } ///< Sets \f$\chi^2_{topo}\f$ cut for \f$\Xi\f$ and \f$\Omega\f$.
  void SetChi2CutXiOmega(float cut)     { fCutsTrackV0[0][2] = cut; } ///< Sets \f$\chi^2_{geo}\f$ cut for \f$\Xi\f$ and \f$\Omega\f$.

  void SetChi2TopoCutResonances(float cut) { fCutsTrackV0[2][1] = cut; } ///< Sets \f$\chi^2_{topo}\f$ cut for resonances.
  void SetChi2CutResonances(float cut)     { fCutsTrackV0[2][2] = cut; } ///< Sets \f$\chi^2_{geo}\f$ cut for resonances.

  void SetPtCutLMVM(float cut) { fCutLVMPt = cut; }  ///< Sets the cut on transverse momentum of each daughter track of low mass vector mesons.
  void SetPCutLMVM(float cut)  { fCutLVMP = cut; }   ///< Sets the cut on momentum of each daughter track of low mass vector mesons in dimuon channel.
  void SetPtCutJPsi(float cut) { fCutJPsiPt = cut; } ///< Sets the cut on transverse momentum of each daughter track of \f$J/\psi\f$.
  
  void SetPtCutCharm(float cut)         { fCutCharmPt = cut; } ///< Sets the cut on transverse momentum of each daughter track of open charm particles.
  void SetChiPrimaryCutCharm(float cut) { fCutCharmChiPrim = cut; } ///< Sets cut on \f$\chi^2_{prim}\f$ of each track for open charm particles.
  void SetLdLCutCharmManybodyDecays(float cut)      { fCutsTrackV0[1][0] = cut; } ///< Sets \f$l/\Delta l\f$ cut for open charm with >=3 daughters.
  void SetChi2TopoCutCharmManybodyDecays(float cut) { fCutsTrackV0[1][1] = cut; } ///< Sets \f$\chi^2_{topo}\f$ cut for open charm with >=3 daughters.
  void SetChi2CutCharmManybodyDecays(float cut)     { fCutsTrackV0[1][2] = cut; } ///< Sets \f$\chi^2_{geo}\f$ cut for open charm with >=3 daughters.

  void SetLdLCutCharm2D(float cut)      { fCutsCharm[1] = cut; } ///< Sets \f$l/\Delta l\f$ cut for open charm with 2 daughters.
  void SetChi2TopoCutCharm2D(float cut) { fCutsCharm[2] = cut; } ///< Sets \f$\chi^2_{topo}\f$ cut for open charm with 2 daughters.
  void SetChi2CutCharm2D(float cut)     { fCutsCharm[0] = cut; } ///< Sets \f$\chi^2_{geo}\f$ cut for open charm with 2 daughters.
  
  void CopyCuts(const KFParticleFinder* finder)
  {
    /** Copies all cuts from the external KFParticleFinder "finder" to the current object.
     ** \param[in] finder - constant pointer to the external KFParticleFinder object
     **/
    fDistanceCut = finder->fDistanceCut;
    fLCut = finder->fLCut;
    for(int iCut=0; iCut<3; iCut++)
      fCuts2D[iCut] = finder->fCuts2D[iCut];
    for(int iCut=0; iCut<3; iCut++)
      fSecCuts[iCut] = finder->fSecCuts[iCut];
    for(int iCut=0; iCut<3; iCut++)
      for(int jCut=0; jCut<3; jCut++)
        fCutsTrackV0[iCut][jCut] = finder->fCutsTrackV0[iCut][jCut];
    for(int iCut=0; iCut<2; iCut++)
      for(int jCut=0; jCut<3; jCut++)
        fCutsPartPart[iCut][jCut] = finder->fCutsPartPart[iCut][jCut];
    fCutCharmPt = finder->fCutCharmPt;
    fCutCharmChiPrim = finder->fCutCharmChiPrim;
    for(int iCut=0; iCut<3; iCut++)
      fCutsCharm[iCut] = finder->fCutsCharm[iCut];  
    fCutLVMPt = finder->fCutLVMPt;
    fCutLVMP = finder->fCutLVMP;
    fCutJPsiPt = finder->fCutJPsiPt;
  }
  
  //Functionality to check the cuts
  const float GetMaxDistanceBetweenParticlesCut() const { return fDistanceCut; } ///< Returns cut on the distance between secondary tracks at the DCA point.
  const float GetLCut() const { return fLCut; } ///< Returns cut on the distance to the primary vertex from the decay vertex.
  
  const float GetChiPrimaryCut2D() const { return fCuts2D[0]; } ///< Returns cut on \f$\chi^2_{prim}\f$ of each track for 2-daughter decays.
  const float GetChi2Cut2D()       const { return fCuts2D[1]; } ///< Returns cut on \f$\chi^2_{geo}\f$ for 2-daughter decays.
  const float GetLdLCut2D()        const { return fCuts2D[2]; } ///< Returns cut on \f$l/\Delta l\f$ for 2-daughter decays.
  
  const float GetSecondarySigmaMassCut() const { return fSecCuts[0]; } ///< Returns \f$\sigma_{M}\f$ cut for selection of primary and secondary candidates.
  const float GetSecondaryChi2TopoCut()  const { return fSecCuts[1]; } ///< Returns \f$\chi^2_{topo}\f$ cut for selection of primary and secondary candidates.
  const float GetSecondaryLdLCut()       const { return fSecCuts[2]; } ///< Returns \f$l/\Delta l\f$ cut for selection of primary and secondary candidates.
  
  const float GetLdLCutXiOmega()      const { return fCutsTrackV0[0][0]; } ///< Returns \f$l/\Delta l\f$ cut for \f$\Xi\f$ and \f$\Omega\f$.
  const float GetChi2TopoCutXiOmega() const { return fCutsTrackV0[0][1]; } ///< Returns \f$\chi^2_{topo}\f$ cut for \f$\Xi\f$ and \f$\Omega\f$.
  const float GetChi2CutXiOmega()     const { return fCutsTrackV0[0][2]; } ///< Returns \f$\chi^2_{geo}\f$ cut for \f$\Xi\f$ and \f$\Omega\f$.

  const float GetChi2TopoCutResonances() const { return fCutsTrackV0[2][1]; } ///< Returns \f$\chi^2_{topo}\f$ cut for resonances.
  const float GetChi2CutResonances()     const { return fCutsTrackV0[2][2]; } ///< Returns \f$\chi^2_{geo}\f$ cut for resonances.

  const float GetPtCutLMVM() const { return fCutLVMPt; }  ///< Returns cut on transverse momentum of each daughter track of low mass vector mesons.
  const float GetPCutLMVM()  const { return fCutLVMP; }   ///< Returns cut on momentum of each daughter track of low mass vector mesons in dimuon channel.
  const float GetPtCutJPsi() const { return fCutJPsiPt; } ///< Returns cut on transverse momentum of each daughter track of \f$J/\psi\f$.
  
  const float GetPtCutCharm()         const { return fCutCharmPt; } ///< Returns the cut on transverse momentum of each daughter track of open charm particles.
  const float GetChiPrimaryCutCharm() const { return fCutCharmChiPrim; } ///< Returns cut on \f$\chi^2_{prim}\f$ of each track for open charm particles.
  const float GetLdLCutCharmManybodyDecays()      const { return fCutsTrackV0[1][0]; } ///< Returns \f$l/\Delta l\f$ cut for open charm with >=3 daughters.
  const float GetChi2TopoCutCharmManybodyDecays() const { return fCutsTrackV0[1][1]; } ///< Returns \f$\chi^2_{topo}\f$ cut for open charm with >=3 daughters.
  const float GetChi2CutCharmManybodyDecays()     const { return fCutsTrackV0[1][2]; } ///< Returns \f$\chi^2_{geo}\f$ cut for open charm with >=3 daughters.

  const float GetLdLCutCharm2D()      const { return fCutsCharm[1]; } ///< Returns \f$l/\Delta l\f$ cut for open charm with 2 daughters.
  const float GetChi2TopoCutCharm2D() const { return fCutsCharm[2]; } ///< Returns \f$\chi^2_{topo}\f$ cut for open charm with 2 daughters.
  const float GetChi2CutCharm2D()     const { return fCutsCharm[0]; } ///< Returns \f$\chi^2_{geo}\f$ cut for open charm with 2 daughters.
  
  /** Add decay to the reconstruction list. If at least one is added - only those channels are considered which are in the list. Otherwise 
   ** all decays are reconstructed.
   ** \param[in] pdg - PDG code of the decay which should be reconstructed
   **/
  void AddDecayToReconstructionList(int pdg) { fDecayReconstructionList[pdg] = true; }
  const std::map<int,bool> GetReconstructionList() const { return fDecayReconstructionList; } ///< Returns list of decays to be reconstructed.
  void SetReconstructionList(const std::map<int,bool>& decays) { fDecayReconstructionList = decays; } ///< Set enitre reconstruction list

 private:

  short int fNPV; ///< Number of primary vertex candidates in the event.
  short int fNThreads; ///< Number of threads to be run in parallel. Currently is not used. 
  
  float fDistanceCut; ///< Cut on the distance between secondary tracks at the DCA point, is soft and used to speed up the algorithm only.
  float fLCut; ///< Cut on the distance to the primary vertex from the decay vertex. Is applied to \f$K^0_s\f$, \f$\Lambda\f$, \f$\Xi\f$, \f$\Omega\f$, hypernuclei and dibaryons.

  float fCuts2D[3]; ///< Cuts on 2-daughter decays: \f$\chi^2_{prim}\f$, \f$\chi^2_{geo}\f$, \f$l/\Delta l\f$
  float fSecCuts[3]; ///< Cuts to select secondary and primary particle candidates: \f$\sigma_{M}\f$, \f$\chi^2_{topo}\f$, \f$l/\Delta l\f$
  /** \brief Cuts on the combination of track and short-lived particle candidate: \f$l/\Delta l\f$, \f$\chi^2_{topo}\f$, \f$\chi^2_{geo}\f$.
   ** Three sets of cuts are defined: 1) for \f$\Xi\f$ and \f$\Omega\f$, 2) for hypernuclei and open charm, 3) for resonances. **/
  float fCutsTrackV0[3][3]; 
  /** \brief Cuts on the combination of two short-lived particle candidates: \f$l/\Delta l\f$, \f$\chi^2_{topo}\f$, \f$\chi^2_{geo}\f$. 
   ** Two sets are defined: 1) for particles that fly away from the primary vertex and 2) for resonances. **/
  float fCutsPartPart[2][3];
  
  //cuts on open charm particles with 2 daughters
  float fCutCharmPt; ///< Cut on transverse momentum of the track for open charm reconstruction.
  float fCutCharmChiPrim; ///< Cut on the \f$\chi^2_{prim}\f$ deviation of the track from the primary vertex for open charm reconstruction.
  float fCutsCharm[3]; ///< Cuts on reconstructed 2-daughter charm candidates: \f$\chi^2_{geo}\f$, \f$l/\Delta l\f$, \f$\chi^2_{topo}\f$.
  
  //cuts on LVM
  float fCutLVMPt; ///< Cut on transverse momentum of daughter tracks for low mass vector mesons.
  float fCutLVMP;  ///< Cut on momentum of low mass vector mesons in dimuon channel.
  
  //cuts on J/Psi
  float fCutJPsiPt; ///< Cut on transverse momentum of daughter tracks for \f$J/\psi\f$.
  
  //vectors with temporary particles for charm reconstruction
  std::vector<KFParticle> fD0;         ///<Vector with temporary D0->K-pi+ candidates.
  std::vector<KFParticle> fD0bar;      ///<Vector with temporary D0_bar->K+pi- candidates.
  std::vector<KFParticle> fD04;        ///<Vector with temporary D0->K-pi+pi+pi- candidates.
  std::vector<KFParticle> fD04bar;     ///<Vector with temporary D0_bar->K+pi+pi-pi- candidates.
  std::vector<KFParticle> fD0KK;       ///<Vector with temporary D0->K+K- candidates.
  std::vector<KFParticle> fD0pipi;     ///<Vector with temporary D0->pi+pi- candidates.
  std::vector<KFParticle> fDPlus;      ///<Vector with temporary D+->K-pi+pi+ candidates.
  std::vector<KFParticle> fDMinus;     ///<Vector with temporary D-->K+pi-pi- candidates.
  std::vector<KFParticle> fDPlus3Pi;   ///<Vector with temporary D+->pi+pi+pi- candidates.
  std::vector<KFParticle> fDMinus3Pi;  ///<Vector with temporary D-->pi+pi-pi- candidates.
  std::vector<KFParticle> fDsPlusK2Pi; ///<Vector with temporary Ds+->K+pi+pi- candidates.
  std::vector<KFParticle> fDsMinusK2Pi;///<Vector with temporary Ds-->K-pi+pi- candidates.
  std::vector<KFParticle> fLcPlusP2Pi; ///<Vector with temporary Lambda_c->p pi+pi- candidates.
  std::vector<KFParticle> fLcMinusP2Pi;///<Vector with temporary Lambda_c_bar->p-pi+pi- candidates.
  
  //vectors with temporary particles for H0
  std::vector<KFParticle> fLPi;      ///< Temporary Lambda pi+ combinations
  std::vector<int> fLPiPIndex;       ///< Index of the proton in Labmda for Lambda pi+ combinations
  std::vector<KFParticle> fHe3Pi;    ///< Temporary He3+ pi- combinations
  std::vector<KFParticle> fHe3PiBar; ///< Temporary He3- pi+ combinations
  std::vector<KFParticle> fHe4Pi;    ///< Temporary He4+ pi- combinations
  std::vector<KFParticle> fHe4PiBar; ///< Temporary He4- pi+ combinations
  std::vector<KFParticle> fHe4L;     ///< Vector with temporary He4_Lambda->He3 p pi- candidates
  std::vector<KFParticle> fHe5L;     ///< Vector with temporary He4_Lambda->He4 p pi- candidates
  std::vector<KFParticle> fLLn;      ///< Vector with temporary H3_Lambda pi- candidates
  std::vector<KFParticle> fH5LL;     ///< Vector with temporary H5_LL->He5_Lambda pi- candidates
  
  //vectors of candidates with the mass constraint
  static const int fNSecCandidatesSets = 5; ///< Number of sets of secondary particle candidates.
  /** \brief Array of vectors with secondary candidates: 0) \f$K_s^0\f$, 1) \f$\Lambda\f$, 2) \f$\overline{\Lambda}\f$, 3) \f$\gamma\f$, 4) \f$\pi^0\f$. */
  std::vector<KFParticle> fSecCandidates[fNSecCandidatesSets];
  static const int fNPrimCandidatesSets = 11; ///< Number of sets of primary particle candidates.
  /** \brief Array of vectors with primary candidates for each primary vertex: 
   ** 0) \f$K_s^0\f$, 1) \f$\Lambda\f$, 2) \f$\overline{\Lambda}\f$, 3) \f$\gamma\f$, 4) \f$\pi^0\f$, 
   ** 5) \f$\Xi^-\f$, 6) \f$\overline{\Xi}^+\f$, 7) \f$\Omega^-\f$, 8) \f$\overline{\Omega}^+\f$, 9) \f$\Xi^{0*}\f$, 10) \f$\overline{\Xi}^{0*}\f$. */
  std::vector< std::vector<KFParticle> > fPrimCandidates[fNPrimCandidatesSets]; //
  static const int fNPrimCandidatesTopoSets = 9; ///< Number of sets of primary particle candidates with topological constraint.
  /** \brief Array of vectors with primary candidates for each primary vertex with a topological constraint set:
   ** 0) \f$K_s^0\f$, 1) \f$\Lambda\f$, 2) \f$\overline{\Lambda}\f$, 3) \f$\gamma\f$, 4) \f$\pi^0\f$, 
   ** 5) \f$\Xi^-\f$, 6) \f$\overline{\Xi}^+\f$, 7) \f$\Omega^-\f$, 8) \f$\overline{\Omega}^+\f$. */
  std::vector< std::vector<KFParticle> > fPrimCandidatesTopo[fNPrimCandidatesTopoSets];
  /** \brief Array of vectors with primary candidates for each primary vertex with a topological and mass constraints set:
   ** 0) \f$K_s^0\f$, 1) \f$\Lambda\f$, 2) \f$\overline{\Lambda}\f$, 3) \f$\gamma\f$, 4) \f$\pi^0\f$, 
   ** 5) \f$\Xi^-\f$, 6) \f$\overline{\Xi}^+\f$, 7) \f$\Omega^-\f$, 8) \f$\overline{\Omega}^+\f$. */
  std::vector< std::vector<KFParticle> > fPrimCandidatesTopoMass[fNPrimCandidatesTopoSets];
  
  KFPEmcCluster* fEmcClusters; ///< Pointer to the input gamma-clusters from the electromagnetic calorimeter.

  bool fMixedEventAnalysis; ///< Flag defines if the mixed event analysis is run. In mixed event mode limited number of decays is reconstructed.
  
  /** \brief Map defines if the reconstruction of the decay with a certain PDG hypothesis should be run. If the map is empty - all decays are reconstructed. If at least one decay is added - only those decays will be reconstructed which are specified in the list. **/
  std::map<int,bool> fDecayReconstructionList;
  
  KFParticleFinder(const KFParticleFinder&); ///< Copying is disabled for this class.
  KFParticleFinder& operator=(const KFParticleFinder&); ///< Copying is disabled for this class.
};

#endif /* !KFParticleFinder_h */

