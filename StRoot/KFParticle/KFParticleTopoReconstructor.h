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

#ifndef KFParticleTopoReconstructor_H
#define KFParticleTopoReconstructor_H

#include "KFParticlePVReconstructor.h"
#include "KFParticleFinder.h"

#include <vector>
#include <string>

#include "KFPTrackVector.h"
#include "KFParticleSIMD.h"

#ifdef USE_TIMERS
#ifndef HLTCA_STANDALONE
#include "TStopwatch.h"
typedef TStopwatch Stopwatch;
#else
#include "Stopwatch.h"
#endif
#endif

#ifdef KFPWITHTRACKER
class AliHLTTPCCAGBTracker;
#endif

#ifdef WITHSCIF
#include <scif.h>
#endif

/** @class KFParticleTopoReconstructor
 ** @brief Class for reconstruction of the full event topology including primary vertices and short-lived particles.
 ** @author  I.Kisel, M.Zyzak
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class receives as an input tracks from the track finder (both at first and last hit positions)
 ** with the PDG hypothesis, reconstructs primary vertex candidates, divides tracks into secondary and 
 ** primary, each of these groups are subdivided into positive and negative tracks, then they are
 ** sorted according to the PDG hypothesis, short-lived particles are constructed, optionally competition
 ** between different particle hypothesis is run for the constructed candidates.
 **/

class KFParticleTopoReconstructor{
 public:
  KFParticleTopoReconstructor():fKFParticlePVReconstructor(0),fKFParticleFinder(0),fTracks(0), fParticles(0), fPV(0), fNThreads(1)
#ifdef USE_TIMERS
  ,fTime(0.),timer()
#endif
  {
    /** The default constructor. Allocates memory for all pointers. **/
#ifdef USE_TIMERS
    for ( int i = 0; i < fNTimers; i++ ) fStatTime[i] = 0;
#endif
    fKFParticlePVReconstructor = new KFParticlePVReconstructor;
    
    fKFParticleFinder = new KFParticleFinder;
    fKFParticleFinder->SetNThreads(fNThreads);
  }
  virtual ~KFParticleTopoReconstructor();

#ifdef KFPWITHTRACKER
  /** Copies tracks from the standalone CA track finder to the vector KFParticleTopoReconstructor::fTracks
   ** assuming a given pdg hypothesis for each track. Tracks are rotated to the global coordinate system 
   ** and checked to have finite parameters. The KFParticleTopoReconstructor::fKFParticlePVReconstructor
   ** is initialised with the copied tracks.
   ** \param[in] tracker - the standalone CA track finder
   ** \param[in] pdg - pointer to the vector with PDG hypothesis for each track, if pointer is not provided
   ** "-1" is set as the pdg hypothesis for all tracks
   **/
  void Init(AliHLTTPCCAGBTracker* tracker, std::vector<int>* pdg=0); // init array of particles
#endif
  /** Copies provided particles to the vector KFParticleTopoReconstructor::fTracks
   ** assuming a given PDG hypothesis for each particle. If pointer to pdg-vector is not provided
   ** "-1" PDG is assigned to each input particle. If pointer to number of precise measurements
   ** is provided then this field of KFPTrackVector is also initialised. 
   ** Only tracks at the first hit position are initialised by this method.
   ** The KFParticleTopoReconstructor::fKFParticlePVReconstructor is initialised with the copied tracks.
   ** \param[in] particles - vector with input particles (tracks)
   ** \param[in] pdg - pointer to the vector with PDG hypothesis for each track, if pointer is not provided
   ** "-1" is set as the pdg hypothesis for all tracks
   ** \param[in] nPixelHits - pointer to the vector with number of precise measurement in each track
   **/  
  void Init(std::vector<KFParticle> &particles, std::vector<int>* pdg=0, std::vector<int>* nPixelHits=0);
  /** Initialises the pointer KFParticleTopoReconstructor::fTracks with the external pointer "particles".
   ** Primary vertices are assumed to be found and are also provided externally. Only reconstruction
   ** of short-lived particles should be run if this initialisation method is used.
   ** After reconstruction of short-lived particles the KFParticleTopoReconstructor::DeInit() method
   ** should be called, otherwise an external memory will be released that can lead to the memory
   ** corruption and segmentation fault.
   ** \param[in] particles - pointer to the external vectors with input tracks
   ** \param[in] pv - vector with externally reconstructed primary vertex candidates
   **/
  void Init(const KFPTrackVector *particles, const std::vector<KFParticle>& pv);
  /** Initialises tracks at the first and last hit positions. 
   ** The KFParticleTopoReconstructor::fKFParticlePVReconstructor is initialised with the copied 
   ** tracks at the first hit position.
   ** \param[in] tracks - vector with the tracks at the first hit position
   ** \param[in] tracksAtLastPoint - vector with the tracks at the last hit position
   **/
  void Init(KFPTrackVector &tracks, KFPTrackVector &tracksAtLastPoint);

  /** \brief Sets input clusters of the electromagnetic calorimeter to KFParticleFinder. */
  void SetEmcClusters(KFPEmcCluster* clusters) { fKFParticleFinder->SetEmcClusters(clusters); }
  void SetMixedEventAnalysis() { fKFParticleFinder->SetMixedEventAnalysis(); } ///< KFParticleFinder is forced to be run in the mixed event analysis mode.
  
  void DeInit() { fTracks = NULL; } ///< Sets a pointer to the input tracks KFParticleTopoReconstructor::fTracks to NULL.
  /** \brief Cleans all candidates for primary vertices and short-lived particles. */
  void Clear() { fParticles.clear(); fPV.clear(); fKFParticlePVReconstructor->CleanPV(); }
  
  void ReconstructPrimVertex(bool isHeavySystem = 1); // find primary vertex
  void SortTracks(); //sort tracks according to the pdg hypothesis and pv index
  void ReconstructParticles(); //find short-lived particles 
  void SelectParticleCandidates(); //clean particle candidates: track can belong to only one particle
#ifdef WITHSCIF
  void SendDataToXeonPhi( int iHLT, scif_epd_t& endpoint, void* buffer, off_t& offsetServer, off_t& offsetSender, float Bz);
#endif
  int NPrimaryVertices() const { return fKFParticlePVReconstructor->NPrimaryVertices(); } ///< Returns number of the found primary vertex candidates.
  KFParticle &GetPrimVertex(int iPV=0) const { return fKFParticlePVReconstructor->GetPrimVertex(iPV); } ///< Return primary vertex candidate with index "iPV".
  KFVertex &GetPrimKFVertex(int iPV=0) const { return fKFParticlePVReconstructor->GetPrimKFVertex(iPV); } ///< Return primary vertex candidate with index "iPV".
  /** Returns vector with track indices from a cluster with index "iPV".  */
  std::vector<int>& GetPVTrackIndexArray(int iPV=0) const { return fKFParticlePVReconstructor->GetPVTrackIndexArray(iPV); }
  
  std::vector<KFParticle> const &GetParticles() const { return fParticles; } ///< Returns constant reference to the vector with short-lived particle candidates.
  /** \brief Logically kills the candidate for short-lived particle with index "iParticle" by setting its PDG hypothesis to "-1". */
  void RemoveParticle(const int iParticle) { if(iParticle>=0 && iParticle<int(fParticles.size())) fParticles[iParticle].SetPDG(-1); } 
  const KFPTrackVector* GetTracks() const { return fTracks; } ///< Returns a pointer to the arrays with tracks KFParticleTopoReconstructor::fTracks.
  const kfvector_float* GetChiPrim() const { return fChiToPrimVtx; } ///<Returns a pointer to the arrays with chi2-deviations KFParticleTopoReconstructor::fChiToPrimVtx.
  
  KFParticleFinder* GetKFParticleFinder() { return fKFParticleFinder; } ///< Returns a pointer to the KFParticleFinder object.
  const KFParticleFinder* GetKFParticleFinder() const { return fKFParticleFinder; } ///< Returns a constant pointer to the KFParticleFinder object.
  
  void CleanPV() {
    /** Cleans vectors with primary vertex candidates and corresponding clusters by calling KFParticlePVReconstructor::CleanPV(). */
    fKFParticlePVReconstructor->CleanPV();
  }
  void AddPV(const KFVertex &pv, const std::vector<int> &tracks) { 
    /** Adds externally found primary vertex to the list together with the cluster of
     ** tracks from this vertex.
     ** \param[in] pv - external primary vertex
     ** \param[in] tracks - vector with indices of tracks associated with the provided primary vertex.
     **/
    fKFParticlePVReconstructor->AddPV(pv,tracks);
    KFParticle pvPart=pv;
    fPV.push_back(pvPart);
    fKFParticleFinder->SetNPV(fPV.size());
  }
  void AddPV(const KFVertex &pv) { 
   /** Adds externally found primary vertex to the list.
    ** \param[in] pv - external primary vertex
    **/
    fKFParticlePVReconstructor->AddPV(pv);
    KFParticle pvPart=pv;
    fPV.push_back(pvPart);
    fKFParticleFinder->SetNPV(fPV.size());
  }
  void FillPVIndices()
  {
    /** Assigns index of the corresponding primary vertex to each input track
     ** according to the clusters reconstructed by KFParticlePVReconstructor. */
    if(fTracks)
      for(int iPV=0; iPV<NPrimaryVertices(); iPV++)
        for(unsigned int iPVTrack=0; iPVTrack<GetPVTrackIndexArray(iPV).size(); iPVTrack++)
          fTracks[0].SetPVIndex(iPV,GetPVTrackIndexArray(iPV)[iPVTrack]);
  }
  /** \brief Adds an external particle candidate to the vector. */
  void AddParticle(const KFParticle& particle) { fParticles.push_back(particle); }
  /** \brief Adds an external particle candidate to the vector with primary or secondary candidates of KFParticleFinde. */
  void AddCandidate(const KFParticle& candidate, int iPV = -1) { fKFParticleFinder->AddCandidate(candidate, iPV); }

  void SetBeamLine(KFParticle& p) { fKFParticlePVReconstructor->SetBeamLine(p); } ///< Sets the beam line for precise reconstruction of the primary vertex.
#ifdef HomogeneousField
  void SetField(double b);
#endif
  
  //speed measurements
#ifdef USE_TIMERS
  void SetTime(double d) { fTime = d; } ///< Sets the total execution time.
  double Time() const { return fTime; } ///< Returns the total execution time.
  /** Returns the time of the part of the topology reconstruction according to the index "iTimer":\n
   ** 0) initialisation, \n
   ** 1) reconstruction of primary vertices, \n
   ** 2) sorting of input particles, \n
   ** 3) reconstruction of short-lived particles.
   **/
  double StatTime( int iTimer ) const { return fStatTime[iTimer]; } 
  int NTimers() const { return fNTimers; } ///< returns number of the timers to measure performance of different parts of the procedure.
#endif

  void SaveInputParticles(const std::string prefix = "KFPData", bool onlySecondary = 0);
  void SetNThreads(short int n) { fNThreads=n; } ///< Sets the number of threads to be run in KFParticleFinder. Currently is not used.
  
  void SetChi2PrimaryCut(float chi) {
    /** Sets the same chi-primary cut to the primary vertex finder and KF Particle Finder. */
    fKFParticlePVReconstructor->SetChi2PrimaryCut(chi); 
    fKFParticleFinder->SetChiPrimaryCut2D(chi);
  }
  
  void GetListOfDaughterTracks(const KFParticle& particle, std::vector<int>& daughters);
  bool ParticleHasRepeatingDaughters(const KFParticle& particle);

  const KFParticleTopoReconstructor &operator=(const KFParticleTopoReconstructor& a)
  {
    /** Copy operator. All pointers are set to zero, other members are copied. Returns the current object after copying is finished. **/
    fKFParticlePVReconstructor = 0;
    fKFParticleFinder = 0;
    fTracks = 0;
    
    fNThreads = a.fNThreads;
    
    return *this;
  }
  
  /** \brief A copy constructor. All pointers are set to zero, other members are copied. **/
  KFParticleTopoReconstructor(const KFParticleTopoReconstructor& a):fKFParticlePVReconstructor(0),fKFParticleFinder(0),fTracks(0), fParticles(), fPV(), fNThreads(a.fNThreads)
#ifdef USE_TIMERS
  ,fTime(0.),timer()
#endif
  {
  }
  
  /** Copy cuts from KF Particle Finder of another topology reconstructor object topo. */
  void CopyCuts(const KFParticleTopoReconstructor* topo) { fKFParticleFinder->CopyCuts(topo->fKFParticleFinder); }
 private:

  void GetChiToPrimVertex(KFParticleSIMD* pv, const int nPV);
  void TransportPVTracksToPrimVertex();
  
  KFParticlePVReconstructor* fKFParticlePVReconstructor; ///< Pointer to the KFParticlePVReconstructor. Allocated in the constructor.
  KFParticleFinder* fKFParticleFinder; ///< Pointer to the KFParticleFinder object. Allocated in the constructor.
  /** Pointer to the array with the input tracks. Memory is allocated by the Init() functions.
   ** For reconstruction of primary vertex candidates unsorted tracks are used. For reconstruction of short-lived particles
   ** Tracks should be sorted by the KFParticleTopoReconstructor::SortTracks() function. The tracks after sorting are divided 
   ** into several groups: \n
   ** 0) secondary positive at the first hit position; \n
   ** 1) secondary negative at the first hit position; \n
   ** 2) primary positive at the first hit position; \n
   ** 3) primary negative at the first hit position; \n
   ** 4) secondary positive at the last hit position; \n
   ** 5) secondary negative at the last hit position; \n
   ** 6) primary positive at the last hit position; \n
   ** 7) primary negative at the last hit position.
   **/
  KFPTrackVector *fTracks; 
  kfvector_float fChiToPrimVtx[2]; ///< Chi2-deviation of the secondary tracks.
  std::vector<KFParticle> fParticles; ///< Vector of the reconstructed candidates of short-lived particles.
  std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> > fPV; ///< Vector of the reconstructed primary vertices.
    
  short int fNThreads; ///< Number of threads to be run in KFParticleFinder. Currently is not used.
  
  //speed measurements
#ifdef USE_TIMERS
  double fTime; ///< Total run time.
  static const int fNTimers = 4; ///< Number of timers to measure different part of the code.
  /** \brief Execution time of different parts of the code: initialisation, reconstruction of primary vertices, 
   ** sorting of input particles, reconstruction of short-lived particles. */ 
  double fStatTime[fNTimers];
  Stopwatch timer; ///< Timer.
#endif // USE_TIMERS

}__attribute__((aligned(sizeof(float_v)))); // class KFParticleTopoReconstructor


  
#endif // KFParticleTopoReconstructor_H
  
