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

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE

#ifndef KFTOPOPERFORMANCE_H
#define KFTOPOPERFORMANCE_H


#include "KFParticlePerformanceBase.h"

#include "KFMCVertex.h"
#include "KFMCTrack.h"
#include <cstdio>
#include <map>

#include "KFPartMatch.h"
#include "KFMCParticle.h"

class AliHLTTPCCAGBTracker;

class KFParticleTopoReconstructor;
class KFPHistogram;

/** @class KFTopoPerformance
 ** @brief The class to collect histograms.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class collects a set of histograms.
 ** For each particle from the KF Particle Reconstruction scheme histograms with parameter distribution,
 ** efficiencies, fit QA, fit QA of daughters, histograms for the side bands method and histograms for
 ** multi-differential extraction of spectra are collected. Also, a set of histograms for quality of
 ** the reconstructed parameters of primary vertices is created: distribution of parameters; fit QA;
 ** fit QA of primary tracks; contamination of ghost, secondary (background) tracks and tracks from
 ** another primary vertex; efficiency.
 **/

class KFTopoPerformance: public KFParticlePerformanceBase
{
 public:
  
  KFTopoPerformance();
  virtual ~KFTopoPerformance();
#ifdef KFPWITHTRACKER
  virtual void SetNewEvent(
    const AliHLTTPCCAGBTracker * const Tracker,
    AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
    AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
    AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints);
#endif  
  void SetTopoReconstructor( const KFParticleTopoReconstructor * const TopoReconstructor );
  const KFParticleTopoReconstructor * GetTopoReconstructor() const { return fTopoReconstructor; } ///< Returns pointer to the KFParticleTopoReconstructor object.
    
    // Check if MC track is reconstructable. Calculate set of MC track. Etc.
  virtual void CheckMCTracks(); // fill mcData.
    // Find reco-MCTracks correspondence
  virtual void MatchTracks();   // fill recoData.
    // Calculate efficiencies
  
  /// Histograms
  void FillHistos();
  void FillHistos(const KFPHistogram* histograms);
  void FillMCHistos();

  void AddV0Histos();
  
  void SetTrackMatch(const std::vector<int>& trackMatch) { fTrackMatch = trackMatch;} ///< Fill matching between Monte Carlo and reconstructed tracks.
  void SetMCTracks(const std::vector<KFMCTrack>& mcTracks) { vMCTracks = mcTracks; } ///< Fill Monte Carlo tracks.
  
  const KFPartEfficiencies GetEfficiency() const { return fParteff; } ///< Returns KFPartEfficiencies object with calculated efficiency.
  void SetPrintEffFrequency(int n) { fPrintEffFrequency = n;} ///< Sets frequency in events for efficiency table to be printed on the screen.

  const std::vector<KFMCVertex> GetPrimVertices() { return fPrimVertices; } ///< Returns Monte Carlo primary vertices in the current event.
  const std::vector<KFMCParticle>& MCParticles() { return vMCParticles; } ///< Returns Monte Carlo particles in the current event.
  const std::vector<KFPartMatch>& ParticlesMatch() { return RtoMCParticleId; } ///< Returns matching between reconstructed and Monte Carlo particles.
  const std::vector<KFPartMatch>& GetMCtoRPVId() { return MCtoRPVId; } ///< Returns matching between Monte Carlo and reconstructed primary vertices.
  const std::vector<KFPartMatch>& GetRtoMCPVId() { return RtoMCPVId; } ///< Returns matching between reconstructed and Monte Carlo primary vertices.
  const KFMCTrack& GetMCTrack(const int iRecoTrack)
  { 
    /** Returns Monte Carlo track matched with the reconstructed track with index "iRecoTrack". */
    int iMCTrack = 0;
    if(RtoMCParticleId[iRecoTrack].IsMatched())
      iMCTrack = RtoMCParticleId[iRecoTrack].GetBestMatch();
    return vMCTracks[iMCTrack]; 
  }
  
  void SetCentralityBin(const int iBin) { fCentralityBin = iBin; } ///< Sets centrality bin of the current event.
  void SetCentralityWeight(const float weight) { fCentralityWeight = weight; } ///< Sets weight of the centrality bin of the current event.
  
 private:

  const KFTopoPerformance& operator = (const KFTopoPerformance&); ///< Copying of objects of this class is forbidden.
  KFTopoPerformance(const KFTopoPerformance&); ///< Copying of objects of this class is forbidden.
   
  void GetMCParticles();
  void MatchParticles();
  void MatchPV();
  void CalculateEfficiency();
  void CalculatePVEfficiency();
  void FindReconstructableMCParticles();
  void CheckMCParticleIsReconstructable(KFMCParticle &part);
  void FindReconstructableMCVertices();
  void FillParticleParameters(KFParticle& TempPart,
                              int iParticle,
                              int iP,
                              int iPV,
                              TH1F* histoParameters[4][KFPartEfficiencies::nParticles][nHistoPartParam],
                              TH2F* histoParameters2D[4][KFPartEfficiencies::nParticles][nHistoPartParam2D],
                              TH3F* histoParameters3D[1][KFPartEfficiencies::nParticles][nHistoPartParam3D],
                              TH1F* histoFit[KFPartEfficiencies::nParticles][nFitQA] = 0,
                              TH1F* histoFitDaughtersQA[KFPartEfficiencies::nParticles][nFitQA] = 0,
                              TH1F* histoDSToParticleQA[KFPartEfficiencies::nParticles][nDSToParticleQA] = 0,
                              std::vector<int>* multiplicities = 0);
  
  const KFParticleTopoReconstructor *fTopoReconstructor; ///< Pointer to the KFParticleTopoReconstructor object with particles and vertices to be analysed.

  std::vector<KFMCVertex> fPrimVertices; ///< Monte Carlo primary vertices.
  std::vector<int> fMCTrackToMCPVMatch; ///< Matching of Monte Carlo tracks and corresponding primary vertex 
  std::vector<double> fPVPurity; ///< Purity of the primary vertices.
  std::vector<double> fPVTracksRate[4]; ///< Ratio in the primary vertices of: 0 - ghost tracks, 1 - from trigger PV, 2 - from pileup, 3 - from physics background.
  std::vector<int> fNCorrectPVTracks; ///< Number of correctly attached tracks in the corresponding reconstructed primary vertex.

  std::vector<int> fTrackMatch; ///< Matching between reconstructed tracks and 
  std::vector<KFMCTrack> vMCTracks;  ///< Monte Carlo tracks (parameters of the particle trajectories at the production point).
  std::vector<KFMCParticle> vMCParticles;  ///< Monte Carlo particles.
  std::vector<int> fNeutralIndex; ///< Index of the created neutral daughters for missing mass method in vMCTracks for the Monte Carlo track with given index.
  
  /** Matching between Monte Carlo and reconstructed particles. MCtoRParticleId[i] provides index of the reconstructed particle in the 
   ** fTopoReconstructor->GetParticles() array for the Monte Carlo particle (or track) with index "i". **/
  std::vector<KFPartMatch> MCtoRParticleId;
  /** Matching between reconstructed and Monte Carlo particles. RtoMCParticleId[i] provides index of the Monte Carlo particle in the 
   ** vMCTracks and vMCParticles arrays for the reconstructed particle with index "i" from fTopoReconstructor->GetParticles(). **/
  std::vector<KFPartMatch> RtoMCParticleId;

  /** Matching between Monte Carlo and reconstructed primary vertices. MCtoRPVId[i] provides index of the reconstructed vertex in the 
   ** fTopoReconstructor->GetPrimVertex() array for the Monte Carlo vertex with index "i". **/
  std::vector<KFPartMatch> MCtoRPVId;
  /** Matching between reconstructed and Monte Carlo primary vertices. RtoMCPVId[i] provides index of the Monte Carlo vertex in the 
   ** fPrimVertices array for the reconstructed vertex with index "i" from fTopoReconstructor->GetPrimVertex(). **/
  std::vector<KFPartMatch> RtoMCPVId;
  
  int fPrintEffFrequency; ///< Frequency in events with which efficiency table is printed on the screen. 
  
  int fCentralityBin; ///< Centrality bin for the current event. 
  float fCentralityWeight; ///< Centrality weight for the current event.
};

#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
