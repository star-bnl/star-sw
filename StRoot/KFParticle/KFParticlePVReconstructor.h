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


#ifndef KFParticlePVReconstructor_H
#define KFParticlePVReconstructor_H

#include "KFVertex.h"
#include "assert.h"

#include <vector>

class KFParticle;
class KFPTrackVector;

/** @class KFParticlePVReconstructor
 ** @brief Class for reconstruction of primary vertices.
 ** @author  I.Kisel, M.Zyzak
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class is based on KFVertex. For reconstruction of primary vertices
 ** the Kalman filter mathematics is used. Allows reconstruction of multiple
 ** primary vertices.
 **/

class KFParticlePVReconstructor{
 public:
  KFParticlePVReconstructor():fParticles(0), fNParticles(0), fWeight(0.f), fBeamLine(), fIsBeamLine(0), fClusters(0), fPrimVertices(0), fChi2CutPreparation(100), fChi2Cut(16) {};
  virtual ~KFParticlePVReconstructor(){};
  
  void Init(KFPTrackVector *tracks, int nParticles);
  
  void ReconstructPrimVertex();

  int NPrimaryVertices() const { return fPrimVertices.size(); } ///< Returns number of the found candidates for the primary vertex.
  KFParticle &GetPrimVertex(int iPV=0)   { return fPrimVertices[iPV]; } ///< Returns primary vertex candidate in KFParticle with index "iPV".
  KFVertex   &GetPrimKFVertex(int iPV=0)   { return fPrimVertices[iPV]; } ///< Returns primary vertex candidate in KFVertex with index "iPV".
  std::vector<int>& GetPVTrackIndexArray(int iPV=0) { return fClusters[iPV].fTracks; } ///< Returns vector with track indices from a cluster with index "iPV".
  KFParticle &GetParticle(int i){ assert( i < fNParticles ); return fParticles[i]; } ///< Returns input particle with index "i".
  
  void SetBeamLine(KFParticle& p) { fBeamLine = p; fIsBeamLine = 1; } ///< Sets the beam line position and direction, sets corresponding flag to "true".
  bool IsBeamLine() const { return fIsBeamLine; } ///< Check if the beam line is set.
  
  /** Adds externally found primary vertex to the list together with the cluster of
   ** tracks from this vertex.
   ** \param[in] pv - external primary vertex
   ** \param[in] tracks - vector with indices of tracks associated with the provided primary vertex.
   **/
  void AddPV(const KFVertex &pv, const std::vector<int> &tracks);
  /** Adds externally found primary vertex to the list.
   ** \param[in] pv - external primary vertex
   **/
  void AddPV(const KFVertex &pv);
  void CleanPV() { fClusters.clear(); fPrimVertices.clear(); } ///< Clean vectors with primary vertex candidates and corresponding clusters.

  /** \brief Sets cut fChi2Cut on chi2-deviation of primary tracks from the vertex candidate to "chi2"
   ** and a soft preparation cut fChi2CutPreparation to "10*chi2". */
  void SetChi2PrimaryCut(float chi2) { fChi2Cut = chi2; fChi2CutPreparation = chi2*5; }
  
 private:
  KFParticlePVReconstructor &operator=(KFParticlePVReconstructor &); ///< Is not defined. Deny copying of the objects of this class.
  KFParticlePVReconstructor(KFParticlePVReconstructor &); ///< Is not defined. Deny copying of the objects of this class.

  void FindPrimaryClusters( int cutNDF = 1);

  std::vector<KFParticle> fParticles; ///< Array of the input particles constructed from tracks.
  int fNParticles;                    ///< Number of the input particles.

  std::vector<float> fWeight; ///< Vector with weights of each track, the weight is defined in KFParticlePVReconstructor::Init().
  
  KFParticle fBeamLine; ///< Position and direction of the beam line.
  bool fIsBeamLine;     ///< Flag showing if the beam line is set.
  
  /** @class KFParticleCluster
   ** @brief A helper structure for reconstruction of a primary vertex.
   ** @author  I.Kisel, M.Zyzak
   ** @date 05.02.2019
   ** @version 1.0
   ** Contains a list of track and initial approximation for the vertex position,
   ** which are then provided to KFVertex object for fit.
   **/
  struct KFParticleCluster {
    KFParticleCluster():fTracks(0) {};
    std::vector<int> fTracks; ///< List of tracks in a cluster.
    float fP[3]; ///< Estimation of the vertex position based on the current cluster: {X, Y, Z}.
    float fC[6]; ///< Estimated errors of the position approximation.
  };

  std::vector< KFParticleCluster > fClusters; ///< Vector with clusters to be used for fit of a primary vertex.
  std::vector<KFVertex> fPrimVertices;  ///< Vector with reconstructed candidates for a primary vertex.
  
  float fChi2CutPreparation; ///< A soft cut on the chi2-deviation which is used to form a cluster.
  float fChi2Cut;            ///< Cut on the chi2-deviation of the tracks to the primary vertex \see KFVertex::ConstructPrimaryVertex(), where it is used.
}; // class KFParticlePVReconstructor


#endif // KFParticlePVReconstructor_H
  
