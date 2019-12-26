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

#ifndef _KFMCParticle_h_
#define _KFMCParticle_h_

#include <vector>

#ifdef HLTCA_STANDALONE
#include "RootTypesDef.h"
#else
#include "TObject.h"
#endif

/** @class KFMCParticle
 ** @brief A class to store relations between mother and daughter Monte Carlo simulated particles.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class is used to calculate reconstruction efficiency of all Monte Carlo particles. It is
 ** simplifies the procedure for short-lived particles. Contains a vector with unique Ids of all
 ** MC daughters, a unique Id of the corresponding MC track, a unique Id of the MC mother particle,
 ** the PDG code of the MC particle, flags showing if particle can be reconstructed according
 ** to several different definitions, flags showing if particle creates a secondary vertex with
 ** two or more daughters, an index of the initial particle Id in case of the K->mu+nu and pi-> mu+nu
 ** decays, since GEANT engines do not store neutrinos. 
 **/

class KFMCParticle :public TObject
{
 public:
  KFMCParticle();
  ~KFMCParticle();

  void AddDaughter( int i ); ///< Adds an Id of the new particle to the list with Ids of daughter particles.
  int  NDaughters() const { return fDaughterIds.size(); } ///< Returns number of daughter particles.
  const std::vector<int>&  GetDaughterIds() const { return fDaughterIds; } ///< Returns a reference to the vector with Id of daughter particle KFMCParticle::fDaughterIds.
  void CleanDaughters() { fDaughterIds.resize(0); } ///< Remove Ids of all daughter particles from the current object.

  void SetPDG(int pdg) {fPDG = pdg;} ///< Set the PDG code of the current particle KFMCParticle::fPDG.
  void SetMCTrackID(int id) {fMCTrackID = id;} ///< Sets the Id of the corresponding Monte Carlo track KFMCParticle::fMCTrackID.
  void SetMotherId(int id) {fMotherId = id;} ///< Sets the Id of the mother particle or primary vertex KFMCParticle::fMotherId.
  
  int  GetMCTrackID()      const {return fMCTrackID;} ///< Returns Id of the corresponding MC track KFMCParticle::fMCTrackID.
  int  GetMotherId()       const {return fMotherId;}  ///< Returns Id of the mother particle or primary vertex KFMCParticle::fMotherId.
  int  GetPDG()            const {return fPDG;}       ///< Returns PDG code of the current particle KFMCParticle::fPDG.
  
  bool IsReconstructable(int i) const {return fIsReconstructable[i];} ///< Returns a flag showing if particle can be reconstructed with KFMCParticle::fIsReconstructable index "i".
  void SetAsReconstructable(int i) { fIsReconstructable[i] = 1;} ///< Defines the particle as those which should be reconstructed for the efficiency set "i".
    
  bool IsReconstructableV0(int i) const {return fIsV0[i];} ///< Returns a flag showing if particle is a reconstructable V0.
  void SetAsReconstructableV0(int i) { fIsV0[i] = 1;}      ///< Defines the particle as V0 which should be reconstructed for the efficiency set "i".
  
  void SetInitialParticleId(int i) {fInitialParticleId = i;} ///< Sets Id of the Monte Carlo particle, from which the current particle was copied.
  int InitialParticleId() const {return fInitialParticleId;} ///< Returns the Id of the Monte Carlo particle, from which the current particle was copied.
 private: //data
  std::vector<int> fDaughterIds; ///< A vector with Ids of the daughter Monte Carlo particles.
  int fMCTrackID; ///< A unique Id of the corresponding Monte Carlo track.
  int fMotherId;  ///< A unique Id of the mother particle. If the current particle is primary the Id of the primary vertex with a negative sigh is stored.
  int fPDG;       ///< A PDG code of the current particle.
  
  /** Flags for calculation of the efficiency, define the denominator in each set of efficiency. 
   ** Flags 0-2 are used for particles reconstructed by the conventional method, 3 and 4 are used
   ** for particles found by the missing mass method: \n
   ** [0] - true for all particles, is used for calculation of the efficiency in 4pi;\n
   ** [1] - true if the particle is long-lived and can be reconstructed in the detector or
   ** if the particle is short-lived and all its daughter particles can be reconstructed; detector-dependent;\n
   ** [2] - true if the particle is long-lived and is reconstructed in the detector or
   ** if the particle is short-lived and all its daughter particles are reconstructed, 
   ** is used in calculation of efficiency of the KF Particle Finder method;\n
   ** [3] - true if the particle is long-lived and is reconstructed in the detector or
   ** if the particle is short-lived, can be reconstructed by the missing mass method
   ** and all its daughter particles are reconstructed; \n
   ** [4] - true for all particles, which can be found by the missing mass method,
   ** is used for calculation of the efficiency in 4pi.
   **/
  bool fIsReconstructable[5];
  /** Flags to calculate efficiency of short-lived particles producing a secondary vertex with 
   ** two or more daughters; similar to KFMCParticle::fIsReconstructable[0-2].
   **/
  bool fIsV0[3]; 

  /** For calculation of missing mass method efficiency a copy of the mother particle
   ** is created. fInitialParticleId is an Id of the initial mother particle.
   **/
  int fInitialParticleId;
#ifndef KFParticleStandalone
  ClassDef( KFMCParticle, 1 )
#endif
};

#endif

