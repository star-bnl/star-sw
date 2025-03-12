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

#ifndef KFMCVERTEX_H
#define KFMCVERTEX_H

#include <iostream>
#include <vector>

/** @class KFMCVertex
 ** @brief A class to store information about simulated Monte Carlo primary vertices.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class contains coordinates of the vertex, indices of the Monte Carlo tracks produced
 ** at this vertex, classification flags, number of the reconstructed Monte Carlo tracks.
 **/

class KFMCVertex
{
 public:
  KFMCVertex();

  float Par( int i )  const { return fPar[i]; } ///< Returns parameter with index "i" from KFMCVertex::fPar

  float X()           const { return fPar[0]; } ///< Returns X coordinate of the vertex.
  float Y()           const { return fPar[1]; } ///< Returns Y coordinate of the vertex.
  float Z()           const { return fPar[2]; } ///< Returns Z coordinate of the vertex.
  
  const float* GetPar()      const { return fPar; } ///< Returns pointer to the parameters of the vertex KFMCVertex::fPar
  
  void SetPar( int i, float v )   { fPar[i] = v; } ///< Sets a value "v" to parameter "i".
  
  void SetX( float v )            { fPar[0] = v; } ///< Sets value "v" to the X coordinate.
  void SetY( float v )            { fPar[1] = v; } ///< Sets value "v" to the Y coordinate.
  void SetZ( float v )            { fPar[2] = v; } ///< Sets value "v" to the Z coordinate.

  int NDaughterTracks() const { return fDaughterTracks.size(); } ///< Returns number of Monte Carlo tracks produced at the current vertex.
  int NReconstructedDaughterTracks() const { return fNReconstructedDaughters; } ///< Returns number of reconstructed tracks from this vertex.
  void AddDaughterTrack( int iTr ) { fDaughterTracks.push_back(iTr); } ///< Adds unique id of the Monte Carlo track produced at the current vertex.
  int DaughterTrack( int iTr ) const 
  { 
    /** Returns unique id of the Monte Carlo track from this vertex with index "iTr".
     ** \param[in] iTr - index of the track.
     **/
    if(iTr >= NDaughterTracks())
    {
      std::cout << "ERROR!!!! MC PV contains only " << NDaughterTracks() << " tracks" << std::endl; 
      return -1;
    }
    return fDaughterTracks[iTr];
  }

  bool IsMCReconstructable() const { return fIsMCReconstructable; } ///< Returns flag showing if the vertex can be found (definition is based on the MC tracks)
  bool IsReconstructable() const { return fIsReconstructable; }     ///< Returns flag showing if the vertex can be found (definition is based on the reconstructed tracks)
  bool IsReconstructed()  const { return fIsReconstructed;   }      ///< Returns flag showing if the vertex was reconstructed
  
  void SetReconstructable() { fIsReconstructable = 1; }        ///< Defines the current vertex as such that can be reconstructed (based on the reconstructed tracks)
  void SetUnReconstructable() { fIsReconstructable = 0; }      ///< Defines the current vertex as such that can not be reconstructed (based on the reconstructed tracks)     
  
  void SetMCReconstructable() { fIsMCReconstructable = 1; }    ///< Defines the current vertex as such that can be reconstructed (based on the MC tracks)
  void SetMCUnReconstructable() { fIsMCReconstructable = 0; }  ///< Defines the current vertex as such that can not be reconstructed (based on the MC tracks)
  
  void SetReconstructed() { fIsReconstructed = 1; }   ///< Defines the current vertex as such that was reconstructed
  void SetUnReconstructed() { fIsReconstructed = 0; } ///< Defines the current vertex as such that was not reconstructed

  void SetNReconstructedDaughters(int n) { fNReconstructedDaughters = n; } ///< Defines number of the reconstructed tracks produced at the current vertex.
  
  bool IsTriggerPV() const { return fIsTriggerPV; } ///< Returns flag showing if the vertex is considerred as tigger.
  void SetTriggerPV() { fIsTriggerPV = 1; }         ///< Defines the current vertex as the trigger primary vertex.
  
  friend std::ostream& operator<<(std::ostream& out, const KFMCVertex &a);
  friend std::istream& operator>>(std::istream& in, KFMCVertex &a);

 protected:

  float fPar[3];                    ///< Cartesian coordinates of the vertex: { X, Y, Z }.
  std::vector<int> fDaughterTracks; ///< Vector with unique ids of the Monte Carlo tracks produced at this vertex.
  bool fIsReconstructable;          ///< Flag showing if the vertex considered as reconstructable based on the reconstructed tracks.
  bool fIsMCReconstructable;        ///< Flag showing if the vertex considered as reconstructable based on the Monte Carlo tracks.
  bool fIsReconstructed;            ///< Flag showing if vertex was reconstructed.
  int fNReconstructedDaughters;     ///< Number of found tracks, produced at the current vertex.
  bool fIsTriggerPV;                ///< Flag showing if the vertex was a trigger primary vertex.
};

#endif
