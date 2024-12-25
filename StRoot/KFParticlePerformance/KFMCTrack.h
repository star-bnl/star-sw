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

#ifndef KFMCTRACK_H
#define KFMCTRACK_H

#include <cmath>

/** @class KFMCTrack
 ** @brief A class for storage of the Monte Carlo simulated track in the cartesian parametrisation.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** A track is described with the parameters { X, Y, Z, Px, Py, Pz, q/P }. Parameters are stored at
 ** the origin position. Class also contains Id of the mother track, PDG code for the current track,
 ** number of Monte Carlo points produced by the simulation engine at the detector stations,
 ** number of Monte Carlo points produced at the precise detectors (like MVD in CBM, HFT in STAR, 
 ** ITS in ALICE, etc.). It also has a flag showing if track was found by the reconstruction procedure
 ** for efficiency calculation, and a flag showing if track was out of acceptance.
 **/

class KFMCTrack
{
 public:
  KFMCTrack():fMotherId(-1),fPDG(0),fNMCPoints(0), fNMCPixelPoints(0), fIsReconstructed(0),fIsOutOfDetector(0) {};

  int   MotherId()        const { return fMotherId; } ///< Returns a uniqueue Id of the mother track or primary vertex KFMCTrack::fMotherId.
  int   PDG()             const { return fPDG;}       ///< Returns PDG code of the track KFMCTrack::fPDG.
  float Par( int i )      const { return fPar[i]; }   ///< Returns value of the parameter KFMCTrack::fPar with index "i".
  float X()               const { return fPar[0]; }   ///< Returns X coordinate of the track at the origin position.
  float Y()               const { return fPar[1]; }   ///< Returns Y coordinate of the track at the origin position.
  float Z()               const { return fPar[2]; }   ///< Returns Y coordinate of the track at the origin position.
  float L()               const { return sqrt(X()*X() + Y()*Y() + Z()*Z()); } ///< Returns distance from the origin of the track to a point {0,0,0}.
  float Px()              const { return fPar[3]; }   ///< Returns Px momentum component of the track at the origin position.
  float Py()              const { return fPar[4]; }   ///< Returns Py momentum component of the track at the origin position.
  float Pz()              const { return fPar[5]; }   ///< Returns Pz momentum component of the track at the origin position.
  float P()               const { return sqrt(fPar[3]*fPar[3] + fPar[4]*fPar[4] + fPar[5]*fPar[5]); } ///< Returns momentum of the track.
  float Pt()              const { return sqrt(fPar[3]*fPar[3] + fPar[4]*fPar[4]); } ///< Returns transverse momentum of the track.
  const float *Par()      const { return fPar; }             ///< Returns a pointer to the array with track parameters KFMCTrack::fPar.
  int   NMCPoints()       const { return fNMCPoints; }       ///< Returns number of MC points KFMCTrack::fNMCPoints.
  int   NMCPixelPoints()  const { return fNMCPixelPoints; }  ///< Returns number of MC points at the precise detectors KFMCTrack::fNMCPixelPoints.
  bool  IsReconstructed() const { return fIsReconstructed; } ///< Returns a flag showing if track was found by the reconstruction procedure.
  bool  IsOutOfDetector() const { return fIsOutOfDetector; } ///< Returns a flag showing if track was out of acceptance.
  
  void SetPar( int i, float v )  { fPar[i] = v; }          ///< Sets a value "v" to the parameter with index "i".
  void SetX( float v )           { fPar[0] = v; }          ///< Sets X coordinate at the origin position of the track.
  void SetY( float v )           { fPar[1] = v; }          ///< Sets Y coordinate at the origin position of the track.
  void SetZ( float v )           { fPar[2] = v; }          ///< Sets Z coordinate at the origin position of the track.
  void SetPx( float v )          { fPar[3] = v; }          ///< Sets Px momentum component at the origin position of the track.
  void SetPy( float v )          { fPar[4] = v; }          ///< Sets Py momentum component at the origin position of the track.
  void SetPz( float v )          { fPar[5] = v; }          ///< Sets Pz momentum component at the origin position of the track.
  void SetQP( float v )          { fPar[6] = v; }          ///< Sets q/P at the origin position of the track.
  void SetMotherId( int v )      { fMotherId = v; }        ///< Sets a unique id of the mother track if track is secondary or primary vertex with a negative sign if it is primary.
  void SetPDG( int v )           { fPDG = v; }             ///< Sets PDG code of the current track.
  void SetNMCPoints( int v )     { fNMCPoints = v; }       ///< Sets number of MC points produced at the detector planes.
  void SetNMCPixelPoints( int v ){ fNMCPixelPoints = v; }  ///< Sets number of the MC points produced at the precise detectors.
  void SetReconstructed()        { fIsReconstructed = 1; } ///< Defines the track as reconstructed.
  void SetNotReconstructed()     { fIsReconstructed = 0; } ///< Defines the track as not reconstructed.
  void SetOutOfDetector()        { fIsOutOfDetector = 1; } ///< Defines the track out of acceptance.
  
 protected:

  int   fMotherId;       ///< Index of the mother track in tracks array. If track is produced at the primary vertex (PV) negative value with the PV Id is assigned.
  int   fPDG;            ///< The PDG code of the current Monte Carlo track.
  float fPar[7];         ///< Parameters of the track: { X, Y, Z, Px, Py, Pz, q/P }, where "q" is its charge.
  int   fNMCPoints;      ///< Total number of Monte Carlo points produced by the simulation engine at the detector stations.
  int   fNMCPixelPoints; ///< Number of Monte Carlo points produced at the precise detectors (like MVD in CBM, HFT in STAR, ITS in ALICE, etc.).
  
  bool fIsReconstructed; ///< A flag showing if track was found by the reconstruction procedure. Is required for correct efficiency calculation.
  bool fIsOutOfDetector; ///< A flag showing if track was out of acceptance. Is required for correct calculation of the acceptance.
};

#endif
