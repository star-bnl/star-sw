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

#ifndef KFPTrackVector_H
#define KFPTrackVector_H

#include "KFPTrack.h"
#include "KFParticleDef.h"

/** @class KFPTrackVector
 ** @brief A class to store vectors of input tracks in the cartesian parametrisation.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** A track is described with the state vector { X, Y, Z, Px, Py, Pz }
 ** and the corresponding covariance matrix. Also contains charge of the track,
 ** unique Id, assigned PDG hypothesis, charge, index of the corresponding
 ** primary vertex in case of primary track, number of hits from precise
 ** detectors (like MVD in CBM, HFT in STAR, ITS in ALICE, etc.) 
 ** and the magnetic field approximation along the track trajectory 
 ** (in case of nonhomogeneous CBM-like field). \n
 ** The data model implemented in the class is "Structure Of Arrays":
 ** each parameter is stroed in a separate vector. Such data structure
 ** allows fast vectorised access to the aligned data providing the
 ** maximum possible speed for data reading, and at the same time easy
 ** random access to the data members. Tracks are sorted by KFParticleTopoReconstructor::SortTracks():
 ** electrons, muons, pions, tracks without PID, kaons, protons, deuterons, tritons, He3, He4.
 **/

class KFPTrackVector
{
  friend class KFParticleTopoReconstructor;
 public:
  KFPTrackVector():fId(), fPDG(), fQ(), fPVIndex(), fNPixelHits(), fNE(0), fNMu(0), fNPi(0), fNK(0), fNP(0), fND(0), fNT(0), fNHe3(0), fNHe4(0) { }
  virtual ~KFPTrackVector() { }

  /**Returns size of the vectors. All data vectors have the same size. */
  int Size() const { return fP[0].size(); }
  int DataSize() const { 
    /**Returns size of the memory in floats (4 bytes or 32 bits) allocated by the current object. */
    const int& size = fP[0].size();
    
    const int dataSize = size * 32 
#ifdef NonhomogeneousField
                       + size * 10
#endif
                       + 9;
    return dataSize; 
  }
  
  void Resize(const int n);
  void Set(KFPTrackVector& v, int vSize, int offset);
  void SetTracks(const KFPTrackVector& track, const kfvector_uint& trackIndex, const int nIndexes);
  void GetTrack(KFPTrack& track, const int n);
  
  const kfvector_float& X()  const { return fP[0]; } ///< Returns constant reference to the vector with X coordinates.
  const kfvector_float& Y()  const { return fP[1]; } ///< Returns constant reference to the vector with Y coordinates.
  const kfvector_float& Z()  const { return fP[2]; } ///< Returns constant reference to the vector with Z coordinates.
  const kfvector_float& Px() const { return fP[3]; } ///< Returns constant reference to the vector with Px components of momentum.
  const kfvector_float& Py() const { return fP[4]; } ///< Returns constant reference to the vector with Py components of momentum.
  const kfvector_float& Pz() const { return fP[5]; } ///< Returns constant reference to the vector with Pz components of momentum.

  const kfvector_float& Parameter(const int i)  const { return fP[i]; }  ///< Returns constant reference to the track parameter vector with index "i".
  const kfvector_float& Covariance(const int i)  const { return fC[i]; } ///< Returns constant reference to the vector of the covariance matrix elements with index "i".
#ifdef NonhomogeneousField
  const kfvector_float& FieldCoefficient(const int i)  const { return fField[i]; } ///< Returns constant reference to the magnetic field coefficient with index "i".
#endif

  const kfvector_int& Id()         const { return fId; }      ///< Returns constant reference to the vector with track Id KFPTrackVector::fId.
  const kfvector_int& PDG()        const { return fPDG; }     ///< Returns constant reference to the vector with assigned PDG hypothesis KFPTrackVector::fPDG.
  const kfvector_int& Q()          const { return fQ; }       ///< Returns constant reference to the vector with charge KFPTrackVector::fQ.
  const kfvector_int& PVIndex()    const { return fPVIndex; } ///< Returns constant reference to the vector with indices of corresponding primary vertex KFPTrackVector::fPVIndex.
  const kfvector_int& NPixelHits() const { return fNPixelHits; } ///< Returns constant reference to the vector with the number of precise measurements KFPTrackVector::fNPixelHits.

  float Pt(const int n) const { return sqrt(fP[3][n]*fP[3][n]+fP[4][n]*fP[4][n]); } ///< Returns transverse momentum of the track with index "n".
  float P(const int n)  const { return sqrt(fP[3][n]*fP[3][n]+fP[4][n]*fP[4][n]+fP[5][n]*fP[5][n]); } ///< Returns momentum of the track with index "n".

  //modifiers 
  void SetParameter (float value, int iP, int iTr) { fP[iP][iTr] = value; } ///< Sets the "value" of the parameter "iP" of the track with index "iTr".
  void SetCovariance(float value, int iC, int iTr) { fC[iC][iTr] = value; } ///< Sets the "value" of the element of covariance matrix "iC" of the track with index "iTr".
  
  void SetParameter (const float_v& value, int iP, int iTr);
  void SetCovariance(const float_v& value, int iC, int iTr);
  
#ifdef NonhomogeneousField
  void SetFieldCoefficient(float value, int iP, int iTr) { fField[iP][iTr] = value; } ///< Sets the "value" of the field coefficient "iP" of the track with index "iTr".
#endif
  void SetId          (int value, int iTr) { fId[iTr] = value; }         ///< Sets Id of the track with index "iTr".
  void SetPDG         (int value, int iTr) { fPDG[iTr] = value; }        ///< Sets PDG hypothesis of the track with index "iTr".
  void SetQ           (int value, int iTr) { fQ[iTr] = value; }          ///< Sets charge of the track with index "iTr".
  void SetPVIndex     (int value, int iTr) { fPVIndex[iTr] = value; }    ///< Sets index of the corresponding primary vertex of the track with index "iTr".
  void SetNPixelHits  (int value, int iTr) { fNPixelHits[iTr] = value; } ///< Sets number of precise measurement of the track with index "iTr".
  void SetLastElectron(int n)              { fNE = n; }                  ///< Sets index of the last electron.
  void SetLastMuon    (int n)              { fNMu = n; }                 ///< Sets index of the last muon.
  void SetLastPion    (int n)              { fNPi = n; }                 ///< Sets index of the last pion.
  void SetLastKaon    (int n)              { fNK = n; }                  ///< Sets index of the last kaon.
  void SetLastProton  (int n)              { fNP = n; }                  ///< Sets index of the last proton.
  void SetLastDeuteron(int n)              { fND = n; }                  ///< Sets index of the last deuteron.
  void SetLastTritium (int n)              { fNT = n; }                  ///< Sets index of the last triton.
  void SetLastHe3     (int n)              { fNHe3 = n; }                ///< Sets index of the last He3.
  void SetLastHe4     (int n)              { fNHe4 = n; }                ///< Sets index of the last He4.
  
  void RecalculateLastIndex()
  {
    /** Recalculate the last index of each track specie. Should be called after track sorting. */
    fNE = 0; fNMu = 0; fNPi = 0; fNK = 0; fNP = 0; fND = 0; fNT = 0; fNHe3 = 0; fNHe4 = 0;
    for(int i=0; i<Size(); i++)
    {
      switch (abs(fPDG[i]))
      {
        case         11: fNE++; break;
        case         13: fNMu++; break;
        case         19: fNMu++; break;
        case        211: fNPi++; break;
        case          1: fNPi++; break;
        case          3: fNPi++; break;
        case        321: fNK++; break;
        case       2212: fNP++; break;
        case 1000010020: fND++; break;
        case 1000010030: fNT++; break;
        case 1000020030: fNHe3++; break;
        case 1000020040: fNHe4++; break;
      }
    }
    
    fNMu += fNE; fNPi += fNMu; fNK  += fNPi; fNP  += fNK;
    fND += fNP; fNT += fND; fNHe3 += fNT; fNHe4 += fNHe3;
  }
  
  int FirstElectron()  { return 0; } ///< Returns index of the first electron.
  const int& LastElectron()  const { return fNE; } ///< Returns index of the last electron.
  int NElectrons() { return fNE; } ///< Returns number of electrons.
  int FirstMuon()  { return int(fNE/float_vLen)*float_vLen; } ///< Returns index of the first element of the SIMD vector with the first muon.
  const int& LastMuon()  const { return fNMu; } ///< Returns index of the last muon.
  int NMuons() { return fNMu - fNE; } ///< Returns number of muons.
  int FirstPion()  { return int(fNMu/float_vLen)*float_vLen; } ///< Returns index of the first element of the SIMD vector with the first pion.
  const int& LastPion()  const { return fNPi; } ///< Returns index of the last pion.
  int NPions() { return fNPi - fNMu; } ///< Returns number of pions.
  int FirstKaon()  { return int(fNPi/float_vLen)*float_vLen; } ///< Returns index of the first element of the SIMD vector with the first kaon.
  const int& LastKaon()  const { return fNK; } ///< Returns index of the last kaon.
  int NKaons() { return fNK - fNPi; } ///< Returns number of kaons.
  int FirstProton()  { return int(fNK/float_vLen)*float_vLen; } ///< Returns index of the first element of the SIMD vector with the first proton.
  const int& LastProton()  const { return fNP; } ///< Returns index of the last proton.
  int NProtons() { return fNP - fNK; } ///< Returns number of protons.
  int FirstDeuteron()  { return int(fNP/float_vLen)*float_vLen; } ///< Returns index of the first element of the SIMD vector with the first deuteron.
  const int& LastDeuteron()  const { return fND; } ///< Returns index of the last deuteron.
  int NDeuterons() { return fND - fNP; } ///< Returns number of deuterons.
  int FirstTritium()  { return int(fND/float_vLen)*float_vLen; } ///< Returns index of the first element of the SIMD vector with the first triton.
  const int& LastTritium()  const { return fNT; } ///< Returns index of the last triton.
  int NTritiums() { return fNT - fND; } ///< Returns number of tritons.
  int FirstHe3()  { return int(fNT/float_vLen)*float_vLen; } ///< Returns index of the first element of the SIMD vector with the first He3.
  const int& LastHe3()  const { return fNHe3; } ///< Returns index of the last He3.
  int NHe3s() { return fNHe3 - fNT; } ///< Returns number of He3 tracks.
  int FirstHe4()  { return int(fNHe3/float_vLen)*float_vLen; } ///< Returns index of the first element of the SIMD vector with the first He4.
  const int& LastHe4()  const { return fNHe4; } ///< Returns index of the last He4.
  int NHe4s() { return fNHe4 - fNHe3; } ///< Returns number of He4 tracks.
  
  void AddElectron() {fNE++;}   ///< Increases by one index of the last electron.
  void AddMuon()     {fNMu++;}  ///< Increases by one index of the last muon.
  void AddPion()     {fNPi++;}  ///< Increases by one index of the last pion.
  void AddKaon()     {fNK++;}   ///< Increases by one index of the last kaon.
  void AddProton()   {fNP++;}   ///< Increases by one index of the last proton.
  void AddDeuteron() {fND++;}   ///< Increases by one index of the last deuteron.
  void AddTririum()  {fNT++;}   ///< Increases by one index of the last triton.
  void AddHe3()      {fNHe3++;} ///< Increases by one index of the last He3.
  void AddHe4()      {fNHe4++;} ///< Increases by one index of the last He4.

  void RotateXY( float_v alpha, int firstElement );
  
  void PrintTrack(int n);
  void Print();
  
  const KFPTrackVector& operator = (const KFPTrackVector& track)
  {
    /** Operator to copy one KFPTrackVector object to another. Makes one-to-one copy.*/
    const int localSize = track.Size();
    
    for(int i=0; i<6; i++)
    {
      fP[i].resize(localSize);
      for(int n=0; n<localSize; n++)
        fP[i][n] = track.fP[i][n];
    }
    
    for(int i=0; i<21; i++)
    {
      fC[i].resize(localSize);
      for(int n=0; n<localSize; n++)
        fC[i][n] = track.fC[i][n];
    }

#ifdef NonhomogeneousField
    for(int i=0; i<10; i++)
    {
      fField[i].resize(localSize);
      for(int n=0; n<localSize; n++)
        fField[i][n] = track.fField[i][n];
    }
#endif

    fId.resize(localSize);
    for(int n=0; n<localSize; n++)
      fId[n] = track.fId[n];

    fPDG.resize(localSize);
    for(int n=0; n<localSize; n++)
      fPDG[n] = track.fPDG[n];

    fQ.resize(localSize);
    for(int n=0; n<localSize; n++)
      fQ[n] = track.fQ[n];

    fPVIndex.resize(localSize);
    for(int n=0; n<localSize; n++)
      fPVIndex[n] = track.fPVIndex[n];
    
    fNPixelHits.resize(localSize);
    for(int n=0; n<localSize; n++)
      fNPixelHits[n] = track.fNPixelHits[n];
    
    fNE   = track.fNE;
    fNMu  = track.fNMu;
    fNPi  = track.fNPi;
    fNK   = track.fNK;
    fNP   = track.fNP;
    fND   = track.fND;
    fNT   = track.fNT;
    fNHe3 = track.fNHe3;
    fNHe4 = track.fNHe4;
    
    return *this;
  }
  
  void SetDataToVector(int* data, int& offset)
  {
    /** Copies entire vector to the provided memory starting form the position "offset".
     ** The function is used in KFPInputData::SetDataToVector().
     ** \param[out] data - pointer to the memory where the track vectors should be copied; since all fields of
     ** KFPTrackVector are of the same size (int or float) pointer can be safely casted to int*
     ** \param[in,out] offset - starting position in "data" where vectors should be copied; after all vectors are
     ** copied the offset is shifted on the size of the written object so the next KFPTrackVector object
     ** can be copied to the "data"
     **/
    for(int iP=0; iP<6; iP++)
    {
      memcpy( &(data[offset]), &(fP[iP][0]), Size()*sizeof(float));
      offset += Size();
    }
    
    for(int iC=0; iC<21; iC++)
    {
      memcpy( &(data[offset]), &(fC[iC][0]), Size()*sizeof(float));
      offset += Size();
    }
    
    memcpy( &(data[offset]), &(fId[0]), Size()*sizeof(float));
    offset += Size();
    
    memcpy( &(data[offset]), &(fPDG[0]), Size()*sizeof(float));
    offset += Size();
    
    memcpy( &(data[offset]), &(fQ[0]), Size()*sizeof(float));
    offset += Size();
    
    memcpy( &(data[offset]), &(fPVIndex[0]), Size()*sizeof(float));
    offset += Size();

    memcpy( &(data[offset]), &(fNPixelHits[0]), Size()*sizeof(float));
    offset += Size();
    
#ifdef NonhomogeneousField
    for(int iF=0; iF<10; iF++)
    {
      memcpy( &(data[offset]), &(fField[iF][0]), Size()*sizeof(float));
      offset += Size();
    }
#endif

    data[offset] = fNE;   offset++;
    data[offset] = fNMu;  offset++;
    data[offset] = fNPi;  offset++;
    data[offset] = fNK;   offset++;
    data[offset] = fNP;   offset++;
    data[offset] = fND;   offset++;
    data[offset] = fNT;   offset++;
    data[offset] = fNHe3; offset++;
    data[offset] = fNHe4; offset++;
  }
  
  void ReadDataFromVector(int* data, int& offset)
  {
    /** Copies entire vector from the provided memory starting form the position "offset".
     ** The function is used in KFPInputData::ReadDataFromVector().
     ** \param[in] data - pointer to the memory with the track vectors; since all fields of
     ** KFPTrackVector are of the same size (int or float) pointer can be safely casted to int*
     ** \param[in,out] offset - starting position of the memory to be copied; after all vectors are
     ** copied the offset is shifted on the size of the read object so the next KFPTrackVector object
     ** can be copied 
     **/
    for(int iP=0; iP<6; iP++)
    {
      memcpy( &(fP[iP][0]), &(data[offset]), Size()*sizeof(float));
      offset += Size();
    }
    
    for(int iC=0; iC<21; iC++)
    {
      memcpy( &(fC[iC][0]), &(data[offset]), Size()*sizeof(float));
      offset += Size();
    }
    
    memcpy( &(fId[0]), &(data[offset]), Size()*sizeof(float));
    offset += Size();
    
    memcpy( &(fPDG[0]), &(data[offset]), Size()*sizeof(float));
    offset += Size();
    
    memcpy( &(fQ[0]), &(data[offset]), Size()*sizeof(float));
    offset += Size();
    
    memcpy( &(fPVIndex[0]), &(data[offset]), Size()*sizeof(float));
    offset += Size();
    
    memcpy( &(fNPixelHits[0]), &(data[offset]), Size()*sizeof(float));
    offset += Size();
    
#ifdef NonhomogeneousField
    for(int iF=0; iF<10; iF++)
    {
      memcpy( &(fField[iF][0]), &(data[offset]), Size()*sizeof(float));
      offset += Size();
    }
#endif

    fNE = data[offset];   offset++;
    fNMu = data[offset];  offset++;
    fNPi = data[offset];  offset++;
    fNK = data[offset];   offset++;
    fNP = data[offset];   offset++;
    fND = data[offset];   offset++;
    fNT = data[offset];   offset++;
    fNHe3 = data[offset]; offset++;
    fNHe4 = data[offset]; offset++;
  }
  
  void *operator new(size_t size) { return _mm_malloc(size, sizeof(float_v)); }     ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size) { return _mm_malloc(size, sizeof(float_v)); }   ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new(size_t size, void *ptr) { return ::operator new(size, ptr);}   ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size, void *ptr) { return ::operator new(size, ptr);} ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void operator delete(void *ptr, size_t) { _mm_free(ptr); }                        ///< delete operator for the SIMD-alligned dynamic memory release
  void operator delete[](void *ptr, size_t) { _mm_free(ptr); }                      ///< delete operator for the SIMD-alligned dynamic memory release
  
 private:  
  kfvector_float fP[6];  ///< Vectors with parameters of the track : X, Y, Z, Px, Py, Pz.
  kfvector_float fC[21]; ///< Vectors with covariance matrix of the track parameters stroed in a lower triangular form.

  kfvector_int fId;         ///< Vector with the unique Id of tracks.
  kfvector_int fPDG;        ///< Vector with the PDG hypothesis.
  kfvector_int fQ;          ///< Vector with the charge of the tracks.
  kfvector_int fPVIndex;    ///< Vector with the index of the corresponding primary vertex. If track is considered secondary "-1" is stored.
  kfvector_int fNPixelHits; ///< Vector with the number of hits from precise detectors (like MVD in CBM, HFT in STAR, ITS in ALICE, etc.) 
  
  /** The coefficients of the field approximation of each field component along the track trajectory using parabolas: \n
   ** cx0 = fField[0], cx1 = fField[1], cx2 = fField[2] - coefficients of the Bx approximation; \n
   ** cy0 = fField[3], cy1 = fField[4], cy2 = fField[5] - coefficients of the By approximation; \n
   ** cz0 = fField[6], cz1 = fField[7], cz2 = fField[8] - coefficients of the Bz approximation; \n
   ** z0 = fField[9] - reference Z coordinate. \n
   ** Bx(z) = cx0 + cx1*(z-z0) + cx2*(z-z0)^2 \n
   ** By(z) = cy0 + cy1*(z-z0) + cy2*(z-z0)^2 \n
   ** Bz(z) = cz0 + cz1*(z-z0) + cz2*(z-z0)^2
   **/
#ifdef NonhomogeneousField
  kfvector_float fField[10];
#endif
  
  int fNE;   ///< Index of the last electron.
  int fNMu;  ///< Index of the last muon.
  int fNPi;  ///< Index of the last pion (plus tracks without PID).
  int fNK;   ///< Index of the last kaon.
  int fNP;   ///< Index of the last proton.
  int fND;   ///< Index of the last deuteron.
  int fNT;   ///< Index of the last triton.
  int fNHe3; ///< Index of the last He3.
  int fNHe4; ///< Index of the last He4.
} __attribute__((aligned(sizeof(float_v))));

#endif
