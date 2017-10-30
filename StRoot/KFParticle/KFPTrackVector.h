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

#ifndef KFPTrackVector_H
#define KFPTrackVector_H

#include <vector>

#include "KFPTrack.h"
#include <iostream>
#include "KFParticleDef.h"

class KFPTrackVector
{
  friend class KFParticleTopoReconstructor;
 public:
  KFPTrackVector():fId(), fPDG(), fQ(), fPVIndex(), fNPixelHits(), fNE(0), fNMu(0), fNPi(0), fNK(0), fNP(0), fND(0), fNT(0), fNHe3(0), fNHe4(0) { }
  ~KFPTrackVector() { }

  int Size() const { return fP[0].size(); }
  int DataSize() const { 
    const int& size = fP[0].size();
    
    const int dataSize = size * 31 
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
  
  const kfvector_float& X()  const { return fP[0]; }
  const kfvector_float& Y()  const { return fP[1]; }
  const kfvector_float& Z()  const { return fP[2]; }
  const kfvector_float& Px() const { return fP[3]; }
  const kfvector_float& Py() const { return fP[4]; }
  const kfvector_float& Pz() const { return fP[5]; }

  const kfvector_float& Parameter(const int i)  const { return fP[i]; }
  const kfvector_float& Covariance(const int i)  const { return fC[i]; }
#ifdef NonhomogeneousField
  const kfvector_float& FieldCoefficient(const int i)  const { return fField[i]; }
#endif

  const kfvector_int& Id()         const { return fId; }
  const kfvector_int& PDG()        const { return fPDG; }
  const kfvector_int& Q()          const { return fQ; }
  const kfvector_int& PVIndex()    const { return fPVIndex; }
  const kfvector_int& NPixelHits() const { return fNPixelHits; }

  float Pt(const int n) const { return sqrt(fP[3][n]*fP[3][n]+fP[4][n]*fP[4][n]); }
  float P(const int n)  const { return sqrt(fP[3][n]*fP[3][n]+fP[4][n]*fP[4][n]+fP[5][n]*fP[5][n]); }

  //modifiers 
  void SetParameter (float value, int iP, int iTr) { fP[iP][iTr] = value; }
  void SetCovariance(float value, int iC, int iTr) { fC[iC][iTr] = value; }
  
  void SetParameter (const float_v& value, int iP, int iTr);
  void SetCovariance(const float_v& value, int iC, int iTr);
  
#ifdef NonhomogeneousField
  void SetFieldCoefficient(float value, int iP, int iTr) { fField[iP][iTr] = value; }
#endif
  void SetId          (int value, int iTr) { fId[iTr] = value; }
  void SetPDG         (int value, int iTr) { fPDG[iTr] = value; }
  void SetQ           (int value, int iTr) { fQ[iTr] = value; }
  void SetPVIndex     (int value, int iTr) { fPVIndex[iTr] = value; }
  void SetNPixelHits  (int value, int iTr) { fNPixelHits[iTr] = value; }
  void SetLastElectron(int n)              { fNE = n; }
  void SetLastMuon    (int n)              { fNMu = n; }
  void SetLastPion    (int n)              { fNPi = n; }
  void SetLastKaon    (int n)              { fNK = n; }
  void SetLastProton  (int n)              { fNP = n; }
  void SetLastDeuteron(int n)              { fND = n; }
  void SetLastTritium (int n)              { fNT = n; }
  void SetLastHe3     (int n)              { fNHe3 = n; }
  void SetLastHe4     (int n)              { fNHe4 = n; }
  
  void RecalculateLastIndex()
  {
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
  
  int FirstElectron()  { return 0; }
  const int& LastElectron()  const { return fNE; }
  int NElectrons() { return fNE; }
  int FirstMuon()  { return int(fNE/float_vLen)*float_vLen; }
  const int& LastMuon()  const { return fNMu; }
  int NMuons() { return fNMu - fNE; }   
  int FirstPion()  { return int(fNMu/float_vLen)*float_vLen; }
  const int& LastPion()  const { return fNPi; }
  int NPions() { return fNPi - fNMu; }   
  int FirstKaon()  { return int(fNPi/float_vLen)*float_vLen; }
  const int& LastKaon()  const { return fNK; }
  int NKaons() { return fNK - fNPi; }   
  int FirstProton()  { return int(fNK/float_vLen)*float_vLen; }
  const int& LastProton()  const { return fNP; }
  int NProtons() { return fNP - fNK; }   
  int FirstDeuteron()  { return int(fNP/float_vLen)*float_vLen; }
  const int& LastDeuteron()  const { return fND; }
  int NDeuterons() { return fND - fNP; } 
  int FirstTritium()  { return int(fND/float_vLen)*float_vLen; }
  const int& LastTritium()  const { return fNT; }
  int NTritiums() { return fNT - fND; } 
  int FirstHe3()  { return int(fNT/float_vLen)*float_vLen; }
  const int& LastHe3()  const { return fNHe3; }
  int NHe3s() { return fNHe3 - fNT; } 
  int FirstHe4()  { return int(fNHe3/float_vLen)*float_vLen; }
  const int& LastHe4()  const { return fNHe4; }
  int NHe4s() { return fNHe4 - fNHe3; } 
  
  void AddElectron() {fNE++;}
  void AddMuon()     {fNMu++;}
  void AddPion()     {fNPi++;}
  void AddKaon()     {fNK++;}
  void AddProton()   {fNP++;}
  void AddDeuteron() {fND++;}
  void AddTririum()  {fNT++;}
  void AddHe3()      {fNHe3++;}
  void AddHe4()      {fNHe4++;}

  void RotateXY( float_v alpha, int firstElement );
  
  void PrintTrack(int n);
  void Print();
  
  const KFPTrackVector& operator = (const KFPTrackVector& track)
  {
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
  
  void *operator new(size_t size) { return _mm_malloc(size, sizeof(float_v)); }
  void *operator new[](size_t size) { return _mm_malloc(size, sizeof(float_v)); }
  void *operator new(size_t size, void *ptr) { return ::operator new(size, ptr);}
  void *operator new[](size_t size, void *ptr) { return ::operator new(size, ptr);}
  void operator delete(void *ptr, size_t) { _mm_free(ptr); }
  void operator delete[](void *ptr, size_t) { _mm_free(ptr); }
  
 private:  
  kfvector_float fP[6];  //coordinates of the track : x, y, z, px, py, pz
  kfvector_float fC[21];  //Covariance matrix of the track parameters

  kfvector_int fId;
  kfvector_int fPDG;
  kfvector_int fQ;     //charge
  kfvector_int fPVIndex; //index of Primary Vertex, to which the track is attached
  kfvector_int fNPixelHits;
  
#ifdef NonhomogeneousField
  kfvector_float fField[10];
#endif
  
  int fNE, fNMu, fNPi, fNK, fNP, fND, fNT, fNHe3, fNHe4;
} __attribute__((aligned(sizeof(float_v))));

#endif
