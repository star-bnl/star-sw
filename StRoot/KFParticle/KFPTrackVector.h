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
 public:
  KFPTrackVector():fId(), fPDG(), fQ(), fPVIndex(), fNE(0), fNMu(0), fNPi(0), fNK(0), fNP(0), fND(0), fNT(0), fNHe3(0), fNHe4(0) { }
  ~KFPTrackVector() { }

  int Size() const { return fP[0].size(); }
  
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

  const kfvector_int& Id()    const { return fId; }
  const kfvector_int& PDG()   const { return fPDG; }
  const kfvector_int&  Q()     const { return fQ; }
  const kfvector_int&  PVIndex()     const { return fPVIndex; }

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
  void SetId        (int value, int iTr)           { fId[iTr] = value; }
  void SetPDG       (int value, int iTr)           { fPDG[iTr] = value; }
  void SetQ         (int value, int iTr)           { fQ[iTr] = value; }
  void SetPVIndex   (int value, int iTr)           { fPVIndex[iTr] = value; }
  void SetLastElectron(int n) { fNE = n; }
  void SetLastMoun    (int n) { fNMu = n; }
  void SetLastPion    (int n) { fNPi = n; }
  void SetLastKaon    (int n) { fNK = n; }
  void SetLastProton  (int n) { fNP = n; }
  void SetLastDeuteron(int n) { fND = n; }
  void SetLastTritium (int n) { fNT = n; }
  void SetLastHe3     (int n) { fNHe3 = n; }
  void SetLastHe4     (int n) { fNHe4 = n; }
  
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
  void PrintTracks();
  
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
  
 private:  
  kfvector_float fP[6];  //coordinates of the track : x, y, z, px, py, pz
  kfvector_float fC[21];  //Covariance matrix of the track parameters

  kfvector_int fId;
  kfvector_int fPDG;
  kfvector_int fQ;     //charge
  kfvector_int fPVIndex; //index of Primary Vertex, to which the track is attached
  
#ifdef NonhomogeneousField
  kfvector_float fField[10];
#endif
  
  int fNE, fNMu, fNPi, fNK, fNP, fND, fNT, fNHe3, fNHe4;
};

#endif
