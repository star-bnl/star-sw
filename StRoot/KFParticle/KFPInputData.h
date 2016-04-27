//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef KFPINPUTDATA_H
#define KFPINPUTDATA_H

#include "KFPTrackVector.h"
#include "KFParticle.h"

#include <iostream>
#include <vector>
#include <string>
using std::string;

#include <fstream>
using std::ofstream;
using std::ifstream;

#include <algorithm>

struct KFPTrackIndex
{
  int fIndex;
  short fPdg;
  
  static bool Compare(const KFPTrackIndex& a, const KFPTrackIndex& b)
  {
    int pdg1 = a.fPdg == -1 ? 250 : a.fPdg;
    int pdg2 = b.fPdg == -1 ? 250 : b.fPdg;

    return (abs(pdg1) < abs(pdg2));
  }
};

class KFPInputData
{
 public:
   
  void *operator new(size_t size) { return _mm_malloc(size, sizeof(float_v)); }
  void *operator new[](size_t size) { return _mm_malloc(size, sizeof(float_v)); }
  void *operator new(size_t size, void *ptr) { return ::operator new(size, ptr);}
  void *operator new[](size_t size, void *ptr) { return ::operator new(size, ptr);}
  void operator delete(void *ptr, size_t) { _mm_free(ptr); }
  void operator delete[](void *ptr, size_t) { _mm_free(ptr); }
  
  KFPInputData():fPV(0),fBz(0.f) {};
  ~KFPInputData() {};

  bool ReadDataFromFile( string prefix )
  {
    ifstream ifile(prefix.data());
    if ( !ifile.is_open() ) return 0;
    int nSets;
    ifile >> fBz;
    ifile >> nSets;
    for(int iSet=0; iSet<nSets; iSet++)
    {
      int nTracks = 0;
      ifile >> nTracks;
      fTracks[iSet].Resize(nTracks);
      
      for(int iP=0; iP<6; iP++)
      {
        float value;
        for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
        {
          ifile >> value;
          fTracks[iSet].SetParameter(value, iP, iTr);
        }
      }

      for(int iC=0; iC<21; iC++)
      {
        float value;
        for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
        {
          ifile >> value;
          fTracks[iSet].SetCovariance(value, iC, iTr);
        }
      }

      int tmpInt;
      for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
      {
        ifile >> tmpInt;
        fTracks[iSet].SetId(tmpInt, iTr);
      }
      
      for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
      {
        ifile >> tmpInt;
        fTracks[iSet].SetPDG(tmpInt, iTr);
      }

      for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
      {
        ifile >> tmpInt;
        fTracks[iSet].SetQ(tmpInt, iTr);
      }
      
      for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
      {
        ifile >> tmpInt;
        fTracks[iSet].SetPVIndex(tmpInt, iTr);
      }
      
      ifile >> tmpInt;
      fTracks[iSet].SetLastElectron(tmpInt);
      ifile >> tmpInt;
      fTracks[iSet].SetLastMuon    (tmpInt);
      ifile >> tmpInt;
      fTracks[iSet].SetLastPion    (tmpInt);
      ifile >> tmpInt;
      fTracks[iSet].SetLastKaon    (tmpInt);
      ifile >> tmpInt;
      fTracks[iSet].SetLastProton  (tmpInt);
    }

    int nPV;
    ifile>>nPV;
    fPV.resize(nPV);
    for(unsigned int iPV=0; iPV < fPV.size(); iPV++)
    {
      for(int iP=0; iP<3; iP++)
        ifile >> fPV[iPV].Parameter(iP);
  
      for(int iC=0; iC<6; iC++)
        ifile >> fPV[iPV].Covariance(iC);
    }
    
    ifile.close();
    return 1;
  }
  
  void SetDataToVector(int* data, int& dataSize)
  {
    dataSize = NInputSets + 1 + 1; //sizes of the track vectors and pv vector, and field
    for(int iSet=0; iSet<NInputSets; iSet++)
      dataSize += fTracks[iSet].DataSize();
    dataSize += fPV.size() * 9;
        
    for(int iSet=0; iSet<NInputSets; iSet++)
      data[iSet] = fTracks[iSet].Size();
    data[NInputSets] = fPV.size();
    
    float& field = reinterpret_cast<float&>(data[NInputSets+1]);
    field = fBz;
    
    int offset = NInputSets+2;
        
    for(int iSet=0; iSet<NInputSets; iSet++)
      fTracks[iSet].SetDataToVector(data, offset);
    
    for(int iP=0; iP<3; iP++)
    {
      for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      {
        float& tmpFloat = reinterpret_cast<float&>(data[offset + iPV]);
        tmpFloat = fPV[iPV].Parameter(iP);
      }
      offset += fPV.size();
    }
    
    for(int iC=0; iC<6; iC++)
    {
      for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      {
        float& tmpFloat = reinterpret_cast<float&>(data[offset + iPV]);
        tmpFloat = fPV[iPV].Covariance(iC);
      }
      offset += fPV.size();
    }    
  }

  void ReadDataFromVector(int* data)
  { 
    int offset = NInputSets+2;
    for(int iSet=0; iSet<NInputSets; iSet++)
    {
      fTracks[iSet].Resize(data[iSet]);
      fTracks[iSet].ReadDataFromVector(data, offset);
    }
    
    float& field = reinterpret_cast<float&>(data[NInputSets+1]);
    fBz = field;
    
    fPV.resize(data[NInputSets]);
                
    for(int iP=0; iP<3; iP++)
    {
      for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      {
        float& tmpFloat = reinterpret_cast<float&>(data[offset + iPV]);
        fPV[iPV].Parameter(iP) = tmpFloat;
      }
      offset += fPV.size();
    }
    
    for(int iC=0; iC<6; iC++)
    {
      for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      {
        float& tmpFloat = reinterpret_cast<float&>(data[offset + iPV]);
        fPV[iPV].Covariance(iC) = tmpFloat;
      }
      offset += fPV.size();
    }        
  }
  
  void Print()
  {
    for(int iSet=0; iSet<NInputSets; iSet++)
      fTracks[iSet].Print();
    std::cout << "N PV: " << fPV.size() << std::endl;
    
    std::cout << "X: ";
    for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      std::cout << fPV[iPV].X() <<" ";
    std::cout << std::endl;
        std::cout << "Y: ";
    for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      std::cout << fPV[iPV].Y() <<" ";
    std::cout << std::endl;
    std::cout << "Z: ";
    for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      std::cout << fPV[iPV].Z() <<" ";
    std::cout << std::endl;

    std::cout << "Cov matrix: " << std::endl;
    for(int iC=0; iC<6; iC++)
    {
      std::cout << "  iC " << iC << ":  ";
      for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
        std::cout << fPV[iPV].Covariance(iC) <<" ";
      std::cout << std::endl;
    }
    
    std::cout << "Field: " << fBz << std::endl;
  }
  
  KFPTrackVector* GetTracks()  { return fTracks; }
  float GetBz() const { return fBz; }
  const std::vector<KFParticle>& GetPV() const { return fPV; }

  const KFPInputData& operator = (const KFPInputData& data)
  {
    for(int i=0; i<NInputSets; i++)
      fTracks[i] = data.fTracks[i];
    fPV = data.fPV;
    fBz = data.fBz;
    
    return *this;
  }
  KFPInputData(const KFPInputData& data):fPV(0),fBz(0.f)
  {
    for(int i=0; i<NInputSets; i++)
      fTracks[i] = data.fTracks[i];
    fPV = data.fPV;
    fBz = data.fBz;
  }
  
 protected:
  KFPTrackVector fTracks[NInputSets]__attribute__((aligned(sizeof(float_v)))); //0 - pos sec, 1 - neg sec, 2 - pos prim, 3 - neg prim
  std::vector<KFParticle> fPV;
  float fBz;
} __attribute__((aligned(sizeof(float_v))));

struct KFPInputDataArray{
  KFPInputDataArray():fInput(0){};
  ~KFPInputDataArray() { if(fInput) delete [] fInput; }

  KFPInputData *fInput;
  
 private:
   const KFPInputDataArray& operator = (const KFPInputDataArray&);
   KFPInputDataArray(const KFPInputDataArray&);
};


struct KFPLinkedList
{
  void *operator new(size_t size) { return _mm_malloc(size, sizeof(float_v)); }
  void *operator new[](size_t size) { return _mm_malloc(size, sizeof(float_v)); }
  void *operator new(size_t size, void *ptr) { return ::operator new(size, ptr);}
  void *operator new[](size_t size, void *ptr) { return ::operator new(size, ptr);}
  void operator delete(void *ptr, size_t) { _mm_free(ptr); }
  void operator delete[](void *ptr, size_t) { _mm_free(ptr); }
  
  KFPInputData data __attribute__((aligned(sizeof(float_v))));
  KFPLinkedList* next;
} __attribute__((aligned(sizeof(float_v))));

#endif
