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
  KFPInputData():fPV(0),fCluster(0),fBz(0.f) {};
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
      fTracks[iSet].SetLastMoun    (tmpInt);
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
  
  void PrintTracks()
  {
    for(int iTrType=0; iTrType<4; iTrType++)
      for(int i=0; i<fTracks[iTrType].Size(); i++)
        std::cout << fTracks[iTrType].X()[i] << " " <<  fTracks[iTrType].Y()[i] << " " << fTracks[iTrType].Z()[i] << " " <<  std::endl;
  }
  
  KFPTrackVector* GetTracks()  { return fTracks; }
  float GetBz() const { return fBz; }
  const std::vector<KFParticle>& GetPV() const { return fPV; }
  const std::vector<short int>& GetCluster() const { return fCluster; }

  const KFPInputData& operator = (const KFPInputData& data)
  {
    for(int i=0; i<4; i++)
      fTracks[i] = data.fTracks[i];
    fPV = data.fPV;
    fCluster = data.fCluster;
    fBz = data.fBz;
    
    return *this;
  }
  KFPInputData(const KFPInputData& data):fPV(0),fCluster(0),fBz(0.f)
  {
    for(int i=0; i<4; i++)
      fTracks[i] = data.fTracks[i];
    fPV = data.fPV;
    fCluster = data.fCluster;
    fBz = data.fBz;
  }
  
 protected:
  KFPTrackVector fTracks[4]; //0 - pos sec, 1 - neg sec, 2 - pos prim, 3 - neg prim
  std::vector<KFParticle> fPV;
  std::vector<short int> fCluster;
  float fBz;
};

struct KFPInputDataArray{
  KFPInputDataArray():fInput(0){};
  ~KFPInputDataArray() { if(fInput) delete [] fInput; }

  KFPInputData *fInput;
  
 private:
   const KFPInputDataArray& operator = (const KFPInputDataArray&);
   KFPInputDataArray(const KFPInputDataArray&);
};

#endif
