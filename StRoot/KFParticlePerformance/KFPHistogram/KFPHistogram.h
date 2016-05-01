//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef KFPHISTOGRAM
#define KFPHISTOGRAM

#include "KFPHistogramSet.h"
#include "KFPartEfficiencies.h"
#include "KFParticleTopoReconstructor.h"
#include <map>
#include <iostream>
#include <fstream>

class KFPHistogram
{  
 public:
  KFPHistogram(): fPdgToIndex(), fOutFileName("KFPHistograms.txt"), fMemory(0)
  {
    KFPartEfficiencies partEff;
    fPdgToIndex = partEff.GetPdgToIndexMap();
    
    int dataSize = 0;
    for(int iParticle=0; iParticle<KFPartEfficiencies::nParticles; iParticle++)
    {
      fKFPHistogramSet[iParticle] = KFPHistogramSet(iParticle);
      dataSize += fKFPHistogramSet[iParticle].DataSize();
    }
    
    fMemory = new int[dataSize];
    std::memset(fMemory, 0, dataSize);
    
    int* pointer = fMemory;
    for(int iParticle=0; iParticle<KFPartEfficiencies::nParticles; iParticle++)
    {
      fKFPHistogramSet[iParticle].SetHistogramMemory(pointer);
      pointer += fKFPHistogramSet[iParticle].DataSize();
    }
  }
  ~KFPHistogram() { if(fMemory) delete [] fMemory; }
  
  void SetOutFileName(std::string name) { fOutFileName = name; }
  
  inline void Fill(const KFParticle& particle)
  {
    std::map<int, int>::iterator it;
    it=fPdgToIndex.find(particle.GetPDG());
    if(it != fPdgToIndex.end())
      fKFPHistogramSet[it->second].Fill(particle);
  }
  
  inline void Fill(const KFParticleTopoReconstructor& topoReconstructor)
  {
    for(unsigned int iParticle=0; iParticle<topoReconstructor.GetParticles().size(); iParticle++)
      Fill(topoReconstructor.GetParticles()[iParticle]);
  }
    
  KFPHistogramSet GetHistogramSet(int iSet)   const { return fKFPHistogramSet[iSet]; }
  KFPHistogram1D  GetHistogram(int iSet, int iHistogram) const { return fKFPHistogramSet[iSet].GetHistogram1D(iHistogram); }
  
  friend std::fstream & operator<<(std::fstream &strm, KFPHistogram &histograms)
  {
    for(int iParticle=0; iParticle<KFPartEfficiencies::nParticles; iParticle++ )
    {
      strm << iParticle << std::endl;
      const int& nHistograms = histograms.fKFPHistogramSet[iParticle].GetNHisto1D();
      for(int iHistogram = 0; iHistogram<nHistograms; iHistogram++)
      {
        const KFPHistogram1D& histogram = histograms.fKFPHistogramSet[iParticle].GetHistogram1D(iHistogram);
        strm << histogram.Name() << " " << histogram.MinBin() << " " << histogram.MaxBin() << " " << histogram.NBins() << std::endl;
        for(int iBin=0; iBin<histogram.NBins()+2; iBin++)
          strm << histogram.GetHistogram()[iBin] << " ";
        strm << std::endl;
      }
    }

    return strm;
  }
  
  void Save() 
  {
    std::fstream file(fOutFileName.data(),std::fstream::out);
    file << (*this);
    file.close();
  }
  
  bool FillFromFile( std::string prefix )
  {
    std::fstream ifile(prefix.data(),std::fstream::in);
    if ( !ifile.is_open() ) return 0;

    int iSet = 0;
    
    for(int iParticle=0; iParticle<KFPartEfficiencies::nParticles; iParticle++ )
    {
      ifile >> iSet;
      const int& nHistograms = fKFPHistogramSet[iParticle].GetNHisto1D();
      for(int iHistogram = 0; iHistogram<nHistograms; iHistogram++)
      {
        std::string name;
        float minBin = 0.f, maxBin = 0.f;
        int nBins = 0;
        ifile >> name >> minBin >> maxBin >> nBins;
        if(nBins  != fKFPHistogramSet[iParticle].GetHistogram1D(iHistogram).NBins()  || 
           minBin != fKFPHistogramSet[iParticle].GetHistogram1D(iHistogram).MinBin() || 
           maxBin != fKFPHistogramSet[iParticle].GetHistogram1D(iHistogram).MaxBin() ) 
        {
          std::cout << "Fatal error: size of the histograms is not in an agreement with the current version." << std::endl;
          exit(1);
        }
        
        int binContent = 0;
        for(int iBin=0; iBin<nBins+2; iBin++)
        {
          ifile >> binContent;
          fKFPHistogramSet[iParticle].SetHisto1DBinContent(iHistogram, iBin, binContent);
        }
      }
    }
    
    ifile.close();
    return 1;
  }
  
  inline void operator += ( const KFPHistogram &h )
  {
    for(int i=0; i<KFPartEfficiencies::nParticles; i++)
      fKFPHistogramSet[i] += h.fKFPHistogramSet[i];
  }
  
 private:
  std::map<int, int> fPdgToIndex;
  std::string fOutFileName;
  KFPHistogramSet fKFPHistogramSet[KFPartEfficiencies::nParticles];
  int* fMemory;
  
  KFPHistogram(const KFPHistogram&);
  KFPHistogram& operator=(const KFPHistogram&);
};

#endif
