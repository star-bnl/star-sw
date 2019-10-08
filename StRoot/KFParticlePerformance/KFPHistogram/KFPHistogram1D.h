//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef KFPHISTOGRAM1D
#define KFPHISTOGRAM1D

#include <vector>
#include <string>
#include <cmath>
#include <iostream>
#include <assert.h>
class KFPHistogram1D
{
 public:
   
  KFPHistogram1D(): fHistogram(0), fSize(0), fName(""), fMinBin(0), fMaxBin(0) {};
  KFPHistogram1D(std::string name, int nBins, float minX, float maxX):
    fHistogram(0), fSize(nBins+2), fName(name), fMinBin(minX), fMaxBin(maxX) {};
  ~KFPHistogram1D() {};
  
  int* GetHistogram() const { return fHistogram; } 
  std::string Name()  const { return fName; }
  float MinBin()      const { return fMinBin; }
  float MaxBin()      const { return fMaxBin; }
  int   NBins()       const { return (fSize-2); }
  int   DataSize()    const { return fSize; }
  int   Size()        const { return fSize; }
  
  inline void SetBinContent(int iBin, int value) { fHistogram[iBin] = value; }
  inline void SetHistogramMemory(int* pointer)   { fHistogram = pointer; }
  
  void Fill(float value)
  {
    int iBin = floor(float(value - fMinBin)/float(fMaxBin - fMinBin) * float(fSize-2)) + 1;
    
    if(iBin > fSize-1)
      iBin = fSize-1;
    if(iBin < 1)
      iBin = 0;
    
    if( !(iBin==iBin) || !(std::isfinite(iBin)) ) iBin = 0;
    
    fHistogram[iBin]++;    
  }
  
  inline void operator += ( const KFPHistogram1D &h )
  {
    assert(fSize == h.fSize );
    
    for(int i=0; i<fSize; i++)
      fHistogram[i] += h.fHistogram[i];
  }
  
  KFPHistogram1D(const KFPHistogram1D& h): fHistogram(h.fHistogram), fSize(h.fSize), fName(h.fName), fMinBin(h.fMinBin), fMaxBin(h.fMaxBin)
  {
  }
  
  const KFPHistogram1D& operator=(const KFPHistogram1D& h)
  {
    fHistogram = h.fHistogram;
    fSize = h.fSize;
    fName = h.fName;
    fMinBin = h.fMinBin;
    fMaxBin = h.fMaxBin;
    
    return *this;
  }
  
 private:
  int* fHistogram;
  int fSize;
  
  std::string fName;
  float fMinBin;
  float fMaxBin;
};

#endif
