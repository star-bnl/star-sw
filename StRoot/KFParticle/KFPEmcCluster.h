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

#ifndef KFPEmcCluster_H
#define KFPEmcCluster_H

#include <vector>

#include <iostream>
#include "KFParticleDef.h"

class KFPEmcCluster
{
 public:
  KFPEmcCluster():fId() { }
  ~KFPEmcCluster() { }

  int Size() const { return fP[0].size(); }
  
  void Resize(const int n);
  void Set(KFPEmcCluster& v, int vSize, int offset);
  void SetTracks(const KFPEmcCluster& track, const kfvector_uint& trackIndex, const int nIndexes);
  
  const kfvector_float& X()  const { return fP[0]; }
  const kfvector_float& Y()  const { return fP[1]; }
  const kfvector_float& Z()  const { return fP[2]; }
  const kfvector_float& E() const { return fP[3]; }

  const kfvector_float& Parameter(const int i)  const { return fP[i]; }
  const kfvector_float& Covariance(const int i)  const { return fC[i]; }
  const kfvector_int& Id()    const { return fId; }

  //modifiers 
  void SetParameter (float value, int iP, int iTr) { fP[iP][iTr] = value; }
  void SetCovariance(float value, int iC, int iTr) { fC[iC][iTr] = value; }

  void SetParameter (const float_v& value, int iP, int iTr);  
  void SetCovariance(const float_v& value, int iC, int iTr);

  void SetId        (int value, int iTr)           { fId[iTr] = value; }
  
  void PrintTrack(int n);
  void PrintTracks();
  
  const KFPEmcCluster& operator = (const KFPEmcCluster& clusters)
  {
    const int localSize = clusters.Size();
    
    for(int i=0; i<4; i++)
    {
      fP[i].resize(localSize);
      for(int n=0; n<localSize; n++)
        fP[i][n] = clusters.fP[i][n];
    }

    for(int i=0; i<10; i++)
    {
      fC[i].resize(localSize);
      for(int n=0; n<localSize; n++)
        fC[i][n] = clusters.fC[i][n];
    }
    
    fId.resize(localSize);
    for(int n=0; n<localSize; n++)
      fId[n] = clusters.fId[n];
    
    return *this;
  }
  
 private:  
  kfvector_float fP[4];  //coordinates of the cluster : x, y, z, E
  kfvector_float fC[10];  //Covariance matrix of the track parameters

  kfvector_int fId;
};

#endif
