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

#ifndef KFMCVERTEX_H
#define KFMCVERTEX_H

#include <iostream>
#include <vector>
using std::ostream;
using std::istream;


/**
 * @class KFMCVertex
 * store MC track information for Performance
 */
class KFMCVertex
{
 public:
  KFMCVertex();

  float Par( int i )  const { return fPar[i]; }

  float X()           const { return fPar[0]; }
  float Y()           const { return fPar[1]; }
  float Z()           const { return fPar[2]; }
  
  const float* GetPar()      const { return fPar; }
  
  void SetPar( int i, float v )   { fPar[i] = v; }
  
  void SetX( float v )            { fPar[0] = v; }
  void SetY( float v )            { fPar[1] = v; }
  void SetZ( float v )            { fPar[2] = v; }

  int NDaughterTracks() const { return fDaughterTracks.size(); }
  int NReconstructedDaughterTracks() const { return fNReconstructedDaughters; }
  void AddDaughterTrack( int iTr ) { fDaughterTracks.push_back(iTr); }
  int DaughterTrack( int iTr ) const 
  { 
    if(iTr >= NDaughterTracks())
    {
      std::cout << "ERROR!!!! MC PV contains only " << NDaughterTracks() << " tracks" << std::endl; 
      return -1;
    }
    return fDaughterTracks[iTr];
  }

  bool IsMCReconstructable() const { return fIsMCReconstructable; }
  bool IsReconstructable() const { return fIsReconstructable; }
  bool IsReconstructed()  const { return fIsReconstructed;   }
  
  void SetReconstructable() { fIsReconstructable = 1; }
  void SetUnReconstructable() { fIsReconstructable = 0; }
  
  void SetMCReconstructable() { fIsMCReconstructable = 1; }
  void SetMCUnReconstructable() { fIsMCReconstructable = 0; }
  
  void SetReconstructed() { fIsReconstructed = 1; }
  void SetUnReconstructed() { fIsReconstructed = 0; }

  void SetNReconstructedDaughters(int n) { fNReconstructedDaughters = n; }
  
  bool IsTriggerPV() const { return fIsTriggerPV; }
  void SetTriggerPV() { fIsTriggerPV = 1; }
  
  friend ostream& operator<<(ostream& out, const KFMCVertex &a);
  friend istream& operator>>(istream& in, KFMCVertex &a);

 protected:

  float fPar[3];         //* x,y,z
  std::vector<int> fDaughterTracks;
  bool fIsReconstructable;
  bool fIsMCReconstructable;
  bool fIsReconstructed;
  int fNReconstructedDaughters;
  bool fIsTriggerPV;
};

#endif
