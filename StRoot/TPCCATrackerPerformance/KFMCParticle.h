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

#ifndef _KFMCParticle_h_
#define _KFMCParticle_h_

#include <vector>
using std::vector;

class KFMCParticle
{
 public:
  KFMCParticle();
  ~KFMCParticle();

  void AddDaughter( int i );
  int  NDaughters() const { return fDaughterIds.size(); };
  const vector<int>&  GetDaughterIds() const { return fDaughterIds; };
  void FindCommonMC();

  void SetPDG(int pdg) {fPDG = pdg;}
  void SetMCTrackID(int id) {fMCTrackID = id;}
  void SetMotherId(int id) {fMotherId = id;}
  
  int  GetMCTrackID()      const {return fMCTrackID;}
  int  GetMotherId()       const {return fMotherId;}
  int  GetPDG()            const {return fPDG;}
  
  bool IsReconstructable(int i) const {return fIsReconstructable[i];}
  void SetAsReconstructable(int i) { fIsReconstructable[i] = 1;}
    
  bool IsReconstructableV0(int i) const {return fIsV0[i];}
  void SetAsReconstructableV0(int i) { fIsV0[i] = 1;}
  
 private: //data
  vector<int> fDaughterIds;
  int fMCTrackID; // sim id of MC track, which corresponds to the particle
  int fMotherId;  // index in L1 array of mother particle
  int fPDG;
  
  bool fIsReconstructable[3]; //all particles ,all daughters are reconstructable, all daughters are reconstructed
  bool fIsV0[3];
  
};

#endif

