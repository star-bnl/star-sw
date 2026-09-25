#ifndef RHICFSMDHIT_H
#define RHICFSMDHIT_H 1

#include <vector>
#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4ios.hh"

class RHICfSMDHit: public G4VHit
{
public:
  RHICfSMDHit();
  RHICfSMDHit(G4int axy, G4int asmd, G4double aedep);
  virtual ~RHICfSMDHit();

  /// Copy constructor & assignment operator
  RHICfSMDHit(const RHICfSMDHit& right);
  const RHICfSMDHit& operator=(const RHICfSMDHit& right);
  G4int operator==(const RHICfSMDHit& right) const;
  
  /// new/delete operators
  void* operator new(size_t);
  void operator delete(void* aHit);
  
  /// set/get functions
  void SetXY(G4int axy) {xy=axy;}
  G4int GetXY() const {return xy;}
  void SetSMD(G4int asmd) {smd=asmd;}
  G4int GetSMD() const {return smd;}
  void SetEdep(G4double aedep) {edep=aedep;}
  G4double GetEdep() const {return edep;}

  /// Methods
  virtual void Draw();
  virtual void Print();

private:
  G4int xy;
  G4int smd;
  G4double edep;
};

/////////
/// inline functions
inline RHICfSMDHit::RHICfSMDHit(const RHICfSMDHit& right): G4VHit()
{
  xy=right.xy;
  smd=right.smd;
  edep=right.edep;
}

inline const RHICfSMDHit& RHICfSMDHit::operator=(const RHICfSMDHit& right)
{
  xy=right.xy;
  smd=right.smd;
  edep=right.edep;
  return *this;
}

inline G4int RHICfSMDHit::operator==(const RHICfSMDHit& right) const 
{
   return (this==&right) ? 1 : 0; 
}

typedef G4THitsCollection<RHICfSMDHit> SMDHitsCollection;
extern G4Allocator<RHICfSMDHit> RHICfSMDHitAllocator; 

inline void* RHICfSMDHit::operator new(size_t)
{
  void* aHit = (void*)RHICfSMDHitAllocator.MallocSingle();
  return aHit;
}

inline void RHICfSMDHit::operator delete(void* aHit)
{
  RHICfSMDHitAllocator.FreeSingle((RHICfSMDHit*) aHit);
}

#endif
