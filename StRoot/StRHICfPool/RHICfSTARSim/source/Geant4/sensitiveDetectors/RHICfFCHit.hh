#ifndef RHICFFCHIT_H
#define RHICFFCHIT_H 1

#include <vector>
#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4ios.hh"

class RHICfFCHit: public G4VHit
{
public:
  RHICfFCHit();
  RHICfFCHit(G4int atower, G4double aedep);
  virtual ~RHICfFCHit();

  /// Copy constructor & assignment operator
  RHICfFCHit(const RHICfFCHit& right);
  const RHICfFCHit& operator=(const RHICfFCHit& right);
  G4int operator==(const RHICfFCHit& right) const;
  
  /// new/delete operators
  void* operator new(size_t);
  void operator delete(void* aHit);
  
  /// set/get functions
  void SetTower(G4int atower) {tower=atower;}
  G4int GetTower() const {return tower;}
  void SetEdep(G4double aedep) {edep=aedep;}
  G4double GetEdep() const {return edep;}

  /// Methods
  virtual void Draw();
  virtual void Print();

private:
  G4int tower;
  G4double edep;
};

/////////
/// inline functions
inline RHICfFCHit::RHICfFCHit(const RHICfFCHit& right): G4VHit()
{
  tower=right.tower;
  edep=right.edep;
}

inline const RHICfFCHit& RHICfFCHit::operator=(const RHICfFCHit& right)
{
  tower=right.tower;
  edep=right.edep;
  return *this;
}

inline G4int RHICfFCHit::operator==(const RHICfFCHit& right) const 
{
   return (this==&right) ? 1 : 0; 
}

typedef G4THitsCollection<RHICfFCHit> FCHitsCollection;
extern G4Allocator<RHICfFCHit> RHICfFCHitAllocator; 

inline void* RHICfFCHit::operator new(size_t)
{
  void* aHit = (void*)RHICfFCHitAllocator.MallocSingle();
  return aHit;
}

inline void RHICfFCHit::operator delete(void* aHit)
{
  RHICfFCHitAllocator.FreeSingle((RHICfFCHit*) aHit);
}

#endif
