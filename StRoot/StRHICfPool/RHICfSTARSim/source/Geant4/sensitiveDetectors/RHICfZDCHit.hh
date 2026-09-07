#ifndef RHICFZDCHIT_H
#define RHICFZDCHIT_H 1

#include <vector>
#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4ios.hh"

class RHICfZDCHit: public G4VHit
{
public:
  RHICfZDCHit();
  RHICfZDCHit(G4int amodule, G4int anphoton, G4double aedep);
  virtual ~RHICfZDCHit();

  /// Copy constructor & assignment operator
  RHICfZDCHit(const RHICfZDCHit& right);
  const RHICfZDCHit& operator=(const RHICfZDCHit& right);
  G4int operator==(const RHICfZDCHit& right) const;
  
  /// new/delete operators
  void* operator new(size_t);
  void operator delete(void* aHit);
  
  /// set/get functions
  void SetModule(G4int amodule) {module=amodule;}
  G4int GetModule() const {return module;}
  void SetNphoton(G4int anphoton) {nphoton=anphoton;}
  G4int GetNphoton() const {return nphoton;}
  void SetEdep(G4double aedep) {edep=aedep;}
  G4double GetEdep() const {return edep;}

  /// Methods
  virtual void Draw();
  virtual void Print();

private:
  G4int module;
  G4int nphoton;
  G4double edep;
};

/////////
/// inline functions
inline RHICfZDCHit::RHICfZDCHit(const RHICfZDCHit& right): G4VHit()
{
  module=right.module;
  nphoton=right.nphoton;
  edep=right.edep;
}

inline const RHICfZDCHit& RHICfZDCHit::operator=(const RHICfZDCHit& right)
{
  module=right.module;
  nphoton=right.nphoton;
  edep=right.edep;
  return *this;
}

inline G4int RHICfZDCHit::operator==(const RHICfZDCHit& right) const 
{
   return (this==&right) ? 1 : 0; 
}

typedef G4THitsCollection<RHICfZDCHit> ZDCHitsCollection;
extern G4Allocator<RHICfZDCHit> RHICfZDCHitAllocator; 

inline void* RHICfZDCHit::operator new(size_t)
{
  void* aHit = (void*)RHICfZDCHitAllocator.MallocSingle();
  return aHit;
}

inline void RHICfZDCHit::operator delete(void* aHit)
{
  RHICfZDCHitAllocator.FreeSingle((RHICfZDCHit*) aHit);
}

#endif
