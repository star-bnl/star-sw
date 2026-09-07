#ifndef RHICFGSOPLATEHIT_H
#define RHICFGSOPLATEHIT_H 1

#include <vector>
#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4ios.hh"

class RHICfGSOplateHit: public G4VHit
{
public:
  RHICfGSOplateHit();
  RHICfGSOplateHit(G4int atower, G4int aplate, G4double aedep_truth, G4double aedep);
  virtual ~RHICfGSOplateHit();

  /// Copy constructor & assignment operator
  RHICfGSOplateHit(const RHICfGSOplateHit& right);
  const RHICfGSOplateHit& operator=(const RHICfGSOplateHit& right);
  G4int operator==(const RHICfGSOplateHit& right) const;
  
  /// new/delete operators
  void* operator new(size_t);
  void operator delete(void* aHit);
  
  /// set/get functions
  void SetTower(G4int atower) {tower=atower;}
  G4int GetTower() const {return tower;}
  void SetPlate(G4int aplate) {plate=aplate;}
  G4int GetPlate() const {return plate;}
  void SetEdep_truth(G4double aedep_truth) {edep_truth=aedep_truth;}
  G4double GetEdep_truth() const {return edep_truth;}
  void SetEdep(G4double aedep) {edep=aedep;}
  G4double GetEdep() const {return edep;}

  /// Methods
  virtual void Draw();
  virtual void Print();

private:
  G4int tower;
  G4int plate;
  G4double edep_truth;
  G4double edep;
};

/////////
/// inline functions
inline RHICfGSOplateHit::RHICfGSOplateHit(const RHICfGSOplateHit& right): G4VHit()
{
  tower=right.tower;
  plate=right.plate;
  edep_truth=right.edep_truth;
  edep=right.edep;
}

inline const RHICfGSOplateHit& RHICfGSOplateHit::operator=(const RHICfGSOplateHit& right)
{
  tower=right.tower;
  plate=right.plate;
  edep_truth=right.edep_truth;
  edep=right.edep;
  return *this;
}

inline G4int RHICfGSOplateHit::operator==(const RHICfGSOplateHit& right) const 
{
   return (this==&right) ? 1 : 0; 
}

typedef G4THitsCollection<RHICfGSOplateHit> GSOplateHitsCollection;
extern G4Allocator<RHICfGSOplateHit> RHICfGSOplateHitAllocator; 

inline void* RHICfGSOplateHit::operator new(size_t)
{
  void* aHit = (void*)RHICfGSOplateHitAllocator.MallocSingle();
  return aHit;
}

inline void RHICfGSOplateHit::operator delete(void* aHit)
{
  RHICfGSOplateHitAllocator.FreeSingle((RHICfGSOplateHit*) aHit);
}

#endif
