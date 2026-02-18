#ifndef RHICFGSOBARHIT_H
#define RHICFGSOBARHIT_H 1

#include <vector>
#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4ios.hh"

class RHICfGSObarHit: public G4VHit
{
public:
  RHICfGSObarHit();
  RHICfGSObarHit(G4int atower, G4int abelt, G4int axy, G4int abar, G4double aedep_truth, G4double aedep);
  virtual ~RHICfGSObarHit();

  /// Copy constructor & assignment operator
  RHICfGSObarHit(const RHICfGSObarHit& right);
  const RHICfGSObarHit& operator=(const RHICfGSObarHit& right);
  G4int operator==(const RHICfGSObarHit& right) const;
  
  /// new/delete operators
  void* operator new(size_t);
  void operator delete(void* aHit);
  
  /// set/get functions
  void SetTower(G4int atower) {tower=atower;}
  G4int GetTower() const {return tower;}
  void SetBelt(G4int abelt) {belt=abelt;}
  G4int GetBelt() const {return belt;}
  void SetXY(G4int axy) {xy=axy;}
  G4int GetXY() const {return xy;}
  void SetBar(G4int abar) {bar=abar;}
  G4int GetBar() const {return bar;}
  void SetEdep_truth(G4double aedep_truth) {edep_truth=aedep_truth;}
  G4double GetEdep_truth() const {return edep_truth;}
  void SetEdep(G4double aedep) {edep=aedep;}
  G4double GetEdep() const {return edep;}
  
  /// Methods
  virtual void Draw();
  virtual void Print();

private:
  G4int tower;
  G4int belt;
  G4int xy;
  G4int bar;
  G4double edep_truth;
  G4double edep;
};

/////////
/// inline functions
inline RHICfGSObarHit::RHICfGSObarHit(const RHICfGSObarHit& right): G4VHit()
{
  tower=right.tower;
  belt=right.belt;
  xy=right.xy;
  bar=right.bar;
  edep=right.edep;
  edep_truth=right.edep_truth;
}

inline const RHICfGSObarHit& RHICfGSObarHit::operator=(const RHICfGSObarHit& right)
{
  tower=right.tower;
  belt=right.belt;
  xy=right.xy;
  bar=right.bar;
  edep=right.edep;
  edep_truth=right.edep_truth;
  return *this;
}

inline G4int RHICfGSObarHit::operator==(const RHICfGSObarHit& right) const 
{
   return (this==&right) ? 1 : 0; 
}

typedef G4THitsCollection<RHICfGSObarHit> GSObarHitsCollection;
extern G4Allocator<RHICfGSObarHit> RHICfGSObarHitAllocator; 

inline void* RHICfGSObarHit::operator new(size_t)
{
  void* aHit = (void*)RHICfGSObarHitAllocator.MallocSingle();
  return aHit;
}

inline void RHICfGSObarHit::operator delete(void* aHit)
{
  RHICfGSObarHitAllocator.FreeSingle((RHICfGSObarHit*) aHit);
}

#endif
