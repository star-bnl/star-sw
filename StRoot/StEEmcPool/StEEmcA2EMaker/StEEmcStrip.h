#ifndef __StEEmcStrip_h__
#define __StEEmcStrip_h__

#include "StEEmcElement.h"
#include "TString.h" 
#include <vector>

class StEEmcStrip : public StEEmcElement {
public:

  /// Constructor
  StEEmcStrip();
  /// Destructor
  virtual ~StEEmcStrip(){ /* nada */ };

  /// Sets the sector for this SMD strip
  void sector(Int_t s) { mSector=s; }
  /// Sets the plane for this SMD strip, 0=U, 1=V
  void plane(Int_t p) { mPlane=p; }
  /// Sets the index for this SMD strip, 0..287
  void index(Int_t i);

  /// Returns the sector containing this strip
  Int_t sector()const { return mSector; }
  /// Returns the plane containing this strip
  Int_t plane()const { return mPlane; }
  /// Returns the index containing this strip
  Int_t index()const { return mIndex; }

  /// Prints information about strip
  void print() const; 

  /// A strip is "less than" another strip if its energy is less
  Bool_t operator<( const StEEmcStrip &other) const { return (this->energy() < other.energy()); } 
  /// A strip is "equal to" another strip if they have the same StEEmcElement::mIndex
  Bool_t operator==( const StEEmcStrip &other) const { return (this->mIndex == other.mIndex); }

protected:
  Int_t mSector; // Sector containing this strip
  Int_t mPlane;  // Plane containing this strip (0=U,1=V)
  Int_t mIndex;  // Index of this strip [0,288)

  ClassDef(StEEmcStrip,1);
};

typedef std::vector<StEEmcStrip> StEEmcStripVec_t;

#endif
