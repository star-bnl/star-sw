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
  ~StEEmcStrip(){ /* nada */ };

  /// Sets the sector for this SMD strip
  void sector(Int_t s);
  /// Sets the plane for this SMD strip, 0=U, 1=V
  void plane(Int_t p);
  /// Sets the index for this SMD strip, 0..287
  void index(Int_t i);

  /// Returns the sector containing this strip
  Int_t sector();
  /// Returns the plane containing this strip
  Int_t plane();
  /// Returns the index containing this strip
  Int_t index();

  /// Prints information about strip
  void print(); 

  /// A strip is "less than" another strip if its energy is less
  Bool_t operator<( const StEEmcStrip &other) const { return (this->energy() < other.energy()); } 
  /// A strip is "equal to" another strip if they have the same StEEmcElement::mIndex
  Bool_t operator==( const StEEmcStrip &other) const { return (this->mIndex == other.mIndex); }

 private:
 protected:

  /// Sector containing this strip
  Int_t mSector;
  /// Plane containing this strip (0=U,1=V)
  Int_t mPlane;
  /// Index of this strip [0,288)
  Int_t mIndex;

  /// Makes class available to root
  ClassDef(StEEmcStrip,1);

};

inline void StEEmcStrip::sector(Int_t s){ mSector=s; }
inline void StEEmcStrip::plane(Int_t p){ mPlane=p; }
//inline void StEEmcStrip::index(Int_t i){ mIndex=i; }

inline Int_t StEEmcStrip::sector(){ return mSector; }
inline Int_t StEEmcStrip::plane(){ return mPlane; }
inline Int_t StEEmcStrip::index(){ return mIndex; }

typedef std::vector<StEEmcStrip> StEEmcStripVec_t;

#endif
