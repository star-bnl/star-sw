#ifndef StPicoMtdPidTraits_hh
#define StPicoMtdPidTraits_hh

#include "TObject.h"
class StMuMtdPidTraits;
class StMuMtdHit;

class StPicoMtdPidTraits : public TObject
{
 public:
  StPicoMtdPidTraits();
  StPicoMtdPidTraits(const StMuMtdHit *, const StMuMtdPidTraits*, const Int_t);
  ~StPicoMtdPidTraits();
  virtual void Print(const Char_t *option = "") const; 
    
  // Matching information
  Int_t    trackIndex()        const { return (Int_t)mTrackIndex;             }
  Int_t    backleg()           const { return (Int_t)mMtdHitChan/60 + 1;      }
  Int_t    module()            const { return ((Int_t)mMtdHitChan%60)/12 + 1; }
  Int_t    cell()              const { return (Int_t)mMtdHitChan%12;          }
  Int_t    matchFlag()         const { return (Int_t)mMatchFlag;              }
  Float_t  deltaY()            const { return mDeltaY;                        }
  Float_t  deltaZ()            const { return mDeltaZ;                        } 
  Float_t  deltaTimeOfFlight() const { return mDeltaTimeOfFlight;             }
  Float_t  beta()              const { return mBeta;                          }
   
  // Setting functions
  void    setTrackIndex(Int_t index)      { mTrackIndex = (Short_t) index; }
  void    setMatchFlag(Char_t flag)       { mMatchFlag = flag;             } 
  void    setDeltaY(Float_t dy)           { mDeltaY = dy;                  }
  void    setDeltaZ(Float_t dz)           { mDeltaZ = dz;                  }
  void    setDeltaTimeOfFlight(Float_t t) { mDeltaTimeOfFlight = t;        }
  void    setBeta(Float_t beta)           { mBeta = beta;                  }
  
private:    
  Short_t   mMtdHitChan;            // (backleg-1) * 60 + (module-1) * 12 + cell
  Short_t   mTrackIndex;            // Index to the associated track in the event
  Char_t    mMatchFlag;             // Matching flag indicating multiple matches
  Float_t   mDeltaY;                // DeltaY between matched track-hit pair
  Float_t   mDeltaZ;                // DeltaZ between matched track-hit pair
  Float_t   mDeltaTimeOfFlight;     // Difference between measured and expected time-of-flight
  Float_t   mBeta;                  // Beta of matched tracks
  
  ClassDef(StPicoMtdPidTraits,1)
};

#endif
