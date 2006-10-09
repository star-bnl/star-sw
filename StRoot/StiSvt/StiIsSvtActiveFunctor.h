/**
 * @file StiIsSvtActiveFunctor.h
 * @class StiIsSvtActiveFunctor
 * @brief function object for determine a detector's active regions
 *
 * Returns whether or not a given detector is active (capable of providing
 * hit information) as a function of local z and y.  Local x is not
 * required because the detector is considered a surface, not a solid.
 *
 * @author Ben Norman, Kent State University
 * @date March 2002
 */

#ifndef StiIsSvtActiveFunctor_h
#define StiIsSvtActiveFunctor_h
#include "Rtypes.h"
#include "Sti/StiIsActiveFunctor.h"
class StiIsSvtActiveFunctor : public StiIsActiveFunctor {
 public:
  StiIsSvtActiveFunctor(Bool_t active=kTRUE,Bool_t editable=kTRUE) : 
  StiIsActiveFunctor(active, editable), _Barrel(0), _Ladder(0),_nWafers(0), _dY(0), _dZ(0) {}
  StiIsSvtActiveFunctor(Int_t barrel, Int_t ladder, Int_t nWafers, Double_t dY = 3.0, Double_t dZ = 3.0,  Bool_t active=kTRUE,Bool_t editable=kTRUE) : 
  StiIsActiveFunctor(active, editable), _Barrel(barrel), _Ladder(ladder),_nWafers(nWafers), _dY(dY), _dZ(dZ) {}
  virtual ~StiIsSvtActiveFunctor() {}
  virtual Bool_t operator()(Double_t dYlocal, Double_t dZlocal) const;
  
 protected: 
  Int_t    _Barrel;
  Int_t    _Ladder;
  Int_t    _nWafers;
  Double_t _dY;
  Double_t _dZ;
};

#endif 

