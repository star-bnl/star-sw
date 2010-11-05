/**
 * @file StiTpcIsActiveFunctor.h
 * @class StiTpcIsActiveFunctor
 * @brief function object for determine a TPC padrow's active regions
 *
 * @author Ben Norman, Kent State University
 * @author Claude Pruneau, Wayne State University
 * @date March 2002
 */

#ifndef STI_TPC_IS_ACTIVE_FUNCTOR
#define STI_TPC_IS_ACTIVE_FUNCTOR
#include "Sti/StiIsActiveFunctor.h"

///Class defines a isActiveFunctor specific to the STAR tpc.
///The isActive status depends on the livelihood of the east and 
///west side of the TPC. 
class StiTpcIsActiveFunctor : public StiIsActiveFunctor
{
 public:
  StiTpcIsActiveFunctor(Bool_t active=kTRUE, Bool_t west=kTRUE, Bool_t east=kTRUE) :
    StiIsActiveFunctor(active,kTRUE),  _eastActive(east), _westActive(west) {}
  virtual ~StiTpcIsActiveFunctor() {}
  virtual Bool_t operator()(Double_t /* dYlocal */, Double_t /* dZlocal */) const {return isActive();}
//		correct version not swiched on yet
//Bool_t operator()(Double_t /* dYlocal */, Double_t dZlocal) const 
//  {if (dZlocal<-1) return _eastActive;
//   if (dZlocal> 1) return _westActive;
//   if return  _eastActive || _westActive;}
  virtual Bool_t isActive() const {return _active && (_eastActive || _westActive);}
  virtual Bool_t isEastActive() const {return _eastActive;}
  virtual Bool_t isWestActive() const {return _westActive;}
  void setEastActive(Bool_t value) {_eastActive = value;}
  void setWestActive(Bool_t value) {_westActive = value;}

 protected:
  /// is the east half of the padrow on?
  Bool_t _eastActive;
  /// is the west half of the padrow on?
  Bool_t _westActive;
};
#endif // ifndef STI_TPC_IS_ACTIVE_FUNCTOR
