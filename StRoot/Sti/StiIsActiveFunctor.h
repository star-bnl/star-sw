/**
 * @file StiIsActiveFunctor.h
 * @class StiIsActiveFunctor
 * @brief function object for determine a detector's active regions
 *
 * Returns whether or not a given detector is active (capable of providing
 * hit information) as a function of local z and y.  Local x is not
 * required because the detector is considered a surface, not a solid.
 *
 * @author Ben Norman, Kent State University
 * @date March 2002
 */

#ifndef STI_IS_ACTIVE_FUNCTOR
#define STI_IS_ACTIVE_FUNCTOR
#include "Rtypes.h"
class StiIsActiveFunctor
{
 public:
  
  StiIsActiveFunctor(Bool_t active=kTRUE,Bool_t editable=kTRUE);
  virtual ~StiIsActiveFunctor();
  virtual Bool_t isActive() const;
  virtual void setIsActive(Bool_t value);
  virtual Bool_t isEditable() const;
  virtual void setIsEditable(Bool_t value);
  virtual Bool_t operator()(Double_t dYlocal, Double_t dZlocal) const;

 protected: 
  Bool_t _active;
  Bool_t _editable;
};

#endif // ifndef STI_IS_ACTIVE_FUNCTOR

