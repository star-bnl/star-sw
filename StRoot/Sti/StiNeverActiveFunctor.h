/**
 * @file StiNeverActiveFunctor.h
 * @class StiNeverActiveFunctor
 * @brief function object which always returns false
 *
 * @author Ben Norman, Kent State University
 * @date March 2002
 */

#ifndef STI_NEVER_ACTIVE_FUNCTOR
#define STI_NEVER_ACTIVE_FUNCTOR

#include "StiIsActiveFunctor.h"

///Class implements an  object which is never active.
class StiNeverActiveFunctor : public StiIsActiveFunctor
{
public:
  StiNeverActiveFunctor();
  virtual ~StiNeverActiveFunctor();
};

#endif // defined STI_NEVER_ACTIVE_FUNCTOR
