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

struct StiNeverActiveFunctor : public StiIsActiveFunctor{
    StiNeverActiveFunctor();
    virtual ~StiNeverActiveFunctor();
    virtual bool operator()(double dYlocal, double dZlocal);
};

#endif // defined STI_NEVER_ACTIVE_FUNCTOR
