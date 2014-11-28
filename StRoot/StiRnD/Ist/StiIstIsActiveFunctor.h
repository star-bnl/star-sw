/**
 * @file StiIstIsActiveFunctor.h
 * @class StiIstIsActiveFunctor
 * @brief function object for determine a Ist padrow's active regions
 *
 * @author Ben Norman, Kent State University
 * @date March 2002
 */

#ifndef STI_Ist_IS_ACTIVE_FUNCTOR
#define STI_Ist_IS_ACTIVE_FUNCTOR

#include "Sti/StiIsActiveFunctor.h"

class StiIstIsActiveFunctor : public StiIsActiveFunctor{
public:
    StiIstIsActiveFunctor();
    virtual ~StiIstIsActiveFunctor();
    virtual bool operator()(double dYlocal, double dZlocal) const;
    
protected:
};

#endif // ifndef STI_Ist_IS_ACTIVE_FUNCTOR
