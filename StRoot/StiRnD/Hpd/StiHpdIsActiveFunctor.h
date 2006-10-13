/**
 * @file StiHpdIsActiveFunctor.h
 * @class StiHpdIsActiveFunctor
 * @brief function object for determine a Ist padrow's active regions
 *
 * @author Ben Norman, Kent State University
 * @date March 2002
 */

#ifndef STI_Hpd_IS_ACTIVE_FUNCTOR
#define STI_Hpd_IS_ACTIVE_FUNCTOR

#include "Sti/StiIsActiveFunctor.h"

class StiHpdIsActiveFunctor : public StiIsActiveFunctor{
public:
    StiHpdIsActiveFunctor();
    virtual ~StiHpdIsActiveFunctor();
    virtual bool operator()(double dYlocal, double dZlocal) const;
    
protected:
};

#endif // ifndef STI_Hpd_IS_ACTIVE_FUNCTOR
