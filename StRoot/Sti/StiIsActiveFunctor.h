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

struct StiIsActiveFunctor{
    StiIsActiveFunctor();
    virtual ~StiIsActiveFunctor();
    virtual bool operator()(double dYlocal, double dZlocal) = 0;
};

#endif // ifndef STI_IS_ACTIVE_FUNCTOR
