/**
 * @file StiPixelIsActiveFunctor.h
 * @class StiPixelIsActiveFunctor
 * @brief function object for determine a Pixel padrow's active regions
 *
 * @author Ben Norman, Kent State University
 * @date March 2002
 */

#ifndef STI_Pixel_IS_ACTIVE_FUNCTOR
#define STI_Pixel_IS_ACTIVE_FUNCTOR

#include "Sti/StiIsActiveFunctor.h"

class StiPixelIsActiveFunctor : public StiIsActiveFunctor{
public:
    StiPixelIsActiveFunctor();
    virtual ~StiPixelIsActiveFunctor();
    virtual bool operator()(double dYlocal, double dZlocal) const;
    
protected:
};

#endif // ifndef STI_Pixel_IS_ACTIVE_FUNCTOR
