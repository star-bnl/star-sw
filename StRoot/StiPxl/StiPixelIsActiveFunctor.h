/**
 * @file StiPxlIsActiveFunctor.h
 * @class StiPxlIsActiveFunctor
 * @brief function object for determine a Pixel padrow's active regions
 *
 * @author Ben Norman, Kent State University
 * @date March 2002
 */

#ifndef STI_Pixel_IS_ACTIVE_FUNCTOR
#define STI_Pixel_IS_ACTIVE_FUNCTOR

#include "Sti/StiIsActiveFunctor.h"

class StiPxlIsActiveFunctor : public StiIsActiveFunctor
{
public:
   StiPxlIsActiveFunctor();
   virtual ~StiPxlIsActiveFunctor();
   virtual bool operator()(double dYlocal, double dZlocal) const;

protected:
};

#endif // ifndef STI_Pixel_IS_ACTIVE_FUNCTOR
