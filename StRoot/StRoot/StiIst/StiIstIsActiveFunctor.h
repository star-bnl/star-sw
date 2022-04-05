#ifndef StiIstIsActiveFunctor_h
#define StiIstIsActiveFunctor_h

#include "Sti/StiIsActiveFunctor.h"

/**
 * @brief function object for determine a Ist padrow's active regions
 *
 * @author Ben Norman, Kent State University
 * @date March 2002
 */
class StiIstIsActiveFunctor : public StiIsActiveFunctor
{
public:
   StiIstIsActiveFunctor();
   virtual ~StiIstIsActiveFunctor();
   virtual bool operator()(double dYlocal, double dZlocal) const;

protected:
};

#endif
