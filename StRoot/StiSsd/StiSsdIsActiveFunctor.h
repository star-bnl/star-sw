// $Id: StiSsdIsActiveFunctor.h,v 1.2.10.1 2016/06/03 15:49:02 smirnovd Exp $
// 
// $Log: StiSsdIsActiveFunctor.h,v $
// Revision 1.2.10.1  2016/06/03 15:49:02  smirnovd
// Revert "Squashed commit of the following:"
//
// This reverts commit b0c5699a781ed8e5724e065390d3870af5de5b7c.
//
// Revision 1.2  2005/06/21 15:31:48  lmartin
// CVS tags added
//
/**
 * file StiSsdIsActiveFunctor.h
 * class StiSsdIsActiveFunctor
 * brief function object for determine a SSD ladder's active regions
 */

#ifndef STI_SSD_IS_ACTIVE_FUNCTOR
#define STI_SSD_IS_ACTIVE_FUNCTOR

#include "Sti/StiIsActiveFunctor.h"

struct StiSsdIsActiveFunctor : public StiIsActiveFunctor
{
    StiSsdIsActiveFunctor();
    virtual ~StiSsdIsActiveFunctor();
    virtual bool operator()(double dYlocal, double dZlocal) const;
};

#endif // defined STI_SSD_IS_ACTIVE_FUNCTOR
