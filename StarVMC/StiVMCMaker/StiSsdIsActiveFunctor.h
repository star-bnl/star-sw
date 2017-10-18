// $Id: StiSsdIsActiveFunctor.h,v 2.2 2009/08/02 19:05:36 fisyak Exp $
// 
// $Log: StiSsdIsActiveFunctor.h,v $
// Revision 2.2  2009/08/02 19:05:36  fisyak
// Add reference track
//
// Revision 2.1  2009/04/29 14:36:34  fisyak
// Freeze 0-th version of VMC base reconstruction
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

#include "StiVMC/StiIsActiveFunctor.h"

struct StiSsdIsActiveFunctor : public StiIsActiveFunctor
{
    StiSsdIsActiveFunctor();
    virtual ~StiSsdIsActiveFunctor();
    virtual bool operator()(double dYlocal, double dZlocal) const;
};

#endif // defined STI_SSD_IS_ACTIVE_FUNCTOR
