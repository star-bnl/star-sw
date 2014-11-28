//  $Id: StRandomSelector.cxx,v 1.1 2009/09/03 11:56:43 rfatemi Exp $
//
//  $Log: StRandomSelector.cxx,v $
//  Revision 1.1  2009/09/03 11:56:43  rfatemi
//  This module allows users to randomly remove members of any group in a set container
//
//

/*  StRandomSelector.cxx
 *
 *  Written by Wayne Witzke for the University of Kentucky Department of
 *  Physics and Astronomy.
 *
 *  This random selector will allow a developer to randomly pick TObjects from
 *  a TCollection.
 */

#include <iostream>
#include <cmath>
#include "StRandomSelector.h"

//  This is required so that the .cxx and .h files are tied together correctly
//  for loading into root macros.
ClassImp(StRandomSelector)

TObject * StRandomSelector::GetNextRandom()
{
    //	This is just for convenience and speed, in theory.
    double totalElem = GetTotalNumber();

    //	This is so that we can easily calculate when we're close enough to
    //	zero in the calculations below.  It needs to be just a *little* bit
    //	less than the smallest double that still indicates that
    //	mTotalElemReturned and mTotalElemSkipped are too large to indicate that
    //	the target probability has been reached.
    double tolerance = 1/(totalElem+1/totalElem);

    if ( mAbsoluteThreshold )
    {
	if (
	    std::abs((double)mTotalElemReturned/totalElem - mProbability) < tolerance
	)
	{
	    Skip( GetTotalNumber() );
	    return NULL;
	}
    }

    while (
	    mRand.Rndm() > mProbability
	&&  mTotalElemSkipped+mTotalElemReturned < totalElem
    )
    {
	//  I'd rather not do this every time, but I don't think I have a
	//  choice.  Also, notice the strange way that the check on the
	//  probability is being calculated.  That's a work around for double
	//  precision problems.
	if (
		mAbsoluteThreshold
	    &&	std::abs((double)mTotalElemSkipped/totalElem - 1.0 + mProbability) < tolerance
	)
	    break;

	Skip();
    }
    return GetNext();
}
