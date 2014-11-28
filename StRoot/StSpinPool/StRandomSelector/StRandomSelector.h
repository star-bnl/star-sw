//  $Id: StRandomSelector.h,v 1.1 2009/09/03 11:57:11 rfatemi Exp $
//
//  $Log: StRandomSelector.h,v $
//  Revision 1.1  2009/09/03 11:57:11  rfatemi
//  This class allows the user to randomly remove members of any group
//

/*  StRandomSelector.h
 *
 *  Written by Wayne Witzke for the University of Kentucky Department of
 *  Physics and Astronomy.
 *
 *  This random selector will allow a developer to randomly pick TObjects from
 *  a TObject.  It is very iterator-like, but this class is not an iterator,
 *  since there is just slightly too much work for this class to do to make it
 *  really behave like a proper iterator.
 *
 *  Originally this was going to be a template, taking container and element
 *  types, but this proved to be basically impossible to implement sanely, both
 *  because the ROOT library fails to implement an STL-like interface, and
 *  because ROOT/CINT doesn't know how to handle templates properly (i.e., I
 *  shouldn't have to enumerate the vast number of possible template parameters
 *  and combinations thereof in order to make the template work. . .  That's
 *  just a stupid requirement and defeats the whole purpose of developing
 *  generics/templates in the first place, and if ROOT/CINT *is* capable of
 *  doing this, then why isn't the documentation for making it work in a palce
 *  that is easily accessible?)
 *
 *  Now, instead, it is going to follow the non-template-based ROOT library
 *  conventions, which is an inferior way to implement this class.
 */
#ifndef STRANDOMSELECTOR_H
#define STRANDOMSELECTOR_H

#include <TRandom3.h>
#include <TCollection.h>
#include <TIterator.h>
#include <TObject.h>
#include <Rtypes.h>

//  This class can be used to select TObjects randomly (but sequentially) from
//  a given TCollection.  That is, random TObjects in the collection will be
//  selected based on the probability specified for the object.  If an TObject
//  is not randomly chosen for selection, it will be skipped when using the
//  random selector to iterate over the TCollection.
//
//  WARNING: This class has not been fully tested for very, very large
//  collections, on the order of 100,000 or more elements, with absolute
//  thresholding enabled.  While absolute thresholding should still *almost*
//  work for very large collections, and it might even work perfectly, there is
//  a chance that the returned and skipped sets of elements will not actually
//  confirm exactly to the absolute threshold and be only almost absolute.  I
//  think that they may be off by at most plus or minus 1.
class StRandomSelector
{
    protected:
	//  Protected member variables

	//  This is the random number generator that we will use. This should
	//  use the Mersenne Twister algorithm to generate random numbers.
	TRandom3 mRand;	

	//  This is the total number of TObjects that have been returned 
	//  through selection.
	Int_t mTotalElemReturned;

	//  This is the number of TObjects that have been skipped through
	//  selection.
	Int_t mTotalElemSkipped;

	//  This is the probability of selecting an TObject when selection
	//  occurs.  This is a ratio, so values should be in the range [0,1].
	Double_t mProbability;

	//  This determines if mProbability represents an absolute threshold
	//  for the number of TObjects selected.  If this is set, then the
	//  mProbabiility not only determines the chance of selecting a given
	//  TObject, but also determines the absolute total number of TObjects
	//  that can possibly be returned or skipped.
	bool mAbsoluteThreshold;

	//  This is the collection of TObjects from which selections should be
	//  made.
	const TCollection * mContainer;

	//  This is an iterator to the current TObject.
	TIterator * mIterator;

	//  Protected methods

	//  This method will perform basic initialization of an object.
	//  As a quick reference, newContainer is a new TCollection, newProb is
	//  the probability that should be set for this object, newAT is
	//  whether or not the absolute threshold should be true or false for
	//  this object, and seed is the seed that should be used for this
	//  object.
	void initialize(
	    const TCollection * newContainer,
	    Double_t newProb,
	    bool newAt,
	    UInt_t newSeed
	)
	{
	    mRand.SetSeed( newSeed );
	    mProbability = newProb;
	    mAbsoluteThreshold = newAt;

	    mContainer = newContainer;
	    mIterator = 0;
	    Rewind();
	}

    public:
	//  Public constructors

	//  This is a basic initializer that does not take an initial
	//  TCollection.
	//  newProb is the probability of selecting an TObject from the
	//	TCollection.  This should be ratio in the range [0,1].
	//  newAt is a flag that specifies whether or not the specified
	//	selection probability is an absolute threshold or not.  If it
	//	is an absolute threshold, then the probability represents the
	//	absolute ratio of selected to skipped TObjects once all
	//	TObjects have been exhausted.  If the selection probability is
	//	not an absolute threshold, then it is theoretically possible
	//	for no Elements to be selected, or for all TObjects to be
	//	selected, regardless of the probability.
	//  seed is the seed that should be used when seeding the random number
	//	generator.  If the seed is zero a random seed will be
	//	generated.  NOTE: If random seeds are generated too quickly in
	//	succession, they will be identical because of the way that
	//	TRandom3 is generating random seeds (based, apparently, on the
	//	current unix time, to the second).
	StRandomSelector(
	    Double_t newProb = 1.0,
	    bool newAT = false,
	    UInt_t seed = 0
	)
	{
	    initialize( NULL, newProb, newAT, seed );
	}

	//  This is a basic initializer allowing the initial specification of
	//  a TCollection.
	//  newElement is a TCollection pointer that contains a set of TObjects
	//	from which this class should select (when asked).
	//  newProb is the probability of selecting an TObject from the
	//	TCollection.  This should be ratio in the range [0,1].
	//  newAt is a flag that specifies whether or not the specified
	//	selection probability is an absolute threshold or not.  If it
	//	is an absolute threshold, then the probability represents the
	//	absolute ratio of selected to skipped TObjects once all
	//	TObjects have been exhausted.  If the selection probability is
	//	not an absolute threshold, then it is theoretically possible
	//	for no Elements to be selected, or for all TObjects to be
	//	selected, regardless of the probability.
	//  seed is the seed that should be used when seeding the random number
	//	generator.  If the seed is zero a random seed will be
	//	generated.  NOTE: If random seeds are generated too quickly in
	//	succession, they will be identical because of the way that
	//	TRandom3 is generating random seeds (based, apparently, on the
	//	current unix time, to the second).
	StRandomSelector(
	    TCollection * newContainer,
	    Double_t newProb = 1.0,
	    bool newAT = false,
	    UInt_t seed = 0
	)
	{
	    initialize( newContainer, newProb, newAT, seed );
	}

	//  We can no longer use the default copy constructor because of the
	//  mIterator pointer.  This method is called automatically whenever
	//  a copy is made of an StRandomSelector.
	StRandomSelector( const StRandomSelector & toCopy )
	    : mRand(toCopy.mRand),
	      mTotalElemReturned(toCopy.mTotalElemReturned),
	      mTotalElemSkipped(toCopy.mTotalElemSkipped),
	      mProbability(toCopy.mProbability),
	      mAbsoluteThreshold(toCopy.mAbsoluteThreshold),
	      mContainer(toCopy.mContainer),
	      mIterator( 0 )
	{
	    if ( mContainer )
	    {
		mIterator = mContainer->MakeIterator();
		//  This next line is a good example of why you don't really
		//  want to have pointers to iterators.
		(*mIterator) = (*toCopy.mIterator);
	    }
	}

	//  Public methods

	//  This will rewind the selection process so that selection can start
	//  anew at the beginning of the TCollection.  This will NOT reseed the
	//  object, however.  To reseed, SetSeed must be called.
	void Rewind()
	{
	    mTotalElemReturned = 0;
	    mTotalElemSkipped = 0;

	    if ( mIterator )
		delete mIterator;

	    if ( mContainer )
	    {
		mIterator = mContainer->MakeIterator();
	    }
	    else
	    {
		mIterator = NULL;
	    }
	}

	//  This method will allow a user to manually skip TObjects.  This
	//  method will correctly maintain all internal state variables (so a
	//  call to GetNumberSkipped() will have incremented after a call to
	//  Skip()).
	void Skip( Int_t numberToSkip = 1 )
	{
	    if ( !mContainer )
		return;

	    TObject * checkme;
	    for ( int ii = 0; ii < numberToSkip; ++ii )
	    {
		checkme = mIterator->Next();
		if ( !checkme )
		{
		    break;
		}
		++mTotalElemSkipped;
	    }
	}

	//  This next method will get the next TObject from the TCollection and
	//  return it.  This method does NOT select randomly and will never
	//  skip TObjects.  All internal state variables are updated properly
	//  when this method is called (so that a call to GetNumberReturned()
	//  will have incremented after a call to GetNext()).
	//
	//  This will return NULL if there are no TObjects left to return.
	TObject * GetNext()
	{
	    if ( !mContainer )
		return NULL;

	    TObject * retVal = mIterator->Next();
	    if ( retVal )
	    {
		++mTotalElemReturned;
	    }

	    return retVal;
	}

	//  This will randomly select the next TObject to return.
	//
	//  This will return NULL if there are no TObjects left to return.
	TObject * GetNextRandom();

	//  Accessor methods

	//  These two methods allow the user to get and set the seed for the
	//  random selection.  SetSeed is called automatically when an object
	//  is instantiated, but can be set again using these calls (for
	//  instance, if a specific seed is desired so that a sequence can be
	//  reproduced).  This will not automatically rewind the random
	//  selection mechanism, so that the new seed will take place only on
	//  newly selected TObjects.  NOTE: If random seeds are generated too
	//  quickly in succession, they will be identical because of the way
	//  that TRandom3 is generating random seeds (based, apparently, on the
	//  current unix time, to the second).
	void SetSeed( UInt_t seed = 0 )
	{
	    mRand.SetSeed( seed );
	}
	const UInt_t GetSeed()
	{
	    return mRand.GetSeed();
	}

	//  These two methods allow the user to change the probability of
	//  selecting a TObject from the TCollection.  The probability should
	//  be passed as a ratio in the range [0,1].  Values greater than 1
	//  will have the same effect as 1, while values less than 0 will have
	//  the same effect as 0.  It is important to note that the number
	//  passed here is the probability of SELECTING an TObject.  So, if you
	//  want to select 3/4ths of the TObjects, then call SetProbability
	//  with the number 0.75  This change will only effect selections that
	//  occur after the probability change has taken place.
	void SetProbability( Double_t newProb )
	{
	    mProbability = newProb;
	}
	const Double_t GetProbability()
	{
	    return mProbability;
	}

	//  These methods allow the user to select whether or not mProbability
	//  represents an absolute threshold.  That is, if mProbability
	//  represents an absolute threshold, then selection will fail to
	//  return additional TObjects once that threshold of returned-to-total
	//  TObjects has been reached.  If mProbability does *not* represent an
	//  absolute threshold, then even if the returned-to-total ratio
	//  exceeds the mProbability value, the selector will still randomly
	//  determine on each remaining TObject whether or not that TObject is
	//  returned.  In the first case, there is no chance that the ratio of
	//  returned-to-total will exceed mProbability.  In the second case, it
	//  is conceivable that no TObjects will be withheld.  In addition, if
	//  the absolute threshold is set to true, then the number of TObjects
	//  withheld will not exceed (by much)
	//  GetTotalNumber()*(100-mProbability), and if the number of TObjects
	//  withheld exceeds this value, then every remaining TObject will be
	//  returned.
	void SetAbsoluteThreshold( bool newAT )
	{
	    mAbsoluteThreshold = newAT;
	}
	const bool GetAbsoluteThreshold()
	{
	    return mAbsoluteThreshold;
	}

	//  This method will return the number of TObjects in the TCollection.
	const Int_t GetTotalNumber()
	{
	    if ( mContainer )
	    {
		return mContainer->GetEntries();
	    }
	    else
	    {
		return 0;
	    }
	}

	//  This method will return the total number of TObjects that have been
	//  returned so far.
	const Int_t GetNumberReturned()
	{
	    return mTotalElemReturned;
	}

	//  This method will return the total number of TObjects that have been
	//  skipped so far.
	const Int_t GetNumberSkipped()
	{
	    return mTotalElemSkipped;
	}

	//  These next methods will allow the user to get and set the
	//  Container.  Setting the TCollection will cause all selection
	//  information (current position in collection, total return/skipped)
	//  to be reset.  This will NOT cause the probability, absolute
	//  threshold flag, or random seed to change.
	void SetContainer( TCollection * newCont )
	{
	    mContainer = newCont;
	    Rewind();
	}
	const TCollection * GetContainer()
	{
	    return mContainer;
	}

	//  We have to clean up our pointer mIterator (yet another reason why
	//  the STL is superior, sure it's quicker to pass around pointers,
	//  buter iterator creation is unusual and so has very little overhead,
	//  and iterators can be passed around either as references or as const
	//  references).
	virtual ~StRandomSelector()
	{
	    if ( mIterator )
	    {
		delete mIterator;
	    }
	}

    //  This is required to tie this header to the class implementation defined
    //	in the .cxx file, so that the library can be successfully loaded into a
    //	root macro.
    ClassDef(StRandomSelector,0)
};

#endif // STARRANDOMSELECTOR_H
