/*!
 * \class StFgtRawBase 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtRawBase.cxx,v 1.2 2011/09/26 16:55:53 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: See header file.
 *
 ***************************************************************************
 *
 * $Log: StFgtRawBase.cxx,v $
 * Revision 1.2  2011/09/26 16:55:53  sgliske
 * Continued work on cosmic QA plots
 *
 * Revision 1.1  2011/09/21 17:49:33  sgliske
 * alternate base class with more
 *  functionality and not an StMaker
 *
 *
 **************************************************************************/

#include "StFgtRawBase.h"

// constructors
StFgtRawBase::StFgtRawBase( UInt_t numDiscs, Int_t numRawHits, Int_t numClusters, Int_t numPoints ) :
   mFgtEventPtr( 0 ),
   mNumDiscs( numDiscs ),
   mNumRawHits( numRawHits ),
   mNumClusters( numClusters ),
   mNumPoints( numPoints ),
   mEOF( 0 )
{
   // nothing else to do
};

StFgtRawBase::StFgtRawBase( const StFgtRawBase& rhs ) :
   mFgtEventPtr( 0 ),
   mNumDiscs( rhs.mNumDiscs ),
   mNumRawHits( rhs.mNumRawHits ),
   mNumClusters( rhs.mNumClusters ),
   mNumPoints( rhs.mNumPoints ) {

   // in this case, construct the event here
   if( rhs.mFgtEventPtr )
      mFgtEventPtr = new StFgtEvent( *(rhs.mFgtEventPtr) );
};

// equals operator
StFgtRawBase& StFgtRawBase::operator=( const StFgtRawBase& rhs ){
   mFgtEventPtr = 0;
   mNumDiscs = rhs.mNumDiscs;
   mNumRawHits = rhs.mNumRawHits;
   mNumClusters = rhs.mNumClusters;
   mNumPoints = rhs.mNumPoints;

   if( mFgtEventPtr ){
      delete mFgtEventPtr;
      mFgtEventPtr = 0;
   };

   if( rhs.mFgtEventPtr )
      mFgtEventPtr = new StFgtEvent( *(rhs.mFgtEventPtr) );

   return *this;
};

Int_t StFgtRawBase::constructFgtEvent(){
   if( mFgtEventPtr )
      delete mFgtEventPtr;

   mFgtEventPtr = new StFgtEvent( mNumDiscs, mNumRawHits, mNumClusters, mNumPoints );

   return ( mFgtEventPtr ? kStOk : kStErr );
};
