/*!
 * \class StFgtRawBase 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtRawBase.h,v 1.1 2011/09/21 17:49:33 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Base raw maker for data from either the cosmic test
 * stand or from within STAR.  Note: this is not actually a "maker" at
 * all, as the two children either seperately inherit from StMaker or
 * StRTSBaseMaker.
 *
 ***************************************************************************
 *
 * $Log: StFgtRawBase.h,v $
 * Revision 1.1  2011/09/21 17:49:33  sgliske
 * alternate base class with more
 *  functionality and not an StMaker
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_BASE_RAW_MAKER_H_
#define _ST_FGT_BASE_RAW_MAKER_H_

#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"

class StFgtRawBase  {
 public:
   // constructors
   StFgtRawBase( UInt_t numDiscs = 6, Int_t numRawHits = 20, Int_t numClusters = 5, Int_t numPoints = 1 );
   StFgtRawBase( const StFgtRawBase& );

   // deconstructor
   virtual ~StFgtRawBase();

   // equals operator
   StFgtRawBase& operator=(const StFgtRawBase&);

   // Construct the StFgtEvent
   Int_t constructFgtEvent();

   // accessors
   StFgtEvent* getFgtEventPtr();
   const StFgtEvent* getFgtEventPtr() const;

   // modifiers
   void setNumDiscs( UInt_t num );
   void setNumRawHits( Int_t num );
   void setNumClusters( Int_t num );
   void setNumPoints( Int_t num );


 protected:
   StFgtEvent* mFgtEventPtr;

   UInt_t mNumDiscs;
   Int_t mNumRawHits;
   Int_t mNumClusters;
   Int_t mNumPoints;

 private:   
   ClassDef(StFgtRawBase,1);
}; 

// inline functions

// deconstructor
inline StFgtRawBase::~StFgtRawBase() { /* */ };

// accessors
inline StFgtEvent* StFgtRawBase::getFgtEventPtr(){
   return mFgtEventPtr;
};

inline const StFgtEvent* StFgtRawBase::getFgtEventPtr() const {
   return mFgtEventPtr;
};

// modifiers
inline void StFgtRawBase::setNumDiscs( UInt_t num ){ mNumDiscs = num; };
inline void StFgtRawBase::setNumRawHits( Int_t num ){ mNumRawHits = num; };
inline void StFgtRawBase::setNumClusters( Int_t num ){ mNumClusters = num; };
inline void StFgtRawBase::setNumPoints( Int_t num ){ mNumPoints = num; };

#endif
