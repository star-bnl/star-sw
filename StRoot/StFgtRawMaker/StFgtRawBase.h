/*!
 * \class StFgtRawBase 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtRawBase.h,v 1.7 2011/10/26 20:57:48 avossen Exp $
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
 * Revision 1.7  2011/10/26 20:57:48  avossen
 * hopefully made cosmic and raw maker compatible with bfc (again), added clear in make. Unnecessary if member fkt clear() is called after every event
 *
 * Revision 1.6  2011/10/18 03:16:08  avossen
 * make compatible with chain like event saving, first step
 *
 * Revision 1.5  2011/09/30 19:08:12  sgliske
 * general update
 *
 * Revision 1.4  2011/09/30 17:24:39  sgliske
 * LOG_* bug solved, so can now return kStEof
 *
 * Revision 1.3  2011/09/26 16:55:53  sgliske
 * Continued work on cosmic QA plots
 *
 * Revision 1.2  2011/09/21 19:31:31  sgliske
 * minor update
 *
 * Revision 1.1  2011/09/21 17:49:33  sgliske
 * alternate base class with more
 *  functionality and not an StMaker
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_BASE_RAW_MAKER_H_
#define _ST_FGT_BASE_RAW_MAKER_H_

//#include <TObject.h>
#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"

class StFgtRawBase {
 public:
   // constructors
   StFgtRawBase( UInt_t numDiscs = 6, Int_t numRawHits = 20, Int_t numClusters = 5, Int_t numPoints = 1 );
   StFgtRawBase( const StFgtRawBase& );

   // deconstructor
   virtual ~StFgtRawBase();

   // equals operator
   StFgtRawBase& operator=(const StFgtRawBase&);

   // Construct the StFgtEvent, or grab the old one
   virtual Int_t prepareEnvironment();

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
