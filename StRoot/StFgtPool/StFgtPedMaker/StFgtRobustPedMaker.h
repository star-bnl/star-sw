/*!
 * \class StFgtRobustPedMaker 
 * \author S. Gliske, Jan 2012
 */

/***************************************************************************
 *
 * $Id: StFgtRobustPedMaker.h,v 1.2 2014/08/06 11:43:11 jeromel Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: More robust method of computing the pedistals.
 *
 ***************************************************************************
 *
 * $Log: StFgtRobustPedMaker.h,v $
 * Revision 1.2  2014/08/06 11:43:11  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.1  2012/01/31 08:52:51  sgliske
 * StFgtPedMaker moved to StFgtPool
 *
 * Revision 1.2  2012/01/25 11:25:02  sgliske
 * Added GetCVS tag
 *
 * Revision 1.1  2012/01/17 20:08:49  sgliske
 * creation
 *
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_ROBUST_PED_MAKER_
#define _ST_FGT_ROBUST_PED_MAKER_

#include "StFgtPedMaker.h"

class StFgtRobustPedMaker : public StFgtPedMaker {
 public:
   // constructors
   StFgtRobustPedMaker( const Char_t* name = "FgtPedMaker" );

   // default OK
   // StFgtRobustPedMaker(const StFgtRobustPedMaker&);

   // equals operator -- default OK
   // StFgtRobustPedMaker& operator=(const StFgtRobustPedMaker&);

   // deconstructor
   virtual ~StFgtRobustPedMaker();

   Int_t Make();
   Int_t Finish();

   // modifiers
   void setNumBins( Int_t val );
   void setMaxAdc( Int_t val );
   void setNumSmooth( Int_t val );

   // Get CVS
   virtual const char *GetCVS() const;

 protected:
   // parameters
   Int_t mNumBins, mMaxAdc, mNumSmooth;

   // internal data
/*    typedef std::map< Int_t, TH1F* > histMap_t; */
/*    histMap_t mHistMap; */
/*    histMap_t::iterator mHistMapIter; */
   std::vector< TH1F* > mHistVec;

 private:   
   ClassDef(StFgtRobustPedMaker,1);

   Int_t mInternalEventNum;
}; 

// inline functions

// deconstructor
inline StFgtRobustPedMaker::~StFgtRobustPedMaker(){ /* */ };

inline void StFgtRobustPedMaker::setNumBins( Int_t val ){ mNumBins = val; };
inline void StFgtRobustPedMaker::setMaxAdc( Int_t val ){ mMaxAdc = val; };
inline void StFgtRobustPedMaker::setNumSmooth( Int_t val ){ mNumSmooth = val; };

inline const char *StFgtRobustPedMaker::GetCVS() const {
   static const char cvs[] = "Tag $Name:  $ $Id: StFgtRobustPedMaker.h,v 1.2 2014/08/06 11:43:11 jeromel Exp $ built " __DATE__ " " __TIME__ ;
   return cvs;
}

#endif
