/***************************************************************************
 *
 * $Id: StRichMCHit.h,v 2.1 2000/05/22 21:44:32 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description: Definition of the persistent MC Hit object 
 *
 ***************************************************************************
 *
 * $Log: StRichMCHit.h,v $
 * Revision 2.1  2000/05/22 21:44:32  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRichMCHit_hh
#define StRichMCHit_hh

#include "StRichHit.h"
#include "StRichMCInfo.h"

class StRichMCHit : public StRichHit {
public:
    StRichMCHit();
    StRichMCHit(const StThreeVectorF& xg, const StThreeVectorF& dx);
    StRichMCHit(const StThreeVectorF& xg, const StThreeVectorF& dx,
	      ULong_t hp, Float_t q, Float_t maxAdc, UChar_t tc);
    StRichMCHit(const StThreeVectorF& xg, const StThreeVectorF& dx,
		ULong_t hp, Float_t q, Float_t maxAdc, UChar_t tc,
		StRichMCInfo& info);
    
    ~StRichMCHit();
    
    //StRichMCHit(const StRichMCHit&){}
    //StRichMCHit& operator=(const StRichMCHit&){}

    void setMCInfo(const StRichMCInfo&);
    const StRichMCInfo& getMCInfo() const;

protected:
    StObject* clone();
    
    StRichMCInfo  mInfo;
    
    ClassDef(StRichMCHit,1) 
};

inline const StRichMCInfo& StRichMCHit::getMCInfo() const { return mInfo; }
inline void StRichMCHit::setMCInfo(const StRichMCInfo& info) { mInfo = info;}
#endif
