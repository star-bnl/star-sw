/***************************************************************************
 *
 * $Id: StRichSimpleMCHit.h,v 2.0 2000/08/09 16:22:13 gans Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Implementation of MCHit definition
 *
 ***************************************************************************
 * $Log: StRichSimpleMCHit.h,v $
 * Revision 2.0  2000/08/09 16:22:13  gans
 * Cosmetic Changes. Naming convention for TDrawable objects
 *
 * Revision 1.1  2000/05/23 16:54:19  lasiuk
 * Initial Revision
 *
 ***************************************************************************/
#ifndef ST_RICH_SIMPLE_MC_HIT_H
#define ST_RICH_SIMPLE_MC_HIT_H

#ifdef __ROOT__
class StRichMCHit;
#endif

#include "StRichSimpleHit.h"
#include "StRrsMaker/StRichPadPlane.h"

class StRichSimpleMCHit : public StRichSimpleHit {
public:
    StRichSimpleMCHit();
    StRichSimpleMCHit(const StThreeVector<double>& xl, const StThreeVector<double>& dx);
    StRichSimpleMCHit(const StThreeVector<double>& xl, const StThreeVector<double>& dx, const StRichID& info);
#ifdef __ROOT__
    StRichSimpleMCHit(const StRichMCHit*);
#endif

    ~StRichSimpleMCHit();
    StRichSimpleMCHit* clone();

    //StRichSimpleMCHit(const StRichSimpleMCHit&){}
    //StRichSimpleMCHit& operator=(const StRichSimpleMCHit&){}

    void  setMCInfo(StRichID&);
    const StRichID& getMCInfo() const; 

protected:
    StRichID mMCInfo;
};

inline void  StRichSimpleMCHit::setMCInfo(StRichID& mcInfo) { mMCInfo = mcInfo;}
inline const StRichID& StRichSimpleMCHit::getMCInfo() const {return mMCInfo;} 
inline StRichSimpleMCHit* StRichSimpleMCHit::clone() {return new StRichSimpleMCHit(*this);}
#endif
