/****************************************************************
 * $Id: StRichDrawableTHit.h,v 1.1 2000/04/05 16:39:37 lasiuk Exp $
 *
 * Description:
 *   Cluster which is drawn in the pad monitor
 *
 ****************************************************************
 *
 * $Log: StRichDrawableTHit.h,v $
 * Revision 1.1  2000/04/05 16:39:37  lasiuk
 * Initial Revision
 *
 * Revision 1.2  2000/05/18 11:42:35  lasiuk
 * mods for pre StEvent writing
 *
 * Revision 1.1  2000/04/05 16:39:37  lasiuk
 * Initial Revision
 *
 ****************************************************************/
#ifdef __ROOT__
#ifndef ST_RICH_DRAWABLE_THIT_H
#define ST_RICH_DRAWABLE_THIT_H

#include <iostream.h>
#include <fstream.h>
#include "StRichHit.h"
#include "TMarker.h"

#include "StRichSimpleHit.h"

class StRichDrawableTHit : public TMarker {
    StRichDrawableTHit(StRichHit&, int type=3);
    StRichDrawableTHit();
    StRichDrawableTHit(double xl, double yl, int type=3);
    StRichDrawableTHit(StRichSimpleHit&, int type=3);
    
    virtual ~StRichDrawableTHit();

    //StRichDrawableTHit(const StRichDrawableTHit&) {/*use default*/|
    //StRichDrawableTHit& operator=(const StRichDrawableTHit&) {/*use default*/}

    //void ExecuteEvent(Int_t event, Int_t px, Int_t py) ;

protected:
    double    mX;
    double    mY;

protected:

    ClassDef(StRichDrawableTHit,1)
};


#endif /* THIT_H */
#endif /* ROOT */
