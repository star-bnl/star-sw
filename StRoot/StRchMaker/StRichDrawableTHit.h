/****************************************************************
 * $Id: StRichDrawableTHit.h,v 1.5 2000/06/16 02:07:31 lasiuk Exp $
 *
 * Description:
 *   Cluster which is drawn in the pad monitor
 *
 ****************************************************************
 *
 * $Log: StRichDrawableTHit.h,v $
 * Revision 1.5  2000/06/16 02:07:31  lasiuk
 * copy c'tor
 *
 * Revision 1.4  2000/05/25 21:35:32  fisyak
 * Make rootcint happy
 *
 * Revision 1.3  2000/05/23 16:55:51  lasiuk
 * Incorporate new MC info
 * add clone() where necessary
 * accomodate name changes
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


#include "TMarker.h"

class StRichSimpleHit;

class StRichDrawableTHit : public TMarker {
public:
    StRichDrawableTHit();
    StRichDrawableTHit(double xl, double yl, int type=3);
    StRichDrawableTHit(StRichSimpleHit&, int type=3);
    StRichDrawableTHit(StRichDrawableTHit&);
    virtual ~StRichDrawableTHit();

    //StRichDrawableTHit(const StRichDrawableTHit&) {/*use default*/|
    //StRichDrawableTHit& operator=(const StRichDrawableTHit&) {/*use default*/}

    //void ExecuteEvent(Int_t event, Int_t px, Int_t py) ;

protected:
    double    mX; // Brian..why are these here??
    double    mY;

protected:

    ClassDef(StRichDrawableTHit,1)
};


#endif /* THIT_H */
#endif /* ROOT */
