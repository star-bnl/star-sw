/****************************************************************
 * $Id: StRichDrawableTHit.h,v 2.1 2000/11/01 16:53:59 lasiuk Exp $
 *
 * Description:
 *   Cluster which is drawn in the pad monitor
 *
 ****************************************************************
 *
 * $Log: StRichDrawableTHit.h,v $
 * Revision 2.1  2000/11/01 16:53:59  lasiuk
 * add interface for the StRichHit
 *
 * Revision 2.0  2000/08/09 16:28:02  gans
 * Created New Maker for all drawable objects.
 *
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

class StRichSimpleHit;
class StRichHit;

#include "TMarker.h"

class StRichDrawableTHit : public TMarker {
public:
    StRichDrawableTHit();

    //
    // type[4](5) is a [circle](cross)
    StRichDrawableTHit(double xl, double yl, int type=5);
    StRichDrawableTHit(StRichSimpleHit&, int type=5);
    StRichDrawableTHit(StRichHit*, int type=4);

    virtual ~StRichDrawableTHit();

    // should be constant but has warnings
    StRichDrawableTHit(StRichDrawableTHit&);
    //StRichDrawableTHit& operator=(const StRichDrawableTHit&);

    double     getCharge()  const;
    StRichHit* getRichHit() const;
    
    //void ExecuteEvent(Int_t event, Int_t px, Int_t py) ;

protected:
    StRichHit* mRichHit;//!
    double     mCharge;

    ClassDef(StRichDrawableTHit,1)
};

inline double StRichDrawableTHit::getCharge() const {return mCharge;}

#endif /* THIT_H */
#endif /* ROOT */
