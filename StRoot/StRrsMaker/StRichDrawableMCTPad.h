/****************************************************************
 * $Id: StRichDrawableMCTPad.h,v 1.2 2000/05/17 22:18:40 lasiuk Exp $
 *
 * Description:
 *   MC Pad which is drawn in the pad monitor
 *
 ****************************************************************
 *
 * $Log: StRichDrawableMCTPad.h,v $
 * Revision 1.2  2000/05/17 22:18:40  lasiuk
 * use charge() instead of adc()/amp()
 *
 * Revision 1.1  2000/04/05 15:54:59  lasiuk
 * Initial Revision
 *
 ****************************************************************/
#ifdef __ROOT__
#ifndef ST_RICH_DRAWABLE_MCTPAD_H
#define ST_RICH_DRAWABLE_MCTPAD_H

#include "StRichDrawableTPad.h"
#include "StRichSingleMCPixel.h"

class StRichDrawableMCTPad : public StRichDrawableTPad {
public:
    StRichDrawableMCTPad();
    StRichDrawableMCTPad(double xl, double yl, double xu, double yu,
			 const StRichSingleMCPixel* mcPix);
//     StRichDrawableMCTPad(double xl, double yl, double xu, double yu, int pad, int row, int adc);
    
    virtual ~StRichDrawableMCTPad();

    //StRichDrawableMCTPad(const StRichDrawableMCTPad&) {/*use default*/|
    //StRichDrawableMCTPad& operator=(const StRichDrawableMCTPad&) {/*use default*/}

    // access functions
    //int trackp(int no) const;


protected:
    int mGID1;
    int mTrackp1;
    float mQ1;
    StRichSignalType mType1;
    int mGID2;
    int mTrackp2;
    float mQ2;
    StRichSignalType mType2;
    
    ClassDef(StRichDrawableMCTPad,1)
};
#endif /* TPAD_H */
#endif /* ROOT */
