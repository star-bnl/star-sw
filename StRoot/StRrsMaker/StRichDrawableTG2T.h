/****************************************************************
 * $Id: StRichDrawableTG2T.h,v 1.1 2000/04/05 15:55:07 lasiuk Exp $
 *
 * Description:
 *
 * Enumerated Types for ROOT Actions are in: TButton.h
 * enum EEventType {
 *   kNoEvent       =  0,
 *  kButton1Down   =  1, kButton2Down   =  2, kButton3Down   =  3, kKeyDown  =  4,
 *  kButton1Up     = 11, kButton2Up     = 12, kButton3Up     = 13, kKeyUp    = 14,
 *  kButton1Motion = 21, kButton2Motion = 22, kButton3Motion = 23, kKeyPress = 24,
 *  kButton1Locate = 41, kButton2Locate = 42, kButton3Locate = 43,
 *  kMouseMotion   = 51, kMouseEnter    = 52, kMouseLeave    = 53,
 *  kButton1Double = 61, kButton2Double = 62, kButton3Double = 63
 *  };
 *
 * enum EEditMode {
 *  kPolyLine  = 1,  kSPolyLine = 2,  kPolyGone  = 3,
 *  kSPolyGone = 4,  kBox       = 5,  kDelete    = 6,
 *  kPad       = 7,  kText      = 8,  kEditor    = 9,
 *  kExit      = 10, kPave      = 11, kPaveLabel = 12,
 *  kPaveText  = 13, kPavesText = 14, kEllipse   = 15,
 *  kArc       = 16, kLine      = 17, kArrow     = 18,
 *  kGraph     = 19, kMarker    = 20, kPolyMarker= 21,
 *  kPolyLine3D= 22, kWbox      = 23, kGaxis     = 24,
 *  kF1        = 25, kF2        = 26, kF3        = 27,
 *  kDiamond   = 28, kPolyMarker3D = 29
 *  };
 ****************************************************************
 *
 * $Log: StRichDrawableTG2T.h,v $
 * Revision 1.1  2000/04/05 15:55:07  lasiuk
 * Initial Revision
 *
 ****************************************************************/
#ifdef __ROOT__
#ifndef ST_RICH_DRAWABLE_TG2T_H
#define ST_RICH_DRAWABLE_TG2T_H

#include "TCanvas.h"
#include "TText.h"

class StRichPadMonitor;

#include "StRichG2TInfo.h"

class StRichDrawableTG2T : public TText {
public:
    StRichDrawableTG2T();
    StRichDrawableTG2T(double x, double y, int trackp, char* type="u");
    StRichDrawableTG2T(const StRichG2TInfo& g2t);
    
    virtual ~StRichDrawableTG2T();

    //StRichDrawableTG2T(const StRichDrawableTG2T&) {/*use default*/|
    //StRichDrawableTG2T& operator=(const StRichDrawableTG2T&) {/*use default*/}

    virtual void ExecuteEvent(Int_t event, Int_t px, Int_t py) ;
    static void setPadMonitor(StRichPadMonitor*);
    
public:
    int       mTrackp;

private:
    static StRichPadMonitor* mPadMonitor;
    
    ClassDef(StRichDrawableTG2T,1)
};

inline void StRichDrawableTG2T::setPadMonitor(StRichPadMonitor* moni) { mPadMonitor = moni; }
#endif /* TPAD_H */
#endif /* ROOT */
