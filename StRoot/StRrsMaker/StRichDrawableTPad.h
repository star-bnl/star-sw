/****************************************************************
 * $Id: StRichDrawableTPad.h,v 1.3 2000/04/05 15:57:56 lasiuk Exp $
 *
 * Description:
 *   Pad which is drawn in the pad monitor
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
 * $Log: StRichDrawableTPad.h,v $
 * Revision 1.3  2000/04/05 15:57:56  lasiuk
 * const pointer for compiler
 *
 * Revision 1.2  2000/03/17 14:54:24  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.1  2000/02/29 18:18:59  lasiuk
 * Initial Revision
 ****************************************************************/
#ifdef __ROOT__
#ifndef ST_RICH_DRAWABLETPAD_H
#define ST_RICH_DRAWABLETPAD_H

class StRichPadMonitorText;
class StRichSinglePixel;

#include "TCanvas.h"
#include "TBox.h"

class StRichDrawableTPad : public TBox {
public:
    StRichDrawableTPad();
    StRichDrawableTPad(double xl, double yl, double xu, double yu, const StRichSinglePixel* pix);
    StRichDrawableTPad(double xl, double yl, double xu, double yu, int pad, int row, int adc);
    
    virtual ~StRichDrawableTPad();

    //StRichDrawableTPad(const StRichDrawableTPad&) {/*use default*/|
    //StRichDrawableTPad& operator=(const StRichDrawableTPad&) {/*use default*/}

    void setPosition(int p, int r);
    void setAdc(int adc);

    int  adc() const;
    int  pad() const;
    int  row() const;
    
    void draw();
    
    virtual void ExecuteEvent(Int_t event, Int_t px, Int_t py) ;

    static void setPadMonitorText(StRichPadMonitorText*);

protected:
    int    mPad;
    int    mRow;
    int    mAdc;

protected:
    static StRichPadMonitorText* mText;

    ClassDef(StRichDrawableTPad,1)
};

inline void StRichDrawableTPad::setPadMonitorText(StRichPadMonitorText* t) { mText = t; }
inline void StRichDrawableTPad::draw() { this->Draw(); }
inline int  StRichDrawableTPad::adc() const { return mAdc; }
inline int  StRichDrawableTPad::pad() const { return mPad; }
inline int  StRichDrawableTPad::row() const { return mRow; }

#endif /* TPAD_H */
#endif /* ROOT */
