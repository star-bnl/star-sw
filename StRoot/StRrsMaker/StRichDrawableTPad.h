/****************************************************************
 * $Id: StRichDrawableTPad.h,v 1.1 2000/02/29 18:18:59 lasiuk Exp $
 *
 * Description:
 *   Pad which is drawn in the pad monitor
 *
 ****************************************************************
 *
 * $Log: StRichDrawableTPad.h,v $
 * Revision 1.1  2000/02/29 18:18:59  lasiuk
 * Initial Revision
 *
 * Revision 1.2  2000/03/17 14:54:24  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
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
    StRichDrawableTPad(double xl, double yl, double xu, double yu, StRichSinglePixel* pix);
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
    
    void ExecuteEvent(Int_t event, Int_t px, Int_t py) ;

    static void setPadMonitorText(StRichPadMonitorText*);

private:
    int    mPad;
    int    mRow;
    int    mAdc;

private:
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
