/***************************************************************
 * $Id: StRichDrawableTPad.cxx,v 1.5 2000/06/16 02:01:32 lasiuk Exp $
 *
 * Description:
 *
 ***************************************************************
 * $Log: StRichDrawableTPad.cxx,v $
 * Revision 1.5  2000/06/16 02:01:32  lasiuk
 * cast for integer ADC for drawing
 *
 * Revision 1.4  2000/05/17 22:18:40  lasiuk
 * use charge() instead of adc()/amp()
 *
 * Revision 1.3  2000/04/05 15:57:52  lasiuk
 * const pointer for compiler
 *
 * Revision 1.2  2000/03/17 14:54:21  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.1  2000/02/29 18:18:59  lasiuk
 * Initial Revision
 *
 ***************************************************************/
#ifdef __ROOT__

#include "StRichPadMonitorText.h"
#include "StRichSinglePixel.h"

ClassImp(StRichDrawableTPad)

StRichPadMonitorText* StRichDrawableTPad::mText = 0;

StRichDrawableTPad::StRichDrawableTPad() {/*nopt*/}

StRichDrawableTPad::StRichDrawableTPad(double xl, double yl, double xu, double yu, const StRichSinglePixel* pix)
    : TBox(xl,yl,xu,yu), mPad(pix->pad()), mRow(pix->row())
{
    mAdc=static_cast<int>(pix->charge());
}

StRichDrawableTPad::StRichDrawableTPad(double xl, double yl, double xu, double yu, int row, int pad, int adc)
    : TBox(xl,yl,xu,yu), mPad(pad), mRow(row), mAdc(adc) {/* nopt */}

StRichDrawableTPad::~StRichDrawableTPad() {/*nopt*/}

void StRichDrawableTPad::setPosition(int p, int r)
{
    mPad = p;
    mRow = r;
}

void StRichDrawableTPad::setAdc(int adc)
{
    mAdc = adc;
}

void StRichDrawableTPad::ExecuteEvent(int event, int px, int py)
{
    if (!gPad->IsEditable()) return;

    //TBox::ExecuteEvent(event, px, py);

    if (!gPad) return;
    if (!gPad->IsEditable() && event != kMouseEnter) return;

    switch(event) {

    case kMouseEnter:
	//this->Dump();
	mText->drawText(this);
	break;
    case kButton2Down:
	this->Dump();
	break;
    case kMouseLeave:
	// mText->removeText();
    default:
	break;
    }
    
    
}
       
#endif /* ROOT */
