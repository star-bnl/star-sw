/***************************************************************
 * $Id: StRichDrawableTHit.cxx,v 1.2 2000/05/18 11:42:32 lasiuk Exp $
 *
 * Description:
 *
 ***************************************************************
 * $Log: StRichDrawableTHit.cxx,v $
 * Revision 1.2  2000/05/18 11:42:32  lasiuk
 * mods for pre StEvent writing
 *
 * Revision 1.2  2000/05/18 11:42:32  lasiuk
 * mods for pre StEvent writing
 *
 * Revision 1.1  2000/04/05 16:39:34  lasiuk
 * Initial Revision
 *
 ***************************************************************/
#ifdef __ROOT__
#include "StRichDrawableTHit.h"

ClassImp(StRichDrawableTHit)

StRichDrawableTHit::StRichDrawableTHit() {/*nopt*/}

StRichDrawableTHit::StRichDrawableTHit(double xl, double yl, int type)
    : TMarker(xl,yl,type)
{/* nopt */}

StRichDrawableTHit::StRichDrawableTHit(StRichSimpleHit& hit, int type)
    : TMarker(hit.local().x(),hit.local().y(),type)
{
    // include information you want available for clicking
}

StRichDrawableTHit::~StRichDrawableTHit() {/*nopt*/}

// void StRichDrawableTHit::ExecuteEvent(int event, int px, int py)
// {
//     if (!gPad->IsEditable()) return;

//     //TBox::ExecuteEvent(event, px, py);

//     if (!gPad) return;
//     if (!gPad->IsEditable() && event != kMouseEnter) return;

//     switch(event) {

//     case kMouseEnter:
// 	//this->Dump();
// 	mText->drawText(this);
// 	break;
//     case kButton2Down:
// 	this->Dump();
// 	break;
//     case kMouseLeave:
// 	// mText->removeText();
//     default:
// 	break;
//     }
    
    
// }
       
#endif /* ROOT */
