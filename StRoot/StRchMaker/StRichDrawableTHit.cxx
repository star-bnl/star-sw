/***************************************************************
 * $Id: StRichDrawableTHit.cxx,v 1.4 2000/05/25 21:35:32 fisyak Exp $
 *
 * Description:
 *
 ***************************************************************
 * $Log: StRichDrawableTHit.cxx,v $
 * Revision 1.4  2000/05/25 21:35:32  fisyak
 * Make rootcint happy
 *
 * Revision 1.3  2000/05/23 16:55:48  lasiuk
 * Incorporate new MC info
 * add clone() where necessary
 * accomodate name changes
 *
 * Revision 1.2  2000/05/18 11:42:32  lasiuk
 * mods for pre StEvent writing
 *
 * Revision 1.1  2000/04/05 16:39:34  lasiuk
 * Initial Revision
 *
 ***************************************************************/
#include <iostream.h>
#include <fstream.h>
#ifdef __ROOT__
#include "StRichDrawableTHit.h"
#include "StRichSimpleHit.h"

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
