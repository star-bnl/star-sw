/***************************************************************
 * $Id: StRichDrawableTHit.cxx,v 2.1 2000/11/01 16:53:57 lasiuk Exp $
 *
 * Description:
 *
 ***************************************************************
 * $Log: StRichDrawableTHit.cxx,v $
 * Revision 2.1  2000/11/01 16:53:57  lasiuk
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
#include "StRchMaker/StRichSimpleHit.h"
#include "StEvent/StRichHit.h"

ClassImp(StRichDrawableTHit)

StRichDrawableTHit::StRichDrawableTHit()
    : mRichHit(0) {/*nopt*/}

StRichDrawableTHit::StRichDrawableTHit(double xl, double yl, int type)
    : TMarker(xl,yl,type), mRichHit(0)
{
    this->SetMarkerSize(1.7);
    mCharge = 0;
}

StRichDrawableTHit::StRichDrawableTHit(StRichDrawableTHit& hit)
    : TMarker(hit.GetX(), hit.GetY(), hit.GetMarkerStyle()), mRichHit(0)
{
    this->SetMarkerSize(hit.GetMarkerSize());
    this->SetMarkerColor(hit.GetMarkerColor());
    mCharge = hit.getCharge();
}

StRichDrawableTHit::StRichDrawableTHit(StRichSimpleHit& hit,int type)
    : TMarker(hit.local().x(),hit.local().y(),type), mRichHit(0)
{
    this->SetMarkerSize(1.7);
    mCharge = hit.charge();
}

StRichDrawableTHit::StRichDrawableTHit(StRichHit* hit, int type)
    : TMarker(hit->local().x(),hit->local().y(),type), mRichHit(hit)
{
    this->SetMarkerSize(2.);
    mCharge = hit->charge();
}

StRichDrawableTHit::~StRichDrawableTHit() {/*nopt*/}

StRichHit* StRichDrawableTHit::getRichHit() const {return mRichHit;}


// void StRichDrawableTHit::ExecuteEvent(int event, int px, int py)
// {
//     //if (!gPad->IsEditable()) return;

//     //TBox::ExecuteEvent(event, px, py);
    
//     // if (!gPad) return;
//     //if (!gPad->IsEditable() && event != kMouseEnter) return;

//     switch(event) {

//     case kMouseEnter:
// 	cerr <<"i did it";
// 	break;
//     case kButton2Down:
// 	cerr <<"i did it";
// 	break;
//     case kMouseLeave:
// 	cerr << "work!!!";
//     default:
// 	break;
//     }
    
// }
#endif // ROOT
