/***************************************************************
 * $Id: StRichDrawableTG2T.cxx,v 2.1 2003/09/02 17:58:52 perev Exp $
 *
 * Description:
 *
 ***************************************************************
 * $Log: StRichDrawableTG2T.cxx,v $
 * Revision 2.1  2003/09/02 17:58:52  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.0  2000/08/09 16:28:02  gans
 * Created New Maker for all drawable objects.
 *
 * Revision 1.1  2000/04/05 15:55:10  lasiuk
 * Initial Revision
 *
 ***************************************************************/
#ifdef __ROOT__
#include <Stiostream.h>
#include "StRichDrawableTG2T.h"
#include "StRichPadMonitor.h"

ClassImp(StRichDrawableTG2T)
    
StRichPadMonitor* StRichDrawableTG2T::mPadMonitor = 0;

StRichDrawableTG2T::StRichDrawableTG2T() {/*nopt*/}

StRichDrawableTG2T::StRichDrawableTG2T(double x, double y, int trackp, char* type)
    : TText(x,y,type), mTrackp(trackp)
{/* nopt */}

StRichDrawableTG2T::StRichDrawableTG2T(const StRichG2TInfo& g2t)
    : TText(g2t.mX,g2t.mY,g2t.mType), mTrackp(g2t.mTrackp)
{/* nopt */}

StRichDrawableTG2T::~StRichDrawableTG2T() {/*nopt*/}

void StRichDrawableTG2T::ExecuteEvent(int event, int px, int py)
{
    if (!gPad->IsEditable()) return;

    if (!gPad) return;
    if (!gPad->IsEditable() && event != kMouseEnter) return;

    switch(event) {

    case kButton1Down:
	mPadMonitor->drawGeantGroup(mTrackp);
	break;
    default:
	break;
    }
    
    
}
       
#endif /* ROOT */
