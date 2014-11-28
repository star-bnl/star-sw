/***************************************************************************
 *
 * $Id: muEventInfo.cc,v 1.1 2004/08/10 16:09:11 perev Exp $
 *
 * Author: Wei-Ming Zhang, KSU, Mar 2004 
 ***************************************************************************
 *
 * Description:  This is an example of a function which performs
 *               some simple analysis using MuDst 
 *
 ***************************************************************************
 *
 * $Log: muEventInfo.cc,v $
 * Revision 1.1  2004/08/10 16:09:11  perev
 * new GridCollector stuff
 *
 *
 **************************************************************************/
#include "StMuDSTMaker/COMMON/StMuEvent.h"

static const char rcsid[] = "$Id: muEventInfo.cc,v 1.1 2004/08/10 16:09:11 perev Exp $";

void muEventInfo(StMuEvent& event, const int &nevents)
{

    cout << "StMuAnalysisMaker,  Reading Event: " << nevents
		       << "  eventId: " << event.eventId()
		       << "  Run: " << event.runId() << endl;
    cout << "primary vertex:   "
		       << event.primaryVertexPosition() << endl;
    cout << "refmult all, pos, and neg:   "
		       << event.refMult() << " "
		       << event.refMultPos() << " "
		       << event.refMultNeg() << endl;
}
