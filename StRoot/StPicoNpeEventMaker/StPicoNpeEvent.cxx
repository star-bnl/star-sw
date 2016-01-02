#include "StPicoDstMaker/StPicoEvent.h"
#include "StPicoDstMaker/StPicoTrack.h"

#include "StPicoNpeEvent.h"
#include "StElectronPair.h"

ClassImp(StPicoNpeEvent)

TClonesArray *StPicoNpeEvent::fgElectronPairArray = 0;

//-----------------------------------------------------------------------
StPicoNpeEvent::StPicoNpeEvent() : mRunId(-1), mEventId(-1), mNElectronPair(0), mNElectrons(0), mNPartners(0), mElectronPairArray(NULL)
{
    if (!fgElectronPairArray) fgElectronPairArray = new TClonesArray("StElectronPair");
    mElectronPairArray = fgElectronPairArray;

}

//-----------------------------------------------------------------------
void StPicoNpeEvent::addPicoEvent(StPicoEvent const & picoEvent)
{
    // StPicoEvent variables
    mRunId = picoEvent.runId();
    mEventId = picoEvent.eventId();
}

//-----------------------------------------------------------------------
void StPicoNpeEvent::clear(char const *option)
{
    mElectronPairArray->Clear(option);
    mRunId = -1;
    mEventId = -1;
    mNElectronPair = 0;
    mNElectrons = 0;
    mNPartners = 0;
}
//---------------------------------------------------------------------
void StPicoNpeEvent::addElectronPair(StElectronPair const* t)
{
    TClonesArray &electronPairArray = *mElectronPairArray;
    new(electronPairArray[mNElectronPair++]) StElectronPair(t);
}
