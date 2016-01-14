#include "StPicoDstMaker/StPicoEvent.h"
#include "StPicoDstMaker/StPicoTrack.h"

#include "StPicoD0Event.h"
#include "StKaonPion.h"

ClassImp(StPicoD0Event)

TClonesArray *StPicoD0Event::fgKaonPionArray = 0;

//-----------------------------------------------------------------------
StPicoD0Event::StPicoD0Event() : mRunId(-1), mEventId(-1), mpVtx(), mKfVertex(), mNKaonPion(0), mNKaons(0), mNPions(0), mKaonPionArray(NULL)
{
   if (!fgKaonPionArray) fgKaonPionArray = new TClonesArray("StKaonPion");
   mKaonPionArray = fgKaonPionArray;
}

//-----------------------------------------------------------------------
void StPicoD0Event::addPicoEvent(StPicoEvent const & picoEvent, const StThreeVectorF* kfVertex, const StThreeVectorF* pVtx)
{
   // StPicoEvent variables
   mRunId = picoEvent.runId();
   mEventId = picoEvent.eventId();

   if(pVtx) mpVtx = *pVtx;
   if(kfVertex) mKfVertex = *kfVertex;
   else mKfVertex.set(-999.,-999.,-999.);
}

//-----------------------------------------------------------------------
void StPicoD0Event::clear(char const *option)
{
   mKaonPionArray->Clear(option);
   mRunId = -1;
   mEventId = -1;
   mKfVertex.set(-999.,-999.,-999.);
   mNKaonPion = 0;
   mNKaons = 0;
   mNPions = 0;
}
//---------------------------------------------------------------------
void StPicoD0Event::addKaonPion(StKaonPion const* t)
{
   TClonesArray &kaonPionArray = *mKaonPionArray;
   new(kaonPionArray[mNKaonPion++]) StKaonPion(t);
}
