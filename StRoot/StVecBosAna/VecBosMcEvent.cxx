#include "VecBosMcEvent.h"


ClassImp(VecBosMcEvent)


using namespace std;


VecBosMcEvent::VecBosMcEvent() : TObject(), mStMcEvent(0)
{
}


VecBosMcEvent::VecBosMcEvent(StMcEvent &stMcEvent) : TObject(), mStMcEvent(&stMcEvent)
{
}
