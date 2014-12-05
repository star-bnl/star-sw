#include "TChain.h"

Int_t gHftGuiEventCounter = 0;

void process_event(TChain &fhtree, Int_t iEvt);


class GuiEventHandler : public TObject
{
public:

   GuiEventHandler(TChain &hftree) : TObject(), fHftree(hftree) {}

   void Fwd() {
      process_event(fHftree, ++gHftGuiEventCounter);
   }

   void Bck() {
      gHftGuiEventCounter--;
      process_event(fHftree, gHftGuiEventCounter);
   }

private:

   TChain &fHftree;

   ClassDef(GuiEventHandler, 0)
};
