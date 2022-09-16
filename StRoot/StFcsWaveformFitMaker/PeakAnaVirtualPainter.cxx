#include "PeakAnaVirtualPainter.h"

TClass* PeakAnaVirtualPainter::mpa_Painter = 0;

ClassImp(PeakAnaVirtualPainter);

PeakAnaVirtualPainter::PeakAnaVirtualPainter()
{}

PeakAnaVirtualPainter::~PeakAnaVirtualPainter()
{}

PeakAnaVirtualPainter* PeakAnaVirtualPainter::MakePainter(PeakAna* ana)
{
  // if no painter set yet, create a default painter via the PluginManager
  if (!mpa_Painter) {
    /*
    TPluginHandler *h;
    if ((h = gROOT->GetPluginManager()->FindHandler("PeakAnaVirtualPainter"))) {
      if (h->LoadPlugin() == -1){return 0;}
      PeakAnaVirtualPainter::SetPainter(h->GetClass());
      if (!mpa_Painter){ return 0; }
    }
    */
    /*@[March 11, 2022]
    > In order to use the plugin handler for this class need to add it using the plugin handler. This can be done with the command below.
    > `gPluginMgr->AddHandler("Rtools@PeakAnaVirtualPainter","*","PeakAnaPainter","PeakAnaPainter()");`
    > Once the plugin is added to the handler then the command `gPluginMgr->FindHandler("PeakAnaVirtualPainter")` will no longer return a null pointer for the handler.
    > So in order for this to work with plugins I need to call the `gPluginMgr->AddHandler()` command before ROOT starts.
    > Since I only care about one kind of painter for now I will just give the name of the class here
    > NOTE: 'gPluginMgr'=='gROOT->GetPluginManager()'
    > Ref:"https://root.cern.ch/doc/master/classTPluginManager.html"
    */
    PeakAnaVirtualPainter::SetPainter("PeakAnaPainter");
    if( mpa_Painter==0 ){
      // mpa_Painter is still null
      return 0;
    }
  }
 
  //create an instance of the histogram painter
  PeakAnaVirtualPainter *p = (PeakAnaVirtualPainter*)mpa_Painter->New();
  if (p) p->SetPeakAna(ana);
  return p;
}

void PeakAnaVirtualPainter::SetPainter(const char* painter)
{
  mpa_Painter = TClass::GetClass(painter);
}
