#include <stdexcept>
#include <Stiostream.h>
//root
#include "TRootEmbeddedCanvas.h"
#include "TShape.h"
#include "TBRIK.h"
#include "TVolume.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TPaveLabel.h"
#include "TileFrame.h"

/// Create tile view container. Used to show colormap.
TileFrame::TileFrame(const TGWindow *p) :
  TGCompositeFrame(p, 10, 10, kHorizontalFrame, GetWhitePixel())
{
  fCanvas = 0;
  SetLayoutManager(new TGTileLayout(this, 8));
  // Handle only buttons 4 and 5 used by the wheel mouse to scroll
  gVirtualX->GrabButton(fId, kButton4, kAnyModifier,kButtonPressMask | kButtonReleaseMask,kNone, kNone);
  gVirtualX->GrabButton(fId, kButton5, kAnyModifier,kButtonPressMask | kButtonReleaseMask,kNone, kNone);
}

/// Handle wheel mouse to scroll.
Bool_t TileFrame::HandleButton(Event_t *event)
{
  Int_t page = 0;
  if (event->fCode == kButton4 || event->fCode == kButton5) 
    {
      if (!fCanvas) return kTRUE;
      if (fCanvas->GetContainer()->GetHeight())
	page = Int_t(Float_t(fCanvas->GetViewPort()->GetHeight() *
			     fCanvas->GetViewPort()->GetHeight()) /
		     fCanvas->GetContainer()->GetHeight());
    }
  Int_t newpos;
  switch (event->fCode)
    {
    case kButton4 :      //scroll up
      newpos = fCanvas->GetVsbPosition() - page;
      if (newpos < 0) newpos = 0;
      fCanvas->SetVsbPosition(newpos);
      break;
    case kButton5:      // scroll down
      newpos = fCanvas->GetVsbPosition() + page;
      fCanvas->SetVsbPosition(newpos);
      break;
    }
  return kTRUE;
}

