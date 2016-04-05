// @(#)root/gui:$Id: TGTab2.cxx,v 1.1 2016/04/05 20:33:01 evpops Exp $
// Author: Fons Rademakers   13/01/98

/*************************************************************************
 * Copyright (C) 1995-2000, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/
/**************************************************************************

    This source is based on Xclass95, a Win95-looking GUI toolkit.
    Copyright (C) 1996, 1997 David Barth, Ricky Ralston, Hector Peraza.

    Xclass95 is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

**************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TGTab2, TGTabElement2, TGTabLayout2                                     //
//                                                                      //
// A tab widget contains a set of composite frames each with a little   //
// tab with a name (like a set of folders with tabs).                   //
//                                                                      //
// The TGTab2 is user callable. The TGTabElement2 and TGTabLayout2 are     //
// service classes of the tab widget.                                   //
//                                                                      //
// Clicking on a tab will bring the associated composite frame to the   //
// front and generate the following event:                              //
// kC_COMMAND, kCM_TAB, tab id, 0.                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TGTab2.h"
#include "TGResourcePool.h"
#include "TList.h"
#include "Riostream.h"
#include "TClass.h"
#include "TGPicture.h"

const TGFont *TGTab2::fgDefaultFont = 0;
const TGGC   *TGTab2::fgDefaultGC = 0;

ClassImp(TGTabElement2)
ClassImp(TGTabLayout2)
ClassImp(TGTab2)


//______________________________________________________________________________
TGTabElement2::TGTabElement2(const TGWindow *p, TGString *text, UInt_t w, UInt_t h,
                           GContext_t norm, FontStruct_t font,
                           UInt_t options, ULong_t back) :
   TGFrame(p, w, h, options, back)
{
   // Create a tab element. Text is adopted by tab element.

   fClosePic     = 0;
   fClosePicD    = 0;
   fShowClose    = kFALSE;
   fActive       = kFALSE;
   fText         = text;
   fBorderWidth  = 0;
   fNormGC       = norm;
   fFontStruct   = font;
   fEditDisabled = kEditDisableGrab | kEditDisableBtnEnable;

   fClosePic      = fClient->GetPicture("closetab.png");
   fClosePicD     = fClient->GetPicture("closetab_d.png");
   int max_ascent, max_descent;
   if (fText)
      fTWidth = gVirtualX->TextWidth(fFontStruct, fText->GetString(), fText->GetLength());
   gVirtualX->GetFontProperties(fFontStruct, max_ascent, max_descent);
   fTHeight = max_ascent + max_descent;

   //printf("%s w=%d\n", fText->Data(), fTWidth+12);

   Resize(TMath::Max(fTWidth+12, (UInt_t)12), fTHeight+6);
   fEnabled = kTRUE;
   gVirtualX->GrabButton(fId, kButton1, kAnyModifier, kButtonPressMask, kNone, kNone);

   ContainerMapped = kFALSE;
   Mapped = kFALSE;
}

//______________________________________________________________________________
TGTabElement2::~TGTabElement2()
{
   // Delete tab element.

   if (fClosePic) gClient->FreePicture(fClosePic);
   if (fClosePicD) gClient->FreePicture(fClosePicD);
   if (fText) delete fText;
}

//______________________________________________________________________________
void TGTabElement2::DrawBorder()
{
   // Draw little tab element.

   gVirtualX->DrawLine(fId, GetHilightGC()(), 0, fHeight-1, 0, 2);
   gVirtualX->DrawLine(fId, GetHilightGC()(), 0, 2, 2, 0);
   gVirtualX->DrawLine(fId, GetHilightGC()(), 2, 0, fWidth-3, 0);
   gVirtualX->DrawLine(fId, GetShadowGC()(),  fWidth-2, 1, fWidth-2, fHeight-1);
   if (gClient->GetStyle() < 2) {
      gVirtualX->DrawLine(fId, GetBlackGC()(), fWidth-2, 1, fWidth-1, 2);
      gVirtualX->DrawLine(fId, GetBlackGC()(), fWidth-1, 2, fWidth-1, fHeight-2);
   }
   gVirtualX->DrawLine(fId, GetHilightGC()(), fWidth-1, fHeight-1, fWidth-1, fHeight-1);

   if (fText) {
      int max_ascent, max_descent;
      gVirtualX->GetFontProperties(fFontStruct, max_ascent, max_descent);
      if (fEnabled) {
         fText->Draw(fId, fNormGC, 6, max_ascent+3);
      } else {
         fText->Draw(fId, GetHilightGC()(), 7, max_ascent + 1);
         fText->Draw(fId, GetShadowGC()(), 6, max_ascent);
      }
   }
   if (fShowClose && fClosePic && fClosePicD) {
      if (fEnabled && fActive)
         fClosePic->Draw(fId, fNormGC, fTWidth+12, fHeight/2-7);
      else 
         fClosePicD->Draw(fId, fNormGC, fTWidth+12, fHeight/2-7);
   }
}

//______________________________________________________________________________
Bool_t TGTabElement2::HandleButton(Event_t *event)
{
    // Handle button event in the tab widget. Basically we only handle
    // button events in the small tabs.

    
    if (event->fType == kButtonPress) {
	TGTab2* main = (TGTab2*)fParent;

	if (main) {
	    //printf("Handle button %s\n", fText->Data());
	    if(strcmp(fText->Data(), "<<") == 0) {
		main->HandleDisplayShift(-1);
		return kTRUE;
	    }
	    else if (strcmp(fText->Data(), ">>") == 0) {
		main->HandleDisplayShift(1);
		return kTRUE;
	    }
	  
	    if (fShowClose && event->fWindow == GetId() &&
		(UInt_t)event->fX > fTWidth+12 && (UInt_t)event->fX < fTWidth+26 &&
		(UInt_t)event->fY > fHeight/2-7 && (UInt_t)event->fY < fHeight/2+7) {
		if (main->GetTabTab(main->GetCurrent()) == this) {
		    main->CloseTab(main->GetCurrent()); // emit signal
		    //main->RemoveTab(main->GetCurrent());
		    return kTRUE;
		}
	    }
	    TGFrameElement *el;
	    TIter next(main->GetList());

	    next();   // skip first container
	    next();   // skip right/left buttons...
	    next();

	    Int_t i = 0;
	    Int_t c = main->GetCurrent();
	    while ((el = (TGFrameElement *) next())) {
		if (el->fFrame->GetId() == (Window_t)event->fWindow)
		    c = i;
		next(); i++;
	    }

	    // change tab and generate event
	    main->SetTab(c);
	}
    }
    return kTRUE;
}

//______________________________________________________________________________
TGDimension TGTabElement2::GetDefaultSize() const
{
   // Return default size of tab element.

   if (fShowClose && fClosePic && fClosePicD)
      return TGDimension(TMath::Max(fTWidth+30, (UInt_t)45), fTHeight+6);
   else
      return TGDimension(TMath::Max(fTWidth+12, (UInt_t)12), fTHeight+6);
}

//______________________________________________________________________________
void TGTabElement2::SetText(TGString *text)
{
   // Set new tab text.

   if (fText) delete fText;
   fText = text;

   int max_ascent, max_descent;
   fTWidth = gVirtualX->TextWidth(fFontStruct, fText->GetString(), fText->GetLength());
   gVirtualX->GetFontProperties(fFontStruct, max_ascent, max_descent);
   fTHeight = max_ascent + max_descent;

   fClient->NeedRedraw(this);
}

//______________________________________________________________________________
void TGTabElement2::ShowClose(Bool_t show)
{
   // Show/hide close icon on the tab element, then apply layout
   // to compute correct elements size.

   TGTab2* main = (TGTab2*)fParent;
   fShowClose = show;
   if (fShowClose && fClosePic && fClosePicD)
      Resize(TMath::Max(fTWidth+30, (UInt_t)45), fTHeight+6);
   else
      Resize(TMath::Max(fTWidth+12, (UInt_t)12), fTHeight+6);
   if (main)
      main->GetLayoutManager()->Layout();
}

//______________________________________________________________________________
TGTabLayout2::TGTabLayout2(TGTab2 *main)
{
   // Create a tab layout manager.

   fMain = main;
   fList = fMain->GetList();

   isLayedOut = 0;

}

//______________________________________________________________________________
void TGTabLayout2::Layout()
{
    Int_t  i;
    UInt_t tw;
    UInt_t tabh = fMain->GetTabHeight();
    UInt_t bw = fMain->GetBorderWidth();
    UInt_t w = fMain->GetWidth();
    UInt_t h = fMain->GetHeight();

    /*
    if(isLayedOut == 0) {
	// Unmap everything except for the outside border...
	TGFrameElement *el, *elnxt;
	TIter next (fList);
	next();
	el = (TGFrameElement *)next(); el->fFrame->UnmapWindow();
	el = (TGFrameElement *)next(); el->fFrame->UnmapWindow();
	while(el = (TGFrameElement *)next()) {
	    elnxt = (TGFrameElement *)next();
	    el->fFrame->UnmapWindow();
	    elnxt->fFrame->UnmapWindow();
	}
    }
    */

    isLayedOut = 1;
    //printf("Laying out tablayout\n");
    //return;

    UInt_t container_w = (w - (bw << 1));
    UInt_t container_h = (h - tabh - (bw << 1));
  
    //  fMain->MapWindow();
    // fMain->GetContainer()->MapWindow();

    if(container_h > 32768) {
	printf("Container_h huge=%d\n", container_h);
	container_h = 1;
    }
    if(container_w > 32768) {
	printf("Container_w huge=%d\n", container_w);
	container_w = 1;
    } 

    Int_t xtab = 2;

    fMain->GetContainer()->MoveResize(0, tabh, w, h - tabh);
   
    // first frame is the container, so take next...
    TGFrameElement *el, *elnxt;
    TIter next(fList);
    i = 0;
    el = (TGFrameElement *)next();   // skip first
    //el->fFrame->MapWindow();

    TGFrameElement *left = (TGFrameElement *)next();
    TGFrameElement *right = (TGFrameElement *)next();

    int display_done = 0;
    int display_started = 0;
    int displayed_right = 0;
   
    // Make sure that the currently displayed button is actually displayed,
    int current = fMain->GetCurrent();
    int tw_right = right->fFrame->GetDefaultWidth();
 
    // If necessary, put left button in place...
    if(fMain->display_shift > 0) {       // Display Left button!
	tw = left->fFrame->GetDefaultWidth();
	left->fFrame->MoveResize(xtab, 2, tw, tabh-1);
	if(!TE2(left->fFrame)->Mapped) {
	    TE2(left->fFrame)->Mapped = kTRUE;
	    left->fFrame->MapWindow();
	}
	left->fFrame->LowerWindow();
	xtab += tw;
    }
    else {
	if(TE2(left->fFrame)->Mapped) {
	    TE2(left->fFrame)->Mapped = kFALSE;
	    left->fFrame->UnmapWindow();
	}
    }

    // el are the tab labels
    // elnxt are the corresponding frames    
    while ((el = (TGFrameElement *) next())) {
	elnxt = (TGFrameElement *) next();

	tw = el->fFrame->GetDefaultWidth();

	if(i >= fMain->display_shift) {
	    display_started = 1;
	}

	// Check if it is time to display the right button!
	if(!displayed_right) {
	    if(xtab + tw + tw_right > w) {
		//printf("xtab %d tw %d tw_right %d w %d\n", xtab, tw, tw_right, w);
		displayed_right = 1;
		display_done = 1;

		right->fFrame->MoveResize(xtab, 2, w - xtab - 2, tabh - 1);
		if(!TE2(right->fFrame)->Mapped) {
		    TE2(right->fFrame)->Mapped = kTRUE;
		    right->fFrame->MapWindow();
		}
		right->fFrame->LowerWindow();
		xtab = w;
	    }
	}

	// Check if content should be displayed
	if(elnxt) {
	    if(i == current) {   
	        elnxt->fFrame->MoveResize(bw, tabh + bw, container_w, container_h);
		if(!TE2(el->fFrame)->ContainerMapped) {
		    TE2(el->fFrame)->ContainerMapped = kTRUE;
		    elnxt->fFrame->MapWindow();
		    elnxt->fFrame->MapSubwindows();
		}
		elnxt->fFrame->RaiseWindow();
	    }
	    else {
		if(TE2(el->fFrame)->ContainerMapped) {
		    TE2(el->fFrame)->ContainerMapped = kFALSE;
		    elnxt->fFrame->UnmapWindow();
		}
	    }
	}
	
	// Check if tab should be displayed!
	if(display_done || !display_started) {    // No!
	  
	    if(TE2(el->fFrame)->Mapped) {
		//printf("Unmapping %d - %d\n",i, current);
		TE2(el->fFrame)->Mapped = kFALSE;
		el->fFrame->UnmapWindow();
	    }
	    
	    //printf("done %d started %d i %d\n",display_done, display_started, i);
	    i++;
	    continue;   
	}

	Int_t tabx = xtab;
	Int_t taby = 2;
	Int_t tabwid = tw;
	Int_t tabhight = tabh-1;
	
	if(i == current) {
	    tabwid = tw + 3;
	    tabhight = tabh + 1;
	    tabx = xtab - 2;
	    taby = 0;
	}
	
	
	el->fFrame->MoveResize(tabx, taby, tabwid, tabhight);
	if(!TE2(el->fFrame)->Mapped) {
	    TE2(el->fFrame)->Mapped = kTRUE;
	    el->fFrame->MapWindow();
	}
	if(i == current) 
	    el->fFrame->RaiseWindow();
	else
	    el->fFrame->LowerWindow();
	
	//printf("tabw = %d, tab=%d\n", tw, xtab);
	xtab += (Int_t)tw;
	i++;
    }

    if(!displayed_right)  {
	if(TE2(right->fFrame)->Mapped) {
	    TE2(right->fFrame)->Mapped = kFALSE;
	    right->fFrame->UnmapWindow();
	}
    }
}

void TGTab2::MapWindow() {
    //printf("Here mapping window\n");
    TGCompositeFrame::MapWindow();
}

void TGTab2::MapSubwindows() {
    //printf("Here mapping subwindows\n");
    //TGCompositeFrame::MapSubwindows();

    if(!fMapSubwindows) {
	return;
    }

    // return;

    // TGWindow::MapSubwindows();

    if(!fList) return;
    
    TGFrameElement *el;
    TIter next(fList);

    el = (TGFrameElement *)next();
    el->fFrame->MapSubwindows();
    //el = next();  // left arrow
    //el = next();  // right arrow

    //printf("layed out yet? %d\n", ((TGTabLayout2 *)GetLayoutManager())->isLayedOut);
}

/* Last version...
void TGTabLayout2::Layout()
{
    // Layout the tab widget.

    //printf("Layout\n");

    Int_t  i, xtab;
    UInt_t tw;
    UInt_t tabh = fMain->GetTabHeight(), bw = fMain->GetBorderWidth();
    UInt_t w = fMain->GetWidth();
    UInt_t h = fMain->GetHeight();

    printf("tabh = %d Width = %d Height = %d\n", tabh, w, h);

    xtab = 2;

    fMain->GetContainer()->MoveResize(0, tabh, w, h - tabh);
   
    // first frame is the container, so take next...
    TGFrameElement *el, *elnxt;
    TIter next(fList);
    i = 0;
    next();   // skip first
    TGFrameElement *left = (TGFrameElement *)next();
    TGFrameElement *right = (TGFrameElement *)next();

    int display_done = 0;
    int display_started = 0;
    int displayed_right = 0;
   
    // Make sure that the currently displayed button is actually displayed,
    int current = fMain->GetCurrent();
    int tw_right = right->fFrame->GetDefaultWidth();
    tw = left->fFrame->GetDefaultWidth();
    printf("tw=%d\n", tw);
    // If necessary, put left button in place...
    if(fMain->display_shift > 0) {   
	tw = left->fFrame->GetDefaultWidth();
	left->fFrame->MoveResize(xtab, 2, tw, tabh-1);
	left->fFrame->LowerWindow();
	left->fFrame->MapWindow();
	xtab += tw;
    }
    else {
	//left->fFrame->MoveResize(50,50,0,0);
	left->fFrame->UnmapWindow();
    }
    //left->fFrame->LowerWindow();	


    // el are the tab labels
    // elnxt are the corresponding frames    
    while ((el = (TGFrameElement *) next())) {
	elnxt = (TGFrameElement *) next();

	tw = el->fFrame->GetDefaultWidth();
	printf("  tw=%d\n", tw);

	if(i >= fMain->display_shift) {
	    display_started = 1;
	}

	if(!displayed_right) {
	    if(xtab + tw + tw_right > w) {
		//printf("Displaying right\n");
		displayed_right = 1;
		display_done = 1;
		right->fFrame->MoveResize(xtab, 2, tw_right, tabh-1);
		right->fFrame->LowerWindow();
	    }
	}

	if(display_done || !display_started) {    // nothing else gets displayed...

	    // Don't display frame

	    //el->fFrame->MoveResize(30,30,0,0);
	    //el->fFrame->LowerWindow();
	    el->fFrame->UnmapWindow();
	    if(elnxt) {
		if(i == current) {
		    UInt_t nw = (w - (bw << 1));
		    if (nw > 32768) nw = 1;
		    UInt_t nh = (h - tabh - (bw << 1));
		    if (nh > 32768) nh = 1;
		    elnxt->fFrame->MapWindow();
		    elnxt->fFrame->MoveResize(bw, tabh + bw, nw, nh);
		    elnxt->fFrame->RaiseWindow();
		}
		else {
		    // Don't display...
		    //elnxt->fFrame->MoveResize(30,30,10,10);
		    elnxt->fFrame->UnmapWindow();
		    //elnxt->fFrame->Layout();
		}
	    }
	    i++;
	    continue;
	}

	if (i == current) {
	    el->fFrame->MapWindow();
	    el->fFrame->MoveResize(xtab-2, 0, tw+3, tabh+1);
	    if (elnxt) elnxt->fFrame->RaiseWindow();
	    el->fFrame->RaiseWindow();
	} else {
	    el->fFrame->MapWindow();
	    el->fFrame->MoveResize(xtab, 2, tw, tabh-1);
	    el->fFrame->LowerWindow();
	}

	UInt_t nw = (w - (bw << 1));
	if (nw > 32768) nw = 1;
	UInt_t nh = (h - tabh - (bw << 1));
	if (nh > 32768) nh = 1;
	if (elnxt) {
	    elnxt->fFrame->MoveResize(bw, tabh + bw, nw, nh);
	    elnxt->fFrame->Layout();
	}
	xtab += (Int_t)tw;
	i++;
    }

    if(!displayed_right)  {
	right->fFrame->UnmapWindow();
	//right->fFrame->MoveResize(30,30,0,0);
	//right->fFrame->LowerWindow();
    }
}
*/

//______________________________________________________________________________
TGDimension TGTabLayout2::GetDefaultSize() const
{
    // Get default size of tab widget.
    /*
      TGDimension dsize, dsize_te;
      TGDimension size(0,0), size_te(0,0);

      TGFrameElement *el, *elnxt;
      TIter next(fList);
      next();   // skip first container
      next();
      next();

      while ((el = (TGFrameElement *)next())) {
      dsize_te = el->fFrame->GetDefaultSize();
      size_te.fWidth += dsize_te.fWidth;
      elnxt = (TGFrameElement *) next();
      if (elnxt) {
      dsize = elnxt->fFrame->GetDefaultSize();
      if (size.fWidth < dsize.fWidth) size.fWidth = dsize.fWidth;
      if (size.fHeight < dsize.fHeight) size.fHeight = dsize.fHeight;
      }
      }

      // check if tab elements make a larger width than the containers
      if (size.fWidth < size_te.fWidth) size.fWidth = size_te.fWidth;

      size.fWidth += fMain->GetBorderWidth() << 1;
 
      size.fHeight += fMain->GetTabHeight() + (fMain->GetBorderWidth() << 1);
    */

    TGDimension size(0,0);
    size.fWidth = fMain->GetWidth();
    size.fHeight = fMain->GetHeight();
     
    //printf("x=%d y=%d\n", size.fWidth, size.fHeight);

    return size;
    /*

      UInt_t x = 100;
      UInt_t y = 100;
      if(fMain) {
      UInt_t x = fMain->GetDefaultWidth();
      UInt_t y = fMain->GetDefaultHeight();
      }

      printf("x=%d y=%d\n", x, y);
   
      TGDimension size(x,y);
      return size;
    */
}


//______________________________________________________________________________
TGTab2::TGTab2(const TGWindow *p, UInt_t w, UInt_t h,
             GContext_t norm, FontStruct_t font,
             UInt_t options, ULong_t back) :
   TGCompositeFrame(p, w, h, options, back)
{
   // Create tab widget.

   fMsgWindow  = p;

   fBorderWidth = 2;
   fCurrent     = 0;
   fRemoved     = new TList;

   fNormGC     = norm;
   fFontStruct = font;

   display_shift = 0;
   rightTabIdx = -1;
   
   int max_ascent, max_descent;
   gVirtualX->GetFontProperties(fFontStruct, max_ascent, max_descent);
   fTabh = max_ascent + max_descent + 6;

   SetLayoutManager(new TGTabLayout2(this));

   // we need this in order to avoid border blinking when switching tabs...
   fContainer = new TGCompositeFrame(this, fWidth, fHeight - fTabh,
                       kVerticalFrame | kRaisedFrame | kDoubleBorder);
   AddFrame(fContainer, 0);

   AddFrame(new TGTabElement2(this, new TGString("<<"), 12, 20, fNormGC, fFontStruct));
   AddFrame(new TGTabElement2(this, new TGString(">>"), 12, 20, fNormGC, fFontStruct));

   fEditDisabled = kEditDisable | kEditDisableLayout;
   fContainer->SetEditDisabled(kEditDisable | kEditDisableGrab);
}

//______________________________________________________________________________
TGTab2::~TGTab2()
{
   // Delete tab widget. This deletes the tab windows and the containers.
   // The tab string is deleted by the TGTabElement2 dtor.

    //printf("aa\n");
   Cleanup();
   //printf("bb\n");
   fRemoved->Delete();
   //printf("cc\n");
   delete fRemoved;
   //printf("dd\n");
}

//______________________________________________________________________________
TGCompositeFrame *TGTab2::AddTab(TGString *text)
{
   // Add a tab to the tab widget. Returns the new container, which
   // is owned by the tab widget. The text is adopted by the tab widget.

   TGTabElement2 *te = new TGTabElement2(this, text, 50, 20, fNormGC, fFontStruct);
   AddFrame(te, 0);
   
   TGCompositeFrame* cf = new TGCompositeFrame(this, fWidth, fHeight-21);
   AddFrame(cf, 0);
   //cf->SetEditDisabled(kEditDisableResize);

   //te->MapWindow();
   //cf->MapWindow();

   return cf;
}

//______________________________________________________________________________
TGCompositeFrame *TGTab2::AddTab(const char *text)
{
   // Add a tab to the tab widget. Returns the new container. The container
   // is owned by the tab widget.

   return AddTab(new TGString(text));
}

//______________________________________________________________________________
void TGTab2::AddTab(const char *text, TGCompositeFrame *cf)
{
   // Add a tab to the tab widget and fill it with given TGCompositeFrame.

   AddTab(new TGString(text), cf);
}

//______________________________________________________________________________
void TGTab2::AddTab(TGString *text, TGCompositeFrame *cf)
{
   // Add a tab to the tab widget and fill it with given TGCompositeFrame.

   TGTabElement2 *te = new TGTabElement2(this, text, 50, 20, fNormGC, fFontStruct);
   AddFrame(te, 0);

   AddFrame(cf, 0);
   // cf->SetEditDisabled(kEditDisableResize);

   //te->MapWindow();
   //cf->MapWindow();
}

//______________________________________________________________________________
void TGTab2::RemoveTab(Int_t tabIndex, Bool_t storeRemoved)
{
   // Remove container and tab of tab with index tabIndex.
   // Does NOT remove the container contents!
    
   if (tabIndex < 0) {
      tabIndex = fCurrent;
   }

   TGFrameElement *elTab, *elCont;
   Int_t  count = 0;

   // Notify (signal) for removed tab "tabIndex"
   Removed(tabIndex);

   TIter next(fList) ;
   next() ; // skip first container
   next();
   next();

   while ((elTab = (TGFrameElement *) next())) {
      elCont = (TGFrameElement *) next();

      if (count == tabIndex) {
         elCont->fFrame->UnmapWindow();   // will be destroyed later
         TGFrame *frame = elTab->fFrame;
         RemoveFrame(elTab->fFrame);
         frame->DestroyWindow();
         delete frame;
         if (storeRemoved)
            fRemoved->Add(elCont->fFrame);   // delete only in dtor
         RemoveFrame(elCont->fFrame);
         if (tabIndex == fCurrent) {
            // select another tab only if the current is the one we delete
            SetTab(0);
         } else
            fCurrent--;
         break;
      }
      count++;
   }

   GetLayoutManager()->Layout();
}

//______________________________________________________________________________
void TGTab2::SetEnabled(Int_t tabIndex, Bool_t on)
{
   // Enable or disable tab.

   TGTabElement2 *te = GetTabTab(tabIndex);
   if (te) {
      te->SetEnabled(on);
      fClient->NeedRedraw(te);
   }
}

//______________________________________________________________________________
Bool_t TGTab2::IsEnabled(Int_t tabIndex) const
{
   // Returns true if tab is enabled.

   TGTabElement2 *te = GetTabTab(tabIndex);

   return te ? te->IsEnabled() : kFALSE;
}

void TGTab2::HandleDisplayShift(Int_t dir) {
    display_shift += dir;
    GetLayoutManager()->Layout();
}

//______________________________________________________________________________
void TGTab2::ChangeTab(Int_t tabIndex, Bool_t emit)
{
    // Make tabIdx the current tab. Utility method called by SetTab and
    // HandleButton().

    TGTabElement2 *te = GetTabTab(tabIndex);
    if (!te || !te->IsEnabled()) return;

    if (tabIndex != fCurrent) {
	if (GetTabTab(fCurrent)) {
	    GetTabTab(fCurrent)->SetActive(kFALSE);
	    fClient->NeedRedraw(GetTabTab(fCurrent));
	}

	fCurrent = tabIndex;

	GetLayoutManager()->Layout();
	/*
	TGFrameElement *el, *elnxt;
	UInt_t tw;
	Int_t  xtab  = 2;
	Int_t  count = 0;

	TIter next(fList);
	next();           // skip first container
	TGFrameElement *left = (TGFrameElement *)next();
	TGFrameElement *right = (TGFrameElement *)next();

	fCurrent = tabIndex;
	while ((el = (TGFrameElement *) next())) {
	    elnxt = (TGFrameElement *) next();
	    tw = el->fFrame->GetDefaultWidth();
	    if (count == fCurrent) {
		el->fFrame->MoveResize(xtab-2, 0, tw+3, fTabh+1);
		if (elnxt) elnxt->fFrame->RaiseWindow();
		el->fFrame->RaiseWindow();
	    } else {
		if(count > 6) {
		    el->fFrame->MoveResize(50,50,0,0);
		}
		else {
		    el->fFrame->MoveResize(xtab, 2, tw, fTabh-1);
		}
		el->fFrame->LowerWindow();
	    }
	    xtab += tw;
	    count++;
	}

	*/
	if (emit) {
	    SendMessage(fMsgWindow, MK_MSG(kC_COMMAND, kCM_TAB), fCurrent, 0);
	    fClient->ProcessLine(fCommand, MK_MSG(kC_COMMAND, kCM_TAB), fCurrent, 0);
	    Selected(fCurrent);
	}
	GetTabTab(fCurrent)->SetActive(kTRUE);
	fClient->NeedRedraw(GetTabTab(fCurrent));
    }
}

//______________________________________________________________________________
Bool_t TGTab2::SetTab(Int_t tabIndex, Bool_t emit)
{
   // Brings the composite frame with the index tabIndex to the
   // front and generate the following event if the front tab has changed:
   // kC_COMMAND, kCM_TAB, tab id, 0.
   // Returns kFALSE if tabIndex is a not valid index

   // check if tabIndex is a valid index
   if (tabIndex < 0)
      return kFALSE;

   // count the tabs
   TIter next(fList);
   next();
   next();
   next();

   Int_t count = 0;
   while (next())
      count++;

   count = count / 2;
   if (tabIndex > count)
      return kFALSE;

   // change tab and generate event
   ChangeTab(tabIndex, emit);

   return kTRUE;
}

//______________________________________________________________________________
Bool_t TGTab2::SetTab(const char *name, Bool_t emit)
{
   // Brings the composite frame with the name to the
   // front and generate the following event if the front tab has changed:
   // kC_COMMAND, kCM_TAB, tab id, 0.
   // Returns kFALSE if tab with name does not exist.

   TGFrameElement *el;
   Int_t  count = 0;
   TGTabElement2 *tab = 0;

   TIter next(fList);
   next();           // skip first container
   next();
   next();

   while ((el = (TGFrameElement *) next())) {
      next();        // skip tab containter
      tab = (TGTabElement2 *)el->fFrame;

      if (*(tab->GetText()) == name) {
         // change tab and generate event
         ChangeTab(count, emit);
         return kTRUE;
      }
      count++;
   }

   return kFALSE;
}

//______________________________________________________________________________
TGCompositeFrame *TGTab2::GetTabContainer(Int_t tabIndex) const
{
   // Return container of tab with index tabIndex.
   // Return 0 in case tabIndex is out of range.

   if (tabIndex < 0) return 0;

   TGFrameElement *el;
   Int_t  count = 0;

   TIter next(fList);
   next();           // skip first container
   next();
   next();

   while (next()) {
      el = (TGFrameElement *) next();
      if (el && count == tabIndex)
         return (TGCompositeFrame *) el->fFrame;
      count++;
   }

   return 0;
}

//______________________________________________________________________________
TGCompositeFrame *TGTab2::GetTabContainer(const char *name) const
{
   // Return the tab container of tab with string name.
   // Returns 0 in case name is not found.

   TGFrameElement *el;
   TGTabElement2 *tab = 0;
   TGCompositeFrame *comp = 0;

   TIter next(fList);
   next();
   next();
   next();

   while ((el = (TGFrameElement *) next())) {
      tab  = (TGTabElement2 *) el->fFrame;
      el   = (TGFrameElement *) next();
      comp = (TGCompositeFrame *) el->fFrame;
      if (*tab->GetText() == name){
         return comp;
      }
   }

   return 0;
}

//______________________________________________________________________________
TGTabElement2 *TGTab2::GetTabTab(Int_t tabIndex) const
{
   // Return the tab element of tab with index tabIndex.
   // Returns 0 in case tabIndex is out of range.

   if (tabIndex < 0) return 0;

   TGFrameElement *el;
   Int_t  count = 0;

   TIter next(fList);
   next();           // skip first container
   next();
   next();

   while ((el = (TGFrameElement *) next())) {
      next();
      if (count == tabIndex)
         return (TGTabElement2 *) el->fFrame;
      count++;
   }

   return 0;
}

//______________________________________________________________________________
TGTabElement2 *TGTab2::GetTabTab(const char *name) const
{
   // Return the tab element of tab with string name.
   // Returns 0 in case name is not found.

   TGFrameElement *el;
   TGTabElement2 *tab = 0;

   TIter next(fList);
   next();
   next();
   next();

   while ((el = (TGFrameElement *) next())) {
      tab = (TGTabElement2 *)el->fFrame;
      if (name == *(tab->GetText())) {
         return tab;
      }
      next();
   }

   return 0;
}

//______________________________________________________________________________
Int_t TGTab2::GetNumberOfTabs() const
{
   // Return number of tabs.

   Int_t count = 0;

   TIter next(fList);
   next();           // skip first container
   next();
   next();

   while (next()) {
      next();
      count++;
   }

   return count;
}

//______________________________________________________________________________
FontStruct_t TGTab2::GetDefaultFontStruct()
{
   // Return default font structure in use.

   if (!fgDefaultFont)
      fgDefaultFont = gClient->GetResourcePool()->GetDefaultFont();
   return fgDefaultFont->GetFontStruct();
}

//______________________________________________________________________________
const TGGC &TGTab2::GetDefaultGC()
{
   // Return default graphics context in use.

   if (!fgDefaultGC)
      fgDefaultGC = gClient->GetResourcePool()->GetFrameGC();
   return *fgDefaultGC;
}

//______________________________________________________________________________
void TGTab2::NewTab(const char *text)
{
   // Create new tab. Used in context menu.

   TString name;
   if (text)
      name = text;
   else
      name = TString::Format("tab%d", GetNumberOfTabs()+1);
   AddTab(name.Data());

   GetLayoutManager()->Layout();
}

//______________________________________________________________________________
void TGTab2::SetText(const char *text)
{
   // Set text to current tab.

   if (GetCurrentTab()) GetCurrentTab()->SetText(new TGString(text));
   GetLayoutManager()->Layout();
}

//______________________________________________________________________________
TGLayoutManager *TGTab2::GetLayoutManager() const
{
   // Return layout manager.

   TGTab2 *tab = (TGTab2*)this;

   if (tab->fLayoutManager->IsA() != TGTabLayout2::Class()) {
      tab->SetLayoutManager(new TGTabLayout2(tab));
   }

   return tab->fLayoutManager;
}

//______________________________________________________________________________
void TGTab2::SavePrimitive(ostream &out, Option_t *option /*= ""*/)
{
   // Save a tab widget as a C++ statement(s) on output stream out.

   char quote = '"';

   // font + GC
   option = GetName()+5;         // unique digit id of the name
   TString parGC, parFont;
   parFont.Form("%s::GetDefaultFontStruct()",IsA()->GetName());
   parGC.Form("%s::GetDefaultGC()()",IsA()->GetName());

   if ((GetDefaultFontStruct() != fFontStruct) || (GetDefaultGC()() != fNormGC)) {
      TGFont *ufont = gClient->GetResourcePool()->GetFontPool()->FindFont(fFontStruct);
      if (ufont) {
         ufont->SavePrimitive(out, option);
         parFont.Form("ufont->GetFontStruct()");
      }

      TGGC *userGC = gClient->GetResourcePool()->GetGCPool()->FindGC(fNormGC);
      if (userGC) {
         userGC->SavePrimitive(out, option);
         parGC.Form("uGC->GetGC()");
      }
   }

   if (fBackground != GetDefaultFrameBackground()) SaveUserColor(out, option);

   out << endl << "   // tab widget" << endl;

   out << "   TGTab2 *";
   out << GetName() << " = new TGTab2(" << fParent->GetName()
       << "," << GetWidth() << "," << GetHeight();

   if (fBackground == GetDefaultFrameBackground()) {
      if (GetOptions() == kChildFrame) {
         if (fFontStruct == GetDefaultFontStruct()) {
            if (fNormGC == GetDefaultGC()()) {
               out <<");" << endl;
            } else {
               out << "," << parGC.Data() <<");" << endl;
            }
         } else {
            out << "," << parGC.Data() << "," << parFont.Data() <<");" << endl;
         }
      } else {
         out << "," << parGC.Data() << "," << parFont.Data() << "," << GetOptionString() <<");" << endl;
      }
   } else {
      out << "," << parGC.Data() << "," << parFont.Data() << "," << GetOptionString()  << ",ucolor);" << endl;
   }
   if (option && strstr(option, "keep_names"))
      out << "   " << GetName() << "->SetName(\"" << GetName() << "\");" << endl;

   TGCompositeFrame *cf;
   TGLayoutManager * lm;
   for (Int_t i=0; i<GetNumberOfTabs(); i++) {
      cf = GetTabContainer(i);
      if (!cf || !GetTabTab(i)) continue;
      out << endl << "   // container of " << quote
          << GetTabTab(i)->GetString() << quote << endl;
      out << "   TGCompositeFrame *" << cf->GetName() << ";" << endl;
      out << "   " << cf->GetName() << " = " << GetName()
                   << "->AddTab(" << quote << GetTabTab(i)->GetString()
                   << quote << ");" << endl;
      lm = cf->GetLayoutManager();
      if (lm) {
         if ((cf->GetOptions() & kHorizontalFrame) &&
            (lm->InheritsFrom(TGHorizontalLayout::Class()))) {
            ;
         } else if ((GetOptions() & kVerticalFrame) &&
            (lm->InheritsFrom(TGVerticalLayout::Class()))) {
            ;
         } else {
            out << "   " << cf->GetName() <<"->SetLayoutManager(";
            lm->SavePrimitive(out, option);
            out << ");" << endl;
         }
         if (!IsEnabled(i)) {
            out << "   " << GetName() << "->SetEnabled(" << i << ", kFALSE);" << endl;
         }
      }
      cf->SavePrimitiveSubframes(out, option);

      if (GetTabTab(i)->IsCloseShown()) {
         out << "   TGTabElement2 *tab" << i << " = "
             << GetName() << "->GetTabTab(" << i << ");" << endl;
         out << "   tab" << i << "->ShowClose(kTRUE);" << endl;
      }
      if (GetTabTab(i)->GetBackground() != GetTabTab(i)->GetDefaultFrameBackground()) {
         GetTabTab(i)->SaveUserColor(out, option);
         out << "   TGTabElement2 *tab" << i << " = "
             << GetName() << "->GetTabTab(" << i << ");" << endl;
         out << "   tab" << i << "->ChangeBackground(ucolor);" << endl;
      }

   }
   out << endl << "   " << GetName() << "->SetTab(" << GetCurrent() << ");" << endl;
   out << endl << "   " << GetName() << "->Resize(" << GetName()
       << "->GetDefaultSize());" << endl;
}

// __________________________________________________________________________
void TGTabLayout2::SavePrimitive(ostream &out, Option_t * /*= ""*/)
{
   // Save tab layout manager as a C++ statement(s) on out stream.

   out << "new TGTabLayout2(" << fMain->GetName() << ")";

}
