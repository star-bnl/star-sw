// @(#)root/gui:$Id: TGTab2.h,v 1.1 2016/04/05 20:33:01 evpops Exp $
// Author: Fons Rademakers   13/01/98

/*************************************************************************
 * Copyright (C) 1995-2000, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TGTab2
#define ROOT_TGTab2


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TGTab2, TGTabElement2, TGTabLayout2                                     //
//                                                                      //
// A tab widget contains a set of composite frames each with a little   //
// tab with a name (like a set of folders with tabs).                   //
//                                                                      //
// The TGTab2 is user callable. The TGTabElement2 and TGTabLayout2 are     //
// is a service classes of the tab widget.                              //
//                                                                      //
// Clicking on a tab will bring the associated composite frame to the   //
// front and generate the following event:                              //
// kC_COMMAND, kCM_TAB, tab id, 0.                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TGFrame
#include "TGFrame.h"
#endif
#ifndef ROOT_TGWidget
#include "TGWidget.h"
#endif

//#include "RQ_OBJECT.h"

#define TE2(x) ((TGTabElement2 *)(x))
class TList;
class TGTabElement2;
class TGTab2;
class TGPicture;

class TGTabLayout2 : public TGLayoutManager {

protected:
   TGTab2    *fMain;      // container frame
   TList    *fList;      // list of frames to arrange

private:
   TGTabLayout2(const TGTabLayout2&);             // not implemented
   TGTabLayout2& operator=(const TGTabLayout2&);  // not implemented

public:
   TGTabLayout2(TGTab2 *main);

   virtual void Layout();
   virtual TGDimension GetDefaultSize() const;
   virtual void SavePrimitive(ostream &out, Option_t *option = "");

   int isLayedOut;

   ClassDef(TGTabLayout2,0)  // Layout manager for TGTab widget
};



class TGTab2 : public TGCompositeFrame, public TGWidget {
    //RQ_OBJECT("TGTab2");

protected:
   Int_t               fCurrent;        // index of current tab
   UInt_t              fTabh;           // tab height
   TGCompositeFrame   *fContainer;      // main container
   TList              *fRemoved;        // list of removed tabs
   FontStruct_t        fFontStruct;     // font
   GContext_t          fNormGC;         // drawing context

   void ChangeTab(Int_t tabIndex, Bool_t emit=kTRUE);
 

   static const TGFont *fgDefaultFont;
   static const TGGC   *fgDefaultGC;

private:
   TGTab2(const TGTab2&);             // not implemented
   TGTab2& operator=(const TGTab2&);  // not implemented

public:
   static FontStruct_t  GetDefaultFontStruct();
   static const TGGC   &GetDefaultGC();

   void HandleDisplayShift(Int_t direction);

   Int_t display_shift;
   Int_t rightTabIdx;
   
   void MapWindow();
   void MapSubwindows();  // override mapsubwindows...

   TGTab2(const TGWindow *p = 0, UInt_t w = 1, UInt_t h = 1,
         GContext_t norm = GetDefaultGC()(),
         FontStruct_t font = GetDefaultFontStruct(),
         UInt_t options = kChildFrame,
         Pixel_t back = GetDefaultFrameBackground());
   virtual ~TGTab2();

   virtual TGCompositeFrame *AddTab(TGString *text);
   virtual TGCompositeFrame *AddTab(const char *text);
   virtual void              AddTab(const char *text, TGCompositeFrame *cf);
   virtual void              AddTab(TGString *text, TGCompositeFrame *cf);

   virtual void              NewTab(const char *text = "tab");   // *MENU*icon=bld_newtab.png*
   virtual void              RemoveTab(Int_t tabIndex = -1,
                                       Bool_t storeRemoved = kTRUE); // *MENU*icon=bld_removetab.png*
   virtual Bool_t            SetTab(Int_t tabIndex, Bool_t emit = kTRUE);
   virtual Bool_t            SetTab(const char *name, Bool_t emit = kTRUE);
   virtual void              DrawBorder() { }

   TGCompositeFrame *GetContainer() const { return fContainer; }
   Int_t             GetCurrent() const { return fCurrent; }
   TGCompositeFrame *GetTabContainer(Int_t tabIndex) const;
   TGCompositeFrame *GetTabContainer(const char *name) const;
   TGTabElement2     *GetTabTab(Int_t tabIndex) const;
   TGTabElement2     *GetTabTab(const char *name) const;
   TGCompositeFrame *GetCurrentContainer() const { return GetTabContainer(fCurrent); }
   TGTabElement2     *GetCurrentTab() const { return GetTabTab(fCurrent); }
   UInt_t            GetTabHeight() const { return fTabh; }
   Int_t             GetNumberOfTabs() const;
   virtual void      SetEnabled(Int_t tabIndex, Bool_t on = kTRUE);  //*MENU*
   virtual void      SetText(const char *text = "tab");              //*MENU*icon=bld_rename.png*
   Bool_t            IsEnabled(Int_t tabIndex) const;

   virtual void      SavePrimitive(ostream &out, Option_t *option = "");

   virtual void CloseTab(Int_t id) { Emit("CloseTab(Int_t)", id); }  //*SIGNAL*
   virtual void Removed(Int_t id) { Emit("Removed(Int_t)", id); }    //*SIGNAL*
   virtual void Selected(Int_t id) { Emit("Selected(Int_t)", id); }  //*SIGNAL*
   virtual TGLayoutManager *GetLayoutManager() const;

   ClassDef(TGTab2,0)  // Tab widget
};



class TGTabElement2 : public TGFrame {

protected:
   TGString        *fText;            // text on tab
   const TGPicture *fClosePic;        // "close tab" icon
   const TGPicture *fClosePicD;       // "close tab" icon (disabled)
   GContext_t       fNormGC;          // graphics context for drawing tab
   FontStruct_t     fFontStruct;      // font used for tab
   UInt_t           fTWidth;          // width of tab text
   UInt_t           fTHeight;         // height of tab text
   Bool_t           fEnabled;         // enabled or disabled
   Bool_t           fShowClose;       // show or hide close icon
   Bool_t           fActive;          // true if active (in front)

 public:
   Bool_t           ContainerMapped;
   Bool_t           Mapped;
private:
   TGTabElement2(const TGTabElement2&);             // not implemented
   TGTabElement2& operator=(const TGTabElement2&);  // not implemented

public:
   TGTabElement2(const TGWindow *p = 0, TGString *text = 0, UInt_t w = 1, UInt_t h = 1,
                GContext_t norm = TGTab2::GetDefaultGC()(),
                FontStruct_t font = TGTab2::GetDefaultFontStruct(),
                UInt_t options = kRaisedFrame,
                Pixel_t back = GetDefaultFrameBackground());
   virtual ~TGTabElement2();

   virtual void        DrawBorder();
   virtual TGDimension GetDefaultSize() const;
   const TGString     *GetText() const { return fText; }
   const char         *GetString() const { return fText->GetString(); }
   virtual Bool_t      HandleButton(Event_t *event);
   void                SetText(TGString *text);
   virtual void        SetEnabled(Bool_t on = kTRUE) { fEnabled = on; }
   Bool_t              IsEnabled() const { return fEnabled; }
   virtual void        SetEditDisabled(UInt_t) {}
   virtual void        ShowClose(Bool_t on = kTRUE);
   Bool_t              IsCloseShown() const { return fShowClose; }
   virtual void        SetActive(Bool_t on = kTRUE) { fActive = on; }
   Bool_t              IsActive() const { return fActive; }

   ClassDef(TGTabElement2,0)  // Little tab on tab widget
};

#endif
