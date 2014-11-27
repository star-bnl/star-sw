// @(#)root/base:$Name:  $:$Id: TCanvasImp.interface.h,v 1.4 2013/08/30 16:00:19 perev Exp $
// Author: Valeri Fine   25/05/2004

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TCanvasImp                                                           //
//                                                                      //
// ABC describing GUI independent main window (with menubar, scrollbars //
// and a drawing area).                                                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifdef ROOT_FROM_CVS_HEAD
#  undef ROOT_RELEASE 
#  undef ROOT_VERSION_CODE
#  define ROOT_RELEASE "4.00/05"
#  define ROOT_VERSION_CODE 262149
#endif   

protected:

   virtual void   Lock();
   virtual void   Unlock();
   virtual Bool_t IsLocked();

 public:

   virtual void   Close();
   virtual void   ForceUpdate();
#if ROOT_VERSION_CODE > ROOT_VERSION(4,00,4)
   virtual UInt_t GetWindowGeometry(Int_t &x, Int_t &y, UInt_t &w, UInt_t &h);
#else
   virtual void   GetWindowGeometry(Int_t &x, Int_t &y, UInt_t &w, UInt_t &h);
#endif   
   virtual void   Iconify();
   virtual Int_t  InitWindow();
   virtual void   SetStatusText(const char *text = 0, Int_t partidx = 0);
   virtual void   SetWindowPosition(Int_t x, Int_t y);
   virtual void   SetWindowSize(UInt_t w, UInt_t h);
   virtual void   SetWindowTitle(const char *newTitle);
   virtual void   SetCanvasSize(UInt_t w, UInt_t h);
   virtual void   Show();
   virtual void   ShowMenuBar(Bool_t show = kTRUE);
   virtual void   ShowStatusBar(Bool_t show = kTRUE);
   virtual void   RaiseWindow(); 
   virtual void   ReallyDelete();

   virtual void   ShowEditor(Bool_t show = kTRUE);
   virtual void   ShowToolBar(Bool_t show = kTRUE);   
   virtual void   ShowToolTips(Bool_t show = kTRUE);
   
   
   virtual Bool_t HasEditor() const;
   virtual Bool_t HasMenuBar() const;
   virtual Bool_t HasStatusBar() const ;
   virtual Bool_t HasToolBar() const;
   virtual Bool_t HasToolTips() const;
   

   // New methods 12.12.2005
   // void TCanvasImp::RaiseWindow() { ; }
