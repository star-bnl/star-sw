/***************************************************************************
 *
 * $Id: StSvtMenuBar.hh,v 1.1 2004/02/06 02:30:36 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT GUI Menu Bar
 *
 ***************************************************************************
 *
 * $Log: StSvtMenuBar.hh,v $
 * Revision 1.1  2004/02/06 02:30:36  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/
 
#ifndef STSVTMENUBAR_H
#define STSVTMENUBAR_H 

#include <TGMenu.h>

class TGPopupMenu;
class TGLayoutHints;
class TCanvas;

class StSvtMonitor;
class StSvtGuiMonitor;
class StChain;
class StBFChain;
class StEventDisplayMaker;
class StTrackFilter;
 
class StSvtMenuBar : public TGMenuBar {
 
private: 
  TGPopupMenu        *fMenuSource;        //!   
  TGPopupMenu        *fMenuStat;          //!   
  TGPopupMenu        *fMenuBadAn;         //!   
  TGPopupMenu        *fMenuControl;       //!   
  TGPopupMenu        *fMenuCanvas;        //!
  TGPopupMenu        *fMenuEdit;          //!
  TGPopupMenu        *fMenuTrack;         //!
  TGPopupMenu        *fMenuFilter;        //!
  TGPopupMenu        *fMenuGlobal;        //!
  TGPopupMenu        *fMenuPrim;          //!
  TGPopupMenu        *fMenuView;          //!
  TGPopupMenu        *fMenuPedHist;       //!
  TGPopupMenu        *fMenuSingEvtHist;   //!
  TGPopupMenu        *fMenuMultEvtHist;   //!
  TGPopupMenu        *fMenuMultEvtHist1;  //!
  TGPopupMenu        *fMenuMultEvtHist2;  //!
  TGPopupMenu        *fMenuMultEvtHist3;  //!
  TGPopupMenu        *fMenuMultEvtHist4;  //!
  TGPopupMenu        *fMenuHelp;          //!
  
  TGLayoutHints      *fMenuBarItemLayout; //!
  TGLayoutHints      *fMenuBarHelpLayout; //!
  
  StSvtGuiMonitor      *fSvtGuiMonitor;   //!
  StSvtMonitor         *fSvtMonitor;      //!
  StChain              *fMainChain;       //!
  StChain              *fChain;           //!
  StChain              *fDstChain;        //!
  StBFChain            *fBFChain;         //!
  StEventDisplayMaker  *fDisplay;         //!
  StTrackFilter        *fFilter;          //!
  
  TCanvas              *fCanvas;          //!
  TCanvas              *fCanvas1;         //!
  TCanvas              *fCanvas2;         //!
  TCanvas              *fCanvas3;         //!
  TCanvas              *fCanvas4;         //!
  TCanvas              *fCanvas5;         //!
  TCanvas              *fCanvasLadder;    //!
  TCanvas              *fCanvasBarrel;    //!
  TCanvas              *f3DCanvas;        //!  

  int mHist;
  int fFirstEvent;
  char* fFlags;      //!
  char* mDrawOption; //!

public:
   StSvtMenuBar(const TGWindow *p, UInt_t w, UInt_t h, UInt_t o);
   virtual ~StSvtMenuBar();
 
   virtual void SetChain(StChain* aChain, StChain* aBFChain);
   virtual void SetSvtMonitor(StSvtMonitor* aMonitor) {fSvtMonitor = aMonitor;}
   virtual void SetSvtGuiMonitor(StSvtGuiMonitor* aGuiMonitor) {fSvtGuiMonitor = aGuiMonitor;}

   virtual void CloseWindow();
   int SetDefaultCanvas();
   int UpdateCanvas();
   void UnCheckAllEntries();
   void SetMakers();
   void OpenFile(const char* file);
   void InitMakers();
   void CloseFile(char* option="");
   void DrawHist(char* option="");
   virtual Bool_t ProcessMessage(const TGWindow *p, Long_t msg, Long_t parm1, Long_t, const void* buffer=NULL);

   ClassDef(StSvtMenuBar,1)
};

#endif
