/***************************************************************************
 *
 * $Id: StSvtGuiMonitor.hh,v 1.1 2004/02/06 02:30:34 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT GUI Monitor
 *
 ***************************************************************************
 *
 * $Log: StSvtGuiMonitor.hh,v $
 * Revision 1.1  2004/02/06 02:30:34  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/
 
#ifndef STSVTGUIMONITOR_H
#define STSVTGUIMONITOR_H 

#include <TGFrame.h>
  
class TGLayoutHints;
class TRootEmbeddedCanvas;
class TH1F;
class TCanvas;
class TOrdCollection;
class TPad;

class StSvtMenuBar;
class StSvtView; 
class StSvtMonitor; 
class StChain; 
class StBFChain; 
class StSvtHybridCollection;

class StSvtGuiMonitor : public TGMainFrame {
 
private: 
  StSvtMenuBar          *fMenuBar;        //!
  TGLayoutHints         *fMenuBarLayout;  //!
  
  TRootEmbeddedCanvas   *fCanvas;         //!
  StSvtView             *fSvtView;        //! 
  StSvtMonitor          *fSvtMonitor;     //!

  TOrdCollection        *histProj;        //!

  TPad                 **mPadLadder;      //!
  TPad                 **mPadBarrel;      //!

  int mColorIndex;
  int mLineIndex;
  
public:
  StSvtGuiMonitor(char* config, StChain* aChain, StChain* aBFChain, const TGWindow *p, UInt_t w, UInt_t h);
  virtual ~StSvtGuiMonitor();
  
  void drawHist(char* option="H", char* type="REGULAR");
  void drawHistLadder(char* option="H", TCanvas* csumm=0, int firstybin=0, int lastybin=0);
  void drawHistBarrel(char* option="H", TCanvas* csumm=0);
  void drawClusters(StSvtHybridCollection* clustersColl);

  TH1F* projectX(int firstybin, int lastybin, char* option=0);
  TH1F* projectY(int firstxbin, int lastxbin, char* option=0);

  virtual StSvtView* GetSvtView(){return fSvtView;} 
  virtual StSvtMonitor* GetSvtMonitor(){return fSvtMonitor;} 
  virtual StSvtMenuBar* GetSvtMenuBar(){return fMenuBar;} 
  
  virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t);

  ClassDef(StSvtGuiMonitor,1)
};

#endif
