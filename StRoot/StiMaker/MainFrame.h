//MainFrame.h

#ifndef MainFrame_HH
#define MainFrame_HH

#include <stdlib.h>

#include <vector>
using std::vector;
#include <utility>
using std::pair;

#include <string>
using std::string;

#include "TObject.h"

#include <TROOT.h>
#include <TApplication.h>
#include <TVirtualX.h>

#include <TGListBox.h>
#include <TGClient.h>
#include <TGFrame.h>
#include <TGIcon.h>
#include <TGLabel.h>
#include <TGButton.h>
#include <TGTextEntry.h>
#include <TGMsgBox.h>
#include <TGMenu.h>
#include <TGCanvas.h>
#include <TGComboBox.h>
#include <TGTab.h>
#include <TGSlider.h>
#include <TGDoubleSlider.h>
#include <TGFileDialog.h>
#include <TGTextEdit.h>
#include <TGShutter.h>
#include <TGProgressBar.h>
#include <TGNumberEntry.h>
#include <TRootEmbeddedCanvas.h>
#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TRandom.h>
#include <TSystem.h>
#include <TEnv.h>
#include "StChain.h"
class StiView;

enum ETestCommandIdentifiers 
  {
    M_FILE_OPEN,
    M_FILE_SAVE,
    M_FILE_SAVEAS,
    M_FILE_EXIT,
    M_Draw_TestObject,
    M_ShowRootColors,
    M_Det_Navigate,
    M_DetNavigate_MoveIn,
    M_DetNavigate_MoveOut,
    M_DetNavigate_MovePlusPhi,
    M_DetNavigate_MoveMinusPhi,
    M_DetNavigate_SetLayer,
    M_DetNavigate_SetLayerAndAngle,
    M_DetView_ManualView,
    M_DetView_SkeletonView,
    M_DetView_ZoomSkeletonView,
    M_DetView_AllVisible,
    M_DetView_AllInvisible,
    M_DetView_TpcVisible,
    M_DetView_TpcInvisible,
    M_DetView_SvtVisible,
    M_DetView_SvtInvisible,
    M_DetView_IfcVisible,
    M_DetView_IfcInvisible,
    
    M_DetOnOff,
    
    M_Messenger,
    M_Message_Hit,
    M_Message_Track,
    M_Message_Node,
    M_Message_SeedFinder,
    M_Message_Detector,
    
    M_DisplayOptions,
    M_SeedFinderOptions,
    M_LocalSeedFinderOptions,
    M_TrackFinderOptions,
    M_McTrackFilterOptions,
    M_TrackFilterOptions,
    
    M_TrackingSwitch_NextDetector,
    M_TrackingSwitch_ScanLayer,
    
    M_Tracking_ToggleFitFind,
    M_Tracking_DoTrackStep,
    M_Tracking_FinishTrack,
    M_Tracking_FinishEvent,
    M_Tracking_EventStep,
    M_Tracking_ResetEvent,
    M_Tracking_NEventStep,
    
    M_HELP_CONTENTS,
    M_HELP_SEARCH,
    M_HELP_ABOUT
  };


class StIOMaker;
class StiDisplayManager;

class MainFrame : public TGMainFrame
{    
 public:
  ///Only constructor.
  MainFrame(const TGWindow *p, UInt_t w, UInt_t h);
  virtual ~MainFrame();
  virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t);
  void createMenu();
  void createFileMenu(TGMenuBar *menuBar, TGLayoutHints *menuBarLayout);
  void createOptionMenu(TGMenuBar *menuBar, TGLayoutHints *menuBarLayout);
  void createViewMenu(TGMenuBar *menuBar, TGLayoutHints *menuBarLayout);
  void createNavigationMenu(TGMenuBar *menuBar, TGLayoutHints *menuBarLayout);
  void createTrackingMenu(TGMenuBar *menuBar, TGLayoutHints *menuBarLayout);
  void createHelpMenu(TGMenuBar *menuBar, TGLayoutHints *menuBarLayout);
  void createCanvasFrame();
  void createButtonFrame();
  void setStChain(StChain* val) {mchain=val;}
  void setIoMaker(StIOMaker* val) {mIoMaker=val;}
  static MainFrame* instance() {return s_instance;}
  void moveIn();
  void moveOut();    
  void movePlusPhi();
  void moveMinusPhi();
  void setCurrentDetectorToDefault();
  void showCurrentDetector();
  void setView(StiView* view);
    
private:

  static MainFrame* s_instance;
  StiDisplayManager * displayManager;
  virtual void CloseWindow();
  void ShowRootColors();
  
  void setAllVisible();
  void setAllInvisible();
  
  void setTpcVisible();
  void setTpcInvisible();
  
  void setSvtVisible();
  void setSvtInvisible();
  
  void setIfcVisible();
  void setIfcInvisible();
  
  void navigate();
  
  //Finish one track at a time
  void finishTrack();
  
  //Make the next step in the current track
  void doNextTrackStep();
  
  //fit/find the entire event
  void finishEvent(); 
  //step to the next event
  void stepToNextEvent(); 
  //step through user-specified number of events
  void stepThroughNEvents();
  
  //call StiDisplayManager::print()
  void printDisplayManager();
  //print all hits in event
  void printHits(); 
  //print hits for the current detector layer
  void printHitContainerForDetector();
  //print the vertices
  void printVertices();
  
  //call StiDisplayManager::makeSkeletonView
  void setManualView();
  void setSkeletonView();
  void setZoomSkeletonView();
  
  //call StiDetectorLayerContainer::setSector(int)
  void setLayer(); 
  void setLayerAndAngle();
  
  void memoryInfo();
  void printFactorySize();
  
  void toggleFitFind();
  
 private:
  
  MainFrame(); ///Not implemented
  MainFrame(const MainFrame&); //Not implemented
  StChain* mchain;
  StIOMaker* mIoMaker;
  StiView* mView;
  ClassDef(MainFrame,1)
    
};

#endif
