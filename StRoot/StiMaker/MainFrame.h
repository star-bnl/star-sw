//MainFrame.h

#ifndef MainFrame_HH
#define MainFrame_HH

#include <stdlib.h>

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
#include <TRootEmbeddedCanvas.h>
#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TRandom.h>
#include <TSystem.h>
#include <TEnv.h>

#include "StChain.h"

enum ETestCommandIdentifiers {
    M_FILE_OPEN,
    M_FILE_SAVE,
    M_FILE_SAVEAS,
    M_FILE_EXIT,

    M_Draw_TestObject,
    
    M_Det_Navigate,

    M_DetNavigate_MoveIn,
    M_DetNavigate_MoveOut,
    M_DetNavigate_MovePlusPhi,
    M_DetNavigate_MoveMinusPhi,
    M_DetNavigate_SetLayer,
    M_DetNavigate_SetLayerAndAngle,

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

    M_Tracking_ToggleFitFind,
    M_Tracking_FinishTrack,
    M_Tracking_FinishEvent,
    M_Tracking_EventStep,
    M_Tracking_NEventStep,

    M_HELP_CONTENTS,
    M_HELP_SEARCH,
    M_HELP_ABOUT
};


class TileFrame;

class StIOMaker;

class MainFrame : public TGMainFrame
{    
public:
    ///Only constructor.
    MainFrame(const TGWindow *p, UInt_t w, UInt_t h);
    
    ///Default destructor.
    virtual ~MainFrame();

    ///This function handles the user input
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t);

public:
    
    //General access
    void setStChain(StChain* val) {mchain=val;}
    void setIoMaker(StIOMaker* val) {mIoMaker=val;}
    
private:
    
    virtual void CloseWindow();

    //Add some new test function
    void setAllVisible();
    void setAllInvisible();

    void setTpcVisible();
    void setTpcInvisible();
    
    void setSvtVisible();
    void setSvtInvisible();
    
    void setIfcVisible();
    void setIfcInvisible();

    void navigate();

    void testDraw();
    
    //do next action within this event
    void doNextStiGuiAction();
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
    void setSkeletonView();
    void setZoomSkeletonView();

    //Navigate through detector (should be sub-menued/cascaded)
    void setCurrentDetectorToDefault();
    //Show the current
    void showCurrentDetector(); 
    
    //call StiDetectorLayerContainer::setSector(int)
    void setLayer(); 
    void setLayerAndAngle();
    
    void moveIn();
    void moveOut();    
    void movePlusPhi();
    void moveMinusPhi();
    
    void memoryInfo();
    void printFactorySize();
    
    void toggleFitFind();
    
private:
    
    MainFrame(); ///Not implemented
    MainFrame(const MainFrame&); //Not implemented
    
    TGCanvas           *fCanvasWindow;
    TileFrame          *fContainer;
    
    TGMenuBar          *fMenuBar;
    TGPopupMenu        *fMenuFile, *fMenuHelp;
    
    TGPopupMenu *fDetectorMenu;
    TGPopupMenu *fDetectorViewMenu;

    TGPopupMenu *mSvtViewMenu;
    TGPopupMenu *mTpcViewMenu;
    TGPopupMenu *mIfcViewMenu;
    TGPopupMenu *mAllViewMenu;
    TGPopupMenu *mNavigateMenu;
    
    TGPopupMenu *mTrackingMenu;
    
    TGLayoutHints *fMenuBarLayout, *fMenuBarItemLayout, *fMenuBarHelpLayout;

    StChain* mchain;
    StIOMaker* mIoMaker;
    
    TGCompositeFrame* fTrackingFrame;
    TGTextButton* fFinishTrackButton;
    TGTextButton* fFinishEventButton;
    TGTextButton* fNextEventButton;

    ClassDef(MainFrame,1)
};

class TileFrame : public TGCompositeFrame
{    
private:
    TGCanvas *fCanvas;
    
public:
    TileFrame(const TGWindow *p);
    virtual ~TileFrame() { }
    
    void SetCanvas(TGCanvas *canvas) { fCanvas = canvas; }
    Bool_t HandleButton(Event_t *event);
};


#endif
