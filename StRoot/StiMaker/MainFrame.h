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

enum ETestCommandIdentifiers {
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

//Define some simple classes for controlling the "view" that we're in
class StiView
{
public:
    virtual void setToDefault() = 0;
};

//Padrow 45, svt, ssd
class StiSkeletonView : public StiView
{
public:
    virtual void setToDefault();
};

//Padrow 1, svt, ssd
class StiZoomSkeletonView : public StiView
{
public:
    virtual void setToDefault();
};

//Only those detectors which satisfy isOn()==true
class StiManualView : public StiView
{
public:
    virtual void setToDefault();
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

    //Access to the most recently created instance (should be only one, but not guarunteed)
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
    
    virtual void CloseWindow();
    
    void ShowRootColors();
    
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
    TGPopupMenu* mNextStepMenu;

    TGPopupMenu* mOptionsMenu;
    
    TGLayoutHints *fMenuBarLayout, *fMenuBarItemLayout, *fMenuBarHelpLayout;
    
    StChain* mchain;
    StIOMaker* mIoMaker;
    
    TGCompositeFrame* fTrackingFrame;
    TGTextButton* fDoTrackStepButton;
    TGTextButton* fFinishTrackButton;
    TGTextButton* fFinishEventButton;
    TGTextButton* fResetEventButton;
    TGTextButton* fNextEventButton;

    StiView* mView;
    
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


class Navigator : public TGTransientFrame
{
private:
    TGCompositeFrame *f1;//, *f2, *f3;
    TGButton* mMoveIn;
    TGButton* mMoveOut;
    TGButton* mMovePlusPhi;
    TGButton* mMoveMinusPhi;
    TGButton* mClose;

    //TGGroupFrame         *fG1;
    TGLayoutHints        *fL1, *fL21;//, *fL2, *fL3, *fL4, *fL21;

    void moveIn();
    void moveOut();
    void movePlusPhi();
    void moveMinusPhi();

public:
    Navigator(const TGWindow *p, const TGWindow *main, UInt_t w, UInt_t h,
	       UInt_t options = kVerticalFrame);
    virtual ~Navigator();
    
    virtual void CloseWindow();
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
};

#ifndef __CINT__
typedef pair<unsigned int, TGCheckButton*> MessengerPair;
#else
class MessengerPair;
#endif

#ifndef __CINT__
typedef vector<MessengerPair> MsgPairVec;
#else
class MsgPairVec;
#endif

class TestMsgBox : public TGTransientFrame
{
private:
    
    TGCompositeFrame     *f1, *f2, *f3;
    TGButton             *fTestButton, *fCloseButton;
    
    MsgPairVec fC;
    
    TGGroupFrame         *fG1;
    TGLayoutHints        *fL1, *fL2, *fL3, *fL4, *fL21;
    TGGC                  fRedTextGC;

    void updateMessenger();
    
public:
    TestMsgBox(const TGWindow *p, const TGWindow *main, UInt_t w, UInt_t h,
	       UInt_t options = kVerticalFrame);
    virtual ~TestMsgBox();
    
    virtual void CloseWindow();
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
};

#ifndef __CINT__
typedef pair<string, TGCheckButton*> DetectorActivatePair;
#else
class DetectorActivatePair;
#endif

#ifndef __CINT__
typedef vector<DetectorActivatePair> DetActivatePairVec;
#else
class DetActivatePairVec;
#endif

class DetectorActivator : public TGTransientFrame
{
private:
    
    TGCompositeFrame     *f1, *f2, *f3;

    TGButton             *fTestButton, *fCloseButton;
    
    DetActivatePairVec fC;
    
    TGGroupFrame         *fG1;
    TGLayoutHints        *fL1, *fL2, *fL3, *fL4, *fL42, *fL21;
    TGGC                  fRedTextGC;

    void updateDetectors();
    
public:
    DetectorActivator(const TGWindow *p, const TGWindow *main, UInt_t w, UInt_t h,
	       UInt_t options = kVerticalFrame);
    virtual ~DetectorActivator();
    
    virtual void CloseWindow();
    virtual void activateLayer(const string& name, bool on);
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
};

#ifndef __CINT__
typedef vector<TGHorizontalFrame*> HorizontalFrameVec;
typedef pair<string, TGNumberEntry*> NamedNumberEntry;
typedef vector<NamedNumberEntry> NumberEntryVec;
//typedef vector<TGNumberEntry*> NumberEntryVec;
typedef vector<TGLabel*> LabelVec;
#else
class HorizontalFrameVec;
class NamedNumberEntry;
class NumberEntryVec;
class LabelVec;
#endif

class EntryTestDlg : public TGTransientFrame
{    
private:
    TGVerticalFrame      *fF1;
    TGVerticalFrame      *fF2;
    HorizontalFrameVec fF;
    TGLayoutHints        *fL1;
    TGLayoutHints        *fL2;
    TGLayoutHints        *fL3;
    LabelVec fLabel;
    NumberEntryVec fNumericEntries;
    TGButton             *fSetButton;
    TGButton             *fExitButton;
    DetActivatePairVec fC;
    
    void makeNumberEntries();
    
public:
    EntryTestDlg(const TGWindow *p, const TGWindow *main);
    virtual ~EntryTestDlg();
    virtual void CloseWindow();
    
    void SetLimits();
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t);
};

class SeedFinderIO : public TGTransientFrame
{    
private:
    TGVerticalFrame      *fF1;
    TGVerticalFrame      *fF2;
    HorizontalFrameVec fF;
    TGLayoutHints        *fL1;
    TGLayoutHints        *fL2;
    TGLayoutHints        *fL3;
    LabelVec fLabel;
    NumberEntryVec fNumericEntries;
    TGButton             *fSetButton;
    TGButton             *fExitButton;
    
    void makeNumberEntries();
    
public:
    SeedFinderIO(const TGWindow *p, const TGWindow *main);
    virtual ~SeedFinderIO();
    virtual void CloseWindow();
    
    void SetLimits();
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t);
};

class LocalSeedFinderIO : public TGTransientFrame
{    
private:
    TGVerticalFrame      *fF1;
    TGVerticalFrame      *fF2;
    HorizontalFrameVec fF;
    TGLayoutHints        *fL1;
    TGLayoutHints        *fL2;
    TGLayoutHints        *fL3;
    LabelVec fLabel;
    NumberEntryVec fNumericEntries;
    TGButton             *fSetButton;
    TGButton             *fExitButton;
    DetActivatePairVec fC;
    
    void makeNumberEntries();
    
public:
    LocalSeedFinderIO(const TGWindow *p, const TGWindow *main);
    virtual ~LocalSeedFinderIO();
    virtual void CloseWindow();
    
    void SetLimits();
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t);
};

// Kalman Track Finder IO

class KalmanTrackFinderIO : public TGTransientFrame
{    
private:
    TGVerticalFrame      *fF1;
    TGVerticalFrame      *fF2;
    HorizontalFrameVec fF;
    TGLayoutHints        *fL1;
    TGLayoutHints        *fL2;
    TGLayoutHints        *fL3;
    LabelVec fLabel;
    NumberEntryVec fNumericEntries;
    TGButton             *fSetButton;
    TGButton             *fExitButton;
    DetActivatePairVec fC;
    
    void makeNumberEntries();
    
public:
    KalmanTrackFinderIO(const TGWindow *p, const TGWindow *main);
    virtual ~KalmanTrackFinderIO();
    virtual void CloseWindow();
    
    void SetLimits();
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t);
};


#endif
