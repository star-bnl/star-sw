//MainFrame.cxx

#include <iostream.h>
#include <algorithm>
using std::find_if;

//root
#include "TRootEmbeddedCanvas.h"
#include "TShape.h"
#include "TBRIK.h"
#include "TVolume.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TPaveLabel.h"

//Star
#include "StChain.h"
#include "StIOMaker/StIOMaker.h"

//SCL
#include "StMemoryInfo.hh"

//Sti
#include "Sti/StiIOBroker.h"
#include "Sti/Messenger.h"
#include "Sti/StiDetector.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiCompositeTreeNode.h"
#include "Sti/StiFactoryTypes.h"
#include "Sti/StlUtilities.h"

//StiGui
#include "StiGui/StiRootDrawableDetector.h"
#include "StiGui/StiDrawable.h"
#include "StiGui/StiDisplayManager.h"
#include "StiGui/StiGuiIOBroker.h"

//StiMaker
#include "StiMaker.h"

#include "MainFrame.h"

MainFrame* MainFrame::s_instance = 0;

Int_t mb_button_id[9] = { kMBYes, kMBNo, kMBOk, kMBApply,
			  kMBRetry, kMBIgnore, kMBCancel,
			  kMBClose, kMBDismiss };

EMsgBoxIcon mb_icon[4] = { kMBIconStop, kMBIconQuestion,
			   kMBIconExclamation, kMBIconAsterisk };

const char *filetypes[] = { "All files",     "*",
			    "ROOT files",    "*.root",
			    "ROOT macros",   "*.C",
			    0,               0 };

TileFrame::TileFrame(const TGWindow *p) :
    TGCompositeFrame(p, 10, 10, kHorizontalFrame, GetWhitePixel())
{
    // Create tile view container. Used to show colormap.
    
    fCanvas = 0;
    SetLayoutManager(new TGTileLayout(this, 8));
    
    // Handle only buttons 4 and 5 used by the wheel mouse to scroll
    gVirtualX->GrabButton(fId, kButton4, kAnyModifier,
			  kButtonPressMask | kButtonReleaseMask,
			  kNone, kNone);
    gVirtualX->GrabButton(fId, kButton5, kAnyModifier,
			  kButtonPressMask | kButtonReleaseMask,
			  kNone, kNone);
}

Bool_t TileFrame::HandleButton(Event_t *event)
{
    // Handle wheel mouse to scroll.
    
    Int_t page = 0;
    if (event->fCode == kButton4 || event->fCode == kButton5) {
	if (!fCanvas) return kTRUE;
	if (fCanvas->GetContainer()->GetHeight())
	    page = Int_t(Float_t(fCanvas->GetViewPort()->GetHeight() *
				 fCanvas->GetViewPort()->GetHeight()) /
			 fCanvas->GetContainer()->GetHeight());
    }
    
    if (event->fCode == kButton4) {
	//scroll up
	Int_t newpos = fCanvas->GetVsbPosition() - page;
	if (newpos < 0) newpos = 0;
	fCanvas->SetVsbPosition(newpos);
	return kTRUE;
    }
    if (event->fCode == kButton5) {
	// scroll down
	Int_t newpos = fCanvas->GetVsbPosition() + page;
	fCanvas->SetVsbPosition(newpos);
	return kTRUE;
    }
    return kTRUE;
}


ClassImp(MainFrame)
    
    MainFrame::MainFrame(const TGWindow *p, UInt_t w, UInt_t h)
	: TGMainFrame(p, w, h),
	  
	  fCanvasWindow(0), fContainer(0),
	  fMenuBar(0), fMenuFile(0), fMenuHelp(0), fDetectorMenu(0),
	  fDetectorViewMenu(0), mSvtViewMenu(0), mTpcViewMenu(0),
	  mIfcViewMenu(0), mAllViewMenu(0), mNavigateMenu(0),
	  mTrackingMenu(0), mNextStepMenu(0),
	  fMenuBarLayout(0), fMenuBarItemLayout(0), fMenuBarHelpLayout(0),
	  mchain(0), mIoMaker(0),
	  fTrackingFrame(0), fDoTrackStepButton(0),
	  fFinishTrackButton(0), fFinishEventButton(0),
	  fNextEventButton(0)
{
    cout <<"MainFrame::MainFrame()"<<endl;
    s_instance = this;

    // Create test main frame. A TGMainFrame is a top level window.
    
    // Create menubar and popup menus. The hint objects are used to place
    // and group the different menu widgets with respect to eachother.
    fMenuBarLayout = new TGLayoutHints(kLHintsTop | kLHintsLeft | kLHintsExpandX,
				       0, 0, 1, 1);
    fMenuBarItemLayout = new TGLayoutHints(kLHintsTop | kLHintsLeft, 0, 4, 0, 0);
    fMenuBarHelpLayout = new TGLayoutHints(kLHintsTop | kLHintsRight);
    
    fMenuFile = new TGPopupMenu(fClient->GetRoot());
    fMenuFile->AddEntry("&Open...", M_FILE_OPEN);
    fMenuFile->AddEntry("&Save", M_FILE_SAVE);
    fMenuFile->AddEntry("S&ave as...", M_FILE_SAVEAS);
    fMenuFile->AddEntry("&Close", -1);
    fMenuFile->AddSeparator();
    fMenuFile->AddEntry("&Print", -1);
    fMenuFile->AddEntry("P&rint setup...", -1);

    fMenuFile->AddSeparator();
    fMenuFile->AddEntry("E&xit", M_FILE_EXIT);
    
    fMenuFile->DisableEntry(M_FILE_SAVEAS);
    
    mSvtViewMenu = new TGPopupMenu(fClient->GetRoot());
    mSvtViewMenu->AddEntry("Svt Visible", M_DetView_SvtVisible);
    mSvtViewMenu->AddEntry("Svt Invisible", M_DetView_SvtInvisible);
    
    mTpcViewMenu = new TGPopupMenu(fClient->GetRoot());
    mTpcViewMenu->AddEntry("Tpc Visible", M_DetView_TpcVisible);
    mTpcViewMenu->AddEntry("Tpc Invisible", M_DetView_TpcInvisible);
    mIfcViewMenu = new TGPopupMenu(fClient->GetRoot());
    mIfcViewMenu->AddEntry("Ifc Visible", M_DetView_IfcVisible);
    mIfcViewMenu->AddEntry("Ifc Invisible", M_DetView_IfcInvisible);
    
    mAllViewMenu = new TGPopupMenu(fClient->GetRoot());
    mAllViewMenu->AddEntry("All Visible", M_DetView_AllVisible);
    mAllViewMenu->AddEntry("All Invisible", M_DetView_AllInvisible);
    
    fDetectorViewMenu = new TGPopupMenu(fClient->GetRoot());
    fDetectorViewMenu->AddEntry("Manual View", M_DetView_ManualView);
    fDetectorViewMenu->AddEntry("Skeleton View", M_DetView_SkeletonView);
    fDetectorViewMenu->AddEntry("Zoom Skeleton View",M_DetView_ZoomSkeletonView);
    fDetectorViewMenu->AddSeparator();
    fDetectorViewMenu->AddPopup("&All",mAllViewMenu);
    fDetectorViewMenu->AddPopup("&Tpc",mTpcViewMenu);
    fDetectorViewMenu->AddPopup("&Svt",mSvtViewMenu);
    fDetectorViewMenu->AddPopup("&Ifc",mIfcViewMenu);
    
    mNavigateMenu = new TGPopupMenu(fClient->GetRoot());
    mNavigateMenu->AddEntry("Navigator",M_Det_Navigate);
    mNavigateMenu->AddSeparator();
    mNavigateMenu->AddEntry("Move &In",M_DetNavigate_MoveIn);
    mNavigateMenu->AddEntry("Move &Out",M_DetNavigate_MoveOut);
    mNavigateMenu->AddEntry("Move &Plus Phi",M_DetNavigate_MovePlusPhi);
    mNavigateMenu->AddEntry("Move &Minus Phi",M_DetNavigate_MoveMinusPhi);
    mNavigateMenu->AddEntry("Set Layer",M_DetNavigate_SetLayer);
    mNavigateMenu->AddEntry("Set Layer and Angle",M_DetNavigate_SetLayerAndAngle);
    
    fDetectorMenu = new TGPopupMenu(fClient->GetRoot());
    fDetectorMenu->AddLabel("Access to the Detector Model");
    fDetectorMenu->AddSeparator();
    fDetectorMenu->AddEntry("Add/Remove Detectors", M_DetOnOff);
    fDetectorMenu->AddPopup("&Navigate", mNavigateMenu);
    fDetectorMenu->AddPopup("&Visibility", fDetectorViewMenu);
    
    mNextStepMenu = new TGPopupMenu(fClient->GetRoot());
    mNextStepMenu->AddEntry("Next Detector",M_TrackingSwitch_NextDetector);
    mNextStepMenu->AddEntry("Scan Layer",M_TrackingSwitch_ScanLayer);
    
    mTrackingMenu = new TGPopupMenu(fClient->GetRoot());
    mTrackingMenu->AddLabel("Access to tracking functions");
    mTrackingMenu->AddEntry("Toggle Fit/Find",M_Tracking_ToggleFitFind);
    mTrackingMenu->AddPopup("Define Next Step", mNextStepMenu);
    mTrackingMenu->AddSeparator();
    mTrackingMenu->AddEntry("Next Track Step",M_Tracking_DoTrackStep);
    mTrackingMenu->AddEntry("Finish Track",M_Tracking_FinishTrack);
    mTrackingMenu->AddEntry("Finish Event",M_Tracking_FinishEvent);
    mTrackingMenu->AddEntry("Event Step",M_Tracking_EventStep);
    mTrackingMenu->AddEntry("N-Event Step",M_Tracking_NEventStep);

    mOptionsMenu = new TGPopupMenu(fClient->GetRoot());
    mOptionsMenu->AddEntry("Messenger Options", M_Messenger);
    mOptionsMenu->AddEntry("Display Options", M_DisplayOptions);
    mOptionsMenu->AddEntry("MC Track Colors", M_ShowRootColors);
    mOptionsMenu->AddEntry("Evaluable Seed Finder Options", M_SeedFinderOptions);
    mOptionsMenu->AddEntry("Local Seed Finder Options", M_LocalSeedFinderOptions);
    mOptionsMenu->AddEntry("Kalman Track Finder Options", M_TrackFinderOptions);

    fMenuHelp = new TGPopupMenu(fClient->GetRoot());
    fMenuHelp->AddEntry("&Contents", M_HELP_CONTENTS);
    fMenuHelp->AddEntry("&Search...", M_HELP_SEARCH);
    fMenuHelp->AddSeparator();
    fMenuHelp->AddEntry("&About", M_HELP_ABOUT);
    
    // Menu button messages are handled by the main frame (i.e. "this")
    // ProcessMessage() method.
    fMenuFile->Associate(this);
    fMenuHelp->Associate(this);
    fDetectorMenu->Associate(this);
    fDetectorViewMenu->Associate(this);
    mSvtViewMenu->Associate(this);
    mTpcViewMenu->Associate(this);
    mIfcViewMenu->Associate(this);
    mAllViewMenu->Associate(this);
    mNavigateMenu->Associate(this);
    mNextStepMenu->Associate(this);
    mTrackingMenu->Associate(this);
    mOptionsMenu->Associate(this);
    
    fMenuBar = new TGMenuBar(this, 1, 1, kHorizontalFrame);
    fMenuBar->AddPopup("&File", fMenuFile, fMenuBarItemLayout);
    fMenuBar->AddPopup("&Detector",fDetectorMenu, fMenuBarItemLayout);
    fMenuBar->AddPopup("&Tracking",mTrackingMenu, fMenuBarItemLayout);
    fMenuBar->AddPopup("&Options",mOptionsMenu, fMenuBarItemLayout);
    fMenuBar->AddPopup("&Help", fMenuHelp, fMenuBarHelpLayout);
    
    AddFrame(fMenuBar, fMenuBarLayout);

    fCanvasWindow =
	new TRootEmbeddedCanvas("My Embedded Canvas", this, 650, 600);

    fContainer = new TileFrame(fCanvasWindow->GetViewPort());
    fContainer->SetCanvas(fCanvasWindow);
    fCanvasWindow->SetContainer(fContainer);
    
    TRootEmbeddedCanvas* temp =
	dynamic_cast<TRootEmbeddedCanvas*>(fCanvasWindow);
    if (!temp) {
	cout <<"MainFrame::testDraw(). ERROR:\t";
	cout <<"Downcast of canvas failed.  Abort"<<endl;
	return;
    }

    //This has to be the first call to StiDisplayManager::instance()
    StiDisplayManager::instance(temp->GetCanvas());

    //Add the canvas to the frame
    AddFrame(fCanvasWindow, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY,
					      0, 0, 2, 2));

    fTrackingFrame = new TGCompositeFrame(this, 60, 20, kHorizontalFrame |
					  kSunkenFrame);

    fDoTrackStepButton = new TGTextButton(fTrackingFrame,
					  "&Next Track Step", M_Tracking_DoTrackStep);
    fDoTrackStepButton->Associate(this);
    fDoTrackStepButton->SetToolTipText("Perform the next step in the current track");
    
    fFinishTrackButton = new TGTextButton(fTrackingFrame,
					  "Finish &Track", M_Tracking_FinishTrack);
    fFinishTrackButton->Associate(this);
    fFinishTrackButton->SetToolTipText("Finish The Current Track");

    fFinishEventButton = new TGTextButton(fTrackingFrame,
					  "Finish &Event", M_Tracking_FinishEvent);
    fFinishEventButton->Associate(this);
    fFinishEventButton->SetToolTipText("Finish The Current Event");


    fResetEventButton = new TGTextButton(fTrackingFrame,
					 "Reset Event", M_Tracking_ResetEvent);
    fResetEventButton->Associate(this);
    fResetEventButton->SetToolTipText("Reset The Current Event");
    
    fNextEventButton = new TGTextButton(fTrackingFrame,
					"&Next Event", M_Tracking_EventStep);
    fNextEventButton->Associate(this);
    fNextEventButton->SetToolTipText("Step To Next Event");

    fTrackingFrame->AddFrame(fDoTrackStepButton,
			     new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2));
    fTrackingFrame->AddFrame(fFinishTrackButton,
			     new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2));
    fTrackingFrame->AddFrame(fFinishEventButton,
			     new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2));

    fTrackingFrame->AddFrame(fResetEventButton,
			     new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2));
    
    fTrackingFrame->AddFrame(fNextEventButton,
			     new TGLayoutHints(kLHintsTop | kLHintsRight, 2, 0, 2, 2));

    AddFrame(fTrackingFrame, new TGLayoutHints(kLHintsBottom | kLHintsExpandX,
					       0, 0, 1, 0));
    

    setView(new StiSkeletonView());

    SetWindowName("The STAR Integraged Tracker");
    
    MapSubwindows();
    
    // we need to use GetDefault...() to initialize the layout algorithm...
    Resize(GetDefaultSize());
    
    MapWindow();
    
}

MainFrame::~MainFrame()
{
    // Delete all created widgets.
    cout <<"MainFrame::~MainFrame()"<<endl;
    
    delete fContainer;
    fContainer=0;
    
    delete fCanvasWindow;
    fCanvasWindow=0;
    
    delete fMenuBarLayout;
    fMenuBarLayout=0;
    
    delete fMenuBarItemLayout;
    fMenuBarItemLayout=0;
    
    delete fMenuBarHelpLayout;
    fMenuBarHelpLayout=0;
    
    delete fMenuBar;
    fMenuBar=0;
    
    delete fMenuFile;
    fMenuFile=0;
    
    delete fMenuHelp;
    fMenuHelp=0;

    delete fDetectorMenu;
    fDetectorMenu=0;
    
    delete fDetectorViewMenu;
    fDetectorViewMenu=0;

    delete mTrackingMenu;
    mTrackingMenu=0;

    delete mOptionsMenu;
    mOptionsMenu=0;
    
    delete mSvtViewMenu;
    mSvtViewMenu=0;

    delete mNavigateMenu;
    mNavigateMenu=0;
    
    delete mTpcViewMenu;
    mTpcViewMenu=0;

    delete mIfcViewMenu;
    mIfcViewMenu=0;

    delete mAllViewMenu;
    mAllViewMenu=0;

    delete fMenuBarLayout;
    fMenuBarLayout=0;

    delete fMenuBarItemLayout;
    fMenuBarItemLayout=0;

    delete fMenuBarHelpLayout;
    fMenuBarHelpLayout=0;

    delete fTrackingFrame;
    fTrackingFrame=0;

    delete fDoTrackStepButton;
    fDoTrackStepButton=0;
    
    delete fFinishTrackButton;
    fFinishTrackButton=0;

    delete fFinishEventButton;
    fFinishEventButton=0;

    delete fResetEventButton;
    fResetEventButton=0;
    
    delete fNextEventButton;
    fNextEventButton=0;

    delete mNextStepMenu;
    mNextStepMenu=0;

}

void MainFrame::CloseWindow()
{
    // Got close message for this MainFrame. Terminate the application
    // or returns from the TApplication event loop (depending on the
    // argument specified in TApplication::Run()).
    StiMaker::kill();
    gApplication->Terminate(0);
}

Bool_t MainFrame::ProcessMessage(Long_t msg, Long_t parm1, Long_t)
{
    // Handle messages send to the MainFrame object. E.g. all menu button
    // messages.
    
    switch (GET_MSG(msg)) {
	
    case kC_COMMAND:
	switch (GET_SUBMSG(msg)) {
	    
	case kCM_BUTTON:
	    //printf("Button was pressed, id = %ld\n", parm1);
	    if (parm1 == M_Tracking_DoTrackStep) {
		doNextTrackStep();
	    }
	    
	    if (parm1 == M_Tracking_FinishTrack) {
		finishTrack();
	    }
	    else if (parm1 == M_Tracking_FinishEvent) {
		finishEvent();
	    }
	    else if (parm1 == M_Tracking_ResetEvent) {
		setCurrentDetectorToDefault();
		StiMaker::instance()->Clear();
		StiMaker::instance()->Make();
		showCurrentDetector();
	    }
	    else if (parm1 == M_Tracking_EventStep) {
		stepToNextEvent();
	    }
	    break;
	    
	    
	case kCM_MENUSELECT:
	    //printf("Pointer over menu entry, id=%ld\n", parm1);
	    break;
	    
	case kCM_MENU:
	    switch (parm1) {
		
	    case M_FILE_OPEN:
		{
		    static TString dir("/star/data22/ITTF/");
		    TGFileInfo fi;
		    fi.fFileTypes = filetypes;
		    fi.fIniDir    = StrDup(dir);
		    new TGFileDialog(fClient->GetRoot(), this, kFDOpen, &fi);
		    printf("Open file: %s (dir: %s)\n", fi.fFilename,
			   fi.fIniDir);
		    dir = fi.fIniDir;
		    mIoMaker->Close();
		    mIoMaker->SetFile(fi.fFilename);
		    stepToNextEvent();
		}
		break;

		//Mike's stuff here:

	    case M_Messenger:	
		new TestMsgBox(fClient->GetRoot(), this, 400, 200);
		break;

	    case M_ShowRootColors:
		ShowRootColors();
		break;
		
	    case M_DisplayOptions:
		new EntryTestDlg(fClient->GetRoot(), this);		
		break;

	    case M_SeedFinderOptions:
		new SeedFinderIO(fClient->GetRoot(), this);
		break;

	    case M_TrackFinderOptions:
		new KalmanTrackFinderIO(fClient->GetRoot(), this);
		break;

	    case M_LocalSeedFinderOptions:
		new LocalSeedFinderIO(fClient->GetRoot(), this);
		break;
		
	    case M_Draw_TestObject:
		testDraw();
		break;
		
	    case M_DetView_AllVisible:
		setAllVisible();
		break;

	    case M_DetView_AllInvisible:
		setAllInvisible();
		break;

	    case M_DetView_TpcVisible:
		setTpcVisible();
		break;

	    case M_DetView_TpcInvisible:
		setTpcInvisible();
		break;

	    case M_DetView_SvtVisible:
		setSvtVisible();
		break;

	    case M_DetView_SvtInvisible:
		setSvtInvisible();
		break;

	    case M_DetView_IfcVisible:
		setIfcVisible();
		break;

	    case M_DetView_IfcInvisible:
		setIfcInvisible();
		break;

	    case M_DetView_ManualView:
		setManualView();
		break;
		
	    case M_DetView_SkeletonView:
		setSkeletonView();
		break;

	    case M_DetView_ZoomSkeletonView:
		setZoomSkeletonView();
		break;

	    case M_DetOnOff:
		new DetectorActivator(fClient->GetRoot(), this, 800, 200);
		break;
		
	    case M_Det_Navigate:
		new Navigator(fClient->GetRoot(), this, 400, 200);
		break;
		
	    case M_DetNavigate_MoveIn:
		moveIn();
		break;

	    case M_DetNavigate_MoveOut:
		moveOut();
		break;

	    case M_DetNavigate_MovePlusPhi:
		movePlusPhi();
		break;

	    case M_DetNavigate_MoveMinusPhi:
		moveMinusPhi();
		break;

	    case M_DetNavigate_SetLayer:
		setLayer();
		break;

	    case M_DetNavigate_SetLayerAndAngle:
		setLayerAndAngle();
		break;

	    case M_Tracking_ToggleFitFind:
		toggleFitFind();
		break;

	    case M_TrackingSwitch_NextDetector:
		StiMaker::instance()->defineNextTrackStep(StepByDetector);
		break;
		
	    case M_TrackingSwitch_ScanLayer:
		StiMaker::instance()->defineNextTrackStep(StepByLayer);
		break;
		
	    case M_Tracking_DoTrackStep:
		doNextTrackStep();
		break;

	    case M_Tracking_FinishTrack:
		finishTrack();
		break;

	    case M_Tracking_FinishEvent:
		finishEvent();
		break;

	    case M_Tracking_EventStep:
		stepToNextEvent();
		break;

	    case M_Tracking_NEventStep:
		stepThroughNEvents();
		break;
		
	    case M_FILE_SAVE:
		printf("M_FILE_SAVE\n");
		break;
		
	    case M_FILE_EXIT:
		CloseWindow();   // this also terminates theApp
		break;
		
	    default:
		break;
	    }
	default:
	    break;
	}
    default:
	break;
    }
    return kTRUE;
}

void MainFrame::testDraw()
{
    cout <<"\nMainFrame::testDraw()"<<endl;
    TRootEmbeddedCanvas* temp =
	dynamic_cast<TRootEmbeddedCanvas*>(fCanvasWindow);
    if (!temp) {
	cout <<"MainFrame::testDraw(). ERROR:\t";
	cout <<"Downcast of canvas failed.  Abort"<<endl;
	return;
    }

    TCanvas* c1 = temp->GetCanvas();
    c1->cd();

    TShape* shape = new TBRIK("brikname","BRIK","void", 600., 600., 600.);
    TVolume* node = new TVolume("mainnode", "The mainnode title",shape);
    node->Draw();

    c1->Update();
}

void MainFrame::setView(StiView* view)
{
    if (mView!=0) {
	delete mView;
    }
    mView=view;
}

void MainFrame::doNextTrackStep()
{
    setCurrentDetectorToDefault();
    StiMaker::instance()->doNextTrackStep();
    showCurrentDetector();
}

void MainFrame::finishTrack()
{
    // cout <<"MainFrame::finishTrack()"<<endl;
    setCurrentDetectorToDefault();
    StiMaker::instance()->finishTrack();
    showCurrentDetector();
    // cout <<"\tMainFramefinishTrack() done"<<endl;
}

void MainFrame::finishEvent()
{
    setCurrentDetectorToDefault();
    StiMaker::instance()->finishEvent();
    showCurrentDetector();
}

void MainFrame::stepToNextEvent()
{
    setCurrentDetectorToDefault();    
    mchain->Clear();
    mchain->Make();
    showCurrentDetector();
}

void MainFrame::stepThroughNEvents()
{
    setCurrentDetectorToDefault();    
    cout <<"\nEnter number of events to process (int) "<<endl;
    int nevents;
    cin >> nevents;
    for (int i=0; i<nevents; ++i) {
	mchain->Clear();
	mchain->Make();
	showCurrentDetector();
    }
}

void MainFrame::printFactorySize()
{
    StiMaker::instance()->printStatistics();
}

void MainFrame::setAllVisible()
{
    StiDisplayManager::instance()->setVisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void MainFrame::setAllInvisible()
{
    StiDisplayManager::instance()->setInvisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void MainFrame::setSkeletonView()
{
    StiDisplayManager::instance()->setSkeletonView();
    setView(new StiSkeletonView());
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void MainFrame::setManualView()
{
    StiDisplayManager::instance()->setVisible();
    setView(new StiManualView());
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void MainFrame::setZoomSkeletonView()
{
    StiDisplayManager::instance()->setZoomSkeletonView();
    setView(new StiZoomSkeletonView());
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void MainFrame::setSvtVisible()
{
    StiDisplayManager::instance()->setSvtVisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void MainFrame::setSvtInvisible()
{
    StiDisplayManager::instance()->setSvtInvisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void MainFrame::setTpcVisible()
{
    StiDisplayManager::instance()->setTpcVisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void MainFrame::setTpcInvisible()
{
    StiDisplayManager::instance()->setTpcInvisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void MainFrame::setIfcVisible()
{
    cout <<"MainFrame::setIfcVisible(). Not yet implemented"<<endl;
    StiDisplayManager::instance()->setIfcVisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void MainFrame::setIfcInvisible()
{
    cout <<"MainFrame::setIfcInvisible(). Not yet implemented"<<endl;
    StiDisplayManager::instance()->setIfcInvisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void MainFrame::printDisplayManager()
{
    StiDisplayManager::instance()->print();
}

//This is a memory leak, so don't push it too far!
void MainFrame::ShowRootColors()
{
    TCanvas* colorCanvas = new TCanvas("colorCanvas","MC Track Color Scheme", 100,100,200,400);
    colorCanvas->Draw();
    
    double xstart = .1;
    double ystart = .9;
    double dx = .8;
    double dy = .1;
    double deltay = .15;
    
    TPaveLabel* pion = new TPaveLabel(xstart, ystart, xstart+dx, ystart-dy, "Pion");
    pion->SetTextColor(2);
    pion->Draw();

    ystart-=deltay;
    TPaveLabel* kaon = new TPaveLabel(xstart, ystart, xstart+dx, ystart-dy, "Kaon");
    kaon->SetTextColor(3);
    kaon->Draw();

    ystart-=deltay;
    TPaveLabel* proton = new TPaveLabel(xstart, ystart, xstart+dx, ystart-dy, "Proton");
    proton->SetTextColor(4);
    proton->Draw();

    ystart-=deltay;
    TPaveLabel* muon = new TPaveLabel(xstart, ystart, xstart+dx, ystart-dy, "Muon");
    muon->SetTextColor(6);
    muon->Draw();

    ystart-=deltay;
    TPaveLabel* electron = new TPaveLabel(xstart, ystart, xstart+dx, ystart-dy, "Electron");
    electron->SetTextColor(1);
    electron->Draw();

    ystart-=deltay;
    TPaveLabel* other = new TPaveLabel(xstart, ystart, xstart+dx, ystart-dy, "Other");
    other->SetTextColor(5);
    other->Draw();

    StiDisplayManager::instance()->cd();
}

void MainFrame::setLayer()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    setCurrentDetectorToDefault();
    cout <<"\nEnter position: (double)"<<endl;
    double position;
    cin >>position;
    cout <<"Setting to  position:\t"<<position<<endl;
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    rdet.setToDetector(position);
    StiDetector* layer = *rdet;
    if (!layer) {
	cout <<"Error in setSectorAndPadrow"<<endl;
	return;
    }
    cout <<"Detector Set To: "<<layer->getName()<<endl;
    showCurrentDetector();
}

void MainFrame::setLayerAndAngle()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    setCurrentDetectorToDefault();
    cout <<"\nEnter position: (double)"<<endl;
    double position;
    cin >>position;

    cout <<"\nEnter angle: (double)"<<endl;
    double angle;
    cin >> angle;
    cout <<"Setting to  position:\t"<<position<<"\tangle:\t"<<angle<<endl;

    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    rdet.setToDetector(position, angle);
    StiDetector* layer = *rdet;
    if (!layer) {
	cout <<"Error in setSectorAndPadrow"<<endl;
	return;
    }
    cout <<"Detector Set To: "<<layer->getName()<<endl;
    showCurrentDetector();
}

void MainFrame::moveOut()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    //cout <<"MainFrame::moveOut()"<<endl;
    setCurrentDetectorToDefault();
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    cout <<"\t DetectorContainer returned: "<<rdet.moveOut()<<endl;
    //rdet.moveOut();
    showCurrentDetector();
    //cout <<"\t Leaving MainFrame::moveOut()"<<endl;
}

void MainFrame::moveIn()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    //cout <<"MainFrame::moveIn()"<<endl;
    setCurrentDetectorToDefault();
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());

    cout <<"\t DetectorContainer returned: "<<rdet.moveIn()<<endl;
    //rdet.moveIn();
    
    showCurrentDetector();
    //cout <<"\t Leaving MainFrame::moveIn()"<<endl;
}

void MainFrame::movePlusPhi()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    setCurrentDetectorToDefault();
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    rdet.movePlusPhi();
    showCurrentDetector();
}

void MainFrame::moveMinusPhi()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    setCurrentDetectorToDefault();
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    rdet.moveMinusPhi();
    showCurrentDetector();
}

void MainFrame::printHitContainerForDetector()
{
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    StiDetector* layer = *rdet;
    
    if (!layer) {
	cout <<"Error! printHitContainerForDetector(): Failed to get detector"<<endl;
	return;
    }
    cout << StiHitContainer::instance()->hits( layer->getPlacement()->getCenterRefAngle(),
					       layer->getPlacement()->getCenterRadius() )
	 <<endl;
}

void MainFrame::showCurrentDetector()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    StiRootDrawableDetector* layer = dynamic_cast<StiRootDrawableDetector*>(*rdet);
    
    if (!layer) {
	cout <<"Error! MainFrame::showCurrentDetector(): ";
	cout <<"Failed to get drawable detector"<<endl;
	return;
    }
    layer->setVisibility(true);
    layer->setColor(2);
    
    cout<<*layer<<endl;
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
    
    return;
}

void MainFrame::memoryInfo()
{
    StMemoryInfo::instance()->snapshot();
    StMemoryInfo::instance()->print();
}

void MainFrame::setCurrentDetectorToDefault()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    //cout <<"MainFrame::setCurrentDetectorToDefault()"<<endl;
    if (!mView) {
	cout <<"Error: mView not defined.  abort"<<endl;
	return;
    }
    mView->setToDefault();
}

void StiZoomSkeletonView::setToDefault()
{
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    StiRootDrawableDetector* layer = dynamic_cast<StiRootDrawableDetector*>(*rdet);
    if (!layer) {
	cout <<"Error! MainFrame::setCurrentDetectorToDefault():";
	cout <<"Failed to get drawable detector"<<endl;
	return;
    }
    layer->setColor(1);
    
    //Keep all silicon layers visible
    const string& name = layer->StiDrawable::name();
    string::size_type where = name.find("Svg");

    if ( where != name.npos && layer->isOn() ) {
	layer->setVisibility(true);
	return;
    }

    //Keep Tpc layer 45 visible
    where = name.find("Tpc");
    string::size_type where2 = name.find("Padrow_1/");
    if (where!=name.npos && where2!=name.npos && layer->isOn()) {
	layer->setVisibility(true);
    }
    else {    //else, hide!
	layer->setVisibility(false);
    }
}

void StiSkeletonView::setToDefault()
{
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    StiRootDrawableDetector* layer = dynamic_cast<StiRootDrawableDetector*>(*rdet);
    if (!layer) {
	cout <<"Error! MainFrame::setCurrentDetectorToDefault() Error:\t";
	cout <<"Failed to get drawable detector"<<endl;
	return;
    }
    layer->setColor(1);
    
    //Keep all active silicon layers visible
    const string& name = layer->StiDrawable::name();
    string::size_type where = name.find("Svg");
    
    if ( where != name.npos && layer->isOn() ) {
	layer->setVisibility(true);
	return;
    }

    //Keep Tpc layer 45 visible
    where = name.find("Tpc");
    string::size_type where2 = name.find("Padrow_45");
    if (where!=name.npos && where2!=name.npos && layer->isOn()) {
	layer->setVisibility(true);
    }
    else {    //else, hide!
	layer->setVisibility(false);
    }
}

void StiManualView::setToDefault()
{
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    StiRootDrawableDetector* layer = dynamic_cast<StiRootDrawableDetector*>(*rdet);
    if (!layer) {
	cout <<"Error! MainFrame::setCurrentDetectorToDefault():";
	cout <<"Failed to get drawable detector"<<endl;
	return;
    }
    layer->setColor(1);
}

void MainFrame::printHits()
{
    cout <<*StiHitContainer::instance()<<endl;
}

void MainFrame::toggleFitFind()
{
    int dummy;
    cout <<"Please enter a number\n\t1\tFit Tracks\n\t2\tFind Tracks"<<endl;
    cin >> dummy;
    if (dummy==1) {
	StiIOBroker::instance()->setDoTrackFit(true);
    }
    else if (dummy==2) {
	StiIOBroker::instance()->setDoTrackFit(false);
    }
    else {
	cout <<"Error: entry "<<dummy<<" invalid.  No action taken"<<endl;
    }
}

void MainFrame::printVertices()
{
    cout <<StiHitContainer::instance()->vertices()<<endl;
}

// Non members

Navigator::Navigator(const TGWindow *p, const TGWindow *main,
		     UInt_t w, UInt_t h, UInt_t options) :
    TGTransientFrame(p, main, w, h, options)
{
    int ax, ay;
    
    ChangeOptions((GetOptions() & ~kVerticalFrame) | kHorizontalFrame);
    
    f1 = new TGCompositeFrame(this, 60, 20, kVerticalFrame | kFixedWidth);
    
    mMoveIn = new TGTextButton(f1, "Move In", 1);
    mMoveOut = new TGTextButton(f1, "Move Out", 2);
    mMovePlusPhi = new TGTextButton(f1, "Move Plus Phi", 3);
    mMoveMinusPhi = new TGTextButton(f1, "Move Minus Phi", 4);
    mClose = new TGTextButton(f1, "Close", 5);

    f1->Resize(mMoveMinusPhi->GetDefaultWidth()+40, GetDefaultHeight());

    mMoveIn->Associate(this);
    mMoveOut->Associate(this);
    mMovePlusPhi->Associate(this);
    mMoveMinusPhi->Associate(this);
    mClose->Associate(this);

    fL1 = new TGLayoutHints(kLHintsTop | kLHintsExpandX,
			    2, 2, 3, 0);
    fL21 = new TGLayoutHints(kLHintsTop | kLHintsRight,
			     2, 5, 10, 0);

    f1->AddFrame(mMoveIn, fL1);
    f1->AddFrame(mMoveOut, fL1);
    f1->AddFrame(mMovePlusPhi, fL1);
    f1->AddFrame(mMoveMinusPhi, fL1);
    f1->AddFrame(mClose, fL1);
    
    AddFrame(f1, fL21);
    
    MapSubwindows();
    Resize(GetDefaultSize());
    
    // position relative to the parent's window
    Window_t wdum;
    gVirtualX->TranslateCoordinates(main->GetId(), GetParent()->GetId(),
				    (((TGFrame *) main)->GetWidth() - fWidth) >> 1,
				    (((TGFrame *) main)->GetHeight() - fHeight) >> 1,
				    ax, ay, wdum);
    Move(ax, ay);
    
    SetWindowName("ITTF Detector Navigator");
    
    MapWindow();
    //fClient->WaitFor(this);
}

Navigator::~Navigator()
{
    delete mMoveIn;
    mMoveIn=0;

    delete mMoveOut;
    mMoveOut=0;

    delete mMovePlusPhi;
    mMovePlusPhi=0;

    delete mMoveMinusPhi;
    mMoveMinusPhi=0;

    delete mClose;
    mClose=0;

    delete f1;
    f1=0;

    delete fL1;
    fL1=0;

    delete fL21;
    fL21=0;
}

void Navigator::CloseWindow()
{
    delete this;
}

Bool_t Navigator::ProcessMessage(Long_t msg, Long_t parm1, Long_t)
{
    // Process messages sent to this dialog.
    
    switch(GET_MSG(msg)) {
    case kC_COMMAND:
	
	switch(GET_SUBMSG(msg)) {
	case kCM_BUTTON:
	    switch(parm1) {
		
	    case 1:
		MainFrame::instance()->moveIn();
		break;
		
	    case 2:
		MainFrame::instance()->moveOut();
		break;
		
	    case 3:
		MainFrame::instance()->movePlusPhi();
		break;
		
	    case 4:
		MainFrame::instance()->moveMinusPhi();
		break;
		
	    case 5:
		CloseWindow();
		break;
	    }
	    break;
	    
	case kCM_RADIOBUTTON:
	    
	case kCM_CHECKBUTTON:
	    break;
	    
	default:
	    break;
	}
	break;
	
    default:
	break;
    }
    
    return kTRUE;
}

// -----------------

TestMsgBox::TestMsgBox(const TGWindow *p, const TGWindow *main,
                       UInt_t w, UInt_t h, UInt_t options) :
    TGTransientFrame(p, main, w, h, options),
    fRedTextGC(TGButton::GetDefaultGC())
{

    ULong_t red;
    fClient->GetColorByName("red", red);
    fRedTextGC.SetForeground(red);
    
    int ax, ay;
    
    ChangeOptions((GetOptions() & ~kVerticalFrame) | kHorizontalFrame);
    
    f1 = new TGCompositeFrame(this, 60, 20, kVerticalFrame | kFixedWidth);
    f2 = new TGCompositeFrame(this, 60, 20, kVerticalFrame);
    f3 = new TGCompositeFrame(f2, 60, 20, kHorizontalFrame);
    
    fTestButton = new TGTextButton(f1, "&Apply", 1, fRedTextGC());
    
    // Change background of fTestButton to green
    ULong_t green;
    fClient->GetColorByName("green", green);
    fTestButton->ChangeBackground(green);
    
    fCloseButton = new TGTextButton(f1, "&Close", 2);

    f1->Resize(fTestButton->GetDefaultWidth()+40, GetDefaultHeight());
    
    fTestButton->Associate(this);
    fCloseButton->Associate(this);
    
    fL1 = new TGLayoutHints(kLHintsTop | kLHintsExpandX,
			    2, 2, 3, 0);
    fL2 = new TGLayoutHints(kLHintsTop | kLHintsRight | kLHintsExpandX,
			    2, 5, 0, 2);
    fL21 = new TGLayoutHints(kLHintsTop | kLHintsRight,
			     2, 5, 10, 0);
    
    f1->AddFrame(fTestButton, fL1);
    f1->AddFrame(fCloseButton, fL1);    
    
    AddFrame(f1, fL21);
    
    //--------- create check and radio buttons groups
    
    fG1 = new TGGroupFrame(f3, new TGString("Message Streams"));

    
    fL3 = new TGLayoutHints(kLHintsTop | kLHintsLeft |
			    kLHintsExpandX | kLHintsExpandY,
			    2, 2, 2, 2);
    fL4 = new TGLayoutHints(kLHintsTop | kLHintsLeft,
			    0, 0, 5, 0);

    for(unsigned int iMessageType=0; iMessageType<MessageType::getNtypes();
        iMessageType++){
      MessageType *pType = MessageType::getTypeByIndex(iMessageType);

      fC.push_back( MessengerPair( pType->getCode(),
                                   new TGCheckButton(fG1, new TGHotString(pType->getName().c_str()), -1) ));
    }

    for (unsigned int i=0; i<fC.size(); ++i) {
	//cout <<"Adding Frame: "<<i<<endl;
	fG1->AddFrame(fC[i].second, fL4);
    }
    
    Messenger* msgr = Messenger::instance();
    //Set current state here!
    for (unsigned int i=0; i<fC.size(); ++i) {
	unsigned int theBit = msgr->getRoutingBits(fC[i].first);
	cout <<"Bit for message: "<<fC[i].first<<" = "<<theBit<<endl;
	if (theBit) {
	    fC[i].second->SetState(kButtonDown);
	}
    }
    
    f3->AddFrame(fG1, fL3);
    
    f2->AddFrame(f3, fL1);
    
    AddFrame(f2, fL2);
    
    MapSubwindows();
    Resize(GetDefaultSize());
    
    // position relative to the parent's window
    Window_t wdum;
    gVirtualX->TranslateCoordinates(main->GetId(), GetParent()->GetId(),
				    (((TGFrame *) main)->GetWidth() - fWidth) >> 1,
				    (((TGFrame *) main)->GetHeight() - fHeight) >> 1,
				    ax, ay, wdum);
    Move(ax, ay);
    
    SetWindowName("Activate ITTF Message Streams");
    
    MapWindow();
    //fClient->WaitFor(this);
}

// Order is important when deleting frames. Delete children first,
// parents last.

TestMsgBox::~TestMsgBox()
{
    // Delete widgets created by dialog.
    
    delete fTestButton; delete fCloseButton;
    for (unsigned int i=0; i<fC.size(); ++i) {
	delete fC[i].second;
    }

    delete f3; delete f2; delete f1;
    delete fL1; delete fL2; delete fL3; delete fL4;
    delete fL21;
}

void TestMsgBox::CloseWindow()
{
    // Close dialog in response to window manager close.
    
    delete this;
}

Bool_t TestMsgBox::ProcessMessage(Long_t msg, Long_t parm1, Long_t)
{
    // Process messages sent to this dialog.
    
    switch(GET_MSG(msg)) {
    case kC_COMMAND:
	
	switch(GET_SUBMSG(msg)) {
	case kCM_BUTTON:
	    switch(parm1) {
	    case 1:
		updateMessenger();		
		break;
		
	    case 2:
		CloseWindow();
		break;
		
	    }
	    break;
	    
	case kCM_RADIOBUTTON:
	    
	case kCM_CHECKBUTTON:
	    break;
	    
	default:
	    break;
	}
	break;
	
    default:
	break;
    }
    
    return kTRUE;
}

void TestMsgBox::updateMessenger()
{
    Messenger* msgr = Messenger::instance();
    
    cout <<endl;

    for (unsigned int j=0; j<fC.size(); ++j) {
	if (fC[j].second->GetState() == kButtonDown) {
	    cout <<"Button "<<j<<" is checked with enum: "<<fC[j].first<<endl;
	    msgr->setRoutingBits( fC[j].first );
	}
	else {
	    msgr->clearRoutingBits( fC[j].first );
	}
    }
}

//-----------------

DetectorActivator::DetectorActivator(const TGWindow *p, const TGWindow *main,
                       UInt_t w, UInt_t h, UInt_t options) :
    TGTransientFrame(p, main, w, h, options),
    fRedTextGC(TGButton::GetDefaultGC())
{

    ULong_t red;
    fClient->GetColorByName("red", red);
    fRedTextGC.SetForeground(red);
    
    int ax, ay;
    
    ChangeOptions((GetOptions() & ~kVerticalFrame) | kHorizontalFrame);
    
    f1 = new TGCompositeFrame(this, 60, 20, kVerticalFrame | kFixedWidth);
    f2 = new TGCompositeFrame(this, 60, 20, kVerticalFrame);
    f3 = new TGCompositeFrame(f2, 60, 20, kHorizontalFrame);
    
    fTestButton = new TGTextButton(f1, "&Apply", 1, fRedTextGC());
    
    fCloseButton = new TGTextButton(f1, "&Close", 2);

    f1->Resize(fTestButton->GetDefaultWidth()+40, GetDefaultHeight());
    
    fTestButton->Associate(this);
    fCloseButton->Associate(this);
    
    fL1 = new TGLayoutHints(kLHintsTop | kLHintsExpandX,
			    2, 2, 3, 0);
    fL2 = new TGLayoutHints(kLHintsTop | kLHintsRight | kLHintsExpandX,
			    2, 5, 0, 2);
    fL21 = new TGLayoutHints(kLHintsTop | kLHintsRight,
			     2, 5, 10, 0);
    
    f1->AddFrame(fTestButton, fL1);
    f1->AddFrame(fCloseButton, fL1);    
    
    AddFrame(f1, fL21);
    
    //--------- create check and radio buttons groups

    //cout <<"Make fG1-fG4"<<endl;
    fG1 = new TGGroupFrame(f3, new TGString("ITTF Detector Layers"));
    //cout <<"done"<<endl;
    
    fL3 = new TGLayoutHints(kLHintsTop | kLHintsLeft |
			    kLHintsExpandX | kLHintsExpandY,
			    2, 2, 2, 2);
    fL4 = new TGLayoutHints(kLHintsTop | kLHintsLeft,
			    0, 0, 5, 0);
    
    //Build Dynamically from Detector Tree:
    StiDetectorContainer* detStore = StiDetectorContainer::instance();
    const data_node* root = detStore->root();
    const data_node* midRapidity = *(root->begin());
    if (midRapidity->getName()!="midrapidity") {
	cout <<"DetectorActivator::DetectorActivator() ERROR:\t"
	     <<"Did not find midrapidity region.  Undefined behavior"<<endl;
    }
    
    //cout <<"Loop on radii"<<endl;
    //Now loop on radii:
    typedef data_node::vec_type vecType;
    for (vecType::const_iterator it=midRapidity->begin(); it!=midRapidity->end(); ++it) {
	string tempName = (*it)->getName();
	
	//Check first node in phi for active key
	StiDetector* tempDetector = (*(*it)->begin())->getData();
	if (!tempDetector) {
	    cout <<"DetectorActivator::DetectorActivator() ERROR:\t"
		 <<"Null StiDetector on phi-node.  Undefined behavior"<<endl;
	}
	
	//check if it's from the tpc.  if so, skip it
	string::size_type where = tempDetector->getName().find("Tpc");
	if ( where != tempDetector->getName().npos ) { //it's from tpc
	    //skip it
	}
	else {
	    TGCheckButton* tempButton = new TGCheckButton(fG1, new TGHotString(tempName.c_str()), -1);
	    bool active = tempDetector->isOn();
	    if (active) {
		tempButton->SetState(kButtonDown);
	    }
	    
	    DetectorActivatePair tempPair(tempName, tempButton);
	    fC.push_back(tempPair);
	}
    }
    
    //cout <<"\tdone"<<endl;
    
    //put 20 buttons/vertical frame
    //cout <<"Hang buttons on frame"<<endl;
    for (unsigned int i=0; i<fC.size(); ++i) {
	fG1->AddFrame(fC[i].second, fL4);
    }
    //cout <<"\tdone"<<endl;
    
    //cout <<"Add Frames fG1-fG4 to f3"<<endl;
    f3->AddFrame(fG1, fL3);
    //cout <<"\tdone"<<endl;
    
    f2->AddFrame(f3, fL1);
    AddFrame(f2, fL2);
    MapSubwindows();
    Resize(GetDefaultSize());
    
    // position relative to the parent's window
    Window_t wdum;
    gVirtualX->TranslateCoordinates(main->GetId(), GetParent()->GetId(),
				    (((TGFrame *) main)->GetWidth() - fWidth) >> 1,
				    (((TGFrame *) main)->GetHeight() - fHeight) >> 1,
				    ax, ay, wdum);
    Move(ax, ay);
    
    SetWindowName("ITTF Inner Detectors");
    
    MapWindow();
    //fClient->WaitFor(this);
}

// Order is important when deleting frames. Delete children first,
// parents last.

DetectorActivator::~DetectorActivator()
{
    // Delete widgets created by dialog.
    
    delete fTestButton; delete fCloseButton;
    for (unsigned int i=0; i<fC.size(); ++i) {
	delete fC[i].second;
    }

    delete f3; delete f2; delete f1;
    delete fL1; delete fL2; delete fL3; delete fL4;
    delete fL21;
}

void DetectorActivator::CloseWindow()
{
    // Close dialog in response to window manager close.
    
    delete this;
}

Bool_t DetectorActivator::ProcessMessage(Long_t msg, Long_t parm1, Long_t)
{
    // Process messages sent to this dialog.
    
    switch(GET_MSG(msg)) {
    case kC_COMMAND:
	
	switch(GET_SUBMSG(msg)) {
	case kCM_BUTTON:
	    switch(parm1) {
	    case 1:
		updateDetectors();		
		break;
		
	    case 2:
		CloseWindow();
		break;
		
	    }
	    break;
	    
	case kCM_RADIOBUTTON:
	    
	case kCM_CHECKBUTTON:
	    break;
	    
	default:
	    break;
	}
	break;
	
    default:
	break;
    }
    
    return kTRUE;
}

void DetectorActivator::updateDetectors()
{
    for (unsigned int j=0; j<fC.size(); ++j) {
	if (fC[j].second->GetState() == kButtonDown) {
	    activateLayer(fC[j].first, true);
	}
	else {
	    activateLayer(fC[j].first, false);
	}
    }
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();

}

void DetectorActivator::activateLayer(const string& name, bool on)
{
    StiDetectorContainer* detStore = StiDetectorContainer::instance();
    const data_node* root = detStore->root();
    const data_node* midRapidity = *(root->begin());
    if (midRapidity->getName()!="midrapidity") {
	cout <<"DetectorActivator::activateNode() ERROR:\t"
	     <<"Did not find midrapidity region.  Undefined behavior"<<endl;
    }
    
    typedef data_node::vec_type vecType;

    //find this radial layer
    //SameNodeName<StiDetector> mySameName(name);
    
    vecType::const_iterator where = find_if(midRapidity->begin(), midRapidity->end(),
					    //mySameName);
					    SameNodeName<StiDetector>(name));
    if (where==midRapidity->end()) {
	cout <<"DetectorActivator::DetectorActivator() ERROR:\t"
	     <<"Node "<<name<<" not found in tree.  Abort"<<endl;
	return;
    }

    //we'll write the loop by hand so that it's easy to see what's going on
    //This will not be used in real running, so efficiency not so important
    for (vecType::iterator it=(*where)->begin(); it!=(*where)->end(); ++it) {
	StiDetector* tempDetector = (*it)->getData();
	if (!tempDetector) {
	    cout <<"DetectorActivator::DetectorActivator() ERROR:\t"
		 <<"Null StiDetector on phi-node.  Abort"<<endl;
	    return;
	}
	//now we have the detector, do what we like:
	tempDetector->setIsOn(on);
	//cout <<"Setting detector  "<<tempDetector->getName()<<" to state isOn="<<on<<endl;
	StiDrawable* tempDrawable = dynamic_cast<StiDrawable*>(tempDetector);
	if (tempDrawable) {
	    tempDrawable->setVisibility(on);
	    //cout <<"Setting detector  "<<tempDetector->getName()
	    //<<" to state visible="<<on<<endl;	    
	}
	else {
	    cout <<"DetectorActivator::DetectorActivator() ERROR:\t"
		 <<"Cast to drawable failed."<<endl;
	}
	
    }
}

EntryTestDlg::EntryTestDlg(const TGWindow * p, const TGWindow * main)
    : TGTransientFrame(p, main, 10, 10, kHorizontalFrame)
{
    // build widgets
    fF1 = new TGVerticalFrame(this, 200, 300);
    fL1 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 2, 2, 2);
    AddFrame(fF1, fL1);

    fL2 = new TGLayoutHints(kLHintsCenterY | kLHintsRight, 2, 2, 2, 2);

    makeNumberEntries();
    
    fF2 = new TGVerticalFrame(this, 200, 500);
    fL3 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 2, 2, 2);
    AddFrame(fF2, fL3);
    
    fSetButton = new TGTextButton(fF2, " Apply ", 2);
    fSetButton->Associate(this);
    fF2->AddFrame(fSetButton, fL3);
    
    fExitButton = new TGTextButton(fF2, " Close ", 1);
    fExitButton->Associate(this);
    fF2->AddFrame(fExitButton, fL3);
    
    // set dialog box title
    SetWindowName("Display Options");
    SetIconName("Display Options");
    SetClassHints("DisplayOptions", "DisplayOptions");
    // resize & move to center
    MapSubwindows();
    UInt_t width = GetDefaultWidth();
    UInt_t height = GetDefaultHeight();
    Resize(width, height);

    Int_t ax;
    Int_t ay;
    if (main) {
	Window_t wdum;
	gVirtualX->TranslateCoordinates(main->GetId(), GetParent()->GetId(),
					(((TGFrame *) main)->GetWidth() -
					 fWidth) >> 1,
					(((TGFrame *) main)->GetHeight() -
					 fHeight) >> 1, ax, ay, wdum);
;    } else {
	UInt_t root_w, root_h;
	gVirtualX->GetWindowSize(fClient->GetRoot()->GetId(), ax, ay,
				 root_w, root_h);
	ax = (root_w - fWidth) >> 1;
	ay = (root_h - fHeight) >> 1;
    }
    Move(ax, ay);
    SetWMPosition(ax, ay);
    // make the message box non-resizable
    SetWMSize(width, height);
    SetWMSizeHints(width, height, width, height, 0, 0);
    SetMWMHints(kMWMDecorAll | kMWMDecorResizeH | kMWMDecorMaximize |
		kMWMDecorMinimize | kMWMDecorMenu,
		kMWMFuncAll | kMWMFuncResize | kMWMFuncMaximize |
		kMWMFuncMinimize, kMWMInputModeless);
    
    MapWindow();
    
    //fClient->WaitFor(this);
}

void EntryTestDlg::makeNumberEntries()
{
    StiGuiIOBroker* broker = StiGuiIOBroker::instance();
    
    //Marked Hit Color
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MarkedHitColor",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->markedHitColor() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 10);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Marked Hit Color") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //UnMarked Hit Color
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("UnMarkedHitColor",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->unMarkedHitColor() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 10);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "UnMarked Hit Color") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Marked Hit Style
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MarkedHitStyle",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->markedHitStyle() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 30);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Marked Hit Style") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //UnMarked Hit Style
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("UnMarkedHitStyle",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->unMarkedHitStyle() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 30);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "UnMarked Hit Style") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Marked Hit Size
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MarkedHitSize",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->markedHitSize() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESRealOne, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 1.);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Marked Hit Size") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //UnMarked Hit Size
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("UnMarkedHitSize",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->unMarkedHitSize() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESRealOne, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 1.);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "UnMarked Hit Size") );
    fF.back()->AddFrame(fLabel.back(), fL2);


}

EntryTestDlg::~EntryTestDlg()
{
    if (fNumericEntries.size()!=fLabel.size() || fLabel.size()!=fF.size()) {
	cout <<"EntryTestDlg::~EntryTestDlg. ERROR:\t"
	     <<"Mismatch in cleanup vector size"<<endl;
    }
    for (unsigned int i=0; i<fF.size(); ++i) {
	delete fNumericEntries[i].second;
	delete fLabel[i];
	delete fF[i];
	fNumericEntries[i].second=0;
	fLabel[i]=0;
	fF[i]=0;
    }
    delete fSetButton;
    delete fExitButton;
    delete fF1;
    delete fF2;
    delete fL1;
    delete fL2;
    delete fL3;
}

void EntryTestDlg::CloseWindow()
{
    delete this;
}

void EntryTestDlg::SetLimits()
{
    StiGuiIOBroker* broker = StiGuiIOBroker::instance();

    for (NumberEntryVec::const_iterator it=fNumericEntries.begin();
	 it!=fNumericEntries.end(); ++it) {
	//cout <<"Number Entry\t"<<(*it).first<<" has value:\t"<<(*it).second->GetNumber()<<endl;
	const string& name = (*it).first;
	
	if (name=="MarkedHitColor") {
	    broker->setMarkedHitColor( (*it).second->GetNumber() );
	}
	else if (name=="UnMarkedHitColor") {
	    broker->setUnMarkedHitColor( (*it).second->GetNumber() );
	}
	else if (name=="MarkedHitStyle") {
	    broker->setMarkedHitStyle( (*it).second->GetNumber() );
	}
	else if (name=="UnMarkedHitStyle") {
	    broker->setUnMarkedHitStyle( (*it).second->GetNumber() );
	}
	else if (name=="MarkedHitSize") {
	    broker->setMarkedHitSize( (*it).second->GetNumber() );
	}
	else if (name=="UnMarkedHitSize") {
	    broker->setUnMarkedHitSize( (*it).second->GetNumber() );
	}
	else {
	    cout <<"EntryTestDlg::SetLimits(). ERROR:\t"
		 <<"Unknown name for NumberEntry:\t"<<name
		 <<"\tYou had a compile time error"<<endl;
	}
    }
}

Bool_t EntryTestDlg::ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2)
{
    switch (GET_MSG(msg)) {
    case kC_COMMAND:
	{
	    switch (GET_SUBMSG(msg)) {
	    case kCM_BUTTON:
		{
		    switch (parm1) {
			// exit button
		    case 1:
			{
			    CloseWindow();
			    break;
			}
			// set button
		    case 2:
			{
			    SetLimits();
			    break;
			}
		    }
		    break;
		}
	    }
	    break;
	}
    }
    return kTRUE;
}


SeedFinderIO::SeedFinderIO(const TGWindow * p, const TGWindow * main)
    : TGTransientFrame(p, main, 10, 10, kHorizontalFrame)
{
    // build widgets
    fF1 = new TGVerticalFrame(this, 200, 300);
    fL1 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 2, 2, 2);
    AddFrame(fF1, fL1);

    fL2 = new TGLayoutHints(kLHintsCenterY | kLHintsRight, 2, 2, 2, 2);

    makeNumberEntries();
    
    fF2 = new TGVerticalFrame(this, 200, 500);
    fL3 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 2, 2, 2);
    AddFrame(fF2, fL3);
    
    fSetButton = new TGTextButton(fF2, " Apply ", 2);
    fSetButton->Associate(this);
    fF2->AddFrame(fSetButton, fL3);
    
    fExitButton = new TGTextButton(fF2, " Close ", 1);
    fExitButton->Associate(this);
    fF2->AddFrame(fExitButton, fL3);
    
    // set dialog box title
    SetWindowName("Display Options");
    SetIconName("Display Options");
    SetClassHints("DisplayOptions", "DisplayOptions");
    // resize & move to center
    MapSubwindows();
    UInt_t width = GetDefaultWidth();
    UInt_t height = GetDefaultHeight();
    Resize(width, height);

    Int_t ax;
    Int_t ay;
    if (main) {
	Window_t wdum;
	gVirtualX->TranslateCoordinates(main->GetId(), GetParent()->GetId(),
					(((TGFrame *) main)->GetWidth() -
					 fWidth) >> 1,
					(((TGFrame *) main)->GetHeight() -
					 fHeight) >> 1, ax, ay, wdum);
;    } else {
	UInt_t root_w, root_h;
	gVirtualX->GetWindowSize(fClient->GetRoot()->GetId(), ax, ay,
				 root_w, root_h);
	ax = (root_w - fWidth) >> 1;
	ay = (root_h - fHeight) >> 1;
    }
    Move(ax, ay);
    SetWMPosition(ax, ay);
    // make the message box non-resizable
    SetWMSize(width, height);
    SetWMSizeHints(width, height, width, height, 0, 0);
    SetMWMHints(kMWMDecorAll | kMWMDecorResizeH | kMWMDecorMaximize |
		kMWMDecorMinimize | kMWMDecorMenu,
		kMWMFuncAll | kMWMFuncResize | kMWMFuncMaximize |
		kMWMFuncMinimize, kMWMInputModeless);
    
    MapWindow();
    
    //fClient->WaitFor(this);
}

void SeedFinderIO::makeNumberEntries()
{
    StiIOBroker* broker = StiIOBroker::instance();
    
    //Minimum Padrow
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MinimumPadrow",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->tphfMinPadrow() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 1, 45);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Minimum Padrow") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Maximum Padrow
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MaximumPadrow",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->tphfMaxPadrow() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 1, 45);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Maximum Padrow") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Lower Bound
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("LowerBound",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->etsfLowerBound() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMin, 0);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Lower Bound for Good Association") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Max Hits
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MaxHits",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->etsfMaxHits() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 1, 45);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Number of Hits in Seed") );
    fF.back()->AddFrame(fLabel.back(), fL2);

}

SeedFinderIO::~SeedFinderIO()
{
    if (fNumericEntries.size()!=fLabel.size() || fLabel.size()!=fF.size()) {
	cout <<"SeedFinderIO::~SeedFinderIO. ERROR:\t"
	     <<"Mismatch in cleanup vector size"<<endl;
    }
    for (unsigned int i=0; i<fF.size(); ++i) {
	delete fNumericEntries[i].second;
	delete fLabel[i];
	delete fF[i];
	fNumericEntries[i].second=0;
	fLabel[i]=0;
	fF[i]=0;
    }
    delete fSetButton;
    delete fExitButton;
    delete fF1;
    delete fF2;
    delete fL1;
    delete fL2;
    delete fL3;
}

void SeedFinderIO::CloseWindow()
{
    delete this;
}

void SeedFinderIO::SetLimits()
{
    StiIOBroker* broker = StiIOBroker::instance();

    for (NumberEntryVec::const_iterator it=fNumericEntries.begin();
	 it!=fNumericEntries.end(); ++it) {
	//cout <<"Number Entry\t"<<(*it).first<<" has value:\t"<<(*it).second->GetNumber()<<endl;
	const string& name = (*it).first;
	
	if (name=="MinimumPadrow") {
	    broker->setTPHFMinPadrow( (*it).second->GetNumber() );
	}
	else if (name=="MaximumPadrow") {	
	    broker->setTPHFMaxPadrow( (*it).second->GetNumber() );
	}
	else if (name=="LowerBound") {	
	    broker->setETSFLowerBound( (*it).second->GetNumber() );
	}
	else if (name=="MaxHits") {
	    broker->setETSFMaxHits( (*it).second->GetNumber() );
	}
	else {
	    cout <<"SeedFinderIO::SetLimits(). ERROR:\t"
		 <<"Unknown name for NumberEntry:\t"<<name
		 <<"\tYou had a compile time error"<<endl;
	}
    }
}

Bool_t SeedFinderIO::ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2)
{
    switch (GET_MSG(msg)) {
    case kC_COMMAND:
	{
	    switch (GET_SUBMSG(msg)) {
	    case kCM_BUTTON:
		{
		    switch (parm1) {
			// exit button
		    case 1:
			{
			    CloseWindow();
			    break;
			}
			// set button
		    case 2:
			{
			    SetLimits();
			    break;
			}
		    }
		    break;
		}
	    }
	    break;
	}
    }
    return kTRUE;
}

LocalSeedFinderIO::LocalSeedFinderIO(const TGWindow * p, const TGWindow * main)
    : TGTransientFrame(p, main, 10, 10, kHorizontalFrame)
{
    // build widgets
    fF1 = new TGVerticalFrame(this, 200, 300);
    fL1 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 2, 2, 2);
    AddFrame(fF1, fL1);

    fL2 = new TGLayoutHints(kLHintsCenterY | kLHintsRight, 2, 2, 2, 2);

    makeNumberEntries();
    
    fF2 = new TGVerticalFrame(this, 200, 500);
    fL3 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 2, 2, 2);
    AddFrame(fF2, fL3);
    
    fSetButton = new TGTextButton(fF2, " Apply ", 2);
    fSetButton->Associate(this);
    fF2->AddFrame(fSetButton, fL3);
    
    fExitButton = new TGTextButton(fF2, " Close ", 1);
    fExitButton->Associate(this);
    fF2->AddFrame(fExitButton, fL3);
    
    // set dialog box title
    SetWindowName("Local Seed Finder Options");
    SetIconName("Local Seed Finder Options");
    SetClassHints("LocalSeedFinderOptions", "LocalSeedFinderOptions");
    // resize & move to center
    MapSubwindows();
    UInt_t width = GetDefaultWidth();
    UInt_t height = GetDefaultHeight();
    Resize(width, height);

    Int_t ax;
    Int_t ay;
    if (main) {
	Window_t wdum;
	gVirtualX->TranslateCoordinates(main->GetId(), GetParent()->GetId(),
					(((TGFrame *) main)->GetWidth() -
					 fWidth) >> 1,
					(((TGFrame *) main)->GetHeight() -
					 fHeight) >> 1, ax, ay, wdum);
	;    } else {
	    UInt_t root_w, root_h;
	    gVirtualX->GetWindowSize(fClient->GetRoot()->GetId(), ax, ay,
				     root_w, root_h);
	    ax = (root_w - fWidth) >> 1;
	    ay = (root_h - fHeight) >> 1;
	}
    Move(ax, ay);
    SetWMPosition(ax, ay);
    // make the message box non-resizable
    SetWMSize(width, height);
    SetWMSizeHints(width, height, width, height, 0, 0);
    SetMWMHints(kMWMDecorAll | kMWMDecorResizeH | kMWMDecorMaximize |
		kMWMDecorMinimize | kMWMDecorMenu,
		kMWMFuncAll | kMWMFuncResize | kMWMFuncMaximize |
		kMWMFuncMinimize, kMWMInputModeless);
    
    MapWindow();
    
    //fClient->WaitFor(this);
}

void LocalSeedFinderIO::makeNumberEntries()
{
    StiIOBroker* broker = StiIOBroker::instance();
    
    //Y-Window
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("YWindow",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ltsfYWindow() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESRealOne, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0., 10.);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Connection Search Window in Local Y (cm)") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Z-Window
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("ZWindow",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ltsfZWindow() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESRealOne, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0., 20.);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Connection Search Window in Global Z (cm)") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Seed Length
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("SeedLength",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ltsfSeedLength() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 45);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Length of Connection") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Extrapolation Y-Window
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("ExtrapYWindow",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ltsfExtrapYWindow() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESRealOne, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0., 10.);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Extrapolation Search Window in Local Y (cm)") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Extrapolation Z-Window
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("ExtrapZWindow",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ltsfExtrapZWindow() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESRealOne, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0., 20.);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Extrapolation Search Window in Global Z (cm)") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Max skipped during Extrapolation
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("ExtrapMaxSkipped",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ltsfExtrapMaxSkipped() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 45);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Largest Gap During Extrapolation") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Extrapolation Length
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("ExtrapLength",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ltsfExtrapLength() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 45);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Length of Extrapolation") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Add a toggle to include the vertex
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    TGCheckButton* tempButton = new TGCheckButton(fF.back(), new TGHotString("Use Vertex"), -1);
    if ( broker->ltsfUseVertex()==true) {
	tempButton->SetState(kButtonDown);
    }
    DetectorActivatePair tempPair("UseVertex", tempButton);
    fC.push_back(tempPair);
    fF.back()->AddFrame(tempButton, fL2);

    //Add a toggle to turn on/off the fit option
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    TGCheckButton* tempButton2 =
	new TGCheckButton(fF.back(), new TGHotString("Do Helix Fit \n (default==calulate helix)"), -1);
    if ( broker->ltsfDoHelixFit()==true) {
	tempButton2->SetState(kButtonDown);
    }
    fC.push_back( DetectorActivatePair("DoHelixFit", tempButton2) );
    fF.back()->AddFrame(tempButton2, fL2);
    
}

LocalSeedFinderIO::~LocalSeedFinderIO()
{
    for (unsigned int i=0; i<fNumericEntries.size(); ++i) {
	delete fNumericEntries[i].second;
	fNumericEntries[i].second=0;
    }

    for (unsigned int i=0; i<fC.size(); ++i) {
	delete fC[i].second;
	fC[i].second=0;
    }
    
    for (unsigned int i=0; i<fLabel.size(); ++i ) {	
	delete fLabel[i];
	fLabel[i]=0;
    }

    for (unsigned int i=0; i<fF.size(); ++i) {
	delete fF[i];
	fF[i]=0;
    }

    delete fSetButton;
    delete fExitButton;
    delete fF1;
    delete fF2;
    delete fL1;
    delete fL2;
    delete fL3;
}

void LocalSeedFinderIO::CloseWindow()
{
    delete this;
}

void LocalSeedFinderIO::SetLimits()
{
    StiIOBroker* broker = StiIOBroker::instance();

    for (NumberEntryVec::const_iterator it=fNumericEntries.begin();
	 it!=fNumericEntries.end(); ++it) {
	//cout <<"Number Entry\t"<<(*it).first<<" has value:\t"<<(*it).second->GetNumber()<<endl;
	const string& name = (*it).first;
	
	if (name=="YWindow") {
	    broker->setLTSFYWindow( (*it).second->GetNumber() );
	}
	else if (name=="ZWindow") {
	    broker->setLTSFZWindow( (*it).second->GetNumber() );
	}
	else if (name=="SeedLength") {
	    broker->setLTSFSeedLength( (*it).second->GetNumber() );
	}
	else if (name=="ExtrapYWindow") {
	    broker->setLTSFExtrapYWindow( (*it).second->GetNumber() );
	}
	else if (name=="ExtrapZWindow") {
	    broker->setLTSFExtrapZWindow( (*it).second->GetNumber() );
	}
	else if (name=="ExtrapMaxSkipped") {
	    broker->setLTSFExtrapMaxSkipped( (*it).second->GetNumber() );
	}
	else if (name=="ExtrapLength") {
	    broker->setLTSFExtrapLength( (*it).second->GetNumber() );
	}
	else {
	    cout <<"LocalSeedFinderIO::SetLimits(). ERROR:\t"
		 <<"Unknown name for NumberEntry:\t"<<name
		 <<"\tYou had a compile time error"<<endl;
	}
    }

    //Now take care of toggle buttons
    for (unsigned int j=0; j<fC.size(); ++j) {
	const string& tag = fC[j].first;

	if (tag=="UseVertex") {
	    broker->setLTSFUseVertex( fC[j].second->GetState()==kButtonDown );
	}
	else if (tag=="DoHelixFit") {
	    broker->setLTSFDoHelixFit( fC[j].second->GetState()==kButtonDown );
	}
	else {
	    cout <<"LocalSeedFinderIO::ProcessMessage. ERROR:\t"
		 <<"Tag:\t"<<tag<<" is unkown type.  You had a compile time error."<<endl;
	}
    }

}

Bool_t LocalSeedFinderIO::ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2)
{
    switch (GET_MSG(msg)) {
    case kC_COMMAND:
	{
	    switch (GET_SUBMSG(msg)) {
	    case kCM_BUTTON:
		{
		    switch (parm1) {
			// exit button
		    case 1:
			{
			    CloseWindow();
			    break;
			}
			// set button
		    case 2:
			{
			    SetLimits();
			    break;
			}
		    }
		    break;
		}
	    }
	    break;
	}
    }
    return kTRUE;
}


//Kalman Track Finder IO:

KalmanTrackFinderIO::KalmanTrackFinderIO(const TGWindow * p, const TGWindow * main)
    : TGTransientFrame(p, main, 10, 10, kHorizontalFrame)
{
    // build widgets
    fF1 = new TGVerticalFrame(this, 200, 300);
    fL1 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 2, 2, 2);
    AddFrame(fF1, fL1);

    fL2 = new TGLayoutHints(kLHintsCenterY | kLHintsRight, 2, 2, 2, 2);

    makeNumberEntries();
    
    fF2 = new TGVerticalFrame(this, 200, 500);
    fL3 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 2, 2, 2);
    AddFrame(fF2, fL3);
    
    fSetButton = new TGTextButton(fF2, " Apply ", 2);
    fSetButton->Associate(this);
    fF2->AddFrame(fSetButton, fL3);
    
    fExitButton = new TGTextButton(fF2, " Close ", 1);
    fExitButton->Associate(this);
    fF2->AddFrame(fExitButton, fL3);
    
    // set dialog box title
    SetWindowName("Kalman Track Finder Options");
    SetIconName("Kalman Track Finder Options");
    SetClassHints("KalmanTrackFinderOptions", "KalmanTrackFinderOptions");
    // resize & move to center
    MapSubwindows();
    UInt_t width = GetDefaultWidth();
    UInt_t height = GetDefaultHeight();
    Resize(width, height);

    Int_t ax;
    Int_t ay;
    if (main) {
	Window_t wdum;
	gVirtualX->TranslateCoordinates(main->GetId(), GetParent()->GetId(),
					(((TGFrame *) main)->GetWidth() -
					 fWidth) >> 1,
					(((TGFrame *) main)->GetHeight() -
					 fHeight) >> 1, ax, ay, wdum);
	;    } else {
	    UInt_t root_w, root_h;
	    gVirtualX->GetWindowSize(fClient->GetRoot()->GetId(), ax, ay,
				     root_w, root_h);
	    ax = (root_w - fWidth) >> 1;
	    ay = (root_h - fHeight) >> 1;
	}
    Move(ax, ay);
    SetWMPosition(ax, ay);
    // make the message box non-resizable
    SetWMSize(width, height);
    SetWMSizeHints(width, height, width, height, 0, 0);
    SetMWMHints(kMWMDecorAll | kMWMDecorResizeH | kMWMDecorMaximize |
		kMWMDecorMinimize | kMWMDecorMenu,
		kMWMFuncAll | kMWMFuncResize | kMWMFuncMaximize |
		kMWMFuncMinimize, kMWMInputModeless);
    
    MapWindow();
    
    //fClient->WaitFor(this);
}

void KalmanTrackFinderIO::makeNumberEntries()
{
    StiIOBroker* broker = StiIOBroker::instance();

    //Max Chi2 for selection
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MaxChi2ForSelection",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ktfMaxChi2ForSelection() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESRealOne, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0., 50.);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Max Chi2 for Selection") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //B-Field
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("BField",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ktfBField() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESRealTwo);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, -2., 2.);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Magnetic Field (Tesla)") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Max Chi2 for selection
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MassHypothesis",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ktfMassHypothesis() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESRealFour, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0., 1.);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Kalman Mass Hypothesis(GeV)") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Min ContiguousHitCount
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MinContHitCount",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ktfMinContiguousHitCount() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 60);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Min. Contiguous Hit Count") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Max Null Count
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MaxNullCount",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ktfMaxNullCount() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 60);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Max. Null Count") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Max Contiguous Null Count
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MaxContNullCount",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ktfMaxContiguousNullCount() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESInteger, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 60);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Max. Contiguous Null Count") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Min Search Radius
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MinSearchRadius",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ktfMinSearchRadius() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESRealFour, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 10);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Min. Search Radius") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Max Search Radius
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    fNumericEntries.push_back( NamedNumberEntry("MaxSearchRadius",
						new TGNumberEntry( fF.back() ) ) );
    fNumericEntries.back().second->SetNumber( broker->ktfMaxSearchRadius() );
    fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESRealFour, TGNumberFormat::kNEAPositive);
    fNumericEntries.back().second->SetLimits(TGNumberFormat::kNELLimitMinMax, 0, 10);
    fNumericEntries.back().second->Associate(this);
    fF.back()->AddFrame(fNumericEntries.back().second, fL2);
    fLabel.push_back( new TGLabel(fF.back(), "Max. Search Radius") );
    fF.back()->AddFrame(fLabel.back(), fL2);

    //Toggle for Mcs Calculation
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    TGCheckButton* tempButton = new TGCheckButton(fF.back(),
						  new TGHotString("MCS Calculated"), -1);
    if ( broker->ktfMcsCalculated()==true) {
	tempButton->SetState(kButtonDown);
    }
    DetectorActivatePair tempPair("McsCalculated", tempButton);
    fC.push_back(tempPair);
    fF.back()->AddFrame(tempButton, fL2);
    
    //Toggle for Eloss Calculation
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    TGCheckButton* tempButton2 = new TGCheckButton(fF.back(),
						   new TGHotString("E-Loss Calculated"), -1);
    if ( broker->ktfElossCalculated()==true) {
	tempButton2->SetState(kButtonDown);
    }
    DetectorActivatePair tempPair2("ELossCalculated", tempButton2);
    fC.push_back(tempPair2);
    fF.back()->AddFrame(tempButton2, fL2);

    //Toggle for Helix Extrapolation    
    fF.push_back( new TGHorizontalFrame(fF1, 200, 30) );
    fF1->AddFrame(fF.back(), fL2);
    TGCheckButton* tempButton3 = new TGCheckButton(fF.back(),
						   new TGHotString("Helix Extrapolation"), -1);
    if ( broker->ktfUseHelixExtrapolation()==true) {
	tempButton3->SetState(kButtonDown);
    }
    DetectorActivatePair tempPair3("HelixExtrapolation", tempButton3);
    fC.push_back(tempPair3);
    fF.back()->AddFrame(tempButton3, fL2);

}

KalmanTrackFinderIO::~KalmanTrackFinderIO()
{
    //if (fNumericEntries.size()!=fLabel.size() || fLabel.size()!=fF.size()) {
    //cout <<"KalmanTrackFinderIO::~KalmanTrackFinderIO. ERROR:\t"
    //     <<"Mismatch in cleanup vector size"<<endl;
    //}
    for (unsigned int i=0; i<fNumericEntries.size(); ++i) {
	delete fNumericEntries[i].second;
	fNumericEntries[i].second=0;
    }

    for (unsigned int i=0; i<fC.size(); ++i) {
	delete fC[i].second;
	fC[i].second=0;
    }
    
    for (unsigned int i=0; i<fLabel.size(); ++i ) {	
	delete fLabel[i];
	fLabel[i]=0;
    }

    for (unsigned int i=0; i<fF.size(); ++i) {
	delete fF[i];
	fF[i]=0;
    }

    delete fSetButton;
    delete fExitButton;
    delete fF1;
    delete fF2;
    delete fL1;
    delete fL2;
    delete fL3;
}

void KalmanTrackFinderIO::CloseWindow()
{
    delete this;
}

void KalmanTrackFinderIO::SetLimits()
{
    StiIOBroker* broker = StiIOBroker::instance();

    for (NumberEntryVec::const_iterator it=fNumericEntries.begin();
	 it!=fNumericEntries.end(); ++it) {
	//cout <<"Number Entry\t"<<(*it).first<<" has value:\t"<<(*it).second->GetNumber()<<endl;
	const string& name = (*it).first;
    
	if (name=="MaxChi2ForSelection") {
	    broker->setKTFMaxChi2ForSelection( (*it).second->GetNumber() );
	}
	else if (name=="BField") {
	    broker->setKTFBField( (*it).second->GetNumber() );
	}
	else if (name=="MassHypothesis") {
	    broker->setKTFMassHypothesis( (*it).second->GetNumber() );
	}
	else if (name=="MinContHitCount") {
	    broker->setKTFMinContiguousHitCount( (*it).second->GetNumber() );
	}
	else if (name=="MaxNullCount") {
	    broker->setKTFMaxNullCount( (*it).second->GetNumber() );
	}
	else if (name=="MaxContNullCount") {
	    broker->setKTFMaxContiguousNullCount( (*it).second->GetNumber() );
	}
	else if (name=="MinSearchRadius") {
	    broker->setKTFMinSearchRadius( (*it).second->GetNumber() );
	}
	else if (name=="MaxSearchRadius") {
	    broker->setKTFMaxSearchRadius( (*it).second->GetNumber() );
	}
	else {
	    cout <<"KalmanTrackFinderIO::SetLimits(). ERROR:\t"
		 <<"Unknown name for NumberEntry:\t"<<name
		 <<"\tYou had a compile time error"<<endl;
	}
    }

    //Now take care of toggle buttons
    for (unsigned int j=0; j<fC.size(); ++j) {
	const string& tag = fC[j].first;

	if (tag=="McsCalculated") {
	    broker->setKTFMcsCalculated( fC[j].second->GetState()==kButtonDown );
	}
	else if (tag=="ELossCalculated") {
	    broker->setKTFElossCalculated( fC[j].second->GetState()==kButtonDown );
	}
	else if (tag=="HelixExtrapolation") {
	    broker->setKTFUseHelixExtrapolation( fC[j].second->GetState()==kButtonDown );
	}
	else {
	    cout <<"KalmanTrackFinderIO::ProcessMessage. ERROR:\t"
		 <<"Tag:\t"<<tag<<" is unkown type.  You had a compile time error."<<endl;
	}
    }

}

Bool_t KalmanTrackFinderIO::ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2)
{
    switch (GET_MSG(msg)) {
    case kC_COMMAND:
	{
	    switch (GET_SUBMSG(msg)) {
	    case kCM_BUTTON:
		{
		    switch (parm1) {
			// exit button
		    case 1:
			{
			    CloseWindow();
			    break;
			}
			// set button
		    case 2:
			{
			    SetLimits();
			    break;
			}
		    }
		    break;
		}
	    }
	    break;
	}
    }
    return kTRUE;
}

