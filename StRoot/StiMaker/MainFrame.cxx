//MainFrame.cxx

#include <iostream.h>

#include "TRootEmbeddedCanvas.h"
#include "TShape.h"
#include "TBRIK.h"
#include "TVolume.h"
#include "TF1.h"

//Star
#include "StChain.h"
#include "StIOMaker/StIOMaker.h"

//SCL
#include "StMemoryInfo.hh"

//Sti
#include "Sti/StiDetector.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiHitContainer.h"

//StiGui
#include "StiGui/StiRootDrawableDetector.h"
#include "StiGui/StiDisplayManager.h"

//StiMaker
#include "StiMaker.h"

#include "MainFrame.h"

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
      mTrackingMenu(0),
      fMenuBarLayout(0), fMenuBarItemLayout(0), fMenuBarHelpLayout(0),
      mchain(0), mIoMaker(0),
      fTrackingFrame(0), fFinishTrackButton(0), fFinishEventButton(0),
      fNextEventButton(0)
{
    cout <<"MainFrame::MainFrame()"<<endl;
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
    fDetectorViewMenu->AddEntry("Skeleton View", M_DetView_SkeletonView);
    fDetectorViewMenu->AddEntry("Zoom Skeleton View",M_DetView_ZoomSkeletonView);
    fDetectorViewMenu->AddPopup("&All",mAllViewMenu);
    fDetectorViewMenu->AddPopup("&Tpc",mTpcViewMenu);
    fDetectorViewMenu->AddPopup("&Svt",mSvtViewMenu);
    fDetectorViewMenu->AddPopup("&Ifc",mIfcViewMenu);

    mNavigateMenu = new TGPopupMenu(fClient->GetRoot());
    mNavigateMenu->AddEntry("Move &In",M_DetNavigate_MoveIn);
    mNavigateMenu->AddEntry("Move &Out",M_DetNavigate_MoveOut);
    mNavigateMenu->AddEntry("Move &Plus Phi",M_DetNavigate_MovePlusPhi);
    mNavigateMenu->AddEntry("Move &Minus Phi",M_DetNavigate_MoveMinusPhi);
    mNavigateMenu->AddEntry("Set Layer",M_DetNavigate_SetLayer);
    mNavigateMenu->AddEntry("Set Layer and Angle",M_DetNavigate_SetLayerAndAngle);

    fDetectorMenu = new TGPopupMenu(fClient->GetRoot());
    fDetectorMenu->AddLabel("Access to the Detector Model");
    fDetectorMenu->AddSeparator();
    fDetectorMenu->AddPopup("&Navigate", mNavigateMenu);
    fDetectorMenu->AddPopup("&Visibility", fDetectorViewMenu);

    mTrackingMenu = new TGPopupMenu(fClient->GetRoot());
    mTrackingMenu->AddLabel("Access to tracking functions");
    mTrackingMenu->AddEntry("Toggle Fit/Find",M_Tracking_ToggleFitFind);
    mTrackingMenu->AddSeparator();
    mTrackingMenu->AddEntry("Finish Track",M_Tracking_FinishTrack);
    mTrackingMenu->AddEntry("Finish Event",M_Tracking_FinishEvent);
    mTrackingMenu->AddEntry("Event Step",M_Tracking_EventStep);
    mTrackingMenu->AddEntry("N-Event Step",M_Tracking_NEventStep);
    
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
    mTrackingMenu->Associate(this);
    
    fMenuBar = new TGMenuBar(this, 1, 1, kHorizontalFrame);
    fMenuBar->AddPopup("&File", fMenuFile, fMenuBarItemLayout);
    fMenuBar->AddPopup("&Detector",fDetectorMenu, fMenuBarItemLayout);
    fMenuBar->AddPopup("&Tracking",mTrackingMenu, fMenuBarItemLayout);
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

    fFinishTrackButton = new TGTextButton(fTrackingFrame,
					  "Finish &Track", M_Tracking_FinishTrack);
    fFinishTrackButton->Associate(this);
    fFinishTrackButton->SetToolTipText("Finish The Current Track");

    fFinishEventButton = new TGTextButton(fTrackingFrame,
					  "Finish &Event", M_Tracking_FinishEvent);
    fFinishEventButton->Associate(this);
    fFinishEventButton->SetToolTipText("Finish The Current Event");


    fNextEventButton = new TGTextButton(fTrackingFrame,
					"&Next Event", M_Tracking_EventStep);
    fNextEventButton->Associate(this);
    fNextEventButton->SetToolTipText("Step To Next Event");
    
    fTrackingFrame->AddFrame(fFinishTrackButton,
			     new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2));
    fTrackingFrame->AddFrame(fFinishEventButton,
			     new TGLayoutHints(kLHintsTop | kLHintsLeft, 10, 2, 2, 2));
    fTrackingFrame->AddFrame(fNextEventButton,
			     new TGLayoutHints(kLHintsTop | kLHintsLeft, 18, 2, 2, 2));

    AddFrame(fTrackingFrame, new TGLayoutHints(kLHintsBottom | kLHintsExpandX,
					       0, 0, 1, 0));
    
    
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

    delete fFinishTrackButton;
    fFinishTrackButton=0;

    delete fFinishEventButton;
    fFinishEventButton=0;

    delete fNextEventButton;
    fNextEventButton=0;

}

void MainFrame::CloseWindow()
{
    // Got close message for this MainFrame. Terminate the application
    // or returns from the TApplication event loop (depending on the
    // argument specified in TApplication::Run()).
    
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
	    if (parm1 == M_Tracking_FinishTrack) {
		doNextStiGuiAction();
	    }
	    else if (parm1 == M_Tracking_FinishEvent) {
		finishEvent();
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
		    static TString dir("/star/data17/ITTF/");
		    TGFileInfo fi;
		    fi.fFileTypes = filetypes;
		    fi.fIniDir    = StrDup(dir);
		    new TGFileDialog(fClient->GetRoot(), this, kFDOpen, &fi);
		    printf("Open file: %s (dir: %s)\n", fi.fFilename,
			   fi.fIniDir);
		    dir = fi.fIniDir;
		    mIoMaker->Close();
		    mIoMaker->SetFile(fi.fFilename);
		}
		break;

		//Mike's stuff here:

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
		    
		//case M_Det_Navigate:
		//navigate();
		//break;

	    case M_DetView_SkeletonView:
		setSkeletonView();
		break;

	    case M_DetView_ZoomSkeletonView:
		setZoomSkeletonView();
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

	    case M_Tracking_FinishTrack:
		doNextStiGuiAction();
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

void MainFrame::doNextStiGuiAction()
{
    //cout <<"MainFrame::doNextStGuiAction()"<<endl;
    setCurrentDetectorToDefault();
    StiMaker::instance()->doNextAction();
    showCurrentDetector();
    //cout <<"\t Leaving doNextStGuiAction()"<<endl;
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
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void MainFrame::setZoomSkeletonView()
{
    StiDisplayManager::instance()->setZoomSkeletonView();
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
    rdet.moveOut();
    showCurrentDetector();
    //cout <<"\t Leaving MainFrame::moveOut()"<<endl;
}

void MainFrame::moveIn()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    //cout <<"MainFrame::moveIn()"<<endl;
    setCurrentDetectorToDefault();
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    rdet.moveIn();
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
    if ( where != name.npos ) {
	layer->setVisibility(true);
	return;
    }

    //Keep Tpc layer 45 visible
    where = name.find("Tpc");
    string::size_type where2 = name.find("45");
    if (where!=name.npos && where2!=name.npos) {
	layer->setVisibility(true);
    }
    else {    //else, hide!
	layer->setVisibility(false);
    }
    //cout <<"\t Leaving MainFrame::setCurrentDetectorToDefault()"<<endl;
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
	StiMaker::instance()->setDoFit(true);
    }
    else if (dummy==2) {
	StiMaker::instance()->setDoFit(false);
    }
    else {
	cout <<"Error: entry "<<dummy<<" invalid.  No action taken"<<endl;
    }
}

void MainFrame::printVertices()
{
    cout <<StiHitContainer::instance()->vertices()<<endl;
}

//Extras

/*

*/
