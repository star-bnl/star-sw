//MainFrame.cxx
#include <stdexcept>
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
#include "Sti/StiCompositeTreeNode.h"
#include "Sti/StlUtilities.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiIOBroker.h"
#include "Sti/Base/Messenger.h"
#include "Sti/StiDetector.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiKalmanTrackFinder.h"

#include "StiMaker/DetectorActivator.h"
#include "StiMaker/Navigator.h"
#include "StiMaker/StiView.h"
#include "StiMaker/TileFrame.h"

//StiGui
#include "StiGui/StiRootDrawableDetector.h"
#include "StiGui/StiDrawable.h"
#include "StiGui/StiRootDisplayManager.h"
#include "StiGui/StiGuiIOBroker.h"
#include "StiMaker/MainFrame.h"
#include "StiMaker/StiOptionFrame.h"
#include "Sti/StiDefaultTrackFilter.h"
#include "MessengerOptionsDialog.h"

#include "Sti/Base/EditableParameters.h"

MainFrame* MainFrame::s_instance = 0;

Int_t mb_button_id[9] = { kMBYes, kMBNo, kMBOk, kMBApply, kMBRetry, kMBIgnore, kMBCancel, kMBClose, kMBDismiss };
EMsgBoxIcon mb_icon[4] = { kMBIconStop, kMBIconQuestion,  kMBIconExclamation, kMBIconAsterisk };
const char *filetypes[] = { "All files",     "*",
			    "ROOT files",    "*.root",
			    "ROOT macros",   "*.C",
			    0,               0 };

ClassImp(MainFrame)

MainFrame::MainFrame(const TGWindow *p, UInt_t w, UInt_t h)
  : TGMainFrame(p, w, h),
    mchain(0), mIoMaker(0)
{
  s_instance = this;
  createMenu();
  createCanvasFrame();
  createButtonFrame();
  setView(new StiSkeletonView());
  SetWindowName("STAR Integraged Tracker");
  MapSubwindows();
  Resize(GetDefaultSize());
  MapWindow();
}


void MainFrame::createMenu()
{
  TGLayoutHints * menuItemLayout = new TGLayoutHints(kLHintsTop | kLHintsLeft,0, 4, 0, 0);
  TGLayoutHints * menuBarLayout  = new TGLayoutHints(kLHintsTop | kLHintsLeft | kLHintsExpandX,0, 0, 1, 1);
  TGLayoutHints * helpItemLayout = new TGLayoutHints(kLHintsTop | kLHintsRight);

  TGMenuBar * menuBar = new TGMenuBar(this, 1, 1, kHorizontalFrame);
  createFileMenu(menuBar,menuItemLayout); 
  createOptionMenu(menuBar,menuItemLayout);
  createViewMenu(menuBar,menuItemLayout);  
  createNavigationMenu(menuBar,menuItemLayout);
  createTrackingMenu(menuBar,menuItemLayout);
  createHelpMenu(menuBar,helpItemLayout);
  AddFrame(menuBar, menuBarLayout);
}

void MainFrame::createFileMenu(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{
  TGPopupMenu * menu = new TGPopupMenu(fClient->GetRoot());
  menu->AddEntry("&Open...", M_FILE_OPEN);
  menu->AddEntry("&Save", M_FILE_SAVE);
  menu->AddEntry("S&ave as...", M_FILE_SAVEAS);
  menu->AddEntry("&Close", -1);
  menu->AddSeparator();
  menu->AddEntry("&Print", -1);
  menu->AddEntry("P&rint setup...", -1);
  menu->AddSeparator();
  menu->AddEntry("E&xit", M_FILE_EXIT);
  menu->DisableEntry(M_FILE_SAVEAS);  
  menu->Associate(this);
  menuBar->AddPopup("&File", menu, itemLayout);
}

void MainFrame::createOptionMenu(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{
  TGPopupMenu * menu = new TGPopupMenu(fClient->GetRoot());
  menu->AddEntry("Messenger Options", M_Messenger);
  menu->AddEntry("Display Options", M_DisplayOptions);
  menu->AddEntry("MC Track Colors", M_ShowRootColors);
  menu->AddEntry("Evaluable Seed Finder Options", M_SeedFinderOptions);
  menu->AddEntry("Local Seed Finder Options", M_LocalSeedFinderOptions);
  menu->AddEntry("Kalman Track Finder Options", M_TrackFinderOptions);
  menu->AddEntry("MC Track Filter Options", M_McTrackFilterOptions);
  menu->AddEntry("Rec Track Filter Options", M_TrackFilterOptions); 
  menu->Associate(this);
  menuBar->AddPopup("&Options",menu, itemLayout);
}

void MainFrame::createViewMenu(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{ 
  /*
  TGPopupMenu * menu = new TGPopupMenu(fClient->GetRoot());
  menu->AddEntry("Svt Visible", M_DetView_SvtVisible);
  menu->AddEntry("Svt Invisible", M_DetView_SvtInvisible);
  menu->Associate(this);
  
  menu = new TGPopupMenu(fClient->GetRoot());
  menu->AddEntry("Tpc Visible", M_DetView_TpcVisible);
  menu->AddEntry("Tpc Invisible", M_DetView_TpcInvisible);
  menu = new TGPopupMenu(fClient->GetRoot());
  menu->AddEntry("Ifc Visible", M_DetView_IfcVisible);
  menu->AddEntry("Ifc Invisible", M_DetView_IfcInvisible);
  menu->Associate(this);
  
  menu = new TGPopupMenu(fClient->GetRoot());
  menu->AddEntry("All Visible", M_DetView_AllVisible);
  menu->AddEntry("All Invisible", M_DetView_AllInvisible);
  menu->Associate(this);
  
  menu = new TGPopupMenu(fClient->GetRoot());
  menu->AddEntry("Manual View", M_DetView_ManualView);
  menu->AddEntry("Skeleton View", M_DetView_SkeletonView);
  menu->AddEntry("Zoom Skeleton View",M_DetView_ZoomSkeletonView);
  menu->AddSeparator();
  menu->AddPopup("&All",mAllViewMenu);
  menu->Associate(this);
  menu = new TGPopupMenu(fClient->GetRoot());
  menu->AddPopup("&Tpc",menu);
  menu->AddPopup("&Svt",menu);
  menu->AddPopup("&Ifc",menu);
  menu->Associate(this);

  menuBar->AddPopup("&File", menu, itemLayout);
  */
}

void MainFrame::createNavigationMenu(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{ 
  TGPopupMenu * menu = new TGPopupMenu(fClient->GetRoot());
  menu->AddEntry("Navigator", M_Det_Navigate);
  menu->AddSeparator();
  menu->AddEntry("Move &In",M_DetNavigate_MoveIn);
  menu->AddEntry("Move &Out",M_DetNavigate_MoveOut);
  menu->AddEntry("Move &Plus Phi",M_DetNavigate_MovePlusPhi);
  menu->AddEntry("Move &Minus Phi",M_DetNavigate_MoveMinusPhi);
  menu->AddEntry("Set Layer",M_DetNavigate_SetLayer);
  menu->AddEntry("Set Layer and Angle",M_DetNavigate_SetLayerAndAngle);
  menu->Associate(this);
  menuBar->AddPopup("&Navigation", menu, itemLayout);
}

void MainFrame::createTrackingMenu(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{  
  TGPopupMenu * menu = new TGPopupMenu(fClient->GetRoot());
  menu->AddLabel("Access to tracking functions");
  menu->AddEntry("Toggle Fit/Find",M_Tracking_ToggleFitFind);
  menu->AddSeparator();
  menu->AddEntry("Next Track Step",M_Tracking_DoTrackStep);
  menu->AddEntry("Finish Track",M_Tracking_FinishTrack);
  menu->AddEntry("Finish Event",M_Tracking_FinishEvent);
  menu->AddEntry("Event Step",M_Tracking_EventStep);
  menu->AddEntry("N-Event Step",M_Tracking_NEventStep);
  menu->Associate(this);
  menuBar->AddPopup("&Tracking", menu, itemLayout);
}

void MainFrame::createHelpMenu(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{  
  TGPopupMenu * menu = new TGPopupMenu(fClient->GetRoot());
  menu->AddEntry("&Contents",  M_HELP_CONTENTS);
  menu->AddEntry("&Search...", M_HELP_SEARCH);
  menu->AddSeparator();
  menu->AddEntry("&About",     M_HELP_ABOUT);  
  menu->Associate(this);
  menuBar->AddPopup("&Help", menu, itemLayout);
}

void MainFrame::createCanvasFrame()
{
  TRootEmbeddedCanvas * canvas = new TRootEmbeddedCanvas("Embedded Canvas", this, 650, 600);
  TileFrame * tileFrame = new TileFrame(canvas->GetViewPort());
  tileFrame->SetCanvas(canvas);
  canvas->SetContainer(tileFrame);
  StiRootDisplayManager::instance(canvas->GetCanvas());
  AddFrame(canvas, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 0, 0, 2, 2));
}

void MainFrame::createButtonFrame()
{
  TGCompositeFrame * frame = new TGCompositeFrame(this, 60, 20, kHorizontalFrame | kSunkenFrame);
  TGTextButton * button;

  button = new TGTextButton(frame,"&Next Track Step", M_Tracking_DoTrackStep);
  button->Associate(this);
  button->SetToolTipText("Perform the next step in the current track");
  frame->AddFrame(button, new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2));
  
  button = new TGTextButton(frame,"Finish &Track", M_Tracking_FinishTrack);
  button->Associate(this);
  button->SetToolTipText("Finish The Current Track");
  frame->AddFrame(button, new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2));
  
  button = new TGTextButton(frame,"Finish &Event", M_Tracking_FinishEvent);
  button->Associate(this);
  button->SetToolTipText("Finish The Current Event");
  frame->AddFrame(button, new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2));
  
  button = new TGTextButton(frame,"Reset Event", M_Tracking_ResetEvent);
  button->Associate(this);
  button->SetToolTipText("Reset The Current Event");
  frame->AddFrame(button, new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2));
    
  button = new TGTextButton(frame, "&Next Event", M_Tracking_EventStep);
  button->Associate(this);
  button->SetToolTipText("Step To Next Event");
  frame->AddFrame(button, new TGLayoutHints(kLHintsTop | kLHintsRight, 2, 0, 2, 2));

  AddFrame(frame, new TGLayoutHints(kLHintsBottom | kLHintsExpandX, 0, 0, 1, 0));
}


MainFrame::~MainFrame()
{
  cout <<"MainFrame::~MainFrame()"<<endl;
}

/// Got close message for this MainFrame. Terminate the application
/// or returns from the TApplication event loop (depending on the
/// argument specified in TApplication::Run()).
void MainFrame::CloseWindow()
{
  gApplication->Terminate(0);
}

/// Handle messages send to the MainFrame object. E.g. all menu button
/// messages.
Bool_t MainFrame::ProcessMessage(Long_t msg, Long_t parm1, Long_t)
{
  switch (GET_MSG(msg)) 
    {
    case kC_COMMAND:
      switch (GET_SUBMSG(msg)) 
	{
	case kCM_BUTTON:
	  if (parm1 == M_Tracking_DoTrackStep) doNextTrackStep();
	  else if (parm1 == M_Tracking_FinishTrack) finishTrack();
	  else if (parm1 == M_Tracking_FinishEvent) finishEvent();
	  else if (parm1 == M_Tracking_ResetEvent) 
	    {
	      setCurrentDetectorToDefault();
	      StiToolkit * toolkit = StiToolkit::instance();
	      toolkit->getTrackFinder()->reset();
	      toolkit->getDisplayManager()->reset();
	      toolkit->getDisplayManager()->draw();
	      toolkit->getDisplayManager()->update();
	      showCurrentDetector();
	    }
	  else if (parm1 == M_Tracking_EventStep) stepToNextEvent();
	  break;
	case kCM_MENUSELECT:
	  break;
	case kCM_MENU:
	    switch (parm1) 
	      {
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
	      case M_Messenger:	new MessengerOptionsDialog(fClient->GetRoot(), this, 400, 200);break;
	      case M_DisplayOptions: break;//new EntryTestDlg(fClient->GetRoot(), this); break;
	      case M_SeedFinderOptions:
		{
		  /*
		    StiLocalTrackSeedFinder * seedFinder = StiToolkit::instance()->getTrackSeedFinder();
		    if (seedFinder)
		    new StiOptionFrame(fClient->GetRoot(), this, seedFinder);
		    else
		    cout << "case M_TrackFinderOptions - null StiLocalTrackSeedFinder * seedFinder "<<endl;*/
		  break;
		}
	      case M_TrackFinderOptions:
		{
		  EditableParameters * pars = dynamic_cast<EditableParameters *>(StiToolkit::instance()->getTrackFinder()->getParameters());
		  if (pars)
		    new StiOptionFrame(fClient->GetRoot(), this, pars);
		  else
		    cout << "case M_TrackFinderOptions - null StiKalmanTrackFinderParameters * pars"<<endl;
		  break;
		}
	      case M_McTrackFilterOptions: 
		{
		  StiDefaultTrackFilter * mcFilter;
		  Filter<StiTrack> * filter = StiToolkit::instance()->getTrackFinder()->getGuiMcTrackFilter();
		  mcFilter = dynamic_cast<StiDefaultTrackFilter*>(filter);
		  if (mcFilter)
		      new StiOptionFrame(fClient->GetRoot(), this, mcFilter);
		  else
		    cout << "case M_McTrackFilterOptions: Null mcFilter." << endl;
		  break;
		}
	      case M_TrackFilterOptions: 
		{
		  StiDefaultTrackFilter * filter;
		  filter = static_cast<StiDefaultTrackFilter *>(StiToolkit::instance()->getTrackFinder()->getGuiTrackFilter());
		  new StiOptionFrame(fClient->GetRoot(), this, filter);
		  break;
		}
	      case M_DetView_AllVisible: 	 setAllVisible();  break;
	      case M_DetView_AllInvisible: setAllInvisible();break;
	      case M_DetView_TpcVisible:   setTpcVisible();    break;
	      case M_DetView_TpcInvisible: setTpcInvisible();  break;
	      case M_DetView_SvtVisible:   setSvtVisible();    break;
	      case M_DetView_SvtInvisible: setSvtInvisible();  break;
	      case M_DetView_IfcVisible:   setIfcVisible();    break;
	      case M_DetView_IfcInvisible: setIfcInvisible();  break;
	      case M_DetView_ManualView:   setManualView();    break;
	      case M_DetView_SkeletonView: setSkeletonView();  break;
	      case M_DetView_ZoomSkeletonView: setZoomSkeletonView(); break;
	      case M_DetOnOff:	 new DetectorActivator(fClient->GetRoot(), this, 800, 200); break;
	      case M_Det_Navigate: 
		new Navigator(fClient->GetRoot(), this, 400, 200); break;
	      case M_DetNavigate_MoveIn:      moveIn();  break;
	      case M_DetNavigate_MoveOut:	    moveOut(); break;
	      case M_DetNavigate_MovePlusPhi: movePlusPhi();break;
	      case M_DetNavigate_MoveMinusPhi:moveMinusPhi();break;
	      case M_DetNavigate_SetLayer:    setLayer();	break;
	      case M_DetNavigate_SetLayerAndAngle:setLayerAndAngle();break;
	      case M_Tracking_ToggleFitFind:toggleFitFind();break;
	      case M_Tracking_DoTrackStep:doNextTrackStep();break;
	      case M_Tracking_FinishTrack:finishTrack();	break;
	      case M_Tracking_FinishEvent:finishEvent();	break;
	      case M_Tracking_EventStep:	stepToNextEvent();break;
	      case M_Tracking_NEventStep:	stepThroughNEvents();break;
	      case M_FILE_SAVE:	printf("M_FILE_SAVE\n");break;
	      case M_FILE_EXIT:	CloseWindow();  break;		
	      default:      break;
	    }
	default:  break;
	}
    default:    break;
    }
  return kTRUE;
}

void MainFrame::setView(StiView* view)
{
  if (mView) delete mView;
  mView=view;
}

void MainFrame::doNextTrackStep()
{
  setCurrentDetectorToDefault();
  StiToolkit::instance()->getTrackFinder()->findNextTrackSegment();
  showCurrentDetector();
}

void MainFrame::finishTrack()
{
  // cout <<"MainFrame::finishTrack()"<<endl;
  setCurrentDetectorToDefault();
  StiToolkit::instance()->getTrackFinder()->findNextTrack();
  showCurrentDetector();
  // cout <<"\tMainFrame::finishTrack() done"<<endl;
}

void MainFrame::finishEvent()
{
  cout<<"MainFrame::finishEvent() - INFO - Started"<<endl;
  setCurrentDetectorToDefault();
  StiToolkit::instance()->getTrackFinder()->findTracks();
  showCurrentDetector();
  cout<<"MainFrame::finishEvent() - INFO - Done"<<endl;
}

void MainFrame::stepToNextEvent()
{
  cout <<"MainFrame::stepToNextEvent() - INFO - Started"<<endl;
  setCurrentDetectorToDefault();    
  if (!mchain)
    throw runtime_error("MainFrame::stepToNextEvent() - FATAL - mchain==0");
  mchain->Clear();
  mchain->Make();
  showCurrentDetector();
  cout <<"MainFrame::stepToNextEvent() - INFO - Done"<<endl;
}

void MainFrame::stepThroughNEvents()
{
  setCurrentDetectorToDefault();    
  cout <<"\nEnter number of events to process (int) "<<endl;
  int nevents;
  cin >> nevents;
  for (int i=0; i<nevents; ++i) 
    {
      mchain->Clear();
      mchain->Make();
      showCurrentDetector();
    }
}

void MainFrame::printFactorySize()
{
  //StiToolkit::instance()->printStatistics();
}

void MainFrame::setAllVisible()
{
  StiRootDisplayManager::instance()->setVisible(true);
  StiRootDisplayManager::instance()->draw();
  StiRootDisplayManager::instance()->update();
}

void MainFrame::setAllInvisible()
{
  StiRootDisplayManager::instance()->setVisible(false);
  StiRootDisplayManager::instance()->draw();
  StiRootDisplayManager::instance()->update();
}

void MainFrame::setSkeletonView()
{
    StiRootDisplayManager::instance()->setView(0);
    setView(new StiSkeletonView());
    StiRootDisplayManager::instance()->draw();
    StiRootDisplayManager::instance()->update();
}

void MainFrame::setManualView()
{
    StiRootDisplayManager::instance()->setVisible(true);
    setView(new StiManualView());
    StiRootDisplayManager::instance()->draw();
    StiRootDisplayManager::instance()->update();
}

void MainFrame::setZoomSkeletonView()
{
    StiRootDisplayManager::instance()->setView(1);
    setView(new StiZoomSkeletonView());
    StiRootDisplayManager::instance()->draw();
    StiRootDisplayManager::instance()->update();
}

void MainFrame::setSvtVisible()
{
    StiRootDisplayManager::instance()->setVisible("Svg",true);
    StiRootDisplayManager::instance()->draw();
    StiRootDisplayManager::instance()->update();
}

void MainFrame::setSvtInvisible()
{
    StiRootDisplayManager::instance()->setVisible("Svg",false);
    StiRootDisplayManager::instance()->draw();
    StiRootDisplayManager::instance()->update();
}

void MainFrame::setTpcVisible()
{
    StiRootDisplayManager::instance()->setVisible("Tpc",true);
    StiRootDisplayManager::instance()->draw();
    StiRootDisplayManager::instance()->update();
}

void MainFrame::setTpcInvisible()
{
    StiRootDisplayManager::instance()->setVisible("Tpc",false);
    StiRootDisplayManager::instance()->draw();
    StiRootDisplayManager::instance()->update();
}

void MainFrame::setIfcVisible()
{
  StiRootDisplayManager::instance()->setVisible("Ifc",true);
  StiRootDisplayManager::instance()->draw();
  StiRootDisplayManager::instance()->update();
}

void MainFrame::setIfcInvisible()
{
  StiRootDisplayManager::instance()->setVisible("Ifc",false);
  StiRootDisplayManager::instance()->draw();
  StiRootDisplayManager::instance()->update();
}

void MainFrame::printDisplayManager()
{
    StiRootDisplayManager::instance()->print();
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

    StiRootDisplayManager::instance()->cd();
}

void MainFrame::setLayer()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    setCurrentDetectorToDefault();
    cout <<"\nEnter position: (double)"<<endl;
    double position;
    cin >>position;
    cout <<"Setting to  position:\t"<<position<<endl;
    StiDetectorContainer*rdet = StiToolkit::instance()->getDetectorContainer();
    rdet->setToDetector(position);
    StiDetector* layer = **rdet;
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
    StiDetectorContainer*rdet = StiToolkit::instance()->getDetectorContainer();
    rdet->setToDetector(position, angle);
    StiDetector* layer = **rdet;
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
    StiDetectorContainer*rdet = StiToolkit::instance()->getDetectorContainer();
    cout <<"\t DetectorContainer returned: "<<rdet->moveOut()<<endl;
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
    if (!layer)
      throw runtime_error("MainFrame::printHitContainerForDetector() - FATAL - Failed to get detector");
    cout << StiToolkit::instance()->getHitContainer()->hits( layer->getPlacement()->getCenterRefAngle(),
							     layer->getPlacement()->getCenterRadius() )
	 <<endl;
}

void MainFrame::showCurrentDetector()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    StiRootDrawableDetector* layer = dynamic_cast<StiRootDrawableDetector*>(*rdet);
    
    if (!layer) 
      throw runtime_error("MainFrame::showCurrentDetector() - FATAL - Failed to get drawable detector");
    layer->setVisibility(true);
    layer->setColor(2);
    cout<<"MainFrame::showCurrentDetector() - Layer:"<<*layer<<endl;
    StiRootDisplayManager::instance()->draw();
    StiRootDisplayManager::instance()->update();
}

void MainFrame::memoryInfo()
{
  StMemoryInfo::instance()->snapshot();
  StMemoryInfo::instance()->print();
}

void MainFrame::setCurrentDetectorToDefault()
{
  //cout <<"MainFrame::setCurrentDetectorToDefault()"<<endl;
  if (!mView) 
    throw runtime_error("MainFrame::setCurrentDetectorToDefault() - FATAL -  mView not defined.  abort");
  mView->setToDefault();
}


void MainFrame::printHits()
{
  cout <<*(StiToolkit::instance()->getHitContainer())<<endl;
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
  cout <<StiToolkit::instance()->getHitContainer()->vertices()<<endl;
}


