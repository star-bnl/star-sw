// Private EVP specific includes
# include "ServerGui.h"

#include "DGHelp.h"

#include "HistoHandler.h"
#include "EvpUtil.h"

//#ifdef STANDALONE
//#ifndef EVENTLOOP_H
#include "EvpServer.h"
//#endif
//#endif





const char *filetypes[] =
  {
    "New",     "8038*",
    "All files",     "*",
    "DAQ files",     "*.daq",
    "ROOT files",    "*.root",
    "PS   files",    "*.ps",
    0,               0
  };

TGString *mHistoOutputFile;


ClassImp(ServerGui);

ServerGui::ServerGui(const TGWindow *p, UInt_t w, UInt_t h, EvpServer* server) : mEvpServer(server) {
  gClient->GetColorByName("blue", blue);
  gClient->GetColorByName("green", green);
  gClient->GetColorByName("grey", grey);
  gClient->GetColorByName("red", red);
  gClient->GetColorByName("black", black);

  // Some preparations here
  // Here is starting directory name

  SetDebugLevel(0);
  // Create test main frame. A TGMainFrame is a top level window.

  fCleanUp = new TList;


  fMain   = new TGMainFrame(p, w, h);
  fMain->Connect("CloseWindow()", "ServerGui", this, "CloseWindow()");

  fMain1 = new TGCompositeFrame(fMain,400,220,kHorizontalFrame);
  fCleanUp->Add(fMain1);

  // Define Layouts for various widgets

  fL1 = new TGLayoutHints(kLHintsTop | kLHintsExpandX | kLHintsCenterX,5,5,5,0);
  fCleanUp->Add(fL1);
  fL2 = new TGLayoutHints(kLHintsTop | kLHintsExpandX, 0, 0, 1, 0);
  fCleanUp->Add(fL2);
  fL3 = new TGLayoutHints(kLHintsTop | kLHintsLeft | kLHintsExpandY, 0, 0, 1, 0);
  fCleanUp->Add(fL3);


  fL5 = new TGLayoutHints(kLHintsLeft | kLHintsCenterY, 3,5,0,0);
  fCleanUp->Add(fL5);
  fL6 = new TGLayoutHints(kLHintsRight | kLHintsCenterY, 0,2,0,0);
  fCleanUp->Add(fL6);


  fL101 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 0, 4, 0, 0);
  fCleanUp->Add(fL101);

  fL102 = new TGLayoutHints(kLHintsTop | kLHintsRight);
  fCleanUp->Add(fL102);

  fL200 = new TGLayoutHints(kLHintsLeft | kLHintsTop, 5, 5, 5, 5);
  fCleanUp->Add(fL200);

  fL201 = new TGLayoutHints(kLHintsLeft | kLHintsTop | kLHintsLeft, 5, 5, 2, 5);
  fCleanUp->Add(fL201);

  fL203 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 0, 0, 5, 5);
  fCleanUp->Add(fL203);

  fL204 = new TGLayoutHints(kLHintsBottom | kLHintsCenterX, 0, 0, 0, 0);
  fCleanUp->Add(fL204);

  //  fLStatusLayout = new TGLayoutHints(kLHintsTop | kLHintsExpandX | kLHintsCenterX, 5,5,5,0);
  //  fCleanUp->Add(fLStatusLayout);

  fLDir = new TGLayoutHints(kLHintsLeft|kLHintsTop,2,2,5,2);
  fCleanUp->Add(fLDir);


  // Create menubar and popup menus. The hint objects are used to place
  // and group the different menu widgets with respect to eachother.


  fMenuBar = new TGMenuBar(fMain, 1, 1, kHorizontalFrame);
  fCleanUp->Add(fMenuBar);

  fMain->AddFrame(fMenuBar, fL1);
  fMain->AddFrame(fMain1,  fL1);
  //fMain->AddFrame(fMain2,  fL1);

  // File Menu

  fMenuFile = new TGPopupMenu(gClient->GetRoot());
  fCleanUp->Add(fMenuFile);
  fMenuFile->AddEntry("&Open...", M_FILE_OPEN);
  fMenuFile->AddEntry("&Save histos", M_FILE_SAVE);
  fMenuFile->AddEntry("S&ave as...", M_FILE_SAVEAS);
  fMenuFile->AddEntry("&Close", M_FILE_CLOSE);
  fMenuFile->AddSeparator();
  fMenuFile->AddEntry("&Print", M_FILE_PRINT);
  fMenuFile->AddEntry("P&rint setup...", M_FILE_PRINTSETUP);
  fMenuFile->AddSeparator();
  fMenuFile->AddEntry("E&xit", M_FILE_EXIT);
  fMenuFile->DisableEntry(M_FILE_SAVEAS);
  fMenuFile->DisableEntry(M_FILE_PRINT);
  fMenuFile->DisableEntry(M_FILE_PRINTSETUP);
  fMenuFile->DisableEntry(M_FILE_CLOSE);

  // Help Menu

  fMenuHelp = new TGPopupMenu(gClient->GetRoot());
  fCleanUp->Add(fMenuHelp);
  //fMenuHelp->AddEntry("&Contents", M_HELP_CONTENTS);
  //fMenuHelp->AddEntry("&Search...", M_HELP_SEARCH);
  //fMenuHelp->AddSeparator();
  fMenuHelp->AddEntry("&About", M_HELP_ABOUT);

  // Menu button messages are handled by the main frame (i.e. "this")
  // ProcessMessage() method.
  fMenuFile->Connect("Activated(Int_t)", "ServerGui", this,
		     "HandleMenu(Int_t)");
  fMenuHelp->Connect("Activated(Int_t)", "ServerGui", this,
		     "HandleMenu(Int_t)");

  // Finish with File-Help (Upper frame). Add it up.

  fMenuBar->AddPopup("&File", fMenuFile, fL101);
  fMenuBar->AddPopup("&Help", fMenuHelp, fL102);

  // Frame with connection controls

  fConnectionFrame = new TGVerticalFrame(fMain1, 64,300,kSunkenFrame);
  fCleanUp->Add(fConnectionFrame);


  fControlFrame = new TGVerticalFrame(fMain1,0,0,kSunkenFrame);
  fCleanUp->Add(fControlFrame);

  fMain1->AddFrame(fConnectionFrame, fL3);
  fMain1->AddFrame(fControlFrame, fL3);

  // Create status frame containing a label and a text entry widget
  fRunFrame = new TGHorizontalFrame(fControlFrame, 0, 0, kSunkenFrame);
  fCleanUp->Add(fRunFrame);

  fHistoFrame = new TGHorizontalFrame(fControlFrame, 0, 0, kSunkenFrame);
  fCleanUp->Add(fHistoFrame);

  fMessagesFrame = new TGHorizontalFrame(fControlFrame, 0, 0, kSunkenFrame);
  fCleanUp->Add(fMessagesFrame);


  fControlFrame->AddFrame(fRunFrame, fL2);
  fControlFrame->AddFrame(fHistoFrame, fL2);
  fControlFrame->AddFrame(fMessagesFrame, fL2);


  //-----------------------------------------------

  // Connection Frame

  fDummyConnectionFrame = new TGGroupFrame(fConnectionFrame, "Connect");
  fCleanUp->Add(fDummyConnectionFrame);
  fConnectionFrame->AddFrame(fDummyConnectionFrame,fL200);

  fLiveButton = new TGTextButton(fDummyConnectionFrame, "&LIVE ", 163);
  fCleanUp->Add(fLiveButton);
  fLiveButton->Connect("Clicked()", "ServerGui", this, "DoLiveButton()");
  fLiveButton->SetToolTipText("Connect to current run");
  fDummyConnectionFrame->AddFrame(fLiveButton, fL203);

  fFileButton = new TGTextButton(fDummyConnectionFrame, "&FILE ", 164);
  fCleanUp->Add(fFileButton);
  fFileButton->Connect("Clicked()", "ServerGui", this, "DoFileButton()");
  fFileButton->SetToolTipText("Open datafile or directory");
  fDummyConnectionFrame->AddFrame(fFileButton, fL203);

  fStarLogo = new TGPictureButton(fConnectionFrame,gClient->GetPicture("starlogo_1.xpm"),165);
  fCleanUp->Add(fStarLogo);
  fStarLogo->SetToolTipText("Experiment shutdown. Don't push this button!");
  fConnectionFrame->AddFrame(fStarLogo, fL204);

  //   Run Frame

  fRunLoopFrame = new TGVerticalFrame(fRunFrame, 0, 0, 0);
  fCleanUp->Add(fRunLoopFrame);

  fRunInfoFrame = new TGVerticalFrame(fRunFrame, 0, 0, 0);
  fCleanUp->Add(fRunInfoFrame);


  fRunFrame->AddFrame(fRunLoopFrame, fL201);
  fRunFrame->AddFrame(fRunInfoFrame, fL201);

  //----------------------------------------------------------------
  //  fStatusLabel = new TGLabel(fRunFrame, new TGString("Event Loop"));
  //  fCleanUp->Add(fStatusLabel);
  //  fRunFrame->AddFrame(fStatusLabel,fLStatusLayout);
  //----------------------------------------------------------------
  // Information about run

  fDummyRunInfoFrame = new TGGroupFrame(fRunInfoFrame, "Event Info");
  fCleanUp->Add(fDummyRunInfoFrame);
  fRunInfoFrame->AddFrame(fDummyRunInfoFrame,fL200);


  fRunIDFrame = new TGCompositeFrame(fDummyRunInfoFrame, 60, 20, kHorizontalFrame);
  fCleanUp->Add(fRunIDFrame);
  fRunLabel = new TGLabel(fRunIDFrame, new TGString("Run Number"));
  fCleanUp->Add(fRunLabel);

  mRun = new TGTextBuffer(32);
  fRun  = new TGTextEntry(fRunIDFrame,mRun);
  fCleanUp->Add(fRun);
  mRun->AddText(0,"Not Known");

  fRun->Resize(75,fRun->GetDefaultHeight());

  fRunIDFrame->AddFrame(fRunLabel, fL5);
  fRunIDFrame->AddFrame(fRun, fL6);

  fDummyRunInfoFrame->AddFrame(fRunIDFrame,fL2);
  //----------------------------------------------------------------------

  fEventCountFrame = new TGCompositeFrame(fDummyRunInfoFrame, 60, 20, kHorizontalFrame);
  fCleanUp->Add(fEventCountFrame);
  fEventCountLabel = new TGLabel(fEventCountFrame, new TGString("Event Counter"));
  fCleanUp->Add(fEventCountLabel);

  fEventCount  = new TGTextEntry(fEventCountFrame, mEventCount = new TGTextBuffer(32));
  fCleanUp->Add(fEventCount);
  mEventCount->AddText(0,"Not Known");

  fEventCount->Resize(75,fEventCount->GetDefaultHeight());

  fEventCountFrame->AddFrame(fEventCountLabel, fL5);
  fEventCountFrame->AddFrame(fEventCount, fL6);

  fDummyRunInfoFrame->AddFrame(fEventCountFrame,fL2);

  //----------------------------------------------------------------------

  fEventNumberFrame = new TGCompositeFrame(fDummyRunInfoFrame, 60, 20, kHorizontalFrame);
  fCleanUp->Add(fEventNumberFrame);
  fEventNumberLabel = new TGLabel(fEventNumberFrame, new TGString("Event Number"));
  fCleanUp->Add(fEventNumberLabel);

  fEventNumber  = new TGTextEntry(fEventNumberFrame, mEventNumber = new TGTextBuffer(32));
  fCleanUp->Add(fEventNumber);
  mEventNumber->AddText(0,"Not Known");

  fEventNumber->Resize(75,fEventNumber->GetDefaultHeight());

  fEventNumberFrame->AddFrame(fEventNumberLabel, fL5);
  fEventNumberFrame->AddFrame(fEventNumber, fL6);

  fDummyRunInfoFrame->AddFrame(fEventNumberFrame,fL2);

  //----------------------------------------------------------------------

  fTokenNumberFrame = new TGCompositeFrame(fDummyRunInfoFrame, 60, 20, kHorizontalFrame);
  fCleanUp->Add(fTokenNumberFrame);
  fTokenNumberLabel = new TGLabel(fTokenNumberFrame, new TGString("Token Number"));
  fCleanUp->Add(fTokenNumberLabel);

  fTokenNumber  = new TGTextEntry(fTokenNumberFrame, mTokenNumber = new TGTextBuffer(32));
  fCleanUp->Add(fTokenNumber);
  mTokenNumber->AddText(0,"Not Known");

  fTokenNumber->Resize(75,fTokenNumber->GetDefaultHeight());

  fTokenNumberFrame->AddFrame(fTokenNumberLabel, fL5);
  fTokenNumberFrame->AddFrame(fTokenNumber, fL6);

  fDummyRunInfoFrame->AddFrame(fTokenNumberFrame,fL2);


  //----------------------------------------------------------------------
  fDummyRunLoopFrame = new TGGroupFrame(fRunLoopFrame, "Event Loop Controls");
  fCleanUp->Add(fDummyRunLoopFrame);
  fRunLoopFrame->AddFrame(fDummyRunLoopFrame,fL200);



  // Source name here

  //------------------------------------------------------------------
  fSourceFileFrame = new TGCompositeFrame(fDummyRunLoopFrame, 60, 20, kHorizontalFrame);
  fSourceFileLabel = new TGLabel(fSourceFileFrame, new TGString("Source:"));
  fCleanUp->Add(fSourceFileLabel);
  fSourceFile  = new TGTextEntry(fSourceFileFrame, mSourceFileName = new TGTextBuffer(128));
  fCleanUp->Add(fSourceFile);

#ifdef STANDALONE

#endif

  fSourceFile->Resize(240,fSourceFile->GetDefaultHeight());

  fSourceFileFrame->AddFrame(fSourceFileLabel, fL5);
  fSourceFileFrame->AddFrame(fSourceFile, fL6);

  fDummyRunLoopFrame->AddFrame(fSourceFileFrame,fL2);
  //-----------------------------------------------------------------
  //------------------------------------------------------------------
  fRunStatusFrame = new TGCompositeFrame(fDummyRunLoopFrame, 60, 20, kHorizontalFrame);
  fRunStatusLabel = new TGLabel(fRunStatusFrame, new TGString("Status: "));
  fCleanUp->Add(fRunStatusLabel);
  fRunStatus  = new TGTextEntry(fRunStatusFrame, mRunStatusName = new TGTextBuffer(128));
  fCleanUp->Add(fRunStatus);

  mRunStatusName->AddText(0,"Ready");

  fRunStatus->Resize(120,fRunStatus->GetDefaultHeight());

  fRunStatusFrame->AddFrame(fRunStatusLabel, fL5);
  fRunStatusFrame->AddFrame(fRunStatus, fL5);

  fDummyRunLoopFrame->AddFrame(fRunStatusFrame,fL2);

  //  //--------------------------------------------------------
  //    // Frame for Buttons

  fButtonFrame = new TGHorizontalFrame(fDummyRunLoopFrame, 300, 20, kSunkenFrame);
  fCleanUp->Add(fButtonFrame);

  fButtonFrameLayout = new TGLayoutHints(kLHintsBottom | kLHintsLeft | kLHintsExpandY | kLHintsExpandX, 2, 2, 5, 1);
  fCleanUp->Add(fButtonFrameLayout);

  fStartButton = new TGTextButton(fButtonFrame, "&START", 160);
  fCleanUp->Add(fStartButton);
  fStartButton->Connect("Clicked()", "ServerGui", this, "DoStartButton()");
  fStartButton->SetToolTipText("Start Event Loop");
  //
  fStopButton = new TGTextButton(fButtonFrame, "&STOP", 161);
  fCleanUp->Add(fStopButton);
  fStopButton->Connect("Clicked()", "ServerGui", this, "DoStopButton()");
  fStopButton->SetToolTipText("Stop Event Loop");

  fNextButton = new TGTextButton(fButtonFrame, "&NEXT EVENT", 162);
  fCleanUp->Add(fNextButton);
  fNextButton->Connect("Clicked()", "ServerGui", this, "DoNextButton()");
  fNextButton->SetToolTipText("Step through events one at time");


  //  fLiveButton = new TGTextButton(fButtonFrame, "&LIVE ", 163);
  //  fCleanUp->Add(fLiveButton);
  //  fLiveButton->Connect("Clicked()", "ServerGui", this, "DoLiveButton()");
  //  fLiveButton->SetToolTipText("Connect to current run");

  //   ULong_t green;
  //gClient->GetColorByName("green", green);
  //fLiveButton->ChangeBackground(green);

  fButtonFrame->AddFrame(fStartButton, fL203);
  fButtonFrame->AddFrame(fStopButton, fL203);
  fButtonFrame->AddFrame(fNextButton, fL203);
  //fButtonFrame->AddFrame(fLiveButton, fL203);
  //fButtonFrame->AddFrame(fTestButton, fL203);


  fDummyRunLoopFrame->AddFrame(fButtonFrame,fButtonFrameLayout);

  //--------------------------------------------------------------

  //   Histo Frame


  fHistoControlFrame = new TGVerticalFrame(fHistoFrame, 0, 0, 0);
  fCleanUp->Add(fHistoControlFrame);

  fHistoInfoFrame    = new TGVerticalFrame(fHistoFrame, 0, 0, 0);
  fCleanUp->Add(fHistoInfoFrame);

  fHistoFrame->AddFrame(fHistoControlFrame, fL201);
  fHistoFrame->AddFrame(fHistoInfoFrame, fL201);


  //  fHistoStatusLabel = new TGLabel(fHistoControlFrame, new TGString("Histograms"));
  //  fCleanUp->Add(fHistoStatusLabel);
  //  fHistoControlFrame->AddFrame(fHistoStatusLabel,fLStatusLayout);

  fDummyHistoFrame = new TGGroupFrame(fHistoControlFrame, "Histograms");
  fCleanUp->Add(fDummyHistoFrame);

  fHistoControlFrame->AddFrame(fDummyHistoFrame,fL200);


  //--------------------------------------------------
  fHistoFileFrame = new TGCompositeFrame(fDummyHistoFrame, 60, 20, kHorizontalFrame);
  fHistoFileLabel = new TGLabel(fHistoFileFrame, new TGString("Save as:"));
  fCleanUp->Add(fHistoFileLabel);

  fHistoFile  = new TGTextEntry(fHistoFileFrame, mHistoOutFileName = new TGTextBuffer(128));
  fCleanUp->Add(fHistoFile);
#ifdef STANDALONE

  //mHistoOutFileName->AddText(0, mEvpServer->mHistoHandler->GetHistoRootFile());
#endif

  fHistoFile->Resize(240,fHistoFile->GetDefaultHeight());

  fHistoFileFrame->AddFrame(fHistoFileLabel, fL5);
  fHistoFileFrame->AddFrame(fHistoFile, fL5);

  fDummyHistoFrame->AddFrame(fHistoFileFrame,fL2);

  //  //--------------------------------------------------------
  //    // Frame for Histo related buttons

  fHistoButtonFrame = new TGHorizontalFrame(fDummyHistoFrame, 300, 20, kSunkenFrame);
  fCleanUp->Add(fHistoButtonFrame);

  fHistoResetButton = new TGTextButton(fHistoButtonFrame, "&Reset", 170);
  fCleanUp->Add(fHistoResetButton);
  fHistoResetButton->Connect("Clicked()", "ServerGui", this, "DoHistoResetButton()");
  fHistoResetButton->SetToolTipText("Reset Histograms");


  fHistoSaveButton = new TGTextButton(fHistoButtonFrame, "&Save Root", 171);
  fCleanUp->Add(fHistoSaveButton);
  fHistoSaveButton->Connect("Clicked()", "ServerGui", this, "DoHistoSaveButton()");
  fHistoSaveButton->SetToolTipText("Save Histograms as a Root File");

  //  fHistoSavePSButton = new TGTextButton(fHistoButtonFrame, "&Save PS", 172);
  //  fCleanUp->Add(fHistoSavePSButton);
  //  fHistoSavePSButton->Connect("Clicked()", "ServerGui", this, "DoHistoSavePSButton()");
  //  fHistoSavePSButton->SetToolTipText("Save Histograms as a PostScript File. Not implemented yet!");


  //  fHistoPrintButton = new TGTextButton(fHistoButtonFrame, "&Print Histograms", 173);
  //  fCleanUp->Add(fHistoPrintButton);
  //  fHistoPrintButton->Connect("Clicked()", "ServerGui", this, "DoHistoPrintButton()");
  //  fHistoPrintButton->SetToolTipText("Print Histograms. Not implemented yet!");

  //  fHistoViewButton = new TGTextButton(fHistoButtonFrame, "&Start Viewer", 174);
  //  fCleanUp->Add(fHistoViewButton);
  //  fHistoViewButton->Connect("Clicked()", "ServerGui", this, "DoHistoViewButton()");
  //  fHistoViewButton->SetToolTipText("Print Histograms. Not implemented yet!");



  fHistoButtonFrame->AddFrame(fHistoResetButton, fL203);
  fHistoButtonFrame->AddFrame(fHistoSaveButton, fL203);
  //fHistoButtonFrame->AddFrame(fHistoSavePSButton, fL203);
  //fHistoButtonFrame->AddFrame(fHistoPrintButton, fL203);
  //fHistoButtonFrame->AddFrame(fHistoViewButton, fL203);

  fDummyHistoFrame->AddFrame(fHistoButtonFrame,fButtonFrameLayout);

  //--------------------------------------------------------------

  fMain->SetWindowName("STAR EVP Server");

  fMain->MapSubwindows();

  // we need to use GetDefault...() to initialize the layout algorithm...
  fMain->Resize(fMain->GetDefaultSize());
  //Resize(400, 200);

  fMain->MapWindow();

}


void ServerGui::SetTarget(const char* name) {
  mSourceFileName->Clear();
  mSourceFileName->AddText(0,name);
  fSourceFile->MapSubwindows();
  fSourceFile->Layout();
  gClient->NeedRedraw(fSourceFile);
  //gSystem->ChangeDirectory(name);
}

ServerGui::~ServerGui()
{
  // Delete all created widgets.
  fCleanUp->Delete();
  delete fCleanUp;

  //   delete mEvpServer;
  delete fMain;
}

void ServerGui::CloseWindow()
{

  gApplication->Terminate(0);
}

void ServerGui::DoStartButton()
{
  // Handle button click.

  if(mDebugLevel) {
    cout<<"Start Button Pressed"<<endl;
  }
  // Change run status message

  ShowStatus("Connecting...");
  mEvpServer->SetGoFlag();
}
void ServerGui::DoStopButton()
{
  // Handle button click.

  if(mDebugLevel) {
    cout<<"Stop Button Pressed"<<endl;
  }

  mEvpServer->SetStopFlag();
  //
  // Change run status message
  //

  ShowStatus("Stopped");
}

//--------------------------------
void ServerGui::DoNextButton(){
  if(mDebugLevel) {
    cout<<"Next Button Pressed"<<endl;
  }
  ShowStatus("Step Mode");
  ShowNext(true);
  mEvpServer->NextEvent();
  ShowNext(false);
}
//--------------------------------
void ServerGui::DoLiveButton() {
  if(mDebugLevel) {
    cout<<"Live Button Pressed"<<endl;
  }
  ShowLiveRun(true);
  mEvpServer->SetStopFlag();
  mEvpServer->SetLive();
}
//--------------------------------
void ServerGui::DoFileButton()
{
  // Handle button click.

  if(mDebugLevel) {
    cout<<"File Button Pressed"<<endl;
  }

  mEvpServer->SetStopFlag();
  ShowLiveRun(false);


  static TGFileInfo* fi = new TGFileInfo();
  fi->fIniDir = "/a";
  fi->fFileTypes = (const char **)filetypes;
  //At this point target is set

    
  if(mDebugLevel) {
    cout<<"Current Directory: "<<gSystem->WorkingDirectory()<<endl;
  }

  TGFileDialog* dialog = new TGFileDialog(gClient->GetRoot(), fMain, kFDOpen,fi);

  if(mDebugLevel) {
    cout <<fi->fFilename<<endl;
  }

  if ( !fi->fFilename ) {  
    cout << " no filename " << endl;
    cout << " no filename " << endl;
    cout << " no filename " << endl;
    cout << " no filename " << endl;
    cout << " no filename " << endl;
  }
  
  if(fi->fFilename) {
    mEvpServer->SetNewTarget(fi->fFilename);
    SetTarget(fi->fFilename);
  }
}

void ServerGui::DoHistoResetButton()
{
  // Handle button click.

  if(mDebugLevel) {
    cout<<"HistoReset Button Pressed"<<endl;
  }
#ifdef STANDALONE
  mEvpServer->Reset();

#endif
}
void ServerGui::DoHistoSaveButton()
{
  if(mDebugLevel) {
    cout<<"HistoSave Button Pressed"<<endl;
  }
  char filename[1014];
  sprintf(filename,"%s",mHistoOutFileName->GetString());
  mEvpServer->Save(filename);
}
void ServerGui::DoHistoSavePSButton()
{
  // Handle button click.

  if(mDebugLevel) {
    cout<<"HistoSavePS Button Pressed"<<endl;
  }
#ifdef STANDALONE

  mEvpServer->MakePS();
#endif
}
void ServerGui::DoHistoPrintButton()
{
  // Handle button click.

  if(mDebugLevel) {
    cout<<"HistoPrint Button Pressed"<<endl;
  }
#ifdef STANDALONE
  mEvpServer->Print();
#endif
}
void ServerGui::DoHistoViewButton()
{
  // Handle button click.

  if(mDebugLevel) {
    cout<<"HistoView Button Pressed"<<endl;
  }
  //    int pid;
  //    if((pid = fork()) < 0){
  //      cout<<"fork error"<<endl;
  //    }

  //  gSystem->Exec("/home/panitkin/online/may/test1/batch.csh &");

  //system("/home/panitkin/online/may/test1/batch.csh &");

}



void ServerGui::HandleMenu(Int_t id)
{

  TGFileInfo hfi;
  // Handle menu items.

  switch (id)
    {

    case M_FILE_OPEN:
      {


	DoFileButton();

	//              TGFileInfo fi;
	//              fi.fFileTypes = (char **)filetypes;
	//  	    //At this point target is set

	//  #ifdef STANDALONE
	//              fi.fFilename = mEvpServer->GetTarget();
	//  #endif
	//              new TGFileDialog(gClient->GetRoot(), fMain, kFDOpen,&fi);
	//  	    if(mDebugLevel)cout <<fi.fFilename<<endl;

	//  	    if(fi.fFilename){
	//  #ifdef STANDALONE
	//  	      mEvpServer->SetNewTarget(fi.fFilename);
	//  #endif
	//  	    if(mDebugLevel)cout <<fi.fIniDir<<endl;
	//  	    }
	//  if(mDebugLevel)cout<<"Current Directory: "<<gSystem->WorkingDirectory()<<endl;

	//  	    mSourceFileName->Clear();
	//  #ifdef STANDALONE
	//  	    mSourceFileName->AddText(0,mEvpServer->GetTarget());
	//  #endif
	//  	    fSourceFile->MapSubwindows();
	//  	    fSourceFile->Layout();
	//  	    gClient->NeedRedraw(fSourceFile);
      }
      break;

    case M_FILE_SAVE:
      if(mDebugLevel)
	printf("M_FILE_SAVE\n");
      // Get file name from user


      hfi.fFileTypes = (const char **)filetypes;
      //At this point target is set

      if(mDebugLevel) {
	cout <<"Set file types"<<endl;
      }

      // Set a default path here
      //gSystem->ChangeDirectory( EvpUtil::GetOutputPath() );

      if(mDebugLevel) {
	cout<<"Current Directory: "<<gSystem->WorkingDirectory()<<endl;
      }

      new TGFileDialog(gClient->GetRoot(), fMain, kFDSave,&hfi);
      if(mDebugLevel) {
	cout <<hfi.fFilename<<endl;
	  cout <<hfi.fIniDir<<endl;
      }
      if(hfi.fFilename) {
	// At this point file name is known, so save files.
	mEvpServer->Save(hfi.fFilename);
	// Redraw root file name in GUI
	UpdateRootFileInfo(hfi.fFilename);
      }
      if(mDebugLevel) {
	cout<<"Current Directory: "<<gSystem->WorkingDirectory()<<endl;
      }
      break;

    case M_FILE_EXIT:
      CloseWindow();   // terminate theApp no need to use SendCloseMessage()
      break;

    case M_HELP_ABOUT:
      new DGHelp( strcat( getenv("ONLINEPLOTSDIR"),"/Infrastructure/messages/duplicate_copy.message"));
      break;

      //        case M_TEST_MSGBOX:
      //           new TestMsgBox(gClient->GetRoot(), fMain, 400, 200);
      //           break;

    default:
      if(mDebugLevel)
	printf("Menu item %d selected\n", id);
      break;
    }
}

void ServerGui::SetDebugLevel(int lDebugLevel)
{

  mDebugLevel = lDebugLevel;
}
int ServerGui::GetDebugLevel(void)
{

  return mDebugLevel;
}
void ServerGui::UpdateRunInfo(int run)
{

  char tmp[32];

  sprintf(tmp,"%d",run);
  if(mDebugLevel) {
    cout<<"Update run info "<<tmp<<endl;
  }

  mRun->Clear();
  mRun->AddText(0,tmp);

  fRun->MapSubwindows();
  fRun->Layout();
  gClient->NeedRedraw(fRun);
}
void ServerGui::UpdateEventCountInfo(int evtCount)
{


  char tmp[32];

  sprintf(tmp,"%d",evtCount);


  mEventCount->Clear();
  mEventCount->AddText(0,tmp);

  fEventCount->MapSubwindows();
  fEventCount->Layout();
  gClient->NeedRedraw(fEventCount);

}
void ServerGui::UpdateEventNumberInfo(int evtNumber)
{


  char tmp[32];

  sprintf(tmp,"%d",evtNumber);


  mEventNumber->Clear();
  mEventNumber->AddText(0,tmp);

  fEventNumber->MapSubwindows();
  fEventNumber->Layout();
  gClient->NeedRedraw(fEventNumber);

}
void ServerGui::UpdateTokenNumberInfo(int tokenNumber)
{


  char tmp[32];

  sprintf(tmp,"%d",tokenNumber);


  mTokenNumber->Clear();
  mTokenNumber->AddText(0,tmp);

  fTokenNumber->MapSubwindows();
  fTokenNumber->Layout();
  gClient->NeedRedraw(fTokenNumber);

}
void ServerGui::UpdateRootFileInfo(char *lfile)
{
  // Gets  file name for histograms from HistoHandler and prints it in GUI

  mHistoOutFileName->Clear();
  mHistoOutFileName->AddText(0,lfile);

  fHistoFile->MapSubwindows();
  fHistoFile->Layout();
  gClient->NeedRedraw(fHistoFile);
  if(mDebugLevel) {
    cout << "Updating" << lfile <<endl;
  }
}
//---------------------------------------------------
void ServerGui::ShowButton(TGButton* button, Pixel_t color ) {
  if ( button->GetBackground() != color ) {
    button->ChangeBackground(color);
    gClient->NeedRedraw(button);
  }
}
//--------------------------------------------------
void ServerGui::ShowRunning(bool run) {
  if ( run ) {
    ShowButton(fStartButton, green);
    ShowButton(fStopButton, grey);
  } else {
    ShowButton(fStartButton, grey);
    ShowButton(fStopButton, red);
  }
}
//--------------------------------------------------
void ServerGui::ShowNext(bool run) {
  if ( run ) {
    ShowButton(fNextButton, green);
  } else { 
    ShowButton(fNextButton, grey);  
  }
}
//--------------------------------------------------
void ServerGui::ShowLiveRun(bool run) {
  if ( run ) {
    ShowButton(fLiveButton, green);
    ShowButton(fFileButton, grey);
  } else {
    ShowButton(fLiveButton, grey);
    ShowButton(fFileButton, green);
  }
}
//--------------------------------------------------
void ServerGui::ShowFileRun(bool run) {
  ShowLiveRun( !run );
}
//--------------------------------------------------
void ServerGui::ShowStatus(char *lstatus) {

  mRunStatusName->Clear();
  mRunStatusName->AddText(0,lstatus);

  fRunStatus->MapSubwindows();
  fRunStatus->Layout();
  gClient->NeedRedraw(fRunStatus);

}
//--------------------------------------------------
void ServerGui::SetEnabled(bool b) {
  fFileButton->SetEnabled(b);
  fStartButton->SetEnabled(b);
  fStopButton->SetEnabled(b);
  fLiveButton->SetEnabled(b);
  fNextButton->SetEnabled(b);
  fHistoResetButton->SetEnabled(b);
  fHistoSaveButton->SetEnabled(b);
  for ( int i=M_FILE_OPEN; i<=M_FILE_EXIT; i++) {
    if (!b) fMenuFile->DisableEntry(i);
    if (b) fMenuFile->EnableEntry(i);
  }
  //fMain->CloseWindow();
}








/***************************************************************************
 *
 * $Id: ServerGui.cxx,v 1.1 2009/01/23 16:11:07 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: ServerGui.cxx,v $
 * Revision 1.1  2009/01/23 16:11:07  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.2  2007/05/24 13:15:18  jml
 * blah
 *
 * Revision 1.1  2007/02/27 15:23:39  laue
 * Initial version
 *
 * Revision 1.2  2006/10/27 17:43:21  laue
 * Resources folder added
 * histogram controll class OTH added
 *
 * Revision 1.1  2006/10/04 20:31:16  laue
 * Initial Version
 *
 *
 ***************************************************************************/

