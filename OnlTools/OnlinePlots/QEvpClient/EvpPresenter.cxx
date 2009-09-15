///  (TH1 *)mfile->Get(myNames[i][j][0].Data(), h);
///  (TH1 *)mfile->Get(myNames[tab][subTab][i].Data());

#include <TLine.h>
#include <TText.h>

#include "EvpPresenter.h"
#include "EndOfRunAction.h"
#include "GroupCollection.h"

#include "qapplication.h"
#include "qwidget.h"
#include "qcursor.h"

#ifndef PR
# define PR(x) { cout << __PRETTY_FUNCTION__ << " ### " << #x << " ### " << x << endl;}
#endif

//------------------------------------------------------------------------
EvpPresenter::EvpPresenter() : mfile(0), mLastDrawnCanvas(0) {
  sprintf(mMapFile,"%s",EvpUtil::mMapFilePath);
  mRS = new RunStatus();
  mSS = new ServerStatus();
  Connect();
  // Some preparations here
  // Here is starting directory name
  SetDefaults();

}
//------------------------------------------------------------------------
EvpPresenter::EvpPresenter(const char* file) :  mfile(0), mLastDrawnCanvas(0) {
  sprintf(mMapFile,"%s",EvpUtil::mMapFilePath);
  mRS = new RunStatus();
  mSS = new ServerStatus();
  // Some preparations here
  // Here is starting directory name
  SetDefaults();
  SetSource(file);
}

//----------------------------------------------------------------
EvpPresenter::~EvpPresenter() {
}
//----------------------------------------------------------------
void EvpPresenter::SetDefaults()
{

  // Set printout level. 0 - silent, 1 - Verbose
  SetDebugLevel(0);
}

//--------------------------------------------------------------
void EvpPresenter::SetSource(const char* file)  {
  if ( mDebugLevel) cout << __PRETTY_FUNCTION__ << endl; 
  emit setEnabled(false);
  Disconnect();
  sprintf(mMapFile,"%s",file);
  Connect();
  emit setEnabled(true);
}
//--------------------------------------------------------------
void EvpPresenter::Disconnect() {
  cout << __PRETTY_FUNCTION__ << endl;

  if(mfile) {
    if(mfile->mapFile()) {
      TMapFile* mapfile = (TMapFile* ) mfile->file();
      mapfile->RemoveAll();
      mapfile->Close();
    }
    mGroups.remove();
  
    delete mfile;
    
    mfile = 0;
    *mRS  = RunStatus();
  }
  cout << __PRETTY_FUNCTION__ << endl;
}
//--------------------------------------------------------------
void EvpPresenter::Connect() {
  // Handle Live button click.
  // Connect to mmap file
  if(mDebugLevel)
    cout << "Connect" << endl;
  //     TMapFile::SetMapAddress(0xb0473000);
  //TMapFile::SetMapAddress(0xb0468000);
  if ( !mfile) {
    cout << mMapFile << endl;
    //mfile = TMapFile::Create("evpEventServer.map");

    // Determine file type
    QString file = mMapFile;
    if ( file.find(".map") < 0 ) {
      TFile* rfile = new TFile(mMapFile);
      mfile = new GenericFile(rfile);
    } else {
      TMapFile* mapfile = TMapFile::Create(mMapFile,"READ",EvpUtil::mSharedMemorySize);
      if (!mapfile || mapfile->IsZombie() ) {
        cerr << " ### error ### Can not map file: evpEventServer.map" << endl;
        exit(-1);
      }
      mapfile->Print();
      EvpUtil::CheckCanvasDefinitions(mapfile);
      mfile = new GenericFile(mapfile);
    }
    mGroups.read(mfile);
  }

  ReconfigureTabs();
  
    
  return;
}


//--------------------------------------------------------------
void EvpPresenter::ReconfigureTabs() {
  cout << __PRETTY_FUNCTION__ << endl;
  emit removeGroupTabs();
  addGroupTabs();  //tmp->remove();
}

//--------------------------------------------------------------
void EvpPresenter::NextEvent() {
  needsUpdate = false;
  bool runStatusChanged = false;
  
  mRS = (RunStatus*) mfile->Get("RunStatus",mRS);
  if ( !mRS ) {
    cerr << " ### error ### can not find RunStatus " << endl;
    mRS = new RunStatus();//exit(-1);
  }
  mSS = (ServerStatus*) mfile->Get("ServerStatus",mSS);
  if ( !mSS ) {
    cerr << " ### error ### can not find ServerStatus " << endl;
    mSS = new ServerStatus();//exit(-1);
  }
      
  if ( mTriggerBitsRun  !=  mRS->getTriggerBitsRun() ) runStatusChanged = true;
  if ( mDetectorBitsRun !=  mRS->getDetectorBitsRun())  runStatusChanged = true;
  if (runStatusChanged) {
    ReconfigureTabs();
  } 

  runNumber = mRS->getRunNumber();
  evtNumber = mRS->getEventNumber();
  evtCounter = mRS->getEventCounter();
  mTriggerBitsRun =  mRS->getTriggerBitsRun();
  mDetectorBitsRun =  mRS->getDetectorBitsRun();

  int end = mRS->getEndOfRun();

  if (mDebugLevel) {
    cout << runNumber << " " << runNumberLast << " " << evtNumber <<  " " << evtNumberLast << " " << evtCounter <<  " " << evtCounterLast << " " << end << endl;
  }

  sprintf(displayText,"Star Histogram Presenter  run# %d,  event# %d,  count# %d", runNumber,evtNumber, evtCounter );

  //if ( runNumber != runNumberLast ) needsUpdate = true;
  //if ( evtNumber != evtNumberLast ) needsUpdate = true;
  if ( evtCounter != evtCounterLast ) needsUpdate = true;

  if( (end==1) && (file_uploaded==0) ) {
    // End of run is detected
    //One needs a flag whether pdf file was already saved for this run
    //EndOfRunAction(runNumber);
  }
    
  tabLast = tab;
  subTabLast = subTab;
  runNumberLast = runNumber;
  evtNumberLast = evtNumber;
  evtCounterLast = evtCounter;

}


void EvpPresenter::CrossOfDeath(TCanvas* gcc) {
  static TLine* a = new TLine(0.,0.,1.,1.);
  static TLine* b = new TLine(0.,1.,1.,0.);
  static TText* t = new TText(0.5,0.5,"not in current run");
  gcc->cd();
  a->SetLineColor(2);
  b->SetLineColor(2);
  t->SetTextColor(3);
  t->SetTextAlign(22);
  a->Draw();
  b->Draw();
  t->Draw();
  gcc->Update();
  //cout << __PRETTY_FUNCTION__ << endl;
  return;
}
 

void EvpPresenter::Draw(TCanvas* gcc, int  tab, int subTab) {
  if(EvpUtil::hGroupName[tab][subTab] != "") {
    Draw(gcc, EvpUtil::hGroupName[tab][subTab]);
    return;
  }

  if ( mDebugLevel) {
    cout << "Draw tab/subtab : " << tab << "/" << subTab << endl;
  }
  if ( (mRS->getDetectorBitsRun()&EvpUtil::canvasDetectorBits[tab][subTab])==0 ) { CrossOfDeath(gcc); return; }
  if ( (mRS->getTriggerBitsRun()&EvpUtil::canvasTriggerBits[tab][subTab])==0 ) { CrossOfDeath(gcc); return; }
  if (!mfile) return;
  
  if ( mLastDrawnCanvas != gcc ) needsUpdate = true;
  if ( !needsUpdate ) return;
  
//  GenericFile* gen = new GenericFile(mfile);
//  EvpUtil::DisplayOneCanvas(gen,gcc,tab,subTab);
//  delete gen;
  EvpUtil::DisplayOneCanvas(mfile,gcc,tab,subTab);
  mLastDrawnCanvas = gcc;
}

void EvpPresenter::Draw(TCanvas* gcc, const char* group) {
  if ( mDebugLevel ) {
    cout << "Draw group : " << group << endl;
  }
  HistogramGroup* hg = mGroups.read(mfile,group);
  if ( !gcc ) {
    cout <<  __PRETTY_FUNCTION__ << " no canvas " << endl;
    return; 
  } 
  if ( !hg ) { 
    cout <<  __PRETTY_FUNCTION__ << " no histogram group " << endl;
    return;
  }
  if ( mLastDrawnCanvas != gcc ) needsUpdate = true;
  if ( !needsUpdate ) return;
  
  hg->draw(gcc);
  mLastDrawnCanvas = gcc;
}



void EvpPresenter::Save(const char* file){
  char filename[1024];
  //cout << " fileame " << file << endl;
  if ( strcmp(file,"")==0 ) {
    sprintf(filename,"%s/run%d.root",EvpUtil::GetOutputPath(),mRS->getRunNumber());
  } else {
    if ( strcmp(file,gSystem->BaseName(file))==0) { // no path given, add default path
      sprintf(filename,"%s/%s",EvpUtil::GetOutputPath(),file);
    } else { // use filename as given
      sprintf(filename,"%s",file);
    }
  }
  cout << " filename " << filename << endl;
  if(mfile->mapFile()) {
    TMapFile* mapfile = (TMapFile*) mfile->file();
    EvpUtil::Map2Root(mapfile,filename);
  } else {
    TFile* rfile = (TFile*) mfile->file();
    rfile->Write(filename);
  }
}

void EvpPresenter::SaveAll(){
  char filename[1024];
  char cmd[1024];
  char title[1024];
  char cname[1024];
  //if ( true) return ;
  sprintf(filename,"run%d.map",mRS->getRunNumber() );
  sprintf(title,"End of run action : run%d",mRS->getRunNumber());
  sprintf(cname,"%d",(int)time(0));
  sprintf(cmd,"/bin/cp %s %s/%s",mMapFile,EvpUtil::GetOutputPath(),filename);
  //cout << cmd << endl;
  gSystem->Exec(cmd);
  
  gSystem->Sleep(1000);
  sprintf(cmd,"pwd; endOfRunAction.csh %s/%s & ",EvpUtil::GetOutputPath(),filename);
  cout << cmd << endl;
  gSystem->Exec(cmd);

}


void EvpPresenter::WriteCurrent(int i, int j){
  char psFilename[1024];
  char pdfFilename[1024];
  sprintf(psFilename,"%s/run%d_tab%d_%d.ps",EvpUtil::GetOutputPath(),mRS->getRunNumber(),i,j);
  sprintf(pdfFilename,"%s/run%d_tab%d_%d.pdf",EvpUtil::GetOutputPath(),mRS->getRunNumber(),i,j);
  WriteCurrentCanvasToPSFile(psFilename,i,j);

  char cmd[1024];
  sprintf(cmd,"/usr/bin/convert %s %s",psFilename, pdfFilename);
  int iret = gSystem->Exec(cmd);
  if ( !iret ) {
    cout << " ### error ### writing cureent canvas failed " << endl;
    cout << " ### error ### psFilename: " << psFilename << endl;
    cout << " ### error ### pdfFilename: " << pdfFilename << endl;
    cout << " ### error ### error-code: " << iret << endl;
  }
  /*
  if (iret==0) {
    PGMessage* pg = new PGMessage("Writing current canvas:",false,0xffffff); 
    pg->AddLine(psFilename);
    pg->AddLine(pdfFilename);
    pg->AddLine("O.K");
    pg->Layout();
  } else {
    PGMessage* pg = new PGMessage("### error ### Writing current canvas:",false,0xff0000); 
    pg->AddLine(psFilename);
    pg->AddLine(pdfFilename);
    pg->AddLine("return error");
    pg->Layout();
  }
  */

}    


void EvpPresenter::WriteCurrentCanvasToPSFile(const char* filename, int tab, int subTab){
  // Find active canvas indexes
  int i = tab;
  int j = subTab;
  //cout << filename << endl;
  int type = 111; //portrait ps
  //int type =112; //landscape ps
  //int type =113; //eps

  const char *name = "Saving Canvas in PS File";
  int canvasWidth  = EvpUtil::mCanvasWidth;
  int canvasHeight = EvpUtil::mCanvasHeight;


  TCanvas* c1 = new TCanvas(name, name, canvasWidth, canvasHeight);
  TPaveLabel title(0.1,0.96,0.9,0.99,mHistoPSFile);

  TDatime ftime;
  TPaveLabel date(0.7,0.01,0.9,0.03,ftime.AsString());


  //cout<<filename<<endl;
  TPostScript *ps = new TPostScript(filename,type);


  c1->cd();
  title.Draw();
  date.Draw();
  EvpUtil::DisplayOneCanvas(mfile,c1,i,j);
 
  c1->Update();

  ps->Close();
  c1->Close();

  //gSystem->Exec("ghostview test.ps");
  delete ps;
  //delete plotPad;
  delete c1;

}



void EvpPresenter::printAll(const char* filename) {
  cout << __PRETTY_FUNCTION__ << " " <<  filename << endl;

  gROOT->SetBatch(kTRUE);
  TCanvas *cc = new TCanvas("printAllCanvas","printAllCanvas",EvpUtil::mCanvasWidth,EvpUtil::mCanvasHeight);
  cc->cd();
  //TPostScript ps(filename,111);
  char openPs[1024]; 
  char printPs[1024];
  char closePs[1024];
  sprintf(openPs,"%s[",filename);
  sprintf(printPs,"%s",filename);
  sprintf(closePs,"%s]",filename);

  cc->Print(openPs);
  //DisplayRunStatus();
  cc->Print(printPs);
  for ( int tab=0; tab<EvpUtil::mNumberOfTabs; tab++) {
    for ( int subTab=0; subTab<EvpUtil::mNumberOfSubTabs[tab]; subTab++) {
      if ( (mRS->getDetectorBitsRun()&EvpUtil::canvasDetectorBits[tab][subTab])==0 ) continue;
      if ( (mRS->getTriggerBitsRun()&EvpUtil::canvasTriggerBits[tab][subTab])==0 )   continue;
      EvpUtil::DisplayOneCanvas(mfile,cc,tab,subTab,true);
      cc->Print(printPs);
    }
  }
  mGroups.print(cc,printPs);
  cc->Print(closePs);
  //mGroups.display(cc);
  //ps.Close();
  cout << filename << " written " << endl;
  delete cc;
}


void EvpPresenter::Print(TCanvas* gcc, int tab, int sub) {
  char psFilename[1024];
  sprintf(psFilename,"%s/run%d_tab%d_%d.ps",EvpUtil::GetOutputPath(),mRS->getRunNumber(),tab,sub);
  gROOT->SetBatch(kTRUE);
  TCanvas* cc = new TCanvas(psFilename,psFilename,EvpUtil::mCanvasWidth,EvpUtil::mCanvasHeight);
  cc->cd();
  if ( (mRS->getDetectorBitsRun()&EvpUtil::canvasDetectorBits[tab][sub])==0 ) { CrossOfDeath(gcc); return; }
  if ( (mRS->getTriggerBitsRun()&EvpUtil::canvasTriggerBits[tab][sub])==0 ) { CrossOfDeath(gcc); return; }
  EvpUtil::DisplayOneCanvas(mfile,cc,tab,sub);
  cc->Update();
  cc->Print(psFilename);
  cc->Close(); 
  gROOT->SetBatch(kFALSE);
  char cmd[1024];
  sprintf(cmd,"lp -d onlprinter2 %s",psFilename);
  sprintf(cmd,"ls %s",psFilename);
  gSystem->Exec(cmd);
}



void EvpPresenter::addGroupTabs() {
    GroupMap groupMap( mGroups );
    char name[1024];
    for( GroupMapIterator mapIter = groupMap.begin(); mapIter != groupMap.end(); mapIter++) {
      //cout << (*mapIter).first.c_str() << endl;
      sprintf(name,"%s",(*mapIter).first.c_str());
      //PR((*mapIter).second.numberOfActiveGroups());
      if ( (*mapIter).second.numberOfActiveGroups() ) {
	emit addGroupTab( name );
	for( GroupIterator groupIter = (*mapIter).second.begin(); groupIter != (*mapIter).second.end(); groupIter++) {
	  //cout << "\t " << (*groupIter)->subGroupName() << " " << (*groupIter)->id() << endl;
	  if ( (*groupIter)->active() ) {
	    //sprintf(name,"%s#%s#%s",(*groupIter)->subGroupName(),(*groupIter)->triggerName(),(*groupIter)->detectorName());
	    //emit addGroup( name );
	    emit addGroup( (*groupIter)->id() );
	  }
	}
      }
    }
}
#if 0
//______________________________________________________________________________ 
void EvpPresenter::ClosePresenter() 
{
     // Qt [slot] to terminate the application
   Stop();
}
#endif



/***************************************************************************
 *
 * $Id: EvpPresenter.cxx,v 1.8 2009/09/15 23:38:06 fine Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: EvpPresenter.cxx,v $
 * Revision 1.8  2009/09/15 23:38:06  fine
 * SL5 warning
 *
 * Revision 1.7  2009/04/29 19:18:05  fine
 * Eliminate the memory leak
 *
 * Revision 1.6  2009/04/06 18:49:29  dkettler
 * Histogram groups can be added to the main tabs by editing CanvasDescriptions.txt
 *
 * Revision 1.5  2009/02/04 03:43:10  dkettler
 * Addes Reference Plot Option
 *
 * Revision 1.4  2009/02/04 01:26:15  dkettler
 * Remove ONLINEPLOTSDIR reference
 *
 * Reference histograms
 *
 * Revision 1.3  2009/01/31 00:30:33  fine
 * Major clean up: Remove the redundant time thread, redundant methods, add protection agaist of crashes, introdcue the new rootrc paramater: Online.GuiRefreshRate, load some shared in batch mode only
 *
 * Revision 1.2  2009/01/30 20:35:32  fine
 * remove the redundant letsgo slot
 *
 * Revision 1.1  2009/01/23 16:16:03  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.8  2009/01/21 01:24:18  genevb
 * EMC updates (from O. Grebenyuk)
 *
 * Revision 1.7  2008/12/19 17:09:32  fine
 * the first full compilation against of the new DAQ Reader
 *
 * Revision 1.6  2007/11/27 11:50:18  psoren
 * fixed printAll() function
 *
 * Revision 1.5  2007/06/01 17:59:05  jml
 * blah
 *
 * Revision 1.4  2007/04/03 13:19:48  laue
 * *** empty log message ***
 *
 * Revision 1.3  2007/03/08 15:09:11  laue
 * Do not redraw same canvas if no new data arrived
 *
 * Revision 1.2  2007/03/01 20:40:34  laue
 * Updates to print active HistogramGroups into ps/pdf files
 *
 * Revision 1.1  2007/02/27 15:24:11  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:15  laue
 * Initial Version
 *
 *
 ***************************************************************************/

