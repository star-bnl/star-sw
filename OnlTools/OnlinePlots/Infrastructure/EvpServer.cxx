//Author: Sergey Panitkin
//Date:   2001

/////////////////////////////////////////////////////////////////
//  Main Event Loop
/////////////////////////////////////////////////////////////////
//
//
//

#include "EvpServer.h"
#include "StReadLaserEvent.h"
#include "HistoHandler.h"
#include "EndOfRunAction.h"
#include "PGMessage.h"
#include "GroupCollection.h"

#include "StRoot/StEEmcPool/muEztPanitkin/EEqaSorter.h"
#include "StRoot/StEEmcPool/muEztPanitkin/Tonko2Ezt.h" 

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include <daqFormats.h>
#else
#  include "DAQ_READER/daqReader.h"
#  include "DAQ_TRG/trgReader.h"
#  include "DAQ_READER/cfgutil.h"
#endif
#include "Lock.h"


//#include "EEmcQA2/EEqaSorter.h"
#include "SsdAdcLogTable.h"               // Bo from Renaud 03/03

//L3 Tracker here
#include "eventTrackerLib.hh"


// Jing Liu, for tofr5  ---
//#include "tofr.h"


#include "TGClient.h"


ClassImp(EvpServer) ;

extern TGClient* gClient;
#ifndef PR
#  define PR(x) {if (mDebugLevel)  cout << __PRETTY_FUNCTION__ << " ::: " << (#x) << " = " << x << endl;}
#endif

//extern void decodeBemcTrigger(const int,unsigned char*, int *, int *);

class HistoHander;

static const int nMaxTrgWd=10;

//PMD Related constants
static const int Crate = 2;			// 2 Crates max
static const int CRAM=12; 			// Max no of Crams
static const int BLOCK=2; 			// Max no of blocks in each cram
static const int CHANNEL=1728; 		//Max no of channels in eack block
static const int pmd_hist_begin=381;  // PMD hist starts after 381 no.



//-----------------------------------
//Buffer for event storage

static const int sizL3_max = 1000000;

static L3_P *l3p =(L3_P *)malloc(sizL3_max);

static EventTracker *evtTracker = new EventTracker();

char* getEvent3(evpReader* evp, int type);




EvpServer::EvpServer(bool gui) :        
        mQuit(false)
      , mMaxEventsPerRun((int)1e9)
      , evp(0)
      , mFile(0)
      , mRS(0)
      , mGui(0)
{
 
  rtsLogLevel(NOTE);
  rtsLogAddDest("172.16.0.1",8002);
  rtsLogOutput(RTS_LOG_NET);
  Lock::unlock();
  //TMapFile::SetMapAddress(0xb0468000);
  //TMapFile::SetMapAddress(0xb0506000);

  mGroups.serverCreate();
  mGroups.list();

  mDoEndOfRun = false;
  mDoEndOfRunQuit = false;
  
  mFile = TMapFile::Create(EvpUtil::mMapFilePath,"RECREATE", EvpUtil::mSharedMemorySize);
  if ( mFile==0 ) {
    cerr << " ### error ### Can ot create memory mapped file " << endl;
    exit(-1);
  }
  mFile->Print();
  mFile->ls();

  mRS = new RunStatus();
  mFile->Add(mRS,"RunStatus");

  mSS = new ServerStatus();
  mFile->Add(mSS,"ServerStatus");


  mHistoHandler = new HistoHandler(mFile);
  mHistoHandler->SetHistoListFile(HistoHandler::mListOfHistograms);
  mHistoHandler->SetDefaults();
  mHistoHandler->Book(); // Read histogram definition file and book histograms
  if ( gui) {
    mGui = new ServerGui(gClient->GetRoot(), 400,220,this);
  }
  SetDefaults();

  
 //csp Temporarily only!!!
  SetDebugLevel(0);
  if(mDebugLevel) cout<<"Instantiating EvpServer"<<endl;
  Update();
  UpdateInfo();
  cout << " size of map file : " << EvpUtil::GetSizeOfMappedObjects(mFile) << endl;

  mGroups.addToMapFile(mFile);
  mGroups.push(mFile);
  mGroups.list();

}

EvpServer::~EvpServer(){
  //Default destructor

  //printf("************ closing evpserver ****************\n");
  //fflush(stdout);

  mFile->Close();
  free(l3p);
}

void EvpServer::UpdateInfo() {

  // update gui
  if (mGui) {
    mGui->UpdateRunInfo( mRS->getRunNumber() );
    mGui->UpdateEventCountInfo( mRS->getEventCounter() );
    mGui->UpdateEventNumberInfo( mRS->getEventNumber() );
    mGui->UpdateTokenNumberInfo( mRS->getToken() ); 
    if( mRS->getLiveSource() ==1 ) mGui->ShowLiveRun(true); // Change GUI status
    if( mRS->getLiveSource() ==0 ) mGui->ShowFileRun(true); // Change GUI status
    if ( mGoFlag )  mGui->ShowRunning(true);
    if ( !mGoFlag ) mGui->ShowRunning(false);
  }
}

//-------------------------------------------------------------
int  EvpServer::NextEvent() {
  static int lastRun;
  static int lastEvent;
  static int lastEndOfRunAction;

  if(mDebugLevel) cout<<"Next Event"<<endl;
  gSystem->ProcessEvents();

  int now = time(0);
  mSS->setRequestTime(now);
  mRS->setEndOfRun(0);
  mRS->setEmpty(0);

  mSS->setRequestType(EVP_TYPE_ANY);

  //printf("Get an event\n");
  //fflush(stdout);

  char* mem = evp->get(0, mSS->getRequestType() );
 
  //printf("Got an event\n");
  //fflush(stdout);
#ifndef NEW_DAQ_READER
  if(mem == NULL) {
    mRS->setEmpty(1);
    switch(evp->status) {
    case EVP_STAT_OK :	// should retry as fast as possible...
      //cout <<"."; cout.flush();
      break;
    case EVP_STAT_EOR :	// EOF
      mRS->setEndOfRun(1);
      cout <<"-"; cout.flush();
      break;
    case EVP_STAT_EVT :
      printf("Bad event - skipping... after sleeping 5 \n") ;
      sleep(5) ;
      break;
    case EVP_STAT_CRIT :
      fprintf(stderr,"Critical error - stopping!\n") ;
      sleep(5) ;
      break;
    default:
      cout << "?"; cout.flush();
      break;
    }    
  }
#endif

  int status = evp->status;

  if(status != EVP_STAT_OK) {
    if ( mDebugLevel) {
      cout<<" Warning! status="<<status<<endl;
    }
    gSystem->Sleep(5);
  }


  if ( mRS->getEventCounter() > mMaxEventsPerRun  ) {
    status = EVP_STAT_EOR;
    mRS->setEndOfRun(1);
  }

#ifndef NEW_DAQ_READER

  int bad_mask = sanityCheck(mem);
  if(bad_mask) {
    // Removed only to make analyzing log files easier!  Still need to figure out what's wrong
    //printf("Bad mask...0x%x\n",bad_mask);
    fflush(stdout);
     
    // pretend like the event was bad...
    mem = NULL;
    status = EVP_STAT_EVT;
  }
#endif

  unsigned int dets = evp->detectors;
  unsigned int evpgroup = evp->evpgroups;

  //printf("update\n");
  //fflush(stdout);

  UpdateInfo();

  //printf("updated\n");
  //fflush(stdout);
  
  // We have got a good event here. Update event information.
  if ( mem ) {
    mSS->setReceiveTime(now);
    mSS->setReceiveType(evp->evb_type);
    mRS->setRunNumber(evp->run);
  }
  
  // check whether to perform end of run action before filling new histograms 
  bool runConfigurationChanged = false;
  bool doEnd = false;
  bool doNew = false;

  //printf("lastRun=%d\n",lastRun);
  //printf("getRunNumber()=%d\n",mRS->getRunNumber());
  //printf("mRS->getEndOfRun()=%d\n",mRS->getEndOfRun());
  //printf("lastEndOfRunAction=%d\n",lastEndOfRunAction);

  if ( lastRun!=mRS->getRunNumber() )              { doEnd = true; doNew = true; runConfigurationChanged = true;}
  if ( mRS->getEndOfRun() == 1 )                   doEnd = true;
  if ( lastRun == lastEndOfRunAction )             doEnd = false;
  if ( lastRun == 0 )                              doEnd = false;

  //printf("Do end = %d\n",doEnd);
  //fflush(stdout);

  if ( doEnd ) {
    mGroups.endRun();
    if (mDoEndOfRun) {
      LaunchEndOfRunAction(lastRun);
    }
    if (mDoEndOfRunQuit) {
      cout << "quit " << endl;
      mQuit = true;
      SetStopFlag();
      return 0;
    } 
    lastEndOfRunAction = lastRun;
  }

  //printf("Do new = %d\n",doNew);
  //fflush(stdout);

  if ( doNew ) {
    Reset();  // reset all histograms 
    mGroups.beginRun(evp, mem);
  }

  // if the run configuration has changed, 
  // delete run specific histograms groups and 
  // create the groups for the new run
  if ( mRS->getTriggerBitsRun() != evp->evpgroupsinrun ) runConfigurationChanged = true;
  if ( mRS->getDetectorBitsRun() != evp->detsinrun )     runConfigurationChanged = true;
  if ( runConfigurationChanged ) {
    mGroups.setTriggerDetectorBits(evp->evpgroupsinrun, evp->detsinrun);
    mGroups.setActive();
    mGroups.push(mFile);
  }

  // update run info
  if ( mem ) { 
    mRS->setEventCounter( mRS->getEventCounter()+1 );
    mRS->setEventNumber(evp->event_number);
    mRS->setToken(evp->token);
    mRS->setType(evp->evb_type);
    mRS->setStatus(evp->status);
    mRS->setTime(evp->evt_time);
    mRS->setRunNumber(evp->run);
    mRS->setTriggerBits(evp->evpgroups);
    mRS->setDetectorBits(evp->detectors);
    mRS->setTriggerBitsRun(evp->evpgroupsinrun);
    mRS->setDetectorBitsRun(evp->detsinrun);
    if( mRS->getRunNumber() != lastRun ) {
      mRS->setEventCounter(1);
      mRS->setEndOfRunActionPerformed(0);
    }
    unsigned int trigBit = evp->evpgroups;
    unsigned int detBit = evp->detectors;

 
    //   if ( mRS->getEventCounter() < mMaxEventsPerRun  ) {

    //printf("Histofill\n");
    //fflush(stdout);
    
    mHistoHandler->fillHistos(evp, mem);
    
    //printf("mgroupsfill\n");
    //fflush(stdout);

    mGroups.fill( evp, mem, trigBit, detBit); 
    
    //printf("mgroupsfilled\n");
    //fflush(stdout);

    //    } else {
    // mQuit = true;
      //}
    
  }

 
  if ( (lastEvent!=mRS->getEventNumber()) || (lastRun!=mRS->getRunNumber()) ) {

    //printf("updating\n");
    //fflush(stdout);
     
    Update();  // update memory mapped file
     
    //printf("updateinginfo\n");
    //fflush(stdout);
     
    UpdateInfo();
     
    //printf("updatedinfo\n");
    //fflush(stdout);
     
    //cout << " update " << mRS->getEventNumber() << " " << mRS->getRunNumber() << endl;
  }
   
  //printf("processEvents\n");
  //fflush(stdout);

  gSystem->ProcessEvents();
  //cout << "."; cout.flush();
  lastRun = mRS->getRunNumber();
  lastEvent = mRS->getEventNumber();
  mFile->Update(mSS);

  //printf("Done stat=%d\n",status);
  //fflush(stdout);
  return status;
}
//-------------------------------------------------------------
void EvpServer::run() {
  while ( !mQuit ) {
    //    gSystem->Sleep(100);
    gSystem->ProcessEvents();
    if ( mGoFlag ) {
      MainLoop();
    }
  }
}

//-------------------------------------------------------------
void EvpServer::Update() {
  Lock::update(mFile);
}

//-------------------------------------------------------------
void EvpServer::MainLoop() {


  int lastRun = 0;
  //    int active = 0; // reading from file or directory
  //    //  int active = 1; // active run
  //    int evtype = 7; //Any event =7
  //gSystem->Sleep(100);
  if ( mGui) {
    mGui->ShowStatus("Connecting ...");
  }

  if(mDebugLevel) cout<<"Main Loop"<<endl;
  
  // If the first time set the reader here
  //if(!evp) {
  //  evp = new evpReader(mTarget);
  //}
  

  while(mGoFlag && !mQuit) {
    if (mGui) {
      mGui->ShowStatus("Run in progress");
    }
    int iret = NextEvent();
    // Bad return flag from a reader
    
    // Check first whether it's an end of run
    if ( mRS->getEndOfRun()==1) {
      if (mGui) {
	mGui->ShowStatus("Run ended");
      }
      // For LIVE run keep going until the next run
      if  ( mRS->getLiveSource() != 1) {
	mGoFlag = 0;
      }
    }

    
    lastRun = mRS->getRunNumber();
    //cout << "."; cout.flush();
  }
  UpdateInfo();

}


void EvpServer::LaunchEndOfRunAction(long run) {
  char filename[1024];
  char cmd[1024];
  char title[1024];
  char cname[1024];
  //if ( true) return ;
  sprintf(filename,"%s/run%d.root",EvpUtil::GetOutputPath(),run );
  LOG(INFO,"Online plots: Requesting end of run action for run #%d   %d/%d events processed",run,mRS->getEventCounter(),mRS->getEventNumber()); 
  Save(filename);
  sprintf(cmd,"pwd = %s %s ",gSystem->WorkingDirectory(),filename);
  cout << cmd << endl;
  sprintf(cmd,"cd %s; unset DISPLAY; /RTS/bin/LINUX/i686/pplotEndOfRun %s & ",gSystem->WorkingDirectory(),filename);
  system(cmd);
  mRS->setEndOfRunActionPerformed(1);
}

//------------------------------------------
void EvpServer::SetNewTarget(char *lTarget) {
  sprintf(mTarget,"%s",lTarget);
  if(mDebugLevel) {
    cout<<"New Target: "<< mTarget<<endl;
  }
  // delete evp reader anyways
  cout << " is this an evpReader ? : " << dynamic_cast<evpReader*>(evp) << endl;
  if(evp) {
    //LOG(INFO,"Jeff, I believe the evpReader destructor is crashing. Thus memory leak");
    delete evp;
    evp = 0;
  }
  //
  // You can reset or save your histograms here, but for now lets do it manually
  //


  // check what type of a run we have here
  if(!strncmp(mTarget,"LIVE",4))
    {
      evp = new evpReader(NULL);
      mRS->setLiveSource(1);
      cout << "Connecting to current (LIVE) run"<<endl;
    }
  else
    {
      evp = new evpReader(mTarget);
      mRS->setLiveSource(0);
    }
  
  if (mGui) {
    mGui->SetTarget(mTarget);
  }
// Beginning of the run
  *mRS = RunStatus();
  Update();
  // Should be set separately via new methods
  // hardwired for now
  mSS->setReceiveType(7);  // take any event type

  // Can be anything here, but lets set "no-go"
  mGoFlag = 0; // Reset GoFlag to "no-go"


  // Reset run info displayed by GUI
  if(mGui) {
    mGui->UpdateRunInfo( mRS->getRunNumber() );
    mGui->UpdateEventCountInfo(0);
    mGui->UpdateEventNumberInfo(0);
    mGui->UpdateTokenNumberInfo(0);
    mGui->ShowStatus("Ready");
  }
}
//----------------------------------------------------------------------
void EvpServer::SetDefaults()
{

  mGoFlag = 1;
  mDebugLevel = 1;

  mHistoSaved = 0; // initially histograms are not saved

  // Starting directory for "FILE" mode
  char *startingDirectory;

  startingDirectory ="/a";

  // Make it a default target
  SetNewTarget(startingDirectory);

  //Set mapping of TPC sector, row, pad indexes to global phi angle
  //    mHistoHander->SetPhiAngleMap();

  if(mDebugLevel) cout<<"Setting Defaults..."<<endl;
}

void EvpServer::SetLive() {
    SetNewTarget("LIVE");
    if (mGui) {
      mGui->SetTarget("");
    }
    mRS->setLiveSource(1);
}


void EvpServer::SetPhiAngleMap() { mHistoHandler->SetPhiAngleMap(); }
void EvpServer::Print() { mHistoHandler->Print(); }
void EvpServer::Reset() { 
  mGroups.reset(); 
  mHistoHandler->Reset(); 
 Lock::update(mFile); 
}
void EvpServer::Save(const char* file) {
  char filename[1024];
  char text[1024];
  if ( strcmp(file,"")==0 ) {
    sprintf(filename,"%s/run%d.root",EvpUtil::GetOutputPath(),mRS->getRunNumber());
  } else {
    if ( strcmp(file,gSystem->BaseName(file))==0) { // no path given, add default path
      sprintf(filename,"%s/%s",EvpUtil::GetOutputPath(),file);
    } else { // use filename as given
      sprintf(filename,"%s",file);
    }
  }
  int iret = mHistoHandler->Save(filename);
  Pixel_t color=0xffffff;
  if ( iret) {
    sprintf(text,"### error ### could not save to file %s",filename);
    color = 0xff0000;
    new PGMessage(text,false, color);
  } else {
    TFile f(filename,"update");
    mGroups.save();
    mRS->Write();
    f.Close();
  }  
}


//-------------------------------------------------------------
void EvpServer::MakePS() {mHistoHandler->MakePS();}


//-------------------------------------------------------------
void EvpServer::SetEnabled(bool b) {
  if ( mGui ) {
    mGui->SetEnabled(b);
  }
}

//-------------------------------------------------------------
void EvpServer::SetGoFlag(void)
  { 
    mGoFlag = 1;
    if(mDebugLevel)
      cout<<"Setting Go Flag"<<endl;
  }

//-------------------------------------------------------------
void EvpServer::SetStopFlag(void)
{
  mGoFlag = 0;
  if(mDebugLevel)
    cout<<"Setting Stop Flag"<<endl;
}






/***************************************************************************
 *
 * $Id: EvpServer.cxx,v 1.1 2009/01/23 16:11:03 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: EvpServer.cxx,v $
 * Revision 1.1  2009/01/23 16:11:03  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.14  2009/01/21 01:24:17  genevb
 * EMC updates (from O. Grebenyuk)
 *
 * Revision 1.13  2008/12/19 17:09:15  fine
 * the first full compilation against of the new DAQ Reader
 *
 * Revision 1.12  2008/12/19 15:51:15  dkettler
 * Added new daqReader
 *
 * Revision 1.11  2008/02/15 18:51:47  dkettler
 * Updates to laser and TOF reader
 *
 * Revision 1.10  2007/05/30 13:13:54  jml
 * blah
 *
 * Revision 1.9  2007/05/25 14:53:44  jml
 * blah
 *
 * Revision 1.8  2007/05/24 16:58:07  jml
 * blah
 *
 * Revision 1.7  2007/04/25 17:52:29  laue
 * Minor updates
 *
 * Revision 1.6  2007/04/05 16:49:29  laue
 * *** empty log message ***
 *
 * Revision 1.5  2007/04/03 13:19:33  laue
 * Some minor modification on laser histograms by request from Blair
 *
 * Revision 1.4  2007/03/21 17:11:45  laue
 * Moved individual HistogramGroups to their own folder
 * Modified Makefile and server to allow profiling with gprof (i.e. must not exot with exit(...) function)
 *
 * Revision 1.3  2007/03/01 20:40:34  laue
 * Updates to print active HistogramGroups into ps/pdf files
 *
 * Revision 1.2  2007/03/01 14:29:08  laue
 *
 * Modified Files: update for laser histograms
 *  	EvpServer.cxx
 * Removed Files: clean up of dynamically created and obsolete files
 *  	evpCint.h evpPresenter.cxx
 *
 * Revision 1.1  2007/02/27 15:23:37  laue
 * Initial version
 *
 * Revision 1.2  2006/10/27 17:43:21  laue
 * Resources folder added
 * histogram controll class OTH added
 *
 * Revision 1.1  2006/10/04 20:31:15  laue
 * Initial Version
 *
 *
 ***************************************************************************/

