#include "evpServerMain.h"
#include "DGHelp.h"
#include "EvpServer.h"
#include "TGWindow.h"
#include "EvpUtil.h"
#include <fstream>
#include <TEnv.h>

EvpServer *evpServerMain::mEvpServer=0;


//_______________________________________________________
static void guitest(const char* name ) {

  cout << "Process id: " << getpid() << endl;

  ifstream testfile("/tmp/serverLock");
  if(testfile.is_open()) { // File may exist, but is content current?
    int procnum;
    testfile >> procnum;
    testfile.close();
    
    char cmd[1024];
    sprintf(cmd,"%s root4starN %d",gEnv->GetValue("Online.testProc","/a/pplot/bin/test_proc.csh"),procnum);
    int j = system(cmd);
    //cout << j << endl;
    if(j==256) {
      cout << "Server is already running!  Aborting.  " << j << endl;
      exit(-1);
    } else {
      cout << "Old lock file found, deleting." << endl;
      sprintf(cmd,"rm -f /tmp/serverLock");
      system(cmd);
    }
  }

  cout << "Creating lock file" << endl;

  char cmd[1024];
  sprintf(cmd,"umask 222; echo %d > /tmp/serverLock",getpid());
  system(cmd);

// char cmd[1024];
// sprintf(cmd,"/RTS/bin/LINUX/i686/find_proc.csh %s",name);
//
// int j = system(cmd);
//
//   //Normal return should be 256 if there is only one process(itself) running
//   if(j==256){
//     cout<<"Good going no duplicate copy is found. Proceed "<< j<<endl;
//        }
//   else {
//     //Yell when something is wrong
//     cout<<"Program evpServer is already running! "<<j<<endl;
//     printf("(%s) returns %d\n",cmd,j);
//     fflush(stdout);
//
//     new DGHelp( strcat( getenv("ONLINEPLOTSDIR"),"/Infrastructure/messages/duplicate_copy.message"),true,0xff0000);
//     
//
//     // Introduce delay here to allow pop-up window to show up on the screen 
//     //for 10 sec
//     cout << " Waiting ";
//     for(int i=10;i>0;i--){
//       //     gSystem->Sleep(20000); // do not use root sleep function
//       cout << " " << i;
//       cout.flush();
//       sleep(1);
//       gSystem->ProcessEvents();
//     }
//
//       exit(-1);
//   }


    
}
//_______________________________________________________
int evpServerMain::main(int argc, const char* argv[]) {

   bool gui = true;
   bool start = false;
   bool live = false;
   bool path = false;
   bool mmap = false;
   bool disabled = false;
   bool end = false;
   int pathArg = 0;
   int mmapArg = 0;
   bool check = true;
   bool quit = false;
   bool help = false;
   int nevents = (int)1e9;

  cout << argc << " command line arguments: ";
  for ( int i=0; i<argc; i++ ) {
    cout << " " << argv[i];
    if ( strcmp(argv[i],"-nogui")==0 ) 	            gui=false;
    if ( strcmp(argv[i],"-live")==0 ) 	            live=true;
    if ( strcmp(argv[i],"-start")==0 ) 	            start=true;
    if ( strcmp(argv[i],"-path")==0 && i+1<argc ) { path = true; pathArg = i+1; }
    if ( strcmp(argv[i],"-map")==0 && i+1<argc )  { mmap = true; mmapArg = i+1; }
    if ( strcmp(argv[i],"-nevents")==0 && i+1<argc )  { nevents = atoi(argv[i+1]); }
    if ( strcmp(argv[i],"-end")==0 )                end = true;
    if ( strcmp(argv[i],"-quit")==0 )               quit = true;
    if ( strcmp(argv[i],"-disable")==0  )           disabled = true;
    if ( strcmp(argv[i],"-nocheck")==0 )            check = false;
    if ( strcmp(argv[i],"-h")==0 )                  help = true;
  }

  cout << endl;
  cout << endl;

  if ( help ) {
    cout << "usage: evpServer [options] " << endl;
    cout << " options are: " << endl;
    cout << "-nogui " << endl;
    cout << "-live " << endl;
    cout << "-start " << endl;
    cout << "-path <inputFilePath>" << endl;
    cout << "-map <mapFilePath> " << endl;
    cout << "-end " << endl;
    cout << "-disable " << endl;
    cout << "-nocheck " << endl;
    cout << "-nevents" << endl;
    cout << "-h " << endl;
    return 0;
  }

  if ( check ) guitest("pplotServer");
  if ( mmap ) sprintf(EvpUtil::mMapFilePath,"%s",argv[mmapArg]); 

   mEvpServer = new EvpServer(gui);
   mEvpServer->SetNewTarget("/a");
   mEvpServer->SetDoEndOfRun(false);
   if (live)     mEvpServer->SetLive();
   if (path)     mEvpServer->SetNewTarget((char*)argv[pathArg]);
   if (start)    mEvpServer->SetGoFlag();
   if (disabled) mEvpServer->SetEnabled(false);

   mEvpServer->SetMaxEventsPerRun(nevents);
   mEvpServer->SetDoEndOfRun( end );
   mEvpServer->SetDoEndOfRunQuit( quit );
	
  mEvpServer->run();
  
  return 0;
}









/***************************************************************************
 *
 * $Id: evpServerMain.cxx,v 1.2 2009/03/06 23:44:12 dkettler Exp $
 *
 * Author: Valeri Fine Laue, fine@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: evpServerMain.cxx,v $
 * Revision 1.2  2009/03/06 23:44:12  dkettler
 * Lock file added
 *
 * Revision 1.1  2009/01/23 16:11:03  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.1  2008/03/17 22:58:46  fine
 * Add the 3 new classes with the static main method
 *
 * Revision 1.5  2007/05/30 13:13:54  jml
 * blah
 *
 * Revision 1.1  2006/10/04 20:31:16  laue
 * Initial Version
 *
 *
 ***************************************************************************/

