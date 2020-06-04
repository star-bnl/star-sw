#include "EvpMain.h"

#include <qapplication.h>
#include <TROOT.h>
#include <TSystem.h>
//#include "JevpLogic.h"
#include "JevpGui.h"
//#include "PresenterConnect.h"
#include "qtimer.h"
#include "ReferenceWidget.h"
//#include "QLabelTest.h"
//#include "StJevpPool/StJevpUtils/EvpUtil.h"
//#include "TSystem.h"
#include <rtsLog.h>
#include <signal.h>

EvpMain *evpMain;

const char *presenterCurrFile = "none";
int presenterCurrLine = 0;
int exiting=0;

static void sigHandler(int arg, siginfo_t *sig, void *v)
{
  static char str[255];
  
  if(exiting == 1) {
    LOG(ERR, "Signal %d, but already exiting", arg);
    return;
  }

  if(arg == 28) return;

  if(arg == 17) {
    LOG(DBG, "Got signal 17: ignoring");
    return;
  }

  sprintf(str,"Signal %d: shutting down! (file=%s line=%d)",arg,presenterCurrFile, presenterCurrLine);
  LOG(ERR, "%s", str);

  exiting = 1;
  exit(-1);
}

static void catchSignals(void)
{
  int i ;
  struct sigaction act ;
  
  LOG(DBG, "catching signals");

  // hook signals to my default...
  act.sa_sigaction = sigHandler ;
  act.sa_flags = SA_SIGINFO ;
  
  for(i=0;i<37;i++) {	// hook'em all!
    sigaction(i,&act,NULL) ;
  }
  
  return ;
}

int EvpMain::main(char *args) 
{
  char *argv[50];
  int argc=0;

  gSystem->ResetSignal(kSigChild);
  gSystem->ResetSignal(kSigBus);
  gSystem->ResetSignal(kSigSegmentationViolation);
  gSystem->ResetSignal(kSigIllegalInstruction);
  gSystem->ResetSignal(kSigSystem);
  gSystem->ResetSignal(kSigPipe);
  gSystem->ResetSignal(kSigAlarm);
  gSystem->ResetSignal(kSigUrgent);
  gSystem->ResetSignal(kSigFloatingException);
  gSystem->ResetSignal(kSigWindowChanged);
  
  catchSignals();

  CP;
  
  rtsLogOutput(RTS_LOG_STDERR);
  rtsLogAddDest((char *)"130.199.60.86", 8004);

  CP;

  argv[0] = strtok(args, " ");
  argc++;

  char *next;
  while((next = strtok(NULL, " "))) {
    argv[argc] = next;
    argc++;
  }
 
  CP;
  return _main(argc, argv);
}


//---- Main program ------------------------------------------------------
int EvpMain::_main(int argc, char **argv )
{
    // printf("_main\n");

  QApplication app(argc, argv, true);


  evpMain = new EvpMain();   // store globals here...

  //printf("constructed\n");
  CP;
  if(evpMain->parseArgs(argc, argv) < 0) return 0;

  //printf("parsed\n");
  CP;

  //QApplication app(argc, argv);

  JevpGui *gui = new JevpGui();
  CP;
  //printf("constructeed\n");
  gui->init();
  //printf("initied\n");

  //gui->jl_JevpLogic();
  //gui->gui_JevpGui(logic);
  //gui->show();
  //gui->pc_PresenterConnect(gui, logic);
  
  // printf("arm\n");
  // This issues an update...to start the update loop...
  QTimer::singleShot (100,gui,SLOT(UpdatePlots()));

  //printf("done\n");

  app.exec();
  return 0;  // just returns to roo4star...
}

int EvpMain::parseArgs(int argc, char *argv[])
{
  if(!argv) return 0;
  if(!argv[0]) return 0;

  for(int i=0;i<argc;i++) {
    if (memcmp(argv[i], "-noserver", 9) == 0) {
      server = NULL;
    }
    else if (memcmp(argv[i], "-server", 7) == 0) {
      i++;
      server = argv[i];
    }
    else if (memcmp(argv[i], "-port", 5) == 0) {
      i++;
      serverport = atoi(argv[i]);
    }
    else if (memcmp(argv[i], "-display", 8) == 0) {
      i++;
      displayFile = 0;
      display = argv[i];
    }
    else if (strcmp(argv[i], "-test") == 0) {
      serverport = JEVP_PORT + 10;
    }
    else if (memcmp(argv[i], "-localdisplay", 12) == 0) {
      i++;
      displayFile = 1;
      display = argv[i];
    }
    else if (memcmp(argv[i], "-loop", 5) == 0) {
    }
    else if (memcmp(argv[i], "-live", 5) == 0) {
      rtsLogOutput(RTS_LOG_NET);
    }
    else {
      printf("%s arguments\n\t-noserver\n\t-server servername\n\t-port port\n\tdisplay <display>\n\t-localdisplay <display>",argv[0]);
      return -1;
    }    
  }
  return 0 ;
}
