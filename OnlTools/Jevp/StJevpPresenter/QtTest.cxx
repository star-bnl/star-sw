#include "QtTest.h"

#include <qapplication.h>
#include <TROOT.h>
#include <TSystem.h>
#include "QtTest.h"
#include "QtTestGui.h"
#include "qtimer.h"
#include <rtsLog.h>
#include <signal.h>

QtTest *qttest;

const char *testCurrFile = "none";
int testCurrLine = 0;

static void sigHandler(int arg, siginfo_t *sig, void *v)
{
  static char str[255];
  
  if(arg == 17) {
    LOG(DBG, "Got signal 17: ignoring");
    return;
  }

  sprintf(str,"Signal %d: shutting down! (file=%s line=%d)",arg,testCurrFile, testCurrLine);

  LOG(ERR, "%s", str);

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

int QtTest::main(char *args) 
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
  //rtsLogAddDest((char *)"130.199.60.86", 8004);

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
int QtTest::_main(int argc, char **argv )
{
  printf("_main\n");
  qttest = new QtTest();   // store globals here...
  CP;

  // Sometimes need this
  // Sometimes don't
  // depends on the STAR distribution!@
  printf("app\n");
  QApplication app(argc, argv);
  printf("app2\n");
  

  QtTestGui *gui = new QtTestGui();
  CP;
  printf("init\n");
  gui->init();
  printf("inited\n");


  printf("arm\n");
  // This issues an update...to start the update loop...
  QTimer::singleShot (100,gui,SLOT(UpdatePlots()));

  printf("done\n");

  //sleep(100);

  app.exec();
  return 0;  // just returns to roo4star...
}
