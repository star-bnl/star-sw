#include <ctype.h>
#include <TROOT.h>
#include <TServerSocket.h>
#include <TSocket.h>
#include <TMessage.h>
#include <TMonitor.h>
#include <TClass.h>
#include <TCanvas.h>
#include <PDFUtil/PdfIndex.hh>
#include <dirent.h>
#include <TFile.h>
#include <TLine.h>
#include <TStyle.h>
#include <TFrame.h>
#include <TText.h>
#include <TSystem.h>
#include <signal.h>
#include <TThread.h>
#include <TApplication.h>
#include <TList.h>
#include <setjmp.h>

#include "EvpConstants.h"
#include "JevpServer.h"
#include <StRoot/RTS/include/rtsLog.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/wait.h>
#include "Jevp/StJevpPlot/JLatex.h"
#include "JTMonitor.h"

#include "Jevp/StJevpBuilders/baseBuilder.h"
#include "Jevp/StJevpBuilders/bbcBuilder.h"
#include "Jevp/StJevpBuilders/daqBuilder.h"
#include "Jevp/StJevpBuilders/eemcBuilder.h"
#include "Jevp/StJevpBuilders/bemcBuilder.h"
#include "Jevp/StJevpBuilders/fpdBuilder.h"
#include "Jevp/StJevpBuilders/hltBuilder.h"
#include "Jevp/StJevpBuilders/l3Builder.h"
#include "Jevp/StJevpBuilders/tofBuilder.h"
#include "Jevp/StJevpBuilders/mtdBuilder.h"
#include "Jevp/StJevpBuilders/tpxBuilder.h"
#include "Jevp/StJevpBuilders/trgBuilder.h"
#include "Jevp/StJevpBuilders/upcBuilder.h"
#include "Jevp/StJevpBuilders/fgtBuilder.h"
#include "Jevp/StJevpBuilders/vpdBuilder.h"
#include "Jevp/StJevpBuilders/fmsBuilder.h"
#include "Jevp/StJevpBuilders/gmtBuilder.h"
#include "Jevp/StJevpBuilders/l4Builder.h"
#include "Jevp/StJevpBuilders/pxlBuilder.h"
#include "Jevp/StJevpBuilders/istBuilder.h"
#include "Jevp/StJevpBuilders/ssdBuilder.h"
#include "Jevp/StJevpBuilders/ppBuilder.h"
#include "Jevp/StJevpBuilders/fpsBuilder.h"

#include <RTS/include/SUNRT/clockClass.h>

static int line_number=0;
static char *line_builder = NULL;
#define CP line_number=__LINE__
#define CP_ENTER_BUILDER(x) line_builder = x
#define CP_LEAVE_BUILDER line_builder = NULL;
static sigjmp_buf env;

int JEVPSERVERport;
JevpServer serv;


static void sigHandler(int arg, siginfo_t *sig, void *v)
{
  static char str[255];
  
  if(arg == SIGCHLD) {
    int status;
    waitpid(-1, &status, WNOHANG);
    LOG(DBG, "Got signal SIGCHLD (reading pdf?) ");
    return;
  }

  // If we are trying to cleam up after a builder!
  if(line_builder) {
    siglongjmp(env,1);
  }

  // Otherwise just get out!
  sprintf(str,"Signal %d: shutting down! (line=%d)", arg, line_number);
  LOG(ERR, "%s", str);

  exit(-1);
}

static void ignoreSignals()
{
  int i ;
  
  for(i=0;i<37;i++) {	// hook'em all!
    signal(i,SIG_IGN) ;
  }
  
  return;
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
  
  return;
}

void _JevpServerMain(int argc, char *argv[])
{

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

  JevpServer::main(argc, argv);
}

unsigned long long int JevpServer::getMemUse() {
  FILE *f = fopen("/proc/self/status", "r");
  if(!f) return 0;

  unsigned long long int sz = 0;
  char *line = (char *)malloc(128);
  size_t lsz = 128;


  for(;;) {
    if(getline(&line, &lsz, f) == -1) {
      fclose(f);
      free(line);
      return sz;
    }

    if(!strncmp(line, "VmData:", 7)) {
      sz = atoll(&line[7]);
      fclose(f);
      free(line);
      return sz;
    }
  }
  return sz;
}

void JevpServer::debugBuilders(int line)
{
#ifdef DEBUG
  TListIter next(&builders);
  JevpPlotSet *curr;
  while((curr = (JevpPlotSet *)next())) {
    LOG("JEFF", "print name (%d): %s",line, curr->getPlotSetName());
  }
#endif
}

void JevpServer::main(int argc, char *argv[])
{
  // gErrorIgnoreLevel = kBreak;   // suppress root messages...
  serv.parseArgs(argc, argv);
  
  rtsLogOutput(serv.log_output);
  rtsLogAddDest(serv.log_dest, serv.log_port);
  rtsLogLevel(serv.log_level);

  LOG("JEFF", "Starting JevpServer: port=%d pid=%d isL4=%d", serv.myport, (int)getpid(),serv.isL4);

  // Each time we start, archive the existing display file...
  serv.init(serv.myport, argc, argv);

  if(serv.justUpdateDisplayPallete == 1) {
    serv.justUpdatePallete();
    return;
  }

  // Start reader thread
  TThread *rThread = new TThread("readerThread", (void(*)(void *))(&JEVPSERVERreaderThread),(void *)&serv);
  rThread->Run();

  
  for(;;) {
    
    serv.readSocket();
    
    LOG(DBG, "Read socket!");
  }
  
  
//     int delay = serv.handleEvent();   // get an event and call the builders...

//     // delay depends on the event status
//     // 0 if successfully read an event
//     // longer if no events available...
//     CP;
//     serv.handleClient(delay);   
}

void JevpServer::readSocket()
{
    TSocket *s;
    TMessage *mess;
  
    CP;
    s = mon->Select(100);
    if((long) s <= 0) {
	CP;
	LOG(DBG, "Got a timeout or an error reading socket");
	return;
    }
    CP;

    // Check if it is a new connection!
    if(s == ssocket) {
	CP;
	TSocket *nsocket = ssocket->Accept();
	//TInetAddress adr = nsocket->GetInetAddress();
	mon->Add(nsocket);
	return;
    }
    CP;

    // No it is data...
    int ret = s->Recv(mess);
    CP;

    if(ret == 0) {
	CP;
	LOG(DBG, "Disconnecting a client...");
	mon->Remove(s);
	delete s;
	delete mess;
	return;
    }
    CP;

    // If it is an EvpMessage
    if(strcmp(mess->GetClass()->GetName(),"EvpMessage") == 0) {
	CP;
	EvpMessage *msg = (EvpMessage *)mess->ReadObject(mess->GetClass());
	CP;

	if(strcmp(msg->getSource(), "readerThread") == 0) {   // From the daqReader!
	    CP;
	    //LOG("JEFF", "Handle message from reader");

	    handleNewEvent(msg);
      
	    EvpMessage m;
	    m.setSource((char *)"serverThread");
	    m.setCmd((char *)"release");
  
	    TMessage mess(kMESS_OBJECT);
	    mess.WriteObject(&m);
	    s->Send(mess);
	    CP;
	}
	else {                                // from a client!
	    CP;
	    //LOG("JEFF", "Handle message from client");
	    handleEvpMessage(s, msg);
	    CP;
	}
    
	delete mess;
	delete msg;
	return;
    }
    CP;
    // Well it must be a JevpPlot from the client!
    if (strcmp(mess->GetClass()->GetName(), "JevpPlot")==0) {
	CP;
	JevpPlot *plot = (JevpPlot *)mess->ReadObject(mess->GetClass());
    
	if(plot->refid != 0) {
	    saveReferencePlot(plot);
	}
	else {
	    LOG(ERR, "Got a JevpPlot from client, but doesn't seem to be a reference plot...");
	}
    
	delete plot;
	delete mess;
	return;
    }

    CP;
    LOG(ERR, "Got invalid message type: %s\n",mess->GetClass()->GetName());
    delete mess;
}  


void JevpServer::parseArgs(int argc, char *argv[])
{
    throttleAlgos = 1;
    isL4 = 0;

    justUpdateDisplayPallete = 0;
    
    throttle_time = .05;

    log_output = RTS_LOG_NET;
    //log_output = RTS_LOG_STDERR;
    log_dest = (char *)"172.16.0.1";
    log_port = 8004;
    log_level = (char *)WARN;
    makepallete = 0;

    rtsLogOutput(log_output);
    rtsLogAddDest(log_dest, log_port);
    rtsLogLevel(log_level);

    clientdatadir =  (char *)"/a/jevp/client";
    ndaqfilenames = 0;
    cdaqfilename = 0;

    for(int i=1;i<argc;i++) {
	if(strcmp(argv[i], "-dd")==0) {
	    i++;
	    displays_fn = argv[i];
	}
	else if (strcmp(argv[i], "-stderr") == 0) {
	    log_output = RTS_LOG_STDERR;
	}
	else if (strcmp(argv[i], "-basedir") == 0) {
	    i++;
	    basedir = argv[i];
	}
	else if (strcmp(argv[i], "-nothrottle") == 0) {
	    throttleAlgos = 0;
	}
	else if (strcmp(argv[i], "-nopdf")==0) {
	    pdfdir = NULL;
	}
	else if (strcmp(argv[i], "-nodb")==0) {
	    nodb = 1;
	}
	else if (strcmp(argv[i], "-db") == 0) {
	    nodb = 0;
	}
	else if (strcmp(argv[i], "-port")==0) {
	    i++;
	    myport = atoi(argv[i]);
	}
	else if (strcmp(argv[i], "-justPallette") == 0) {
	    justUpdateDisplayPallete = 1;
	}
	else if (strcmp(argv[i], "-file")==0) {
	    i++;
	    daqfilename = argv[i];
	}
	else if (strcmp(argv[i], "-log") == 0) {
	    logevent=1;
	}
	else if (strcmp(argv[i], "-pallete") == 0) {
	    makepallete = 1;
	}
	else if (strcmp(argv[i], "-files") == 0) {
	    i++;

	    LOG("JEFF", "files...");

	    while(i<argc && strcmp(argv[i], "-endfiles") != 0) {

		LOG("JEFF", "testing ndaq=%d i=%d arg=%s",i, ndaqfilenames, argv[i]);
		daqfilenames[ndaqfilenames] = argv[i];
		ndaqfilenames++;
		i++;
	    }

	    LOG("JEFF", "ndaqfilenames = %d", ndaqfilenames);
	}
	else if (strcmp(argv[i], "-die")==0) {
	    die = 1;
	}
	else if (strcmp(argv[i], "-production") == 0) {
	    nodb = 0;
	    myport = JEVP_PORT;
	}
	else if (strcmp(argv[i], "-l4production") == 0) {
	    LOG("JEFF", "Using L4");
	    isL4 = 1;
	    log_dest = (char *)"172.17.0.1";
	    nodb = 0;
	    myport = JEVP_PORT;
	    clientdatadir = (char *)"/a/l4jevp/client";
	    basedir = (char *)"/RTScache/conf/l4jevp";
	    pdfdir = (char *)"/a/l4jevp/pdf";
	    refplotdir = (char *)"/a/l4jevp/refplots";
	    rootfiledir = (char *)"/a/l4jevp/rootfiles";
	}
	else if (strcmp(argv[i], "-l4test") == 0) {
	    LOG("JEFF", "Using L4 test");
	    isL4 = 1;
	    nodb = 0;
	    myport = JEVP_PORT+10;
	    clientdatadir = (char *)"/a/l4jevp/client";
	    basedir = (char *)"/RTScache/conf/l4jevp";
	    pdfdir = (char *)"/a/l4jevp/pdf";
	    refplotdir = (char *)"/a/l4jevp/refplots";
	    rootfiledir = (char *)"/a/l4jevp/rootfiles"; 
	    log_output = RTS_LOG_STDERR;
	}
	else if (strcmp(argv[i], "-updatedb")==0) {
	    log_output = RTS_LOG_STDERR;
	    nodb = 0;
	    myport = JEVP_PORT+10;
	    die = 1;
	    log_level = WARN;
	    throttle_time = .005;
	}
	else if (strcmp(argv[i], "-l4updatedb")==0) {
	    nodb = 0;
	    myport = JEVP_PORT+10;
	    die = 1;

	    LOG("JEFF", "Update L4 DB");
	    isL4 = 1;
	    log_dest = (char *)"172.17.0.1";
	    nodb = 0;
      
	    clientdatadir = (char *)"/a/l4jevp/client";
	    basedir = (char *)"/RTScache/conf/l4jevp";
	    pdfdir = (char *)"/a/l4jevp/pdf";
	    refplotdir = (char *)"/a/l4jevp/refplots";
	    rootfiledir = (char *)"/a/l4jevp/rootfiles";
	}
	else if (strcmp(argv[i], "-test")==0) {
	    nodb = 1;
	    log_output = RTS_LOG_STDERR;
	    basedir = (char *)"/RTScache/conf/jevp_test";
	    pdfdir = (char *)"/a/jevp_test/pdf";
	    refplotdir = (char *)"/a/jevp_test/refplots";
	    rootfiledir = (char *)"/a/jevp_test/rootfiles";
	    myport = JEVP_PORT + 10;
	}
	else if (strcmp(argv[i], "-diska")==0) {   // used only to pass to builders on launch...
	    i++;
	    diska = argv[i];
	}
	else {
	    printf("\n\nUsage for %s:  (bad arg %s)\n",argv[0],argv[i]);
	    printf("\t[-dd filename]       for each display definition:\n");
	    printf("\t[-basedir basedir]   config file directory\n");
	    printf("\t[-nodb]\n");
	    printf("\t[-db]    not usually needed, but db usually disabled in reanalysis\n");
	    printf("\t[-port] port]\n");
	    printf("\t[-die]    (exit after end of run..)\n");
	    printf("\t[-file daqfilename]\n");
	    printf("\t[-test]   (set port to %d)\n",myport+10);
	    printf("\t[-production]\n");
	    printf("\t[-diska [/net/a]]  (used to pass to builders on launch)\n");
	    printf("\t[-nothrottle]\n");
	    printf("\t[-log]   (log each event)\n");
	    printf("\n\n");
	    printf("Defaults:  \n");
	    printf("\tbasedir      = '/RTScache/conf'\n");
	    printf("\tdisplay file = 'HistoDefs.txt'\n");
	    printf("\tport         = %d\n", JEVP_PORT);
	    printf("\tuse database!\n");
	    printf("\n\n");
	    exit(0);
	}
    }

    JEVPSERVERport = myport;
     
    LOG(NOTE, "isL4=%d",isL4);
    if(!displays_fn) {
	displays_fn = (char *)"HistoDefs.txt";
    }    
}

int JevpServer::updateDisplayDefs()
{
  char tmp[100];
  sprintf(tmp, "%s/%s", basedir, displays_fn);
  if(displays) delete displays;
  displays = new DisplayFile();
  displays->Read(tmp);

  displays->dump();
  return 0;
}

int JevpServer::init(int port, int argc, char *argv[]) {

  ssocket = new TServerSocket(port,kTRUE,100);
  mon = new JTMonitor();
  mon->Add(ssocket);

  updateDisplayDefs();

  // Create daq reader...
  LOG(DBG, "Reader filename is %s",daqfilename ? daqfilename : "none");
  if(ndaqfilenames) daqfilename = daqfilenames[cdaqfilename];

  rdr = new daqReader(daqfilename);

  if(diska) rdr->setEvpDisk(diska);

  // daqreader resets it?
  rtsLogOutput(log_output);
  rtsLogAddDest(log_dest, log_port);
  rtsLogLevel(log_level);


  //rtsLogOutput(RTS_LOG_STDERR);
  //rtsLogLevel(DBG);
  
  // Create builders...
  if(!isL4) {
    builders.Add(new baseBuilder(this));
    builders.Add(new bbcBuilder(this));
    builders.Add(new daqBuilder(this));
    builders.Add(new bemcBuilder(this));
    builders.Add(new eemcBuilder(this));
    builders.Add(new fpdBuilder(this));
    builders.Add(new hltBuilder(this));
    builders.Add(new l3Builder(this));
    builders.Add(new tofBuilder(this));
    builders.Add(new mtdBuilder(this));
    builders.Add(new tpxBuilder(this));
    builders.Add(new trgBuilder(this));
    builders.Add(new upcBuilder(this));
    builders.Add(new fgtBuilder(this));
    builders.Add(new vpdBuilder(this));
    builders.Add(new fmsBuilder(this));
    builders.Add(new fpsBuilder(this));
    builders.Add(new gmtBuilder(this));
    builders.Add(new pxlBuilder(this));
    builders.Add(new istBuilder(this));
    builders.Add(new ssdBuilder(this));
    builders.Add(new ppBuilder(this));
  }
  else {
    builders.Add(new trgBuilder(this));
    builders.Add(new l4Builder(this));
  }


  TListIter next(&builders);
  JevpPlotSet *curr;
  while((curr = (JevpPlotSet *)next())) {
    LOG(NOTE, "init");
    curr->_initialize(argc, argv);
    curr->clientdatadir = clientdatadir;
    LOG(NOTE, "init done");
  }
  CP;

  debugBuilders(__LINE__);

  return 0;
}  


// returns delay in milliseconds
void JevpServer::handleNewEvent(EvpMessage *m)
{
  
  if(strcmp(m->cmd,"stoprun") == 0) {
    LOG(DBG, "SERVThread: Got stoprun from reader");
    CP;
    if(runStatus.running()) {
      CP;
      performStopRun();

      if(ndaqfilenames) {
	cdaqfilename++;

	if(cdaqfilename < ndaqfilenames) {
	  daqfilename = daqfilenames[cdaqfilename];
	  delete rdr;
	  LOG("JEFF", "Next file is :%s",daqfilename);

	  rdr = new daqReader(daqfilename);
	}
      }

      if(die && (cdaqfilename >= ndaqfilenames)) {
	LOG("JEFF", "die is set, so now exit");
	CP;
	
	ignoreSignals();
	gApplication->Terminate();
	//exit(0);
      }
      
      CP;
    }
	CP;
  }
  else if(strcmp(m->cmd, "readerr") == 0) {
    LOG(ERR, "A read err...");
  }
  else if(strcmp(m->cmd,"newevent") == 0) {
    LOG(DBG, "SERVThread: Got newevent");
    CP;
    JevpPlotSet *curr;
    TListIter next(&builders);
    
    if(rdr->run != (unsigned int)runStatus.run) {
      CP;
      LOG(DBG, "Starting new run #%d  (%d)",rdr->run, runStatus.run);
      performStartRun();
      eventsThisRun = 0;
    }

    eventsThisRun++;
    
    if((eventsThisRun % 100) == 0) LOG(WARN, "Processed %d events this run so far", eventsThisRun);

    // Now we have an event!
    //
    // fill histograms!
    CP;
    while((curr = (JevpPlotSet *)next())) {
      
	//double throttle_time = .05;
      
      if(throttleAlgos) {
	if((curr->processingTime / (double)eventsThisRun) > throttle_time) {
	  LOG(NOTE, "Skipping builder for event %d: %s due to %d ms/event throttle (%lf secs/event : %d of %d so far)",
	      rdr->seq, curr->getPlotSetName(), (int)(throttle_time * 1000), curr->getAverageProcessingTime(), curr->numberOfEventsRun, eventsThisRun);
	  
	  continue;
	}
      }
      
      CP;
      if(logevent) {
	printf("Sending event #%d(%d) to builder: %s  (avg processing time=%lf secs/evt)\n",rdr->seq, rdr->event_number, curr->getPlotSetName(), curr->getAverageProcessingTime());
      }
      
      if(sigsetjmp(env, 1)) {
	  LOG(CAUTION, "Sigsegv in builder: %s.  Disable.  (%s)",curr->getPlotSetName(), curr->getDebugInfo());
	  curr->setDisabled();
	  CP_LEAVE_BUILDER;
      }
      else {
	CP_ENTER_BUILDER(curr->getPlotSetName());
	curr->_event(rdr);
	CP_LEAVE_BUILDER;
      }
      
      CP;
    }
    CP;
  }
  else {
    LOG(ERR, "handleNewEvent got invalid command: %s",m->cmd);
  }


  
}




void JevpServer::handleClient(int delay) {
  TMessage *mess;
  TSocket *s;

  CP;

  // printf("Got a message\n");
  //  LOG(NOTE, "calling sleep");
  //   sleep(1);
  LOG(DBG, "calling select");

  s = mon->Select(delay);
  LOG(DBG, "back from select");
  CP;
  if((long)s <= 0) {
    if(delay > 0) {
      LOG(DBG, "Got a timeout or an error: %d (delay was %d)",s,delay);
    }
    CP;
    return;
  }

  CP;
  LOG(NOTE, "Got a message:  s=%d",s);

  if(s == ssocket) {
    CP;
    TSocket *nsock = ssocket->Accept();
    TInetAddress adr = nsock->GetInetAddress();
    mon->Add(nsock);
  }
  else {
    CP;
    // read...
  
    int ret = s->Recv(mess);

    CP;
    if(ret == 0) {    // Got a disconnection...
      CP;
      LOG(DBG, "Disconnecting a client!");

      mon->Remove(s);
      delete s;
      delete mess;
      CP;
      return;
    }
    
    CP;

    // Handle control messages...
    if(strcmp(mess->GetClass()->GetName(),"EvpMessage")==0) {
      CP;

      EvpMessage *msg = (EvpMessage *)mess->ReadObject(mess->GetClass());
	
      handleEvpMessage(s, msg);
	
      delete msg;
    }
    else if (strcmp(mess->GetClass()->GetName(), "JevpPlot")==0) {
      CP;
      JevpPlot *plot = (JevpPlot *)mess->ReadObject(mess->GetClass());
      
      if(plot->refid != 0) {
	saveReferencePlot(plot);
      }
      else {
	LOG(ERR, "Got a JevpPlot from client, but doesn't seem to be a reference plot...");
      }
      
      delete plot;
    }
    else {
      CP;
      LOG(ERR, "Got invalid message type: %s\n",mess->GetClass()->GetName());
    }

    CP;
    delete(mess);
    CP;
  }    
  CP;
}

void JevpServer::handleEvpMessage(TSocket *s, EvpMessage *msg)
{
  CP;
  if(strcmp(msg->getCmd(), "dump") == 0) {
    CP;
    dump();
  }
  else if(strcmp(msg->getCmd(), "display_desc") == 0) {  // Display Descriptor

    LOG(NOTE, "Got request for display %s", msg->args);
    int ret = displays->setDisplay(msg->args);
    LOG(DBG, "setdisplay returend %d", ret);

    EvpMessage m;
    m.setSource("serv");
    m.setCmd("xml");
    if(!displays) {
      LOG(ERR, "No displays available\n");
      return;
    }
    
    m.setArgs(displays->textBuff);
    CP;

    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&m);
    s->Send(mess);
    LOG(NOTE, "replied to display %s", msg->args);
  }
  else if(strcmp(msg->getCmd(), "GetStatus") == 0) {
    LOG(NOTE, "GetStatus");
    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&runStatus);
    s->Send(mess);
    LOG(NOTE, "Replied to GetStatus");
  }
  else if(strcmp(msg->getCmd(), "ping") == 0) {    
    EvpMessage m;
    m.setSource((char *)"serv");
    m.setCmd((char *)"ping");
    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&m);
    CP;
    s->Send(mess);
  }
  else if(strcmp(msg->getCmd(), "print") == 0) {
    //char printer[100];
    //int tab;
    //int display;

    //sscanf(msg->args, "%s %d %d", printer, &display, &tab);
    //LOG(NOTE, "Request to printing tab %d to printer %s", tab, printer);

    
    writePdf((char *)"/tmp/jevp.pdf", 1);

    gSystem->Exec("/usr/bin/convert /tmp/jevp.pdf /tmp/jevp.ps");
    
  }
  else if(strcmp(msg->getCmd(), "getplot") == 0) {
    LOG(NOTE, "GetPlot");
    CP;
    RtsTimer_root clock;
    clock.record_time();
    handleGetPlot(s,msg->args);
    double t1 = clock.record_time();
    if(t1 > .05) {
      LOG(WARN, "Timing: handleGetPlot(%s) time=%lf",msg->args,t1);
    }
    LOG(NOTE, "Done with GetPlot");
  }
  else if(strcmp(msg->getCmd(), "swaprefs") == 0) {
    CP;
    handleSwapRefs(msg->args);
  }
  else if(strcmp(msg->getCmd(), "deleteplot") == 0) {
    CP;
    char str[256];
    int idx;
    sscanf(msg->args, "%s %d", str, &idx);
    deleteReferencePlot(str,idx);
  }
  else if(strcmp(msg->getCmd(), "getServerTags") == 0) {
    CP;
    EvpMessage m;
    m.setSource((char *)"serv");
    m.setCmd((char *)"getServerTags");
    if(serverTags) {
      LOG(DBG, "server tags are: %s",serverTags);
      m.setArgs(serverTags);
    }
    else {
      LOG(DBG, "No server tags?");
      m.setArgs("");
    }

    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&m);
    s->Send(mess);
  }
  else if(strcmp(msg->getCmd(), "monitor") == 0) {
    CP;
    EvpMessage m;
    m.setSource((char *)"serv");
    m.setCmd((char *)"monitor");

    getMonitorString(msg->args, &m);
    
    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&m);
    CP;
    s->Send(mess);
  }
  else if (strcmp(msg->getCmd(), "launch") == 0) {
    LOG(DBG, "Got launch:  (%s) (%s)", msg->getArgs(), msg->getSource());	


    char *x = (char *)malloc(strlen(msg->getArgs()) + 1);
    strcpy(x, msg->getArgs());
    launchArgs = x;
  }
  else {
    CP;
    LOG(WARN,"Unknown command: %s\n",msg->getCmd());
  }
  CP;
}

void JevpServer::performStartRun()
{
  runStatus.run = rdr->run;

  eventsThisRun = 0;

  LOG("JEFF", "Start run #%d  (mem: %d)",runStatus.run, getMemUse());
  clearForNewRun();

  runStatus.setStatus("running");
}

void JevpServer::writeRootFiles()
{
    char filename[256];
    sprintf(filename, "%s/run_%d.root",rootfiledir, runStatus.run);
  
    LOG(NOTE, "Writing to rootfile: %s",filename);
    TFile *rootfile = new TFile(filename, "recreate");
  
    // Got through all histos...
    JevpPlotSet *curr;
    JevpPlot *currplot = NULL;
    PlotHisto *currhisto = NULL;

    TListIter next(&builders);
  
    while((curr = (JevpPlotSet *)next())) {
    
	TListIter nextplot(&curr->plots);
  
	while((currplot = (JevpPlot *)nextplot())) {


	    TListIter nexthisto(&currplot->histos);

	    while((currhisto = (PlotHisto *)nexthisto())) {
		currhisto->histo->Write();
	    }
	}
    }
  
    rootfile->Close();
    delete rootfile;
}

void JevpServer::justUpdatePallete() {
  JevpPlotSet *curr;
  TListIter next(&builders);

  freePallete();

  next.Reset();
  while((curr = (JevpPlotSet *)next())) {
    printf("Adding plot to pallete: builder=%s:\n",curr->getPlotSetName());
    
    JevpPlot *currplot;
    TListIter nextplot(&curr->plots);
    while((currplot = (JevpPlot *)nextplot())) { 
      printf("              : plot = %s\n",currplot->GetPlotName());
      addToPallete(currplot);
    }
  }

  // Now write the file out!
  char fn[256];
  sprintf(fn, "%s/%s", basedir, displays_fn);

  unlink(fn);
  if(displays->Write(fn) < 0) {
    LOG(ERR, "Error writing display file %s",fn);
  }

}

void JevpServer::performStopRun()
{
  LOG("JEFF", "Got run stop for run #%d (%d displays to write out)",runStatus.run, displays->nDisplays());


  JevpPlotSet *curr;
  TListIter next(&builders);
  
  while((curr = (JevpPlotSet *)next())) {
    CP;
    LOG("JEFF", "End of run report for %s: (%lf secs/event : %d of %d analyzed)",
	curr->getPlotSetName(), curr->getAverageProcessingTime(), curr->numberOfEventsRun, eventsThisRun);
    CP;

    curr->stoprun(rdr);
    CP;

    continue;
  }

  
  eventsThisRun = 0;


  // Write out the pdfs for all displays...
  displays->setServerTags(serverTags ? serverTags : "");
  displays->ignoreServerTags = 0;

  for(int i=0;i<displays->nDisplays();i++) {
    LOG(NOTE,"Writing pdf for display %d, run %d",i,runStatus.run);
    CP;
    writeRunPdf(i, runStatus.run);
    CP;
  }

  writeRootFiles();

  // Update the palletes and write out xml again
  char fn[256];
  sprintf(fn, "%s/%s", basedir, displays_fn);

  LOG(DBG, "fn=%s",fn);
  CP;

  // Add any new plots to the pallet...
  freePallete();

  next.Reset();
  while((curr = (JevpPlotSet *)next())) {

    LOG(DBG, "Adding plot to pallete: builder=%s",curr->getPlotSetName());

    JevpPlot *currplot;
    TListIter nextplot(&curr->plots);
    while((currplot = (JevpPlot *)nextplot())) { 
      LOG(DBG, "                    : plot = %s",currplot->GetPlotName());
      addToPallete(currplot);
    }
  }
  CP;

  LOG(NOTE, "Writing display file...%s",fn);
  unlink(fn);
  if(displays->Write(fn) < 0) {
    LOG(ERR, "Error writing display file %s",fn);
  }

  char *args[4];
  args[0] = "OnlTools/Jevp/archiveHistoDefs.pl";
  args[1] = basedir;
  args[2] = displays_fn;
  args[3] = NULL;

  execScript("OnlTools/Jevp/archiveHistoDefs.pl", args);

  runStatus.setStatus("stopped");
}

void JevpServer::clearForNewRun()
{
  // Delete all from histogram list
  // First free the actual histo, then remove the link...
  LOG(NOTE, "Clear for new run  #%d",runStatus.run);

  TListIter next(&builders);

  JevpPlotSet *curr;
  while((curr = (JevpPlotSet *)next())) {

    LOG(DBG, "Send startrun for: %s", curr->getPlotSetName());
    curr->_startrun(rdr);
  }

  if(serverTags) {
    free(serverTags);
    serverTags = NULL;
  }
}


JevpPlot *JevpServer::getPlot(char *name) {
    RtsTimer_root clock;
    clock.record_time();
    int nexamined=0;

    if(strcmp(name, "serv_JevpSummary") == 0) {
	return getJevpSummaryPlot();
    }

    JevpPlotSet *curr;
    TListIter next(&builders);
  
    JevpPlot *currplot = NULL;

    while((curr = (JevpPlotSet *)next())) {
    
	char *ps_name = curr->getPlotSetName();
	int len = strlen(ps_name);

	if(strncmp(name,ps_name,len) != 0) continue;

	currplot = curr->getPlot(name);
	//LOG("JEFF", "name: %s currplot: %p", name, currplot);
	if(currplot) break;

	/*   
	     TListIter nextplot(&curr->plots);
  
	     while((currplot = (JevpPlot *)nextplot())) {

	     LOG(DBG, "getPlot():  checking %s vs %s",name,currplot->GetPlotName());
	     nexamined++;

	     if(strcmp(currplot->GetPlotName(), name) == 0) {
	     goto done;
	     }
	     }
	*/
    }

    //done:

    double t1 = clock.record_time();
    LOG(DBG, "Ethernet: getPlot(%s) %lf",name,t1);

    return currplot;
}


void JevpServer::handleGetPlot(TSocket *s, char *argstring) 
{
  RtsTimer_root clock;
  double t1=0,t2=0,t3=0,t4=0;
  clock.record_time();

  JevpPlot *plot=NULL;
  char refidstr[20];
  char runidstr[20];
  char plotname[80];

  LOG(DBG,"argstring is (%s)\n",argstring);
  if(!getParamFromString(plotname, argstring)) {
    LOG(ERR,"No plot indicated in getplot?\n");
    return;
  }
 
  LOG(DBG,"Plotname is %s\n",plotname);

  t1 = clock.record_time();

  if(getParamFromString(refidstr, argstring, (char *)"refid")) {
    char fn[256];
    sprintf(fn, "%s/REF.%s.%d.root", DEFAULT_REF_PLOT_DIR, plotname, atoi(refidstr));
      
    LOG(DBG,"Reading [%s] from file %s\n",plotname, fn);

    TFile *f1 = new TFile(fn);
    if(!f1) {
      LOG(ERR,"Error opening file: %s",fn);
      plot = NULL;
    }
    else {
      //f1->GetObject(plotname, plot);
      // If JevpSummary, build a new one first...
      f1->GetObject("JevpPlot",plot);
      f1->Close();

      if(plot) {
	LOG(DBG,"Got plot.....xxx\n");
	plot->refid = atoi(refidstr);
      }
      else {
	LOG(WARN,"Didn't get plot %s\n",plotname);
      }
    }
  }
  else if (getParamFromString(runidstr, argstring, (char *)"run")) {    
    char fn[256];
    sprintf(fn, "%s/%d.root",EVP_SAVEPLOT_DIR, atoi(runidstr));

    TFile *f1 = new TFile(fn);
    if(!f1) {
      LOG(ERR,"Error opening file: %s",fn);
      plot = NULL;
    } 
    else {
      f1->GetObject(plotname, plot);
      f1->Close();
    }
  }
  else {
    LOG(DBG,"getplot..%s\n", plotname);

//     if(strcmp(plotname, "serv_JevpSummary") == 0) {
//       plot = getJevpSummaryPlot();
//     }
//     else {
    plot = getPlot(plotname);
    //    }
  }

  t2 = clock.record_time();
    
  if(!plot) {
    char tmp[100];
    sprintf(tmp, "No plot %s",plotname);
    EvpMessage m;
    m.setSource((char *)"serv");
    m.setCmd((char *)"noplot");
    m.setArgs(tmp);
    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&m);
    
    int ret = s->Send(mess);
    t3=clock.record_time();
    LOG(DBG, "sent (errmess) %d bytes",ret);
  } else {
    clock.record_time();
    TMessage mess(kMESS_OBJECT);
    t1=clock.record_time();
    mess.WriteObject(plot);
    t2=clock.record_time();
    int ret = s->Send(mess);
    t4=clock.record_time();
    LOG(DBG, "Sent (plot) %d bytes",ret);
  }

  LOG(DBG, "getplot %lf %lf %lf %lf",t1,t2,t3,t4);

}

JevpPlot *JevpServer::getJevpSummaryPlot()
{
  if(jevpSummaryPlot) {
    delete jevpSummaryPlot;
    jevpSummaryPlot = NULL;
  }

  debugBuilders(__LINE__);

  CP;
  jevpSummaryPlot = new JevpPlot();
  jevpSummaryPlot->needsdata = 0;
  jevpSummaryPlot->setParent((char *)"serv");
  TH1I *h = new TH1I("JevpSummary", "JevpSummary", 64,0,63);
  //h->GetXaxis()->SetAxisColor(kWhite);
  h->GetXaxis()->SetTickLength(0);
  h->GetXaxis()->SetLabelColor(kWhite);
  //h->GetYaxis()->SetAxisColor(kWhite);
  h->GetYaxis()->SetTickLength(0);
  h->GetYaxis()->SetLabelColor(kWhite);
  //h->SetLineColor(kWhite);
  //h->SetAxisColor(kWhite);
  //h->SetLabelColor(kWhite);

  jevpSummaryPlot->addHisto(h);

  jevpSummaryPlot->setOptStat(0);
  jevpSummaryPlot->gridx = 0;
  jevpSummaryPlot->gridy = 0;
  
  
  CP;
  JLatex *l;
  
  
  int i = 0;
  char tmp[512];

  sprintf(tmp,"Run #%d: (%s for %ld seconds)",runStatus.run, runStatus.status, time(NULL) - runStatus.timeOfLastChange);
  l = new JLatex(2, liney(i++), tmp);
  i++;
  l->SetTextSize(.05);
  jevpSummaryPlot->addElement(l);

  sprintf(tmp, "Tags:   %s", serverTags);
  l = new JLatex(2, liney(i++), tmp);
  i++;
  l->SetTextSize(.035);
  jevpSummaryPlot->addElement(l);

  CP;
  // Now show builders...
  TListIter next(&builders);
  JevpPlotSet *obj;
  int n=0;

  debugBuilders(__LINE__);

  CP;
  while((obj = (JevpPlotSet *)next())) {
    LOG(DBG, "object");
    LOG(DBG, "name=%s",obj->getPlotSetName());
    BuilderStatus *curr = &obj->builderStatus;

    n++;
    sprintf(tmp, "builder %15s: (events %d, avgtime %06.4lf)",
	    curr->name, curr->events, obj->getAverageProcessingTime());
    
    LOG(DBG, "here %s",tmp);
    l = new JLatex(2, liney(i++), tmp);
    l->SetTextSize(.035);

    LOG(DBG, "Here");
    jevpSummaryPlot->addElement(l); 

    LOG(DBG, "HEre");
  }
  
  CP;
  if(n == 0) {
    sprintf(tmp,"There are no builders");
    l = new JLatex(2, liney(i++), tmp);
    l->SetTextSize(.035);
    jevpSummaryPlot->addElement(l);
  }
  CP;

  return jevpSummaryPlot;
}

void JevpServer::handleSwapRefs(char *name)
{
  char name1[256];
  char name2[256];
  char tmp[256];
  char base[256];
  int idx1, idx2;
  sscanf(name, "%s %d %d", base, &idx1, &idx2);
  
  LOG(DBG,"Swapping %s (%d <--> %d)\n",base,idx1,idx2);
  sprintf(name1, "%s/REF.%s.%d.root",refplotdir, base, idx1);
  sprintf(name2, "%s/REF.%s.%d.root",refplotdir, base, idx2);

  sprintf(tmp, "%s/REF.%s.root.tmp",refplotdir, base);
  rename(name1, tmp);
  rename(name2, name1);
  rename(tmp, name2);
}

void JevpServer::writeRunPdf(int display, int run)
{
  RtsTimer_root pdfclock;
  pdfclock.record_time();

  if(pdfdir == NULL) return;

  int ret = displays->setDisplay(display);
  if(ret < 0) {
    LOG(ERR, "Can't set display to %d",display);
    return;
  }
  double t = pdfclock.record_time();
  LOG(NOTE, "write PDF[%d:%s]:  setdisplays took %lf",display,displays->displayRoot->name,t);
  
  char filename[256];
  sprintf(filename, "%s/%s_%d.pdf",pdfdir, displays->displayRoot->name, run);
  CP;
       
  writePdf(filename, 1);

  t = pdfclock.record_time();
  LOG(NOTE, "write PDF[%d:%s]:  writepdf took %lf",display,displays->displayRoot->name,t);
  CP;

  // Save it in the database...
  if(nodb != 1) {
    LOG("JEFF", "Writing PDF file: %s to DB",filename);

    char *args[5];

    args[0] = (char *)"WritePDFToDB";
    char tmp[10];
    sprintf(tmp, "%d", run);
    args[1] = tmp;
    args[2] = filename;
    args[3] = displays->displayRoot->name;
    args[4] = NULL;

    //int ret = char((execScript *)"WritePDFToDB",args);
    int ret = execScript("WritePDFToDB", args, 0);
    LOG(WARN, "Wrote PDF file to DB: %s (ret=%d)", filename, ret);
    
    t = pdfclock.record_time();
    LOG(NOTE, "write PDF[%d:%s]:  writepdfdb took %lf (no wait!)",display,displays->displayRoot->name,t);
  }
}

void JevpServer::writePdf(char *filename, int combo_index)
{
  if(pdfdir == NULL) return;

  LOG(DBG, "Writing pdf: %s index=%d",filename,combo_index);
  DisplayNode *root = displays->getTab(combo_index);

  if(combo_index == 0) {
    LOG(DBG, "disproot = 0x%x root = 0x%x", displays->displayRoot, root);
    root = displays->displayRoot;
  }


  //   char filename[256];
  //   sprintf(filename, "%s/%s_%d.pdf", pdfdir, displays->displayRoot->name, run);

  LOG(DBG, "writeNodePdf root: %s",filename);

  PdfIndex index;
  writeNodePdf(root, &index, NULL, filename, 1, 0);
  
  LOG(DBG, "write endfilename");

  // Now a summary....
  char endfilename[256];
  strcpy(endfilename, filename);
  strcat(endfilename, ")");
  TCanvas summary("c2");
  summary.Print(endfilename, "pdf,Portrait");

  
  CP;
  // Index the file...
  char indexedfilename[256];
  strcpy(indexedfilename, filename);
  // strcat(indexedfilename, ".idx");
  index.CreateIndexedFile(filename, indexedfilename);

  CP;
}

int JevpServer::writeNodePdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page, int nosibs)
{
  LOG(NOTE, "Checking node %s against server tags %s", node->name, serverTags);

  int npages = 0;

  if(!node->matchTags(serverTags)) {
    LOG(NOTE, "node %s does not match tags %s", node->name, serverTags);

    // But, handle siblings!   
    if(node->next && !nosibs) {
      npages += writeNodePdf(node->next, index, prevIndexEntry, filename, page, 0);
    }
    return npages;
  }

  if(node->leaf) {   // We are writing histograms...
      LOG(NOTE, "leaf");
    writeHistogramLeavesPdf(node, index, prevIndexEntry, filename, page);
    return 1;
  }
  else {   // We are just writing index entries
    // are we the child?
      LOG(NOTE, "name");

    index_entry *currIndexEntry;
    if(node->prev == NULL) {
      currIndexEntry = index->add_child(prevIndexEntry, node->name, page, 0);
    }
    else {
      currIndexEntry = index->add_sibling(prevIndexEntry, node->name, page, 0);
    }
    
    if(node->child) {
      npages += writeNodePdf(node->child, index, currIndexEntry, filename, page, 0);
    }
    
    if(node->next && !nosibs) {
      npages += writeNodePdf(node->next, index, currIndexEntry, filename, page + npages, 0);
    }

    return npages;
  }
}    



// If page = 1 prints out start tag --> "filename("
// But assumes a summary follows, so there is no end tag --> "filename)"
//
int JevpServer::writeHistogramLeavesPdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page)
{
  RtsTimer_root clk;
  clk.record_time();

  LOG(DBG, "Write histogram leaves: %s",node->name);

  CP;
  if((node->prev != NULL) || (!node->leaf)) {
    LOG(ERR, "Shouldn't happen: prev=0x%x leaf=%d", node->prev, node->leaf);
  }

  CP;
  // create index first
  index_entry *cindex = index->add_child(prevIndexEntry, node->name, page, 0);
  DisplayNode *cnode = node->next;
  while(cnode) {
    cindex = index->add_sibling(cindex, cnode->name, page, 0);
    cnode = cnode->next;
  }
  CP;
  // Now draw histograms...
  gStyle->SetCanvasColor(19);
  TCanvas *c1 = new TCanvas("c1","c1",1000,800);

  char fname[256];
  strcpy(fname, filename);
  if(page == 1) {
    strcat(fname, "(");
  }
  CP;
  int wide = node->getIntParentProperty("wide");
  if(wide < 0) wide = 1;
  int deep = node->getIntParentProperty("deep");
  if(deep < 0) deep = 1;
  int scaley = node->getIntParentProperty("scaley");
  if(scaley <= 0) scaley = 0;
  CP;
  c1->Clear();
  c1->Divide(wide, deep);
  int pad = 1;
  CP;
  if(scaley) {
    double ymax = -999999;
    cnode = node;
    while(cnode) {

      LOG(DBG, "cnode->name = %s", cnode->name);
      JevpPlot *plot = getPlot(cnode->name);
      if(plot) {
	LOG(DBG, "got plot 0x%x",plot);
	double my = plot->getMaxY();
	if(my > ymax) ymax = my;
      }
      cnode = cnode->next;
    }
    CP;
    
    //printf("Got scaley...  Setting max value to ymax=%lf\n",ymax*1.1);
    cnode = node;
    while(cnode) {
      JevpPlot *plot = getPlot(cnode->name);
      if(plot) {
	if(plot->logy) {
	  plot->setMaxY(ymax * 2);
	}
	else {
	  plot->setMaxY(ymax * 1.1);
	}
      }
      cnode = cnode->next;
    }
  }
  CP;

  cnode = node;
  while(cnode) {
    c1->cd(pad);
    CP;

    LOG(DBG, "Plotting %s on page %d / pad %d",cnode->name, page, pad);

    JevpPlot *plot = NULL;
    //if(strcmp(cnode->name, "serv_JevpSummary") == 0) {
    // plot = getJevpSummaryPlot();
    //}
    //else {
    CP;
    plot = getPlot(cnode->name);
    CP;
    //}

    if(plot) {
      CP;
      LOG(DBG, "Found plot %s",cnode->name);
      CP;
      plot->draw();
      CP;
    }
    else {
      CP;
      LOG(DBG, "Can't find plot %s",cnode->name);
      CP;
      DrawCrossOfDeath(cnode->name);
      CP;
    }

    cnode = cnode->next;
    pad++;
  }
  CP;
  while(pad <= wide*deep) {
    c1->cd(pad);
    TLatex *x = new TLatex(.5,.5," ");
    x->Draw();
    //gPad->Draw();
    // printf("Drawing jeff %d\n",pad);
    pad++;
  }

  double t1 = clk.record_time();
  CP;
  c1->Print(fname, "pdf,Portrait");

  double t2 = clk.record_time();

  LOG(DBG, "Write histogram leaves: %s (%lf/%lf)",node->name,t1,t2);

  delete c1;
  return 1;
}


int JevpServer::getMaxRef(char *name)
{
  int maxid = 0;
  struct dirent *dirent;

  DIR *dir = opendir(refplotdir);
  if(dir == NULL) {
    LOG(ERR,"Error opening dir (%s)\n", refplotdir);
    return -1;
  }

  char basename[256];
  sprintf(basename, "REF.%s.", name);
  
  while((dirent = readdir(dir)) != NULL) {
    if(memcmp(basename, dirent->d_name, strlen(basename)) == 0) {
      char *tmp = dirent->d_name;
      tmp += strlen(basename);

      int id = atoi(tmp);     
      if(id > maxid) maxid = id;
    }
  }
  
  closedir(dir);
  return maxid;
}

void JevpServer::shiftRefPlotsUp(char *name, int first)
{
  int max = getMaxRef(name);
  for(int i=max;i>=first;i--) {
    char dst[256];
    char src[256];
    sprintf(dst, "%s/REF.%s.%d.root",refplotdir,name,i+1);
    sprintf(src, "%s/REF.%s.%d.root",refplotdir,name,i);   
    LOG(DBG,"Renaming file: %s --> %s\n", src, dst);
    rename(src, dst);
  }
}

void JevpServer::shiftRefPlotsDown(char *name, int first)
{
  int max = getMaxRef(name);
  for(int i=first;i<=max;i++) {
    char dst[256];
    char src[256];
    sprintf(dst, "%s/REF.%s.%d.root",refplotdir,name,i-1);
    sprintf(src, "%s/REF.%s.%d.root",refplotdir,name,i);

    LOG(DBG,"Renaming file: %s --> %s\n", src, dst);
    rename(src, dst);
  }
}

void JevpServer::deleteReferencePlot(char *name, int refid) {
  char filename[256];

  sprintf(filename, "%s/REF.%s.%d.root",refplotdir,name,refid);
  LOG(DBG,"Deleting file: %s\n", filename);
  unlink(filename);
  shiftRefPlotsDown(name,refid+1);
}

void JevpServer::saveReferencePlot(JevpPlot *plot) {

  LOG(DBG,"save refplot");

  char plotname[256];

  if(plot->refid > 0) {
    shiftRefPlotsUp(plot->GetPlotName(), plot->refid);
  }


  LOG(DBG, "refplot %s %d",plot->GetPlotName(), plot->refid);
  
  
  sprintf(plotname, "%s/REF.%s.%d.root",refplotdir,plot->GetPlotName(), plot->refid);

  LOG(DBG, "plotname = %s", plotname);

  // Now actually save plot to the file plotname...
  TFile f(plotname, "new");
  plot->Write();
  f.Close();
}

void JevpServer::addServerTag(char *tag)
{
  char tg[100];
  sprintf(tg, "|%s|",tag);

  if(serverTags == NULL) {
    serverTags = (char *)malloc(strlen(tag)+2);
    strcpy(serverTags, "|");
    strcat(serverTags, tag);
    strcat(serverTags, "|");
    return;
  }

  if(strstr(serverTags, tg)) return;
  
  char *ntag = (char *)malloc(strlen(serverTags) + strlen(tag) + 2);
  strcpy(ntag, serverTags);
  strcat(ntag, tag);
  strcat(ntag, "|");

  free(serverTags);
  serverTags = ntag;
}


// tags delimeted by "|"
void JevpServer::addServerTags(char *tags)
{
  LOG(DBG, "Adding tag: %s",tags);

  char *tmp = (char *)malloc(strlen(tags)+1);
  strcpy(tmp, tags);
  
  if(tmp[0] != '|') {
    LOG(ERR, "Bad tag string: %s",tags);
    free(tmp);
    return;
  }

  char *t = strtok(tmp, "|");
  while(t) {
    addServerTag(t);
    t = strtok(NULL, "|");
  }
  
  LOG(DBG, "server tags are: %s",serverTags);
  free(tmp);
}


int JevpServer::execScript(const char *name, char *args[], int waitforreturn)
{
  CP;
  pid_t pid = fork();

  if(pid == -1) {
    LOG(CRIT, "Error spawning script: %s (%s)",name, strerror(errno),0,0,0);
    return 1;
  }

  if(pid == 0) {
    for(int i=0;;i++) {
      if(args[i] == NULL) break;
      LOG(NOTE, "args[%d] = %s",i,args[i]);
    }
    
    int ret = execvp(name,args);
    if(ret < 0) {
      char buff[100];
      LOG(CRIT, "Error spawning script: %s (%s)  (%s)",name, strerror(errno),getcwd(buff,100));
      return 1;
    }
  }

  CP;
  if(!waitforreturn) return 0;
  CP;

  // Wait for child to return....
  int stat=0;
  do {
    waitpid(pid,&stat,0);
  } while(WIFEXITED(stat) == 0);

  return WEXITSTATUS(stat);
}

DisplayNode *JevpServer::getPalleteNode()
{
  if(!displays) return NULL;
  if(!displays->root) return NULL;
  DisplayNode *palleteNode = displays->root->child;
  
  CP;

  while(palleteNode) {
    if(strcmp(palleteNode->name, "pallete") == 0) {
      return palleteNode;
    }
    palleteNode = palleteNode->next;
  }
  return NULL;
}

void JevpServer::freePallete()
{
  CP;
  DisplayNode *palleteNode = getPalleteNode();
  CP;
  if(palleteNode) {
    CP;
    palleteNode->freeChildren();
    CP;
  }
}

// This function actually checks if already in pallete
// if not, adds....
void JevpServer::addToPallete(JevpPlot *plot)
{
  char *builder = plot->getParent();
  char *name = plot->GetPlotName();

  DisplayNode *palleteNode = getPalleteNode();

  CP;
  if(!palleteNode) {
    LOG(ERR, "No pallete found!");
    return;
  }
  CP;

  // Look for builder...
  DisplayNode *builderNode = palleteNode->findChild(builder);

  CP;
  if(!builderNode) {
    CP;
    builderNode = new DisplayNode();
    builderNode->setName(builder);
    palleteNode->insertChildAlpha(builderNode);
  }
  CP;
  // Look for plot...

  DisplayNode *plotNode = builderNode->findChild(name);

  CP;
  if(plotNode) {
    CP;
    LOG(DBG, "We already have a pallete entry for %s:%s",builder,name);
  }
  else {
    CP;
    // The plot was not found... insert it
    plotNode = new DisplayNode();
    plotNode->setName(name);
    plotNode->leaf = 1;
    builderNode->insertChildAlpha(plotNode);
  }
  CP;
}


// Parse a string of the form
// "defaultparam param1=x param2=y"
char *JevpServer::getParamFromString(char *dest, char *source, char *param)
{
  char *tmp = dest;
  char *str = source;

  // Find the "param=" and position directly after it...
  if(param != NULL) {
    str = strstr(source, param);
    if(!str) return NULL;

    str += strlen(param);
    if(*str != '=') {
      return NULL;
    }
    str++;
  }
  
  // copy till semicolon or end...
  while((*str != '\0') && (*str != ';')) {
    *tmp = *str;
    tmp++;
    str++;
  }
  *tmp = '\0';
  return dest;  
}


void JevpServer::DrawCrossOfDeath(char *str)
{
  TLine* a = new TLine(0.,0.,1.,1.);
  TLine* b = new TLine(0.,1.,1.,0.);
  TText* t = new TText(0.5,0.5,str);

//   // This is how we free the memory...
  a->SetBit(kCanDelete);
  b->SetBit(kCanDelete);
  t->SetBit(kCanDelete);
//   screen->addPlot(a);
//   screen->addPlot(b);
//   screen->addPlot(t);

  a->SetLineColor(2);
  b->SetLineColor(2);
  t->SetTextColor(3);
  t->SetTextAlign(22);

  // Already cd()'d to proper pad...
  a->Draw();
  b->Draw();
  t->Draw();

  //delete a;
  //delete b;
  //delete t;
  // gCanvas->Update();
  //cout << __PRETTY_FUNCTION__ << endl;
  return;

}


double JevpServer::liney(double x)
{
  return 1.0 - (x+5.0)/25.0;
}




// Handle the reader get() in a separate thread in order to 
// get true asychronous data.
//
// The problem is that rdr->get() takes a full .1 sec if there is no event
// and this leads to significant delays when many plots are read...
// each control command has a rdr->get() between it...

void readerThreadSend(TSocket *socket, char *cmd)
{
  EvpMessage m;
  m.setSource((char *)"readerThread");
  m.setCmd(cmd);
  
  TMessage mess(kMESS_OBJECT);
  mess.WriteObject(&m);
  socket->Send(mess);
}

void readerThreadWait(TSocket *socket)
{
  TMessage *mess;
  socket->Recv(mess);
  delete mess;

  if(serv.launchArgs) {

    LOG(NOTE, "Got launchArgs!");
    // We need to change the rdr...
    delete serv.rdr;
    serv.rdr = new daqReader(serv.launchArgs);

    free(serv.launchArgs);
    serv.launchArgs = NULL;
  }
}

void *JEVPSERVERreaderThread(void *)
{
  // First connect a socket to myself!

  TSocket *socket = new TSocket("localhost.localdomain", JEVPSERVERport);
  if(!socket) {
    LOG(CRIT, "Can not connect to my own socket!");
    exit(0);
  }

  // Now, the rule is that I attempt to get an event.   
  // Once I have an event, then I send a message to the server
  // via the socket.   I then wait for a response from the server before I 
  // next ask the reader for an event!

  int nevts = 0;

  for(;;) {
    
    usleep(100);  // otherwise we can starve out clients...

    char *ret = serv.rdr->get(0, EVP_TYPE_ANY);
    
    // Obviously some problem, what is it!
    if(ret == NULL) {
      switch(serv.rdr->status) {
      case EVP_STAT_OK:
	LOG(DBG, "EVP reader burped a bit...");
	continue;
      
      case EVP_STAT_EOR:
	LOG(DBG, "RDRThread: End of the run!");
	readerThreadSend(socket, "stoprun");
	readerThreadWait(socket);
	nevts = 0;
	continue;
	
      case EVP_STAT_EVT:
      case EVP_STAT_CRIT:
      default:

	LOG(ERR, "Problem reading event:  perhaps the file is bad?");
	sleep(1);
	readerThreadSend(socket, "readerr");
	readerThreadWait(socket);
	continue;
	
      }
    }
    
    if(serv.rdr->status) {
      LOG(ERR, "Bad status on read?  rdr->status=%d",serv.rdr->status);
      continue;
    }

    nevts++;
    if(serv.makepallete) {
      if(nevts > 1) {
	nevts = 0;
	readerThreadSend(socket, "stoprun");
	readerThreadWait(socket);
	continue;
      }
    }

    LOG(DBG, "RDRThread: Sending newevent to JevpServer: #%d run %d",serv.rdr->event_number,serv.rdr->run);
    readerThreadSend(socket, "newevent");
    LOG(DBG, "RDRThread: Waiting for JevpServer");
    readerThreadWait(socket);
    LOG(DBG, "RDRThread: Trying to read a new event...");
  }
  
  return NULL;
}
