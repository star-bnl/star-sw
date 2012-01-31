#include <unistd.h>
#include "JevpPlotSet.h"
#include <TCanvas.h>
#include <rtsLog.h>
#include <TClass.h>
#include <TMessage.h>
#include "EvpMessage.h"
#include "Jevp/StJevpServer/EvpConstants.h"
#include <PDFUtil/PdfIndex.hh>
#include <RTS/include/SUNRT/clockClass.h>
#include <signal.h>
#include <RTS/include/rtsLog.h>
#include <TSystem.h>
#include "StEvent/StTriggerData2009.h"
#include "StEvent/StTriggerData2012.h"
#include "PdfFileBuilder.h"

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_READER/daq_det.h>

#define NUM_INTERNALPLOTS 3

ClassImp(JevpPlotSet);

static int call_line_number = 0;
static int line_number = 0;
#define CPC call_line_number = __LINE__
#define CP line_number = __LINE__

char myname[256];

static void sigHandler(int arg, siginfo_t *sig, void *v)
{
  static char str[255];
  
  sprintf(str,"Signal %d: shutting down %s! (callline=%d line=%d)", arg, myname, call_line_number, line_number);
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



JevpPlotSet::JevpPlotSet(JevpServer *server)
{
  CP;
  parent = server;
  buildxml = NULL;
  hello_cmds = (char *)"client";
  diska = NULL;
  daqfile = NULL;
  pdf = NULL;
  loglevel = NULL;
  current_run = -1;
  update_time = 5;
  confdatadir = (char *)"/RTScache/conf/jevp";
  clientdatadir = (char *)DEFAULT_CLIENTDATADIR;
  plotsetname = (char *)"def_plotset";
  builderStatus.setStatus("stopped");
  pause = 0;
  xml = NULL;

  processingTimer = new RtsTimer_root();
  processingTime = 0;
  numberOfEventsRun = 0;

  servertags[0] = '\0';
  CP;
}

int JevpPlotSet::addPlot(JevpPlot *hist)
{
  CP;
  hist->setParent(plotsetname);
  CP;
  if(plots.FindObject(hist)) {
    CP;
    LOG(CRIT,"Can't add existing histogram: %s",hist->GetPlotName());
    exit(0);
  }
  CP;
  plots.Add(hist);
  CP;
  return 0;
}

JevpPlot *JevpPlotSet::getPlot(char *name)
{
  CP;
  JevpPlot *curr = (JevpPlot *)plots.First();
  CP;

  while(curr) {
    CP;
    if(strcmp(curr->GetPlotName(), name) == 0) {
      return curr;
    }
    CP;
    curr = (JevpPlot *)plots.After(curr);
    CP;
  }
  
  CP;
  return NULL;
}

void JevpPlotSet::removePlot(char *name)
{
  CP;
  JevpPlot *curr = (JevpPlot *)plots.First();
  CP;
  while(curr) {
    CP;
    if(strcmp(curr->GetPlotName(), name) == 0) {
      CP;
      plots.Remove(curr);
      CP;
      return;
    }
    CP;
    curr = (JevpPlot *)plots.After(curr);
    CP;
  }

  CP;
  return;
}

int JevpPlotSet::getNumberOfPlots()
{
  int i=0;
  CP;
  JevpPlot *curr = (JevpPlot *)plots.First();

  while(curr) {
    i++;
    curr = (JevpPlot *)plots.After(curr);
  }

  CP;
  return i;
}

void JevpPlotSet::resetAllPlots()
{
  JevpPlot *curr = (JevpPlot *)plots.First();
  while(curr) {
    curr->reset();
    curr = (JevpPlot *)plots.After(curr);
  }
}

JevpPlot *JevpPlotSet::getPlotByIndex(int i)
{
  CP;
  i += NUM_INTERNALPLOTS;
  int idx=0;

  JevpPlot *curr = (JevpPlot *)plots.First();

  CP;

  while(curr) {
    if(i == idx) return curr;
    idx++;
    curr = (JevpPlot *)plots.After(curr);
  }

  CP;
  return NULL;
}

void JevpPlotSet::dump()
{
  JevpPlot *curr = (JevpPlot *)plots.First();

  int i=0;
  while(curr) {
    LOG(DBG,"hist[%d] = %s\n",i,curr->GetPlotName());
    i++;
    curr = (JevpPlot *)plots.After(curr);
  }
}


void JevpPlotSet::_initialize(int argc, char *argv[])
{
  char tmp[100];
  TH1 *h;
  PlotHisto *ph;

  LOG("JEFF", "Initializing %sBuilder: pid=%d file=%s",getPlotSetName(), (int)getpid(), daqfile ? daqfile : "live");
  CP;

  builderStatus.setName(getPlotSetName());

  plotEvtsByTrigger = new JevpPlot();
  sprintf(tmp, "%s_EvtsByTrigger", getPlotSetName());
  h = new TH1I(tmp,tmp,64,0,63);
  ph = new PlotHisto();
  ph->histo = h;
  plotEvtsByTrigger->addHisto(ph);
  plotEvtsByTrigger->logy=1;
  addPlot(plotEvtsByTrigger);

  CP;
  plotTimeByTrigger = new JevpPlot();
  sprintf(tmp, "%s_TimeByTrigger", getPlotSetName());
  h = new TH1F(tmp,tmp,64,0,63);
  ph = new PlotHisto();
  ph->histo = h;
  plotTimeByTrigger->addHisto(ph);
  //plotTimeByTrigger->logy=1;
  addPlot(plotTimeByTrigger);
  
  CP;
  plotTime = new JevpPlot();
  sprintf(tmp, "%s_Time", getPlotSetName());
  h = new TH1F(tmp,tmp,100,0,.000000001);
  h->SetBit(TH1::kCanRebin);
  ph = new PlotHisto();
  ph->histo = h;
  plotTime->addHisto(ph);
  addPlot(plotTime);
  plotTime->logy=1;
  // plotTime->logx=1;

  CPC;
  initialize(argc, argv);
  CPC;

  if(buildxml) {
    buildTheXml();
  }
  CP;
}

void JevpPlotSet::initialize(int argc, char *argv[])
{
}

void JevpPlotSet::_startrun(daqReader *rdr)
{
  CP;
  strcpy(myname, plotsetname);

  processingTime = 0;
  numberOfEventsRun = 0;

  CP;
  memset((char *)n_pertrg, 0, sizeof(n_pertrg));
  memset((char *)avg_time_pertrg, 0, sizeof(avg_time_pertrg));

  CP;
  run = rdr->run;
  builderStatus.setStatus("running");
  

  CP;
  builderStatus.run = rdr->run;
  builderStatus.lastEventTime = time(NULL);
  builderStatus.events = 0;

  CP;
  plotEvtsByTrigger->getHisto(0)->histo->Reset();
  CP;
  plotTimeByTrigger->getHisto(0)->histo->Reset();
  CP;
  plotTime->getHisto(0)->histo->Reset();
  CP;
  CPC;

  startrun(rdr);
  CPC;
  CP;
}

void JevpPlotSet::startrun(daqReader *rdr)
{
}

// This calls stoprun...
void JevpPlotSet::_stoprun(daqReader *rdr)  
{
  CP;
  CPC;
  stoprun(rdr);   // perform user actions first...
  CPC;

  builderStatus.setStatus("stopped");

  CP;
}

void JevpPlotSet::stoprun(daqReader *rdr)
{
}

void JevpPlotSet::_event(daqReader *rdr)
{
  processingTimer->record_time();

  CPC;
  builderStatus.events++;
  builderStatus.lastEventTime = time(NULL);
  event(rdr);
  CPC;

  processingTime += processingTimer->record_time();
  numberOfEventsRun++;
}

void JevpPlotSet::event(daqReader *rdr)
{
}

int JevpPlotSet::selectEvent(daqReader *rdr)
{
  return 1;
}

int JevpPlotSet::selectRun(daqReader *rdr)
{
  return 1;
}

char *JevpPlotSet::getPlotSetName() 
{
  return plotsetname;
}
  

// Main is only called in the stand alone version!
void JevpPlotSet::Main(int argc, char *argv[])
{
  static unsigned int last_update = 0;

  if(parseArgs(argc, argv) < 0) {
    return;
  }

  builderStatus.setName(plotsetname);

  rtsLogOutput(RTS_LOG_STDERR);
  rtsLogAddDest((char *)"172.16.0.1",8004);
  rtsLogLevel((char *)WARN);

  if(loglevel) rtsLogLevel(loglevel);
    
  LOG("JEFF", "Starting BUILDER:%s",getPlotSetName());

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

  LOG(DBG, "Initializing reader %s", daqfile ? daqfile : "null");

  // initialize reader
  daqReader *reader = new daqReader(daqfile);

  
  // Do this after the reader...
  rtsLogOutput(RTS_LOG_STDERR);
  rtsLogAddDest((char *)"172.16.0.1",8004);
  rtsLogLevel((char *)WARN);
  if(loglevel) rtsLogLevel(loglevel);

  LOG(DBG, "Got a reader");

  if(diska) reader->setEvpDisk(diska);

  CP;
  RtsTimer_root clock;  // plot clock...

  int _monitorTimerTime = time(NULL);
  char _ofilename[80];
  sprintf(_ofilename, "0");
  int _nget = 0;
  int _nupdate = 0;
  int _nevents = 0;
  double _getClockTime=0;
  double _updateClockTime=0;
  double _burpClockTime=0;
  double _selectionClockTime=0;
  double _eventClockTime=0;
  double _fillsClockTime=0;
  RtsTimer_root _getClock;
  RtsTimer_root _updateClock;
  RtsTimer_root _burpClock;
  RtsTimer_root _selectionClock;
  RtsTimer_root _eventClock;
  RtsTimer_root _fillsClock;

  clock.record_time();

  // initialize client...
  _initialize(argc, argv);

  CP;

  for(;;) {
    CP;
    // Lets update the options for the get call eh?

    _getClock.record_time();
    char *ret = reader->get(0,EVP_TYPE_ANY);
    _nget++;
    _getClockTime += _getClock.record_time();


    if(builderStatus.running()) {
      if(_monitorTimerTime + 20 < time(NULL)) {
      
	
	int elapsed = time(NULL) - _monitorTimerTime;
	_monitorTimerTime = time(NULL);
	
	static char str[512];
	sprintf(str, "%sBuilder: %d elapsed (%s->%s): [%d gets() %4.2lf] [%d updts() %4.2lf] [%4.2lf burp] [%4.2lf selct] [%d evts %4.2lf] [%4.2lf fills]",
		plotsetname,
		elapsed,
		_ofilename,
		reader->file_name,
		_nget, _getClockTime,
		_nupdate, _updateClockTime,
		_burpClockTime,
		_selectionClockTime,
		_nevents, _eventClockTime,
		_fillsClockTime);
	
	strcpy(_ofilename, reader->file_name);
	_nget = 0; _getClockTime = 0;
	_nupdate = 0; _updateClockTime = 0;
	_burpClockTime = 0;
	_selectionClockTime = 0;
	_nevents = 0; _eventClockTime = 0;
	_fillsClockTime = 0;
	
	LOG("JEFF", "%s", str);
      }
    }


    CP;
    clock.record_time();
    
    _burpClock.record_time();
    if(ret == NULL) {  // all kinds of burps...
      switch(reader->status) {

      case EVP_STAT_OK:  
	CP;
	LOG(DBG, "EVP_STAT_OK");

	_burpClockTime += _burpClock.record_time();
	continue;

      case EVP_STAT_EOR:

	CP;
	LOG(DBG, "EVP_STAT_EOR stat=%s",builderStatus.status);
	
	if(!builderStatus.running()) {
	  LOG(NOTE, "Already end of run, don't stop it again... %d",builderStatus.running());
	  // already end of run, don't stop it again...
	  sleep(1);
	  _burpClockTime += _burpClock.record_time();
	  continue;
	}
	
	LOG(DBG, "EOR");
	_stoprun(reader);
	LOG(DBG, "Stoprun");


	LOG(DBG, "End of Run... [previous run=%d, current run=%d]",
	    current_run, reader->run);
     
	if(pdf) {
	  if(xml) {
	    // Here we have some defined format...
	    DisplayFile *displays = new DisplayFile();
	    displays->Read(xml);
	    LOG("JEFF", "pdf writer!");
	    PdfFileBuilder *pdfwriter = new PdfFileBuilder(displays, NULL, this);
	    LOG("JEFF", "Write");
	    pdfwriter->write(pdf,0);
	    LOG("JEFF", "Done");
	  }
	  else {
	    writePdfFile();  // Finish and exit...  
	  }
	}
	
	exit(0);
	
	continue;    // don't have an event to parse... go back to start

      case EVP_STAT_EVT:
	CP;
	LOG(ERR, "Problem reading event... skipping");
	sleep(1);
	_burpClockTime += _burpClock.record_time();
	continue;

      case EVP_STAT_CRIT:
	CP;
	LOG(CRIT, "Critical problem reading event... exiting");
	exit(0);
      }

    }
    
    CP;
    if(reader->status) {
      LOG(ERR, "bad event: status=0x%x",reader->status);
      _burpClockTime += _burpClock.record_time();
      continue;
    }

    _burpClockTime += _burpClock.record_time();

    CP;
    LOG(DBG, "We've got some kind of event:  token=%d seq=%d",reader->token,reader->seq);

    LOG(DBG, "reader run = %d,  curr run = %d",reader->run,current_run);
    if(reader->run != current_run) {
      LOG(DBG, "Got an event for a run change: prev run=%d new run=%d",
	  current_run, reader->run);
      
      current_run = reader->run;
      _startrun(reader);
      _monitorTimerTime = time(NULL);
      strcpy(_ofilename, "0");
      _nget = 0; _getClockTime = 0;
      _nupdate = 0; _updateClockTime = 0;
      _burpClockTime = 0;
      _selectionClockTime = 0;
      _nevents = 0; _eventClockTime = 0;
      _fillsClockTime = 0;
    }
   
    CP;

    _selectionClock.record_time();
    if(!selectRun(reader) || !selectEvent(reader)) {
      CP;
      LOG(NOTE, "Event doesn't contribute...");

      double t = clock.record_time();

      for(int i=0;i<32;i++) {

	if(reader->daqbits & (1<<i)) {
	  int idx=2*i+1;
	  
	  double x = n_pertrg[idx]*avg_time_pertrg[idx];
	  x += t;
	  x /= (n_pertrg[idx]+1);

	  avg_time_pertrg[idx] = x;
	  n_pertrg[idx]++;

	  // EvtsByTrg
	  (plotEvtsByTrigger->getHisto(0)->histo)->Fill(idx);
	  // AvgTime
	  (plotTimeByTrigger->getHisto(0)->histo)->Fill(idx,avg_time_pertrg[idx]);

	  //printf("idx=%d avg=%lf\n",idx, avg_time_pertrg[idx]);
	}
      }

      _selectionClockTime += _selectionClock.record_time();
      continue;
    }
    _selectionClockTime += _selectionClock.record_time();

    CP;
    LOG(NOTE, "Call user code");

    _eventClock.record_time();
    _event(reader);   // process...
    _nevents++;
    _eventClockTime += _eventClock.record_time();

    LOG(NOTE, "Done with user code");

    CP;
    double t = clock.record_time();

    _fillsClock.record_time();
    for(int i=0;i<32;i++) {
      CP;
      if(reader->daqbits & (1<<i)) {
	int idx=2*i;
	  
	LOG(NOTE,"idx = %d",idx);
	double x = n_pertrg[idx]*avg_time_pertrg[idx];
	x += t;
	x /= n_pertrg[idx]+1;

	avg_time_pertrg[idx] = x;
	n_pertrg[idx]++;
	
	// EvtsByTrg
	(plotEvtsByTrigger->getHisto(0)->histo)->Fill(idx);
	// AvgTime
	LOG(DBG, "avg time: %d %lf %lf",idx, avg_time_pertrg[idx], n_pertrg[idx]);
	(plotTimeByTrigger->getHisto(0)->histo)->Fill(idx,avg_time_pertrg[idx]);
      }
    }

    CP;
    // Time...    
    ((TH1F *)plotTime->getHisto(0)->histo)->Fill(t);
    CP;

    _fillsClockTime += _fillsClock.record_time();
  }
}


// Private functions...

int JevpPlotSet::parseArgs(int argc, char *argv[])
{
  CP;

  for(int i=1;i<argc;i++) {
    if(memcmp(argv[i], "-diska", 6) == 0) {
      i++;
      diska = argv[i];
    }
    else if (memcmp(argv[i], "-file", 5) == 0) {
      i++;
      daqfile = argv[i];
    }
    else if (memcmp(argv[i], "-pdf", 4) == 0) {
      i++;
      static char pdf_buff[256];
      pdf_buff[0] = '\0';

      if(argv[i][0] != '/') {	
	getcwd(pdf_buff, 256);
	strcat(pdf_buff, "/");
      }
      strcat(pdf_buff, argv[i]);
      pdf = pdf_buff;
    }
    else if (memcmp(argv[i], "-loglevel", 9) == 0) {
      i++;
      loglevel = argv[i];
    }
    else if (memcmp(argv[i], "-confdatadir", 12) == 0) {
      i++;
      confdatadir = argv[i];
    }
    else if (memcmp(argv[i], "-clientdatadir", 15) == 0) {
      i++;
      clientdatadir = argv[i];
    }
    else if (strcmp(argv[i], "-buildxml") == 0) {
      i++;
      buildxml = argv[i];
    }
    else if (strcmp(argv[i], "-xml") == 0) {
      i++;
      xml = argv[i];
    }
    else {
      printf("No arg #%d = %s\n",i,argv[i]);
      printf("%s arguments\n\t-diska diskapath\n\t-file filename\n\t-pdf pdffilename\n\t-loglevel level\n\t-confdatadir datadir (/RTScache/conf/jevp)\n\t-clientdatadir datadir (/a/jevp/client)\n\t-buildxml <file>\n\t-xml <file>\n",argv[0]);
      CP;
      return -1;
    }    
  }

  CP;
  return 0;
}

void JevpPlotSet::buildTheXml()
{
  CP;

  FILE *f = fopen(buildxml, "w");
  if(!f) {
    printf("Error.   Can't open file %s\n",buildxml);
    exit(0);
  }

  fprintf(f, "<doc>\n\t<display_def>%sDisplay\n", getPlotSetName());

  JevpPlot *curr = (JevpPlot *)plots.First();
  while(curr) {
    fprintf(f,"\t\t<histogram>%s</histogram>\n",curr->GetPlotName());
    curr = (JevpPlot *)plots.After(curr);
  }
  fprintf(f, "\t</display_def>\n");

  fprintf(f, "\t<pallete>\n\t\t<tab>%s\n",getPlotSetName());
  curr = (JevpPlot *)plots.First();
  while(curr) {
    fprintf(f,"\t\t\t<histogram>%s</histogram>\n",curr->GetPlotName());

    curr = (JevpPlot *)plots.After(curr);
  }
  fprintf(f, "\t\t</tab>\n\t</pallete>\n</doc>");
  fclose(f);

  exit(0);
}

void JevpPlotSet::writePdfFile()
{
  LOG(DBG,"writing pdf\n");

  PdfIndex index;
  int page = 1;

  // TCanvas canvas("c1","c1",(400*4)/3,400);
  
  JevpPlot *curr = (JevpPlot *)plots.First();

  char firstname[256];
  char lastname[256];

  strcpy(firstname, pdf);
  strcat(firstname, "(");
  strcpy(lastname, pdf);
  strcat(lastname, ")");

  //tonkoLogLevel = 0;

  while(curr) {  

    TCanvas *canvas = new TCanvas("c1","c1", 500, 400);

    LOG("DBG", "page=%d curr->GetPlotName()=%s",page,curr->GetPlotName());

    index.add(NULL,curr->GetPlotName(),page++,0);
    
    LOG(DBG, "Here");
    curr->draw();
    LOG(DBG, "About to print:  before=0x%x after=0x%x",plots.Before(curr),plots.After(curr));

    if(plots.Before(curr) == NULL) {  
      LOG(DBG, "b: print %s",firstname);
      canvas->Print(firstname,"pdf,Portrait");
    }
    else if (plots.After(curr) == NULL) {
      LOG(DBG, "a: print %s",lastname);
      canvas->Print(lastname,"pdf,Portrait");
    }
    else {
      LOG(DBG, "print %s",pdf);
      canvas->Print(pdf,"pdf,Portrait");
    }

    curr = (JevpPlot *)plots.After(curr);
    LOG(DBG, "curr = 0x%x",curr);

    delete canvas;
  }

  LOG(NOTE, "Done with pdf");

  index.CreateIndexedFile(pdf,pdf);

  LOG(NOTE, "Done with index");
  return;
}

void JevpPlotSet::addServerTags(char *tags)
{
  
  if(parent) {
    LOG("JEFF", "Adding: %s",tags);
    parent->addServerTags(tags);
  }
  else {
    LOG("JEFF", "NO parent");
  }
}


// Helper for getting data
StTriggerData *JevpPlotSet::getStTriggerData(daqReader *rdr)
{
  StTriggerData *trgd = NULL;
  int run = rdr->run;
  
  daq_dta *dd = rdr->det("trg")->get("raw");
  if(dd && dd->iterate()) {
    char *td = (char *)dd->Void;
    
    if(td[3] == 0x40) {
      TriggerDataBlk2009 *trgdatablock2009 = (TriggerDataBlk2009 *)td;
      StTriggerData2009 *trgd2009 = new StTriggerData2009(trgdatablock2009, run);
      trgd = (StTriggerData *)trgd2009;
    }
    else if(td[3] == 0x41) {
      TriggerDataBlk2012 *trgdatablock2012 = (TriggerDataBlk2012 *)td;
      StTriggerData2012 *trgd2012 = new StTriggerData2012(trgdatablock2012, run);
      trgd = (StTriggerData *)trgd2012;
    }
    else {
      LOG("ERR", "TRG RAW: version mismatch 0x%2x-0x%2x-0x%2x-0x%2x", td[0], td[1], td[2], td[3]);
      return NULL;
    }

    return trgd;
  }


  LOG(ERR, "No trigger data exists...");
  return NULL;
}
