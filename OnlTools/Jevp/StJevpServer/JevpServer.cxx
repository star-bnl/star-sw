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

#include "EvpConstants.h"
#include "JevpServer.h"
#include <StRoot/RTS/include/rtsLog.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/wait.h>
#include "Jevp/StJevpPlot/JLatex.h"
#include "JTMonitor.h"


//ClassImp(JevpServer);

static int nselects=0;
static int line_number=0;
#define CP line_number=__LINE__

static void sigHandler(int arg, siginfo_t *sig, void *v)
{
  static char str[255];
  
  if(arg == SIGCHLD) {
    int status;
    waitpid(-1, &status, WNOHANG);
    LOG(DBG, "Got signal SIGCHLD (reading pdf?) ");
    return;
  }

  sprintf(str,"Signal %d: shutting down! (line=%d)", arg, line_number);
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

void JevpServer::launchBuilders()
{
  TListIter next(&builders);
  BuilderStatus *curr;
    
  while((curr = (BuilderStatus *)next())) {
    if(!curr->official) continue;

    LOG(DBG, "launching %sBuilder",curr->name);

    char builderName[20];
    sprintf(builderName, "%sBuilder",curr->name);

    const char *args[20];
      
    int i=0;
    args[i++] = "OnlTools/Jevp/launch";
    args[i++] = builderName;

    if(daqfilename) {
      args[i++] = "-file";
      args[i++] = daqfilename;
    }
    else {
      args[i++] = "-diska";
      args[i++] = diska;
    }
      
    if(socketName) {
      args[i++] = "-socket";
      args[i++] = socketName;
    }
    else {
      char portstring[10];
      sprintf(portstring, "%d", myport);
      args[i++] = "-port";
      args[i++] = portstring;
    }
      
    args[i++] = "-server";
    args[i++] = NULL;

    CP;
      
    execScript("OnlTools/Jevp/launch", (char **)args, 0);
  }
}

int JevpServer::init(int port) {
  LOG(DBG, "Server port %d", port);
  if(socketName) {
    ssocket = new TServerSocket(socketName, kTRUE,100);
  }
  else {
    ssocket = new TServerSocket(port,kTRUE,100);
  }

  mon = new JTMonitor();

  mon->Add(ssocket);
  
  updateDisplayDefs();

  if(launchbuilders) {  // If launchbuilders is set launch the builders...
    launchBuilders();
  }

  return 0;
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
  
  // copy till whitespace or end...
  while((*str != '\0') && !isspace(*str)) {
    *tmp = *str;
    tmp++;
    str++;
  }
  *tmp = '\0';
  return dest;  
}

void JevpServer::getMessage() {
  TMessage *mess;
  TSocket *s;

  CP;

  // printf("Got a message\n");
  //  LOG("JEFF", "calling sleep");
  //   sleep(1);
  LOG(DBG, "calling select");
  nselects++;

  
  //for(;;) sleep(10);
  s = mon->Select();
  LOG(DBG, "back from select");
  CP;
  if((long)s <= 0) {
    LOG(WARN, "Got a timeout or an error: %d",s);
    return;
  }

  LOG(NOTE, "Got a message:  s=%d",s);

  if(s == ssocket) {
    CP;
    TSocket *nsock = ssocket->Accept();
    TInetAddress adr = nsock->GetInetAddress();
    mon->Add(nsock);
//     Move from "connect" to the hello message... 
//     BuilderStatus *stat = new BuilderStatus();
//     stat->sockid = (unsigned long long int)nsock;
//     stat->setName("unknown");
//     stat->setStatus("newconnect");
//     stat->lastTransaction = time(NULL);
//     builders.Add(stat);
  }
  else {
    CP;
    // read...
  
    int ret = s->Recv(mess);

    CP;
    if(ret == 0) {    // Got a disconnection...
      CP;

      // Handle the BuilderStatus
      BuilderStatus *stat = getBuilderStatusBySocket((unsigned long long int)s);
      if(stat) {
	LOG(WARN, "Disconnecting Builder Socket for (%s)",stat->name);
	stat->setStatus("unknown");
	stat->sockid = 0ll;
	stat->events = 0;
      }
      else {
	LOG(DBG, "Disconnecting socket, but not a builder socket...");
      }

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
    else if (strcmp(mess->GetClass()->GetName(), "BuilderStatus") == 0) {
      BuilderStatus *newstat = (BuilderStatus *)mess->ReadObject(mess->GetClass());

      // Find the appropriate builder
      BuilderStatus *builderstat = getBuilderStatusBySocket((unsigned long long int)s);
      if(!builderstat) {
	LOG(ERR, "Couldn't find builder status for socket=%lld name=%s",(unsigned long long int)s, newstat->name);
	return;
      }
  
      if(strcmp(builderstat->name, "unknown") == 0) {
	LOG(NOTE, "Got the first message from %s, setting builder's name...", newstat->name);
	builderstat->setName(newstat->name);
      }

      if(strcmp(newstat->name, builderstat->name) != 0) {
	LOG(ERR, "Builder Status's don't match for %s vs %s", newstat->name, builderstat->name);
	return;
      }
      
      LOG(DBG, "Got status change from %s: (%s --> %s)",newstat->name, builderstat->status, newstat->status);

      // update the builder
      builderstat->lastTransaction = time(NULL);
      
      LOG(DBG, "old builder run = %d new =%d  run=%d",builderstat->run,newstat->run,runStatus.run);
      builderstat->run  = newstat->run;
      builderstat->setStatus(newstat->status);
      builderstat->lastEventTime = newstat->lastEventTime;
      builderstat->events = newstat->events;
      builderstat->detectorsNeeded = newstat->detectorsNeeded;


      char oldstatus[20];
      strcpy(oldstatus, runStatus.status);
      int statuschanged = calculateAndUpdateRunStatus(builderstat);
    
      if(statuschanged) {	
	LOG("JEFF", "Got a change to the run status: %s -> %s",oldstatus, runStatus.status);
	if(strcmp(runStatus.status, "stopped")==0) performStopRun();
	if(strcmp(runStatus.status, "running")==0) performStartRun(); 
      }
      CP;
    }
    else if (strcmp(mess->GetClass()->GetName(), "JevpPlot")==0) {
      CP;
      JevpPlot *plot = (JevpPlot *)mess->ReadObject(mess->GetClass());
      handleEvpPlot(s, plot);
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

void JevpServer::performStartRun()
{
  LOG("JEFF", "Start run #%d",runStatus.run);
  clearForNewRun();
}

void JevpServer::performStopRun()
{
  //  LOG("JEFF", "Got end of run...%d",displays->nDisplays());

  // Write out the pdfs for all displays...
  for(int i=0;i<displays->nDisplays();i++) {
    LOG("JEFF","Writing pdf for display %d, run %d",i,runStatus.run);
    CP;
    writeRunPdf(i, runStatus.run);
    CP;
  }

  // Update the palletes and write out xml again
  char fn[256];
  sprintf(fn, "%s/%s", basedir, displays_fn);

  CP;

  // Add any new plots to the pallet...
  JevpPlot *curr = (JevpPlot *)plots.First();
  while(curr) {
    CP;
    LOG(DBG, "Add plot:  %s / %s", curr->GetPlotName(), (curr->getParent()) ? curr->getParent() : "null");
    CP;
    addToPallete(curr);
    CP;
    curr = (JevpPlot *)plots.After(curr);	  
  }
	
  CP;

  LOG(DBG, "Writing display file...%s",fn);
  if(displays->Write(fn) < 0) {
    LOG(ERR, "Error writing xml file %s",fn);
  }

  CP;

  if(killbuilders) {
    LOG("JEFF", "Killbuilders is set, so kill builders");
    
    TListIter next(&builders);
    BuilderStatus *curr;
    while((curr = (BuilderStatus *)next())) {
      if(curr->sockid) {
	TSocket *s = (TSocket *)curr->sockid;
	mon->Remove(s);
	s->Close();
	delete s;
	curr->sockid = 0;
      }
    }
  }

  CP;

  if(die) {
    LOG("JEFF", "die is set, so now exit");
    exit(0);
  }

  //displays->dump();
}

// This function actually checks if already in pallete
// if not, adds....
void JevpServer::addToPallete(JevpPlot *plot)
{
  char *builder = plot->getParent();
  char *name = plot->GetPlotName();

  CP;
  DisplayNode *palleteNode = displays->root->child;

  while(palleteNode) {
    if(strcmp(palleteNode->name, "pallete") == 0) {
      break;   // Found the node we are looking for!
    }
    else {
      LOG(DBG,"Looking for pallete: checking dir %s",palleteNode->name);
    }
    palleteNode = palleteNode->next;
  }

  CP;
  if(!palleteNode) {
    LOG(ERR, "No pallete found!");
    return;
  }
  LOG(DBG, "Found pallete node");

  CP;

  // Look for builder...
  DisplayNode *builderNode = palleteNode->child;

  CP;

  while(builderNode) {
    if(strcmp(builderNode->name, builder) == 0) {
      break;
    }
    else {
      LOG(DBG, "Looking for builder %s:  checking dir %s",builderNode->name);
    }

    builderNode = builderNode->next;
  }
  
  CP;

  // If not there, create builder...
  if(!builderNode) {
    CP;
    LOG(DBG, "Creating builder node!");
    builderNode = new DisplayNode();
    builderNode->setName(builder);
    // alphabetize...
    palleteNode->insertChildAlpha(builderNode);
  }
  
  CP;
  LOG(DBG, "Have builder node...");
  
  CP;
  // Look for plot...
  DisplayNode *plotNode = builderNode->child;
  while(plotNode) {
    
    if(strcmp(plotNode->name,name) == 0) {    // Its there, nothing to be done!
      LOG(DBG, "plot was already there...do nothing");
      return;
    }
  
    plotNode = plotNode->next;
  }
  CP;

  LOG("JEFF", "inserting plot %s/%s into pallete", builder, name);
  // The plot was not found... insert it
  plotNode = new DisplayNode();
  plotNode->setName(name);
  plotNode->leaf = 1;
  builderNode->insertChildAlpha(plotNode);

  CP;
}

JevpPlot *JevpServer::getPlot(char *name) {
  JevpPlot *curr = (JevpPlot *)plots.First();

  while(curr) {
    if(strcmp(curr->GetPlotName(), name) == 0) {
      return curr;
    }
    curr = (JevpPlot *)plots.After(curr);
  }

  return NULL;
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

  LOG("JEFF","save refplot");

  char plotname[256];

  if(plot->refid > 0) {
    shiftRefPlotsUp(plot->GetPlotName(), plot->refid);
  }


  LOG("JEFF", "refplot %s %d",plot->GetPlotName(), plot->refid);
  
  
  sprintf(plotname, "%s/REF.%s.%d.root",refplotdir,plot->GetPlotName(), plot->refid);

  LOG("JEFF", "plotname = %s", plotname);

  // Now actually save plot to the file plotname...
  TFile f(plotname, "new");
  plot->Write();
  f.Close();
}

void JevpServer::handleEvpPlot(TSocket *s, JevpPlot *plot) {

  if(plot->refid != 0) {     // If this is a reference plot 
    saveReferencePlot(plot); // it goes to a disk file...
    return;
  }

  LOG(DBG, "Receiving plot: %s", plot->GetPlotName());

  // Otherwise, the plot stays in memory...
  // Remove previous version...
  JevpPlot *curr = (JevpPlot *)plots.First();

  while(curr) {

    if(strcmp(curr->GetPlotName(), plot->GetPlotName()) == 0) {
      plots.Remove(curr);
      delete curr;
      break;
    }

    curr = (JevpPlot *)plots.After(curr);
  }
  
  plot->lastUpdate = time(NULL);
  plots.Add(plot);
}

double JevpServer::liney(double x)
{
  return 1.0 - (x+5.0)/25.0;
}

JevpPlot *JevpServer::getJevpSummaryPlot()
{
  TListIter nextplot(&plots);
  JevpPlot *cplot;
  while((cplot = (JevpPlot *)nextplot())) {
    if(strcmp(cplot->GetPlotName(), "serv_JevpSummary") == 0) {
      plots.Remove(cplot);
      delete cplot;
      break;
    }
  }

  
  CP;
  JevpPlot *p = new JevpPlot();
  p->setParent((char *)"serv");
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

  p->addHisto(h);

  p->setOptStat(0);
  p->gridx = 0;
  p->gridy = 0;
  
  
  CP;
  JLatex *l;
  
  
  int i = 0;
  char tmp[256];

  sprintf(tmp,"Run #%d: (%s for %ld seconds)",runStatus.run, runStatus.status, time(NULL) - runStatus.timeOfLastChange);
  l = new JLatex(2, liney(i++), tmp);
  i++;
  l->SetTextSize(.05);
  p->addElement(l);

  sprintf(tmp, "Tags:   %s", serverTags);
  l = new JLatex(2, liney(i++), tmp);
  i++;
  l->SetTextSize(.035);
  p->addElement(l);


  // Now show builders...
  TListIter next(&builders);
  BuilderStatus *curr;
  int n=0;
  while((curr = (BuilderStatus *)next())) {
    n++;
    sprintf(tmp, "builder %10s%c: \t(run #%d, status %s, events %d, evttime %ld, contacttime %ld)",
	    curr->name, curr->official ? '*' : '-', curr->run, curr->status, curr->events, time(NULL) - curr->lastEventTime, time(NULL) - curr->lastTransaction);
    l = new JLatex(2, liney(i++), tmp);
    l->SetTextSize(.035);
    p->addElement(l); 
  }
  
  CP;
  if(n == 0) {
    sprintf(tmp,"There are no builders");
    l = new JLatex(2, liney(i++), tmp);
    l->SetTextSize(.035);
    p->addElement(l);
  }
  CP;

  return p;
}

void JevpServer::handleGetPlot(TSocket *s, char *argstring) 
{
  JevpPlot *plot=NULL;
  char refidstr[20];
  char runidstr[20];
  char plotname[80];

  LOG(DBG,"argstring is %s\n",argstring);
  if(!getParamFromString(plotname, argstring)) {
    LOG(ERR,"No plot indicated in getplot?\n");
    return;
  }
 
  LOG(DBG,"Plotname is %s\n",plotname);

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
    LOG(DBG,"getplot..\n");

    if(strcmp(plotname, "serv_JevpSummary") == 0) {
      JevpPlot *p = getJevpSummaryPlot();
 //      CP;
//       LOG("JEFF", "Got summary Plot 0x%x",p);
//       TMessage mess(kMESS_OBJECT);
//       CP;
//       mess.WriteObject(p);
//       CP;
//       s->Send(mess);
//       CP;
//       LOG("JEFF", "Sent summary plot");
      
      handleEvpPlot(NULL, p);
    }
    plot = getPlot(plotname);
  }

    
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
    LOG(DBG, "sent (errmess) %d bytes",ret);
  } else {
    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(plot);
    int ret = s->Send(mess);
    LOG(DBG, "Sent (plot) %d bytes",ret);
  }
}

int JevpServer::launchNewServer(char *filename)
{
  CP;
  printf("scan\n");
  int port=JEVP_PORT;
  int i;
  for(i=1;i<10;i++) {
    TSocket *s = new TSocket("evp", port+i);
    if(!s->IsValid()) {
      s->Close();
      delete s;
      break;
    }
    s->Close();
    delete s;
  }
  
 
  CP;

  if(i < 10) {
    port = JEVP_PORT+i;
    LOG("JEFF", "Launch found port %d", port);
  }
  else {
    LOG("JEFF", "Launch but no unoccupied ports");
    return 0;
  }

  printf("Scan found %d\n",port+i);

  CP;

  // Now launch...
  char fullpath[256];
  sprintf(fullpath, "/a/%s",filename);

  LOG("JEFF", "Launch file %s on port %d", fullpath, port);
  
  char portstring[12];
  sprintf(portstring, "%d", port);

  char builderList[256];
  builderList[0] = '\0';
  TListIter next(&builders);
  BuilderStatus *curr;
  while((curr = (BuilderStatus *)next())) {
    if(!curr->official) continue;

    if(builderList[0] != '\0')
      strcat(builderList,",");

    strcat(builderList,curr->name);
  }

  
  const char *args[20];
  i=0;
  args[i++] = "OnlTools/Jevp/launch";
  args[i++] = "JevpServerMain";
  args[i++] = "-builders";
  args[i++] = builderList;
  args[i++] = "-port";
  args[i++] = portstring;
  args[i++] = "-launchbuilders";
  args[i++] = "-file";
  args[i++] = fullpath;
  args[i++] = "-kill";
  args[i++] = "-nodb";
  args[i++] = NULL;

  CP;
  execScript("launch", (char **)args, 0);

  CP;

  LOG("JEFF", ">> launch new run:  %s %s",portstring, fullpath);

  CP;
  return port;
}

void JevpServer::handleEvpMessage(TSocket *s, EvpMessage *msg)
{
  CP;
  if(strcmp(msg->cmd, "newrun") == 0) {
    CP;
    LOG("JEFF", "Got a newrun from %s....  Now obsolete",msg->source);
    //clearForNewRun();
  }
  else if(strcmp(msg->cmd, "dump") == 0) {
    CP;
    dump();
  }
  else if(strcmp(msg->cmd, "launch") == 0) {
    CP;
    LOG("JEFF", "Got a launch command");
    int port = launchNewServer(msg->args);
    LOG("JEFF", "Launch returned %d",port);
    CP;

    char portstring[12];
    sprintf(portstring,"%d",port);

    CP;
    EvpMessage m;
    m.setSource((char *)"serv");
    m.setCmd((char *)"launch");
    m.setArgs((char *)portstring);
    CP;
    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&m);
    s->Send(mess);
    CP;
  }
  else if(strcmp(msg->cmd, "kill") == 0) {
    CP;
    LOG("JEFF", "Got a kill command... Dying now...");
    exit(0);
  }
  else if(strcmp(msg->cmd, "display_desc") == 0) {  // Display Descriptor

    LOG(DBG, "Got request for display %s", msg->args);
    int ret = displays->setDisplay(msg->args);
    LOG(DBG, "setdisplay returend %d", ret);

    EvpMessage m;
    m.setSource("serv");
    m.setCmd("xml");
    if(!displays) {
      LOG("JEFF", "No displays available\n");
      return;
    }

    
    m.setArgs(displays->textBuff);

    CP;

    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&m);
    s->Send(mess);
  }
  else if(strcmp(msg->cmd, "GetStatus") == 0) {
    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&runStatus);
    s->Send(mess);
  }
  else if(strcmp(msg->cmd, "hello") == 0) {
    CP;
    LOG(NOTE,"Got hello from: %s : (%s)", msg->source, msg->args);

    // This is a builder, so set the builder status!
    BuilderStatus *stat = getBuilderStatusByName(msg->source);
    if(stat) {
      if(stat->sockid) {
	CP;
	LOG(ERR, "A new builder is connecting (%s) but already connected.  Killing new connection...", msg->source);
	
	mon->Remove(s);
	s->Close();
      }
      else {
	LOG(NOTE, "A new builder is connecting (%s) {previously disconnected}", msg->source);
	stat->sockid = (unsigned long long int)s;
	stat->setStatus("newconnect");
	stat->lastTransaction = time(NULL);
      }
    }
    else {
      LOG(WARN, "A new builder is connecting (%s)",msg->source);
      BuilderStatus *stat = new BuilderStatus();
      stat->sockid = (unsigned long long int)s;
      stat->setName(msg->source);
      stat->setStatus("newconnect");
      stat->lastTransaction = time(NULL);
      builders.Add(stat);
    }

    CP;
    EvpMessage m;
    m.setSource((char *)"serv");
    m.setCmd((char *)"hello");
    if((base_client[0] == '\0') ||
       (strcmp(msg->args, "steal")==0) ) {
      strcpy(base_client, msg->source);
      
      m.setArgs((char *)"base");
    }
    else {
      m.setArgs((char *)"client");
    }
    
    CP;
    TMessage mess(kMESS_OBJECT);
    CP;
    mess.WriteObject(&m);
    CP;
    s->Send(mess);
  }
  else if(strcmp(msg->cmd, "ping") == 0) {    
    EvpMessage m;
    m.setSource((char *)"serv");
    m.setCmd((char *)"ping");
    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&m);
    CP;
    s->Send(mess);
  }
  else if(strcmp(msg->cmd, "print") == 0) {
    char printer[100];
    int tab;
    int display;

    sscanf(msg->args, "%s %d %d", printer, &display, &tab);
    LOG("JEFF", "Request to printing tab %d to printer %s", tab, printer);
    
    writePdf((char *)"/tmp/jevp.pdf", display, tab);

    gSystem->Exec("/usr/bin/convert /tmp/jevp.pdf /tmp/jevp.ps");
    
  }
  else if(strcmp(msg->cmd, "newserver") == 0) {
    
  }
  else if(strcmp(msg->cmd, "stoprun") == 0) {
    CP;
    // writePdf();
  }
  else if(strcmp(msg->cmd, "getplot") == 0) {
    CP;
    handleGetPlot(s,msg->args);
  }
  else if(strcmp(msg->cmd, "swaprefs") == 0) {
    CP;
    handleSwapRefs(msg->args);
  }
  else if(strcmp(msg->cmd, "deleteplot") == 0) {
    CP;
    char str[256];
    int idx;
    sscanf(msg->args, "%s %d", str, &idx);
    deleteReferencePlot(str,idx);
  }
  else if(strcmp(msg->cmd, "addServerTag") == 0) {
    CP;
    LOG("JEFF", "Adding serverTags: %s", msg->args);
    addServerTags(msg->args);
    CP;
  }
  else if(strcmp(msg->cmd, "getServerTags") == 0) {
    CP;
    EvpMessage m;
    m.setSource((char *)"serv");
    m.setCmd((char *)"getServerTags");
    if(serverTags) {
      m.setArgs(serverTags);
    }
    else {
      m.setArgs("");
    }

    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&m);
    s->Send(mess);
  }
  else if(strcmp(msg->cmd, "monitor") == 0) {
    CP;
    EvpMessage m;
    m.setSource((char *)"serv");
    m.setCmd((char *)"monitor");

    if(memcmp(msg->args, "launch", 5) == 0) {
      CP;
      
      LOG("JEFF", "Got launch:  launching builders for file %s",msg->args);
      static char _launchname[256];
      strcpy(_launchname, msg->args);
      daqfilename = &_launchname[7];
      CP;
      launchBuilders();
      CP;
      m.setArgs("launched builders...");
    }
    else {
      getMonitorString(msg->args, &m);
    }

    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&m);
    CP;
    s->Send(mess);
  }
  else {
    CP;
    LOG(WARN,"Unknown command: %s\n",msg->cmd);
  }
  CP;
}

void JevpServer::clearForNewRun()
{
  // Delete all from histogram list
  // First free the actual histo, then remove the link...
  LOG("JEFF", "Clear for new run  #%d",runStatus.run);

  TListIter next(&plots);
  JevpPlot *curr;

  while((curr = (JevpPlot *)next())) {   // delete the plots
    delete curr;
  }
  plots.Clear();   // clear the list...

  if(serverTags) {
    free(serverTags);
    serverTags = NULL;
  }
}

void JevpServer::dump()
{
  JevpPlot *curr = (JevpPlot *)plots.First();

  int i=0;
  while(curr) {

    LOG("JEFF","Histogram[%d]: %s\n",i, curr->GetPlotName());
    
    i++;
    curr = (JevpPlot *)plots.After(curr);
  } 
}

int JevpServer::updateDisplayDefs()
{
  char tmp[100];
  sprintf(tmp, "%s/%s", basedir, displays_fn);
  if(displays) delete displays;
  displays = new DisplayFile();
  displays->Read(tmp);
  return 0;
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

// If page = 1 prints out start tag --> "filename("
// But assumes a summary follows, so there is no end tag --> "filename)"
//
int JevpServer::writeHistogramLeavesPdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page)
{
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
    
    printf("Got scaley...  Setting max value to ymax=%lf\n",ymax*1.1);
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

    JevpPlot *plot = getPlot(cnode->name);
    if(plot) {
      LOG(DBG, "Found plot %s",cnode->name);
      plot->draw();
    }
    else {
      LOG(DBG, "Can't find plot %s",cnode->name);
      DrawCrossOfDeath(cnode->name);
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
  
  CP;
  c1->Print(fname, "pdf,Portrait");

  delete c1;
  return 1;
}

int JevpServer::writeNodePdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page, int nosibs)
{
  
  LOG(DBG, "writeNodePdf:  %s page=%d",node->name,page);

  int npages = 0;
  if(node->leaf) {   // We are writing histograms...
    writeHistogramLeavesPdf(node, index, prevIndexEntry, filename, page);
    return 1;
  }
  else {   // We are just writing index entries
    // are we the child?
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

void JevpServer::writeRunPdf(int display, int run)
{
  char filename[256];
  sprintf(filename, "%s/%s_%d.pdf",pdfdir, displays->displayRoot->name, run);
  
  writePdf(filename, display, 1);

  // Save it in the database...
  if(nodb != 1) {
    LOG(DBG, "Writing PDF file: %s",filename);

    char *args[5];

    args[0] = (char *)"WritePDFToDB";
    char tmp[10];
    sprintf(tmp, "%d", run);
    args[1] = tmp;
    args[2] = filename;
    args[3] = displays->displayRoot->name;
    args[4] = NULL;

    //int ret = char((execScript *)"WritePDFToDB",args);
    int ret = execScript("WritePDFToDB", args);
    LOG("JEFF", "Wrote PDF file: %s (ret=%d)", filename, ret);
  }
}

void JevpServer::writePdf(char *filename, int display, int combo_index)
{
  int ret = displays->setDisplay(display);
  if(ret < 0) {
    LOG(ERR, "Can't set display to %d",display);
    return;
  }
  LOG(DBG, "Set displays to %d",ret);
  
  DisplayNode *root = displays->getTab(combo_index);

  if(combo_index == 0) {
    LOG("JEFF", "disproot = 0x%x root = 0x%x", displays->displayRoot, root);
    root = displays->displayRoot;
  }


//   char filename[256];
//   sprintf(filename, "%s/%s_%d.pdf", pdfdir, displays->displayRoot->name, run);

  PdfIndex index;
  writeNodePdf(root, &index, NULL, filename, 1, 0);
  
  // Now a summary....
  char endfilename[256];
  strcpy(endfilename, filename);
  strcat(endfilename, ")");
  TCanvas summary("c2");
  summary.Print(endfilename, "pdf,Portrait");
  
  // Index the file...
  char indexedfilename[256];
  strcpy(indexedfilename, filename);
  // strcat(indexedfilename, ".idx");
  index.CreateIndexedFile(filename, indexedfilename);
}

    


     
// void JevpServer::writePdf(int display, int run)
// {
//   char fullname[100];
  
//   int ret = displays->setDisplay(display);
//   if(ret < 0) {
//     LOG(ERR, "Can't set display to %d",display);
//     return;
//   }

//   sprintf(fullname, "%s/%s_%d.pdf",pdfdir,displays->displayRoot->name,run);

//   int firstdraw=1;

//   // PDF file index support...
//   PdfIndex index;   
//   index_entry *index_tab = NULL;
//   int page = 1;

//   // TAB's and descriptors...
//   int tab;
//   int subtab;
//   //int hist;

//   DisplayNode *tab;
//   DisplayNode *hist;

//   // The ROOT Canvas and pdf names for the drawings...
//   TCanvas canvas("c1");
//   char firstname[256];
//   char lastname[256];

//   strcpy(firstname, fullname);
//   strcat(firstname, "(");
//   strcpy(lastname, fullname);
//   strcat(lastname, ")");

//   // Build the structure of the file...
//   for(tab=0;;tab++) {
//     td = displays[display]->GetTab(tab);
//     if(td == NULL) break;

//     LOG(DBG,"pdf:  TabDescriptor=%s\n",td->name);

//     index_tab = index.add(NULL, td->name, page, 0);
    
//     for(subtab=0;;subtab++) {
//       std = displays[display]->GetTab(tab,subtab);
//       if(std == NULL) {   // Do we move on?
// 	break;
// 	continue;
//       }

//       LOG(DBG,"pdf:  subtabdescriptor=%s\n",td->name);

//       index.add(index_tab, std->name, page, 0);
//       page++;

//       // Build the canvas...
//       CanvasDescriptor *cd = std->canvas;

//       canvas.Clear();
//       canvas.Divide(cd->wide,cd->deep);

//       for(int x=0;x<cd->wide;x++) {
// 	for(int y=0;y<cd->deep;y++) {
// 	  int i=x*cd->deep + y;

// 	  canvas.cd(i+1);

// 	  // Find the name for the current subcanvas...
//  	  hd = displays[display]->GetHist(tab,subtab,i);
// 	  if(hd == NULL) {
// 	    continue;
// 	  }
// 	  else {
// 	    LOG(DBG,"pdf: found display for %d %d %d (%s)\n",tab,subtab,i,hd->name);
// 	  }

// 	  // Have the histogram name, now find the histogram
// 	  JevpPlot *plot = getPlot(hd->name);

// 	  if(plot) {
// 	    LOG(DBG,"Found plot %s\n",hd->name);
// 	    plot->draw();
// 	  }
// 	  else {
// 	    LOG(DBG,"Can't find plot %s\n",hd->name);
// 	    DrawCrossOfDeath(hd->name);
// 	  }
// 	}
//       }

//       if(firstdraw) {
// 	LOG(DBG,"print:  %s\n",firstname);
// 	canvas.Print(firstname,"pdf,Portrait");
// 	firstdraw = 0;
//       }
//       else {
// 	LOG(DBG,"print:  %s\n",fullname);
// 	canvas.Print(fullname,"pdf,Portrait");
//       }
//     }
//   }
//   TCanvas summary("c2");
//   LOG(DBG,"print:  %s\n",lastname);
//   summary.Print(lastname, "pdf,Portrait");

//   index.CreateIndexedFile(fullname,fullname);

//   if(nodb != 1) {
//     LOG("JEFF", "Writing PDF file: %s",fullname);

//     char *args[5];

//     args[0] = (char *)"WritePDFToDB";
//     char tmp[10];
//     sprintf(tmp, "%d", run);
//     args[1] = tmp;
//     args[2] = fullname;
//     args[3] = displays[display]->display_name;
//     args[4] = NULL;

//     //int ret = char((execScript *)"WritePDFToDB",args);
//     int ret = execScript((char *)"WritePDFToDB", args);
//     LOG("JEFF", "Wrote PDF file:  ret=%d",ret);
//   }

// }

void JevpServer::parseArgs(int argc, char *argv[])
{
  // Default builders...
  char bstr[500];
  strcpy(bstr, "base,bbc,bemc,daq,eemc,fpd,ftp,hlt,l3,tof,tpx,trg,upc");

  for(int i=1;i<argc;i++) {
    if(strcmp(argv[i], "-dd")==0) {
      i++;
      displays_fn = argv[i];
    }
    else if (strcmp(argv[i], "-basedir") == 0) {
      i++;
      basedir = argv[i];
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
    else if (strcmp(argv[i], "-builders")==0) {
      i++;
      strcpy(bstr, argv[i]);
    }
    else if (strcmp(argv[i], "-launchbuilders")==0) {
      launchbuilders = 1;
    }
    else if (strcmp(argv[i], "-localsocket")==0) {
      static char _sockname[256];
      sprintf(_sockname, "/tmp/jevp_sock_%d", getpid());
      socketName = _sockname;
    }
    else if (strcmp(argv[i], "-file")==0) {
      i++;
      daqfilename = argv[i];
    }
    else if (strcmp(argv[i], "-kill")==0) {
      killbuilders = 1;
    }
    else if (strcmp(argv[i], "-die")==0) {
      die = 1;
    }
    else if (strcmp(argv[i], "-production") == 0) {
      launchbuilders = 1;
      nodb = 0;
      myport = JEVP_PORT;
    }
    else if (strcmp(argv[i], "-test")==0) {
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
      printf("\t[-localsocket]    use local socket\n");
      printf("\t[-builders] builder,builder2...\n");
      printf("\t[-kill]    (kill builders at end...)\n");
      printf("\t[-die]\n");
      printf("\t[-launchbuilders]\n");
      printf("\t[-file daqfilename]\n");
      printf("\t[-test]   (set port to %d)\n",myport+10);
      printf("\t[-production]\n");
      printf("\t[-diska [/net/a]]  (used to pass to builders on launch)\n");
    
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

  char *s = strtok(bstr,",");
  while(s) {
    BuilderStatus *stat = new BuilderStatus();
    stat->setName(s);
    stat->setStatus("unknown");
    stat->lastTransaction = time(NULL);
    stat->official = 1;
    builders.Add(stat);
    
    s = strtok(NULL, ",");
  }
  
  if(!displays_fn) {
    displays_fn = (char *)"HistoDefs.txt";
  }    
}

void JevpServer::archive_display_file()
{
  LOG("JEFF", "Archiving display file");
  // First find existing display def files..
  char orig[256];
  char archive[256];
  strcpy(archive, basedir);
  strcat(archive, "/display_archive");
  
  
  char tstr[64];
  time_t t = time(NULL);
  struct tm *tmp = localtime(&t);
  strftime(tstr, 64, "%F-%T", tmp);

  sprintf(archive, "%s/%s.%s", archive, displays_fn, tstr);
  sprintf(orig, "%s/%s", basedir, displays_fn);
  
  int fdi = open(orig, O_RDONLY); 
  if(fdi < 0) {
    LOG(ERR, "Can't archive display file %s (%s)", orig, strerror(errno));
  }
  int fdo = open(archive, O_CREAT | O_WRONLY, 0666);
  if(fdo < 0) {
    LOG(ERR, "Can't archive display file %s (%s)", archive, strerror(errno));
  }

  char buff[256];
  int ret = read(fdi, buff, 256);
  while(ret > 0) {
    write(fdo, buff, ret);

    ret = read(fdi, buff, 256);
  }

  close(fdi);
  close(fdo);
}


void JevpServer::main(int argc, char *argv[])
{
  //  gErrorIgnoreLevel = kBreak;   // suppress root messages...
  JevpServer serv;

  rtsLogOutput(RTS_LOG_NET);
  rtsLogAddDest((char *)"172.16.0.1",8004);
  rtsLogLevel((char *)WARN);

  serv.parseArgs(argc, argv);


  LOG("JEFF", "Starting JevpServer: port=%d pid=%d", serv.myport, (int)getpid());

  // Each time we start, archive the existing display file...
  serv.archive_display_file();

  serv.init(serv.myport);

  for(;;) {
    serv.getMessage();
  }
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
      LOG(CRIT, "Error spawning script: %s (%s)",name, strerror(errno),0,0,0);
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


void JevpServer::getMonitorString(char *s, EvpMessage *m)
{
  int max_buff=10*1024;
  char buff[max_buff];
  char *b = buff;

  CP;

  b += sprintf(b,"Run #%d:  status %s  [line %d, nselects %d]\n",runStatus.run, runStatus.status, line_number, nselects);

  CP;

  if((strcmp(s, "show builders") == 0) || (strcmp(s, "show all") == 0)) {
    TListIter next(&builders);
    BuilderStatus *curr;
    int n=0;
    CP;
    while((curr = (BuilderStatus *)next())) {
      n++;
      b += sprintf(b, "builder %10s%c: \t(run #%d, status %s, events %d, evttime %ld, contacttime %ld)\n",
		   curr->name, curr->official ? '*' : '-', curr->run, curr->status, curr->events, time(NULL) - curr->lastEventTime, time(NULL) - curr->lastTransaction);
    }
    CP;
    if(n == 0) {
      b+= sprintf(b,"There are no builders\n");
    }
    CP;
  } 

  CP;
  if((strcmp(s, "show plots") == 0) || (strcmp(s, "show all") == 0)) {
    CP;
    TListIter next(&plots);
    JevpPlot *plot;
    int n=0;

    CP;
    while((plot = (JevpPlot *)next())) {
      CP;
      n++;
      b += sprintf(b, "plot %50s:\t(run #%d:  %ld sec old)\n",
		   plot->GetPlotName(),
		   plot->run,
		   (time(NULL) - plot->lastUpdate));
    }
    CP;
    if(n==0) {
      CP;
      b += sprintf(b, "There are no plots\n");
    }
    CP;
  }
  
  CP;
  if(b == buff) {
    m->setArgs("The status is good...?");
  }
  else
    m->setArgs(buff);

}

BuilderStatus *JevpServer::getBuilderStatusBySocket(unsigned long long int sock)
{
  TListIter next(&builders);
  BuilderStatus *curr;

  while((curr = (BuilderStatus *)next())) {
    if(curr->sockid == sock) {
      return curr;
    }
  }
  
  return NULL;
}

BuilderStatus *JevpServer::getBuilderStatusByName(char *name)
{
  TListIter next(&builders);
  BuilderStatus *curr;

  if(name == NULL) return NULL;

  while((curr = (BuilderStatus *)next())) {
    if(curr->name == NULL) continue;
    if(strcmp(curr->name, name) == 0) return curr;
  }
  
  return NULL;
}

// Returns the new status.   Note... This MAY destroy the run number of the builders
// use with care :-)
int JevpServer::calculateAndUpdateRunStatus(BuilderStatus *changedBuilder)
{
  char *newstatus = runStatus.status;

  if(!changedBuilder->official) return 0;

  TListIter next(&builders);
  BuilderStatus *curr;
  
  int allstopped = 1;
  int curr_run = changedBuilder->run;
  if(curr_run == 0) curr_run = runStatus.run;    // set buildersStatus to 0 for some reason after stop...

  while((curr = (BuilderStatus *)next())) {
    if(!curr->official) continue;
    if(curr == changedBuilder) continue;

    if((strcmp(curr->status, "stopped") != 0) ||
       (curr->run != curr_run)) {
      allstopped = 0;
    }
  }

  if(strcmp(changedBuilder->status, "running") == 0) {
    newstatus = (char *)"running";
  }
  else if (strcmp(changedBuilder->status, "stopped") == 0) {
    if(allstopped) {
      newstatus = (char *)"stopped";

      // Set the builders to run = 0 when stopped (this allows a new run with the same run # to work...)
      TListIter next(&builders);
      BuilderStatus *cb;
      while((cb = (BuilderStatus *)next())) {
	cb->run = 0;
      }
    }
  }
   
  //LOG("JEFF", "status Checking... old %s,  new %s,  chbuilder %s,  curr_run %d  chbuilder_run %d",
  //runStatus.status, newstatus, changedBuilder->status, curr_run, changedBuilder->run);
  //LOG("JEFF", "status allstop %d", allstopped);

  if(strcmp(newstatus, runStatus.status) == 0) {
    return 0;
  }

  runStatus.run = curr_run;
  runStatus.setStatus(newstatus);
  
  return 1;
}

// char *JevpServer::checkRunStatus(BuilderStatus *builderstat, RunStatus *stat)
// {
//   static char returnval[16];

//   stat->setStatus(runStatus.status);   // if no change, keep the old one, ugly!!!


//   if(!builderstat->official) return NULL;

//   TListIter next(&builders);
//   BuilderStatus *curr;
 
//   int allstopped=1;
 
//   int curr_run = builderstat->run;
//   if(curr_run == 0) {
//     curr_run = runStatus.run;
//   }

//   stat->run = curr_run;  

//   while((curr = (BuilderStatus *)next())) {
//     if(!curr->official) continue;
//     if(curr == builderstat) continue;

//     if((strcmp(curr->status, "stopped") != 0) ||
//        (curr->run != curr_run)) {
//       allstopped = 0;
//     }
//   }

//   if(strcmp(builderstat->status, "running") == 0) {
//     // The run starts when the first builder goes to running, not the last :-)
//     // This is because histos are cleared on startrun
//     stat->setStatus("running");
//   }
//   else if (strcmp(builderstat->status, "stopped") == 0) {
//     if(allstopped) {
//       stat->setStatus("stopped");

//       // Hack, set all runs to zero for the case where the same run is 
//       // started again...
//       TListIter next(&builders);
//       BuilderStatus *cb;
//       while((cb = (BuilderStatus *)next())) {
// 	cb->run = 0;
//       }
//     }
//   } 

//   strcpy(returnval, runStatus.status);
 
//   runStatus.setStatus(stat->status);
//   runStatus.run = stat->run;
 
//   return returnval;
// }

// server tag always has leading/trainling "|"
void JevpServer::addServerTag(char *tag)
{
  char tmp[40];
  sprintf(tmp, "|%s|",tag);
  
  if(!serverTags) {   // if this is the first, add it!
    serverTags = (char *)malloc(strlen(tmp) +1);
    strcpy(serverTags, tmp);
    return;
  }

  if(strstr(serverTags, tmp)) {  // if its there do nothing!
    return;
  }

  serverTags = (char *)realloc(serverTags, strlen(serverTags) + strlen(tmp));
  strcat(serverTags, tag);
  strcat(serverTags, "|");
}

// tags delimeted by "|"
void JevpServer::addServerTags(char *tags)
{
  char *tmp = (char *)malloc(sizeof(tags)+1);
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
  
  free(tmp);
}

