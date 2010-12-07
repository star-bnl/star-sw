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


//ClassImp(JevpServer);


static int line_number=0;
#define CP line_number=__LINE__

static void sigHandler(int arg, siginfo_t *sig, void *v)
{
  static char str[255];
  
  if(arg == 17) {
    LOG(WARN, "Got signal 17 (reading pdf?): ignoring");
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


int JevpServer::init(int port) {
  LOG("JEFF", "Server port %d", port);
  ssocket = new TServerSocket(port,kTRUE,100);
  mon = new TMonitor();

  mon->Add(ssocket);
  
  updateDisplayDefs();

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
  s = mon->Select();
  //LOG("JEFF", "back from select");
  CP;
  if((int)s <= 0) {
    LOG(WARN, "Got a timeout or an error: %d",s);
    return;
  }

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
    if(ret == 0) {    // Got a disconnection...
      CP;
      mon->Remove(s);
      delete s;
      delete mess;
      return;
    }

    // Handle control messages...
    if(strcmp(mess->GetClass()->GetName(),"EvpMessage")==0) {
      CP;

      EvpMessage *msg = (EvpMessage *)mess->ReadObject(mess->GetClass());
	
      handleEvpMessage(s, msg);
	
      delete msg;
    }
    else if (strcmp(mess->GetClass()->GetName(), "RunStatus") == 0) {
      CP;
      RunStatus *newstat = (RunStatus *)mess->ReadObject(mess->GetClass());

      if(status) {
	delete status;
      }
      
      status = newstat;

      CP;

      status->dump(); 

      if(status->getEndOfRun() == 1) {   // stoprun
	CP;
	LOG("JEFF", "Got end of run...%d",displays->nDisplays());

	// Write out the pdfs for all displays...
	for(int i=0;i<displays->nDisplays();i++) {
	  LOG(DBG,"Writing pdf for display %d, run %d",i,status->getRunNumber());
	  CP;
	  writePdf(i, status->getRunNumber());
	  CP;
	}

	// Update the palletes and write out xml again
	char fn[256];
	sprintf(fn, "%s/%s", basedir, displays_fn);


	//	xxxxx (got to add new histos... //
	JevpPlot *curr = (JevpPlot *)plots.First();
	while(curr) {
	  LOG(DBG, "Add plot:  %s / %s", curr->GetPlotName(), curr->getParent());

	  addToPallete(curr);

	  curr = (JevpPlot *)plots.After(curr);	  
	}
	
	LOG(DBG, "Writing display file...%s",fn);
	if(displays->Write(fn) < 0) {
	  LOG(ERR, "Error writing xml file %s",fn);
	}

	displays->dump();
      }

      if(status->getEndOfRun() == 0) {  // startrun
	CP;
	JevpPlot *curr = (JevpPlot *)plots.First();
	CP;
	while(curr) {
	  CP;
	  plots.Remove(curr);
	  CP;
	  delete curr;
	  CP;
	  curr = (JevpPlot *)plots.First();
	  CP;
	}  
      }
 
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

// This function actually checks if already in pallete
// if not, adds....
void JevpServer::addToPallete(JevpPlot *plot)
{
  char *builder = plot->getParent();
  char *name = plot->GetPlotName();

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

  if(!palleteNode) {
    LOG(ERR, "No pallete found!");
    return;
  }
  LOG(DBG, "Found pallete node");

  // Look for builder...
  DisplayNode *builderNode = palleteNode->child;
  while(builderNode) {
    if(strcmp(builderNode->name, builder) == 0) {
      break;
    }
    else {
      LOG(DBG, "Looking for builder %s:  checking dir %s",builderNode->name);
    }

    builderNode = builderNode->next;
  }
  
  // If not there, create builder...
  if(!builderNode) {
    LOG(DBG, "Creating builder node!");
    builderNode = new DisplayNode();
    builderNode->setName(builder);
    // alphabetize...
    palleteNode->insertChildAlpha(builderNode);
  }
  
  LOG(DBG, "Have builder node...");
  
  // Look for plot...
  DisplayNode *plotNode = builderNode->child;
  while(plotNode) {
    
    if(strcmp(plotNode->name,name) == 0) {    // Its there, nothing to be done!
      LOG(DBG, "plot was already there...do nothing");
      return;
    }
  
    plotNode = plotNode->next;
  }

  LOG("JEFF", "inserting plot %s/%s into pallete", builder, name);
  // The plot was not found... insert it
  plotNode = new DisplayNode();
  plotNode->setName(name);
  plotNode->leaf = 1;
  builderNode->insertChildAlpha(plotNode);
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
    s->Send(mess);
  } else {
    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(plot);
    s->Send(mess);
  }
}

int JevpServer::launchNewServer(char *filename)
{
  CP;
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

  CP;

  // Now launch...
  char fullpath[256];
  sprintf(fullpath, "/a/%s",filename);
  LOG("JEFF", "Launch file %s on port %d", fullpath, port);
  
  char portstring[12];
  sprintf(portstring, "%d", port);

  char *args[5];
  args[0] = (char *)"launch_reanalyze";
  args[1] = portstring;
  args[2] = fullpath;
  args[3] = NULL;

  CP;

  execScript((char *)"launch_reanalyze", args, 0);

  CP;

  LOG("JEFF", ">  launch_reanalyze %s %s",portstring, fullpath);

  CP;
  return port;
}

void JevpServer::handleEvpMessage(TSocket *s, EvpMessage *msg)
{
  CP;
  if(strcmp(msg->cmd, "newrun") == 0) {
    CP;
    LOG("WARN", "Got a newrun");
    clearForNewRun();
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
    if(!status) {
      char tmp[100];
      sprintf(tmp, "No status available");
      EvpMessage m;
      m.setSource((char *)"serv");
      m.setCmd((char *)"nostatus");
      m.setArgs(tmp);
      TMessage mess(kMESS_OBJECT);
      mess.WriteObject(&m);
      s->Send(mess);
    } else {
      TMessage mess(kMESS_OBJECT);
      mess.WriteObject(status);
      s->Send(mess);
    }
  }
  else if(strcmp(msg->cmd, "hello") == 0) {
    CP;
    LOG(WARN,"Got hello from: %s : (%s)\n", msg->source, msg->args);

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
  else if(strcmp(msg->cmd, "monitor") == 0) {
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
  JevpPlot *h = (JevpPlot *)plots.First();

  while(h) {
    delete h;
    plots.Remove(plots.FirstLink());
    h = (JevpPlot *)plots.First();
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
  LOG("JEFF", "Write histogram leaves: %s",node->name);

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

      LOG("JEFF", "cnode->name = %s", cnode->name);
      JevpPlot *plot = getPlot(cnode->name);
      if(plot) {
	LOG("JEFF", "got plot 0x%x",plot);
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

    LOG("JEFF", "Plotting %s on page %d / pad %d",cnode->name, page, pad);
    
    JevpPlot *plot = getPlot(cnode->name);
    if(plot) {
      LOG("JEFF", "Found plot %s",cnode->name);
      plot->draw();
    }
    else {
      LOG("JEFF", "Can't find plot %s",cnode->name);
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
  
  LOG("JEFF", "writeNodePdf:  %s page=%d",node->name,page);

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

void JevpServer::writePdf(int display, int run)
{
  int ret = displays->setDisplay(display);
  if(ret < 0) {
    LOG(ERR, "Can't set display to %d",display);
    return;
  }

  LOG("JEFF", "Set displays to %d",ret);

  char filename[256];
  sprintf(filename, "%s/%s_%d.pdf", pdfdir, displays->displayRoot->name, run);

  PdfIndex index;
  writeNodePdf(displays->displayRoot, &index, NULL, filename, 1, 1);
  
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
  
  // Save it in the database...
  if(nodb != 1) {
    LOG("JEFF", "Writing PDF file: %s",filename);

    char *args[5];

    args[0] = (char *)"WritePDFToDB";
    char tmp[10];
    sprintf(tmp, "%d", run);
    args[1] = tmp;
    args[2] = indexedfilename;
    args[3] = displays->displayRoot->name;
    args[4] = NULL;

    //int ret = char((execScript *)"WritePDFToDB",args);
    int ret = execScript((char *)"WritePDFToDB", args);
    LOG("JEFF", "Wrote PDF file:  ret=%d",ret);
  }
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
    else if (strcmp(argv[i], "-port")==0) {
      i++;
      myport = atoi(argv[i]);
    }
    else {
      printf("\n\nUsage for %s:\n",argv[0]);
      printf("\t[-dd filename]       for each display definition:\n");
      printf("\t[-basedir basedir]   config file directory\n");
      printf("\t[-nodb]\n");
      printf("\t[-port] port]\n");

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

  // Each time we start, archive the existing display file...
  serv.archive_display_file();

  serv.init(serv.myport);

  for(;;) {
    serv.getMessage();
  }
}


int JevpServer::execScript(char *name, char *args[], int waitforreturn)
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
      LOG("JEFF", "args[%d] = %s",i,args[i]);
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

  if((strcmp(s, "show plots") == 0) || (strcmp(s, "show all") == 0)) {
    
    JevpPlot *plot = (JevpPlot *)plots.First();
    if(!plot) {
      b += sprintf(b, "There are no plots\n");
    }
    while(plot) {
      b += sprintf(b, "plot %50s:\t(run #%d:  %d sec old)\n",
		   plot->GetPlotName(),
		   plot->run,
		   (time(NULL) - plot->lastUpdate));
      
      plot = (JevpPlot *)plots.After(plot);
    }
  }
  
  if(b == buff) {
    m->setArgs("The status is good...?");
  }
  else
    m->setArgs(buff);

}
