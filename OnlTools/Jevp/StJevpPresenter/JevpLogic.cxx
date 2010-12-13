///  (TH1 *)mfile->Get(myNames[i][j][0].Data(), h);
///  (TH1 *)mfile->Get(myNames[tab][subTab][i].Data());

#include <TROOT.h>
#include <TLine.h>
#include <TText.h>
#include <TSocket.h>
#include <TClass.h>
#include <TMessage.h>

#include "EvpMain.h"
#include "JevpGui.h"
#include "JevpLogic.h"
#include "Jevp/StJevpPlot/EvpMessage.h"
#include "Jevp/StJevpPlot/JevpPlot.h"

//#include "StJevpPool/StJevpUtils/EndOfRunAction.h"
//#include "StJevpPool/StJevpUtils/GroupCollection.h"

#include "qapplication.h"
#include "qwidget.h"
#include "qcursor.h"

#ifndef PR
# define PR(x) { cout << __PRETTY_FUNCTION__ << " ### " << #x << " ### " << x << endl;}
#endif

int JevpLogic::ConnectToServerPort(int port, int ntries)
{
  // First disconnect from current server, if sensible.
  if(socket != NULL) {
    socket->Close();
    socket = NULL;
  }
  
  evpMain->serverport = port;
  TSocket *nsocket;
  for(int i=0;i<ntries;i++) {
    nsocket = new TSocket(evpMain->server, evpMain->serverport);
    if(nsocket->IsValid()) {
      socket = nsocket;
      return 0;
    }

    printf("Error Connecting (%dth of %d tries): %d\n", i, ntries, nsocket->GetErrorCode());
    sleep(1);
  }

  return -1;
}

int JevpLogic::LaunchRun(char *runNumber) {
  EvpMessage msg;
  msg.setCmd((char *)"launch");
  msg.setSource((char *)"presenter");
  msg.setArgs(runNumber);
  send(&msg);


  // Get response...
  TMessage *mess;
  int ret = socket->Recv(mess);
  if(ret == 0) {  // disconnect
    printf("Server disconnected?\n");
    exit(0);
  }
  
  //int x = (int)mess->GetClass();
  if(strcmp(mess->GetClass()->GetName(), "EvpMessage") != 0) {
    printf("Didn't get a EvpMessage class\n");
    exit(0);
  }

  EvpMessage *response = (EvpMessage *)mess->ReadObject(mess->GetClass());

  printf("Got a response: %s %s\n",response->cmd,response->args);

  if(strcmp(response->cmd, "launch") != 0) {
    printf("Didn't get a launch command...");
    return 0;
  }

  int port = atoi(response->args);
  
  return port;
}

void JevpLogic::killServer() {
  EvpMessage msg;
  msg.setCmd((char *)"kill");
  send(&msg);

  socket->Close();
  socket = NULL;
}


JevpLogic::JevpLogic() : mLastDrawnCanvas(0) 
{
  // We need to set up the sockets...
  screens = new TList();

  socket = NULL;

  //   if(evpMain->server) {
  //     socket = new TSocket(evpMain->server,  evpMain->serverport);
  //     if(!socket->IsValid()) {
  //       socket->NetError("connect: ",socket->GetErrorCode());
  //       exit(0);
  //     }
  //   }

  if(ConnectToServerPort(evpMain->serverport,5) < 0) return;
  
  if(evpMain->display == NULL) {
    printf("Need to specify the display\n");
    exit(0);
  }

  if(!evpMain->server || evpMain->displayFile==1) {
    // Read Display from file...

    displayFile = new DisplayFile();
    if(displayFile->Read(evpMain->display) < 0) {
      printf("Error reading display file: %s\n",evpMain->display);
      exit(0);
    }
    
    displayFile->dump();
  }
  else {
    // Read display from server...
    
    EvpMessage msg;
    msg.setCmd((char *)"display_desc");
    msg.setArgs(evpMain->display);
    send(&msg);
    
    // get response...
    //printf("Waiting for tab data from server...\n");
    TMessage *mess;
    int ret = socket->Recv(mess);
    if(ret == 0) {  // disconnect
      printf("Server disconnected?\n");
      exit(0);
    }
    
    //printf("Got something ret=%d mess=0x%x\n",ret,mess);
    int x = (int)mess->GetClass();

    printf("--->0x%x\n",x);// , x[0],x[1],x[2],x[3]);

    if(strcmp(mess->GetClass()->GetName(), "EvpMessage") != 0) {
      printf("Didn't get a DisplayDefSender class\n");
      exit(0);
    }

    EvpMessage *tabdata = (EvpMessage *)mess->ReadObject(mess->GetClass());

    //   printf("....cmd was %s\n",tabdata->cmd);

    if(tabdata->args == NULL) {
      printf("No display '%s' found...\n", evpMain->display);
      exit(0);
    }

    displayFile = new DisplayFile();

    //    printf("asdf\n");
    //    printf("tabdata 0x%x\n",tabdata->args);
    //    printf("%c%c%c\n",tabdata->args[0],tabdata->args[1],tabdata->args[2]);

    displayFile->ReadBuff(tabdata->args, strlen(tabdata->args));
    
    displayFile->setDisplay(evpMain->display);
    // printf("I just got the display file: \n");
    // displayFile->dump();
    // printf("Done dumping it...\n");
    
    delete tabdata;


  }


  // Connect();
  // Some preparations here
  // Here is starting directory name
  SetDebugLevel(0);
}

JevpLogic::JevpLogic(const char* file) : mLastDrawnCanvas(0) {
  // Some preparations here
  // Here is starting directory name
  SetDebugLevel(0);
}

//----------------------------------------------------------------
JevpLogic::~JevpLogic() {
}


//--------------------------------------------------------------
// void EvpPresenter::SetSource(const char* file)  {
//   if ( mDebugLevel) cout << __PRETTY_FUNCTION__ << endl; 
//   emit setEnabled(false);
//   Disconnect();
//   sprintf(mMapFile,"%s",file);
//   Connect();
//   emit setEnabled(true);
// }
//--------------------------------------------------------------



//--------------------------------------------------------------
// void EvpPresenter::ReconfigureTabs() {
//   cout << __PRETTY_FUNCTION__ << endl;
//   emit removeGroupTabs();
//   addGroupTabs();  //tmp->remove();
// }

//--------------------------------------------------------------


void JevpLogic::DrawPlot(JevpScreenWidget *screen) {
  char tmp[256];

  screen->setUpdatesEnabled(false);   // keep double buffer in place...

  int combo_index = screen->combo_index;
  TCanvas *gcc = screen->GetCanvas();

  //printf("sleeping 5\n");
  //sleep(5);

  //int isCanvas=0;
  
  printf("drawplot....combo_index = %d\n",combo_index);

  // I need to use the display def version to get a 
  // real object rather than a string...
  CP;
  DisplayNode *thetab = displayFile->getTab(combo_index);
  CP;
  if(!thetab->leaf) {
    sprintf(tmp, "No histo for index=%d",combo_index);
    CrossOfDeath(screen, tmp);
    CP;
    return;
  }

  //   void *thetab = displayFile->getTab(combo_index, &isCanvas);
  //   if(!isCanvas) {
  //     sprintf(tmp, "No canvas for index=%d",combo_index);
  //     CrossOfDeath(screen, tmp);
  //     return;
  //   }

  
  CP;
  // CanvasDescriptor *cd = (CanvasDescriptor *)thetab;
  int nplots = thetab->nSiblings() + 1;
  screen->Clear();
  // gcc->Clear();
  
  int wide = thetab->getIntParentProperty("wide");
  if(wide <= 0) wide = 1;
  int deep = thetab->getIntParentProperty("deep");
  if(deep <= 0) deep = 1;
  int scaley = thetab->getIntParentProperty("scaley");
  if(scaley <= 0) scaley = 0;

  CP;
  gcc->Divide(wide, deep);
  //sleep(3);

  double maxY = -9999;

  DisplayNode *hd = thetab;
  for(int i=0;i<nplots;i++) {   // First get plots!
    char tmp[256];
    JevpPlot *plot = getPlotFromServer(hd->name,  tmp);
  
    if(plot) {
      screen->addPlot(plot);
      screen->addJevpPlot(plot);
      double my = plot->getMaxY();
      if(my > maxY) maxY = my;
    }

    hd = hd->next;   
  }

  CP;

  // Now plot them
  hd = thetab;
  CP;
  for(int i=0;i<nplots;i++) {
    JevpPlot *plot = screen->getJevpPlot(hd->name);
    gcc->cd(i+1);

    if(plot == NULL) {
      CP;
      CrossOfDeath(screen, tmp);     // Some error...
    }
    else {
      CP;
      if(scaley) {
	plot->setMaxY(maxY * 1.2);
      }
      
      CP;
      plot->draw();
      //delete plot;
    }
    CP;
    //printf("drew %s\n",hd->name);

    hd = hd->next;    
    // sleep(3);
  }

  CP;

  screen->setUpdatesEnabled(true);   // reenable the update
  gcc->Update();
  screen->update();                  // and trigger!

  CP;
  //showDirectories();
}
    
void JevpLogic::saveExistingPlot(JevpPlot *plot)
{
  plot->refid = -plot->refid;
  writePlotToServer(plot);
}

void JevpLogic::writePlotToServer(JevpPlot *plot)
{
  TMessage mess(kMESS_OBJECT);
  mess.WriteObject(plot);
  socket->Send(mess);
}

void JevpLogic::deletePlot(JevpPlot *plot) {
  EvpMessage msg;
  msg.setCmd((char *)"deleteplot");
  char str[256];
  sprintf(str, "%s %d",plot->GetPlotName(), plot->refid);
  msg.setArgs(str);
  send(&msg);
}

void JevpLogic::swapRefsOnServer(char *name, int idx1, int idx2)
{
  EvpMessage msg;
  msg.setCmd((char *)"swaprefs");
  char str[256];
  sprintf(str, "%s %d %d",name, idx1, idx2);
  msg.setArgs(str);
  send(&msg);
}

JevpPlot *JevpLogic::getPlotFromServer(char *name, char *error)
{
  error[0] = '\0';   // clear error...
  
  // Ask server for plot...
  EvpMessage msg;
  msg.setCmd("getplot");
  msg.setArgs(name);
  send(&msg);
  
  // get response...
  printf("Waiting for plot from server...\n");
  TMessage *mess;
  int ret = socket->Recv(mess);
  if(ret == 0) {  // disconnect
    printf("Server disconnected?\n");
    sprintf(error, "Can't get plot: %s  (no server)",name);

    return NULL;
  }
  
  if(strcmp(mess->GetClass()->GetName(), "EvpMessage") == 0) {
    // There was no valid object...
    EvpMessage *msg = (EvpMessage *)mess->ReadObject(mess->GetClass());
    sprintf(error, "Can't get plot: (%s)", msg->args);
    
    delete msg;
    delete mess;
    return NULL;
  }

  if(strcmp(mess->GetClass()->GetName(), "JevpPlot") == 0) {
    JevpPlot *plot = (JevpPlot *)mess->ReadObject(mess->GetClass());

    delete mess;
    return plot;
  }

  printf("Invalid message type %s\n",mess->GetClass()->GetName());
  sprintf(error, "Invalid message type %s",mess->GetClass()->GetName());
  delete mess;
  return NULL;
}


void JevpLogic::CrossOfDeath(JevpScreenWidget *screen, char *str) {

  TLine* a = new TLine(0.,0.,1.,1.);
  TLine* b = new TLine(0.,1.,1.,0.);
  TText* t = new TText(0.5,0.5,str);

  // This is how we free the memory...
  a->SetBit(kCanDelete);
  b->SetBit(kCanDelete);
  t->SetBit(kCanDelete);
  screen->addPlot(a);
  screen->addPlot(b);
  screen->addPlot(t);

  a->SetLineColor(2);
  b->SetLineColor(2);
  t->SetTextColor(3);
  t->SetTextAlign(22);

  // Already cd()'d to proper pad...
  a->Draw();
  b->Draw();
  t->Draw();

  //gcc->Update();
  //cout << __PRETTY_FUNCTION__ << endl;
  return;
}
 

void JevpLogic::Draw(TCanvas* gcc, int  tab, int subTab) {
  //   if(EvpUtil::hGroupName[tab][subTab] != "") {
  //     Draw(gcc, EvpUtil::hGroupName[tab][subTab]);
  //     return;
  //   }

  //   if ( mDebugLevel) {
  //     cout << "Draw tab/subtab : " << tab << "/" << subTab << endl;
  //   }
  //   if ( (mRS->getDetectorBitsRun()&EvpUtil::canvasDetectorBits[tab][subTab])==0 ) { CrossOfDeath(gcc); return; }
  //   if ( (mRS->getTriggerBitsRun()&EvpUtil::canvasTriggerBits[tab][subTab])==0 ) { CrossOfDeath(gcc); return; }
  //   if (!mfile) return;
  
  //   if ( mLastDrawnCanvas != gcc ) needsUpdate = true;
  //   if ( !needsUpdate ) return;
  
  // //  GenericFile* gen = new GenericFile(mfile);
  // //  EvpUtil::DisplayOneCanvas(gen,gcc,tab,subTab);
  // //  delete gen;
  //   EvpUtil::DisplayOneCanvas(mfile,gcc,tab,subTab);
  //   mLastDrawnCanvas =  gcc;
}

void JevpLogic::Draw(TCanvas* gcc, const char* group) {
  //   if ( mDebugLevel ) {
  //     cout << "Draw group : " << group << endl;
  //   }
  //   HistogramGroup* hg = mGroups.read(mfile,group);
  //   if ( !gcc ) {
  //     cout <<  __PRETTY_FUNCTION__ << " no canvas " << endl;
  //     return; 
  //   } 
  //   if ( !hg ) { 
  //     cout <<  __PRETTY_FUNCTION__ << " no histogram group " << endl;
  //     return;
  //   }
  //   if ( mLastDrawnCanvas != gcc ) needsUpdate = true;
  //   if ( !needsUpdate ) return;
  
  //   hg->draw(gcc);
  //   mLastDrawnCanvas = gcc;
}



void JevpLogic::Save(const char* file){
  //   char filename[1024];
  //   //cout << " fileame " << file << endl;
  //   if ( strcmp(file,"")==0 ) {
  //     sprintf(filename,"%s/run%d.root",EvpUtil::GetOutputPath(),mRS->getRunNumber());
  //   } else {
  //     if ( strcmp(file,gSystem->BaseName(file))==0) { // no path given, add default path
  //       sprintf(filename,"%s/%s",EvpUtil::GetOutputPath(),file);
  //     } else { // use filename as given
  //       sprintf(filename,"%s",file);
  //     }
  //   }
  //   cout << " filename " << filename << endl;
  //   if(mfile->mapFile()) {
  //     TMapFile* mapfile = (TMapFile*) mfile->file();
  //     EvpUtil::Map2Root(mapfile,filename);
  //   } else {
  //     TFile* rfile = (TFile*) mfile->file();
  //     rfile->Write(filename);
  //   }
}

void JevpLogic::SaveAll(){
  //   char filename[1024];
  //   char cmd[1024];
  //   char title[1024];
  //   char cname[1024];
  //   //if ( true) return ;
  //   sprintf(filename,"run%d.map",mRS->getRunNumber() );
  //   sprintf(title,"End of run action : run%d",mRS->getRunNumber());
  //   sprintf(cname,"%d",(int)time(0));
  //   sprintf(cmd,"/bin/cp %s %s/%s",mMapFile,EvpUtil::GetOutputPath(),filename);
  //   //cout << cmd << endl;
  //   gSystem->Exec(cmd);
  
  //   gSystem->Sleep(1000);
  //   sprintf(cmd,"pwd; endOfRunAction.csh %s/%s & ",EvpUtil::GetOutputPath(),filename);
  //   cout << cmd << endl;
  //   gSystem->Exec(cmd);

}


void JevpLogic::WriteCurrent(int i, int j){
  //   char psFilename[1024];
  //   char pdfFilename[1024];
  //   sprintf(psFilename,"%s/run%d_tab%d_%d.ps",EvpUtil::GetOutputPath(),mRS->getRunNumber(),i,j);
  //   sprintf(pdfFilename,"%s/run%d_tab%d_%d.pdf",EvpUtil::GetOutputPath(),mRS->getRunNumber(),i,j);
  //   WriteCurrentCanvasToPSFile(psFilename,i,j);

  //   char cmd[1024];
  //   sprintf(cmd,"/usr/bin/convert %s %s",psFilename, pdfFilename);
  //   int iret = gSystem->Exec(cmd);
  //   if ( !iret ) {
  //     cout << " ### error ### writing cureent canvas failed " << endl;
  //     cout << " ### error ### psFilename: " << psFilename << endl;
  //     cout << " ### error ### pdfFilename: " << pdfFilename << endl;
  //     cout << " ### error ### error-code: " << iret << endl;
  //   }
  //   /*
  //   if (iret==0) {
  //     PGMessage* pg = new PGMessage("Writing current canvas:",false,0xffffff); 
  //     pg->AddLine(psFilename);
  //     pg->AddLine(pdfFilename);
  //     pg->AddLine("O.K");
  //     pg->Layout();
  //   } else {
  //     PGMessage* pg = new PGMessage("### error ### Writing current canvas:",false,0xff0000); 
  //     pg->AddLine(psFilename);
  //     pg->AddLine(pdfFilename);
  //     pg->AddLine("return error");
  //     pg->Layout();
  //   }
  //   */

}    


void JevpLogic::WriteCurrentCanvasToPSFile(const char* filename, int tab, int subTab){
  //   // Find active canvas indexes
  //   int i = tab;
  //   int j = subTab;
  //   //cout << filename << endl;
  //   int type = 111; //portrait ps
  //   //int type =112; //landscape ps
  //   //int type =113; //eps

  //   char *name = "Saving Canvas in PS File";
  //   int canvasWidth  = EvpUtil::mCanvasWidth;
  //   int canvasHeight = EvpUtil::mCanvasHeight;


  //   TCanvas* c1 = new TCanvas(name, name, canvasWidth, canvasHeight);
  //   TPaveLabel title(0.1,0.96,0.9,0.99,mHistoPSFile);

  //   TDatime ftime;
  //   TPaveLabel date(0.7,0.01,0.9,0.03,ftime.AsString());


  //   //cout<<filename<<endl;
  //   TPostScript *ps = new TPostScript(filename,type);


  //   c1->cd();
  //   title.Draw();
  //   date.Draw();
  //   EvpUtil::DisplayOneCanvas(mfile,c1,i,j);
 
  //   c1->Update();

  //   ps->Close();
  //   c1->Close();

  //   //gSystem->Exec("ghostview test.ps");
  //   delete ps;
  //   //delete plotPad;
  //   delete c1;

}



void JevpLogic::printAll(const char* filename) {
  //   cout << __PRETTY_FUNCTION__ << " " <<  filename << endl;

  //   gROOT->SetBatch(kTRUE);
  //   TCanvas *cc = new TCanvas("printAllCanvas","printAllCanvas",EvpUtil::mCanvasWidth,EvpUtil::mCanvasHeight);
  //   cc->cd();
  //   //TPostScript ps(filename,111);
  //   char openPs[1024]; 
  //   char printPs[1024];
  //   char closePs[1024];
  //   sprintf(openPs,"%s[",filename);
  //   sprintf(printPs,"%s",filename);
  //   sprintf(closePs,"%s]",filename);

  //   cc->Print(openPs);
  //   //DisplayRunStatus();
  //   cc->Print(printPs);
  //   for ( int tab=0; tab<EvpUtil::mNumberOfTabs; tab++) {
  //     for ( int subTab=0; subTab<EvpUtil::mNumberOfSubTabs[tab]; subTab++) {
  //       if ( (mRS->getDetectorBitsRun()&EvpUtil::canvasDetectorBits[tab][subTab])==0 ) continue;
  //       if ( (mRS->getTriggerBitsRun()&EvpUtil::canvasTriggerBits[tab][subTab])==0 )   continue;
  //       EvpUtil::DisplayOneCanvas(mfile,cc,tab,subTab,true);
  //       cc->Print(printPs);
  //     }
  //   }
  //   mGroups.print(cc,printPs);
  //   cc->Print(closePs);
  //   //mGroups.display(cc);
  //   //ps.Close();
  //   cout << filename << " written " << endl;
  //   delete cc;
}


void JevpLogic::Print(TCanvas* gcc, int tab, int sub) {
  //   char psFilename[1024];
  //   sprintf(psFilename,"%s/run%d_tab%d_%d.ps",EvpUtil::GetOutputPath(),mRS->getRunNumber(),tab,sub);
  //   gROOT->SetBatch(kTRUE);
  //   TCanvas* cc = new TCanvas(psFilename,psFilename,EvpUtil::mCanvasWidth,EvpUtil::mCanvasHeight);
  //   cc->cd();
  //   if ( (mRS->getDetectorBitsRun()&EvpUtil::canvasDetectorBits[tab][sub])==0 ) { CrossOfDeath(gcc); return; }
  //   if ( (mRS->getTriggerBitsRun()&EvpUtil::canvasTriggerBits[tab][sub])==0 ) { CrossOfDeath(gcc); return; }
  //   EvpUtil::DisplayOneCanvas(mfile,cc,tab,sub);
  //   cc->Update();
  //   cc->Print(psFilename);
  //   cc->Close(); 
  //   gROOT->SetBatch(kFALSE);
  //   char cmd[1024];
  //   sprintf(cmd,"lp -d onlprinter2 %s",psFilename);
  //   sprintf(cmd,"ls %s",psFilename);
  //   gSystem->Exec(cmd);
}



void JevpLogic::addGroupTabs() {
  //     GroupMap groupMap( mGroups );
  //     char name[1024];
  //     for( GroupMapIterator mapIter = groupMap.begin(); mapIter != groupMap.end(); mapIter++) {
  //       //cout << (*mapIter).first.c_str() << endl;
  //       sprintf(name,"%s",(*mapIter).first.c_str());
  //       //PR((*mapIter).second.numberOfActiveGroups());
  //       if ( (*mapIter).second.numberOfActiveGroups() ) {
  // 	emit addGroupTab( name );
  // 	for( GroupIterator groupIter = (*mapIter).second.begin(); groupIter != (*mapIter).second.end(); groupIter++) {
  // 	  //cout << "\t " << (*groupIter)->subGroupName() << " " << (*groupIter)->id() << endl;
  // 	  if ( (*groupIter)->active() ) {
  // 	    //sprintf(name,"%s#%s#%s",(*groupIter)->subGroupName(),(*groupIter)->triggerName(),(*groupIter)->detectorName());
  // 	    //emit addGroup( name );
  // 	    emit addGroup( (*groupIter)->id() );
  // 	  }
  // 	}
  //       }
  //     }
}
#if 0
//______________________________________________________________________________ 
void JevpLogic::ClosePresenter() 
{
  // Qt [slot] to terminate the application
  Stop();
}

#endif

u_int JevpLogic::getTabBase()
{
  return DisplayFile::getTabBase();
}

// Gets the multiplier to access the final
u_int JevpLogic::getTabDepthMult(u_int idx)
{
  return DisplayFile::getTabDepthMult(idx);
}

u_int JevpLogic::getTabNextIdx(u_int idx)
{
  return DisplayFile::getTabNextIdx(idx);
}

u_int JevpLogic::getTabChildIdx(u_int idx)
{
  return DisplayFile::getTabChildIdx(idx);
}

u_int JevpLogic::getTabIdxAtDepth(u_int idx, u_int depth)
{
  return DisplayFile::getTabIdxAtDepth(idx, depth);
}

int JevpLogic::send(TObject *msg) {
  TMessage mess(kMESS_OBJECT);
  
  mess.WriteObject(msg);
  socket->Send(mess);
  return 0;
}


void JevpLogic::showDirectories()
{
  printf("gDirectories ls()-------->\n");
  gDirectory->ls();
  printf("screen objects ls()------>\n");
  
  TIter next(screens);
  int i=0;
  JevpScreenWidget *widget;
  while((widget = (JevpScreenWidget *)next())) {
    printf("Widget: %d\n",i++);
    widget->GetCanvas()->ls();
  }
}
