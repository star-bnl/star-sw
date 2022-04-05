// example macro to use  EEmcTTMMaker
// Author: Piotr A. Zolnierczuk
class TGeoNode;
class TGeoVolume;

class StPhysicalHelixD;
class StThreeVectorD;

class StChain;
class StMuTrack;
class StMuDstMaker;
class StEventInfo;
class StEventSummary;


class EEmcTower;
class EEmcTTDisplay;
class EEmcTTMMaker;


StChain       *chain=0;
EEmcTTMMaker  *ttm  =0;
StMuDstMaker  *muDstMk   = 0;
EEmcTTDisplay *eemc      = 0;
TPaveText     *eventInfo = 0;
TPaveLabel    *dateInfo  = 0;
TDatime       *now       = 0;

void      printNodeTree(TObjArray *nodeList, Int_t level=0);
TGeoNode *findNode     (TObjArray *nodeList,const char *name);


void
show
(
 char* inpDir  = "",         // MuDST directory
 char* inpFile = "show.lis", // MuDST file(s);                      
 char* outFile = "show.root",// output tree file
 Int_t nFiles  = 50,         // # of MuDST file(s)
 Int_t nEvents = 100         // # of events
 )
  // remeber to adjust dbase timestamp below !!!! 
  // what a ... design
{ 
  //gErrorIgnoreLevel=1999;

  // load root/root4star libraries
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  // load more libraries :)
  gSystem->Load("libmysqlclient");
  gSystem->Load("libGeom");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");

  // load even more libraries (EEMC stuff) 
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcPoolTTM");

  // create the chain    
  chain = new StChain("StChain"); 
  //

  now = new TDatime;
  // for display
  TCanvas    *c1   = new TCanvas("eemc","eemc",10,10,1000,1000);
  TPaveLabel *tlab = new TPaveLabel(-0.99,+0.99,+0.99,+0.90,
				    "EEMC TOWERS & TPC TRACKS     Piotr A Zolnierczuk (IU)");

  eventInfo = new TPaveText (-0.99,-0.99,+0.0 ,-0.75);
  dateInfo  = new TPaveLabel(+0.60,-0.99,+0.99,-0.95,now->AsString());

  TGeoManager  *gm    = new TGeoManager("eemc", "eemc tower display");
  TGeoVolume   *top   = gm->MakeBox("star",0, 200., 200., 350.);
  TGeoVolume   *smbox = gm->MakeBox("smbox1",0, 2., 2., 2.);
  smbox->SetLineColor(kRed);
  // eemc 
  eemc  = new EEmcTTDisplay();
  eemc->SetMagneticField(0.5); // in Tesla
  eemc->SetShowExtrapolatedTracks(true);
 
  TGeoTranslation *etra = new TGeoTranslation(0.0,0.0,0.5*(eemc->getZ1()+eemc->getZ2()));
  top->AddNode(smbox, 1,NULL);
  top->AddNode(eemc(),1,etra);
  gm->SetTopVolume(top);
  gm->CloseGeometry();
  gm->SetVisLevel(4);
  gm->SetVisOption(0);

  c1->SetTheta(90);
  c1->SetPhi(0);

  top->Draw();
  tlab->Draw();

  gPad->Update();
  
  // now we add Makers to the chain...  some of that is black magic :) 
  muDstMk  = new StMuDstMaker(0,0,inpDir,inpFile,"",nFiles);       // muDST main chain
  StMuDbReader  *db       = StMuDbReader::instance();              // need the database
  St_db_Maker   *dbMk        = new St_db_Maker("StarDb", "MySQL:StarDb");   // need another db(?) 
  new StEEmcDbMaker("eemcDb");                     // need EEMC database  

  // now comment in/out/change the below if you want it your way
  dbMk->setTimeStampDay(20040331);   // format: yyyymmdd

  // finally after so many lines we arrive at the good stuff

  ttm = new  EEmcTTMMaker ("TTM",muDstMk,eemcDbMk);
  ttm->Summary(cout);    // 

  StMuDebug::setLevel(0);

  chain->Init();

  StEEmcDb *eemcDb = (StEEmcDb*)chain->GetDataSet("StEEmcDb");
  eemcDb->setSectors(1,12);            // request EEMC DB for sectors you need (dafault:1-12)
  eemcDb->setPreferedFlavor("onlped","eemcPMTped"); // request alternative flavor

  chain->ls(3);

  //---------------------------------------------------
  next();
}



void 
next(int nMatch=1)
{
  char buffer[1024];
  int  stat=0;
  int  match=0;
  while(match<nMatch) {
    stat=chain->Make();
    if( stat==2 || stat==4) break;
    if( stat!=0 ) continue;

    TList       *matchList = ttm->GetMatchList();
    if(matchList->IsEmpty()) continue;
    TIter  nextMatch(matchList);

    match++;
    
    StEventInfo    &evinfo = muDstMk->muDst()->event()->eventInfo();   // event info
    StEventSummary &evsumm = muDstMk->muDst()->event()->eventSummary();// event summary
    sprintf(buffer,"Run #%d Event #%d\n",evinfo.runId(),evinfo.id());
    eventInfo->Clear();
    eventInfo->SetTextAlign(12);
    eventInfo->AddText(buffer);

    eemc->Clear();
    cerr << "<Event";
    cerr << "Run=\""  << evinfo.runId() << "\"\t";
    cerr << "Event=\""<< evinfo.id()    << "\">\n";

    EEmcTTMatch *tmatch;
    while ((tmatch = (EEmcTTMatch *) nextMatch())) {
      TString outs;
      eemc->AddMatch(*tmatch);
      eemc->Out(cerr,*tmatch);
      eemc->Out(outs,*tmatch);
      eventInfo->AddText(outs);
    }
    cerr << "</Event>" << endl;

    now->Set();
    dateInfo->SetLabel(now->AsString());

    eemc->Draw();
    eventInfo->Draw();
    dateInfo->Draw();
    gPad->Update();

  }
  ttm->Summary(cerr);
}







void printNodeTree(TObjArray *nodeList, Int_t level) 
{
  TIter nextNode(nodeList);
  TGeoNode *node;
  while( (node=(TGeoNode *)nextNode())!=NULL ) {
    for(int k=0;k<level;k++) cerr << "\t";
    cerr << node->GetVolume()->GetName() << endl;
    printNodeTree(node->GetNodes(),level+1);
  }
}



TGeoNode *
findNode(TObjArray *nodeList,const char *name)
{
  TIter nextNode(nodeList);
  TGeoNode *node;
  while( (node=(TGeoNode *)nextNode())!=NULL ) {
    if(strcmp(node->GetVolume()->GetName(),name)==0)     return node;
    if( (node=findNode(node->GetNodes(),name))!= NULL ) return node;
  }
  return NULL;
}
