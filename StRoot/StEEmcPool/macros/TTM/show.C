// example macro to use  EETowTrackMatchMaker
// Author: Piotr A. Zolnierczuk


class StChain;
class StMuTrack;
class EEmcTower;
class EETowDisplay;
class StPhysicalHelixD;
class StThreeVectorD;

class TGeoNode;
class TGeoVolume;

StChain *chain=0;


void printNodeTree(TObjArray *nodeList, Int_t level=0) 
{
  TIter nextNode(nodeList);
  TGeoNode *node;
  while( (node=(TGeoNode *)nextNode())!=NULL ) {
    for(int k=0;k<level;k++) cerr << "\t";
    cerr << node->GetVolume()->GetName() << endl;
    printNodeTree(node->GetNodes(),level+1);
  }
}



TGeoNode *findNode(TObjArray *nodeList,const char *name)
{
  TIter nextNode(nodeList);
  TGeoNode *node;
  while( (node=(TGeoNode *)nextNode())!=NULL ) {
    if(strcmp(node->GetVolume()->GetName(),name)==0)     return node;
    if( (node=findNode(node->GetNodes(),name))!= NULL ) return node;
  }
  return NULL;
}


void
show
(
 char* inpDir  = "/star/2003/mudst/",                        // MuDST directory
 char* inpFile = "",// "st_physics_4145010_raw_0010001.MuDst.root",  // MuDST file(s)
 char* outFile = "R4145010.root",
 Int_t nFiles  = 1,                                            // # of MuDST file(s)
 Int_t nEvents = -1
 )
{ 
  gErrorIgnoreLevel=1999;

  // load root/root4star libraries
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  // load more libraries :)
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
  TCanvas  *c1 = new TCanvas("eemc","eemc",0,0,1000,1000);

  //
  TGeoManager     *gm   = new TGeoManager("eemc", "eemc tower display");
  TGeoVolume      *top  = gm->MakeBox("star",0, 350., 350., 350.);
  TGeoVolume      *smbox = gm->MakeBox("smbox1",0, 10., 20., 30.);
  EETowDisplay    *eemc = new EETowDisplay();
 
  TGeoTranslation *etra = new TGeoTranslation(0.0,0.0,0.5*(eemc->getZ1()+eemc->getZ2()));
  top->AddNode(smbox, 1,NULL);
  top->AddNode(eemc(),1,etra);
  gm->SetTopVolume(top);
  gm->CloseGeometry();
  gm->SetVisLevel(4);
  gm->SetVisOption(0);

  //printNodeTree(gm->GetListOfNodes());
  //TGeoNode *gf = findNode(gm->GetListOfNodes(),"05TA07");
  //if( gf!=NULL )  cerr << "VOLUME FOUND " << gf->GetVolume()->GetName() << endl;

  c1->SetTheta(90);
  c1->SetPhi(0);

  top->Draw();
  //gPad->GetView()->ShowAxis();
 

  
  // now we add Makers to the chain...  some of that is black magic :) 
  muDstMaker       = new StMuDstMaker(0,0,inpDir,inpFile,"MuDst.root",nFiles);  // muDST main chain
  StMuDbReader* db = StMuDbReader::instance();                                  // need the database
  StEEmcDbMaker  *eemcDbMaker=new StEEmcDbMaker("eemcDb");                      // need EEMC database  
  St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");                // need the database (???)


  // now comment in/out/change the below if you want it your way
  eemcDbMaker->setSectors(5,8);            // request EEMC DB for sectors you need (dafault:1-12)
  eemcDbMaker->setTimeStampDay(20030514);  // format: yyyymmdd
  // eemcDbMaker->setDBname("TestScheme/eemc");               // use alternative database
  // eemcDbMaker->setPreferedFlavor("set430","eemcPMTcal");   // request alternative flavor of DB table (if needed)

  // finally after so many lines we arrive at the good stuff
  EETowTrackMatchMaker *mm = new  EETowTrackMatchMaker ("TTM",muDstMaker,eemcDbMaker);
  mm->SetFileName(outFile);
  mm->Summary(cout);    // 

  StMuDebug::setLevel(0);

  chain->Init();
  chain->ls(3);



 //---------------------------------------------------
  int stat=0;
 
  
  for(int counter=0; nEvents<0 || counter<nEvents ; ++counter) {
    if( (stat = chain->Make()) != 0 ) break;
    //cerr << "analyzed " << counter << " events" << endl;
    TIter  nextTower(mm->GetTowers());
    TIter  nextMatch(mm->GetMatch()->GetTable());
    
    EEmcTower *tower;
    StMuTrack *track;
    TPair     *mapPair;
    if(mm->GetMatch()->GetTable()->IsEmpty()) continue;

    eemc->Clear();
    cerr << "<Event>\n";
    while ((mapPair = (TPair*) nextMatch())) {

      tower = (EEmcTower *)mapPair->Key();
      track = (StMuTrack *)mapPair->Value();
      eemc->towerHit(*tower);
      eemc->trackHit(*track);
      eemc->Out(cerr,*track,*tower);
      StPhysicalHelixD helix = track->helix();

      Float_t zPos[] = { 270.290, 279.542 , 306.058 , -1.0 };
      for(int i=0; zPos[i]>0.0; i++) {
	double             dipAng = helix.dipAngle();
	double             z0     = helix.origin().z();
	if(dipAng<1.0e-10) continue;
	double s  = ( zPos[i] - z0 ) / sin(dipAng)  ;
	StThreeVectorD hit = helix.at(s);
	cerr << hit.x() << " " << hit.y() << " " << hit.z() << "\t";
	gm->SetCurrentPoint(hit.x(),hit.y(),hit.z());
	TGeoNode *gnode=gm->FindNode();
	if(gnode!=NULL) cerr << gnode->GetVolume()->GetName() << endl;
	else cerr << "node not found " << endl;
      }

    }
    cerr << "</Event>" << endl;
    eemc->DrawHits();

#if 0
    int nhits=0;
    while( (tower=(EEmcTower *)nextTower()) != NULL ) { 
      if(tower->edep>20 && tower->edep<100 ) {
	nhits++;
      }
    }
    if(nhits<20) eemc->DrawHits();
#endif    

    gPad->Update();
    char c;
    cout << "enter a single character " << endl;
    cin >> c;
    if(c=='q') break;
    if(counter%1000==0) cerr << "analyzed " << counter << " events" << endl;
  }

  mm->Summary(cout);    // 
}
