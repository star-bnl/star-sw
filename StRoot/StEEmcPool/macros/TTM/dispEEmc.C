class EEmcTTDisplay;

EEmcTTDisplay  *eemc=NULL;

void loadLdLibs();
void printNodeTree(TObjArray *nodeList, Int_t level=0); 
void shoot(unsigned int nevents=5, unsigned int maxTower=5); 

void dispEEmc()
{
  // load
  loadLdLibs();

  TGeoManager     *gm    = new TGeoManager("eemc", "eemc tower display");
  TGeoVolume      *top   = gm->MakeBox("star",0, 200., 200., 350.);
  TGeoVolume      *smbox = gm->MakeBox("smbox",0, 10., 20., 30.); // small box at zero 

  eemc  = new EEmcTTDisplay(); // global 

  TGeoTranslation *etra  = new TGeoTranslation("eemc" ,0.0,0.0,0.5*(eemc->getZ1()+eemc->getZ2()));

  top->AddNode(smbox ,1,NULL);
  top->AddNode(eemc(),1,etra);

  gm->SetTopVolume(top);
  gm->CloseGeometry();

  gm->SetVisLevel(4);
  gm->SetVisOption(0);

  TCanvas  *canvas = new TCanvas("eemc","eemc",10,10,1000,1000);
  canvas->SetTheta(30);
  canvas->SetPhi(40); 
  top->Draw();  
  //gPad->x3d();

  //printNodeTree(gm->GetListOfNodes());

  shoot();
  cerr << "shoot() for more random towers" << endl;

  gPad->Update();
}


void
shoot(unsigned int nevents, unsigned int maxTower) 
{
  for(unsigned k=0; k<nevents; k++) {
    eemc->Clear();
    unsigned nt=gRandom->Integer(maxTower);
    cerr << "<SHOOT> ";
    for(unsigned t=0;t<nt;t++) {
      char name[256];
      unsigned s  = gRandom->Integer(12);
      unsigned ss = gRandom->Integer(5);
      unsigned e  = gRandom->Integer(12);
      sprintf(name,"%02dT%1c%02d",s+1,ss+'A',e+1);
      cerr << name << ",";
      eemc->towerHit(s,ss,e);
    }
    cerr << endl;
    eemc->DrawHits();
  }
  gSystem->Sleep(1000);
  gPad->Update();
}


void printNodeTree(TObjArray *nodeList, Int_t level) 
{
  TIter nextNode(nodeList);
  TGeoNode *node;
  while( (node=(TGeoNode *)nextNode())!=NULL ) {
    node->LocalToMaster(&oLoc[0],&oMas[0]);
    for(int k=0;k<level;k++) cerr << "\t";
    cerr << node->GetVolume()->GetName() << endl;
    printNodeTree(node->GetNodes(),level+1);
  }
}



void loadLdLibs() {
  cerr << "loading millions of (un)neccessary libraries ..." << endl;
  gSystem->Load("libTable");
  gSystem->Load("libPhysics");
  gSystem->Load("libGeom");
  // the *.so nightmare
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");     
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");  

  // that what we wanted :)
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcPoolTTM");
}
