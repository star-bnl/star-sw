class EETowDisplay;
void loadLdLibs();

void dispEEmc()
{
  loadLdLibs();


  TGeoManager     *gm   = new TGeoManager("eemc", "eemc display");
  TGeoVolume      *top  = gm->MakeBox("TOP",0, 270., 270., 120.);

  EETowDisplay    *eemc = new EETowDisplay();
  TGeoTranslation *etra = new TGeoTranslation(0.0,0.0,0.5*(eemc->getZ1()+eemc->getZ2()));
  
  //eemc->Position(top,1,etra);
  top->AddNode(eemc(),1,etra);

  gm->SetTopVolume(top);
  gm->CloseGeometry();
   
  gm->SetVisLevel(3);

  cerr << "hit 3" << endl;
  eemc->towerHit(3);
  top->Draw("S");

  cerr << "hit 9" << endl;
  eemc->towerHit(9);
  top->Draw("S");

  cerr << "hit 1" << endl;
  eemc->towerHit(1);
  top->Draw("S");

  if(gPad) gPad->x3d();
}



void loadLdLibs() {
  gSystem->Load("libTable");
  gSystem->Load("libPhysics");
  gSystem->Load("libGeom");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");        // new addition 22jul99
  //gSystem->Load("StTreeMaker");
  //gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  //gSystem->Load("StTriggerDataMaker"); // new starting from April 2003
  //gSystem->Load("StBichsel");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities");
  gSystem->Load("StEmcUtil");
  //gSystem->Load("StPreEclMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");  

  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcPoolTTM");
}
