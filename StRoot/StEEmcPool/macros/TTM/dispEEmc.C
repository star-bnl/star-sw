
class EETowDisplay;
void loadLdLibs();


void printNodeTree(TObjArray *nodeList, Int_t level=0) 
{
  TIter nextNode(nodeList);
  TGeoNode *node;
  while( (node=(TGeoNode *)nextNode())!=NULL ) {
    Double_t oLoc[3] = { 0.0, 0.0, 0.0 };
    Double_t oMas[3] = {-1.0,-1.0,-1.0 };
    node->LocalToMaster(&oLoc[0],&oMas[0]);
    for(int k=0;k<level;k++) cerr << "\t";
    printf("%s (%6.2f,%6.2f,%6.2f) => (%6.2f,%6.2f,%6.2f)\n",
	   node->GetVolume()->GetName(),
	   oLoc[0],oLoc[1],oLoc[2],
	   oMas[0],oMas[1],oMas[2]);
    printNodeTree(node->GetNodes(),level+1);
  }
}




void dispEEmc()
{
  const double GeV    = 1.0;
  const double cm     = 1.0;
  const double second = 1.0;
  const double tesla  = 1e-9*GeV*second/(100.0*cm*100.0*cm);
  const double zPOST=306.06;

  loadLdLibs();


  TGeoManager     *gm    = new TGeoManager("eemc", "eemc tower display");
  TGeoVolume      *top   = gm->MakeBox("star",0, 350., 350., 350.);

  EETowDisplay    *eemc  = new EETowDisplay();
  TGeoTranslation *etra  = new TGeoTranslation("eemc" ,0.0,0.0,0.5*(eemc->getZ1()+eemc->getZ2()));
  TGeoVolume      *smbox = gm->MakeBox("smbox1",0, 10., 20., 30.);

  top->AddNode(smbox ,1,NULL);
  top->AddNode(eemc(),1,etra);


  gm->SetTopVolume(top);
  gm->CloseGeometry();

  gm->SetVisLevel(4);
  gm->SetVisOption(0);

  TCanvas  *c1 = new TCanvas("eemc","eemc",1000,1000,1000,1000);
  c1->SetTheta(90);
  c1->SetPhi(0); 
  top->Draw();  
  //gPad->x3d();

  //printNodeTree(gm->GetListOfNodes());
  //return;

  THelix *thelix=NULL;

  for(unsigned k=0; k<1; k++) {
    cerr << "============> " << endl;
    eemc->Clear();
    //unsigned nt=gRandom->Integer(2);
    //for(unsigned t=0;t<nt;t++) {
    //  unsigned s  = gRandom->Integer(12);
    //  unsigned ss = gRandom->Integer(5);
    //  unsigned e  = gRandom->Integer(12);
    //  cerr << int(s+1) << "," << char(ss+'A') << "," << int(e+1) << endl;
    //  eemc->towerHit(s,ss,e);
    //   //eemc->trackHit(0.0,0.0,0.0,10.0,0.0,10.0,0.5);
    // }
    eemc->towerHit("05TA08");
    eemc->DrawHits();
    //gPad->GetView()->ShowAxis();
 
    double q = -1.0;
    double B = 0.5*tesla;
    double c = 3e10*cm/second;

  
    StThreeVectorD po(0.091773 ,   0.242446 , -75.0201);
    StThreeVectorD pp( 0.31801 ,  -0.51822 ,   1.50135);
 
    thelix = new THelix(po.x(),po.y(),po.z(), pp.x(),pp.y(),pp.z(), q*B*c);
    thelix->SetRange(po.z(),zPOST);
    thelix->SetLineColor(kBlue);
    thelix->SetLineWidth(2);
    thelix->Draw();


    gSystem->Sleep(1000);
  }

  gPad->Update();
  //TVirtualPad *thisPad = gPad;
  //if (thisPad) {
  //  TView *view = thisPad->GetView(); 
  //  if (!view) return;
  //  view->SetPhi(0.0);
  // }
  //if(gPad) gPad->x3d();
}

void hanza() 
{
    //StPhysicalHelixD phelix(pp,po,B,q);
    pline = new TPolyLine3D();
    pline->SetLineColor(kBlue);
    double             dipAng = phelix.dipAngle();
    double             z0     = phelix.origin().z();
    double             s      = 0.0;
    s  = (dipAng>1e-10) ? ( zPOST - z0 ) / sin(dipAng)  : 0.0 ;
    for(double xs=0.0; xs<=s; xs+=s/5.0) {
      StThreeVectorD hit = phelix.at(xs);
      cerr << xs << " ==> " << hit.x() << "," << hit.y() << "," << hit.z() << endl;
      pline->SetNextPoint(hit.x(),hit.y(),hit.z());
    }

    pline->Draw();


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
