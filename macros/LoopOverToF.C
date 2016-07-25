/* 
   root.exe $STAR/StarDb/AgiGeometry/y2012.h  LoopOverToF.C
*/
static Int_t k = 0;
static const TString separator("/_");
void LoopOverToF(TGeoNode *nodeT = 0, TString pathT = "HALL_1/CAVE_1/BTOF_1", TString NameV = "BRSG") {
  if (! nodeT) {
    TObjectSet *set = 0;
    if (! gGeoManager) (TObjectSet *) gDirectory->Get("Geometry");
    if (! gGeoManager) return;
    gGeoManager->RestoreMasterVolume();
    if (pathT != "")     gGeoManager->cd(pathT);
    nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) return;
    TString path = nodeT->GetName();
    //    cout << "top " << nodeT->GetName() << "\t" << nodeT->GetVolume()->GetName() << "\t" << path << endl;
    LoopOverToF(nodeT,path,NameV);
  } else {
    const TGeoVolume *vol = nodeT->GetVolume();
    TObjArray *nodes = vol->GetNodes();
    Int_t nd = nodeT->GetNdaughters();
    TString name(vol->GetName(),4);
    //      if (name == "SVTD" || 
    //
    if (name == NameV) {
      k++;
      // CAVE_1/BTOF_1/BTOH_2/BSEC_59/BTRA_1/BXTR_1/BRTC_1/BGMT_1/BRMD_21
      Int_t half, sector, tray, sensor, gasgap;
      Int_t n = sscanf(pathT.Data(),"BTOF_1/BTOH_%d/BSEC_%d/BTRA_1/BXTR_1/BRTC_1/BGMT_1/BRMD_%d/BRDT_1/BRSG_%d",&half,&sector,&sensor,&gasgap);
      tray = 60*(half-1) + sector;
      if (gasgap != 3) return;
      if (sensor != 1) return;
      vol->Print(); vol->InspectShape();
      const TGeoHMatrix *matrix = gGeoManager->GetCurrentMatrix(); 
      Double_t     *xyz    = matrix->GetTranslation();
      Int_t phi = TMath::Nint(TMath::RadToDeg()*TMath::ATan2(xyz[1],xyz[0])/6.);
      phi += 60;
      phi %= 60;
      cout << "pathT " << pathT << endl;
      cout << "half " << half << " sector " << sector << " tray " << tray << " phi " << phi << " sensor " << sensor << " gas gap " << gasgap << endl; 
      matrix->Print();
      return;
    }
    //    if (nd > 5) nd = 5;
    for (Int_t id = 0; id < nd; id++) {
      TGeoNode *node = (TGeoNode*)nodes->UncheckedAt(id);
      if (! node) continue;
      vol = node->GetVolume();
      if (! vol) continue; 
      TString path = pathT;
      if (path != "") path += "/";
      path += node->GetName();
      //      gGeoManager->cdDown(node->GetIndex());
      Int_t nodeid = gGeoManager->GetCurrentNode()->GetVolume()->GetIndex(node);
      gGeoManager->CdDown(nodeid);
      //      gGeoManager->cd(node->GetName());
      //      gGeoManager->cdNode(node->GetIndex());
      LoopOverToF(node,path,NameV);
      gGeoManager->CdUp();
    }
  }
}
