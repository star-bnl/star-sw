static Int_t k = 0;
static const TString separator("/_");
/* 
   .L $STAR/StarDb/VmcGeometry/Geometry.upgr13.C
   CreateTable()
   .x LoopOverTgeo.C
*/
void LoopOverMTD(TGeoNode *nodeT = 0, TString pathT = "HALL_1/CAVE_1/MUTD_1", TString NameV = "MGAP") {
  if (! nodeT) {
    TObjectSet *set = 0;
    if (! gGeoManager) (TObjectSet *) gDirectory->Get("Geometry");
    if (! gGeoManager) return;
    gGeoManager->RestoreMasterVolume();
    if (pathT != "")     gGeoManager->cd(pathT);
    nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) return;
    TString path = nodeT->GetName();
    cout << "top " << nodeT->GetName() << "\t" << nodeT->GetVolume()->GetName() << "\t" << path << endl;
    LoopOverMTD(nodeT,path,NameV);
  } else {
    //    cout << nodeT->GetName() << endl;
    const TGeoVolume *vol = nodeT->GetVolume();
    TObjArray *nodes = vol->GetNodes();
    Int_t nd = nodeT->GetNdaughters(); // cout << "nd " << nd << endl;
    TString name(vol->GetName(),4);
    if (name == NameV) {
      k++;
      //      cout << pathT.Data() << endl;
      // MUTD_1/MTMF_1/MMBL_1/MTRF_5/MIGF_1/MGAP_4
      Int_t half, sector, tray, sensor, gasgap;
      Int_t n = sscanf(pathT.Data(),"MUTD_1/MTMF_1/MMBL_1/MTRF_%d/MIGF_1/MGAP_%d",&sector,&gasgap);
      //      tray = 60*(half-1) + sector;
      if (gasgap != 3) return;
      vol->Print(); vol->InspectShape();
      const TGeoHMatrix *matrix = gGeoManager->GetCurrentMatrix(); 
      cout << "pathT " << pathT << endl;
#if 0
      Double_t     *xyz    = matrix->GetTranslation();
      Int_t phi = TMath::Nint(TMath::RadToDeg()*TMath::ATan2(xyz[1],xyz[0])/6.);
      phi += 60;
      phi %= 60;
      cout << "half " << half << " sector " << sector << " tray " << tray << " phi " << phi << " sensor " << sensor << " gas gap " << gasgap << endl; 
#endif
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
      LoopOverMTD(node,path,NameV);
      gGeoManager->CdUp();
    }
  }
}
