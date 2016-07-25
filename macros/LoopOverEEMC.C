static Int_t k = 0;
static const TString separator("/_");
/* 
   .L $STAR/StarDb/VmcGeometry/Geometry.upgr13.C
   CreateTable()
   .x LoopOverTgeo.C
*/
void LoopOverEEMC(TGeoNode *nodeT = 0, TString pathT = "HALL_1/CAVE_1/ECAL_1", TString NameV = "EHM") {
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
    LoopOverEEMC(nodeT,path,NameV);
  } else {
    //    cout << pathT.Data() << endl;
    const TGeoVolume *vol = nodeT->GetVolume();
    //    Bool_t isActive = vol->GetMedium()->GetParam(0) == 1;
    TObjArray *nodes = vol->GetNodes();
    Int_t nd = nodeT->GetNdaughters(); // cout << "nd " << nd << endl;
    TString name(vol->GetName(),3);
    if (name == NameV) {
      k++;
      cout << "pathT " << pathT << endl;
      //      cout << pathT.Data() << endl;
      // ECAL_1/EAGA_1/EMSS_1/ESHM_1/ESP1_2/EXS4_6/EHMi_20
      Int_t l, m, nn;
      Int_t n = sscanf(pathT.Data(),"ECAL_1/EAGA_1/EMSS_1/ESHM_1/ESP%*c_%d/EXS%*c_%d/EHM%*c_%d",&l,&m,&nn);
      cout << "read " << n << " l " << l << " m " << m << " nn " << nn << endl;
      vol->Print(); vol->InspectShape();
      const TGeoHMatrix *matrix = gGeoManager->GetCurrentMatrix(); 
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
      LoopOverEEMC(node,path,NameV);
      gGeoManager->CdUp();
    }
  }
}
