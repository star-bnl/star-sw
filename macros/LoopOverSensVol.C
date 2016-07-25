static Int_t k = 0;
static const TString separator("/_");
void LoopOverSensVol(TGeoNode *nodeT = 0, TString pathT = "") {
  if (! nodeT) {
    TObjectSet *set = 0;
    if (! gGeoManager) (TObjectSet *) gDirectory->Get("Geometry");
    if (! gGeoManager) return;
    gGeoManager->RestoreMasterVolume();
    gGeoManager->CdTop();
    nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) return;
    TString path = nodeT->GetName();
    cout << "top " << nodeT->GetName() << "\t" << nodeT->GetVolume()->GetName() 
	 << "\t" << path << endl;
    LoopOverSensVol(nodeT,path);
  } else {
    //    cout << "pathT " << pathT << endl;
    TGeoVolume *vol = nodeT->GetVolume();
    TGeoMedium *medium = vol->GetMedium();
    Int_t Isvol = (Int_t) medium->GetParam(0);
    if (Isvol) {
      cout << "\t" << pathT << "\t" << nodeT->GetName() 
	   << "\t" << vol->GetName() 
	   << "\t" << gGeoManager->GetCurrentNode()->GetName()
	   << endl;
    }
    TObjArray *nodes = vol->GetNodes();
    if (! nodes) continue;
    Int_t nd = nodes->GetSize();
    //    cout << nd << "\t" << pathT  << endl;
    if (! nd) return;
    for (Int_t id = 0; id < nd; id++) {
      TGeoNode *node = (TGeoNode*)nodes->UncheckedAt(id);
      if (! node) {//cout << "node not found at " << id << endl; 
	continue;}
      vol = node->GetVolume();
      if (! vol) {//cout << "volume not found at " << id << endl; 
	continue;}
      TString path = pathT;
      if (path != "") path += "/";
      path += node->GetName();
      //      gGeoManager->cdDown(node->GetIndex());
      Int_t nodeid = gGeoManager->GetCurrentNode()->GetVolume()->GetIndex(node);
      //      cout << "nodeid " << nodeid << "\t" << path << endl;
      gGeoManager->CdDown(nodeid);
      LoopOverSensVol(node,path);
      gGeoManager->CdUp();
    }
  }
}
