/* 
   root.exe $STAR/StarDb/AgiGeometry/y2012.h  LoopOverTpc.C
*/
static Int_t k = 0;
static const TString separator("/_");
//________________________________________________________________________________
void LoopOverTpc(TGeoNode *nodeT = 0, TString pathT = "", TPRegexp NameV = "(tpad|TPAD|TPA1)") {
  TString Format("TPCE_1/TpcSectorWhole_%d/TpcGas_1/TpcPadPlane_%d/tpad_%d");
  Bool_t newRefSystem = kTRUE;
  if (! nodeT) {
//     TString path("HALL_1/CAVE_1/TpcRefSys_1/TPCE_1");
//     TObjectSet *set = 0;
//     if (! gGeoManager) (TObjectSet *) gDirectory->Get("Geometry");
//     if (! gGeoManager) return;
//     gGeoManager->RestoreMasterVolume();
//     if (! gGeoManager->cd(path)) {
      newRefSystem = kFALSE;
      TString path = "HALL_1/CAVE_1/TPCE_1";
      if (! gGeoManager->cd(path)) {
	path = "HALL_1/CAVE_1/TpcRefSys_1/TPCE_1";
	if (! gGeoManager->cd(path)) return;
      }
      Format = "TPCE_1/TPGV_%d/TPSS_%d/TPA%*c_%d";
//     }
    pathT = path;
    if (pathT != "")     gGeoManager->cd(pathT);
    nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) return;
    path = nodeT->GetName();
    //    cout << "top " << nodeT->GetName() << "\t" << nodeT->GetVolume()->GetName() << "\t" << path << endl;
    LoopOverTpc(nodeT,path,NameV);
  } else {
    const TGeoVolume *vol = nodeT->GetVolume();
    TObjArray *nodes = vol->GetNodes();
    Int_t nd = nodeT->GetNdaughters();
    TString name(vol->GetName(),4);
    //      if (name == "SVTD" || 
    //
    if (name.Contains(NameV)) {
      k++;
      // /HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TpcSectorWhole_%d/TpcGas_1/TpcPadPlane_%d/tpad_1
      Int_t part, sector, row;
      Int_t n = (newRefSystem) ?
	sscanf(pathT.Data(),Format.Data(),&sector,&part,&row) :
	sscanf(pathT.Data(),Format.Data(),&part,&sector,&row);
      //      vol->Print(); vol->InspectShape();
      const TGeoHMatrix *matrix = gGeoManager->GetCurrentMatrix(); 
      cout << "pathT " << pathT << endl;
      cout << "part " << part << " sector " << sector << " row " << row << endl;
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
      LoopOverTpc(node,path,NameV);
      gGeoManager->CdUp();
      
    }
  }
}
