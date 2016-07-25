static Int_t k = 0;
static const TString separator("/_");
/* 
   .L $STAR/StarDb/VmcGeometry/Geometry.upgr13.C
   CreateTable()
   .x LoopOverTgeo.C
*/
void LoopOverTgeo(TGeoNode *nodeT = 0, TString pathT = "", TString NameV = "SFDM") {
  if (! nodeT) {
    TObjectSet *set = 0;
    if (! gGeoManager) (TObjectSet *) gDirectory->Get("Geometry");
    if (! gGeoManager) return;
    gGeoManager->RestoreMasterVolume();
    gGeoManager->cd("/HALL_1/CAVE_1/IDSM_1/SFMO_1");
    nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) return;
    TString path = nodeT->GetName();
    cout << "top " << nodeT->GetName() << "\t" << nodeT->GetVolume()->GetName() 
	 << "\t" << path << endl;
    LoopOverTgeo(nodeT,path,NameV);
  } else {
    TGeoVolume *vol = nodeT->GetVolume();
    TObjArray *nodes = vol->GetNodes();
    Int_t nd = nodeT->GetNdaughters();
    TString name(vol->GetName(),4);
    //      if (name == "SVTD" || 
    if (name == NameV) {
      k++;
      TObjString *objs;
      TObjArray *array = pathT.Tokenize(separator); 
      Int_t N = array->GetEntriesFast();
      Int_t id = 0;
      // SVTT_1/SLYD:SLY4_5/SLSD:SLS4_7/SLDI:SLD2_1/STLI:STL2_1/STSI:STS2_5/SVTD_1
      //        layer       ladder                              wafer
      // SVTT_1/SFMO_1     /SFLM_1-20  /SFDM_1                 /SFSW_1-16/SFSD_1
      // SVTT_1/SFMO_1     /SFLM_14    /SFDM_1                 /SFSW_7/SFSD_
      // id = ladder + 100*wafer + 1000*layer;
      Int_t ladder = 0; // numbv(2)
      Int_t wafer  = 0; // numbv(3)
      Int_t layer  = 0; // numbv(1)
      for (Int_t i = 0; i < N; i +=2) {
	objs = (TObjString *) array->At(i); // cout << objs->GetString().Data() << endl;
	TString Name(objs->GetString(),4);
	objs = (TObjString *) array->At(i+1); // cout << objs->GetString().Data() << endl;
	Int_t j = atoi(objs->GetString().Data());
	// cout << Name << "\t" << j << endl;
	if      (Name == "SFMO" ) {layer = 7;}
	else if (Name == "SLYD" ) {layer = j;} // layer
	else {
	  if   (Name == "SLSD" || Name == "SFLM") {ladder = j;}       // ladder
	  else {
	    if (Name == "STSI" || Name == "SFSW") {wafer = j;}   // wafer 
	  }
	}
      }
      delete array;
      Int_t nladder = 0;
      if ( ladder == 0) {// This is the year 1 ladder
	nladder = 1;
	wafer   = layer;
	ladder  = 12;
	layer = 4;
      }
      else if (layer<=2)  nladder = 8;
      else if (layer<=4)  nladder  = 12;// Set 2nd barrel ids
      else if (layer<=6)  nladder  = 16;// Set 3rd barrel ids
      // PN: change geant numbering (CCW) to STAR numbering(CW):
      if (nladder>1 && layer < 7) {//             inner sub-layer - 0, outer - 1:
	Int_t lsub    = (layer-1)%2;
	//             NEW: 12 o'clock is geant's first and STAR last element:
	ladder=nladder-(ladder-1)*2-lsub;
      }
      id =  ladder + 100*wafer + 1000*layer;
      //	cout << "id = " << id << endl;
      const TGeoHMatrix *matrix = gGeoManager->GetCurrentMatrix();
      //	if (matrix) matrix->Print();
      const Double_t *tt = matrix->GetTranslation();
      const Double_t *rr = matrix->GetRotationMatrix();
      Double_t r[9];
      memcpy(r,rr,9*sizeof(Double_t));
      Double_t trans[3];
      memcpy(trans,tt,3*sizeof(Double_t));
      for (Int_t l = 0; l < 3; l++) {
	if (TMath::Abs(trans[l]) < 1e-10) trans[l] = 0;
      }
      for (Int_t l = 0; l < 9; l++) {
	if (TMath::Abs(r[l]) < 1e-10) r[l] = 0;
	if (TMath::Abs(TMath::Abs(r[l]) - 1.) < 1.e-10) r[l] = TMath::Sign(1.,r[l]);
      }
      Double_t drift[3], normal[3], transverse[3];
      drift[0] = r[0]; normal[0] = r[1]; transverse[0] = r[2];
      drift[1] = r[3]; normal[1] = r[4]; transverse[1] = r[5];
      drift[2] = r[6]; normal[2] = r[7]; transverse[2] = r[8];
      Double_t norm;
      TVector3 d(drift); norm = 1/d.Mag(); d *= norm;
      TVector3 t(transverse); norm = 1/t.Mag(); t *= norm;
      TVector3 n(normal);
      TVector3 c = d.Cross(t);
      if (c.Dot(n) < 0) c *= -1;
      d.GetXYZ(drift);
      t.GetXYZ(transverse);
      c.GetXYZ(normal);
      cout << Form("{%5i,  %10.7f,%10.7f,%10.7f",id,drift[0],drift[1],drift[2]) ;
      cout <<     Form(",  %10.7f,%10.7f,%10.7f",normal[0],normal[1],normal[2]) ;
      cout <<     Form(",  %10.7f,%10.7f,%10.7f",transverse[0],transverse[1],transverse[2]) ;
      cout <<     Form(",  %10.5f,%10.5f,%10.5f},",trans[0],trans[1],trans[2]) ;
      cout << "// " <<  pathT << endl;
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
      //      cout << "path " << path << endl;
      //      gGeoManager->cd(node->GetName());
      //      gGeoManager->cdNode(node->GetIndex());
      LoopOverTgeo(node,path,NameV);
      gGeoManager->CdUp();
    }
  }
}
