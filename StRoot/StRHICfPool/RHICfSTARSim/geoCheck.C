{
  gROOT->Reset();

  gSystem->Load("libGeom");
  gSystem->Load("libGdml");

  TGeoManager::Import("./geometry/full_tl.gdml");

  TList* mat = gGeoManager->GetListOfMaterials();
  TIter next(mat);
  TObject *obj;
  while(obj = next()) obj->Print();

  gGeoManager->CheckOverlaps(0.01);
  gGeoManager->PrintOverlaps();

  TIter next(gGeoManager->GetListOfOverlaps());
  int count = 0;
  TGeoOverlap* overlap;
  while ((overlap=(TGeoOverlap*)next())) {
    gGeoManager->GetListOfOverlaps()->At(count)->Draw();
    ++count;
  }

  TObjArray *va = gGeoManager->GetListOfVolumes();
  int nv = va->GetEntries();
  for(int i = 0; i < nv; i++) {
    TGeoVolume *v = (TGeoVolume*)va->At(i);
    cout << "Volume" << i
	 << " Name: " << v->GetName()
	 << " Mat: " << v->GetMaterial()->GetName()
	 << endl;
    string m = v->GetMaterial()->GetName();

    v->SetTransparency(90);

    if(m=="Galactic") v->SetInvisible();
    else if(m=="Air") v->SetInvisible();
    else if(m=="Beryllium") v->SetLineColor(kGray);
    else if(m=="Iron") v->SetLineColor(kGray);
    else if(m=="Duralumin") v->SetInvisible();//v->SetLineColor(kCyan-10);
    else if(m=="Aluminium") v->SetInvisible();// v->SetLineColor(kGray);
    else if(m=="G10") v->SetLineColor(kGreen-10);
    else if(m=="Acrylic") v->SetLineColor(kWhite);
    else if(m=="Tungsten") v->SetLineColor(kGray+2);
    else if(m=="GSO") v->SetLineColor(kCyan);
    else if(m=="GSObar") v->SetLineColor(kRed);
    else if(m=="Quartz") v->SetLineColor(kYellow);
    else if(m=="FPMMA") v->SetLineColor(kGreen);
    else if(m=="PMMA") v->SetLineColor(kCyan);
    else if(m=="Aluminium") v->SetLineColor(kGray-1);
    else if(m=="Lead") v->SetLineColor(kGray);
    else if(m=="Scintillator") v->SetLineColor(kCyan-1);
  }

  gGeoManager->GetTopVolume()->Draw("ogl");


}