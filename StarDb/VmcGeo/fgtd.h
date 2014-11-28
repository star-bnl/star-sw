#ifdef fgtdConfig
TGeoVolume *fgtd() {
TGeoVolume *FGMO = gGeoManager->MakeTube("FGMO",GetMed("FGTD_AIR"),7.5,43,45.9); FGMO->SetTitle("the mother of the Forward GEM detector");
  FGMO->SetVisibility(0);
  FGMO->SetLineColor(6);
TGeoVolume *FGDO = gGeoManager->MakeTube("FGDO",GetMed("FGTD_AR_MIX"),7.5,43,0.9); FGDO->SetTitle("");
  FGDO->SetVisibility(0);
  FGDO->SetLineColor(6);
TGeoVolume *FGFO = gGeoManager->MakeTube("FGFO",GetMed("FGTD_ALKAP"),7.5,43,0.2500000E-01); FGFO->SetTitle("the GEM foils");
  FGFO->SetLineColor(4);
TGeoVolume *FGIS = gGeoManager->MakeTube("FGIS",GetMed("FGTD_G10"),7.5,8.5,0.2); FGIS->SetTitle("the inner support or spacer");
  FGIS->SetLineColor(3);
TGeoVolume *FGOS = gGeoManager->MakeTube("FGOS",GetMed("FGTD_G10"),42,43,0.2); FGOS->SetTitle("");
  FGOS->SetLineColor(3);
TGeoVolume *FGSC = gGeoManager->MakeTube("FGSC",GetMed("FGTD_SENSITIVE"),8.5,42,0.2); FGSC->SetTitle("the sensitive area");
  FGSC->SetLineColor(6);
TGeoVolume *FGIS_FGI1 = gGeoManager->MakeTube("FGIS",GetMed("FGTD_G10"),7.5,8.5,0.15); FGIS_FGI1->SetTitle("the inner support or spacer");
  FGIS_FGI1->SetLineColor(3);
TGeoVolume *FGOS_FGO1 = gGeoManager->MakeTube("FGOS",GetMed("FGTD_G10"),42,43,0.15); FGOS_FGO1->SetTitle("");
  FGOS_FGO1->SetLineColor(3);
TGeoVolume *FGRL = gGeoManager->MakeTube("FGRL",GetMed("FGTD_G10"),7.5,43,0.15); FGRL->SetTitle("the readout layer");
  FGRL->SetLineColor(3);
    FGDO->AddNode(FGFO,1,new TGeoTranslation(0,0,-0.875));
    FGDO->AddNode(FGIS,1,new TGeoTranslation(0,0,-0.65));
    FGDO->AddNode(FGOS,1,new TGeoTranslation(0,0,-0.65));
    FGDO->AddNode(FGSC,1,new TGeoTranslation(0,0,-0.65));
    FGDO->AddNode(FGFO,2,new TGeoTranslation(0,0,-0.425));
    FGDO->AddNode(FGIS_FGI1,2,new TGeoTranslation(0,0,-0.25));
    FGDO->AddNode(FGOS_FGO1,2,new TGeoTranslation(0,0,-0.25));
    FGDO->AddNode(FGFO,3,new TGeoTranslation(0,0,-0.7500003E-01));
    FGDO->AddNode(FGIS_FGI1,3,new TGeoTranslation(0,0,0.9999998E-01));
    FGDO->AddNode(FGOS_FGO1,3,new TGeoTranslation(0,0,0.9999998E-01));
    FGDO->AddNode(FGFO,4,new TGeoTranslation(0,0,0.275));
    FGDO->AddNode(FGIS_FGI1,4,new TGeoTranslation(0,0,0.45));
    FGDO->AddNode(FGOS_FGO1,4,new TGeoTranslation(0,0,0.45));
    FGDO->AddNode(FGRL,1,new TGeoTranslation(0,0,0.75));
   FGMO->AddNode(FGDO,1,new TGeoTranslation(0,0,-45));
   FGMO->AddNode(FGDO,2,new TGeoTranslation(0,0,-27));
   FGMO->AddNode(FGDO,3,new TGeoTranslation(0,0,-9.000002));
   FGMO->AddNode(FGDO,4,new TGeoTranslation(0,0,8.999998));
   FGMO->AddNode(FGDO,5,new TGeoTranslation(0,0,27));
   FGMO->AddNode(FGDO,6,new TGeoTranslation(0,0,45));
  return FGMO;
}
#endif
