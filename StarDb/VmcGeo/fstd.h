// $Id: fstd.h,v 1.3 2008/08/27 21:48:13 fisyak Exp $
// $Log: fstd.h,v $
// Revision 1.3  2008/08/27 21:48:13  fisyak
// *** empty log message ***
//
// the geometry of the readout structure of the FTPC
#if defined(fstdConfig)
TGeoVolume *fstd() {
TGeoVolume *FSMO = gGeoManager->MakeTube("FSMO",GetMed("FSTD_AIR"),7,22.3,5.0381); FSMO->SetTitle("the mother of one endcap of FSTD");
  FSMO->SetLineColor(6);
TGeoVolume *FDMO = gGeoManager->MakeTube("FDMO",GetMed("FSTD_AIR"),7,22.3,0.5381); FDMO->SetTitle("the mother of an individual two-layer disk assembly (wafers and cooling)");
  FDMO->SetVisibility(0);
  FDMO->SetLineColor(6);
TGeoVolume *FDMW = gGeoManager->MakeTrd1("FDMW",GetMed("FSTD_AIR"),1.05508,3.315966,0.1843,7.5); FDMW->SetTitle("the mother wedge, housing plate, sensor  and chips");
  FDMW->SetVisibility(0);
  FDMW->SetLineColor(4);
TGeoVolume *FDSW = gGeoManager->MakeTrd1("FDSW",GetMed("FSTD_SENSITIVE"),1.05508,2.863789,0.1500000E-01,6); FDSW->SetTitle("the Silicon Wafer (all active)");
  FDSW->SetLineColor(4);
TGeoVolume *FDTP = gGeoManager->MakeTrd1("FDTP",GetMed("FSTD_ALN"),2.863789,3.315966,0.3810000E-01,1.5); FDTP->SetTitle("the AlN Thermal Plate");
  FDTP->SetLineColor(6);
TGeoVolume *FDSC = gGeoManager->MakeBox("FDSC",GetMed("FSTD_SILICON"),2.7,0.3500000E-01,0.41); FDSC->SetTitle("the readout Chip");
TGeoVolume *FDTP_FDT1 = gGeoManager->MakeTrd1("FDTP",GetMed("FSTD_ALN"),2.863789,3.165241,0.3810000E-01,1); FDTP_FDT1->SetTitle("the AlN Thermal Plate");
  FDTP_FDT1->SetLineColor(6);
TGeoVolume *FDWD = gGeoManager->MakeBox("FDWD",GetMed("FSTD_CARBON"),3.15,0.25,0.5); FDWD->SetTitle("the water duct made of carbon composite");
TGeoVolume *FDWW = gGeoManager->MakeBox("FDWW",GetMed("FSTD_WATER"),3.15,0.15,0.4); FDWW->SetTitle("the water inside the carbon duct");
  FDWW->SetLineColor(3);
   FSMO->AddNode(FDMO,1,new TGeoTranslation(0,0,-4.5));
    TGeoVolume *FDMS = FDMO->Divide("FDMS",2,21,0,17.14286); FDMS->SetTitle("FDMS");
     FDMS->AddNode(FDMW,1,new TGeoCombiTrans(14.5,0,0,GetRot("90XD")));
      FDMW->AddNode(FDSW,1,new TGeoTranslation(0,-0.9930000E-01,-1.5));
      FDMW->AddNode(FDSW,2,new TGeoCombiTrans(0,0.9930000E-01,-1.5,GetRot("R180")));
      FDMW->AddNode(FDTP,1,new TGeoTranslation(0,0,6));
      FDMW->AddNode(FDSC,1,new TGeoTranslation(0,0.1493,4.91));
      FDMW->AddNode(FDSC,2,new TGeoTranslation(0,-0.1493,4.91));
      FDMW->AddNode(FDTP_FDT1,2,new TGeoTranslation(0,0.7620000E-01,5.5));
      FDMW->AddNode(FDTP_FDT1,3,new TGeoTranslation(0,-0.7620000E-01,5.5));
     FDMS->AddNode(FDWD,1,new TGeoCombiTrans(21.5,0,-0.2881,GetRot("90XD")));
      FDWD->AddNode(FDWW,1,gGeoIdentity);
     FDMS->AddNode(FDWD,2,new TGeoCombiTrans(21.5,0,0.2881,GetRot("90XD")));
   FSMO->AddNode(FDMO,2,new TGeoTranslation(0,0,-1.5));
   FSMO->AddNode(FDMO,3,new TGeoTranslation(0,0,1.5));
   FSMO->AddNode(FDMO,4,new TGeoTranslation(0,0,4.5));
  return FSMO;
}
#endif
