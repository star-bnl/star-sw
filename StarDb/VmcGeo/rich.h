// $Id: rich.h,v 1.3 2008/08/27 21:48:16 fisyak Exp $
// $Log: rich.h,v $
// Revision 1.3  2008/08/27 21:48:16  fisyak
// *** empty log message ***
//
// Ring Image Cerenkov geometry
#ifdef richConfig
TGeoVolume *rich() {
TGeoVolume *RICH = gGeoManager->MakeBox("RICH",GetMed("RICH_ALUMINIUM"),49.45,11.325,73.15); RICH->SetTitle("just an aluminum box");
  RICH->SetLineColor(5);
TGeoVolume *SRIC = gGeoManager->MakeBox("SRIC",GetMed("RICH_METHANE"),42.45,11.325,66.15); SRIC->SetTitle("sensitive part of the whole RICH ... full of Methane");
TGeoVolume *ALUM = gGeoManager->MakeBox("ALUM",GetMed("RICH_ALUMINIUM"),42.45,0.2500000E-01,66.15); ALUM->SetTitle("an Aluminum sheet");
  ALUM->SetLineColor(5);
TGeoVolume *HONE = gGeoManager->MakeBox("HONE",GetMed("RICH_CARBONIO"),42.45,0.188,66.15); HONE->SetTitle("a CARBONIO");
  HONE->SetLineColor(6);
TGeoVolume *OQUA = gGeoManager->MakeBox("OQUA",GetMed("RICH_OPACO_QUARTZ"),20.65,0.2,66.5); OQUA->SetTitle("e me scelto per labellare il quarzo opaco");
  OQUA->SetLineColor(2);
TGeoVolume *OQUF = gGeoManager->MakeBox("OQUF",GetMed("RICH_OPACO"),20.65,0.5,66.5); OQUF->SetTitle("FRAME QUARZO OPACO");
  OQUF->SetLineColor(2);
TGeoVolume *FREO = gGeoManager->MakeBox("FREO",GetMed("RICH_FREON"),20.15,0.5,65.5); FREO->SetTitle("FREON");
  FREO->SetLineColor(3);
TGeoVolume *SPAC = gGeoManager->MakeTube("SPAC",GetMed("RICH_QUARZ"),0,0.5,0.5); SPAC->SetTitle("e Spacers (quarz cylinders)");
  SPAC->SetLineColor(2);
TGeoVolume *QUAR = gGeoManager->MakeBox("QUAR",GetMed("RICH_QUARTZ"),20.65,0.25,66.5); QUAR->SetTitle("da me scelto per labellare il quarzo");
  QUAR->SetLineColor(2);
TGeoVolume *BARR = gGeoManager->MakeBox("BARR",GetMed("RICH_OPACO"),20.65,0.25,0.1); BARR->SetTitle("e Barrette quarzo opaco");
  BARR->SetLineColor(2);
TGeoVolume *RGAP = gGeoManager->MakeBox("RGAP",GetMed("RICH_METHANE_GAP"),42.45,0.2,66.15); RGAP->SetTitle("METANOL gap");
  RGAP->SetLineColor(4);
TGeoVolume *ALMF = gGeoManager->MakeBox("ALMF",GetMed("RICH_ALUMINIUM"),42.45,1.6,66.15); ALMF->SetTitle("an Aluminum sheet");
  ALMF->SetLineColor(5);
TGeoVolume *HOLE = gGeoManager->MakeBox("HOLE",GetMed("RICH_AIR"),19.65,1.6,32); HOLE->SetTitle("an oppening in the aluminum frame");
  HOLE->SetLineColor(4);
TGeoVolume *BORD = gGeoManager->MakeBox("BORD",GetMed("RICH_CARBONIO"),19.65,0.2,32); BORD->SetTitle("carbon");
  BORD->SetLineColor(6);
TGeoVolume *RCSI = gGeoManager->MakeBox("RCSI",GetMed("RICH_CSI"),19.6,0.2500000E-01,32); RCSI->SetTitle("Cesium Iodide");
  RCSI->SetLineColor(6);
   rot = new TGeoRotation("next",89.96212,-150.1828,89.97997,-60.18284,0.4285610E-01,57.68942);
    RICH->AddNode(SRIC,1,gGeoIdentity);
     SRIC->AddNode(ALUM,1,new TGeoTranslation(0,-10.825,0));
     SRIC->AddNode(HONE,1,new TGeoTranslation(0,-7.525,0));
     SRIC->AddNode(ALUM,2,new TGeoTranslation(0,-4.225,0));
     SRIC->AddNode(OQUA,1,new TGeoTranslation(-21.65,-1.95,0));
     SRIC->AddNode(OQUA,2,new TGeoTranslation(21.65,-1.95,0));
     SRIC->AddNode(OQUF,1,new TGeoTranslation(-21.65,-1.25,0));
      OQUF->AddNode(FREO,1,gGeoIdentity);
       FREO->AddNode(SPAC,1,new TGeoCombiTrans(6.7,0,57.6,GetRot("90YD")));
       FREO->AddNode(SPAC,2,new TGeoCombiTrans(-6.7,0,57.6,GetRot("90YD")));
       FREO->AddNode(SPAC,3,new TGeoCombiTrans(6.7,0,43.2,GetRot("90YD")));
       FREO->AddNode(SPAC,4,new TGeoCombiTrans(-6.7,0,43.2,GetRot("90YD")));
       FREO->AddNode(SPAC,5,new TGeoCombiTrans(6.7,0,28.8,GetRot("90YD")));
       FREO->AddNode(SPAC,6,new TGeoCombiTrans(-6.7,0,28.8,GetRot("90YD")));
       FREO->AddNode(SPAC,7,new TGeoCombiTrans(6.7,0,14.4,GetRot("90YD")));
       FREO->AddNode(SPAC,8,new TGeoCombiTrans(-6.7,0,14.4,GetRot("90YD")));
       FREO->AddNode(SPAC,9,new TGeoCombiTrans(6.7,0,0,GetRot("90YD")));
       FREO->AddNode(SPAC,10,new TGeoCombiTrans(-6.7,0,0,GetRot("90YD")));
       FREO->AddNode(SPAC,11,new TGeoCombiTrans(6.7,0,-14.4,GetRot("90YD")));
       FREO->AddNode(SPAC,12,new TGeoCombiTrans(-6.7,0,-14.4,GetRot("90YD")));
       FREO->AddNode(SPAC,13,new TGeoCombiTrans(6.7,0,-28.8,GetRot("90YD")));
       FREO->AddNode(SPAC,14,new TGeoCombiTrans(-6.7,0,-28.8,GetRot("90YD")));
       FREO->AddNode(SPAC,15,new TGeoCombiTrans(6.7,0,-43.2,GetRot("90YD")));
       FREO->AddNode(SPAC,16,new TGeoCombiTrans(-6.7,0,-43.2,GetRot("90YD")));
       FREO->AddNode(SPAC,17,new TGeoCombiTrans(6.7,0,-57.6,GetRot("90YD")));
       FREO->AddNode(SPAC,18,new TGeoCombiTrans(-6.7,0,-57.6,GetRot("90YD")));
     SRIC->AddNode(OQUF,2,new TGeoTranslation(21.65,-1.25,0));
     SRIC->AddNode(QUAR,1,new TGeoTranslation(-21.6,-0.5,0));
      QUAR->AddNode(BARR,1,new TGeoTranslation(0,0,-21.7));
      QUAR->AddNode(BARR,2,new TGeoTranslation(0,0,21.7));
     SRIC->AddNode(QUAR,2,new TGeoTranslation(21.6,-0.5,0));
     SRIC->AddNode(RGAP,1,new TGeoTranslation(0,7.55,0));
     SRIC->AddNode(ALMF,1,new TGeoTranslation(0,9.35,0));
      ALMF->AddNode(HOLE,1,new TGeoTranslation(-21.65,0,-33.5));
       HOLE->AddNode(BORD,1,new TGeoTranslation(0,-1.35,0));
       HOLE->AddNode(RCSI,1,new TGeoTranslation(0,-1.575,0));
      ALMF->AddNode(HOLE,2,new TGeoTranslation(21.65,0,-33.5));
      ALMF->AddNode(HOLE,3,new TGeoTranslation(-21.65,0,33.5));
      ALMF->AddNode(HOLE,4,new TGeoTranslation(21.65,0,33.5));
  return RICH;
}
#endif
