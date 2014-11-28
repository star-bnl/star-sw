// $Id: zcal.h,v 1.3 2008/08/27 21:48:18 fisyak Exp $
// $Log: zcal.h,v $
// Revision 1.3  2008/08/27 21:48:18  fisyak
// *** empty log message ***
//
// region between the DX and the D0 magnets, the geometry of the Zero deg. Quartz Calorimeter
#ifdef zcalConfig
TGeoVolume *zcal() {
TGeoVolume *ZCAL = gGeoManager->MakeTube("ZCAL",GetMed("ZCAL_STANDARD"),0,40,250); ZCAL->SetTitle("the region between the DX and the D0 magnets");
  ZCAL->SetVisibility(0);
TGeoVolume *PIPH = gGeoManager->MakeTube("PIPH",GetMed("ZCAL_IRON"),0,20.96,133.35); PIPH->SetTitle("the Large diameter Pipe before the beam pipes split");
  PIPH->SetLineColor(2);
TGeoVolume *PVAH = gGeoManager->MakeTube("PVAH",GetMed("ZCAL_VACUUM"),0,20,133.35); PVAH->SetTitle("the Vacuum Volume of the large diameter pipe");
  PVAH->SetLineColor(2);
TGeoVolume *PLAT = gGeoManager->MakeTube("PLAT",GetMed("ZCAL_IRON"),0,20.96,0.47); PLAT->SetTitle("the End Plate of the large dia. Pipe");
  PLAT->SetLineColor(2);
TGeoVolume *PLVA = gGeoManager->MakeTube("PLVA",GetMed("ZCAL_VACUUM"),0,6.35,0.47); PLVA->SetTitle("the Vacuum Volume of the beam pipe holes in the end plate");
  PLVA->SetLineColor(2);
TGeoVolume *QCAL = gGeoManager->MakeBox("QCAL",GetMed("ZCAL_DIRTY_LEAD"),5,5,65); QCAL->SetTitle("the Zero degree calorimeter");
  QCAL->SetLineColor(4);
TGeoVolume *QSCI = gGeoManager->MakeBox("QSCI",GetMed("ZCAL_SCINTILLATOR"),5,5,0.5000000E-01); QSCI->SetTitle("a sensitive Fiber layer");
  QSCI->SetLineColor(3);
TGeoVolume *PIPJ = gGeoManager->MakeTube("PIPJ",GetMed("ZCAL_IRON"),0,6.35,91.5); PIPJ->SetTitle("the final beam Pipes");
  PIPJ->SetLineColor(7);
TGeoVolume *PVAJ = gGeoManager->MakeTube("PVAJ",GetMed("ZCAL_VACUUM"),0,6.07,91.5); PVAJ->SetTitle("the Vacuum Volume of the final beam pipes");
  PVAJ->SetLineColor(7);
   ZCAL->AddNode(PIPH,1,new TGeoTranslation(0,0,-116.65));
    PIPH->AddNode(PVAH,1,gGeoIdentity);
   ZCAL->AddNode(PLAT,1,new TGeoTranslation(0,0,17.17001));
    PLAT->AddNode(PLVA,1,new TGeoTranslation(11.1,0,0));
    PLAT->AddNode(PLVA,2,new TGeoTranslation(-11.1,0,0));
   ZCAL->AddNode(QCAL,1,new TGeoTranslation(0,0,122.64));
    TGeoVolume *QDIV = QCAL->Divide("QDIV",3,260,-65,0.5); QDIV->SetTitle("QDIV");
     QDIV->AddNode(QSCI,1,new TGeoTranslation(0,0,-0.2));
   rot = new TGeoRotation("next",91.074,0,90,90,1.074,0);
   ZCAL->AddNode(PIPJ,1,new TGeoCombiTrans(12.82,0,109.38,rot));
    PIPJ->AddNode(PVAJ,1,gGeoIdentity);
   rot = new TGeoRotation("next",88.926,0,90,90,-1.074,0);
   ZCAL->AddNode(PIPJ,2,new TGeoCombiTrans(-12.82,0,109.38,rot));
  return ZCAL;
}
#endif
