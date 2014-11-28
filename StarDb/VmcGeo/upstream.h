// $Id: upstream.h,v 1.3 2008/08/27 21:48:18 fisyak Exp $
// $Log: upstream.h,v $
// Revision 1.3  2008/08/27 21:48:18  fisyak
// *** empty log message ***
//
// the geometry  of the UPSTREAM AreA
#ifdef upstreamConfig
TGeoVolume *upstream() {
TGeoVolume *UPST = gGeoManager->MakeTube("UPST",GetMed("UPST_STANDARD"),0,40,385.63); UPST->SetTitle("the upstream mother volume in the STAR cave");
  UPST->SetVisibility(0);
  UPST->SetLineColor(2);
TGeoVolume *PUPD = gGeoManager->MakeTube("PUPD",GetMed("UPST_IRON"),0,6.35,105.5); PUPD->SetTitle("the Beam PIPe before the DX magnet");
TGeoVolume *PVAD = gGeoManager->MakeTube("PVAD",GetMed("UPST_VACUUM"),0,6.08,105.5); PVAD->SetTitle("the Vacuum Volume of the PIPe before the DX magnet");
TGeoVolume *PUPE = gGeoManager->MakeTube("PUPE",GetMed("UPST_IRON"),0,7.14,207.92); PUPE->SetTitle("the Beam PIPe through the DX mAgnet Volume");
TGeoVolume *PVAE = gGeoManager->MakeTube("PVAE",GetMed("UPST_VACUUM"),0,6.99,207.92); PVAE->SetTitle("the Vacuum Volume of DX mAgnet pipe");
TGeoVolume *PUPF = gGeoManager->MakeTube("PUPF",GetMed("UPST_IRON"),9.53,10.16,207.92); PUPF->SetTitle("the Outer PIPe through the DX mAgnet Volume");
  PUPF->SetLineColor(2);
TGeoVolume *DXMG = gGeoManager->MakeTube("DXMG",GetMed("UPST_IRON"),15.34,37,207.92); DXMG->SetTitle("the return yoke for the DX mAgnet");
  DXMG->SetLineColor(3);
TGeoVolume *DCON = gGeoManager->MakeCone("DCON",GetMed("UPST_IRON"),21.21,0,7.77,0,15.24); DCON->SetTitle("the beam pipe Bell section at the end of DX");
  DCON->SetLineColor(4);
TGeoVolume *DVAC = gGeoManager->MakeCone("DVAC",GetMed("UPST_VACUUM"),21.21,0,7.14,0,14.6); DVAC->SetTitle("its cavity");
  DVAC->SetLineColor(4);
TGeoVolume *PUPG = gGeoManager->MakeTube("PUPG",GetMed("UPST_IRON"),0,15.24,51); PUPG->SetTitle("the Beam PIPe After the DX magnet Volume");
  PUPG->SetLineColor(4);
TGeoVolume *PVAG = gGeoManager->MakeTube("PVAG",GetMed("UPST_VACUUM"),0,14.6,51); PVAG->SetTitle("the Vacuum Volume of the pipe after the DX magnet");
  PVAG->SetLineColor(4);
   UPST->AddNode(PUPD,1,new TGeoTranslation(0,0,-280.13));
    PUPD->AddNode(PVAD,1,gGeoIdentity);
   UPST->AddNode(PUPE,1,new TGeoTranslation(0,0,33.28999));
    PUPE->AddNode(PVAE,1,gGeoIdentity);
   UPST->AddNode(PUPF,1,new TGeoTranslation(0,0,33.28999));
   UPST->AddNode(DXMG,1,new TGeoTranslation(0,0,33.28999));
   UPST->AddNode(DCON,1,new TGeoTranslation(0,0,262.42));
    DCON->AddNode(DVAC,1,gGeoIdentity);
   UPST->AddNode(PUPG,1,new TGeoTranslation(0,0,334.63));
    PUPG->AddNode(PVAG,1,gGeoIdentity);
  return UPST;
}
#endif
