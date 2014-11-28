// $Id: support.h,v 1.3 2008/08/27 21:48:17 fisyak Exp $
// $Log: support.h,v $
// Revision 1.3  2008/08/27 21:48:17  fisyak
// *** empty log message ***
//
// the geometry of the Forward TPC supports in STAR
#ifdef supportConfig
TGeoVolume *support() {
TGeoVolume *SUPO = gGeoManager->MakeTube("SUPO",GetMed("SUPO_STANDARD"),36.4,47.5,14.505); SUPO->SetTitle("the FTPC support mother volume");
  SUPO->SetVisibility(0);
TGeoVolume *SUPL = gGeoManager->MakeTubs("SUPL",GetMed("SUPO_STANDARD"),36.4,47.5,14.505,-11,6); SUPL->SetTitle("the lower FTPC support mother volume");
  SUPL->SetVisibility(0);
TGeoVolume *SLRL = gGeoManager->MakeTrd1("SLRL",GetMed("SUPO_ALUMINIUM"),2.035,3.515,12.25,1.48); SLRL->SetTitle("the lower FTPC support rail");
  SLRL->SetLineColor(2);
TGeoVolume *SLWL = gGeoManager->MakeBox("SLWL",GetMed("SUPO_ALUMINIUM"),2.23,0.39,10.91); SLWL->SetTitle("the lower FTPC support side wall");
  SLWL->SetLineColor(2);
TGeoVolume *SLHD = gGeoManager->MakeBox("SLHD",GetMed("SUPO_ALUMINIUM"),3.91,3.515,1.34); SLHD->SetTitle("the lower FTPC support head plate (mounted to TPC)");
  SLHD->SetLineColor(2);
TGeoVolume *SLXW = gGeoManager->MakeBox("SLXW",GetMed("SUPO_ALUMINIUM"),2.23,2.735,0.41); SLXW->SetTitle("the lower FTPC support cross wall");
  SLXW->SetLineColor(2);
TGeoVolume *SLEN = gGeoManager->MakeBox("SLEN",GetMed("SUPO_ALUMINIUM"),0.745,2.735,1.3); SLEN->SetTitle("the lower FTPC support end block");
  SLEN->SetLineColor(2);
TGeoVolume *SLFX = gGeoManager->MakeBox("SLFX",GetMed("SUPO_ALUMINIUM"),1.895,5.11,1.02); SLFX->SetTitle("the lower FTPC support fixture plate");
  SLFX->SetLineColor(2);
TGeoVolume *SLBL = gGeoManager->MakeTube("SLBL",GetMed("SUPO_ALUMINIUM"),0,1.2,1.235005); SLBL->SetTitle("the lower FTPC support bolt");
  SLBL->SetLineColor(2);
TGeoVolume *SUPH = gGeoManager->MakeTubs("SUPH",GetMed("SUPO_STANDARD"),36.4,47.5,14.505,25,52); SUPH->SetTitle("the upper FTPC support mother volume");
  SUPH->SetVisibility(0);
TGeoVolume *SHRL = gGeoManager->MakeBox("SHRL",GetMed("SUPO_ALUMINIUM"),0.875,0.965,14.505); SHRL->SetTitle("the upper FTPC support rail");
  SHRL->SetLineColor(2);
TGeoVolume *SHPT = gGeoManager->MakeTrap("SHPT",GetMed("SUPO_ALUMINIUM"),0.205,0,0,4.345,14.505,5.29,34.60078,4.345,14.505,5.29,34.60078); SHPT->SetTitle("the upper FTPC support main plate");
  SHPT->SetLineColor(2);
TGeoVolume *SHBR = gGeoManager->MakeBox("SHBR",GetMed("SUPO_ALUMINIUM"),1.29,0.405,5.29); SHBR->SetTitle("the upper FTPC support top bar");
  SHBR->SetLineColor(2);
TGeoVolume *SHBK = gGeoManager->MakeBox("SHBK",GetMed("SUPO_ALUMINIUM"),1.29,1.37,2.75); SHBK->SetTitle("the upper FTPC support top block");
  SHBK->SetLineColor(2);
TGeoVolume *SHFX = gGeoManager->MakeBox("SHFX",GetMed("SUPO_G10"),1.895,5.11,1.02); SHFX->SetTitle("the upper FTPC support fixture plate");
  SHFX->SetLineColor(3);
TGeoVolume *SHST = gGeoManager->MakeBox("SHST",GetMed("SUPO_ALUMINIUM"),0.575,4.345,0.205); SHST->SetTitle("the upper FTPC support stabilizers");
  SHST->SetLineColor(2);
   rot = new TGeoRotation("next",90,-30,90,60,0,0);
   SUPO->AddNode(SUPL,1,rot);
    SUPL->AddNode(SLRL,1,new TGeoCombiTrans(37.88,0,2.255005,GetRot("90XD")));
    SUPL->AddNode(SLWL,1,new TGeoTranslation(41.59,3.125,3.595005));
    SUPL->AddNode(SLWL,2,new TGeoTranslation(41.59,-3.125,3.595005));
    SUPL->AddNode(SLHD,1,new TGeoTranslation(43.27,0,-8.654995));
    SUPL->AddNode(SLXW,1,new TGeoTranslation(41.59,0,0.55));
    SUPL->AddNode(SLXW,2,new TGeoTranslation(41.59,0,7.02));
    SUPL->AddNode(SLEN,1,new TGeoTranslation(40.105,0,13.205));
    SUPL->AddNode(SLFX,1,new TGeoTranslation(44.9,-1.54,-13.485));
    SUPL->AddNode(SLBL,1,new TGeoTranslation(44.9,0,-11.23));
   rot = new TGeoRotation("next",270,30,90,120,0,0);
   SUPO->AddNode(SUPL,2,rot);
   SUPO->AddNode(SUPH,1,gGeoIdentity);
    SUPH->AddNode(SHRL,1,new TGeoTranslation(33.52,17.355,0));
    SUPH->AddNode(SHPT,1,new TGeoCombiTrans(34.19,22.665,-3.22,GetRot("90YX")));
    SUPH->AddNode(SHBR,1,new TGeoTranslation(34.19,27.415,-5.995005));
    SUPH->AddNode(SHBK,1,new TGeoTranslation(34.19,29.19,-9.715005));
    SUPH->AddNode(SHFX,1,new TGeoCombiTrans(31.88483,31.49517,-13.485,GetRot("R045")));
    SUPH->AddNode(SHST,1,new TGeoTranslation(33.41,22.665,-6.185005));
    SUPH->AddNode(SHST,2,new TGeoTranslation(34.97,22.665,-6.185005));
    SUPH->AddNode(SHST,3,new TGeoTranslation(33.41,22.665,-0.925005));
    SUPH->AddNode(SHST,4,new TGeoTranslation(34.97,22.665,-0.925005));
   SUPO->AddNode(SUPH,2,GetRot("D270"));
  return SUPO;
}
#endif
