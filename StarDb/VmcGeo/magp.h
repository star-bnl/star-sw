// $Id: magp.h,v 1.3 2008/08/27 21:48:15 fisyak Exp $
// $Log: magp.h,v $
// Revision 1.3  2008/08/27 21:48:15  fisyak
// *** empty log message ***
//
// the geometry of the STAR magnet
#ifdef magpConfig
TGeoVolume *magp() {
TGeoVolume *MAGP = gGeoManager->MakePcon("MAGP",GetMed("MAGP_STANDARD"),0,360,6); MAGP->SetTitle("the magnet mother");
  ((TGeoPcon*)MAGP->GetShape())->DefineSection(0,-357.5,90.028,364.29);
  ((TGeoPcon*)MAGP->GetShape())->DefineSection(1,-310.007,90.028,364.29);
  ((TGeoPcon*)MAGP->GetShape())->DefineSection(2,-310.007,264.9,364.29);
  ((TGeoPcon*)MAGP->GetShape())->DefineSection(3,310.007,264.9,364.29);
  ((TGeoPcon*)MAGP->GetShape())->DefineSection(4,310.007,90.028,364.29);
  ((TGeoPcon*)MAGP->GetShape())->DefineSection(5,357.5,90.028,364.29);
  MAGP->SetVisibility(0);
TGeoVolume *COIL = gGeoManager->MakeTube("COIL",GetMed("MAGP_STANDARD"),264.9,299.3,313.7); COIL->SetTitle("the main coil mother");
  COIL->SetVisibility(0);
  COIL->SetLineColor(2);
TGeoVolume *MCSE = gGeoManager->MakeTube("MCSE",GetMed("MAGP_ALUMINIUM"),264.9,299.3,22.62); MCSE->SetTitle("a single barrel coil");
  MCSE->SetLineColor(3);
TGeoVolume *MCSE_MCS1 = gGeoManager->MakeTube("MCSE",GetMed("MAGP_ALUMINIUM"),264.9,299.3,11.355); MCSE_MCS1->SetTitle("a single barrel coil");
  MCSE_MCS1->SetLineColor(3);
TGeoVolume *MRET = gGeoManager->MakeTube("MRET",GetMed("MAGP_STANDARD"),303.29,364.29,342.2); MRET->SetTitle("Magnet RETurn Yoke");
  MRET->SetVisibility(0);
  MRET->SetLineColor(3);
TGeoVolume *MBAR = gGeoManager->MakeTrd1("MBAR",GetMed("MAGP_IRON"),22.17,28.575,342.2,30); MBAR->SetTitle("a single return yoke bar");
  MBAR->SetLineColor(3);
TGeoVolume *MPTV = gGeoManager->MakePcon("MPTV",GetMed("MAGP_IRON"),0,360,3); MPTV->SetTitle("the magnet pole-tip volume");
  ((TGeoPcon*)MPTV->GetShape())->DefineSection(0,0,91.34,252.882);
  ((TGeoPcon*)MPTV->GetShape())->DefineSection(1,21.27043,91.34,252.882);
  ((TGeoPcon*)MPTV->GetShape())->DefineSection(2,47.49301,98.5701,252.882);
  MPTV->SetLineColor(6);
TGeoVolume *MPCV = gGeoManager->MakeTube("MPCV",GetMed("MAGP_ALUMINIUM"),91.34,152.4,9); MPCV->SetTitle("the coil cavity in the pole-tip (filled with cables ?)");
  MPCV->SetLineColor(7);
TGeoVolume *MTCL = gGeoManager->MakeTube("MTCL",GetMed("MAGP_ALUMINIUM"),91.34,141.28,8.25); MTCL->SetTitle("tRIM COIL Volume (filled with aluminum)");
  MTCL->SetLineColor(3);
TGeoVolume *MRGV = gGeoManager->MakePcon("MRGV",GetMed("MAGP_IRON"),0,360,4); MRGV->SetTitle("the Magnet Return rinG");
  ((TGeoPcon*)MRGV->GetShape())->DefineSection(0,0,263.68,303.29);
  ((TGeoPcon*)MRGV->GetShape())->DefineSection(1,28.5,263.68,303.29);
  ((TGeoPcon*)MRGV->GetShape())->DefineSection(2,28.5,263.68,364.29);
  ((TGeoPcon*)MRGV->GetShape())->DefineSection(3,43.79999,263.68,364.29);
  MRGV->SetLineColor(6);
   MAGP->AddNode(COIL,1,gGeoIdentity);
    COIL->AddNode(MCSE,1,new TGeoTranslation(0,0,30.95));
    COIL->AddNode(MCSE,2,new TGeoTranslation(0,0,-30.95));
    COIL->AddNode(MCSE,3,new TGeoTranslation(0,0,89.05));
    COIL->AddNode(MCSE,4,new TGeoTranslation(0,0,-89.05));
    COIL->AddNode(MCSE,5,new TGeoTranslation(0,0,147.17));
    COIL->AddNode(MCSE,6,new TGeoTranslation(0,0,-147.17));
    COIL->AddNode(MCSE,7,new TGeoTranslation(0,0,205.25));
    COIL->AddNode(MCSE,8,new TGeoTranslation(0,0,-205.25));
    COIL->AddNode(MCSE_MCS1,9,new TGeoTranslation(0,0,249));
    COIL->AddNode(MCSE_MCS1,10,new TGeoTranslation(0,0,-249));
    COIL->AddNode(MCSE,11,new TGeoTranslation(0,0,288.35));
    COIL->AddNode(MCSE,12,new TGeoTranslation(0,0,-288.35));
   MAGP->AddNode(MRET,1,gGeoIdentity);
    TGeoVolume *MSEC = MRET->Divide("MSEC",2,30,0,12); MSEC->SetTitle("MSEC");
     MSEC->AddNode(MBAR,1,new TGeoCombiTrans(333.29,0,0,GetRot("90XD")));
   MAGP->AddNode(MPTV,1,new TGeoTranslation(0,0,310.007));
    MPTV->AddNode(MPCV,1,new TGeoTranslation(0,0,9));
     MPCV->AddNode(MTCL,1,new TGeoTranslation(0,0,-0.75));
   MAGP->AddNode(MPTV,2,new TGeoCombiTrans(0,0,-310.007,GetRot("180R")));
   MAGP->AddNode(MRGV,1,new TGeoTranslation(0,0,313.7));
   MAGP->AddNode(MRGV,2,new TGeoCombiTrans(0,0,-313.7,GetRot("180R")));
  return MAGP;
}
#endif
