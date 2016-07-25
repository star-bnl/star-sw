void Geant() {
 gROOT->LoadMacro("bfc.C");
 bfc(0,"gstar");
 St_geant_Maker *geant = (St_geant_Maker *) chain->Maker("geant");
 geant->SetDebug(2);
  geant->Do("DCAY 0");
  geant->Do("ANNI 0");
  geant->Do("BREM 0");
  geant->Do("COMP 0");
  geant->Do("HADR 0");
  geant->Do("MUNU 0");
  geant->Do("PAIR 0");
  geant->Do("PFIS 0");
  geant->Do("PHOT 0");
  geant->Do("RAYL 0");
  geant->Do("LOSS 4"); // no fluctuations 
  //  geant->Do("LOSS 1"); // with delta electron above dcute
  geant->Do("DRAY 0");
  geant->Do("MULS 0");
  geant->Do("STRA 0");
#if 0
  geant->Do("CUTGAM	1e-3  ");
  geant->Do("CUTELE 	1e-3  ");
  geant->Do("CUTHAD 	.001  ");
  geant->Do("CUTNEU 	.001  ");
  geant->Do("CUTMUO 	.001  ");
  geant->Do("BCUTE 	.001  ");
  geant->Do("BCUTM 	.001  ");
  geant->Do("DCUTE 	1e-3  ");
  geant->Do("DCUTM 	.001  ");
  geant->Do("PPCUTM 	.001  ");
  geant->Do("TOFMAX 	50.e-6");
#endif
  geant->Do("gkine 1 5   1.     1. 0.1 0.1 0.57  0.57  0.   0.");
}
