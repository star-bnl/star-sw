// $Id: E03_g3Config.C,v 1.1.1.2 2009/02/01 17:10:15 fisyak Exp $
//
// Configuration macro for Geant3 VirtualMC for Example03

void Config()
{
#if 0
  if (strstr(gEnv->GetValue("TVirtualMC",""),"TGeant3TGeo")) {
#endif
     TGeant3TGeo* geant3 = new  TGeant3TGeo("C++ Interface to Geant3 using TGeo");
     cout << "TGeant3TGeo has been created." << endl;
#if 0
  } else {
     TGeant3* geant3 = new  TGeant3("C++ Interface to Geant3");
     cout << "TGeant3 has been created." << endl;
  }

  geant3->SetDRAY(1);
  geant3->SetLOSS(1);
  geant3->SetHADR(0);
#endif
}


