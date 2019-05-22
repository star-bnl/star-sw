// $Id$
//
// Configuration macro for Geant3 VMC for Example01 

void Config()
{
  if (strstr(gEnv->GetValue("TVirtualMC",""),"TGeant3TGeo")) {
     new  TGeant3TGeo("C++ Interface to Geant3 using TGeo");
     cout << "TGeant3TGeo has been created." << endl;
  } else {
     new  TGeant3("C++ Interface to Geant3");
     cout << "TGeant3 has been created." << endl;
  }
}


