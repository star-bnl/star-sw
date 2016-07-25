// $Id: starTestConfig.C,v 1.1 2006/01/21 17:51:40 fisyak Exp $
void Config(void)
{
  StTGeant3* geant3 = new StTGeant3("C++ Interface to Geant3"); 
  cout << "Geant3 has been created." << endl;


  if(geoFile) {  // Notify VMC that we are taking the geometry from file
    geant3->SetRootGeometry();     cout<<"********* IMPORT  *********"<<geoFile<<endl;
    TGeoManager::Import(geoFile);
  }

  geant3->SetHADR(0);
}


