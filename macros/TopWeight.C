TDataSet *CreateTable();
//________________________________________________________________________________
void TopWeight(const Char_t *tag = "y2007g") {
  const Double_t lb2kg = 0.45359237;
  const Int_t NVolumes = 55;
  const Char_t *topVolumeNames[55] = {
    "HALL","CAVE","PIPE","UPST","SVTT","SCON","SFMO","FTPC","SUPO","FTMO",
    "FTCM","BTOF","VPDD","CALB","ECAL","BBCM","FBOX","ZCAL","MAGP","FGMO",
    "PHMD","PXMO","RICH","IBMO","FGMO","GMBO","MGMT","SHLD","MUTD","IGMO",
    "YPXM","ITSP","IBSH","IBSG","IBSF","IBSE","IBSD","IBSC","IBSB","IBSA",
    "IBEM","IBEH","DUMM","ITSP:ITS1","FSMO","TPCE","TPCM","TIFC","TOFC","TPEA",
    "TRDV","TPCW","TWSS","TWGI","TWGI:TWG1"};

  if (gClassTable->GetID("TGeoManager") < 0) {
    gSystem->Load("libGeom");
  }
  if (! gGeoManager) {
    gROOT->LoadMacro(Form("/afs/rhic.bnl.gov/star/packages/.DEV2/StarDb/VmcGeometry/Geometry.%s.C",tag));
    CreateTable();
  }
  if (! gGeoManager) return;
  cout << "Geometry " << gGeoManager->GetName() << endl;
  for (Int_t i = 0; i < NVolumes; i++) {
    TGeoVolume *vol = gGeoManager->GetVolume(topVolumeNames[i]);
    if (! vol) continue;
    Double_t WA = vol->Weight();
    cout << vol->GetName() << "\t" << WA << "[kg]\t" << WA/lb2kg << "[lb]" << endl;
#if 0
	 << "_Analytical  = " 
    Double_t W = vol->Weight(1e-2,"");
    cout << vol->GetName() << "_Statistical = " << W << "[kg]\t" << W/lb2kg << "[lb]" << endl;
#endif
  }
}
  
  
