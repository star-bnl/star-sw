TObjectSet* materialSets = 0;

TH1F*       materialPlot(const char* volume) { return (TH1F*)((TObjectSet*)materialSet->Find(volume))->GetObject(); }


void makeMaterialPlot( const char* tag="dev2021" ) {
  gROOT->Macro("StarVMC/Geometry/macros/loadAgML.C");
  StarGeometry::Whitelist("all",0);
  StarGeometry::Whitelist("EPDM",1);
  StarGeometry::Whitelist("BBCM",1);
  StarGeometry::Whitelist("PIPE",1);
  StarGeometry::Whitelist("STGM",1);
  StarGeometry::Whitelist("FTSM",1);
  StarGeometry::Whitelist("FSTM",1);
  // StarGeometry::Whitelist("WCAL",1);
  // StarGeometry::Whitelist("HCAL",1);
  // StarGeometry::Whitelist("PRES",1);
  construct(tag);

  TGeoChecker check; // trick ROOT into loading TGeoChecker
  gSystem->Load("StarAgmlChecker.so");
  StarAgmlChecker* agmlChecker = new StarAgmlChecker(gGeoManager);


  int    nEtaBins= 100;
  double minEta  = 2.0;
  double maxEta  = 5.0;
  int    nPhiBins= 180;
  double minPhi  = -TMath::Pi();
  double maxPhi  = +TMath::Pi();
  double rmin    = 0;
  double rmax    = 50;
  double zmin    = -4000;
  double zmax    = +4000;

  materialSets = agmlChecker->MaterialPlot("CAVE",nEtaBins,minEta,maxEta,nPhiBins,minPhi,maxPhi,rmin,rmax,zmin,zmax);

}
