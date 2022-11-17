void utgeo_create_tag() {

  if ( gApplication->Argc() > 2 ) {
    gROOT->LoadMacro("bfc.C");
    TString mychain = gApplication->Argv()[2];
    mychain += " agml nodefault";
    bfc(0,mychain.Data());
    chain->GetDataBase("VmcGeometry");
    TString msg    = Form("[utgeo_create_tag] %s ",gApplication->Argv()[2]);
    TString result = "SUCCESS";
    if ( 0==gGeoManager ) {
      result = "FAIL";
    }
    else if ( gGeoManager->GetListOfVolumes()->GetSize() < 100 ) {
      result = "FAIL";
    }
    msg = msg + result;
    std::cout << msg.Data() << std::endl;
    
  }
  else {
    std::cout << "Usage: " << std::endl;
    std::cout << "   root4star utgeo_create_tag.C y2022a   ... or other tag" << std::endl;
  }
  gROOT->ProcessLine(".q");

};
