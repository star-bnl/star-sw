void loadAgML( const char* name=0, char* opts="" )
{
  gROOT->LoadMacro("bfc.C");
//  bfc(0,"agml nodefault mysql db detdb misalign sdt20150216");
  bfc(0,Form("agml nodefault mysql %s",opts));

  AgModule::SetStacker( new StarTGeoStacker() );

  if ( 0==name ) {
     cout << "Usage: "<< endl;
     cout << "  .L loadAgML.C" << endl; 
     cout << "  loadAgML(\"y2015a\"); // standard " << endl;
     cout << "  loadAgML(\"y2015x\",\"misalign sdt20150216\"); // misaligned" << endl; 
  }

  //  StarGeometry::y2015a::select();
  //  StarGeometry::AgML::construct();
  if (name) construct(name);
 // if (export) gGeoManager->Export( export );

}

void construct(const char * name ) {
  //  Geometry *geometry = new Geometry();
  //  geometry -> ConstructGeometry( name );

  StarGeometry::Construct(name);

}
