void loadAgML( const char* name=0, char* opts="" )
{
  gROOT->LoadMacro("bfc.C");
//  bfc(0,"agml nodefault mysql db detdb misalign sdt20150216");
  bfc(0,Form("agml nodefault mysql ideal %s",opts));

  AgModule::SetStacker( new StarTGeoStacker() );
AgPosition::SetDebug(2); 

  if ( 0==name ) {
     cout << "Usage: "<< endl;
     cout << "  .L loadAgML.C" << endl; 
     cout << "  loadAgML(\"y2015a\"); // standard " << endl;
     cout << "  loadAgML(\"y2015x\",\"misalign sdt20150216\"); // misaligned" << endl; 
     cout << endl;
     cout << "  loadAgML(); " << endl;
     cout << "  list(\"y2015a\");       " << endl;
     cout << "  construct(\"y2015a\");       " << endl;
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
gGeoManager->SetVisLevel(10); 

}

void list( const char* tag ) {
   StarGeometry::List(tag); 
}
