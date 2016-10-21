void loadAgML( const char* name=0, const char* export=0 )
{
  gROOT->LoadMacro("bfc.C");
  bfc(0,"agml nodefault mysql");

  AgModule::SetStacker( new StarTGeoStacker() );

  //  StarGeometry::y2015a::select();
  //  StarGeometry::AgML::construct();
  if (name) construct(name);
  if (export) gGeoManager->Export( export );

}

void construct(const char * name ) {
  //  Geometry *geometry = new Geometry();
  //  geometry -> ConstructGeometry( name );

  StarGeometry::Construct(name);

}
