void testGeom( const char* name="y2015a", const char* sdt = "sdt20150216" )
{
  gROOT->LoadMacro("bfc.C");

  if ( sdt == 0 )   bfc(0,"");
  else              {
    bfc(0,Form("%s agml usexgeom %s",name,sdt));
    AgPosition::SetReal();
  }

  AgModule::SetStacker( new StarTGeoStacker() );

  //  StarGeometry::y2015a::select();
  //  StarGeometry::AgML::construct();
  if (name) construct(name);
}

void construct(const char * name ) {
  //  Geometry *geometry = new Geometry();
  //  geometry -> ConstructGeometry( name );

  StarGeometry::Construct(name);

}
