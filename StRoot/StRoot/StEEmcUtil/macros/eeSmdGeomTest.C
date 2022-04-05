void eeSmdGeomTest() {

  // Test that the new StEEmcSmdGeom class behaves the same as 
  // the original

  gROOT -> LoadMacro("libs.C");
  libs();

  StEEmcSmdGeom *smdGeom = StEEmcSmdGeom::instance();
  smdGeom -> printGeom();


  for ( Int_t i = 0; i < 12; i++ ) {

    smdGeom -> printSector( smdGeom -> getEEmcSector(0, i) );
    smdGeom -> printSector( smdGeom -> getEEmcSector(1, i) );

    smdGeom -> printSectorPhis( 0, i );
    smdGeom -> printSectorPhis( 1, i );
    smdGeom -> printSectorPhis( 2, i );

  }



  for ( Int_t isector = 0; isector < 12; isector++ ) { 

    for ( Int_t istrip = 0; 
	  istrip < smdGeom -> getNStrips(isector,0); 
	  istrip+=36 ) { 

      smdGeom -> printStrip(*smdGeom -> getStripPtr(istrip,0,isector));
      smdGeom -> printStrip(*smdGeom -> getStripPtr(istrip,1,isector));

    }

  }
  



}
