#include "tests/unit_tests.h"

#include "StEpdUtil/StEpdGeom.h"

//___________________________________________________________________
double _eta  = 0; 
double _phid = 0;
//___________________________________________________________________
void throw_muon_in_epd_tile( double eta, double phid, int charge = 1 ) {

  throw_muon( eta, phid, 10.0, charge ); // energetic
  _eta = eta;
  _phid = phid;

  auto* chain = StMaker::GetChain();
  vertex_table = dynamic_cast<TTable*>( chain->GetDataSet("g2t_vertex")  );
  track_table  = dynamic_cast<TTable*>( chain->GetDataSet("g2t_track")   );
  hit_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_epd_hit") );

}

//___________________________________________________________________
void unit_test_epd_hits() {

  gROOT->ProcessLine("initChain();");

  if ( 0 == hasRuntimeArg("epdutil") ) {
    std::cout << "Please re-run with --epdutil option" << std::endl;
    assert(0);
  }

  StEpdGeom epd;

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.,0.,0.);
  pm->SetSigma(0.0,0.,0.);

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Unit testing of tracks and EPD hits on single muons"     << std::endl;
  LOG_TEST << "=======================================================" << std::endl;

  TVector3 direction;

  for ( int eastwest=-1; eastwest<=+1; eastwest+=2 ) {
    for ( int supersector=1; supersector<=12; supersector++ ) {
      for ( int tilenumber=1; tilenumber<=31; tilenumber++ ) {

      LOG_TEST << "------------------------------------------------------------------" << std::endl;
      LOG_TEST << "eastwest    = " << eastwest << std::endl;
      LOG_TEST << "supersector = " << supersector << std::endl;
      LOG_TEST << "tilenumber  = " << tilenumber << std::endl;

      /* from g2t_volume_id
	 " EPD volume_id "                                                                                                                   
	 " 100,000 : east or west "                                                                                                          
	 "   1,000 : Position clock wise, 1 to 12 "                                                                                          
	 "      10 : Tile number 1 to 31, refer EPD Drupal page"                                                                             
	 "       1 : 1 T1 trap or T2 thin; 0 T1 triangular or T2 thick  
      */

      int expected_id = 
	100000 * (3+eastwest)/2  +
	  1000 * supersector     +
	    10 * tilenumber      ;
      // we cannot target further...	
      direction = epd.TileCenter( supersector, tilenumber, eastwest );
     		      
      throw_muon_in_epd_tile( direction.Eta(), direction.Phi()*180.0/TMath::Pi() );
  
      check_track( "A muon must have been processed by geant",       [=](const g2t_track_st* t){
       	  return PASS; 
      	});
      check_track( "The track should have a start vertex",           [=](const g2t_track_st* t){
       	  return (t->start_vertex_p>0)?PASS:FAIL;      
	});
      check_track( "The start vertex should be in the vertex table", [=](const g2t_track_st* t){
       	  std::string result = FAIL;
       	  int istart = t->start_vertex_p;
       	  const g2t_vertex_st* vertex = (istart>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istart-1) ) : 0;
       	  if ( vertex ) {
       	    result = PASS;
       	  }
       	  return result;
       	});
      check_track( "There should not be a stop vertex in the EPD",   [=](const g2t_track_st* t){
       	  std::string result = TODO;
       	  return result;
       	});
      check_track( "The start vertex should be on the z-axis",       [=](const g2t_track_st* t){
       	  std::string result = FAIL;
       	  int istart = t->start_vertex_p;
	  
       	  const g2t_vertex_st* v = 0;
       	  if ( istart > 0 ) 
       	    v = static_cast<const g2t_vertex_st*>( vertex_table->At(istart-1) );
       	  else 
       	    result = " no vertex in table " + result;
	  
       	  if ( v ) {
       	    double x1 = v->eg_x[0];
       	    double y1 = v->eg_x[1];
       	    double z1 = v->eg_x[2];
       	    double x2 = v->ge_x[0];
       	    double y2 = v->ge_x[1];
       	    double z2 = v->ge_x[2];
       	    double xx1 = sqrt(x1*x1+y1*y1); // event generator
       	    double xx2 = sqrt(x2*x2+y2*y2); // geant vertex
       	    if ( xx1 > 0.0001 ) {
       	      result = Form(" EG: %f %f %f ",x1,y1,z1) + result;
	    } 
	    if ( xx2 > 0.0001 ) {
       	  result = Form(" GE: %f %f %f ",x2,y2,z2) + result;
       	    }
       	    if ( xx1 < 0.0001 && xx2 < 0.0001 ) {
       	      result = PASS;
       	    }	
       	  }
	  
       	  return result;
       	});
      check_track( "The track should be primary",                    [=](const g2t_track_st* t){
       	  std::string result          = PASS;
       	  if ( t->eta ==-999 ) result = FAIL;
       	  return result;
       	});
      check_track( Form("The track should have an eta=%f",_eta),     [=](const g2t_track_st* t){
       	  double delta = TMath::Abs(t->eta-_eta);	
       	  return TMath::Abs(t->eta-_eta)<1E-5 ?PASS:FAIL;      
       	});
      check_track( "Expect 1 hit in the dev2021 geometry",           [=](const g2t_track_st* t){
       	  int n = t->n_epd_hit;
       	  std::string  result = FAIL;
       	  if ( n==1 )  result = PASS;
       	  result = Form(" n=%i ",n) + result;
       	  return result;
       	}); 
      for ( int i=0;i<hit_table->GetNRows();i++ ) {

	auto hit = static_cast<const g2t_epd_hit_st*>( hit_table->At(i) );
	if ( 0==hit ) continue;     // skip null entries
	if ( 1!=hit->track_p ) continue; // not interested in secondaries
	
	check_epd_hit( "Print the hit...", hit, [=](const g2t_epd_hit_st* h) {
	    LOG_TEST << "id=" << h->id 
		     << " track_p=" << h->track_p 
		     << " volume_id=" << h->volume_id 
		     << " de="  << h->de 
			     << std::endl;
	    return PASS;
	  });

	check_epd_hit( "The hit should have a nonzero volume_id",hit,[=](const g2t_epd_hit_st* h) {
	    std::string result = FAIL;
	    if ( h->volume_id > 0 ) result = PASS;
	    result = Form(" volumeId=%i ", h->volume_id ) + result;
	    return result;
	  });
	check_epd_hit( "The decoded side from volume_id should be 1 or 2 (E or W)",hit,[=](const g2t_epd_hit_st* h) {
	    std::string result = FAIL;
	    int ew = h->volume_id / 100000;
	    if ( ew == 1 || ew == 2 ) result = PASS;
	    result = Form("(eastwest=%i) ",ew) + result;
	    return result;
	  });
	check_epd_hit( "The decoded position from volume_id should be 1..12",hit,[=](const g2t_epd_hit_st* h) {
	    std::string result = FAIL;
	    int ew = h->volume_id % 100000 / 1000;
	    if ( ew >= 1 && ew <= 12 ) result = PASS;
	    result = Form("(position=%i) ",ew) + result;
	    return result;
	  });
	check_epd_hit( "The decoded tile from volume_id should be 1..31",hit,[=](const g2t_epd_hit_st* h) {
	    std::string result = FAIL;
	    int ew = h->volume_id % 1000 / 10;
	    if ( ew >= 1 && ew <= 31 ) result = PASS;
	    result = Form("(tile=%i) ",ew) + result;
	    return result;
	  });
	check_epd_hit( "The decoded tile shape from volume_id should be 0..1",hit,[=](const g2t_epd_hit_st* h) {
	    std::string result = FAIL;
	    int ew = h->volume_id % 10;
	    if ( ew == 1 || ew == 0 ) result = PASS;
	    result = Form("(in/out=%i) ",ew) + result;
	    return result;
	  });
	check_epd_hit( Form("The volume_id should be %i or %i",expected_id,expected_id+1),hit,[=](const g2t_epd_hit_st* h) {
	    std::string result = FAIL;
	    int ew = h->volume_id;
	    if      ( ew == expected_id   ) result=PASS;
	    else if ( ew == expected_id+1 ) result=PASS;
	    result = Form("(expected=%i (or+1) volumeId=%i eta=%f phi=%f) ",expected_id,ew,_eta,_phid) + result;
	    return result;
	  });
	
       }
      
      }

    }

  }

}
