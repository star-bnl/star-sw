#include "tests/unit_tests.h"
//___________________________________________________________________
double _eta  = 0; 
double _phid = 0;
//___________________________________________________________________
//void throw_muon_in_fst_wedge( int wedgeid, int inout, int charge = 1 ) {
void throw_muon_in_fst_wedge( double eta, double phid ) {

  // TODO... 
  //double eta  = 2.8;  
  _eta=eta;
  //double phid = 15.0; 
  _phid=phid;

  throw_muon( eta, phid, 5.0, 1 ); // energetic

  vertex_table = dynamic_cast<TTable*>( chain->GetDataSet("g2t_vertex")  ) ;
  track_table  = dynamic_cast<TTable*>( chain->GetDataSet("g2t_track")   ) ;
  hit_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_fsi_hit") ) ;

}
//___________________________________________________________________
void unit_test_fst_hits() {

  gROOT->ProcessLine("initChain();");

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.,0.,0.);
  pm->SetSigma(0.0,0.,0.);

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Unit testing of tracks and forward silicon hits on single muons"     << std::endl;
  LOG_TEST << "=======================================================" << std::endl;

  double etas[] = { 2.55, 3.00, 3.50,  3.95 };
  double phis[] = { 15.0, 45.0, 75.0, 105.0, 135.0, 165.0, 195.0, 225.0, 255.0, 285.0, 315.0, 345.0 };

  for ( auto e : etas ) {
    for ( auto p : phis ) {

      throw_muon_in_fst_wedge( e, p+1.9 );
      
      check_track( "A muon must have been processed by geant",       [=](const g2t_track_st* t){
	  // Failure is tested by check_track when it tests for a valid track pointer
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
      check_track( "There should not be a stop vertex in the FSI",   [=](const g2t_track_st* t){
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
      check_track( "Expect 3 hits in the dev2021 geometry",          [=](const g2t_track_st* t){
	  int n = t->n_fts_hit;
	  std::string  result = FAIL;
	  if ( n==3 ) 
	    result = PASS;
	  else if ( n==4 ) 
	    result = UNKN;
	  result = Form(" n=%i ",n) + result;
	  return result;
	});
  
      for ( int i=0;i<hit_table->GetNRows();i++ ) {
	auto hit = static_cast<const g2t_fts_hit_st*>( hit_table->At(i) );
	if ( 0==hit ) continue;     // skip null entries
	if ( 1!=hit->track_p ) continue; // not interested in secondaries
	
	check_stg_hit( "The hit should have a nonzero volume_id",hit,[=](const g2t_fts_hit_st* h) {
	std::string result = FAIL;
	if ( h->volume_id > 0 ) result = PASS;
	return result;
      });
	check_stg_hit( "The hit should have an energy deposit > 0",hit,[=](const g2t_fts_hit_st* h) {
	std::string result = FAIL;
	if ( h->de > 0 ) result = PASS;
	return result;
      });
	check_stg_hit( "The hit should have a path length > 0",hit,[=](const g2t_fts_hit_st* h) {
	std::string result = FAIL;
	if ( h->ds > 0 ) result = PASS;
	return result;
      });
	check_stg_hit( "The hit should have a nonzero momentum",hit,[=](const g2t_fts_hit_st* h) {
	std::string result = FAIL;
	if ( h->p[0] != 0 ) result = PASS;
	if ( h->p[1] != 0 ) result = PASS;
	if ( h->p[2] != 0 ) result = PASS;
	return result;
      });
	check_stg_hit( "Track's momentum at hit should be < initial value",hit,    [=](const g2t_fts_hit_st* h){
	std::string result = FAIL;
	double px = h->p[0];
	double py = h->p[1];
	double pz = h->p[2];
	double p2 = px*px + py*py + pz*pz;
	if ( p2 < _pmom*_pmom ) result = PASS;
	return result;
      });
	check_stg_hit( "Hit position should be w/in the fiducial volume of the station",hit,[=](const g2t_fts_hit_st* h){
     	// TODO
	return TODO;
      });
	check_stg_hit( "The hit position and tof*c agree to w/in 0.15 mm ",          hit,[=](const g2t_fts_hit_st* h) {
	// There should be some tolerance on this, b/c of roundoff error at each tracking step
	std::string result = FAIL;
	double c_tof = 2.99792458E10 /* cm/s */ * h->tof;
	double s_trk = 
	  sqrt( h->x[0]*h->x[0] +
		h->x[1]*h->x[1] +
		h->x[2]*h->x[2] );			    
	double diff = TMath::Abs(c_tof-s_trk);
	if ( diff < 0.015 ) result = PASS;
	result = Form("c_tof=%f cm  strack=%f cm diff=%f cm ",c_tof,s_trk,diff) + result;
	return result;
      });    
	check_stg_hit( "The station should decode as ...",hit,[=](const g2t_fts_hit_st* h) {
	std::string result=TODO;
	int station = h->volume_id / 10;
	if ( station>=1 && station<=4 ) result=PASS;
	result = Form(" volume_id=%i ",h->volume_id) + result;
	return result;
      });   

      }

    }
  }
  
}
