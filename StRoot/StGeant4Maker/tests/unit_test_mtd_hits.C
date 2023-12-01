#include "tests/unit_tests.h"
#include <assert.h>

struct Cell {
  int icell;
  int bl;  // backleg
  int mod; // module
  float phi;
  float eta;
};
std::vector<Cell> cells;
void init_cells();

#ifndef __CINT__
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/mean.hpp>
#include <boost/accumulators/statistics/median.hpp>
#include <boost/accumulators/statistics/min.hpp>
#include <boost/accumulators/statistics/max.hpp>
#include <boost/accumulators/statistics/moment.hpp>
#include <boost/accumulators/statistics/error_of.hpp>
#include <boost/accumulators/statistics/error_of_mean.hpp>
using namespace boost::accumulators;

using Accumulator_t = accumulator_set<double, 
stats< tag::count, 
       tag::sum,
       tag::mean, 
       tag::median(with_p_square_quantile),
       tag::max, 
       tag::min, 
       tag::error_of<tag::mean>
>>;
#endif

#include <TVector3.h>

//___________________________________________________________________
double _eta  = 0; 
double _phid = 0;
//______________________________________________________________________
void unit_test_mtd_hits( int longtest=0, double _pt=25.0 ) {

  gROOT->ProcessLine("initChain();");

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.,0.,0.);
  pm->SetSigma(0.0,0.,0.);

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Unit testing of tracks and TPC hits on single muons"     << std::endl;
  LOG_TEST << "=======================================================" << std::endl;

  Accumulator_t edep; // Energy deposition
  Accumulator_t step; // Step size
  Accumulator_t time; // Time per throw
  
  init_cells();

  for ( auto cell : cells ) {

    throw_muon( _eta = cell.eta, cell.phi * 180.0 / TMath::Pi(), _pt );
    auto* chain = StMaker::GetChain();
    vertex_table = dynamic_cast<TTable*>( chain->GetDataSet("g2t_vertex")  );
    track_table  = dynamic_cast<TTable*>( chain->GetDataSet("g2t_track")   );
    hit_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_mtd_hit") );



    check_track( "A muon must have been processed by geant",       [=](const g2t_track_st* t){
	assert(t);
	std::string result = PASS;
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
    check_track( "There should not be a stop vertex in the TPC",   [=](const g2t_track_st* t){
      std::string result = FAIL;
      int istop = t->stop_vertex_p;
      const g2t_vertex_st* v = (istop>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istop-1) ) : 0;
      if ( 0==v ) 
	result = PASS;
      else {
	double x1 = v->eg_x[0];
	double y1 = v->eg_x[1];
	double z1 = v->eg_x[2];
	double x2 = v->ge_x[0];
	double y2 = v->ge_x[1];
	double z2 = v->ge_x[2];
	double xx1 = sqrt(x1*x1+y1*y1); // event generator
	double xx2 = sqrt(x2*x2+y2*y2); // geant vertex
	if ( xx1 > 208.0 || xx2 > 208.0 ) 
	  result = PASS;
	result = Form(" r=%f ",TMath::Max(xx1,xx2)) + result;
      }
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
    check_track( "Expect 6 hits in the MTD (dev2021 geometry)",    [=](const g2t_track_st* t){
      int n = t->n_mtd_hit;
      std::string  result = FAIL;
      if ( n==5 ) result = PASS;
      result = Form(" n=%i ",n) + result;
      return result;
    });

    if ( 0==hit_table ) continue;

    for ( int i=0;i<hit_table->GetNRows();i++ ) {

      auto hit = static_cast<const g2t_mtd_hit_st*>( hit_table->At(i) );
      if ( 0==hit ) continue;     // skip null entries
      if ( 1!=hit->track_p ) continue; // not interested in secondaries

      LOG_TEST << "------------------------------------------------------------------" << std::endl;
      LOG_TEST << GIVEN << "A hit on that track" << std::endl;

      edep( TMath::Abs(hit->de) * 1E6 ); // GeV MeV keV
      step( hit->ds );

      check_mtd_hit( "The hit should have a nonzero volume_id",           hit,[=](const g2t_mtd_hit_st* h) {
       	  std::string result = FAIL;
          if ( h->volume_id > 0 ) result = PASS;
     	  result = Form("id=%i vid=%i de=%f ds=%f ",h->id,h->volume_id,h->de,h->ds) + result;
     	  return result;
     	});
      check_mtd_hit( "The hit should have an energy deposit > 0",         hit,[=](const g2t_mtd_hit_st* h) {
       	  std::string result = FAIL; // undetermined
	  if       ( h->de > 0 ) result = PASS;
	  return result;
	});
      check_mtd_hit( "The hit should have a step size > 0",               hit,[=](const g2t_mtd_hit_st* h) {
	  std::string result = FAIL;
	  if ( h->ds > 0 ) result = PASS;
	  return result;
	});
      check_mtd_hit( "The hit should have a path length > 0",             hit,[=](const g2t_mtd_hit_st* h) {
	  std::string result = FAIL;
	  if ( h->s_track > 0 ) result = PASS;
	  return result;
	});
      check_mtd_hit( "The hit should have time-of-flight > 0",            hit,[=](const g2t_mtd_hit_st* h) {
	  std::string result = FAIL;
	  if ( h->tof > 0 ) result = PASS;
	  result = Form(" tof=%f ",h->tof) + result;
	  return result;
	});
      check_mtd_hit( "The hit should have a nonzero momentum",            hit,[=](const g2t_mtd_hit_st* h) {
	  std::string result = FAIL;
	  if ( h->p[0] != 0 ) result = PASS;
	  if ( h->p[1] != 0 ) result = PASS;
	  if ( h->p[2] != 0 ) result = PASS;
	  return result;
	});
      check_mtd_hit( "The hit should have a length > 0",                  hit,[=](const g2t_mtd_hit_st* h) {
	  std::string result = FAIL;
	  if ( h->s_track > 0 ) result = PASS;
	  return result;
	});

      check_mtd_hit( "The hit should be at a radius > 364.25",            hit,[=](const g2t_mtd_hit_st* h) {
	  std::string result = FAIL;
	  double R2 = h->xglobal[0]*h->xglobal[0] + h->xglobal[1]*h->xglobal[1];
	  if ( R2 > 364.25*364.25 ) result = PASS;
	  result = Form(" R=%f ",sqrt(R2)) + result;
	  return result;
	});
      check_mtd_hit( "The hit should be at a radius < 430.0",             hit,[=](const g2t_mtd_hit_st* h) {
	  std::string result = FAIL;
	  double R2 = h->xglobal[0]*h->xglobal[0] + h->xglobal[1]*h->xglobal[1];
	  if ( R2 < 430.0*430.0 ) result = PASS;
	  result = Form(" R=%f ",sqrt(R2)) + result;
	  return result;
	});

      check_mtd_hit( "Track's momentum at hit should be < initial value", hit,[=](const g2t_mtd_hit_st* h) {
	  std::string result = FAIL;
	  double px = h->p[0];
	  double py = h->p[1];
	  double pz = h->p[2];
	  double p2 = px*px + py*py + pz*pz;
	  if ( p2 < _pmom*_pmom ) result = PASS;
	  return result;
	});


      if ( Conditional (
	    check_mtd_hit("Conditional: Is the track's momentum at hit > 50% of its initial value", hit,[=](const g2t_mtd_hit_st* h) {
		std::string result = FAIL;
		double px = h->p[0];
		double py = h->p[1];
		double pz = h->p[2];
		double p2 = px*px + py*py + pz*pz;
		if ( p2 > 0.25* _pmom*_pmom ) result = YES;
		return result;
	      })
			)
	   ) {
	   
	check_mtd_hit( "The track length and tof*c agree to w/in 0.15 mm ",         hit,[=](const g2t_mtd_hit_st* h) {
	  // There should be some tolerance on this, b/c of roundoff error at each tracking step
	  std::string result = FAIL;
	  double c_tof = 2.99792458E10 /* cm/s */ * h->tof;
	  double s_trk = h->s_track;
	  double diff = TMath::Abs(c_tof-s_trk);
	  if ( diff < 0.015 ) result = PASS;
	  result = Form("c_tof=%f cm  strack=%f cm diff=%f cm ",c_tof,s_trk,diff) + result;
	  return result;
	});
	check_mtd_hit( "The track length at the hit should be >= radius at the hit",hit,[=](const g2t_mtd_hit_st* h) {
	  std::string result = FAIL;
	  double R2 = h->xglobal[0]*h->xglobal[0] + h->xglobal[1]*h->xglobal[1];
	  double L2 = h->s_track * h->s_track;
	  if ( R2 <= L2 ) result = PASS;
	  result = Form(" R=%f L=%f",sqrt(R2),sqrt(L2)) + result;
	  return result;
	});

      } else {

	LOG_TEST << "The track length and tof*c agree to w/in 0.15 mm "          << NADA << std::endl;
	LOG_TEST << "The track length at the hit should be >= radius at the hit" << NADA << std::endl;

      }

     
      check_mtd_hit( "The sector (aka backleg) should decode as 1..30",   hit,[=](const g2t_mtd_hit_st* h) {
	  std::string result = FAIL;
	  int sector = h->volume_id / 1000;
	  if ( sector >= 1 && sector <= 30 ) result = PASS;
	  result = Form("sector=%i ",sector) + result;
	  return result;
	});
      check_mtd_hit( "The module should decode as 1..5",                  hit,[=](const g2t_mtd_hit_st* h) {
	  std::string result = FAIL;
	  int module = h->volume_id % 1000 / 100;
	  if ( module >= 1 && module <= 5 ) result = PASS;
	  result = Form("module=%i ",module) + result;
	  return result;
	});
      check_mtd_hit( "The layer should decode as 1..5",                   hit,[=]( const g2t_mtd_hit_st* h) {
	  std::string result = FAIL;
	  int layer = h->volume_id % 10;
	  if ( layer >= 1 && layer <= 5 ) result = PASS;
	  result = Form("layer=%i ",layer) + result;
	  return result;
	});
    }

  }

  std::cout << std::endl << std::endl;

  // Print out energy deposition
  {
  
    double _mean          = boost::accumulators::mean(edep);
    double _median        = boost::accumulators::median(edep);
    double _min           = boost::accumulators::min(edep);
    double _max           = boost::accumulators::max(edep);
    double _error_of_mean = boost::accumulators::error_of<tag::mean>(edep);
    
    LOG_TEST << Form( "energy deposition: mean          = %f keV", _mean )          << std::endl;
    LOG_TEST << Form( "energy deposition: median        = %f keV", _median )        << std::endl;
    LOG_TEST << Form( "energy deposition: min           = %f keV", _min  )          << std::endl;
    LOG_TEST << Form( "energy deposition: max           = %f keV", _max  )          << std::endl;
    LOG_TEST << Form( "energy deposition: error of mean = %f keV", _error_of_mean ) << std::endl;

  }

  // Print out step sizes
  {
  
    double _mean          = boost::accumulators::mean(step);
    double _median        = boost::accumulators::median(step);
    double _min           = boost::accumulators::min(step);
    double _max           = boost::accumulators::max(step);
    double _error_of_mean = boost::accumulators::error_of<tag::mean>(step); 
    LOG_TEST << Form( "step size: mean          = %f cm", _mean )          << std::endl;
    LOG_TEST << Form( "step size: median        = %f cm", _median )        << std::endl;
    LOG_TEST << Form( "step size: min           = %f cm", _min  )          << std::endl;
    LOG_TEST << Form( "step size: max           = %f cm", _max  )          << std::endl;
    LOG_TEST << Form( "step size: error of mean = %f cm", _error_of_mean ) << std::endl;

  }

  // Print out time per track
  {
  
    double _mean          = boost::accumulators::mean(time);
    double _median        = boost::accumulators::median(time);
    double _min           = boost::accumulators::min(time);
    double _max           = boost::accumulators::max(time);
    double _error_of_mean = boost::accumulators::error_of<tag::mean>(time); 
    LOG_TEST << Form( "time / muon: mean          = %f s", _mean )          << std::endl;
    LOG_TEST << Form( "time / muon: median        = %f s", _median )        << std::endl;
    LOG_TEST << Form( "time / muon: min           = %f s", _min  )          << std::endl;
    LOG_TEST << Form( "time / muon: max           = %f s", _max  )          << std::endl;
    LOG_TEST << Form( "time / muon: error of mean = %f s", _error_of_mean ) << std::endl;

  }

  // Reset accumulators
  edep = step = time = {};
  
  if ( longtest > 0 ) {

    std::cout << "-/ running long test with N pi+/pi- =" << longtest << " /-" << std::endl;

    throw_particle( longtest, "pi+,pi-", 0.200, 20.0, -1.0, 1.0, 0.0, TMath::TwoPi() );

    auto* chain = StMaker::GetChain();
    hit_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_mtd_hit") ) ;

    // Accumulate
    for ( int i=0;i<hit_table->GetNRows();i++ ) {

      auto hit = static_cast<const g2t_mtd_hit_st*>( hit_table->At(i) );
      if ( 0==hit ) continue;     // skip null entries
      //      std::cout << *hit << std::endl;

      edep( TMath::Abs(hit->de) * 1E6 ); // GeV MeV keV
      step( hit->ds );

    }

    // Print out energy deposition  
    {
      double _mean          = boost::accumulators::mean(edep);
      double _median        = boost::accumulators::median(edep);
      double _min           = boost::accumulators::min(edep);
      double _max           = boost::accumulators::max(edep);
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(edep);
    
      LOG_TEST << Form( "energy deposition: mean          = %f keV", _mean )          << std::endl;
      LOG_TEST << Form( "energy deposition: median        = %f keV", _median )        << std::endl;
      LOG_TEST << Form( "energy deposition: min           = %f keV", _min  )          << std::endl;
      LOG_TEST << Form( "energy deposition: max           = %f keV", _max  )          << std::endl;
      LOG_TEST << Form( "energy deposition: error of mean = %f keV", _error_of_mean ) << std::endl;
      
    }

    // Print out step sizes
    {
      
      double _mean          = boost::accumulators::mean(step);
      double _median        = boost::accumulators::median(step);
      double _min           = boost::accumulators::min(step);
      double _max           = boost::accumulators::max(step);
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(step); 
      LOG_TEST << Form( "step size: mean          = %f cm", _mean )          << std::endl;
      LOG_TEST << Form( "step size: median        = %f cm", _median )        << std::endl;
      LOG_TEST << Form( "step size: min           = %f cm", _min  )          << std::endl;
      LOG_TEST << Form( "step size: max           = %f cm", _max  )          << std::endl;
      LOG_TEST << Form( "step size: error of mean = %f cm", _error_of_mean ) << std::endl;

    }

  }


}
//___________________________________________________________________
void init_cells() {
cells.push_back({ 0,1,1, 1.510019,-0.416592 });
cells.push_back({ 1,1,1, 1.521069,-0.416592 });
cells.push_back({ 2,1,1, 1.532120,-0.416592 });
cells.push_back({ 3,1,1, 1.543170,-0.416592 });
cells.push_back({ 4,1,1, 1.554221,-0.416592 });
cells.push_back({ 5,1,1, 1.565271,-0.416592 });
cells.push_back({ 6,1,1, 1.576321,-0.416592 });
cells.push_back({ 7,1,1, 1.587372,-0.416592 });
cells.push_back({ 8,1,1, 1.598422,-0.416592 });
cells.push_back({ 9,1,1, 1.609473,-0.416592 });
cells.push_back({ 10,1,1, 1.620523,-0.416592 });
cells.push_back({ 11,1,1, 1.631574,-0.416592 });
// ibackleg=1 imodule=2
cells.push_back({ 0,1,2, 1.509247,-0.212765 });
cells.push_back({ 1,1,2, 1.520438,-0.212765 });
cells.push_back({ 2,1,2, 1.531629,-0.212765 });
cells.push_back({ 3,1,2, 1.542819,-0.212765 });
cells.push_back({ 4,1,2, 1.554010,-0.212765 });
cells.push_back({ 5,1,2, 1.565201,-0.212765 });
cells.push_back({ 6,1,2, 1.576392,-0.212765 });
cells.push_back({ 7,1,2, 1.587582,-0.212765 });
cells.push_back({ 8,1,2, 1.598773,-0.212765 });
cells.push_back({ 9,1,2, 1.609964,-0.212765 });
cells.push_back({ 10,1,2, 1.621154,-0.212765 });
cells.push_back({ 11,1,2, 1.632345,-0.212765 });
// ibackleg=1 imodule=3
cells.push_back({ 0,1,3, 1.510019,-0.000000 });
cells.push_back({ 1,1,3, 1.521069,-0.000000 });
cells.push_back({ 2,1,3, 1.532120,-0.000000 });
cells.push_back({ 3,1,3, 1.543170,-0.000000 });
cells.push_back({ 4,1,3, 1.554221,-0.000000 });
cells.push_back({ 5,1,3, 1.565271,-0.000000 });
cells.push_back({ 6,1,3, 1.576321,-0.000000 });
cells.push_back({ 7,1,3, 1.587372,-0.000000 });
cells.push_back({ 8,1,3, 1.598422,-0.000000 });
cells.push_back({ 9,1,3, 1.609473,-0.000000 });
cells.push_back({ 10,1,3, 1.620523,-0.000000 });
cells.push_back({ 11,1,3, 1.631574,-0.000000 });
// ibackleg=1 imodule=4
cells.push_back({ 0,1,4, 1.632345,0.212765 });
cells.push_back({ 1,1,4, 1.621154,0.212765 });
cells.push_back({ 2,1,4, 1.609964,0.212765 });
cells.push_back({ 3,1,4, 1.598773,0.212765 });
cells.push_back({ 4,1,4, 1.587582,0.212765 });
cells.push_back({ 5,1,4, 1.576392,0.212765 });
cells.push_back({ 6,1,4, 1.565201,0.212765 });
cells.push_back({ 7,1,4, 1.554010,0.212765 });
cells.push_back({ 8,1,4, 1.542819,0.212765 });
cells.push_back({ 9,1,4, 1.531629,0.212765 });
cells.push_back({ 10,1,4, 1.520438,0.212765 });
cells.push_back({ 11,1,4, 1.509247,0.212765 });
// ibackleg=2 imodule=0
// ibackleg=2 imodule=1
cells.push_back({ 0,2,1, 1.719458,-0.416592 });
cells.push_back({ 1,2,1, 1.730509,-0.416592 });
cells.push_back({ 2,2,1, 1.741559,-0.416592 });
cells.push_back({ 3,2,1, 1.752610,-0.416592 });
cells.push_back({ 4,2,1, 1.763660,-0.416592 });
cells.push_back({ 5,2,1, 1.774711,-0.416592 });
cells.push_back({ 6,2,1, 1.785761,-0.416592 });
cells.push_back({ 7,2,1, 1.796812,-0.416592 });
cells.push_back({ 8,2,1, 1.807862,-0.416592 });
cells.push_back({ 9,2,1, 1.818912,-0.416592 });
cells.push_back({ 10,2,1, 1.829963,-0.416592 });
cells.push_back({ 11,2,1, 1.841013,-0.416592 });
// ibackleg=2 imodule=2
cells.push_back({ 0,2,2, 1.718687,-0.212765 });
cells.push_back({ 1,2,2, 1.729878,-0.212765 });
cells.push_back({ 2,2,2, 1.741068,-0.212765 });
cells.push_back({ 3,2,2, 1.752259,-0.212765 });
cells.push_back({ 4,2,2, 1.763450,-0.212765 });
cells.push_back({ 5,2,2, 1.774641,-0.212765 });
cells.push_back({ 6,2,2, 1.785831,-0.212765 });
cells.push_back({ 7,2,2, 1.797022,-0.212765 });
cells.push_back({ 8,2,2, 1.808213,-0.212765 });
cells.push_back({ 9,2,2, 1.819403,-0.212765 });
cells.push_back({ 10,2,2, 1.830594,-0.212765 });
cells.push_back({ 11,2,2, 1.841785,-0.212765 });
// ibackleg=2 imodule=3
cells.push_back({ 0,2,3, 1.719458,-0.000000 });
cells.push_back({ 1,2,3, 1.730509,-0.000000 });
cells.push_back({ 2,2,3, 1.741559,-0.000000 });
cells.push_back({ 3,2,3, 1.752610,-0.000000 });
cells.push_back({ 4,2,3, 1.763660,-0.000000 });
cells.push_back({ 5,2,3, 1.774711,-0.000000 });
cells.push_back({ 6,2,3, 1.785761,-0.000000 });
cells.push_back({ 7,2,3, 1.796812,-0.000000 });
cells.push_back({ 8,2,3, 1.807862,-0.000000 });
cells.push_back({ 9,2,3, 1.818912,-0.000000 });
cells.push_back({ 10,2,3, 1.829963,-0.000000 });
cells.push_back({ 11,2,3, 1.841013,-0.000000 });
// ibackleg=2 imodule=4
cells.push_back({ 0,2,4, 1.841785,0.212765 });
cells.push_back({ 1,2,4, 1.830594,0.212765 });
cells.push_back({ 2,2,4, 1.819403,0.212765 });
cells.push_back({ 3,2,4, 1.808213,0.212765 });
cells.push_back({ 4,2,4, 1.797022,0.212765 });
cells.push_back({ 5,2,4, 1.785831,0.212765 });
cells.push_back({ 6,2,4, 1.774641,0.212765 });
cells.push_back({ 7,2,4, 1.763450,0.212765 });
cells.push_back({ 8,2,4, 1.752259,0.212765 });
cells.push_back({ 9,2,4, 1.741068,0.212765 });
cells.push_back({ 10,2,4, 1.729878,0.212765 });
cells.push_back({ 11,2,4, 1.718687,0.212765 });
// ibackleg=3 imodule=0
// ibackleg=3 imodule=1
cells.push_back({ 0,3,1, 1.928898,-0.416592 });
cells.push_back({ 1,3,1, 1.939948,-0.416592 });
cells.push_back({ 2,3,1, 1.950999,-0.416592 });
cells.push_back({ 3,3,1, 1.962049,-0.416592 });
cells.push_back({ 4,3,1, 1.973100,-0.416592 });
cells.push_back({ 5,3,1, 1.984150,-0.416592 });
cells.push_back({ 6,3,1, 1.995201,-0.416592 });
cells.push_back({ 7,3,1, 2.006251,-0.416592 });
cells.push_back({ 8,3,1, 2.017302,-0.416592 });
cells.push_back({ 9,3,1, 2.028352,-0.416592 });
cells.push_back({ 10,3,1, 2.039402,-0.416592 });
cells.push_back({ 11,3,1, 2.050453,-0.416592 });
// ibackleg=3 imodule=2
cells.push_back({ 0,3,2, 1.928126,-0.212765 });
cells.push_back({ 1,3,2, 1.939317,-0.212765 });
cells.push_back({ 2,3,2, 1.950508,-0.212765 });
cells.push_back({ 3,3,2, 1.961699,-0.212765 });
cells.push_back({ 4,3,2, 1.972889,-0.212765 });
cells.push_back({ 5,3,2, 1.984080,-0.212765 });
cells.push_back({ 6,3,2, 1.995271,-0.212765 });
cells.push_back({ 7,3,2, 2.006461,-0.212765 });
cells.push_back({ 8,3,2, 2.017652,-0.212765 });
cells.push_back({ 9,3,2, 2.028843,-0.212765 });
cells.push_back({ 10,3,2, 2.040034,-0.212765 });
cells.push_back({ 11,3,2, 2.051224,-0.212765 });
// ibackleg=3 imodule=3
cells.push_back({ 0,3,3, 1.928898,-0.000000 });
cells.push_back({ 1,3,3, 1.939948,-0.000000 });
cells.push_back({ 2,3,3, 1.950999,-0.000000 });
cells.push_back({ 3,3,3, 1.962049,-0.000000 });
cells.push_back({ 4,3,3, 1.973100,-0.000000 });
cells.push_back({ 5,3,3, 1.984150,-0.000000 });
cells.push_back({ 6,3,3, 1.995201,-0.000000 });
cells.push_back({ 7,3,3, 2.006251,-0.000000 });
cells.push_back({ 8,3,3, 2.017302,-0.000000 });
cells.push_back({ 9,3,3, 2.028352,-0.000000 });
cells.push_back({ 10,3,3, 2.039402,-0.000000 });
cells.push_back({ 11,3,3, 2.050453,-0.000000 });
// ibackleg=3 imodule=4
cells.push_back({ 0,3,4, 2.051224,0.212765 });
cells.push_back({ 1,3,4, 2.040034,0.212765 });
cells.push_back({ 2,3,4, 2.028843,0.212765 });
cells.push_back({ 3,3,4, 2.017652,0.212765 });
cells.push_back({ 4,3,4, 2.006461,0.212765 });
cells.push_back({ 5,3,4, 1.995271,0.212765 });
cells.push_back({ 6,3,4, 1.984080,0.212765 });
cells.push_back({ 7,3,4, 1.972889,0.212765 });
cells.push_back({ 8,3,4, 1.961699,0.212765 });
cells.push_back({ 9,3,4, 1.950508,0.212765 });
cells.push_back({ 10,3,4, 1.939317,0.212765 });
cells.push_back({ 11,3,4, 1.928126,0.212765 });
// ibackleg=4 imodule=0
// ibackleg=4 imodule=1
cells.push_back({ 0,4,1, 2.138337,-0.416592 });
cells.push_back({ 1,4,1, 2.149388,-0.416592 });
cells.push_back({ 2,4,1, 2.160438,-0.416592 });
cells.push_back({ 3,4,1, 2.171489,-0.416592 });
cells.push_back({ 4,4,1, 2.182539,-0.416592 });
cells.push_back({ 5,4,1, 2.193590,-0.416592 });
cells.push_back({ 6,4,1, 2.204640,-0.416592 });
cells.push_back({ 7,4,1, 2.215690,-0.416592 });
cells.push_back({ 8,4,1, 2.226741,-0.416592 });
cells.push_back({ 9,4,1, 2.237791,-0.416592 });
cells.push_back({ 10,4,1, 2.248842,-0.416592 });
cells.push_back({ 11,4,1, 2.259892,-0.416592 });
// ibackleg=4 imodule=2
cells.push_back({ 0,4,2, 2.137566,-0.212765 });
cells.push_back({ 1,4,2, 2.148757,-0.212765 });
cells.push_back({ 2,4,2, 2.159947,-0.212765 });
cells.push_back({ 3,4,2, 2.171138,-0.212765 });
cells.push_back({ 4,4,2, 2.182329,-0.212765 });
cells.push_back({ 5,4,2, 2.193519,-0.212765 });
cells.push_back({ 6,4,2, 2.204710,-0.212765 });
cells.push_back({ 7,4,2, 2.215901,-0.212765 });
cells.push_back({ 8,4,2, 2.227092,-0.212765 });
cells.push_back({ 9,4,2, 2.238282,-0.212765 });
cells.push_back({ 10,4,2, 2.249473,-0.212765 });
cells.push_back({ 11,4,2, 2.260664,-0.212765 });
// ibackleg=4 imodule=3
cells.push_back({ 0,4,3, 2.138337,-0.000000 });
cells.push_back({ 1,4,3, 2.149388,-0.000000 });
cells.push_back({ 2,4,3, 2.160438,-0.000000 });
cells.push_back({ 3,4,3, 2.171489,-0.000000 });
cells.push_back({ 4,4,3, 2.182539,-0.000000 });
cells.push_back({ 5,4,3, 2.193590,-0.000000 });
cells.push_back({ 6,4,3, 2.204640,-0.000000 });
cells.push_back({ 7,4,3, 2.215690,-0.000000 });
cells.push_back({ 8,4,3, 2.226741,-0.000000 });
cells.push_back({ 9,4,3, 2.237791,-0.000000 });
cells.push_back({ 10,4,3, 2.248842,-0.000000 });
cells.push_back({ 11,4,3, 2.259892,-0.000000 });
// ibackleg=4 imodule=4
cells.push_back({ 0,4,4, 2.260664,0.212765 });
cells.push_back({ 1,4,4, 2.249473,0.212765 });
cells.push_back({ 2,4,4, 2.238282,0.212765 });
cells.push_back({ 3,4,4, 2.227092,0.212765 });
cells.push_back({ 4,4,4, 2.215901,0.212765 });
cells.push_back({ 5,4,4, 2.204710,0.212765 });
cells.push_back({ 6,4,4, 2.193519,0.212765 });
cells.push_back({ 7,4,4, 2.182329,0.212765 });
cells.push_back({ 8,4,4, 2.171138,0.212765 });
cells.push_back({ 9,4,4, 2.159947,0.212765 });
cells.push_back({ 10,4,4, 2.148757,0.212765 });
cells.push_back({ 11,4,4, 2.137566,0.212765 });
// ibackleg=5 imodule=0
// ibackleg=5 imodule=1
cells.push_back({ 0,5,1, 2.347777,-0.416592 });
cells.push_back({ 1,5,1, 2.358827,-0.416592 });
cells.push_back({ 2,5,1, 2.369878,-0.416592 });
cells.push_back({ 3,5,1, 2.380928,-0.416592 });
cells.push_back({ 4,5,1, 2.391979,-0.416592 });
cells.push_back({ 5,5,1, 2.403029,-0.416592 });
cells.push_back({ 6,5,1, 2.414079,-0.416592 });
cells.push_back({ 7,5,1, 2.425130,-0.416592 });
cells.push_back({ 8,5,1, 2.436180,-0.416592 });
cells.push_back({ 9,5,1, 2.447231,-0.416592 });
cells.push_back({ 10,5,1, 2.458281,-0.416592 });
cells.push_back({ 11,5,1, 2.469332,-0.416592 });
// ibackleg=5 imodule=2
cells.push_back({ 0,5,2, 2.347005,-0.212765 });
cells.push_back({ 1,5,2, 2.358196,-0.212765 });
cells.push_back({ 2,5,2, 2.369387,-0.212765 });
cells.push_back({ 3,5,2, 2.380578,-0.212765 });
cells.push_back({ 4,5,2, 2.391768,-0.212765 });
cells.push_back({ 5,5,2, 2.402959,-0.212765 });
cells.push_back({ 6,5,2, 2.414150,-0.212765 });
cells.push_back({ 7,5,2, 2.425340,-0.212765 });
cells.push_back({ 8,5,2, 2.436531,-0.212765 });
cells.push_back({ 9,5,2, 2.447722,-0.212765 });
cells.push_back({ 10,5,2, 2.458913,-0.212765 });
cells.push_back({ 11,5,2, 2.470103,-0.212765 });
// ibackleg=5 imodule=3
cells.push_back({ 0,5,3, 2.347777,-0.000000 });
cells.push_back({ 1,5,3, 2.358827,-0.000000 });
cells.push_back({ 2,5,3, 2.369878,-0.000000 });
cells.push_back({ 3,5,3, 2.380928,-0.000000 });
cells.push_back({ 4,5,3, 2.391979,-0.000000 });
cells.push_back({ 5,5,3, 2.403029,-0.000000 });
cells.push_back({ 6,5,3, 2.414079,-0.000000 });
cells.push_back({ 7,5,3, 2.425130,-0.000000 });
cells.push_back({ 8,5,3, 2.436180,-0.000000 });
cells.push_back({ 9,5,3, 2.447231,-0.000000 });
cells.push_back({ 10,5,3, 2.458281,-0.000000 });
cells.push_back({ 11,5,3, 2.469332,-0.000000 });
// ibackleg=5 imodule=4
cells.push_back({ 0,5,4, 2.470103,0.212765 });
cells.push_back({ 1,5,4, 2.458913,0.212765 });
cells.push_back({ 2,5,4, 2.447722,0.212765 });
cells.push_back({ 3,5,4, 2.436531,0.212765 });
cells.push_back({ 4,5,4, 2.425340,0.212765 });
cells.push_back({ 5,5,4, 2.414150,0.212765 });
cells.push_back({ 6,5,4, 2.402959,0.212765 });
cells.push_back({ 7,5,4, 2.391768,0.212765 });
cells.push_back({ 8,5,4, 2.380578,0.212765 });
cells.push_back({ 9,5,4, 2.369387,0.212765 });
cells.push_back({ 10,5,4, 2.358196,0.212765 });
cells.push_back({ 11,5,4, 2.347005,0.212765 });
// ibackleg=6 imodule=0
// ibackleg=6 imodule=1
cells.push_back({ 0,6,1, 2.557216,-0.416592 });
cells.push_back({ 1,6,1, 2.568267,-0.416592 });
cells.push_back({ 2,6,1, 2.579317,-0.416592 });
cells.push_back({ 3,6,1, 2.590368,-0.416592 });
cells.push_back({ 4,6,1, 2.601418,-0.416592 });
cells.push_back({ 5,6,1, 2.612469,-0.416592 });
cells.push_back({ 6,6,1, 2.623519,-0.416592 });
cells.push_back({ 7,6,1, 2.634569,-0.416592 });
cells.push_back({ 8,6,1, 2.645620,-0.416592 });
cells.push_back({ 9,6,1, 2.656670,-0.416592 });
cells.push_back({ 10,6,1, 2.667721,-0.416592 });
cells.push_back({ 11,6,1, 2.678771,-0.416592 });
// ibackleg=6 imodule=2
cells.push_back({ 0,6,2, 2.556445,-0.212765 });
cells.push_back({ 1,6,2, 2.567636,-0.212765 });
cells.push_back({ 2,6,2, 2.578826,-0.212765 });
cells.push_back({ 3,6,2, 2.590017,-0.212765 });
cells.push_back({ 4,6,2, 2.601208,-0.212765 });
cells.push_back({ 5,6,2, 2.612398,-0.212765 });
cells.push_back({ 6,6,2, 2.623589,-0.212765 });
cells.push_back({ 7,6,2, 2.634780,-0.212765 });
cells.push_back({ 8,6,2, 2.645971,-0.212765 });
cells.push_back({ 9,6,2, 2.657161,-0.212765 });
cells.push_back({ 10,6,2, 2.668352,-0.212765 });
cells.push_back({ 11,6,2, 2.679543,-0.212765 });
// ibackleg=6 imodule=3
cells.push_back({ 0,6,3, 2.557216,-0.000000 });
cells.push_back({ 1,6,3, 2.568267,-0.000000 });
cells.push_back({ 2,6,3, 2.579317,-0.000000 });
cells.push_back({ 3,6,3, 2.590368,-0.000000 });
cells.push_back({ 4,6,3, 2.601418,-0.000000 });
cells.push_back({ 5,6,3, 2.612469,-0.000000 });
cells.push_back({ 6,6,3, 2.623519,-0.000000 });
cells.push_back({ 7,6,3, 2.634569,-0.000000 });
cells.push_back({ 8,6,3, 2.645620,-0.000000 });
cells.push_back({ 9,6,3, 2.656670,-0.000000 });
cells.push_back({ 10,6,3, 2.667721,-0.000000 });
cells.push_back({ 11,6,3, 2.678771,-0.000000 });
// ibackleg=6 imodule=4
cells.push_back({ 0,6,4, 2.679543,0.212765 });
cells.push_back({ 1,6,4, 2.668352,0.212765 });
cells.push_back({ 2,6,4, 2.657161,0.212765 });
cells.push_back({ 3,6,4, 2.645971,0.212765 });
cells.push_back({ 4,6,4, 2.634780,0.212765 });
cells.push_back({ 5,6,4, 2.623589,0.212765 });
cells.push_back({ 6,6,4, 2.612398,0.212765 });
cells.push_back({ 7,6,4, 2.601208,0.212765 });
cells.push_back({ 8,6,4, 2.590017,0.212765 });
cells.push_back({ 9,6,4, 2.578826,0.212765 });
cells.push_back({ 10,6,4, 2.567636,0.212765 });
cells.push_back({ 11,6,4, 2.556445,0.212765 });
// ibackleg=7 imodule=0
// ibackleg=7 imodule=1
cells.push_back({ 0,7,1, 2.767657,-0.416592 });
cells.push_back({ 1,7,1, 2.778526,-0.416592 });
cells.push_back({ 2,7,1, 2.789394,-0.416592 });
cells.push_back({ 3,7,1, 2.800262,-0.416592 });
cells.push_back({ 4,7,1, 2.811131,-0.416592 });
cells.push_back({ 5,7,1, 2.821999,-0.416592 });
cells.push_back({ 6,7,1, 2.832868,-0.416592 });
cells.push_back({ 7,7,1, 2.843736,-0.416592 });
cells.push_back({ 8,7,1, 2.854604,-0.416592 });
cells.push_back({ 9,7,1, 2.865473,-0.416592 });
cells.push_back({ 10,7,1, 2.876341,-0.416592 });
cells.push_back({ 11,7,1, 2.887209,-0.416592 });
// ibackleg=7 imodule=2
cells.push_back({ 0,7,2, 2.766911,-0.212765 });
cells.push_back({ 1,7,2, 2.777915,-0.212765 });
cells.push_back({ 2,7,2, 2.788919,-0.212765 });
cells.push_back({ 3,7,2, 2.799923,-0.212765 });
cells.push_back({ 4,7,2, 2.810927,-0.212765 });
cells.push_back({ 5,7,2, 2.821931,-0.212765 });
cells.push_back({ 6,7,2, 2.832935,-0.212765 });
cells.push_back({ 7,7,2, 2.843939,-0.212765 });
cells.push_back({ 8,7,2, 2.854944,-0.212765 });
cells.push_back({ 9,7,2, 2.865947,-0.212765 });
cells.push_back({ 10,7,2, 2.876951,-0.212765 });
cells.push_back({ 11,7,2, 2.887956,-0.212765 });
// ibackleg=7 imodule=3
cells.push_back({ 0,7,3, 2.767657,-0.000000 });
cells.push_back({ 1,7,3, 2.778526,-0.000000 });
cells.push_back({ 2,7,3, 2.789394,-0.000000 });
cells.push_back({ 3,7,3, 2.800262,-0.000000 });
cells.push_back({ 4,7,3, 2.811131,-0.000000 });
cells.push_back({ 5,7,3, 2.821999,-0.000000 });
cells.push_back({ 6,7,3, 2.832868,-0.000000 });
cells.push_back({ 7,7,3, 2.843736,-0.000000 });
cells.push_back({ 8,7,3, 2.854604,-0.000000 });
cells.push_back({ 9,7,3, 2.865473,-0.000000 });
cells.push_back({ 10,7,3, 2.876341,-0.000000 });
cells.push_back({ 11,7,3, 2.887209,-0.000000 });
// ibackleg=7 imodule=4
cells.push_back({ 0,7,4, 2.887956,0.212765 });
cells.push_back({ 1,7,4, 2.876951,0.212765 });
cells.push_back({ 2,7,4, 2.865947,0.212765 });
cells.push_back({ 3,7,4, 2.854944,0.212765 });
cells.push_back({ 4,7,4, 2.843939,0.212765 });
cells.push_back({ 5,7,4, 2.832935,0.212765 });
cells.push_back({ 6,7,4, 2.821931,0.212765 });
cells.push_back({ 7,7,4, 2.810927,0.212765 });
cells.push_back({ 8,7,4, 2.799923,0.212765 });
cells.push_back({ 9,7,4, 2.788919,0.212765 });
cells.push_back({ 10,7,4, 2.777915,0.212765 });
cells.push_back({ 11,7,4, 2.766911,0.212765 });
// ibackleg=8 imodule=0
// ibackleg=8 imodule=1
cells.push_back({ 0,8,1, 2.947753,-0.416592 });
cells.push_back({ 1,8,1, 2.958220,-0.416592 });
cells.push_back({ 2,8,1, 2.968686,-0.416592 });
cells.push_back({ 3,8,1, 2.979152,-0.416592 });
cells.push_back({ 4,8,1, 2.989618,-0.416592 });
cells.push_back({ 5,8,1, 3.000084,-0.416592 });
cells.push_back({ 6,8,1, 3.010550,-0.416592 });
cells.push_back({ 7,8,1, 3.021016,-0.416592 });
cells.push_back({ 8,8,1, 3.031483,-0.416592 });
cells.push_back({ 9,8,1, 3.041949,-0.416592 });
cells.push_back({ 10,8,1, 3.052415,-0.416592 });
cells.push_back({ 11,8,1, 3.062881,-0.416592 });
// ibackleg=8 imodule=2
cells.push_back({ 0,8,2, 2.946683,-0.212765 });
cells.push_back({ 1,8,2, 2.957275,-0.212765 });
cells.push_back({ 2,8,2, 2.967867,-0.212765 });
cells.push_back({ 3,8,2, 2.978459,-0.212765 });
cells.push_back({ 4,8,2, 2.989050,-0.212765 });
cells.push_back({ 5,8,2, 2.999642,-0.212765 });
cells.push_back({ 6,8,2, 3.010234,-0.212765 });
cells.push_back({ 7,8,2, 3.020826,-0.212765 });
cells.push_back({ 8,8,2, 3.031418,-0.212765 });
cells.push_back({ 9,8,2, 3.042010,-0.212765 });
cells.push_back({ 10,8,2, 3.052601,-0.212765 });
cells.push_back({ 11,8,2, 3.063193,-0.212765 });
// ibackleg=8 imodule=3
cells.push_back({ 0,8,3, 2.947753,-0.000000 });
cells.push_back({ 1,8,3, 2.958220,-0.000000 });
cells.push_back({ 2,8,3, 2.968686,-0.000000 });
cells.push_back({ 3,8,3, 2.979152,-0.000000 });
cells.push_back({ 4,8,3, 2.989618,-0.000000 });
cells.push_back({ 5,8,3, 3.000084,-0.000000 });
cells.push_back({ 6,8,3, 3.010550,-0.000000 });
cells.push_back({ 7,8,3, 3.021016,-0.000000 });
cells.push_back({ 8,8,3, 3.031483,-0.000000 });
cells.push_back({ 9,8,3, 3.041949,-0.000000 });
cells.push_back({ 10,8,3, 3.052415,-0.000000 });
cells.push_back({ 11,8,3, 3.062881,-0.000000 });
// ibackleg=8 imodule=4
cells.push_back({ 0,8,4, 3.063193,0.212765 });
cells.push_back({ 1,8,4, 3.052601,0.212765 });
cells.push_back({ 2,8,4, 3.042010,0.212765 });
cells.push_back({ 3,8,4, 3.031418,0.212765 });
cells.push_back({ 4,8,4, 3.020826,0.212765 });
cells.push_back({ 5,8,4, 3.010234,0.212765 });
cells.push_back({ 6,8,4, 2.999642,0.212765 });
cells.push_back({ 7,8,4, 2.989050,0.212765 });
cells.push_back({ 8,8,4, 2.978459,0.212765 });
cells.push_back({ 9,8,4, 2.967867,0.212765 });
cells.push_back({ 10,8,4, 2.957275,0.212765 });
cells.push_back({ 11,8,4, 2.946683,0.212765 });
// ibackleg=9 imodule=0
// ibackleg=9 imodule=1
// ibackleg=9 imodule=2
// ibackleg=9 imodule=3
// ibackleg=9 imodule=4
// ibackleg=10 imodule=0
// ibackleg=10 imodule=1
cells.push_back({ 0,10,1, -2.887209,-0.416592 });
cells.push_back({ 1,10,1, -2.876341,-0.416592 });
cells.push_back({ 2,10,1, -2.865473,-0.416592 });
cells.push_back({ 3,10,1, -2.854604,-0.416592 });
cells.push_back({ 4,10,1, -2.843736,-0.416592 });
cells.push_back({ 5,10,1, -2.832868,-0.416592 });
cells.push_back({ 6,10,1, -2.821999,-0.416592 });
cells.push_back({ 7,10,1, -2.811131,-0.416592 });
cells.push_back({ 8,10,1, -2.800263,-0.416592 });
cells.push_back({ 9,10,1, -2.789394,-0.416592 });
cells.push_back({ 10,10,1, -2.778526,-0.416592 });
cells.push_back({ 11,10,1, -2.767657,-0.416592 });
// ibackleg=10 imodule=2
cells.push_back({ 0,10,2, -2.887956,-0.212765 });
cells.push_back({ 1,10,2, -2.876952,-0.212765 });
cells.push_back({ 2,10,2, -2.865948,-0.212765 });
cells.push_back({ 3,10,2, -2.854944,-0.212765 });
cells.push_back({ 4,10,2, -2.843939,-0.212765 });
cells.push_back({ 5,10,2, -2.832935,-0.212765 });
cells.push_back({ 6,10,2, -2.821931,-0.212765 });
cells.push_back({ 7,10,2, -2.810927,-0.212765 });
cells.push_back({ 8,10,2, -2.799923,-0.212765 });
cells.push_back({ 9,10,2, -2.788919,-0.212765 });
cells.push_back({ 10,10,2, -2.777915,-0.212765 });
cells.push_back({ 11,10,2, -2.766911,-0.212765 });
// ibackleg=10 imodule=3
cells.push_back({ 0,10,3, -2.887209,-0.000000 });
cells.push_back({ 1,10,3, -2.876341,-0.000000 });
cells.push_back({ 2,10,3, -2.865473,-0.000000 });
cells.push_back({ 3,10,3, -2.854604,-0.000000 });
cells.push_back({ 4,10,3, -2.843736,-0.000000 });
cells.push_back({ 5,10,3, -2.832868,-0.000000 });
cells.push_back({ 6,10,3, -2.821999,-0.000000 });
cells.push_back({ 7,10,3, -2.811131,-0.000000 });
cells.push_back({ 8,10,3, -2.800263,-0.000000 });
cells.push_back({ 9,10,3, -2.789394,-0.000000 });
cells.push_back({ 10,10,3, -2.778526,-0.000000 });
cells.push_back({ 11,10,3, -2.767657,-0.000000 });
// ibackleg=10 imodule=4
cells.push_back({ 0,10,4, -2.766911,0.212765 });
cells.push_back({ 1,10,4, -2.777915,0.212765 });
cells.push_back({ 2,10,4, -2.788919,0.212765 });
cells.push_back({ 3,10,4, -2.799923,0.212765 });
cells.push_back({ 4,10,4, -2.810927,0.212765 });
cells.push_back({ 5,10,4, -2.821931,0.212765 });
cells.push_back({ 6,10,4, -2.832935,0.212765 });
cells.push_back({ 7,10,4, -2.843939,0.212765 });
cells.push_back({ 8,10,4, -2.854944,0.212765 });
cells.push_back({ 9,10,4, -2.865948,0.212765 });
cells.push_back({ 10,10,4, -2.876952,0.212765 });
cells.push_back({ 11,10,4, -2.887956,0.212765 });
// ibackleg=11 imodule=0
// ibackleg=11 imodule=1
cells.push_back({ 0,11,1, -2.676486,-0.416592 });
cells.push_back({ 1,11,1, -2.665851,-0.416592 });
cells.push_back({ 2,11,1, -2.655216,-0.416592 });
cells.push_back({ 3,11,1, -2.644581,-0.416592 });
cells.push_back({ 4,11,1, -2.633946,-0.416592 });
cells.push_back({ 5,11,1, -2.623311,-0.416592 });
cells.push_back({ 6,11,1, -2.612676,-0.416592 });
cells.push_back({ 7,11,1, -2.602042,-0.416592 });
cells.push_back({ 8,11,1, -2.591407,-0.416592 });
cells.push_back({ 9,11,1, -2.580772,-0.416592 });
cells.push_back({ 10,11,1, -2.570137,-0.416592 });
cells.push_back({ 11,11,1, -2.559502,-0.416592 });
// ibackleg=11 imodule=2
cells.push_back({ 0,11,2, -2.677200,-0.212765 });
cells.push_back({ 1,11,2, -2.666435,-0.212765 });
cells.push_back({ 2,11,2, -2.655670,-0.212765 });
cells.push_back({ 3,11,2, -2.644906,-0.212765 });
cells.push_back({ 4,11,2, -2.634141,-0.212765 });
cells.push_back({ 5,11,2, -2.623376,-0.212765 });
cells.push_back({ 6,11,2, -2.612612,-0.212765 });
cells.push_back({ 7,11,2, -2.601847,-0.212765 });
cells.push_back({ 8,11,2, -2.591082,-0.212765 });
cells.push_back({ 9,11,2, -2.580317,-0.212765 });
cells.push_back({ 10,11,2, -2.569553,-0.212765 });
cells.push_back({ 11,11,2, -2.558788,-0.212765 });
// ibackleg=11 imodule=3
cells.push_back({ 0,11,3, -2.676486,-0.000000 });
cells.push_back({ 1,11,3, -2.665851,-0.000000 });
cells.push_back({ 2,11,3, -2.655216,-0.000000 });
cells.push_back({ 3,11,3, -2.644581,-0.000000 });
cells.push_back({ 4,11,3, -2.633946,-0.000000 });
cells.push_back({ 5,11,3, -2.623311,-0.000000 });
cells.push_back({ 6,11,3, -2.612676,-0.000000 });
cells.push_back({ 7,11,3, -2.602042,-0.000000 });
cells.push_back({ 8,11,3, -2.591407,-0.000000 });
cells.push_back({ 9,11,3, -2.580772,-0.000000 });
cells.push_back({ 10,11,3, -2.570137,-0.000000 });
cells.push_back({ 11,11,3, -2.559502,-0.000000 });
// ibackleg=11 imodule=4
cells.push_back({ 0,11,4, -2.558788,0.212765 });
cells.push_back({ 1,11,4, -2.569553,0.212765 });
cells.push_back({ 2,11,4, -2.580317,0.212765 });
cells.push_back({ 3,11,4, -2.591082,0.212765 });
cells.push_back({ 4,11,4, -2.601847,0.212765 });
cells.push_back({ 5,11,4, -2.612612,0.212765 });
cells.push_back({ 6,11,4, -2.623376,0.212765 });
cells.push_back({ 7,11,4, -2.634141,0.212765 });
cells.push_back({ 8,11,4, -2.644906,0.212765 });
cells.push_back({ 9,11,4, -2.655670,0.212765 });
cells.push_back({ 10,11,4, -2.666435,0.212765 });
cells.push_back({ 11,11,4, -2.677200,0.212765 });
// ibackleg=12 imodule=0
// ibackleg=12 imodule=1
// ibackleg=12 imodule=2
cells.push_back({ 0,12,2, -2.466943,-0.212765 });
cells.push_back({ 1,12,2, -2.456327,-0.212765 });
cells.push_back({ 2,12,2, -2.445711,-0.212765 });
cells.push_back({ 3,12,2, -2.435095,-0.212765 });
cells.push_back({ 4,12,2, -2.424479,-0.212765 });
cells.push_back({ 5,12,2, -2.413863,-0.212765 });
cells.push_back({ 6,12,2, -2.403246,-0.212765 });
cells.push_back({ 7,12,2, -2.392630,-0.212765 });
cells.push_back({ 8,12,2, -2.382014,-0.212765 });
cells.push_back({ 9,12,2, -2.371398,-0.212765 });
cells.push_back({ 10,12,2, -2.360782,-0.212765 });
cells.push_back({ 11,12,2, -2.350166,-0.212765 });
// ibackleg=12 imodule=3
cells.push_back({ 0,12,3, -2.466248,-0.000000 });
cells.push_back({ 1,12,3, -2.455759,-0.000000 });
cells.push_back({ 2,12,3, -2.445269,-0.000000 });
cells.push_back({ 3,12,3, -2.434779,-0.000000 });
cells.push_back({ 4,12,3, -2.424289,-0.000000 });
cells.push_back({ 5,12,3, -2.413799,-0.000000 });
cells.push_back({ 6,12,3, -2.403309,-0.000000 });
cells.push_back({ 7,12,3, -2.392820,-0.000000 });
cells.push_back({ 8,12,3, -2.382330,-0.000000 });
cells.push_back({ 9,12,3, -2.371840,-0.000000 });
cells.push_back({ 10,12,3, -2.361350,-0.000000 });
cells.push_back({ 11,12,3, -2.350860,-0.000000 });
// ibackleg=12 imodule=4
cells.push_back({ 0,12,4, -2.350166,0.212765 });
cells.push_back({ 1,12,4, -2.360782,0.212765 });
cells.push_back({ 2,12,4, -2.371398,0.212765 });
cells.push_back({ 3,12,4, -2.382014,0.212765 });
cells.push_back({ 4,12,4, -2.392630,0.212765 });
cells.push_back({ 5,12,4, -2.403246,0.212765 });
cells.push_back({ 6,12,4, -2.413863,0.212765 });
cells.push_back({ 7,12,4, -2.424479,0.212765 });
cells.push_back({ 8,12,4, -2.435095,0.212765 });
cells.push_back({ 9,12,4, -2.445711,0.212765 });
cells.push_back({ 10,12,4, -2.456327,0.212765 });
cells.push_back({ 11,12,4, -2.466943,0.212765 });
// ibackleg=13 imodule=0
// ibackleg=13 imodule=1
// ibackleg=13 imodule=2
cells.push_back({ 0,13,2, -2.257503,-0.212765 });
cells.push_back({ 1,13,2, -2.246888,-0.212765 });
cells.push_back({ 2,13,2, -2.236271,-0.212765 });
cells.push_back({ 3,13,2, -2.225655,-0.212765 });
cells.push_back({ 4,13,2, -2.215039,-0.212765 });
cells.push_back({ 5,13,2, -2.204423,-0.212765 });
cells.push_back({ 6,13,2, -2.193806,-0.212765 });
cells.push_back({ 7,13,2, -2.183191,-0.212765 });
cells.push_back({ 8,13,2, -2.172574,-0.212765 });
cells.push_back({ 9,13,2, -2.161958,-0.212765 });
cells.push_back({ 10,13,2, -2.151342,-0.212765 });
cells.push_back({ 11,13,2, -2.140726,-0.212765 });
// ibackleg=13 imodule=3
cells.push_back({ 0,13,3, -2.256809,-0.000000 });
cells.push_back({ 1,13,3, -2.246319,-0.000000 });
cells.push_back({ 2,13,3, -2.235829,-0.000000 });
cells.push_back({ 3,13,3, -2.225339,-0.000000 });
cells.push_back({ 4,13,3, -2.214849,-0.000000 });
cells.push_back({ 5,13,3, -2.204360,-0.000000 });
cells.push_back({ 6,13,3, -2.193870,-0.000000 });
cells.push_back({ 7,13,3, -2.183380,-0.000000 });
cells.push_back({ 8,13,3, -2.172890,-0.000000 });
cells.push_back({ 9,13,3, -2.162401,-0.000000 });
cells.push_back({ 10,13,3, -2.151911,-0.000000 });
cells.push_back({ 11,13,3, -2.141421,-0.000000 });
// ibackleg=13 imodule=4
cells.push_back({ 0,13,4, -2.140726,0.212765 });
cells.push_back({ 1,13,4, -2.151342,0.212765 });
cells.push_back({ 2,13,4, -2.161958,0.212765 });
cells.push_back({ 3,13,4, -2.172574,0.212765 });
cells.push_back({ 4,13,4, -2.183191,0.212765 });
cells.push_back({ 5,13,4, -2.193806,0.212765 });
cells.push_back({ 6,13,4, -2.204423,0.212765 });
cells.push_back({ 7,13,4, -2.215039,0.212765 });
cells.push_back({ 8,13,4, -2.225655,0.212765 });
cells.push_back({ 9,13,4, -2.236271,0.212765 });
cells.push_back({ 10,13,4, -2.246888,0.212765 });
cells.push_back({ 11,13,4, -2.257503,0.212765 });
// ibackleg=14 imodule=0
// ibackleg=14 imodule=1
// ibackleg=14 imodule=2
cells.push_back({ 0,14,2, -2.048064,-0.212765 });
cells.push_back({ 1,14,2, -2.037448,-0.212765 });
cells.push_back({ 2,14,2, -2.026832,-0.212765 });
cells.push_back({ 3,14,2, -2.016216,-0.212765 });
cells.push_back({ 4,14,2, -2.005599,-0.212765 });
cells.push_back({ 5,14,2, -1.994983,-0.212765 });
cells.push_back({ 6,14,2, -1.984367,-0.212765 });
cells.push_back({ 7,14,2, -1.973751,-0.212765 });
cells.push_back({ 8,14,2, -1.963135,-0.212765 });
cells.push_back({ 9,14,2, -1.952519,-0.212765 });
cells.push_back({ 10,14,2, -1.941902,-0.212765 });
cells.push_back({ 11,14,2, -1.931287,-0.212765 });
// ibackleg=14 imodule=3
cells.push_back({ 0,14,3, -2.047369,-0.000000 });
cells.push_back({ 1,14,3, -2.036879,-0.000000 });
cells.push_back({ 2,14,3, -2.026389,-0.000000 });
cells.push_back({ 3,14,3, -2.015900,-0.000000 });
cells.push_back({ 4,14,3, -2.005410,-0.000000 });
cells.push_back({ 5,14,3, -1.994920,-0.000000 });
cells.push_back({ 6,14,3, -1.984431,-0.000000 });
cells.push_back({ 7,14,3, -1.973941,-0.000000 });
cells.push_back({ 8,14,3, -1.963451,-0.000000 });
cells.push_back({ 9,14,3, -1.952961,-0.000000 });
cells.push_back({ 10,14,3, -1.942471,-0.000000 });
cells.push_back({ 11,14,3, -1.931981,-0.000000 });
// ibackleg=14 imodule=4
cells.push_back({ 0,14,4, -1.931287,0.212765 });
cells.push_back({ 1,14,4, -1.941902,0.212765 });
cells.push_back({ 2,14,4, -1.952519,0.212765 });
cells.push_back({ 3,14,4, -1.963135,0.212765 });
cells.push_back({ 4,14,4, -1.973751,0.212765 });
cells.push_back({ 5,14,4, -1.984367,0.212765 });
cells.push_back({ 6,14,4, -1.994983,0.212765 });
cells.push_back({ 7,14,4, -2.005599,0.212765 });
cells.push_back({ 8,14,4, -2.016216,0.212765 });
cells.push_back({ 9,14,4, -2.026832,0.212765 });
cells.push_back({ 10,14,4, -2.037448,0.212765 });
cells.push_back({ 11,14,4, -2.048064,0.212765 });
// ibackleg=15 imodule=0
// ibackleg=15 imodule=1
// ibackleg=15 imodule=2
cells.push_back({ 0,15,2, -1.838625,-0.212765 });
cells.push_back({ 1,15,2, -1.828008,-0.212765 });
cells.push_back({ 2,15,2, -1.817392,-0.212765 });
cells.push_back({ 3,15,2, -1.806776,-0.212765 });
cells.push_back({ 4,15,2, -1.796160,-0.212765 });
cells.push_back({ 5,15,2, -1.785544,-0.212765 });
cells.push_back({ 6,15,2, -1.774928,-0.212765 });
cells.push_back({ 7,15,2, -1.764312,-0.212765 });
cells.push_back({ 8,15,2, -1.753695,-0.212765 });
cells.push_back({ 9,15,2, -1.743079,-0.212765 });
cells.push_back({ 10,15,2, -1.732463,-0.212765 });
cells.push_back({ 11,15,2, -1.721847,-0.212765 });
// ibackleg=15 imodule=3
cells.push_back({ 0,15,3, -1.837930,-0.000000 });
cells.push_back({ 1,15,3, -1.827440,-0.000000 });
cells.push_back({ 2,15,3, -1.816950,-0.000000 });
cells.push_back({ 3,15,3, -1.806460,-0.000000 });
cells.push_back({ 4,15,3, -1.795970,-0.000000 });
cells.push_back({ 5,15,3, -1.785481,-0.000000 });
cells.push_back({ 6,15,3, -1.774991,-0.000000 });
cells.push_back({ 7,15,3, -1.764501,-0.000000 });
cells.push_back({ 8,15,3, -1.754011,-0.000000 });
cells.push_back({ 9,15,3, -1.743522,-0.000000 });
cells.push_back({ 10,15,3, -1.733032,-0.000000 });
cells.push_back({ 11,15,3, -1.722542,-0.000000 });
// ibackleg=15 imodule=4
cells.push_back({ 0,15,4, -1.721847,0.212765 });
cells.push_back({ 1,15,4, -1.732463,0.212765 });
cells.push_back({ 2,15,4, -1.743079,0.212765 });
cells.push_back({ 3,15,4, -1.753695,0.212765 });
cells.push_back({ 4,15,4, -1.764312,0.212765 });
cells.push_back({ 5,15,4, -1.774928,0.212765 });
cells.push_back({ 6,15,4, -1.785544,0.212765 });
cells.push_back({ 7,15,4, -1.796160,0.212765 });
cells.push_back({ 8,15,4, -1.806776,0.212765 });
cells.push_back({ 9,15,4, -1.817392,0.212765 });
cells.push_back({ 10,15,4, -1.828008,0.212765 });
cells.push_back({ 11,15,4, -1.838625,0.212765 });
// ibackleg=16 imodule=0
// ibackleg=16 imodule=1
// ibackleg=16 imodule=2
cells.push_back({ 0,16,2, -1.629185,-0.212765 });
cells.push_back({ 1,16,2, -1.618569,-0.212765 });
cells.push_back({ 2,16,2, -1.607953,-0.212765 });
cells.push_back({ 3,16,2, -1.597337,-0.212765 });
cells.push_back({ 4,16,2, -1.586720,-0.212765 });
cells.push_back({ 5,16,2, -1.576104,-0.212765 });
cells.push_back({ 6,16,2, -1.565488,-0.212765 });
cells.push_back({ 7,16,2, -1.554872,-0.212765 });
cells.push_back({ 8,16,2, -1.544256,-0.212765 });
cells.push_back({ 9,16,2, -1.533640,-0.212765 });
cells.push_back({ 10,16,2, -1.523023,-0.212765 });
cells.push_back({ 11,16,2, -1.512408,-0.212765 });
// ibackleg=16 imodule=3
cells.push_back({ 0,16,3, -1.628490,-0.000000 });
cells.push_back({ 1,16,3, -1.618000,-0.000000 });
cells.push_back({ 2,16,3, -1.607510,-0.000000 });
cells.push_back({ 3,16,3, -1.597021,-0.000000 });
cells.push_back({ 4,16,3, -1.586531,-0.000000 });
cells.push_back({ 5,16,3, -1.576041,-0.000000 });
cells.push_back({ 6,16,3, -1.565552,-0.000000 });
cells.push_back({ 7,16,3, -1.555062,-0.000000 });
cells.push_back({ 8,16,3, -1.544572,-0.000000 });
cells.push_back({ 9,16,3, -1.534082,-0.000000 });
cells.push_back({ 10,16,3, -1.523592,-0.000000 });
cells.push_back({ 11,16,3, -1.513102,-0.000000 });
// ibackleg=16 imodule=4
cells.push_back({ 0,16,4, -1.512408,0.212765 });
cells.push_back({ 1,16,4, -1.523023,0.212765 });
cells.push_back({ 2,16,4, -1.533640,0.212765 });
cells.push_back({ 3,16,4, -1.544256,0.212765 });
cells.push_back({ 4,16,4, -1.554872,0.212765 });
cells.push_back({ 5,16,4, -1.565488,0.212765 });
cells.push_back({ 6,16,4, -1.576104,0.212765 });
cells.push_back({ 7,16,4, -1.586720,0.212765 });
cells.push_back({ 8,16,4, -1.597337,0.212765 });
cells.push_back({ 9,16,4, -1.607953,0.212765 });
cells.push_back({ 10,16,4, -1.618569,0.212765 });
cells.push_back({ 11,16,4, -1.629185,0.212765 });
// ibackleg=17 imodule=0
// ibackleg=17 imodule=1
// ibackleg=17 imodule=2
cells.push_back({ 0,17,2, -1.419746,-0.212765 });
cells.push_back({ 1,17,2, -1.409129,-0.212765 });
cells.push_back({ 2,17,2, -1.398513,-0.212765 });
cells.push_back({ 3,17,2, -1.387897,-0.212765 });
cells.push_back({ 4,17,2, -1.377281,-0.212765 });
cells.push_back({ 5,17,2, -1.366665,-0.212765 });
cells.push_back({ 6,17,2, -1.356049,-0.212765 });
cells.push_back({ 7,17,2, -1.345433,-0.212765 });
cells.push_back({ 8,17,2, -1.334816,-0.212765 });
cells.push_back({ 9,17,2, -1.324200,-0.212765 });
cells.push_back({ 10,17,2, -1.313584,-0.212765 });
cells.push_back({ 11,17,2, -1.302968,-0.212765 });
// ibackleg=17 imodule=3
cells.push_back({ 0,17,3, -1.419051,-0.000000 });
cells.push_back({ 1,17,3, -1.408561,-0.000000 });
cells.push_back({ 2,17,3, -1.398071,-0.000000 });
cells.push_back({ 3,17,3, -1.387581,-0.000000 });
cells.push_back({ 4,17,3, -1.377092,-0.000000 });
cells.push_back({ 5,17,3, -1.366602,-0.000000 });
cells.push_back({ 6,17,3, -1.356112,-0.000000 });
cells.push_back({ 7,17,3, -1.345622,-0.000000 });
cells.push_back({ 8,17,3, -1.335132,-0.000000 });
cells.push_back({ 9,17,3, -1.324642,-0.000000 });
cells.push_back({ 10,17,3, -1.314153,-0.000000 });
cells.push_back({ 11,17,3, -1.303663,-0.000000 });
// ibackleg=17 imodule=4
cells.push_back({ 0,17,4, -1.302968,0.212765 });
cells.push_back({ 1,17,4, -1.313584,0.212765 });
cells.push_back({ 2,17,4, -1.324200,0.212765 });
cells.push_back({ 3,17,4, -1.334816,0.212765 });
cells.push_back({ 4,17,4, -1.345433,0.212765 });
cells.push_back({ 5,17,4, -1.356049,0.212765 });
cells.push_back({ 6,17,4, -1.366665,0.212765 });
cells.push_back({ 7,17,4, -1.377281,0.212765 });
cells.push_back({ 8,17,4, -1.387897,0.212765 });
cells.push_back({ 9,17,4, -1.398513,0.212765 });
cells.push_back({ 10,17,4, -1.409129,0.212765 });
cells.push_back({ 11,17,4, -1.419746,0.212765 });
// ibackleg=18 imodule=0
// ibackleg=18 imodule=1
// ibackleg=18 imodule=2
cells.push_back({ 0,18,2, -1.210306,-0.212765 });
cells.push_back({ 1,18,2, -1.199690,-0.212765 });
cells.push_back({ 2,18,2, -1.189074,-0.212765 });
cells.push_back({ 3,18,2, -1.178458,-0.212765 });
cells.push_back({ 4,18,2, -1.167842,-0.212765 });
cells.push_back({ 5,18,2, -1.157225,-0.212765 });
cells.push_back({ 6,18,2, -1.146609,-0.212765 });
cells.push_back({ 7,18,2, -1.135993,-0.212765 });
cells.push_back({ 8,18,2, -1.125377,-0.212765 });
cells.push_back({ 9,18,2, -1.114761,-0.212765 });
cells.push_back({ 10,18,2, -1.104145,-0.212765 });
cells.push_back({ 11,18,2, -1.093529,-0.212765 });
// ibackleg=18 imodule=3
cells.push_back({ 0,18,3, -1.209611,-0.000000 });
cells.push_back({ 1,18,3, -1.199121,-0.000000 });
cells.push_back({ 2,18,3, -1.188632,-0.000000 });
cells.push_back({ 3,18,3, -1.178142,-0.000000 });
cells.push_back({ 4,18,3, -1.167652,-0.000000 });
cells.push_back({ 5,18,3, -1.157162,-0.000000 });
cells.push_back({ 6,18,3, -1.146673,-0.000000 });
cells.push_back({ 7,18,3, -1.136183,-0.000000 });
cells.push_back({ 8,18,3, -1.125693,-0.000000 });
cells.push_back({ 9,18,3, -1.115203,-0.000000 });
cells.push_back({ 10,18,3, -1.104713,-0.000000 });
cells.push_back({ 11,18,3, -1.094223,-0.000000 });
// ibackleg=18 imodule=4
cells.push_back({ 0,18,4, -1.093529,0.212765 });
cells.push_back({ 1,18,4, -1.104145,0.212765 });
cells.push_back({ 2,18,4, -1.114761,0.212765 });
cells.push_back({ 3,18,4, -1.125377,0.212765 });
cells.push_back({ 4,18,4, -1.135993,0.212765 });
cells.push_back({ 5,18,4, -1.146609,0.212765 });
cells.push_back({ 6,18,4, -1.157225,0.212765 });
cells.push_back({ 7,18,4, -1.167842,0.212765 });
cells.push_back({ 8,18,4, -1.178458,0.212765 });
cells.push_back({ 9,18,4, -1.189074,0.212765 });
cells.push_back({ 10,18,4, -1.199690,0.212765 });
cells.push_back({ 11,18,4, -1.210306,0.212765 });
// ibackleg=19 imodule=0
// ibackleg=19 imodule=1
// ibackleg=19 imodule=2
cells.push_back({ 0,19,2, -1.000867,-0.212765 });
cells.push_back({ 1,19,2, -0.990250,-0.212765 });
cells.push_back({ 2,19,2, -0.979634,-0.212765 });
cells.push_back({ 3,19,2, -0.969018,-0.212765 });
cells.push_back({ 4,19,2, -0.958402,-0.212765 });
cells.push_back({ 5,19,2, -0.947786,-0.212765 });
cells.push_back({ 6,19,2, -0.937170,-0.212765 });
cells.push_back({ 7,19,2, -0.926554,-0.212765 });
cells.push_back({ 8,19,2, -0.915937,-0.212765 });
cells.push_back({ 9,19,2, -0.905321,-0.212765 });
cells.push_back({ 10,19,2, -0.894705,-0.212765 });
cells.push_back({ 11,19,2, -0.884089,-0.212765 });
// ibackleg=19 imodule=3
cells.push_back({ 0,19,3, -1.000172,-0.000000 });
cells.push_back({ 1,19,3, -0.989682,-0.000000 });
cells.push_back({ 2,19,3, -0.979192,-0.000000 });
cells.push_back({ 3,19,3, -0.968702,-0.000000 });
cells.push_back({ 4,19,3, -0.958213,-0.000000 });
cells.push_back({ 5,19,3, -0.947723,-0.000000 });
cells.push_back({ 6,19,3, -0.937233,-0.000000 });
cells.push_back({ 7,19,3, -0.926743,-0.000000 });
cells.push_back({ 8,19,3, -0.916253,-0.000000 });
cells.push_back({ 9,19,3, -0.905763,-0.000000 });
cells.push_back({ 10,19,3, -0.895274,-0.000000 });
cells.push_back({ 11,19,3, -0.884784,-0.000000 });
// ibackleg=19 imodule=4
cells.push_back({ 0,19,4, -0.884089,0.212765 });
cells.push_back({ 1,19,4, -0.894705,0.212765 });
cells.push_back({ 2,19,4, -0.905321,0.212765 });
cells.push_back({ 3,19,4, -0.915937,0.212765 });
cells.push_back({ 4,19,4, -0.926554,0.212765 });
cells.push_back({ 5,19,4, -0.937170,0.212765 });
cells.push_back({ 6,19,4, -0.947786,0.212765 });
cells.push_back({ 7,19,4, -0.958402,0.212765 });
cells.push_back({ 8,19,4, -0.969018,0.212765 });
cells.push_back({ 9,19,4, -0.979634,0.212765 });
cells.push_back({ 10,19,4, -0.990250,0.212765 });
cells.push_back({ 11,19,4, -1.000867,0.212765 });
// ibackleg=20 imodule=0
// ibackleg=20 imodule=1
// ibackleg=20 imodule=2
cells.push_back({ 0,20,2, -0.791427,-0.212765 });
cells.push_back({ 1,20,2, -0.780811,-0.212765 });
cells.push_back({ 2,20,2, -0.770195,-0.212765 });
cells.push_back({ 3,20,2, -0.759579,-0.212765 });
cells.push_back({ 4,20,2, -0.748963,-0.212765 });
cells.push_back({ 5,20,2, -0.738346,-0.212765 });
cells.push_back({ 6,20,2, -0.727730,-0.212765 });
cells.push_back({ 7,20,2, -0.717114,-0.212765 });
cells.push_back({ 8,20,2, -0.706498,-0.212765 });
cells.push_back({ 9,20,2, -0.695882,-0.212765 });
cells.push_back({ 10,20,2, -0.685266,-0.212765 });
cells.push_back({ 11,20,2, -0.674650,-0.212765 });
// ibackleg=20 imodule=3
cells.push_back({ 0,20,3, -0.790732,-0.000000 });
cells.push_back({ 1,20,3, -0.780242,-0.000000 });
cells.push_back({ 2,20,3, -0.769753,-0.000000 });
cells.push_back({ 3,20,3, -0.759263,-0.000000 });
cells.push_back({ 4,20,3, -0.748773,-0.000000 });
cells.push_back({ 5,20,3, -0.738283,-0.000000 });
cells.push_back({ 6,20,3, -0.727794,-0.000000 });
cells.push_back({ 7,20,3, -0.717304,-0.000000 });
cells.push_back({ 8,20,3, -0.706814,-0.000000 });
cells.push_back({ 9,20,3, -0.696324,-0.000000 });
cells.push_back({ 10,20,3, -0.685834,-0.000000 });
cells.push_back({ 11,20,3, -0.675344,-0.000000 });
// ibackleg=20 imodule=4
cells.push_back({ 0,20,4, -0.674650,0.212765 });
cells.push_back({ 1,20,4, -0.685266,0.212765 });
cells.push_back({ 2,20,4, -0.695882,0.212765 });
cells.push_back({ 3,20,4, -0.706498,0.212765 });
cells.push_back({ 4,20,4, -0.717114,0.212765 });
cells.push_back({ 5,20,4, -0.727730,0.212765 });
cells.push_back({ 6,20,4, -0.738346,0.212765 });
cells.push_back({ 7,20,4, -0.748963,0.212765 });
cells.push_back({ 8,20,4, -0.759579,0.212765 });
cells.push_back({ 9,20,4, -0.770195,0.212765 });
cells.push_back({ 10,20,4, -0.780811,0.212765 });
cells.push_back({ 11,20,4, -0.791427,0.212765 });
// ibackleg=21 imodule=0
// ibackleg=21 imodule=1
cells.push_back({ 0,21,1, -0.582090,-0.416592 });
cells.push_back({ 1,21,1, -0.571456,-0.416592 });
cells.push_back({ 2,21,1, -0.560821,-0.416592 });
cells.push_back({ 3,21,1, -0.550186,-0.416592 });
cells.push_back({ 4,21,1, -0.539551,-0.416592 });
cells.push_back({ 5,21,1, -0.528916,-0.416592 });
cells.push_back({ 6,21,1, -0.518281,-0.416592 });
cells.push_back({ 7,21,1, -0.507646,-0.416592 });
cells.push_back({ 8,21,1, -0.497011,-0.416592 });
cells.push_back({ 9,21,1, -0.486377,-0.416592 });
cells.push_back({ 10,21,1, -0.475742,-0.416592 });
cells.push_back({ 11,21,1, -0.465107,-0.416592 });
// ibackleg=21 imodule=2
cells.push_back({ 0,21,2, -0.582805,-0.212765 });
cells.push_back({ 1,21,2, -0.572040,-0.212765 });
cells.push_back({ 2,21,2, -0.561275,-0.212765 });
cells.push_back({ 3,21,2, -0.550511,-0.212765 });
cells.push_back({ 4,21,2, -0.539746,-0.212765 });
cells.push_back({ 5,21,2, -0.528981,-0.212765 });
cells.push_back({ 6,21,2, -0.518216,-0.212765 });
cells.push_back({ 7,21,2, -0.507452,-0.212765 });
cells.push_back({ 8,21,2, -0.496687,-0.212765 });
cells.push_back({ 9,21,2, -0.485922,-0.212765 });
cells.push_back({ 10,21,2, -0.475158,-0.212765 });
cells.push_back({ 11,21,2, -0.464393,-0.212765 });
// ibackleg=21 imodule=3
cells.push_back({ 0,21,3, -0.582090,-0.000000 });
cells.push_back({ 1,21,3, -0.571456,-0.000000 });
cells.push_back({ 2,21,3, -0.560821,-0.000000 });
cells.push_back({ 3,21,3, -0.550186,-0.000000 });
cells.push_back({ 4,21,3, -0.539551,-0.000000 });
cells.push_back({ 5,21,3, -0.528916,-0.000000 });
cells.push_back({ 6,21,3, -0.518281,-0.000000 });
cells.push_back({ 7,21,3, -0.507646,-0.000000 });
cells.push_back({ 8,21,3, -0.497011,-0.000000 });
cells.push_back({ 9,21,3, -0.486377,-0.000000 });
cells.push_back({ 10,21,3, -0.475742,-0.000000 });
cells.push_back({ 11,21,3, -0.465107,-0.000000 });
// ibackleg=21 imodule=4
cells.push_back({ 0,21,4, -0.464393,0.212765 });
cells.push_back({ 1,21,4, -0.475158,0.212765 });
cells.push_back({ 2,21,4, -0.485922,0.212765 });
cells.push_back({ 3,21,4, -0.496687,0.212765 });
cells.push_back({ 4,21,4, -0.507452,0.212765 });
cells.push_back({ 5,21,4, -0.518216,0.212765 });
cells.push_back({ 6,21,4, -0.528981,0.212765 });
cells.push_back({ 7,21,4, -0.539746,0.212765 });
cells.push_back({ 8,21,4, -0.550511,0.212765 });
cells.push_back({ 9,21,4, -0.561275,0.212765 });
cells.push_back({ 10,21,4, -0.572040,0.212765 });
cells.push_back({ 11,21,4, -0.582805,0.212765 });
// ibackleg=22 imodule=0
// ibackleg=22 imodule=1
cells.push_back({ 0,22,1, -0.373936,-0.416592 });
cells.push_back({ 1,22,1, -0.363067,-0.416592 });
cells.push_back({ 2,22,1, -0.352198,-0.416592 });
cells.push_back({ 3,22,1, -0.341330,-0.416592 });
cells.push_back({ 4,22,1, -0.330462,-0.416592 });
cells.push_back({ 5,22,1, -0.319593,-0.416592 });
cells.push_back({ 6,22,1, -0.308725,-0.416592 });
cells.push_back({ 7,22,1, -0.297857,-0.416592 });
cells.push_back({ 8,22,1, -0.286988,-0.416592 });
cells.push_back({ 9,22,1, -0.276120,-0.416592 });
cells.push_back({ 10,22,1, -0.265251,-0.416592 });
cells.push_back({ 11,22,1, -0.254383,-0.416592 });
// ibackleg=22 imodule=2
cells.push_back({ 0,22,2, -0.374681,-0.212765 });
cells.push_back({ 1,22,2, -0.363677,-0.212765 });
cells.push_back({ 2,22,2, -0.352673,-0.212765 });
cells.push_back({ 3,22,2, -0.341669,-0.212765 });
cells.push_back({ 4,22,2, -0.330665,-0.212765 });
cells.push_back({ 5,22,2, -0.319661,-0.212765 });
cells.push_back({ 6,22,2, -0.308657,-0.212765 });
cells.push_back({ 7,22,2, -0.297653,-0.212765 });
cells.push_back({ 8,22,2, -0.286649,-0.212765 });
cells.push_back({ 9,22,2, -0.275645,-0.212765 });
cells.push_back({ 10,22,2, -0.264641,-0.212765 });
cells.push_back({ 11,22,2, -0.253637,-0.212765 });
// ibackleg=22 imodule=3
cells.push_back({ 0,22,3, -0.373936,-0.000000 });
cells.push_back({ 1,22,3, -0.363067,-0.000000 });
cells.push_back({ 2,22,3, -0.352198,-0.000000 });
cells.push_back({ 3,22,3, -0.341330,-0.000000 });
cells.push_back({ 4,22,3, -0.330462,-0.000000 });
cells.push_back({ 5,22,3, -0.319593,-0.000000 });
cells.push_back({ 6,22,3, -0.308725,-0.000000 });
cells.push_back({ 7,22,3, -0.297857,-0.000000 });
cells.push_back({ 8,22,3, -0.286988,-0.000000 });
cells.push_back({ 9,22,3, -0.276120,-0.000000 });
cells.push_back({ 10,22,3, -0.265251,-0.000000 });
cells.push_back({ 11,22,3, -0.254383,-0.000000 });
// ibackleg=22 imodule=4
cells.push_back({ 0,22,4, -0.253637,0.212765 });
cells.push_back({ 1,22,4, -0.264641,0.212765 });
cells.push_back({ 2,22,4, -0.275645,0.212765 });
cells.push_back({ 3,22,4, -0.286649,0.212765 });
cells.push_back({ 4,22,4, -0.297653,0.212765 });
cells.push_back({ 5,22,4, -0.308657,0.212765 });
cells.push_back({ 6,22,4, -0.319661,0.212765 });
cells.push_back({ 7,22,4, -0.330665,0.212765 });
cells.push_back({ 8,22,4, -0.341669,0.212765 });
cells.push_back({ 9,22,4, -0.352673,0.212765 });
cells.push_back({ 10,22,4, -0.363677,0.212765 });
cells.push_back({ 11,22,4, -0.374681,0.212765 });
// ibackleg=23 imodule=0
// ibackleg=23 imodule=1
// ibackleg=23 imodule=2
// ibackleg=23 imodule=3
// ibackleg=23 imodule=4
// ibackleg=24 imodule=0
// ibackleg=24 imodule=1
cells.push_back({ 0,24,1, 0.066479,-0.416592 });
cells.push_back({ 1,24,1, 0.077280,-0.416592 });
cells.push_back({ 2,24,1, 0.088080,-0.416592 });
cells.push_back({ 3,24,1, 0.098881,-0.416592 });
cells.push_back({ 4,24,1, 0.109682,-0.416592 });
cells.push_back({ 5,24,1, 0.120482,-0.416592 });
cells.push_back({ 6,24,1, 0.131283,-0.416592 });
cells.push_back({ 7,24,1, 0.142084,-0.416592 });
cells.push_back({ 8,24,1, 0.152884,-0.416592 });
cells.push_back({ 9,24,1, 0.163685,-0.416592 });
cells.push_back({ 10,24,1, 0.174485,-0.416592 });
cells.push_back({ 11,24,1, 0.185286,-0.416592 });
// ibackleg=24 imodule=2
cells.push_back({ 0,24,2, 0.066005,-0.212765 });
cells.push_back({ 1,24,2, 0.076940,-0.212765 });
cells.push_back({ 2,24,2, 0.087874,-0.212765 });
cells.push_back({ 3,24,2, 0.098809,-0.212765 });
cells.push_back({ 4,24,2, 0.109743,-0.212765 });
cells.push_back({ 5,24,2, 0.120678,-0.212765 });
cells.push_back({ 6,24,2, 0.131612,-0.212765 });
cells.push_back({ 7,24,2, 0.142547,-0.212765 });
cells.push_back({ 8,24,2, 0.153481,-0.212765 });
cells.push_back({ 9,24,2, 0.164416,-0.212765 });
cells.push_back({ 10,24,2, 0.175351,-0.212765 });
cells.push_back({ 11,24,2, 0.186285,-0.212765 });
// ibackleg=24 imodule=3
cells.push_back({ 0,24,3, 0.066479,-0.000000 });
cells.push_back({ 1,24,3, 0.077280,-0.000000 });
cells.push_back({ 2,24,3, 0.088080,-0.000000 });
cells.push_back({ 3,24,3, 0.098881,-0.000000 });
cells.push_back({ 4,24,3, 0.109682,-0.000000 });
cells.push_back({ 5,24,3, 0.120482,-0.000000 });
cells.push_back({ 6,24,3, 0.131283,-0.000000 });
cells.push_back({ 7,24,3, 0.142084,-0.000000 });
cells.push_back({ 8,24,3, 0.152884,-0.000000 });
cells.push_back({ 9,24,3, 0.163685,-0.000000 });
cells.push_back({ 10,24,3, 0.174485,-0.000000 });
cells.push_back({ 11,24,3, 0.185286,-0.000000 });
// ibackleg=24 imodule=4
cells.push_back({ 0,24,4, 0.186285,0.212765 });
cells.push_back({ 1,24,4, 0.175351,0.212765 });
cells.push_back({ 2,24,4, 0.164416,0.212765 });
cells.push_back({ 3,24,4, 0.153481,0.212765 });
cells.push_back({ 4,24,4, 0.142547,0.212765 });
cells.push_back({ 5,24,4, 0.131612,0.212765 });
cells.push_back({ 6,24,4, 0.120678,0.212765 });
cells.push_back({ 7,24,4, 0.109743,0.212765 });
cells.push_back({ 8,24,4, 0.098809,0.212765 });
cells.push_back({ 9,24,4, 0.087874,0.212765 });
cells.push_back({ 10,24,4, 0.076940,0.212765 });
cells.push_back({ 11,24,4, 0.066005,0.212765 });
// ibackleg=25 imodule=0
// ibackleg=25 imodule=1
cells.push_back({ 0,25,1, 0.254383,-0.416592 });
cells.push_back({ 1,25,1, 0.265252,-0.416592 });
cells.push_back({ 2,25,1, 0.276120,-0.416592 });
cells.push_back({ 3,25,1, 0.286988,-0.416592 });
cells.push_back({ 4,25,1, 0.297857,-0.416592 });
cells.push_back({ 5,25,1, 0.308725,-0.416592 });
cells.push_back({ 6,25,1, 0.319593,-0.416592 });
cells.push_back({ 7,25,1, 0.330462,-0.416592 });
cells.push_back({ 8,25,1, 0.341330,-0.416592 });
cells.push_back({ 9,25,1, 0.352199,-0.416592 });
cells.push_back({ 10,25,1, 0.363067,-0.416592 });
cells.push_back({ 11,25,1, 0.373935,-0.416592 });
// ibackleg=25 imodule=2
cells.push_back({ 0,25,2, 0.253637,-0.212765 });
cells.push_back({ 1,25,2, 0.264641,-0.212765 });
cells.push_back({ 2,25,2, 0.275645,-0.212765 });
cells.push_back({ 3,25,2, 0.286649,-0.212765 });
cells.push_back({ 4,25,2, 0.297653,-0.212765 });
cells.push_back({ 5,25,2, 0.308657,-0.212765 });
cells.push_back({ 6,25,2, 0.319661,-0.212765 });
cells.push_back({ 7,25,2, 0.330665,-0.212765 });
cells.push_back({ 8,25,2, 0.341669,-0.212765 });
cells.push_back({ 9,25,2, 0.352673,-0.212765 });
cells.push_back({ 10,25,2, 0.363677,-0.212765 });
cells.push_back({ 11,25,2, 0.374682,-0.212765 });
// ibackleg=25 imodule=3
cells.push_back({ 0,25,3, 0.254383,-0.000000 });
cells.push_back({ 1,25,3, 0.265252,-0.000000 });
cells.push_back({ 2,25,3, 0.276120,-0.000000 });
cells.push_back({ 3,25,3, 0.286988,-0.000000 });
cells.push_back({ 4,25,3, 0.297857,-0.000000 });
cells.push_back({ 5,25,3, 0.308725,-0.000000 });
cells.push_back({ 6,25,3, 0.319593,-0.000000 });
cells.push_back({ 7,25,3, 0.330462,-0.000000 });
cells.push_back({ 8,25,3, 0.341330,-0.000000 });
cells.push_back({ 9,25,3, 0.352199,-0.000000 });
cells.push_back({ 10,25,3, 0.363067,-0.000000 });
cells.push_back({ 11,25,3, 0.373935,-0.000000 });
// ibackleg=25 imodule=4
cells.push_back({ 0,25,4, 0.374682,0.212765 });
cells.push_back({ 1,25,4, 0.363677,0.212765 });
cells.push_back({ 2,25,4, 0.352673,0.212765 });
cells.push_back({ 3,25,4, 0.341669,0.212765 });
cells.push_back({ 4,25,4, 0.330665,0.212765 });
cells.push_back({ 5,25,4, 0.319661,0.212765 });
cells.push_back({ 6,25,4, 0.308657,0.212765 });
cells.push_back({ 7,25,4, 0.297653,0.212765 });
cells.push_back({ 8,25,4, 0.286649,0.212765 });
cells.push_back({ 9,25,4, 0.275645,0.212765 });
cells.push_back({ 10,25,4, 0.264641,0.212765 });
cells.push_back({ 11,25,4, 0.253637,0.212765 });
// ibackleg=26 imodule=0
// ibackleg=26 imodule=1
cells.push_back({ 0,26,1, 0.462821,-0.416592 });
cells.push_back({ 1,26,1, 0.473872,-0.416592 });
cells.push_back({ 2,26,1, 0.484922,-0.416592 });
cells.push_back({ 3,26,1, 0.495973,-0.416592 });
cells.push_back({ 4,26,1, 0.507023,-0.416592 });
cells.push_back({ 5,26,1, 0.518074,-0.416592 });
cells.push_back({ 6,26,1, 0.529124,-0.416592 });
cells.push_back({ 7,26,1, 0.540174,-0.416592 });
cells.push_back({ 8,26,1, 0.551225,-0.416592 });
cells.push_back({ 9,26,1, 0.562275,-0.416592 });
cells.push_back({ 10,26,1, 0.573326,-0.416592 });
cells.push_back({ 11,26,1, 0.584376,-0.416592 });
// ibackleg=26 imodule=2
cells.push_back({ 0,26,2, 0.462050,-0.212765 });
cells.push_back({ 1,26,2, 0.473241,-0.212765 });
cells.push_back({ 2,26,2, 0.484431,-0.212765 });
cells.push_back({ 3,26,2, 0.495622,-0.212765 });
cells.push_back({ 4,26,2, 0.506813,-0.212765 });
cells.push_back({ 5,26,2, 0.518003,-0.212765 });
cells.push_back({ 6,26,2, 0.529194,-0.212765 });
cells.push_back({ 7,26,2, 0.540385,-0.212765 });
cells.push_back({ 8,26,2, 0.551576,-0.212765 });
cells.push_back({ 9,26,2, 0.562766,-0.212765 });
cells.push_back({ 10,26,2, 0.573957,-0.212765 });
cells.push_back({ 11,26,2, 0.585148,-0.212765 });
// ibackleg=26 imodule=3
cells.push_back({ 0,26,3, 0.462821,-0.000000 });
cells.push_back({ 1,26,3, 0.473872,-0.000000 });
cells.push_back({ 2,26,3, 0.484922,-0.000000 });
cells.push_back({ 3,26,3, 0.495973,-0.000000 });
cells.push_back({ 4,26,3, 0.507023,-0.000000 });
cells.push_back({ 5,26,3, 0.518074,-0.000000 });
cells.push_back({ 6,26,3, 0.529124,-0.000000 });
cells.push_back({ 7,26,3, 0.540174,-0.000000 });
cells.push_back({ 8,26,3, 0.551225,-0.000000 });
cells.push_back({ 9,26,3, 0.562275,-0.000000 });
cells.push_back({ 10,26,3, 0.573326,-0.000000 });
cells.push_back({ 11,26,3, 0.584376,-0.000000 });
// ibackleg=26 imodule=4
cells.push_back({ 0,26,4, 0.585148,0.212765 });
cells.push_back({ 1,26,4, 0.573957,0.212765 });
cells.push_back({ 2,26,4, 0.562766,0.212765 });
cells.push_back({ 3,26,4, 0.551576,0.212765 });
cells.push_back({ 4,26,4, 0.540385,0.212765 });
cells.push_back({ 5,26,4, 0.529194,0.212765 });
cells.push_back({ 6,26,4, 0.518003,0.212765 });
cells.push_back({ 7,26,4, 0.506813,0.212765 });
cells.push_back({ 8,26,4, 0.495622,0.212765 });
cells.push_back({ 9,26,4, 0.484431,0.212765 });
cells.push_back({ 10,26,4, 0.473241,0.212765 });
cells.push_back({ 11,26,4, 0.462050,0.212765 });
// ibackleg=27 imodule=0
// ibackleg=27 imodule=1
cells.push_back({ 0,27,1, 0.672261,-0.416592 });
cells.push_back({ 1,27,1, 0.683311,-0.416592 });
cells.push_back({ 2,27,1, 0.694362,-0.416592 });
cells.push_back({ 3,27,1, 0.705412,-0.416592 });
cells.push_back({ 4,27,1, 0.716463,-0.416592 });
cells.push_back({ 5,27,1, 0.727513,-0.416592 });
cells.push_back({ 6,27,1, 0.738563,-0.416592 });
cells.push_back({ 7,27,1, 0.749614,-0.416592 });
cells.push_back({ 8,27,1, 0.760664,-0.416592 });
cells.push_back({ 9,27,1, 0.771715,-0.416592 });
cells.push_back({ 10,27,1, 0.782765,-0.416592 });
cells.push_back({ 11,27,1, 0.793816,-0.416592 });
// ibackleg=27 imodule=2
cells.push_back({ 0,27,2, 0.671489,-0.212765 });
cells.push_back({ 1,27,2, 0.682680,-0.212765 });
cells.push_back({ 2,27,2, 0.693871,-0.212765 });
cells.push_back({ 3,27,2, 0.705061,-0.212765 });
cells.push_back({ 4,27,2, 0.716252,-0.212765 });
cells.push_back({ 5,27,2, 0.727443,-0.212765 });
cells.push_back({ 6,27,2, 0.738634,-0.212765 });
cells.push_back({ 7,27,2, 0.749824,-0.212765 });
cells.push_back({ 8,27,2, 0.761015,-0.212765 });
cells.push_back({ 9,27,2, 0.772206,-0.212765 });
cells.push_back({ 10,27,2, 0.783396,-0.212765 });
cells.push_back({ 11,27,2, 0.794587,-0.212765 });
// ibackleg=27 imodule=3
cells.push_back({ 0,27,3, 0.672261,-0.000000 });
cells.push_back({ 1,27,3, 0.683311,-0.000000 });
cells.push_back({ 2,27,3, 0.694362,-0.000000 });
cells.push_back({ 3,27,3, 0.705412,-0.000000 });
cells.push_back({ 4,27,3, 0.716463,-0.000000 });
cells.push_back({ 5,27,3, 0.727513,-0.000000 });
cells.push_back({ 6,27,3, 0.738563,-0.000000 });
cells.push_back({ 7,27,3, 0.749614,-0.000000 });
cells.push_back({ 8,27,3, 0.760664,-0.000000 });
cells.push_back({ 9,27,3, 0.771715,-0.000000 });
cells.push_back({ 10,27,3, 0.782765,-0.000000 });
cells.push_back({ 11,27,3, 0.793816,-0.000000 });
// ibackleg=27 imodule=4
cells.push_back({ 0,27,4, 0.794587,0.212765 });
cells.push_back({ 1,27,4, 0.783396,0.212765 });
cells.push_back({ 2,27,4, 0.772206,0.212765 });
cells.push_back({ 3,27,4, 0.761015,0.212765 });
cells.push_back({ 4,27,4, 0.749824,0.212765 });
cells.push_back({ 5,27,4, 0.738634,0.212765 });
cells.push_back({ 6,27,4, 0.727443,0.212765 });
cells.push_back({ 7,27,4, 0.716252,0.212765 });
cells.push_back({ 8,27,4, 0.705061,0.212765 });
cells.push_back({ 9,27,4, 0.693871,0.212765 });
cells.push_back({ 10,27,4, 0.682680,0.212765 });
cells.push_back({ 11,27,4, 0.671489,0.212765 });
// ibackleg=28 imodule=0
// ibackleg=28 imodule=1
cells.push_back({ 0,28,1, 0.881700,-0.416592 });
cells.push_back({ 1,28,1, 0.892751,-0.416592 });
cells.push_back({ 2,28,1, 0.903801,-0.416592 });
cells.push_back({ 3,28,1, 0.914852,-0.416592 });
cells.push_back({ 4,28,1, 0.925902,-0.416592 });
cells.push_back({ 5,28,1, 0.936953,-0.416592 });
cells.push_back({ 6,28,1, 0.948003,-0.416592 });
cells.push_back({ 7,28,1, 0.959053,-0.416592 });
cells.push_back({ 8,28,1, 0.970104,-0.416592 });
cells.push_back({ 9,28,1, 0.981154,-0.416592 });
cells.push_back({ 10,28,1, 0.992205,-0.416592 });
cells.push_back({ 11,28,1, 1.003255,-0.416592 });
// ibackleg=28 imodule=2
cells.push_back({ 0,28,2, 0.880929,-0.212765 });
cells.push_back({ 1,28,2, 0.892120,-0.212765 });
cells.push_back({ 2,28,2, 0.903310,-0.212765 });
cells.push_back({ 3,28,2, 0.914501,-0.212765 });
cells.push_back({ 4,28,2, 0.925692,-0.212765 });
cells.push_back({ 5,28,2, 0.936882,-0.212765 });
cells.push_back({ 6,28,2, 0.948073,-0.212765 });
cells.push_back({ 7,28,2, 0.959264,-0.212765 });
cells.push_back({ 8,28,2, 0.970455,-0.212765 });
cells.push_back({ 9,28,2, 0.981645,-0.212765 });
cells.push_back({ 10,28,2, 0.992836,-0.212765 });
cells.push_back({ 11,28,2, 1.004027,-0.212765 });
// ibackleg=28 imodule=3
cells.push_back({ 0,28,3, 0.881700,-0.000000 });
cells.push_back({ 1,28,3, 0.892751,-0.000000 });
cells.push_back({ 2,28,3, 0.903801,-0.000000 });
cells.push_back({ 3,28,3, 0.914852,-0.000000 });
cells.push_back({ 4,28,3, 0.925902,-0.000000 });
cells.push_back({ 5,28,3, 0.936953,-0.000000 });
cells.push_back({ 6,28,3, 0.948003,-0.000000 });
cells.push_back({ 7,28,3, 0.959053,-0.000000 });
cells.push_back({ 8,28,3, 0.970104,-0.000000 });
cells.push_back({ 9,28,3, 0.981154,-0.000000 });
cells.push_back({ 10,28,3, 0.992205,-0.000000 });
cells.push_back({ 11,28,3, 1.003255,-0.000000 });
// ibackleg=28 imodule=4
cells.push_back({ 0,28,4, 1.004027,0.212765 });
cells.push_back({ 1,28,4, 0.992836,0.212765 });
cells.push_back({ 2,28,4, 0.981645,0.212765 });
cells.push_back({ 3,28,4, 0.970455,0.212765 });
cells.push_back({ 4,28,4, 0.959264,0.212765 });
cells.push_back({ 5,28,4, 0.948073,0.212765 });
cells.push_back({ 6,28,4, 0.936882,0.212765 });
cells.push_back({ 7,28,4, 0.925692,0.212765 });
cells.push_back({ 8,28,4, 0.914501,0.212765 });
cells.push_back({ 9,28,4, 0.903310,0.212765 });
cells.push_back({ 10,28,4, 0.892120,0.212765 });
cells.push_back({ 11,28,4, 0.880929,0.212765 });
// ibackleg=29 imodule=0
// ibackleg=29 imodule=1
cells.push_back({ 0,29,1, 1.091140,-0.416592 });
cells.push_back({ 1,29,1, 1.102190,-0.416592 });
cells.push_back({ 2,29,1, 1.113241,-0.416592 });
cells.push_back({ 3,29,1, 1.124291,-0.416592 });
cells.push_back({ 4,29,1, 1.135342,-0.416592 });
cells.push_back({ 5,29,1, 1.146392,-0.416592 });
cells.push_back({ 6,29,1, 1.157443,-0.416592 });
cells.push_back({ 7,29,1, 1.168493,-0.416592 });
cells.push_back({ 8,29,1, 1.179543,-0.416592 });
cells.push_back({ 9,29,1, 1.190594,-0.416592 });
cells.push_back({ 10,29,1, 1.201644,-0.416592 });
cells.push_back({ 11,29,1, 1.212695,-0.416592 });
// ibackleg=29 imodule=2
cells.push_back({ 0,29,2, 1.090368,-0.212765 });
cells.push_back({ 1,29,2, 1.101559,-0.212765 });
cells.push_back({ 2,29,2, 1.112750,-0.212765 });
cells.push_back({ 3,29,2, 1.123941,-0.212765 });
cells.push_back({ 4,29,2, 1.135131,-0.212765 });
cells.push_back({ 5,29,2, 1.146322,-0.212765 });
cells.push_back({ 6,29,2, 1.157513,-0.212765 });
cells.push_back({ 7,29,2, 1.168703,-0.212765 });
cells.push_back({ 8,29,2, 1.179894,-0.212765 });
cells.push_back({ 9,29,2, 1.191085,-0.212765 });
cells.push_back({ 10,29,2, 1.202276,-0.212765 });
cells.push_back({ 11,29,2, 1.213466,-0.212765 });
// ibackleg=29 imodule=3
cells.push_back({ 0,29,3, 1.091140,-0.000000 });
cells.push_back({ 1,29,3, 1.102190,-0.000000 });
cells.push_back({ 2,29,3, 1.113241,-0.000000 });
cells.push_back({ 3,29,3, 1.124291,-0.000000 });
cells.push_back({ 4,29,3, 1.135342,-0.000000 });
cells.push_back({ 5,29,3, 1.146392,-0.000000 });
cells.push_back({ 6,29,3, 1.157443,-0.000000 });
cells.push_back({ 7,29,3, 1.168493,-0.000000 });
cells.push_back({ 8,29,3, 1.179543,-0.000000 });
cells.push_back({ 9,29,3, 1.190594,-0.000000 });
cells.push_back({ 10,29,3, 1.201644,-0.000000 });
cells.push_back({ 11,29,3, 1.212695,-0.000000 });
// ibackleg=29 imodule=4
cells.push_back({ 0,29,4, 1.213466,0.212765 });
cells.push_back({ 1,29,4, 1.202276,0.212765 });
cells.push_back({ 2,29,4, 1.191085,0.212765 });
cells.push_back({ 3,29,4, 1.179894,0.212765 });
cells.push_back({ 4,29,4, 1.168703,0.212765 });
cells.push_back({ 5,29,4, 1.157513,0.212765 });
cells.push_back({ 6,29,4, 1.146322,0.212765 });
cells.push_back({ 7,29,4, 1.135131,0.212765 });
cells.push_back({ 8,29,4, 1.123941,0.212765 });
cells.push_back({ 9,29,4, 1.112750,0.212765 });
cells.push_back({ 10,29,4, 1.101559,0.212765 });
cells.push_back({ 11,29,4, 1.090368,0.212765 });

}
