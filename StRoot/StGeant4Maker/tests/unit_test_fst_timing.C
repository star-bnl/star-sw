#include "tests/unit_tests.h"
#include <assert.h>

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

//___________________________________________________________________
double _eta  = 0; 
double _phid = 0;
//___________________________________________________________________
void throw_muon_in_fst() {


  throw_particle( 1, "mu+,mu-", 0.2, 5.0, 2.5, 4.0, 0.0, TMath::TwoPi() );

  vertex_table = dynamic_cast<TTable*>( chain->GetDataSet("g2t_vertex")  ) ;
  track_table  = dynamic_cast<TTable*>( chain->GetDataSet("g2t_track")   ) ;
  hit_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_fsi_hit") ) ;

  const g2t_track_st* track = static_cast<const g2t_track_st*>( track_table->At(0) );
  _eta = track->eta;

  _phid = TMath::ATan2( track->p[1], track->p[0] ) * 180.0 / TMath::Pi();
  while ( _phid < 0 )  _phid+=360;
 
  
}
//___________________________________________________________________
void unit_test_fst_timing( int longtest=10000 ) {

  gROOT->ProcessLine("initChain();");

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.,0.,0.);
  pm->SetSigma(0.0,0.,0.);

  Accumulator_t time; // Time per thrown muon
  Accumulator_t time_phi[181]; // time in 2 degree phi slices

  for ( int event=0;event<longtest;event++ ) {

    LOG_TEST << "Event number " << event  << " -------------------------" << std::endl;
    timer.Start();
    throw_muon_in_fst();
    time(timer.CpuTime());
    time_phi[ int(_phid/2) ]( timer.CpuTime() );
    timer.Print();
    timer.Reset();
      
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
             
  }

  auto printTime = [=]( const std::string& message, const Accumulator_t& time ) {
  
    int    _count         = boost::accumulators::count(time);
    double _mean          = boost::accumulators::mean(time);
    double _median        = boost::accumulators::median(time);
    double _min           = boost::accumulators::min(time);
    double _max           = boost::accumulators::max(time);
    double _error_of_mean = boost::accumulators::error_of<tag::mean>(time); 

    LOG_TEST << message.c_str() << std::endl;
    LOG_TEST << Form( "count                      = %i",   _count )         << std::endl;
    LOG_TEST << Form( "time / muon: mean          = %f s", _mean )          << std::endl;
    LOG_TEST << Form( "time / muon: median        = %f s", _median )        << std::endl;
    LOG_TEST << Form( "time / muon: min           = %f s", _min  )          << std::endl;
    LOG_TEST << Form( "time / muon: max           = %f s", _max  )          << std::endl;
    if ( _count > 2 ) 
      LOG_TEST << Form( "time / muon: error of mean = %f s", _error_of_mean ) << std::endl;
  

  };

  printTime( "TOTAL /////////////////////////////////", time );
  int index=0;
  for ( auto t : time_phi ) {

    printTime( Form("PHI=%i /////////////////////////////////",2*index), time_phi[index] );
    index++;

  }
  
}
