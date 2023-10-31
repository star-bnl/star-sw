#include "tests/unit_tests.h"
#include <assert.h>

//___________________________________________________________________
double _eta  = 0; 
double _phid = 0;
//___________________________________________________________________

std::map<int,int> idIsNotUnique;
int               expectedId = 0;

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

struct Track_t {
  double pt;
  double phi;
  double x0;
  double y0;
  double z0;
};

void unit_test_tdm_truth() {

  gROOT->ProcessLine("initChain();");

  Accumulator_t edep; // Energy deposition
  Accumulator_t time; // Time per throw

  auto* chain = StMaker::GetChain();
  auto* kine  = dynamic_cast<StarKinematics*>( chain->GetMaker("StarKine") );
  auto* pm    = dynamic_cast<StarPrimaryMaker*>( chain->GetMaker("PrimaryMaker") );
  pm->SetSigma(0.0,0.,0.);

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Unit testing of tracks "     << std::endl;
  LOG_TEST << "=======================================================" << std::endl;

  timer.Start();



  std::vector<Track_t> input_tracks = {
    { 1.0,  90.0,   0.0, 0.0, -90.0 },
    { 2.0,  90.0,   0.0, 0.0, -70.0 },
    { 3.0,  90.0,   0.0, 0.0, -50.0 },
    { 4.0,  90.0,   0.0, 0.0, -30.0 },
    { 5.0,  90.0,   0.0, 0.0, -10.0 },
    { 6.0,  90.0,   0.0, 0.0,  10.0 },
    { 7.0,  90.0,   0.0, 0.0,  30.0 },
    { 8.0,  90.0,   0.0, 0.0,  50.0 },
    { 9.0,  90.0,   0.0, 0.0,  70.0 },
    { 10.,  90.0,   0.0, 0.0,  90.0 }
  };

  for ( auto t : input_tracks ) {

    pm->SetVertex( t.x0, t.y0, t.z0 );
    auto* part = kine->AddParticle("e+");

    double px = t.pt * TMath::Cos( t.phi * TMath::Pi()/180.0 ); // required eta=0 for now
    double py = t.pt * TMath::Sin( t.phi * TMath::Pi()/180.0 );
    double pz = 0.0;

    part->SetPx(px);
    part->SetPy(py);
    part->SetPz(pz);

    double mass = part->GetMass();  assert(mass>0);
    double energy = TMath::Sqrt(px*px+py*py+mass*mass+pz*pz);

    part->SetEnergy( energy );
  
  }

  chain->Clear();
  chain->Make();

  timer.Stop();
 
  vertex_table      = dynamic_cast<TTable*>( chain->GetDataSet("g2t_vertex")  );
  track_table       = dynamic_cast<TTable*>( chain->GetDataSet("g2t_track")   );
  auto emc_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_emc_hit") ); // EMC hits
  auto tpc_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_tpc_hit") ); // TPC hits
  
  // TRACK VALIDATION
  //  for ( int idx=0;idx<track_table->GetNRows();idx++ ) 
  for ( int idx=0;idx<10;idx++ )     {

  check_track( "A particle must have been processed by geant",                      [=](const g2t_track_st* t){
      LOG_TEST << "-----------------------------------------------------------" << std::endl;
      assert(t);
      std::string result = Form("particle id = %i",t->eg_pid);
      return result + PASS; 
    }, idx);
  check_track( "The track has a unique ID",                                         [=](const g2t_track_st* t){
      std::string result = Form("unique id = %i",t->id);
      if ( idIsNotUnique[ t->id ] ) result += FAIL;
      else                          result += PASS;
      idIsNotUnique[ t->id ]++;      
      return result;
    }, idx);
  check_track( "The track IDs are in numerical order",                              [=](const g2t_track_st* t){
      std::string result = Form("unique id = %i",t->id);
      expectedId++;
      if ( expectedId == t->id  ) result += PASS;
      else                          result += FAIL;
      return result;
    }, idx);

  check_track( "The track should have a start vertex",                              [=](const g2t_track_st* t){
      return (t->start_vertex_p>0)?PASS:FAIL;      
    }, idx);
  
 
  if ( idx<input_tracks.size() ){
    check_track( "... the track should be a positron",                              [=](const g2t_track_st* t){
	std::string result = FAIL;
	auto pdgid = t->eg_pid;
	if ( pdgid == -11 ) result = PASS;
	return result;
      }, idx);
  }
  
  if ( idx<input_tracks.size() ){
    check_track( "... track vertex should be on the beamline",                      [=](const g2t_track_st* t){
	std::string result = FAIL;
	auto idv = t->start_vertex_p;
	if ( idv>0 ) {
	  auto vertex = static_cast<const g2t_vertex_st*>( vertex_table->At(idv-1) );
	  auto x = vertex->ge_x[0];
	  auto y = vertex->ge_x[1];
	  auto r = TMath::Sqrt(x*x+y*y);
	  if ( r<0.01 ) result = PASS;
	}
	return result;
      }, idx);
   }

  if ( idx<input_tracks.size() ){
    check_track( "... the idtruth should be numerically increasing",                [=](const g2t_track_st* t){
	std::string result = FAIL;
	auto truth = t->id;
	if ( t->id==idx+1) result = PASS;
	return result;
      }, idx);
  }  

  if ( idx<input_tracks.size() ){
    check_track( "... the idtruth should match the ith thrown track",               [=](const g2t_track_st* t){
	std::string result = FAIL;
	auto truth = t->id;
	if ( t->id==int(t->pt) ) result = PASS;
	return result;
      }, idx);
  }  

  if ( idx<input_tracks.size() ){
    check_track( "... the track should have 76 tpc hits",                           [=](const g2t_track_st* t){
	std::string result = FAIL;
	if ( t->n_tpc_hit==int(76) ) result = PASS;
	return result;
      }, idx);
  }  

  if ( idx<input_tracks.size() ){
    check_track( "... the track should have emc hits",                               [=](const g2t_track_st* t){
	std::string result = FAIL;
	if ( t->n_emc_hit ) result = PASS;
	return result;
      }, idx);
  }  

  check_track( "... does the track have a stop vertex?",                             [=](const g2t_track_st* t){
      return (t->stop_vertex_p>0)?YES:NOPE;
    }, idx);


  // check_track( "The track should have a stop vertex",                               [=](const g2t_track_st* t){
  //     return (t->stop_vertex_p>0)?PASS:FAIL;      
  //   }, idx);
  // check_track( "The start vertex should be in the vertex table",                    [=](const g2t_track_st* t){
  //     std::string result = FAIL;
  //     int istart = t->start_vertex_p;
  //     const g2t_vertex_st* vertex = (istart>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istart-1) ) : 0;
  //     if ( vertex ) {
  // 	result = PASS;
  // 	std::cout << *vertex << std::endl;
  //     }
  //     return result;
  //   }, idx);  
  // check_track( "The stop vertex should be in the vertex table",                     [=](const g2t_track_st* t){
  //     std::string result = FAIL;
  //     int istart = t->stop_vertex_p;
  //     const g2t_vertex_st* vertex = (istart>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istart-1) ) : 0;
  //     if ( vertex ) {
  // 	result = PASS;
  // 	std::cout << *vertex << std::endl;
  //     }
  //     return result;
  //   }, idx);
  // check_track( "The id of the START vertex is nonzero",                             [=](const g2t_track_st* t){
  //     std::string result = FAIL;
  //     int istart = t->start_vertex_p;
  //     if ( istart > 0 ) result = PASS;      
  //     return result;
  //   }, idx);  
  // check_track( "The START vertex records a valid medium",                           [=](const g2t_track_st* t){
  //     auto result = FAIL;
  //     int istart = t->start_vertex_p;
  //     const g2t_vertex_st* vertex = (istart>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istart-1) ) : 0;
  //     if ( vertex ) {
  // 	if ( vertex->ge_medium>0 ) result = PASS;
  //     }
  //     return result;

  //   }, idx);
  // check_track( "The START vertex records a valid process",                          [=](const g2t_track_st* t){
  //     auto result = FAIL;
  //     int istart = t->start_vertex_p;
  //     const g2t_vertex_st* vertex = (istart>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istart-1) ) : 0;
  //     if ( vertex ) {
  // 	//	vertex_table->Print(istart-1,1);
  // 	result = Form(" (ge_proc=%i %s)", vertex->ge_proc, TMCProcessName[vertex->ge_proc] );
  // 	if ( vertex->ge_proc >= 0 && vertex->ge_proc < 44 && vertex->ge_proc!=kPStop ) result += PASS;
  // 	else                                                result += FAIL;
  //     }
  //     else 
  // 	result = Form("No start vertex on track") + result;

  //     return result;
  //   }, idx);
  // check_track( "The id of the START vertex is less than the id of the STOP vertex", [=](const g2t_track_st* t){
  //     std::string result = FAIL;
  //     int istart = t->start_vertex_p;
  //     int istop  = t->stop_vertex_p;
  //     if ( istart > 0 && istop > istart ) result = PASS;      
  //     if ( istart == istop ) result = TODO;
  //     return Form("(start=%i stop=%i)",istart,istop) + result;
  //   }, idx);
  // check_track( "The STOP vertex records a valid medium",                            [=](const g2t_track_st* t){
  //     auto result = FAIL;
  //     int istop = t->stop_vertex_p;
  //     const g2t_vertex_st* vertex = (istop>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istop-1) ) : 0;
  //     if ( vertex ) {
  // 	if ( vertex->ge_medium>0 ) result = PASS;
  //     }
  //     return result;

  //   }, idx);
  // check_track( "The STOP vertex records a valid process",                           [=](const g2t_track_st* t){
  //     auto result = FAIL;
  //     int istop = t->stop_vertex_p;
  //     const g2t_vertex_st* vertex = (istop>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istop-1) ) : 0;
  //     if ( vertex ) {
  // 	result = Form(" (ge_proc=%i %s)", vertex->ge_proc, TMCProcessName[vertex->ge_proc] );
  // 	if ( vertex->ge_proc > 0 && vertex->ge_proc < 44 ) result += PASS;
  // 	else                                               result += FAIL;
  //     }
  //     else 
  // 	result = Form("No stop vertex on track") + result;

  //     return result;
  //   }, idx);
  // check_track( "The STOP vertex parent track is this track",                        [=](const g2t_track_st* t){
  //     auto result = FAIL;
  //     int istop = t->stop_vertex_p;
  //     const g2t_vertex_st* vertex = (istop>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istop-1) ) : 0;
  //     if ( vertex ) {
  // 	int iparent = vertex->parent_p;
  // 	result = Form(" track id=%i vertex parent id=%i", t->id, vertex->parent_p );
  // 	if ( vertex->parent_p == t->id ) result += PASS;
  // 	else                             result += FAIL;
  //     }
  //     else 
  // 	result = Form("No stop vertex on track") + result;

  //     return result;
  //   }, idx);

  
  }

  // Print the track list
  //  track_table->Print(0, track_table->GetNRows());
  

}
//___________________________________________________________________
