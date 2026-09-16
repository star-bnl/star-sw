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

void unit_test_track_data_model_ntrack() {

  gROOT->ProcessLine("initChain();");

  Accumulator_t edep; // Energy deposition
  Accumulator_t time; // Time per throw

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.,0.,0.);
  pm->SetSigma(0.0,0.,0.);

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Unit testing of tracks "     << std::endl;
  LOG_TEST << "=======================================================" << std::endl;

  timer.Start();
  
  int    _ntrack = 500;
  double _pt = 10.0;
  // for ( int i=1;i<_ntrack;i++ ) 
  //   add_particle( "mu+", 0.4251, 3.1415/4*(1.0+0.8*double(i)), _pt );
  // throw_particle( "mu+", 0.4251, 3.1415/4, _pt );
  throw_particle( _ntrack, "mu+", 0.1, 10.0, -0.95, 0.95, 0.0, TMath::TwoPi() );
  timer.Stop();
  
  auto* chain = StMaker::GetChain();
  vertex_table = dynamic_cast<TTable*>( chain->GetDataSet("g2t_vertex")  );
  track_table  = dynamic_cast<TTable*>( chain->GetDataSet("g2t_track")   );
  hit_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_emc_hit") );

  // Track table validation
  check_track_table( Form("The first %i tracks should be primary muons",_ntrack),   [=](g2t_track_st* begin_, g2t_track_st* end_) {
      std::string result = PASS;
      g2t_track_st* track = begin_;
      for ( int i=0;i<_ntrack;i++ ) {
	if ( track->eg_pid != -13 ) result = FAIL;
	track++;
      }
      return result;
    });
  check_track_table( Form("The first %i tracks should have 0.1 < pT < 10.0 GeV",_ntrack),     [=](g2t_track_st* begin_, g2t_track_st* end_) {
      std::string result = PASS;
      g2t_track_st* track = begin_;
      for ( int i=0;i<_ntrack;i++ ) {
	//if ( TMath::Abs(track->pt - _pt) > 0.001*_pt ) result = FAIL;
	if ( track->pt<0.1 || track->pt>10.0 ) result = FAIL;
	track++;
      }
      return result;
    });
  check_track_table( "80% of primary tracks should have >= 70 TPC hits", [=](g2t_track_st* begin_, g2t_track_st* end_) {
      std::string result = PASS;
      g2t_track_st* track = begin_;
      std::vector<int> nh;
      double fail=0;
      for ( int i=0;i<_ntrack;i++ ) {
	if ( track->n_tpc_hit<70 ) fail+=1;
	track++;
      }
      fail /= _ntrack;
      if ( fail > 0.80 ) result = Form("fail rate=%f%% ",fail*100.0) + FAIL;
      return result;
    });

  check_track_table( "Primary tracks should have < 78 hits",             [=](g2t_track_st* begin_, g2t_track_st* end_) {
      std::string result = PASS;
      g2t_track_st* track = begin_;
      std::vector<int> nh;
      double fail=0;
      std::string nf = "";
      for ( int i=0;i<_ntrack;i++ ) {
	if ( track->n_tpc_hit>=78 ) { fail+=1; nf += Form("%i ",track->n_tpc_hit); }
	track++;
      }
      fail /= _ntrack;
      if ( fail > 0.00 ) result = Form("fail rate=%f%% %s",fail*100.0,nf.c_str()) + FAIL;
      return result;
    });

 
  // TRACK VALIDATION
  if (0)  for ( int idx=0;idx<track_table->GetNRows();idx++ ) {

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
  check_track( "The track should have a stop vertex",                               [=](const g2t_track_st* t){
      return (t->stop_vertex_p>0)?PASS:FAIL;      
    }, idx);
  check_track( "The start vertex should be in the vertex table",                    [=](const g2t_track_st* t){
      std::string result = FAIL;
      int istart = t->start_vertex_p;
      const g2t_vertex_st* vertex = (istart>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istart-1) ) : 0;
      if ( vertex ) {
	result = PASS;
	std::cout << *vertex << std::endl;
      }
      return result;
    }, idx);  
  check_track( "The stop vertex should be in the vertex table",                     [=](const g2t_track_st* t){
      std::string result = FAIL;
      int istart = t->stop_vertex_p;
      const g2t_vertex_st* vertex = (istart>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istart-1) ) : 0;
      if ( vertex ) {
	result = PASS;
	std::cout << *vertex << std::endl;
      }
      return result;
    }, idx);
  check_track( "The id of the START vertex is nonzero",                             [=](const g2t_track_st* t){
      std::string result = FAIL;
      int istart = t->start_vertex_p;
      if ( istart > 0 ) result = PASS;      
      return result;
    }, idx);  
  check_track( "The START vertex records a valid medium",                           [=](const g2t_track_st* t){
      auto result = FAIL;
      int istart = t->start_vertex_p;
      const g2t_vertex_st* vertex = (istart>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istart-1) ) : 0;
      if ( vertex ) {
	if ( vertex->ge_medium>0 ) result = PASS;
      }
      return result;

    }, idx);
  check_track( "The START vertex records a valid process",                          [=](const g2t_track_st* t){
      auto result = FAIL;
      int istart = t->start_vertex_p;
      const g2t_vertex_st* vertex = (istart>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istart-1) ) : 0;
      if ( vertex ) {
	//	vertex_table->Print(istart-1,1);
	result = Form(" (ge_proc=%i %s)", vertex->ge_proc, TMCProcessName[vertex->ge_proc] );
	if ( vertex->ge_proc >= 0 && vertex->ge_proc < 44 && vertex->ge_proc!=kPStop ) result += PASS;
	else                                                result += FAIL;
      }
      else 
	result = Form("No start vertex on track") + result;

      return result;
    }, idx);
  check_track( "The id of the START vertex is less than the id of the STOP vertex", [=](const g2t_track_st* t){
      std::string result = FAIL;
      int istart = t->start_vertex_p;
      int istop  = t->stop_vertex_p;
      if ( istart > 0 && istop > istart ) result = PASS;      
      if ( istart == istop ) result = TODO;
      return Form("(start=%i stop=%i)",istart,istop) + result;
    }, idx);
  check_track( "The STOP vertex records a valid medium",                            [=](const g2t_track_st* t){
      auto result = FAIL;
      int istop = t->stop_vertex_p;
      const g2t_vertex_st* vertex = (istop>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istop-1) ) : 0;
      if ( vertex ) {
	if ( vertex->ge_medium>0 ) result = PASS;
      }
      return result;

    }, idx);
  check_track( "The STOP vertex records a valid process",                           [=](const g2t_track_st* t){
      auto result = FAIL;
      int istop = t->stop_vertex_p;
      const g2t_vertex_st* vertex = (istop>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istop-1) ) : 0;
      if ( vertex ) {
	result = Form(" (ge_proc=%i %s)", vertex->ge_proc, TMCProcessName[vertex->ge_proc] );
	if ( vertex->ge_proc > 0 && vertex->ge_proc < 44 ) result += PASS;
	else                                               result += FAIL;
      }
      else 
	result = Form("No stop vertex on track") + result;

      return result;
    }, idx);
  check_track( "The STOP vertex parent track is this track",                        [=](const g2t_track_st* t){
      auto result = FAIL;
      int istop = t->stop_vertex_p;
      const g2t_vertex_st* vertex = (istop>0) ? static_cast<const g2t_vertex_st*>( vertex_table->At(istop-1) ) : 0;
      if ( vertex ) {
	int iparent = vertex->parent_p;
	result = Form(" track id=%i vertex parent id=%i", t->id, vertex->parent_p );
	if ( vertex->parent_p == t->id ) result += PASS;
	else                             result += FAIL;
      }
      else 
	result = Form("No stop vertex on track") + result;

      return result;
    }, idx);
  
  }

  expectedId=0;
  idIsNotUnique.clear();
  // VERTEX VALIDATION
  if (0) for ( int idx=0;idx<vertex_table->GetNRows();idx++ ) {
  check_vertex( "A vertex must have been processed by geant",                      [=](const g2t_vertex_st* t){
      LOG_TEST << "-----------------------------------------------------------" << std::endl;
      assert(t);
      std::string result = PASS;
      return result;
    }, idx);
  check_vertex( "Is this vertex intermediate?",                                    [=](const g2t_vertex_st* v){
      LOG_TEST << "-----------------------------------------------------------" << std::endl;
      return (v->is_itrmd)? YES : NOPE;
    }, idx);
  check_vertex( "The vertex has a unique ID",                                      [=](const g2t_vertex_st* t){
      std::string result = Form("unique id = %i",t->id);
      if ( idIsNotUnique[ t->id ] ) result += FAIL;
      else                          result += PASS;
      idIsNotUnique[ t->id ]++;      
      return result;
    }, idx);
  check_vertex( "The vertex IDs are in numerical order",                           [=](const g2t_vertex_st* t){
      std::string result = Form("unique id = %i",t->id);
      expectedId++;
      if ( expectedId == t->id  ) result += PASS;
      else                          result += FAIL;
      return result;
    }, idx);
  check_vertex( "Primary vertex has no parents, all others do",                    [=](const g2t_vertex_st* v){
      std::string result;
      int np = v->n_parent;
      if (v->id==1) {
	result = Form("primary vertex: nparent=%i ",np);
	result += (np==0) ? PASS : FAIL;
      }
      else {
	result = Form("secondary vertex: nparent=%i ",np);
	result += (np==1) ? PASS : FAIL;	
      }      
      return result;
    }, idx);
  check_vertex( "Vertex parent track stops on this vertex if not intermediate",    [=](const g2t_vertex_st* v){
      std::string result;
      int np = v->n_parent;
      if (v->id==1) {
	result = Form("primary vertex: nparent=%i ",np);
	result += (np==0) ? PASS : FAIL;
      }
      else {
	int itrack=v->parent_p;
	const g2t_track_st* track = static_cast<const g2t_track_st*>(track_table->At(itrack-1));	
	int idvert=v->id;
	int idstop= (track)? track->stop_vertex_p : -1;
	int isint = v->is_itrmd;
	if ( isint==0 ) 
	  result += ( idvert==idstop )? PASS : FAIL;
	else
	  result += PASS;
      }      
      return result;
    }, idx);
  check_vertex( "Intermediate vertex must have daughter tracks",                   [=](const g2t_vertex_st* v){
      std::string result = NADA;
      if ( v->is_itrmd ) {
	result = Form("(n daughter=%i proc=%i %s) ", v->n_daughter, v->ge_proc, TMCProcessName[v->ge_proc] );
	if ( v->n_daughter>0 ) result += PASS;
	else                   { 
	  result += FAIL;       
	  //	  std::cout << *v << std::endl; 
	}
      }
      return result;
    }, idx);


  }

  // Hit accumulation
  if (0) for ( int idx=0;idx<hit_table->GetNRows();idx++ ) {
    auto hit = static_cast<const g2t_emc_hit_st*>( hit_table->At(idx) );
    if ( 0==hit ) continue;
    edep( hit->de * 1000 );
    //    std::cout << "hit dE=" << hit->de << " volume_id=" << hit->volume_id << std::endl;
    std::cout << *hit << std::endl;
  }

  // Print out energy deposition
  if (0) 
  {
    
    int    _count         = boost::accumulators::count(edep);
    double _sum           = boost::accumulators::sum(edep);
    double _mean          = boost::accumulators::mean(edep);
    double _median        = boost::accumulators::median(edep);
    double _min           = boost::accumulators::min(edep);
    double _max           = boost::accumulators::max(edep);
    double _error_of_mean = boost::accumulators::error_of<tag::mean>(edep);
    
    LOG_TEST << Form( "number of hits                   = %i", _count)          << std::endl;
    LOG_TEST << Form( "energy deposition: sum           = %f MeV", _sum  )          << std::endl;
    LOG_TEST << Form( "energy deposition: mean          = %f MeV", _mean )          << std::endl;
    LOG_TEST << Form( "energy deposition: median        = %f MeV", _median )        << std::endl;
    LOG_TEST << Form( "energy deposition: min           = %f MeV", _min  )          << std::endl;
    LOG_TEST << Form( "energy deposition: max           = %f MeV", _max  )          << std::endl;
    LOG_TEST << Form( "energy deposition: error of mean = %f MeV", _error_of_mean ) << std::endl;

  }

  // Print the track list
  if (0) 
    track_table->Print(0, _ntrack);
  else if (0) 
    track_table->Print(0, track_table->GetNRows() );
  

}
//___________________________________________________________________
