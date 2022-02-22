#include "tests/unit_tests.h"

#include "StGeant4Maker.h"
#include "TMath.h"
#include "TProfile.h"



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

std::vector<TProfile*> sampling_fraction_vs_pt;
std::vector<TProfile*> sampling_fraction_vs_eta;
std::vector<TProfile*> sampling_fraction_vs_phi;

#include <vector>
//___________________________________________________________________
const int neta = 40;
const int nphi = 120;
const double dphi = 3.0;
const double phi0 = 0.0; // not really...


//___________________________________________________________________

// Obtain a track pointer from the given hit
template<typename Hit> const g2t_track_st* get_track( Hit* hit ) {
  int index = hit->track_p - 1;
  if ( index < 0 || index > track_table->GetNRows() ) {
    return 0;
  } 
  else {
    return static_cast<const g2t_track_st*>( track_table -> At( index ) );
  }
}

//___________________________________________________________________
std::vector<double> mean_, median_, min_, max_, error_of_mean_, sum_;
std::vector<int> nhits_;
void trackLoop() {

  auto* chain = StMaker::GetChain();
  vertex_table = dynamic_cast<TTable*>( chain->GetDataSet("g2t_vertex")  );
  track_table  = dynamic_cast<TTable*>( chain->GetDataSet("g2t_track")   );
  hit_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_emc_hit") ) ;

  Accumulator_t edep; // Energy deposition

  std::map< int, double > edep_per_track;

  // Loop on all hits and accumulate all energy depositions
  int nh = hit_table->GetNRows();
  nhits_.push_back(nh);
  for ( int i=0;i<hit_table->GetNRows();i++ ) {

      auto hit = static_cast<const g2t_emc_hit_st*>( hit_table->At(i) );
      if ( 0==hit ) continue;     // skip null entries

      // accumulate energy deposition
      edep( hit->de );

       // accumulate sampling fraction
       auto* track = get_track( hit );
       if ( track ) {
       	double pt = track->pt;
       	double eta = track->eta;
       	if ( pt > 0.100 && pt < 10.0 ) {
	  LOG_INFO << "Accumulate track->id = " << track->id << " hit de = " << hit->de << std::endl;
       	  edep_per_track[ track->id ] += hit->de; // running sum
       	}	
       }

  }

   // Fill histograms
   for ( int i=0;i<track_table->GetNRows(); i++ ) {
     const g2t_track_st* track = static_cast<const g2t_track_st*>( track_table->At(i) );
     if ( track ) {
       double pt = track->pt;
       double eta = track->eta;
       // todo phi
       double E  = track->e + 1.0e-12; // prevent div by zero
       LOG_INFO << "itrack=" << i << " idtruth=" << track->id << "Fill pt=" << pt << " eta=" << track->eta << " E=" << track->e << " edep=" << edep_per_track[ track->id ] << endl;
       if ( pt > 0.100 && pt < 10.0 && eta > -0.95 && eta < 0.95 ) {
   	sampling_fraction_vs_pt.back()->Fill( pt, edep_per_track[ track->id ] / E );
   	sampling_fraction_vs_eta.back()->Fill( eta, edep_per_track[ track->id ] / E );
       }
     }
   }


  double _sum           = boost::accumulators::sum(edep);
  double _mean          = boost::accumulators::mean(edep);
  double _median        = boost::accumulators::median(edep);
  double _min           = boost::accumulators::min(edep);
  double _max           = boost::accumulators::max(edep);
  double _error_of_mean = boost::accumulators::error_of<tag::mean>(edep);
    
  sum_.push_back(_sum);
  mean_.push_back(_mean);
  median_.push_back(_median);
  min_.push_back(_min);
  max_.push_back(_max);
  error_of_mean_.push_back(_error_of_mean);

}
//___________________________________________________________________
void book_histograms() {

  const char* engines[] = { "geant3", "geant4" };
  assert(sampling_fraction_vs_pt.size() < sizeof(engines)/sizeof(const char*) );

  TString base  = Form( "%s_sampling_fraction_vs_",    engines[ sampling_fraction_vs_pt.size() ] );
  TString title = Form( "EMC Sampling fraction [%s]", engines[ sampling_fraction_vs_pt.size() ] );

  sampling_fraction_vs_pt.push_back ( new TProfile(base+"pt",  title+";p_{T}", 50,  0., 10.0, 0.0, 1.0 ) );
  sampling_fraction_vs_eta.push_back( new TProfile(base+"eta", title+";#eta",  40, -0.95, 0.95, 0.0, 1.0 ) );
  sampling_fraction_vs_phi.push_back( new TProfile(base+"phi", title+";#phi",  60, 0., TMath::TwoPi(), 0.0, 1.0 ) );

}
//___________________________________________________________________
void unit_test_single_engine_emc( const char* part = "mu+", int ntracks = 10 ) {

  gROOT->ProcessLine("initChain();");

  TString engineName;
  if ( hasRuntimeArg("application:engine=G3") ) engineName = "GEANT3 ";
  if ( hasRuntimeArg("application:engine=G4") ) engineName = "Geant 4 ";
  std::cout << "Engine name = " << engineName.Data() << endl;

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.,0.,0.);
  pm->SetSigma(0.0,0.,0.);

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Single-engine testing of EMC" << std::endl;
  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << Form("GEANT3 response to %i 1 GeV photons %s",ntracks,part) << std::endl;
  LOG_TEST << "=======================================================" << std::endl;


  auto* gm = dynamic_cast<StGeant4Maker*>( StMaker::GetChain()->GetMaker("geant4star") );

  throw_particle(ntracks, part, 0.09995, 10.00005, -0.95, 0.95, 0., TMath::TwoPi() );

  vertex_table = dynamic_cast<TTable*>( chain->GetDataSet("g2t_vertex")  );
  track_table  = dynamic_cast<TTable*>( chain->GetDataSet("g2t_track")   );
  hit_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_emc_hit") ) ;

  auto all_primary_tracks_have_hits = [=](g2t_track_st* begin_, g2t_track_st* end_) {
   
    int count = 0;
    int hits  = 0;
    for ( const auto* track=begin_; track < end_; track++ ) {

      if ( track->n_emc_hit ) hits++;
      if ( ++count == ntracks ) break;

    }
    return (hits == count) ? PASS : FAIL;

  };

  check_track_table( "All primary tracks have hits", all_primary_tracks_have_hits );
      
  return;
  
}



