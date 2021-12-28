#include "tests/unit_tests.h"

#include "StGeant4Maker.h"
#include "TMath.h"

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

#include <vector>
//___________________________________________________________________
const int neta = 40;
const int nphi = 120;
const double dphi = 3.0;
const double phi0 = 0.0; // not really...


//___________________________________________________________________
std::vector<double> mean_, median_, min_, max_, error_of_mean_, sum_;
std::vector<int> nhits_;
void eventSums() {

  auto* chain = StMaker::GetChain();
  vertex_table = dynamic_cast<TTable*>( chain->GetDataSet("g2t_vertex")  );
  track_table  = dynamic_cast<TTable*>( chain->GetDataSet("g2t_track")   );
  hit_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_emc_hit") ) ;

  Accumulator_t edep; // Energy deposition

  // Loop on all hits and accumulate all energy depositions
  int nh = hit_table->GetNRows();
  nhits_.push_back(nh);
  for ( int i=0;i<hit_table->GetNRows();i++ ) {

      auto hit = static_cast<const g2t_emc_hit_st*>( hit_table->At(i) );
      if ( 0==hit ) continue;     // skip null entries

      // accumulate energy deposition
      edep( hit->de );

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
void unit_test_multi_engine_emc( int nevents = 5, const char* part = "pi+" ) {

  gROOT->ProcessLine("initChain();");

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.,0.,0.);
  pm->SetSigma(0.0,0.,0.);

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Multi-engine testing of EMC" << std::endl;
  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << Form("GEANT3 response to %i 1 GeV photons %s",nevents,part) << std::endl;
  LOG_TEST << "=======================================================" << std::endl;


  auto* gm = dynamic_cast<StGeant4Maker*>( StMaker::GetChain()->GetMaker("geant4star") );

  gm->SetEngineForModule( "CALB", 0 ); 
  throw_particle(nevents, part, 0.99995, 1.00005, -0.95, 0.95, 0., TMath::TwoPi() );
  eventSums();

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Multi-engine testing of EMC" << std::endl;
  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << Form("Geant 4 response to %i 1 GeV %s",nevents,part) << std::endl;
  LOG_TEST << "=======================================================" << std::endl;

  gm->SetEngineForModule( "CALB", 1 );
  throw_particle(nevents, part, 0.99995, 1.00005, -0.95, 0.95, 0., TMath::TwoPi() );
  eventSums();

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Multi-engine testing of EMC" << std::endl;
  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << Form("GEANT3 vs Geant 4 response to %i 1 GeV %s",nevents,part) << std::endl;
  LOG_TEST << "=======================================================" << std::endl;

  LOG_TEST << Form( "energy deposition: sum           = %f %f keV", sum_[0], sum_[1] )          << std::endl;
  LOG_TEST << Form( "energy deposition: mean          = %f %f keV", mean_[0], mean_[1] )          << std::endl;
  LOG_TEST << Form( "energy deposition: median        = %f %f keV", median_[0], median_[1] )        << std::endl;
  LOG_TEST << Form( "energy deposition: min           = %f %f keV", min_[0], min_[1]  )          << std::endl;
  LOG_TEST << Form( "energy deposition: max           = %f %f keV", max_[0], max_[1]  )          << std::endl;
  LOG_TEST << Form( "energy deposition: error of mean = %f %f keV", error_of_mean_[0], error_of_mean_[1] ) << std::endl;
  LOG_TEST << Form( "number of hits:                    %i %i    ", nhits_[0], nhits_[1]         ) << std::endl;

}



