#include "tests/unit_tests.h"

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

#include <TTable.h>

StGeant4Maker* _g4mk = 0;

using Accumulator_t = accumulator_set<double, 
stats< tag::count, 
       tag::sum,
       tag::mean, 
       tag::median(with_p_square_quantile),
       tag::max, 
       tag::min, 
       tag::error_of<tag::mean>
>>;


//const double GeV = 1;
//const double MeV = 1E3;
//const double keV = 1E6;
//const double  eV = 1E9;
const std::map<double,std::string> scale2string = {
  {GeV,"GeV"},
  {MeV,"MeV"},
  {keV,"keV"},
  {eV,"eV"}
};

// template<typename T> double energy_deposit(const T* h) { return h->de; } 
// template<typename T> double path_length   (const T* h) { return h->ds; } 
// template<typename T> double deds          (const T* h) { 
//   double de = h->de;
//   double ds = h->ds;
//   double deds = -999;
//   if ( ds > 0 ) deds = de / ds;
//   return deds;
// } 

//___________________________________________________________________
double _eta  = 0; 
double _phid = 0;
//___________________________________________________________________
const int ntracks = 100;
//___________________________________________________________________

struct tpcTag  { 
  static double energy_deposit(const g2t_tpc_hit_st* h){ return h->de; }
  static double path_length(const g2t_tpc_hit_st* h){ return h->ds; }
  static double de_ds(const g2t_tpc_hit_st* h ) {
    double de = h->de;
    double ds = h->ds;
    double deds = -999;
    if ( ds > 0 ) deds = de / ds;
    return deds;
  }
} tpc; // TPC hits
struct fstmTag {
  static double energy_deposit(const g2t_fts_hit_st* h){ return h->de; }
  static double path_length(const g2t_fts_hit_st* h){ return h->ds; }
  static double de_ds(const g2t_fts_hit_st* h ) {
    double de = h->de;
    double ds = h->ds;
    double deds = -999;
    if ( ds > 0 ) deds = de / ds;
    return deds;
  }
} fstm;
struct stgcTag {
  static double energy_deposit(const g2t_fts_hit_st* h){ return h->de; }
  static double path_length(const g2t_fts_hit_st* h){ return h->ds; }
  static double de_ds(const g2t_fts_hit_st* h ) {
    double de = h->de;
    double ds = h->ds;
    double deds = -999;
    if ( ds > 0 ) deds = de / ds;
    return deds;
  }
} stgc;

struct bemcTag { 
  static double energy_deposit(const g2t_emc_hit_st* h){ return h->de; }
} bemc; // BEMC hits
struct bsmdTag { 
  static double energy_deposit(const g2t_emc_hit_st* h){ return h->de; }
} bsmd; // BSMD hits
struct eemcTag { 
  static double energy_deposit(const g2t_emc_hit_st* h){ return h->de; }
} eemc; // EEMC hits
struct esmdTag { 
  static double energy_deposit(const g2t_emc_hit_st* h){ return h->de; }
} esmd; // ESMD hits

template<typename T> struct HitTraits {
  /* empty, this will not go well */
};
template<> struct HitTraits<tpcTag>   {
  const std::string tableName = "g2t_tpc_hit";
  typedef g2t_tpc_hit_st hit_type;
};
template<> struct HitTraits<bemcTag>  {
  const std::string tableName = "g2t_emc_hit";
  typedef g2t_emc_hit_st hit_type;
};
template<> struct HitTraits<bsmdTag>  {
  const std::string tableName = "g2t_smd_hit";
  typedef g2t_emc_hit_st hit_type;
};
template<> struct HitTraits<eemcTag>  {
  const std::string tableName = "g2t_eem_hit";
  typedef g2t_emc_hit_st hit_type;
};
template<> struct HitTraits<esmdTag>  {
  const std::string tableName = "g2t_esm_hit";
  typedef g2t_emc_hit_st hit_type;
};
template<> struct HitTraits<fstmTag>  {
  const std::string tableName = "g2t_fsi_hit";
  typedef g2t_fts_hit_st hit_type;
};
template<> struct HitTraits<stgcTag>  {
  const std::string tableName = "g2t_stg_hit";
  typedef g2t_fts_hit_st hit_type;
};

template<typename Tag> 
void check_hit_distribution( std::string message, Tag, 
			     std::function<double(const typename HitTraits<Tag>::hit_type *)> stat, 
			     std::function<std::string(const Accumulator_t& acc)>             eval,
			     double scale=1.0,
			     std::function<bool(const typename HitTraits<Tag>::hit_type *)> filt = [](const typename HitTraits<Tag>::hit_type *){ return true; }
			     ) {
  Accumulator_t acc;
  HitTraits<Tag> traits;
  TTable* table = static_cast<TTable*>( _g4mk->GetDataSet(traits.tableName.c_str()) );
  for ( int irow=0; irow<table->GetNRows();irow++ ) {
    auto* hit = static_cast<const typename HitTraits<Tag>::hit_type *>( table->At(irow) );
    if ( filt(hit) ) {
      acc( stat(hit)*scale ); 
    }
  }
  LOG_TEST << message << " " << eval(acc) << std::endl;

}

void unit_test_muons() {

  gROOT->ProcessLine("initChain();");

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.,0.,0.);
  pm->SetSigma(0.0,0.,0.);

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Unit testing of muons" << std::endl;
  LOG_TEST << "=======================================================" << std::endl;

  auto* chain = StMaker::GetChain();
  auto* _kine = dynamic_cast<StarKinematics*> ( chain->GetMaker("StarKine") );
  _g4mk = dynamic_cast<StGeant4Maker*>  ( chain->GetMaker("geant4star") );

  double minPt =  0.100;
  double maxPt = 10.000;
  int    nbinPt = 100;

  // Throw 1k gammas at BEMC, EEMC
  //  _kine->Kine(3000,"e+,e-",4.9999,5.0001,-0.95,+2.00);
  _kine->Kine(10,"mu+,mu-",4.9999,5.0001,-0.95,+2.00);

  chain->Clear();
  chain->Make();

  check_hit_distribution( "BEMC sampling fraction (e+,e-)", bemc, bemc.energy_deposit, [=](const Accumulator_t& acc){
      std::string result = PASS; result += "\n";
      double _count         = boost::accumulators::count(acc);
      double _sum           = boost::accumulators::sum(acc);      
      double _sf = _sum / 1000.0 / 5.0;
      result += Form("Sampling fraction @ 5 GeV = %f\n",_sf);
      return result;      
    });

  check_hit_distribution( "EEMC sampling fraction (e+,e-)", eemc, eemc.energy_deposit, [=](const Accumulator_t& acc){
      std::string result = PASS; result += "\n";
      double _count         = boost::accumulators::count(acc);
      double _sum           = boost::accumulators::sum(acc);      
      double _sf = _sum / 1000.0 / 5.0;
      result += Form("Sampling fraction @ 5 GeV = %f\n",_sf);
      return result;      
    });

  return;
  
  _kine->Kine(ntracks,"mu+,mu-",0.100,10.00,-2.0,5.0);

  chain->Clear();
  chain->Make();

  check_hit_distribution( "TPC energy deposit [keV] (muons)", tpc, tpc.energy_deposit, [=](const Accumulator_t& acc){
      std::string result = "TPC energy deposition " + PASS; result += "\n";
      double _mean          = boost::accumulators::mean(acc);
      double _median        = boost::accumulators::median(acc);
      double _min           = boost::accumulators::min( acc );
      double _max           = boost::accumulators::max( acc );
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(acc);

      result+= Form( "energy deposition: mean          = %f\n", _mean );
      result+= Form( "energy deposition: median        = %f\n", _median );
      result+= Form( "energy deposition: min           = %f\n", _min  );
      result+= Form( "energy deposition: max           = %f\n", _max  );
      result+= Form( "energy deposition: error of mean = %f\n", _error_of_mean );

      return result;      
    },keV);
  check_hit_distribution( "TPC path length (muons)", tpc, tpc.path_length   , [=](const Accumulator_t& acc){
      std::string result = "TPC path length " + PASS; result += "\n";
      double _mean          = boost::accumulators::mean(acc);
      double _median        = boost::accumulators::mean(acc);
      double _min           = boost::accumulators::min( acc );
      double _max           = boost::accumulators::max( acc );
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(acc);

      result+= Form( "path length:       mean          = %f\n", _mean );
      result+= Form( "path length:       median        = %f\n", _median );
      result+= Form( "path length:       min           = %f\n", _min  );
      result+= Form( "path length:       max           = %f\n", _max  );
      result+= Form( "path length:       error of mean = %f\n", _error_of_mean );

      return result;      
    }    );
  check_hit_distribution( "TPC dE/ds [keV/cm] (muons)", tpc, tpc.de_ds, [=](const Accumulator_t& acc){
      std::string result = "TPC dE/ds " + PASS; result += "\n";
      double _mean          = boost::accumulators::mean(acc);
      double _median        = boost::accumulators::mean(acc);
      double _min           = boost::accumulators::min( acc );
      double _max           = boost::accumulators::max( acc );
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(acc);

      result+= Form( "de/ds:             mean          = %f\n", _mean );
      result+= Form( "de/ds:             median        = %f\n", _median );
      result+= Form( "de/ds:             min           = %f\n", _min  );
      result+= Form( "de/ds:             max           = %f\n", _max  );
      result+= Form( "de/ds:             error of mean = %f\n", _error_of_mean );

      return result;      
    }, keV, [](const g2t_tpc_hit_st* h){ return h->ds>0; });

  check_hit_distribution( "BEMC energy deposit [MeV] (muons)", bemc, bemc.energy_deposit, [=](const Accumulator_t& acc){
      std::string result = "BEMC energy deposition " + PASS; result += "\n";
      double _mean          = boost::accumulators::mean(acc);
      double _median        = boost::accumulators::median(acc);
      double _min           = boost::accumulators::min( acc );
      double _max           = boost::accumulators::max( acc );
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(acc);

      result+= Form( "energy deposition: mean          = %f\n", _mean );
      result+= Form( "energy deposition: median        = %f\n", _median );
      result+= Form( "energy deposition: min           = %f\n", _min  );
      result+= Form( "energy deposition: max           = %f\n", _max  );
      result+= Form( "energy deposition: error of mean = %f\n", _error_of_mean );

      return result;      
    },MeV);
  check_hit_distribution( "EEMC energy deposit [MeV] (muons)", eemc, eemc.energy_deposit, [=](const Accumulator_t& acc){
      std::string result = "EEMC energy deposition " + PASS; result += "\n";
      double _mean          = boost::accumulators::mean(acc);
      double _median        = boost::accumulators::median(acc);
      double _min           = boost::accumulators::min( acc );
      double _max           = boost::accumulators::max( acc );
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(acc);

      result+= Form( "energy deposition: mean          = %f\n", _mean );
      result+= Form( "energy deposition: median        = %f\n", _median );
      result+= Form( "energy deposition: min           = %f\n", _min  );
      result+= Form( "energy deposition: max           = %f\n", _max  );
      result+= Form( "energy deposition: error of mean = %f\n", _error_of_mean );

      return result;      
    },MeV);


  check_hit_distribution( "FSTM energy deposit [keV] (muons)", fstm, fstm.energy_deposit, [=](const Accumulator_t& acc){
      std::string result = "FSTM energy deposition " + PASS; result += "\n";
      double _mean          = boost::accumulators::mean(acc);
      double _median        = boost::accumulators::median(acc);
      double _min           = boost::accumulators::min( acc );
      double _max           = boost::accumulators::max( acc );
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(acc);

      result+= Form( "energy deposition: mean          = %f\n", _mean );
      result+= Form( "energy deposition: median        = %f\n", _median );
      result+= Form( "energy deposition: min           = %f\n", _min  );
      result+= Form( "energy deposition: max           = %f\n", _max  );
      result+= Form( "energy deposition: error of mean = %f\n", _error_of_mean );

      return result;      
    },keV);
  check_hit_distribution( "FSTM path length (muons)", fstm, fstm.path_length   , [=](const Accumulator_t& acc){
      std::string result = "FSTM path length " + PASS; result += "\n";
      double _mean          = boost::accumulators::mean(acc);
      double _median        = boost::accumulators::mean(acc);
      double _min           = boost::accumulators::min( acc );
      double _max           = boost::accumulators::max( acc );
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(acc);

      result+= Form( "path length:       mean          = %f\n", _mean );
      result+= Form( "path length:       median        = %f\n", _median );
      result+= Form( "path length:       min           = %f\n", _min  );
      result+= Form( "path length:       max           = %f\n", _max  );
      result+= Form( "path length:       error of mean = %f\n", _error_of_mean );

      return result;      
    }    );
  check_hit_distribution( "FSTM dE/ds [keV/cm] (muons)", fstm, fstm.de_ds, [=](const Accumulator_t& acc){
      std::string result = "FSTM dE/ds " + PASS; result += "\n";
      double _mean          = boost::accumulators::mean(acc);
      double _median        = boost::accumulators::mean(acc);
      double _min           = boost::accumulators::min( acc );
      double _max           = boost::accumulators::max( acc );
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(acc);

      result+= Form( "de/ds:             mean          = %f\n", _mean );
      result+= Form( "de/ds:             median        = %f\n", _median );
      result+= Form( "de/ds:             min           = %f\n", _min  );
      result+= Form( "de/ds:             max           = %f\n", _max  );
      result+= Form( "de/ds:             error of mean = %f\n", _error_of_mean );

      return result;      
    }, keV, [](const g2t_fts_hit_st* h){ return h->ds>0; });

  check_hit_distribution( "STGC energy deposit [keV] (muons)", stgc, stgc.energy_deposit, [=](const Accumulator_t& acc){
      std::string result = "STGC energy deposition " + PASS; result += "\n";
      double _mean          = boost::accumulators::mean(acc);
      double _median        = boost::accumulators::median(acc);
      double _min           = boost::accumulators::min( acc );
      double _max           = boost::accumulators::max( acc );
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(acc);

      result+= Form( "energy deposition: mean          = %f\n", _mean );
      result+= Form( "energy deposition: median        = %f\n", _median );
      result+= Form( "energy deposition: min           = %f\n", _min  );
      result+= Form( "energy deposition: max           = %f\n", _max  );
      result+= Form( "energy deposition: error of mean = %f\n", _error_of_mean );

      return result;      
    },keV);
  check_hit_distribution( "STGC path length (muons)", stgc, stgc.path_length   , [=](const Accumulator_t& acc){
      std::string result = "STGC path length " + PASS; result += "\n";
      double _mean          = boost::accumulators::mean(acc);
      double _median        = boost::accumulators::mean(acc);
      double _min           = boost::accumulators::min( acc );
      double _max           = boost::accumulators::max( acc );
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(acc);

      result+= Form( "path length:       mean          = %f\n", _mean );
      result+= Form( "path length:       median        = %f\n", _median );
      result+= Form( "path length:       min           = %f\n", _min  );
      result+= Form( "path length:       max           = %f\n", _max  );
      result+= Form( "path length:       error of mean = %f\n", _error_of_mean );

      return result;      
    }    );
  check_hit_distribution( "STGC dE/ds [keV/cm] (muons)", stgc, stgc.de_ds, [=](const Accumulator_t& acc){
      std::string result = "STGC dE/ds " + PASS; result += "\n";
      double _mean          = boost::accumulators::mean(acc);
      double _median        = boost::accumulators::mean(acc);
      double _min           = boost::accumulators::min( acc );
      double _max           = boost::accumulators::max( acc );
      double _error_of_mean = boost::accumulators::error_of<tag::mean>(acc);

      result+= Form( "de/ds:             mean          = %f\n", _mean );
      result+= Form( "de/ds:             median        = %f\n", _median );
      result+= Form( "de/ds:             min           = %f\n", _min  );
      result+= Form( "de/ds:             max           = %f\n", _max  );
      result+= Form( "de/ds:             error of mean = %f\n", _error_of_mean );

      return result;      
    }, keV, [](const g2t_fts_hit_st* h){ return h->ds>0; });

  chain->Clear();

}
//___________________________________________________________________
