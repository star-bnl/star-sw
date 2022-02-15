#include "tests/unit_tests.h"
#include <assert.h>

// TODO: Implement test of prompt-hits

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
void throw_muon_in_tpc_sector( int sectorid, int charge = 1 ) {
  assert(sectorid>0 && sectorid <= 24);
  const double sectors[] = { 
    60.0, 30.0, 0.0, 330.0, 300.0, 270., 240.0, 210.0, 180.0, 150.0, 120.0, 90.0,
    120.0, 150.0, 180.0, 210.0, 240.0, 270.0, 300.0, 330.0, 0.0, 30.0, 60.0, 90.0
  };
  double eta = (sectorid<=12) ? 0.5 : -0.5;
  _eta = eta;
  double phid = sectors[sectorid-1];
  _phid = phid; 
  throw_muon( eta, phid, 500.0, charge ); // energetic

  auto* chain = StMaker::GetChain();
  vertex_table = dynamic_cast<TTable*>( chain->GetDataSet("g2t_vertex")  );
  track_table  = dynamic_cast<TTable*>( chain->GetDataSet("g2t_track")   );
  hit_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_tpc_hit") ) ;

  auto* gm    = dynamic_cast<StGeant4Maker*>( StMaker::GetChain()->GetMaker("geant4star") );
  // auto* stack = gm->stack();
  // stack->StackDump();

  assert(vertex_table);

}
//______________________________________________________________________
void unit_test_tpc_hits( int longtest=0 ) {

  gROOT->ProcessLine("initChain();");

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.,0.,0.);
  pm->SetSigma(0.0,0.,0.);

  // Setup post stepping debug output
  auto* gm    = dynamic_cast<StGeant4Maker*>( StMaker::GetChain()->GetMaker("geant4star") );
  auto* stack = gm->stack();
  
  if (0) 
  gm->AddUserPostSteppingAction( [stack]() {
      auto* nav = gGeoManager->GetCurrentNavigator();
      auto* mc = TVirtualMC::GetMC();
      const double *xyz = nav->GetCurrentPoint();
      std::string path = nav->GetPath();
      LOG_INFO << "Post step _________________________________________________________________" << endm;      
      LOG_INFO << "step number    = " << mc->StepNumber() << endm;
      LOG_INFO << "n secondaries  = " << mc->NSecondaries() << endm; 
      LOG_INFO << "track is alive = " << mc->IsTrackAlive() << endm;
      mc->Print();
      LOG_INFO << "x=" << xyz[0] << " y= " << xyz[1] << " z=" << xyz[2] << " " << path.c_str() << endm;
      int current = stack->GetCurrentTrackNumber();
      LOG_INFO << "Current track = " << current << endm;
      LOG_INFO << "Persistent track @ " << stack->GetCurrentPersistentTrack() << endm;
      stack->StackDump(current); 
    });


  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Unit testing of tracks and TPC hits on single muons"     << std::endl;
  LOG_TEST << "=======================================================" << std::endl;

  Accumulator_t edep; // Energy deposition
  Accumulator_t step; // Step size
  Accumulator_t time; // Time per throw
  
  for ( int sector=1; sector<=24; sector++ ) {

    timer.Start();
    throw_muon_in_tpc_sector( sector );
    time( timer.CpuTime() );
    timer.Reset();


    LOG_TEST << "======================================================================================" << std::endl;
    LOG_TEST << GIVEN << "A muon thrown down the center of TPC sector " << sector << std::endl;

    check_track( "A muon must have been processed by geant",       [=](const g2t_track_st* t){
	std::string result = Form("sector=%i ", sector);
	return result+PASS; 
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
    check_track( "Expect 76 hits in the dev2021 geometry",         [=](const g2t_track_st* t){
      int n = t->n_tpc_hit;
      std::string  result = FAIL;
      if ( n==76 ) result = PASS;
      result = Form(" n=%i ",n) + result;
      return result;
    });



    for ( int i=0;i<hit_table->GetNRows();i++ ) {

      auto hit = static_cast<const g2t_tpc_hit_st*>( hit_table->At(i) );
      if ( 0==hit ) continue;     // skip null entries
      if ( 1!=hit->track_p ) continue; // not interested in secondaries

      LOG_TEST << "------------------------------------------------------------------" << std::endl;
      LOG_TEST << GIVEN << "A hit on that track" << std::endl;

      edep( TMath::Abs(hit->de) * 1E6 ); // GeV MeV keV
      step( hit->ds );

      // check_tpc_hit( "Print the hit...", hit, [=](const g2t_tpc_hit_st* h) {
      // 	  LOG_TEST << "id=" << h->id 
      // 		   << " track_p=" << h->track_p 
      // 		   << " volume_id=" << h->volume_id 
      // 		   << " x="  << h->x[0] 
      // 		   << " y="  << h->x[1] 
      // 		   << " z="  << h->x[2] 	    
      // 		   << std::endl;
      // 	  return PASS;
      // 	});
      check_tpc_hit( "The hit should have a nonzero volume_id",hit,[=](const g2t_tpc_hit_st* h) {
	  std::string result = FAIL;
	  if ( h->volume_id > 0 ) result = PASS;
	  result = Form("id=%i vid=%i de=%f ds=%f ",h->id,h->volume_id,h->de,h->ds) + result;
	  return result;
	});
      check_tpc_hit( "The hit should have an energy deposit > 0",hit,[=](const g2t_tpc_hit_st* h) {
	  std::string result = NADA; // undetermined
	  double ds = h->ds;
	  if       ( ds > 1.0 && h->de > 0 ) result = PASS;
	  else if  ( ds > 1.0 && h->de <=0 ) result = FAIL;
	  return result;
	});
      check_tpc_hit( "The hit should have a path length > 0",hit,[=](const g2t_tpc_hit_st* h) {
       	  std::string result = FAIL;
       	  if ( h->ds > 0 ) result = PASS;
       	  return result;
       	});
      check_tpc_hit( "The hit should have a nonzero momentum",hit,[=](const g2t_tpc_hit_st* h) {
       	  std::string result = FAIL;
       	  if ( h->p[0] != 0 ) result = PASS;
       	  if ( h->p[1] != 0 ) result = PASS;
       	  if ( h->p[2] != 0 ) result = PASS;
       	  return result;
       	});
      check_tpc_hit( "The hit should have a nonzero log10(gamma)",hit,[=](const g2t_tpc_hit_st* h) {
       	  std::string result = FAIL;
       	  if ( h->lgam != 0 ) result = PASS;
	  result = Form(" lgam=%f (needs to be filled) ",h->lgam ) + result;
       	  return result;
       	});
      check_tpc_hit( "The hit should have a length > 0",hit,[=](const g2t_tpc_hit_st* h) {
       	  std::string result = FAIL;
       	  if ( h->length > 0 ) result = PASS;
       	  return result;
       	});
      check_tpc_hit( "The hit should have adc, pad and timebucket set to zero",hit,[=](const g2t_tpc_hit_st* h) {
       	  std::string result = PASS;
	  if ( h->adc > 0 ) result = FAIL;
	  if ( h->pad > 0 ) result = FAIL;
	  if ( h->timebucket > 0 ) result = FAIL;
	  return result;
	});
      check_tpc_hit( "Track's momentum at hit should be < initial value",hit,    [=](const g2t_tpc_hit_st* h){
	  std::string result = FAIL;
	  double px = h->p[0];
	  double py = h->p[1];
	  double pz = h->p[2];
	  double p2 = px*px + py*py + pz*pz;
	  if ( p2 < _pmom*_pmom ) result = PASS;
	  return result;
	});
      check_tpc_hit( "Hit position should be w/in the fiducial volume of the sector",hit,[=](const g2t_tpc_hit_st* h){
	  double x = h->x[0];
	  double y = h->x[1];
	  double z = h->x[2];
	  TVector3 hitpos( x, y, z );
	  int rotator = (sector>12)? 12-sector : sector-12;
	  double rotatord = (double) rotator;
	  hitpos.RotateZ( rotatord * TMath::Pi() / 6.0 );
	  bool isInSectorPhi = TMath::Abs(hitpos.Phi() - TMath::Pi()/2.0) < TMath::Pi()/12.0;
	  bool isInSectorR   = TMath::Abs(hitpos.Perp() -124.0) < 76.0;
	  bool isInSector    = isInSectorPhi && isInSectorR;	  
	  std::string result = Form("(%f %f %f / in phi=%i r=%i",x,y,z,isInSectorPhi,isInSectorR);
	  result += ( isInSector ) ? PASS : FAIL;	  
	  return result;
	});
      check_tpc_hit( "The padrow should be 1 <= pad <= 72",hit,[=](const g2t_tpc_hit_st* h) {
	  std::string result = PASS;
	  int padrow = h->volume_id % 100;
	  if ( padrow<1 || padrow > 72 ) result=FAIL;
	  return result;
	});
      check_tpc_hit( "The sector should be 1 <= sector <= 24",hit,[=](const g2t_tpc_hit_st* h) {
	  std::string result = PASS;
	  int sector = ( h->volume_id / 100 ) % 1000;
	  if ( sector<1 || sector>24 ) result=FAIL;
	  return result;
	});
      check_tpc_hit( "The detector state is in (0,1,2)",hit,[=](const g2t_tpc_hit_st* h) {
	  std::string result = PASS;
	  int det = h->volume_id/100000;
	  if ( det<0||det>2 ) result = FAIL;
	  return result;
	});
      check_tpc_hit( Form("The decoded sector number should be %i",sector),hit,[=](const g2t_tpc_hit_st* h) {
	std::string result = FAIL;
	int _sector = ( h->volume_id / 100 ) % 1000;
	if ( sector == _sector ) result = PASS;
	result = Form(" sector=%i",_sector ) + result;
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
    hit_table    = dynamic_cast<TTable*>( chain->GetDataSet("g2t_tpc_hit") ) ;

    // Accumulate
    for ( int i=0;i<hit_table->GetNRows();i++ ) {

      auto hit = static_cast<const g2t_tpc_hit_st*>( hit_table->At(i) );
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
