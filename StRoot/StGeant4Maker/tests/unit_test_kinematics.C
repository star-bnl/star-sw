#include "tests/unit_tests.h"

#include <TTable.h>

template<typename Add, typename Test>
std::string check_kine( std::string message, Add addfunc, Test testfunc ) {
  std::string af = addfunc();
  std::string tf = testfunc();
  std::string result = "\u001b[37m [" + message + "] " + "(" + af +") " + tf;
  gMessMgr->Info() << result << endm;
  return result;
};

void unit_test_kinematics() {

  gROOT->ProcessLine("initChain();");

  auto* pm = dynamic_cast<StarPrimaryMaker*>( StMaker::GetChain()->GetMaker("PrimaryMaker") );
  pm->SetVertex(0.,0.,0.);
  pm->SetSigma(0.0,0.,0.);

  LOG_TEST << "=======================================================" << std::endl;
  LOG_TEST << "Unit testing of paticle gun" << std::endl;
  LOG_TEST << "=======================================================" << std::endl;

  auto* chain = StMaker::GetChain();

  auto no_op = [=]() -> std::string { return "-same-"; }  ;
  auto add_deuteron = [=]() {
    std::string result = "Deuteron";
    const double ptmn =  0.099999;
    const double ptmx = 10.100001;
    const double etamn= -1.; 
    const double etamx= +4.;
    auto* _kine = dynamic_cast<StarKinematics*>( chain->GetMaker("StarKine") );
    pm->Clear();
    _kine->SetAttr("rapidity",1);
    _kine->Kine(1,"D",ptmn,ptmx,etamn,etamx);   
    pm->Make();
    pm->event()->Print();
    return result;
  };
  
  check_kine( "After adding a deuteron to the generator, a single particle appears in the event", add_deuteron, [=](){
      std::string result = FAIL;
      int np = _kine->GetNumberOfParticles();
      if ( 1 == np ) {
	result = PASS;
      }
      result = Form(" /number of particles = %i/ ", np) + result;
      return result;
    });
  check_kine( "The particle should be accessible in the event structure at element 1",            no_op,        [=](){
      auto& event = *pm->event();
      auto  part  = event[1];
      if ( part ) 
	return PASS;
      else
	return FAIL;
    });
  check_kine( "The particle should have PDG id id=1000010020",                                    no_op,        [=](){
      auto& event = *pm->event();
      auto  part  = event[1];
      std::string result = FAIL;
      if ( 1000010020 == part->GetId() )
	result = PASS;
      
      result = Form("/id = %i/ ",part->GetId()) + result;
      return result;      
    });
  check_kine( "The particle should have mass > 0",                                                no_op,        [=](){
      std::string result = FAIL;
      auto& event = *pm->event();
      auto  part  = event[1];
      auto  mass  = part->GetMass();
      if ( mass > 0 ) result = PASS;
      result = Form("/mass = %f GeV/ ",mass) + result;
      return result;
    });
  check_kine( "The dot product of the particl's 4-momentum w/ itself is (mc^2)^2",                no_op,        [=](){
      std::string result = FAIL;
      auto& event = *pm->event();
      auto  part  = event[1];
      auto  mass  = part->GetMass();
      auto  momentum = part->momentum();
      auto  inner = momentum.Dot(momentum);
      //momentum.Print();
      bool good = TMath::Abs( sqrt(inner) - mass ) < 0.005*mass;
      if ( good ) 
	result = PASS;
      result = Form("/sqrt(p4 dot p4) = %f expect %f/", sqrt(inner), mass) + result;
      return result;
    });


  auto add_helium3 = [=]() {
    std::string result = "He3";
    const double ptmn =  0.099999;
    const double ptmx = 10.100001;
    const double etamn= -1.; 
    const double etamx= +4.;
    auto* _kine = dynamic_cast<StarKinematics*>( chain->GetMaker("StarKine") );
    pm->Clear();
    _kine->SetAttr("rapidity",1);
    _kine->Kine(1,"He3",ptmn,ptmx,etamn,etamx);   
    pm->Make();
    pm->event()->Print();
    return result;
  };

#if 0
  auto add_alpha = [=]() {
    std::string result = "alpha";
    const double ptmn =  0.099999;
    const double ptmx = 10.100001;
    const double etamn= -1.; 
    const double etamx= +4.;
    auto* _kine = dynamic_cast<StarKinematics*>( chain->GetMaker("StarKine") );
    pm->Clear();
    _kine->SetAttr("rapidity",1);
    _kine->Kine(1,"He4",ptmn,ptmx,etamn,etamx);   
    pm->Make();
    pm->event()->Print();
    return result;
  };
#endif

  check_kine( "After adding He3 to the generator, a single particle appears in the event", add_helium3, [=](){
      std::string result = FAIL;
      int np = _kine->GetNumberOfParticles();
      if ( 1 == np ) {
	result = PASS;
      }
      result = Form(" /number of particles = %i/ ", np) + result;
      return result;
    });
  check_kine( "The particle should be accessible in the event structure at element 1",            no_op,        [=](){
      auto& event = *pm->event();
      auto  part  = event[1];
      if ( part ) 
	return PASS;
      else
	return FAIL;
    });
  check_kine( "The particle should have PDG id id=1000020020",                                    no_op,        [=](){
      auto& event = *pm->event();
      auto  part  = event[1];
      std::string result = FAIL;
      if ( 1000020020 == part->GetId() )
	result = PASS;
      
      result = Form("/id = %i/ ",part->GetId()) + result;
      return result;      
    });
  check_kine( "The particle should have mass > 0",                                                no_op,        [=](){
      std::string result = FAIL;
      auto& event = *pm->event();
      auto  part  = event[1];
      auto  mass  = part->GetMass();
      if ( mass > 0 ) result = PASS;
      result = Form("/mass = %f GeV/ ",mass) + result;
      return result;
    });
  check_kine( "The dot product of the particl's 4-momentum w/ itself is (mc^2)^2",                no_op,        [=](){
      std::string result = FAIL;
      auto& event = *pm->event();
      auto  part  = event[1];
      auto  mass  = part->GetMass();
      auto  momentum = part->momentum();
      auto  inner = momentum.Dot(momentum);
      //momentum.Print();
      bool good = TMath::Abs( sqrt(inner) - mass ) < 0.005*mass;
      if ( good ) 
	result = PASS;
      result = Form("/sqrt(p4 dot p4) = %f expect %f/", sqrt(inner), mass) + result;
      return result;
    });




}
//___________________________________________________________________
