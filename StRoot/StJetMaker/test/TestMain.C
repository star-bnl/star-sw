#include <TSystem.h>

#include <cppunit/BriefTestProgressListener.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>

void loadSharedObjects();

int
main( int argc, char* argv[] )
{
  loadSharedObjects();

  // Create the event manager and test controller
  CPPUNIT_NS::TestResult controller;

  // Add a listener that colllects test result
  CPPUNIT_NS::TestResultCollector result;
  controller.addListener( &result );        

  // Add a listener that print dots as test run.
  CPPUNIT_NS::BriefTestProgressListener progress;
  controller.addListener( &progress );      

  // Add the top suite to the test runner
  CPPUNIT_NS::TestRunner runner;
  runner.addTest( CPPUNIT_NS::TestFactoryRegistry::getRegistry().makeTest() );
  runner.run( controller );

  // Print test in a compiler compatible format.
  CPPUNIT_NS::CompilerOutputter outputter( &result, CPPUNIT_NS::stdCOut() );
  outputter.write(); 

  return result.wasSuccessful() ? 0 : 1;
}

void loadSharedObjects() {

   gSystem->Load("libTable.so");
   gSystem->Load("libPhysics.so");
   gSystem->Load("libStarRoot.so");
   gSystem->Load("libSt_base.so");
   gSystem->Load("libStChain.so");
   gSystem->Load("libtpc_Tables.so");
   gSystem->Load("libStarClassLibrary.so");
   gSystem->Load("libStTpcDb.so");
   gSystem->Load("libStDb_Tables.so");
   gSystem->Load("libStDetectorDbMaker.so");
   gSystem->Load("libStDbUtilities.so");
   gSystem->Load("libStMcEvent.so");
   gSystem->Load("libStMcEventMaker.so");
   gSystem->Load("libStDaqLib.so");
   gSystem->Load("libStEmcRawMaker.so");
   gSystem->Load("libStEvent.so");
   gSystem->Load("libStStrangeMuDstMaker.so");
   gSystem->Load("libgeometry_Tables.so");
   gSystem->Load("libStEmcUtil.so");
   gSystem->Load("libStMuDSTMaker.so");
   gSystem->Load("libStEmcADCtoEMaker.so");
   gSystem->Load("libStEpcMaker.so");
   gSystem->Load("libStEmcSimulatorMaker.so");
   gSystem->Load("libStDbLib.so");
   gSystem->Load("libStDbBroker.so");
   gSystem->Load("libSt_db_Maker.so");
   gSystem->Load("libStEEmcUtil.so");
   gSystem->Load("libStEEmcDbMaker.so");
   gSystem->Load("libStSpinDbMaker.so");
   gSystem->Load("libStEmcTriggerMaker.so");
   gSystem->Load("libStTriggerUtilities.so");
   gSystem->Load("libStMCAsymMaker.so");
   gSystem->Load("libStJetFinder.so");
   gSystem->Load("libStJets.so");
   gSystem->Load("libStJetSkimEvent.so");
   gSystem->Load("libStJetMaker.so");

   gSystem->Load(".libs/libStJetMakerTest.so");
}
