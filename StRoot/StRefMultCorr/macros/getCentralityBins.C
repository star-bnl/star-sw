//----------------------------------------------------------------------------------------------------
// Example macro how to use StRefMultCorr
// $Id: getCentralityBins.C,v 1.12 2012/05/19 00:51:14 hmasui Exp $
// $Log: getCentralityBins.C,v $
// Revision 1.12  2012/05/19 00:51:14  hmasui
// Update the usage for refmult3
//
// Revision 1.11  2012/05/17 10:31:22  hmasui
// Add comment for library load
//
// Revision 1.10  2012/05/17 09:59:24  hmasui
// Fix the refmult2 argument in initEvent function
//
// Revision 1.9  2012/05/14 00:43:32  hmasui
// Added invalid run number test
//
// Revision 1.8  2012/05/09 22:25:52  hmasui
// Update comments (thanks to Bill Llope for suggestions)
//
// Revision 1.7  2012/05/08 05:38:57  hmasui
// Update the description of the usage for refmult2
//
// Revision 1.6  2012/05/08 03:23:11  hmasui
// Update the usage for refmult2
//
// Revision 1.5  2012/05/08 03:20:00  hmasui
// Move parameters to Centrality_def_refmult.txt
//
// Revision 1.4  2012/04/26 23:40:09  hmasui
// Added how to use CentralityMaker, StRefMultCorr::isBadRun() function to remove outliers
//
// Revision 1.3  2011/11/08 19:12:24  hmasui
// Update usage based on the latest update for 200 GeV (luminosity correction)
//
// Revision 1.2  2011/08/12 20:28:45  hmasui
// Change interface according to the update of StRefMultCorr class
//
//----------------------------------------------------------------------------------------------------

//____________________________________________________________________________________________________
// Example macro for StRefMutlCorr class implementation
//   * Before accessing centrality bins via StRefMutlCorr, you must call 'init(const Int_t RunId)'
//     to specify which parameters you want to use
//     - If you stick to the one specific run, you only need to call this function once in your code.
//     - If you read data from multiple different runs, you need to call this function
//       whenever you switch to the run number
//
//   * In the standard STAR analysis maker, the best (and only) place to call 'init(...)' function 
//     is 'Make(...)'
//
//   * Comment for luminosity (zdc coincidence rate) correction
//     - Luminosity correction is only valid for 200 GeV
//     - The default argument is 0 for zdc coincidence rate in initEvent() function, see header StRefMultCorr.h,
//       so that you can still use previous initEvent() function like
//         void StRefMultCorr::initEvent(refmult, vz) ;
//       without specifying zdc coincidence rate for lower beam energies
//     - (Very important) You should use BBC coincidence rate for refmult2
//
//   * You can now use the interface "CentralityMaker" class to access the StRefMultCorr
//     see the usage below.
//   * The refmult2 & refmult3 correction classes are ready. You can access the correction by
//      - StRefMultCorr* refmult2CorrUtil = CentralityMaker::instance()->getRefMult2Corr() ;
//      (note the '2' in the function, replace 2 to 3 for refmult3)
//      - You can use exactly the same functions used in StRefMultCorr (see below)
void getCentralityBins()
{
  //----------------------------------------------------------------------------------------------------
  // NOTE:
  //   - The usage below is under the assumption that users add StRefMultCorr class in their StRoot/ and compiled it by cons. 
  //   - You can definitely use the correction classes in your local pc with ROOT
  //     because the codes under StRefMultCorr don't depend on any STAR libraries.
  //----------------------------------------------------------------------------------------------------

  // In the compiled code, don't forget to add
  // #include "StRoot/StRefMultCorr/StRefMultCorr.h"
  // #include "StRoot/StRefMultCorr/CentralityMaker.h"

  // Load StRefMultCorr library
  // NOTE: Add this line in your 'macro', not in the source code
  gSystem->Load("StRefMultCorr");

  // For refmult
  StRefMultCorr* refmultCorrUtil  = CentralityMaker::instance()->getRefMultCorr() ;

  // For refmult2
  StRefMultCorr* refmult2CorrUtil = CentralityMaker::instance()->getRefMult2Corr() ;

  // For refmult3
  StRefMultCorr* refmult3CorrUtil = CentralityMaker::instance()->getRefMult3Corr() ;

  // For grefmult
  StRefMultCorr* grefmultCorrUtil = CentralityMaker::instance()->getgRefMultCorr() ;

  // You can also access the StRefMultCorr by direct instantiation
  // StRefMultCorr* refmultCorrUtil  = new StRefMultCorr("refmult");
  // StRefMultCorr* refmult2CorrUtil = new StRefMultCorr("refmult2");

  // You need to specify the run number you are going to process
  refmultCorrUtil->init(11078000);
  refmult2CorrUtil->init(11078000);
  refmult3CorrUtil->init(11078000);
  grefmultCorrUtil->init(15075008);

  //----------------------------------------------------------------------------------------------------
  // *** Optional functions (not necessary to call)
  //    void StRefMultCorr::print(const Optiont_t option="");
  //    Int_t StRefMultCorr::getBeginRun(const Double_t energy, const Int_t year) ;
  //    Int_t StRefMultCorr::getEndRun(const Double_t energy, const Int_t year) ;

  // Print all parameters
//  refmultCorrUtil->print();
//  refmult2CorrUtil->print();
//  refmult3CorrUtil->print();
//  grefmultCorrUtil->print();

  // scale factor test
  // *****************************************************************************************************************************************
  // Actually you don't need it for Run14 Vpd30 and VpdNoVtx, but for easy,just copy these lines to youe macros, and in the StRefmult we already comment these factor in StRefmult Line 482
  grefmultCorrUtil->setVzForWeight(6, -30.0, 30.0);
  grefmultCorrUtil->readScaleForWeight("StRoot/StRefMultCorr/macros/weight_grefmult_vpd30_vpd5_Run14.txt");
  for(Int_t i=0;i<6;i++){
    cout << i << " " << grefmultCorrUtil->get(i, 0) << endl;
  }

  return;

  // Obtain begin and end run number from energy and year
  cout << "Run " << refmultCorrUtil->getBeginRun(200.0, 2010) << " - " << refmultCorrUtil->getEndRun(200.0, 2010) << endl;

  // You can check the 'bad run' based on the event-wise QA for refmult centrality
  // by using StRefMultCorr::isBadRun() function
  if ( refmultCorrUtil->isBadRun(12177061) ) {
    cout << "Run 12177061 is bad" << endl;
  }

  if ( refmult2CorrUtil->isBadRun(12177061) ) {
    cout << "Run 12177061 is bad" << endl;
  }

  if ( refmult3CorrUtil->isBadRun(12177061) ) {
    cout << "Run 12177061 is bad" << endl;
  }

  if ( grefmultCorrUtil->isBadRun(15106001) ) {
    cout << "Run 15106001 is bad" << endl;
  }
  //----------------------------------------------------------------------------------------------------

  // Dummy refmult and primary z-vertex to test the functions
  const UShort_t refmult  = 100 ;
  const UShort_t refmult2 = 100 ;
  const UShort_t refmult3 = 100 ;
  const UShort_t grefmult = 100 ;
  const Double_t vz      = 20.0 ; // cm
  const Double_t zdcCoincidenceRate = 20000 ; // Hz
  const Double_t bbcCoincidenceRate = 20000 ; // Hz

  // The following functions should be called inside the event loop (event-by-event)

  // ******* IMPORTANT ***********
  // Call initEvent(const UShort_t RefMult, const Double_t z) function
  // event-by-event at the beginning before using any other functions
  refmultCorrUtil->initEvent(refmult, vz, zdcCoincidenceRate) ;

  // This also works for 7.7 - 62.4 GeV (see comments above)
//  refmultCorrUtil->initEvent(refmult, vz);

  // ******* VERY IMPORTANT ***********
  // USE BBC COINCIDENCE RATE RATHER THAN ZDC COINCIDENCE RATE FOR REFMULT2
  refmult2CorrUtil->initEvent(refmult2, vz, bbcCoincidenceRate) ;

  // This also works for 7.7 - 62.4 GeV (see comments above)
//  refmult2CorrUtil->initEvent(refmult2, vz);

  refmult3CorrUtil->initEvent(refmult3, vz, zdcCoincidenceRate) ;

  // This also works for 7.7 - 62.4 GeV (see comments above)
//  refmult3CorrUtil->initEvent(refmult3, vz);
  //----------------------------------------------------------------------------------------------------

  grefmultCorrUtil->initEvent(grefmult, vz, zdcCoincidenceRate) ;
  // Get centrality bins
  //   - You can use exactly the same functions to obtain centrality, reweighting
  //     and corrected multiplicity for refmult2
  //
  //   see StRefMultCorr.h for the definition of centrality bins
  const Int_t cent16 = refmultCorrUtil->getCentralityBin16() ;
  const Int_t cent9  = refmultCorrUtil->getCentralityBin9() ;

  // Centrality from refmult2
  const Int_t cent16_refmult2 = refmult2CorrUtil->getCentralityBin16() ;
  const Int_t cent9_refmult2  = refmult2CorrUtil->getCentralityBin9() ;

  // Centrality from refmult3
  const Int_t cent16_refmult3 = refmult3CorrUtil->getCentralityBin16() ;
  const Int_t cent9_refmult3  = refmult3CorrUtil->getCentralityBin9() ;

  // Centrality from grefmult
  const Int_t cent16_grefmult = grefmultCorrUtil->getCentralityBin16() ;
  const Int_t cent9_grefmult  = grefmultCorrUtil->getCentralityBin9() ;

  // Re-weighting corrections for peripheral bins
  const Double_t reweight          = refmultCorrUtil->getWeight() ;
  const Double_t reweight_refmult2 = refmult2CorrUtil->getWeight() ;
  const Double_t reweight_refmult3 = refmult3CorrUtil->getWeight() ;
  const Double_t reweight_grefmult = grefmultCorrUtil->getWeight() ;

  //----------------------------------------------------------------------------------------------------
  // Corrected refmult (with z-vertex dependent correction and luminositiy correction)
  //  NOTE: type should be double or float, not integer
  const Double_t refmultCor  = refmultCorrUtil->getRefMultCorr() ;
  const Double_t refmult2Cor = refmult2CorrUtil->getRefMultCorr() ;
  const Double_t refmult3Cor = refmult3CorrUtil->getRefMultCorr() ;
  const Double_t grefmultCor = grefmultCorrUtil->getRefMultCorr() ;

  //----------------------------------------------------------------------------------------------------
  // Invalid run number test
  const Int_t runId = 12154037 ;
  cout << "Invalid run number test: " << runId << endl;
  cout << "The program should be stopped with the error messages from isIndexOk function" << endl;
  refmult2CorrUtil->init(runId);
  refmult2CorrUtil->getWeight();
  // Program should stop here
  cout << "Problem if you see this message. Contact hmasui@lbl.gov" << endl;
}

