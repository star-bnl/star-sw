/***************************************************************************
 *
 * $Id: simpleTestStandTest.C,v 1.1 2012/01/31 09:26:18 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Simple test macro to read in test stand data and make
 * plot of ADC values.
 *
 ***************************************************************************
 *
 * $Log: simpleTestStandTest.C,v $
 * Revision 1.1  2012/01/31 09:26:18  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.3  2011/09/24 02:14:10  sgliske
 * updated FGT cosmic QA
 *
 * Revision 1.2  2011/09/22 14:10:13  sgliske
 * minor update
 *
 * Revision 1.1  2011/09/21 20:26:47  sgliske
 * creation
 *
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtCosmicMaker;
class StFgtEvent;
class StFgtQaAdcVsChannel;

StChain *analysisChain          = 0;
StFgtCosmicMaker *cosmicMkr     = 0;
StFgtQaAdcVsChannel *adcVsChan1 = 0;
StFgtQaAdcVsChannel *adcVsChan2 = 0;
StFgtQaAdcVsChannel *adcVsChan3 = 0;

int simpleTestStandTest( const Char_t *filename = "testfile.sfs",
                         const Char_t *outputKey = "test",
                         Int_t nevents = 1999,
                         const Char_t *quadname1 = "010",
                         const Char_t *quadname2 = "011",
                         const Char_t *quadname3 = "013",
                         Bool_t pedSub = 1,
                         const Char_t *pedFile = "testfile.Ped.txt",
                         Int_t timeBin = 4,
                         Bool_t vsStrips = 1,
                         Int_t numDiscs = 3 ){

   LoadLibs();
   Int_t ierr = 0;

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   cout << "Constructing the cosmic maker" << endl;
   cosmicMkr = new StFgtCosmicMaker( "cosmicMaker", filename );
   cosmicMkr->setNumDiscs( numDiscs );

   cout << "Constructing the QA makers" << endl;

   adcVsChan1 = new StFgtQaAdcVsChannel( "fgtQaAdc_1", "cosmicMaker", 0, 0, quadname1 );

   adcVsChan1->setTimeBin( timeBin );
   adcVsChan1->setFilenameBase( outputKey );
   adcVsChan1->setToPlotVsStrip( vsStrips );
   adcVsChan1->setToSubtrPeds( pedSub );
   adcVsChan1->setPedReaderFile( pedFile );
   adcVsChan1->setPedThres( 3.0 );

   adcVsChan2 = new StFgtQaAdcVsChannel( "fgtQaAdc_2", "cosmicMaker", 1, 0, quadname2 );

   adcVsChan2->setTimeBin( timeBin );
   adcVsChan2->setFilenameBase( outputKey );
   adcVsChan2->setToPlotVsStrip( vsStrips );
   adcVsChan2->setToSubtrPeds( pedSub );
   adcVsChan2->setPedReaderFile( pedFile );
   adcVsChan2->setPedThres( 3.0 );

   adcVsChan3 = new StFgtQaAdcVsChannel( "fgtQaAdc_3", "cosmicMaker", 2, 0, quadname3 );

   adcVsChan3->setTimeBin( timeBin );
   adcVsChan3->setFilenameBase( outputKey );
   adcVsChan3->setToPlotVsStrip( vsStrips );
   adcVsChan3->setToSubtrPeds( pedSub );
   adcVsChan3->setPedReaderFile( pedFile );
   adcVsChan3->setPedThres( 3.0 );

   cout << "Initializing" << endl;
   ierr = analysisChain->Init();

   if( ierr ){
      cout << "Error initializing" << endl;
      return;
   };

   if( nevents < 0 )
      nevents = 1e100; // a big number

   for( int i=0; i<nevents && !ierr; ++i ){

      //cout << "event number " << i << endl;

      //cout << "clear" << endl;
      analysisChain->Clear();

      //cout << "make" << endl;
      ierr = analysisChain->Make();

   };

   //
   // Calls the ::Finish() method on all makers
   //
   cout << "finish" << endl;
   //analysisChain->Finish();   // bug in LOG_ prevents this
   adcVsChan1->Finish();
   adcVsChan2->Finish();
   adcVsChan3->Finish();

   cout << "all done" << endl;
   return;
};


// load the shared libraries
void LoadLibs() {
   // common shared libraries

  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StEvent");
  cout << "loaded StEvent library" << endl;

  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtRawMaker");
  gSystem->Load("RTS");
  gSystem->Load("StFgtPedMaker");
  gSystem->Load("StFgtQaMakers");
};
