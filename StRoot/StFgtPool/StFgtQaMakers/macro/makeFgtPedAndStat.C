/***************************************************************************
 *
 * $Id: makeFgtPedAndStat.C,v 1.6 2012/11/12 23:22:56 rfatemi Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Compute peds and status from DAQ or cosmic data file.
 * Output QA plots if desired.
 *
 ***************************************************************************
 *
 * $Log: makeFgtPedAndStat.C,v $
 * Revision 1.6  2012/11/12 23:22:56  rfatemi
 * *** empty log message ***
 *
 * Revision 1.5  2012/02/29 15:38:39  sgliske
 * fixed bug in lynx query: ?r -> ?r=
 *
 * Revision 1.4  2012/02/06 04:17:19  balewski
 * *** empty log message ***
 *
 * Revision 1.3  2012/02/02 14:57:46  avossen
 * fixed chain
 *
 * Revision 1.2  2012/02/02 14:14:12  sgliske
 * Need to load RawDaqReader lib, not RawMaker
 * Also, fixed bug in using the runnumber for accessing online.star.bnl.gov
 * Also, now only check RunLog if run number is nonzero
 *
 * Revision 1.1  2012/01/31 08:56:43  sgliske
 * moved StFgtStatusMaker/macro/makeFgtPedAndStat.C to
 * StFgtPool/StFgtQaMakers/macro/
 *
 * Revision 1.7  2012/01/30 17:04:23  sgliske
 * remove automatic guess of cosmic or
 * not based on file extension
 *
 * Revision 1.6  2012/01/28 13:51:47  balewski
 * cleaned up printout
 *
 * Revision 1.5  2012/01/27 14:09:43  balewski
 * switch to new consts
 *
 * Revision 1.4  2012/01/25 12:26:45  sgliske
 * cosmetic change
 *
 * Revision 1.3  2012/01/25 12:25:24  sgliske
 * many updates, including outputting an info file
 *
 * Revision 1.2  2012/01/17 23:28:25  sgliske
 * update to use the DB
 *
 * Revision 1.1  2012/01/17 20:11:31  sgliske
 * creation
 *
 *
 **************************************************************************/

#include <TSystem.h>

// forward declarations
class StChain;
class StFgtDbMaker;
class StFgtRawDaqReader;
class StFgtRobustPedMaker;
class StFgtStatusMaker;
class StFgtPedStatQA;
class St_db_Maker;
class StDbConfigNode;
class StDbManager;

StChain           *analysisChain = 0;
St_db_Maker       *dbMkr         = 0;
StFgtDbMaker      *fgtDbMkr      = 0; 
StFgtRawDaqReader *daqRdr        = 0;
StFgtRobustPedMaker *pedMkr      = 0;
StFgtStatusMaker  *statMkr       = 0;
StFgtPedStatQA    *qaMkr         = 0;

int makeFgtPedAndStat(const Char_t *filenameIn ="inputfile",
		      const Char_t *filenameOutbase ="13025001",
		      Int_t runnumber = 13025001,
		      Int_t nevents = 2000,
		      Short_t timeBinMask = 0x04,
		      Short_t timeBin = 2,
		      Bool_t doOutputTxt = 1,
		      Bool_t doOutputPdf = 1,
		      Bool_t doOutputRoot = 1,
		      Bool_t isCosmic = 0,
		      Bool_t cutShortEvents = 1 ){
  
   LoadLibs();
   Int_t ierr = 0;

   if( isCosmic )
      cout << "Is Cosmic" << endl;
   else
      cout << "Is not cosmic" << endl;

   //
   // COMPUTE FILENAMES
   //

   std::string base(filenameOutbase), pedFile, statFile, txtFile, pdfFile, rootFile, logFile;
   pedFile = base + ".FGT-ped-DB.dat";
   statFile = base + ".FGT-stat-DB.dat";
   if( doOutputTxt )
      txtFile = base + ".FGT-ped-stat.txt";
   if( doOutputPdf )
      pdfFile = base + ".FGT-ped-stat.ps";
   if( doOutputRoot )
      rootFile = base + ".FGT-ped-stat.root";
   logFile = base + ".FGT-ped-stat-info.txt";


   //
   // START CONSTRUCTING THE CHAIN
   //

   cout << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   std::string fgtDbMkrName = "";

   if( !isCosmic ){
      // always cut short events if it is cosmic data
      cutShortEvents = 1;

      cout << "Loading St_db_Maker" << endl;
      gSystem->Load("libStDb_Tables.so");
      gSystem->Load("StDbLib.so");
      gSystem->Load("St_db_Maker");
      gSystem->Load("StDbBroker");

      TString dir0 = "MySQL:StarDb";
      TString dir1 = "$STAR/StarDb";
      St_db_Maker *dbMkr = new St_db_Maker( "dbMkr", dir0, dir1 );
      dbMkr->SetDateTime(20120125,80350);      // run 13025001 2012-01-25 08:03:34 GMT

      fgtDbMkr = new StFgtDbMaker( "fgtDbMkr" );
      fgtDbMkrName = fgtDbMkr->GetName();
   };

   //
   // NOW THE OTHER READERS AND MAKERS
   //

   cout << "Constructing the daq reader" << endl;
   daqRdr = new StFgtRawDaqReader( "daqReader", filenameIn, fgtDbMkrName.data() );
   daqRdr->setIsCosmic( isCosmic );
   daqRdr->cutShortEvents( cutShortEvents );

   cout << "Constructing the Ped Maker" << endl;
   pedMkr = new StFgtRobustPedMaker( "FgtPedMaker" );
   pedMkr->setToSaveToFile( pedFile.data() );
   pedMkr->setTimeBinMask( timeBinMask );
   pedMkr->setFgtDbMkrName( fgtDbMkrName );
   pedMkr->setNumBins( 100 );
   pedMkr->setMaxAdc( 1200 );
   pedMkr->setNumSmooth( 0 );

   cout << "Constructing the Status Maker" << endl;
   statMkr = new StFgtStatusMaker( "FgtStatusMaker", "FgtPedMaker" );
   statMkr->setToSaveToFile( statFile.data() );
   statMkr->setTimeBin( timeBin );
   statMkr->setMaxDeadPerApv(128);

   cout << "Constructing the QA Maker" << endl;
   qaMkr = new StFgtPedStatQA( "FgtPedStatQA", "FgtPedMaker", "FgtStatusMaker" );
   qaMkr->setSaveTxtFile( txtFile.data() );
   qaMkr->setSaveRootFile( rootFile.data() );
   qaMkr->setSavePdfFile( pdfFile.data() );
   qaMkr->setTimeBin( timeBin );

   // debug
   // analysisChain->ls(4);

   cout << "Initializing" << endl;
   ierr = analysisChain->Init();

   if( ierr ){
      cout << "Error initializing" << endl;
      return;
   };

   if( nevents < 0 )
      nevents = 1<<30; // a big number

   cout << "max nevents = " << nevents << endl;
   for( int i=0; i<nevents && !ierr; ++i ){

     if( i+1 % 100 == 1 )
         cout << "\ton event number " << i << endl;

      //cout << "clear" << endl;
      analysisChain->Clear();

      //cout << "make" << endl;
      ierr = analysisChain->Make();

   };

   fgtDbMkr->printFgtDumpCSV1("fgtMapDump.csv");
   //
   // Calls the ::Finish() method on all makers
   //
   cout << "finish" << endl;
   analysisChain->Finish();

   // Now write a status table

   std::ofstream fout( logFile.data() );
   if( fout ){
      fout << "Log file generated by 'makeFgtPedAndStat' for run number " << runnumber << endl << endl;
      fout << "CVS Ids: "<< endl;
      fout << "\t$Id: makeFgtPedAndStat.C,v 1.6 2012/11/12 23:22:56 rfatemi Exp $" << endl;
      if( fgtDbMkr )
	fout << "\t" << fgtDbMkr->GetCVS() << endl;
      fout << "\t" << daqRdr->GetCVS() << endl;
      fout << "\t" << pedMkr->GetCVS() << endl;
      fout << "\t" << statMkr->GetCVS() << endl;
      fout << "\t" << qaMkr->GetCVS() << endl << endl;

      TDatime d;
      fout << "This file was generated " << d.AsString() << endl << endl;
      fout << "The following options were used in this macro " << endl;
      fout << "\tfilenameIn = " << filenameIn << endl;
      fout << "\tfilenameOutbase = " << filenameOutbase << endl;
      fout << "\trunnumber = " << runnumber << endl;
      fout << "\tnevents = " << nevents << endl;
      fout << "\ttimeBinMask = " << timeBinMask << endl;
      fout << "\ttimeBin = " << timeBin << endl;
      fout << "\tdoOutputTxt = " << doOutputTxt << endl;
      fout << "\tdoOutputPdf = " << doOutputPdf << endl;
      fout << "\tdoOutputRoot = " << doOutputRoot << endl;
      fout << "\tisCosmic = " << isCosmic << endl;
      fout << "\tcutShortEvents = " << cutShortEvents << endl << endl;

      if( runnumber ){
         std::stringstream ss;
         fout << "Times given in the run log are " << endl;
         ss << "lynx -dump 'http://online.star.bnl.gov/RunLogRun12/index.php?r=" << runnumber << "' | grep GMT";
         FILE *f = gSystem->OpenPipe(ss.str().data(),"r");
         Char_t c;
         while((c=fgetc(f))!=EOF)
            fout << c;
      };
   };
   fout.close();

   // convert ps to pdf
   if( doOutputPdf ){
      cout << "converting ps to pdf" << endl;
      gSystem->Exec(( std::string("ps2pdf -dAutoRotatePages=/None ") + pdfFile ).data());
   };

   cerr << "\tall done" << endl;
   return;
};


// load the shared libraries
void LoadLibs() {
   // common shared libraries

  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StFgtUtil");
  gSystem->Load("StUtilities");
  gSystem->Load("StEvent");
  cout << "loaded StEvent library" << endl;

  gSystem->Load("RTS");
  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtDbMaker");
  gSystem->Load("StFgtRawDaqReader");
  gSystem->Load("StFgtPedMaker");
  gSystem->Load("StFgtStatusMaker");
  gSystem->Load("StFgtQaMakers");



};
