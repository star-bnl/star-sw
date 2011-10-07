/***************************************************************************
 *
 * $Id: sfsToTxt.C,v 1.1 2011/10/07 19:55:37 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Converts a sfs file to a txt file, similar to that
 * which was done by an older code with the name of 'HALF_HACK' in the
 * title, although this is for several, complete quadrants, not just
 * half of a single quadrant.
 *
 * Output has a 'block' for each event, blocks being seperated by 3
 * empty lines.  There are 32 columns: channel (0-128), timebin (0-7),
 * and 30 columns of ADC values, 10 for each quadrant, one for each
 * APV chip.
 *
 ***************************************************************************
 *
 * $Log: sfsToTxt.C,v $
 * Revision 1.1  2011/10/07 19:55:37  sgliske
 * creation
 *
 *
 **************************************************************************/

// forward declarations
class StChain;
class StFgtCosmicMaker;
class StFgtDaq2TxtMaker;

StChain *analysisChain      = 0;
StFgtCosmicMaker *cosmicMkr = 0;
StFgtDaq2TxtMaker *txtMkr   = 0;

int sfsToTxt( const Char_t *filenameIn = "testfile.sfs",
              const Char_t *filenameOut = "testfile.txt",
              Int_t quadIdx = 0,
              Int_t isCosmic = 1,
              Int_t nevents = -1 ){

   LoadLibs();
   Int_t ierr = 0;

   Int_t numDiscs = isCosmic ? 3 : 6;

   cerr << "Constructing the chain" << endl;
   analysisChain = new StChain("eemcAnalysisChain");

   cerr << "Constructing the makers" << endl;
   cosmicMkr = new StFgtCosmicMaker( "cosmicMaker", filenameIn );
   cosmicMkr->setNumDiscs( numDiscs );

   txtMkr = new StFgtDaq2TxtMaker( "daq2txt", "cosmicMaker", filenameOut, quadIdx );
   txtMkr->setIsCosmic( isCosmic );

   cerr << "Initializing" << endl;
   ierr = analysisChain->Init();

   if( ierr ){
      cerr << "Error initializing" << endl;
      return;
   };

   if( nevents < 0 )
      nevents = 1<<29; // a big number

   for( int i=0; i<nevents && !ierr; ++i ){
      analysisChain->Clear();
      ierr = analysisChain->Make();
   };

   //
   // Calls the ::Finish() method on all makers
   //
   cerr << "finish" << endl;
   analysisChain->Finish(); 

   cerr << "all done" << endl;
   return;
};


// load the shared libraries
void LoadLibs() {
   // common shared libraries

  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StEvent");
  gSystem->Load("StUtilities");
  cerr << "loaded StEvent library" << endl;

  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtRawMaker");
  gSystem->Load("RTS");
  gSystem->Load("StFgtDaq2TxtMaker");
};
