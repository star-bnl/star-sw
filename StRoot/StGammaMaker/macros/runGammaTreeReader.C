class StGammaEvent;
class StGammaTreeReader;
class StGammaCandidate;

#define __TEST__

TFile  *file = 0;
TTree  *tree = 0;
TChain *chain = 0;

StGammaEvent      *event  = 0;
StGammaTreeReader *reader = 0;

// ------------------------------------------------------------------------------
//
// 1. cvs co offline/StGammaMaker
// 2. mkdir StRoot
// 3. cp -R offline/StGammaMaker StRoot/
// 4. cons
// 5. ln -s StRoot/StGammaMaker/macros/runGammaTreeReader.C .
// 6. root4star -q -b runGammaTreeReader.C
// 7. meh
//
// ------------------------------------------------------------------------------

void runGammaTreeReader( int nevents=-1,
	 const Char_t *fname = "/auto/pdsfdv34/starspin/jwebb/2008/02-29-2008-gammas-with-preshower/7864/7136073/",
	 const Char_t *oname = "testgammas.root" )
{

  gROOT->LoadMacro("StRoot/StGammaMaker/macros/loadGammaLibs.C");
  loadGammaLibs();


  reader = new StGammaTreeReader("gammas","GammaEvent");        // reads in the event branch
  reader -> Init();     // should be (but isn't) part of a chain

  // Files exist at PDSF
  chainFiles( "/auto/pdsfdv34/starspin/jwebb/2008/02-29-2008-gammas-with-preshower/7864/7136073/");
  chainFiles( "/auto/pdsfdv34/starspin/jwebb/2008/02-29-2008-gammas-with-preshower/7864/7136080/");

  /* few example events from above chains

    [0] RUN: 7136073 EVENT: 2721 KEY: 71360730000002721 INDEX: 0
    [1] RUN: 7136073 EVENT: 3548 KEY: 71360730000003548 INDEX: 1
    [2] RUN: 7136073 EVENT: 3918 KEY: 71360730000003918 INDEX: 2
    [3] RUN: 7136073 EVENT: 6433 KEY: 71360730000006433 INDEX: 3
    [4] RUN: 7136073 EVENT: 7587 KEY: 71360730000007587 INDEX: 4
    [5] RUN: 7136073 EVENT: 12476 KEY: 71360730000012476 INDEX: 5
    [6] RUN: 7136073 EVENT: 14475 KEY: 71360730000014475 INDEX: 6
    [7] RUN: 7136073 EVENT: 16295 KEY: 71360730000016295 INDEX: 7
    [8] RUN: 7136073 EVENT: 25487 KEY: 71360730000025487 INDEX: 8
    [9] RUN: 7136073 EVENT: 25769 KEY: 71360730000025769 INDEX: 9

    [0] RUN: 7136080 EVENT: 81123 KEY: 71360800000081123 INDEX: 0
    [1] RUN: 7136080 EVENT: 81130 KEY: 71360800000081130 INDEX: 1
    [2] RUN: 7136080 EVENT: 81133 KEY: 71360800000081133 INDEX: 2
    [3] RUN: 7136080 EVENT: 81141 KEY: 71360800000081141 INDEX: 3
    [4] RUN: 7136080 EVENT: 81147 KEY: 71360800000081147 INDEX: 4
    [5] RUN: 7136080 EVENT: 81155 KEY: 71360800000081155 INDEX: 5
    [6] RUN: 7136080 EVENT: 81166 KEY: 71360800000081166 INDEX: 6
    [7] RUN: 7136080 EVENT: 81170 KEY: 71360800000081170 INDEX: 7
    [8] RUN: 7136080 EVENT: 81195 KEY: 71360800000081195 INDEX: 8
    [9] RUN: 7136080 EVENT: 81198 KEY: 71360800000081198 INDEX: 9

   */

  Int_t tryRuns[]   = { 7136073, 7136073, 7136073, 7136080, 7136080, 7136080 };
  Int_t tryEvents[] = {    2721,    3548,   25769,   81133,   81155,   81198 };

  for ( Int_t ii=0;ii< sizeof(tryRuns)/sizeof(Int_t);ii++ ) 
    {

      if ( !reader->getEvent( tryRuns[ii], tryEvents[ii] ) )
	{
	  std::cout << Form("run %i event %i does not seem to exist",tryRuns[ii], tryEvents[ii]) << std::endl;
	}
      else
	{
	  std::cout << Form("run %i event %i exists and is ready to be used ",tryRuns[ii], tryEvents[ii]) << std::endl;
	  StGammaEvent *event = reader->event();
	  //	  event->Print();
	}
      
    }

}



// ----------------------------------------------------------------------------
//
// path: directory containing a group of root files
// filt: extention of files which are to be chained
//
void chainFiles(const Char_t *path, const Char_t *filt=".root")
{
  std::cout << std::endl;
  std::cout << "*************************************************************************" << std::endl;
  std::cout << std::endl;
  std::cout << "Chaining: " << path << std::endl;
  std::cout << std::endl;
  std::cout << "*************************************************************************" << std::endl;
  std::cout << std::endl;
  TString pwd = gSystem->pwd();
  TSystemDirectory dir("dir",path);
  TIter next( dir.GetListOfFiles() );
  Int_t nn=0;
  TObject *file = 0;
  while ( file = (TObject*)next() )
    {
      TString name=path;name+=file->GetName();
      if ( name.Contains(filt) ) {
	reader->chainFile( name );
	nn++;
      }

    }
  gSystem->cd(pwd.Data());
}
// ----------------------------------------------------------------------------
