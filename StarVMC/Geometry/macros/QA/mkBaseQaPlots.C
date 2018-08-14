//
// Wrapper macro for MuMc.C.  Handles setup of the environment and compilation
// of the code.  Runs and directs output to specified location.
//


// If set, default directories to search for MuDsts and to output images
const char *_dir = 0;
const char *_www = 0;


void mkBaseQaPlots( const char *Dir = _dir,  // input directory to search for MuDst files
		    const char *WWW = _www,  // output directory for plots
		    int nev=1000000000,      // number of events to process (max)
		    float _vertzMn = -200.0, // min vertex
		    float _vertzMx = +200.0, // max vertex
		    float _etaMn   = -10.0,  // min eta
		    float _etaMx   = +10.0,  // max eta
		    float _ptMn    =  0.0 )  // min pT
{

  if ( TString(gProgName)!="root4star" )
    {
      cout << endl;
      cout << "**********************************************************" << endl;
      cout << "**********************************************************" << endl;
      cout << "Usage:" << endl
	   << "  root4star -q -b mkBaseQaPlots.C" << endl;
      cout << "**********************************************************" << endl;
      cout << "**********************************************************" << endl;
      gROOT->ProcessLine(".q");
    }

  TString dir = Dir;
  TString www = WWW;    if ( WWW == 0 ) { www = "./"; }
  if ( Dir==0 ) {

    cout << "**********************************************************" << endl;
    cout << "**********************************************************" << endl;
    cout << "Usage:" << endl
	 << "  root4star -q -b mkBaseQaPlots.C(inputdir,outputdir)" << endl;
    cout << "**********************************************************" << endl;
    cout << "**********************************************************" << endl;
    gROOT->ProcessLine(".q");
  }

  // Create target directory for output files
  if ( WWW ) gSystem -> MakeDirectory(WWW);

  // Load shared libs
  gROOT->ProcessLine(".x rootlogon.C");
  gROOT->ProcessLine(".x lMuDst.C");
  gROOT->ProcessLine(".L MuMc.C+");

  // Set vertex and eta cuts as above
  vertzMn = _vertzMn;
  vertzMx = _vertzMx;
  etaMn   = _etaMn;
  etaMx   = _etaMx;
  ptMn    = _ptMn;
  

  // CD to the target directory and run (note: must provide absolute path to MuMc...)
  if ( WWW ) {
    gSystem -> MakeDirectory(WWW);
    gSystem -> ChangeDirectory(WWW);
  }

  // Run base QA codes
  MuMc(nev,dir);

}
