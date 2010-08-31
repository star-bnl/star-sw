// $Id: RunEemcGammaFilterBfc.C,v 1.1 2010/08/11 15:49:37 seluzhen Exp $
//
// Ilya Selyuzhenkov, Indiana U. CEEM
// 
// Original implementation by Pibero Djawotho, TAMU

class StBFChain;
StBFChain* chain = 0;

void RunEemcGammaFilterBfc
(
    int nEvents,
    const char *chainOpt,
    const char *fzdFile
)
{

    // Load the BFC macro and any necessary shared libraries
    gROOT->LoadMacro("bfc.C");
    gSystem->Load("StJetSkimEvent");

    // Create the BFC
    bfc(-1, chainOpt, fzdFile);

  // Manipulate the maker order in the BFC
  // Place TPC Maker after EMC makers
    chain->AddAfter("eess", chain->GetMaker("tpcChain") );
  // Place Filter Makers around TPC
    chain->AddAfter("eess", chain->GetMaker("gammaFilterMaker") );

    // Display the makers in the BFC
    StMaker::lsMakers(chain);

    // Initialize the BFC
    int iStat = chain->Init();

    if(iStat)
    {
        cout << "BFC initialization failed!" << endl;
        chain->Fatal(istat, "Init()");
    }

    // Run the BFC
    chain->EventLoop(nEvents);

}

// $Log: RunEemcGammaFilterBfc.C,v $
// Revision 1.1  2010/08/11 15:49:37  seluzhen
// Script to run EEMC gamma filter with bfc
//
