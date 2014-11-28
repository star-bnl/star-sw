// Michael Betancourt
// Massachussets Institute of Technology
// 
// Original implementation by Pibero Djawotho, TAMU

class StBFChain;
StBFChain* chain = 0;

void gammaFilterBfc
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
    chain->AddAfter("StMcEventMaker", chain->GetMaker("TpcRS") );
    chain->AddAfter("StMcEventMaker", chain->GetMaker("gammaFilterMaker") );
    chain->AddAfter("StMcEventMaker", chain->GetMaker("emcY2") );

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
