
class StMuDstMaker;
class HistMaker;

StMuDstMaker* maker;
HistMaker* myHistMaker;

void RunHistMaker()
{
    if (gClassTable->GetID("TTable") < 0) gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StMagF");
    gSystem->Load("StUtilities");  // new addition 22jul99
    gSystem->Load("StTreeMaker");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StEvent");
    gSystem->Load("StEventUtilities"); 
    gSystem->Load("StMcEvent"); 
    gSystem->Load("StMcEventMaker"); 
    gSystem->Load("StAssociationMaker");
    gSystem->Load("StMcAnalysisMaker");
    gSystem->Load("StStrangeMuDstMaker");
    gSystem->Load("StMuDSTMaker");
    gSystem->Load("UpsilonAna");

    cout << " loading done " << endl;
  
    StMuDebug::setLevel(0);  // switch of some debug output

    int iret=0;
    cout <<" --- Create StMuDstMaker --- "<<endl;
    maker = new StMuDstMaker(0,0,"data/minbias/ReversedFullField/runs/",
			     "","MuDst.root",10);   // set up maker in read mode

    cout <<" --- Set Cuts ---"<<endl;
    //Setup cuts
    AnaCuts cuts;
    cuts.electronMomentumCut = 3.0;    // momentum (not pt) (GeV)
    cuts.pseudoRapidityCutOff = 1.5;
    cuts.minNumberOfFitPoints = 20;
    cuts.minNumberOfPoints = 25;
    cuts.lowerInvariantMassCut = 7.; //GeV
    cuts.upperInvariantMassCut = 13.; //GeV
    cuts.verbose=true;

    cout <<" --- Create HistMaker ---"<<endl;
    myHistMaker = new HistMaker();
    myHistMaker.setCuts(cuts);

    cout <<" --- Loop on events ---"<<endl;
    int nEvents=1000000;
    for (int iev=0; iev<nEvents; ++iev) {
	iret = maker->Make();  // read an event
	myHistMaker->fill(maker);
    }

    cout <<" --- Save Histograms --- "<<endl;
    TFile* outfile = new TFile("TempOut.root","RECREATE");
    outfile->cd();
    myHistMaker->plusPlus()->Write();
    myHistMaker->minusMinus()->Write();
    myHistMaker->plusMinus()->Write();
    outfile->Close();
    
    delete maker;
    maker=0;
    delete myHistMaker;
    myHistMaker=0;
}

