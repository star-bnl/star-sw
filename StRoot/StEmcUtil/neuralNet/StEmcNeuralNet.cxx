#include "StEmcNeuralNet.h"
#include "NeuNetLinkDef.h"
#include "NeuNet.h"
#include "TTree.h"
#include "TKey.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TNtuple.h"
#include <iostream>
#include "TStopwatch.h"

ClassImp(StEmcNeuralNet)

StEmcNeuralNet::StEmcNeuralNet() {

	mKernel = new TNNKernel();

	mHSignal = new TH1F("HSignal", "NN output Signal", 100, 0., 1.);	// trained signal
	mHSignal->SetFillColor(45);
	mHBack = new TH1F("HBack", "NN output Background", 100, 0., 1.);	// trained backgraound
	mHBack->SetFillColor(0);
	mHTested = new TH1F("HTested", "NN output for tested events", 100, 0., 1.);	// trained backgraound
	mHTested->SetFillColor(0);

}
StEmcNeuralNet::~StEmcNeuralNet() {
	freeMemory();
	if (mHSignal)
		delete mHSignal;
	if (mHBack)
		delete mHBack;
}


bool StEmcNeuralNet::initNNet(int first, int last) {

	freeMemory();

	mSignal = new TFile(mFileNames[0].Data());
	mBack = new TFile(mFileNames[1].Data());
	
  mKernel = new TNNKernel("NNKernel", mNPar, "10", 1);
	mKernel->SetLearnParam(0.2); // the learning parameter (<1)
	mKernel->SetInitParam( -2, 2); // bounds for the initialisation of weights
	// set the use of biases in the kernel (default)
	// to inhibit it type k->SetUseBiases(0);
	mKernel->SetUseBiases();
	mKernel->Init(); // initialisation of the kernel
	mKernel->PrintS(); // printing of the network structure

  if(first==0 && last==0) return true; // Net is not going to be trained
  
	mSignalTree = getTree(mSignal);
	mBackTree = getTree(mBack);

	if (!mSignalTree || !mBackTree)
		return false;

	mTrain = new TNNTree();
	mTrain->SetFile("TNNBuffer.root");
	mTrain->SetFormulaTree(mSignalTree);
	mTrain->SetInFormula((Text_t*)mFormula.Data());
	mTrain->SetClip();
	mTrain->AddTree(mSignalTree, "1", first, last);	// signal
	mTrain->AddTree(mBackTree, "0", first, last);	// background
	mTrain->Infos();

	mKernel->SetTrainTree(mTrain);

	mUSignal = new TNNUtils();			// for the signal
	mUSignal->SetTree(mSignalTree);                             // the tree
	mUSignal->SetNewBranchFile("UtilSignal.root"); 	// the file
	mUSignal->SetFormula((Text_t*)mFormula.Data());	// the formula
	mUSignal->SetKernel(mKernel);                            // the NN kernel

	mUBack = new TNNUtils();			// for the signal
	mUBack->SetTree(mBackTree);                             // the tree
	mUBack->SetNewBranchFile("UtilBack.root"); 	// the file
	mUBack->SetFormula((Text_t*)mFormula.Data());	// the formula
	mUBack->SetKernel(mKernel);                            // the NN kernel

	return true;

}
bool StEmcNeuralNet::train(int ncycles, int update) {
	cout << "nbr evts for training : " << mKernel->GetNTrainEvents() << endl;
  
  if(update ==0)
  {
    for(int i =0;i<ncycles;i++)
    { 
      mKernel->Mix();
      mKernel->TrainOneCycle();
      if((i*10)%ncycles==0) cout <<"... "<<10*(i*10)/ncycles<<"% done \n";
    }
    return true;
  }
  if(mControl) delete mControl;
	mControl = new TNNControlE();
	mKernel->TrainNCycles(mControl, update, ncycles);
	return true;
}
TTree* StEmcNeuralNet::getTree(TFile *f) {
	int nh = f->GetNkeys();
	TList *l = f->GetListOfKeys();
	for (int i = 0;i < nh;i++) {
		TKey *key = (TKey*)l->At(i);
		TString nn = key->GetName();
		TString clas = key->GetClassName();
		if (!clas.CompareTo("TTree")) {
			cout << "Found TTree - " << nn.Data() << endl;
			TTree *t = (TTree*)f->Get(nn.Data());
			return t;
		}
	}
	return NULL;
}
void StEmcNeuralNet::saveKernel(char* file) {
	mKernel->Export((Text_t*)file);
}
void StEmcNeuralNet::loadKernel(char* file) {
	mKernel->Import((Text_t*)file);
}
void StEmcNeuralNet::drawNNetHists() {
	mUSignal->FillNB();
	mUBack->FillNB();
	TCanvas* chist = new TCanvas("chist", "NN output", 600, 100, 600, 350);
	chist->SetFillColor(42);
	mSignalTree->Draw("OutputU[0] >> HSignal", "", "", 48, 0);	// signal
	mBackTree->Draw("OutputU[0] >> HBack", "", "", 48, 0);	// background
	mHSignal->Scale(100. / mHSignal->Integral());	// rescale signal histo --> 100 events per histo
	mHBack->Scale(100. / mHBack->Integral());	// the same for background histo
	Float_t maxitr = (Float_t)TMath::Max(mHSignal->GetMaximum(), mHBack->GetMaximum());
	mHSignal->SetMaximum( maxitr + sqrt(maxitr) );
	mHSignal->Draw();
	mHBack->Draw("same");

	TCanvas* chist2 = new TCanvas("chist2", "NN output 2", 600, 100, 600, 350);
	chist2->SetFillColor(42);
	TGraph *g = mUSignal->XY(mUSignal->HIntegral(mHSignal, 1), mUSignal->HIntegral(mHBack, 0), 2);
	g->Draw("ALP");
	g->GetXaxis()->SetTitle("signal efficiency");
	g->GetYaxis()->SetTitle("background rejection");
	g->Draw("LP");
}
float StEmcNeuralNet::getNNGuess(float* Var) {

	if (!mKernel->IsTrained()) return 0;   
	if(mNValid==0) // first event to be validated... create trees ...
  {
    mValidTree = new TTree;
	  mValidTree->Branch("validation", Var, mFormula.Data() );
	  mValidTree->Fill();
    
    mUValid = new TNNUtils;
	  mUValid->SetTree(mValidTree);
	  mUValid->SetNewBranchFile("test_branch_file.root");
	  mUValid->SetFormula((Text_t*)mFormula.Data());
	  mUValid->SetKernel(mKernel);
    mUValid->FillNB();
  }
  else
  {
    mValidTree->SetBranchAddress("validation", Var);
    mValidTree->Fill();
    mUValid->FillNB();
  }
      
  TBranch* Branch = mValidTree->GetBranch("OutputU");
	Branch->GetEntry(mNValid);
	float p = Branch->GetLeaf("OutputU")->GetValue(); 
  
  mNValid++;
  
  if(mNValid==1000) // buffer exausted. Set a limit to avoid huge files in the future
  {
    if(mValidTree) { delete mValidTree; mValidTree = NULL; }
    if(mUValid)    { delete mUValid; mUValid = NULL; }
    mNValid = 0;
  }
  mHTested->Fill(p);
	return p;
}

void StEmcNeuralNet::freeMemory() {

	if (mKernel)
		delete mKernel;
	if (mTrain)
		delete mTrain;
	if (mValid)
		delete mValid;

	if (mSignal)
		delete mSignal;
	if (mBack)
		delete mBack;
	if (mSignalTree)
		delete mSignalTree;
	if (mBackTree)
		delete mBackTree;
	if (mValidTree)
		delete mValidTree;

	if (mUSignal)
		delete mUSignal;
	if (mUBack)
		delete mUBack;
  if(mValidTree) { delete mValidTree; mValidTree = NULL; }
  if(mUValid)    { delete mUValid; mUValid = NULL; }
	if (mControl)
		delete mControl;

}

