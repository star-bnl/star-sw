/*!\file NeuralNet.C
\author Alexandre Suaide

This is an example on how to train and use the StEmcNeuralNet package
This macro has three methods:<br><br>
  - NNLoad() - This method loads the necessary libraries to use the neural net
  - NNTrain() - this function trains the neural net. The train events should
    be saved in a ROOT file as TNTuple. 
  - NNUse()   - this is an example on how to use the neural net trained kernel
*/

/*!\fn NNLoad
\author Alexandre Suaide

This function loads the necessary libraries to use the StEmcNeuralNet
*/
void NNLoad()
{
  gSystem->Load("$ROOTSYS/lib/libProof.so");
  gSystem->Load("$ROOTSYS/lib/libTree.so");
  gSystem->Load("$ROOTSYS/lib/libTreePlayer.so");
  gSystem->Load("$ROOTSYS/lib/libTreeViewer.so");
  gSystem->Load("St_base");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_Tables");
  gSystem->Load("StChain");
  gSystem->Load("StEvent");
  gSystem->Load("StEmcUtil");
}

/*!\fn NNTrain
\author Alexandre Suaide

This function is an example on how to train the StEmcNeuralNet
*/
void NNTrain()
{  
  // instantiate the neural net
  StEmcNeuralNet *n = new StEmcNeuralNet(); 
  // set the file that contains the signal events
  n->setSignalFile("NNsignal_less1GeV.root");
  // set the file that contains the background events
  n->setBackFile("NNbackg_less1GeV.root");
  // set the number of parameters
  n->setNParameters(6);
  // set the neural net formula
  n->setFormula("v1:v2:v3:v5:v6:v7");
  // initializes the neural net for training
  n->initNNet(0,48); 
  // train the neural net. If second parameter is set to 0, no evolution graph is displayed.
  n->train(6000,200);  
  // saves the neural net kernel
  n->saveKernel("kernel.dat");
  // draw QA histograms
  n->drawNNetHists();
}

/*!\fn NNUse
\author Alexandre Suaide

This function is an example on how to use the StEmcNeuralNet
*/
void NNUse()
{  
  // instantiante neuralnet
  StEmcNeuralNet *n = new StEmcNeuralNet();
  // set number of parameters
  n->setNParameters(6);
  // set the neural net formula
  n->setFormula("v1:v2:v3:v5:v6:v7");
  // initializes the neural net (if no parameters are entered, neural net is not supposed to be trained)
  n->initNNet(); 
  // loads the neural net kernel
  n->loadKernel("kernel.dat");
  
  // this just creates an histogram to write the output of the
  // neural net.
  //
  // to use the neural net, just put the parameters in a float
  // array (x[] in this example) and just ask for the neural
  // net output for this set of parameters by using the
  // method getNNGuess(x), as seen in this example
  //
  TH1F *h=new TH1F("test","test",100,0,1);
  TFile f("test.root");
  TTree *nn = (TTree*)f.Get("Scaled_Electrons;1");
  double xx[15];
  float x[6];
  nn->SetBranchAddress("Scaled_Electrons_Branch",&xx[0]);
  for(int i=0;i<40;i++)
  {
    nn->GetEntry(i+1);
    x[0]=xx[1];x[1]=xx[2];x[2]=xx[3];x[3]=xx[5];x[4]=xx[6];x[5]=xx[7];
    float o = n->getNNGuess(x);
    h->Fill(o);
  }
  h->Draw();
}
