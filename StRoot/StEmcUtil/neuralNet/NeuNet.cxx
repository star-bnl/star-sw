//////////////////////////////////////////////////////////////////
//
//  Neural Network classes :
//
//  J.P. Ernenwein, Universite de Haute Alsace
//  rnenwein@in2p3.fr
//  last changes : 25/10/2001 for ROOT V3.01
//
//
//  TNNFormula : used by following classes
//  TNNTree    : trees to prepare the NN work
//  TNNKernel  : feed forward kernel
//  TNNControlE: tool to control the training
//  TNNUtils   : utilities to use a trained kernel with standard TTree's
// 
//////////////////////////////////////////////////////////////////
//
//Begin_Html
/*
<img src="histex.gif">   
<img src="chist.gif">
<img src="ccurves.gif">  
*/
//End_Html
//  NeuNet is a set of classes to do a NN study of a problem
//
//  the NN is a feed forward one with 1 or more hidden layers.
//
//-----------------------------------------------------------------
//  New since 12/1999 :
//
//  the Kernel is in double precision.
//  a momentum parameter and flat spot elimination parameter are
//  available for learning (default values : 0 )
//
//  New for ROOT V3.01 :
//
//  1)Some pointers are initialised at 0 in TNNKernel and TNNUtils.
//  It was necessary because libNeW isn't linked any more
//  and the initialisation is now random if not explicit.
//  2)TNNFormula has a Refresh method used by the AddTree method.
//  3)The TNNUtils SetFormula(Text_t *form, Bool_t clip=1) method
//  has now the clip parameter, to enable the clipping disabling.
//-----------------------------------------------------------------
//
//
//  With these classes you can, from one or more TTree's (ex : mytree) :
//
//  - extract the training and validation events, with the relevant variables
//    ( you can select the events with cuts) : you are building the TNNTree's :
//    one for training, one for validation (if you want).  
//
//  - construct a TNNKernel Object : you choose number of inputs, hidden layers,
//    outputs, learning parameters, ... 
//
//  - associate the TNNTree's to the TNNKernel Object. 
//    
//  - loop on training cycles, with validation or not :
//    method TrainNCycles(TNNControlE *conte, Int_t period=5, Int_t nCycles=10)
//    of the kernel.
//    A TNNControlE object enables to plot the
//    errors between computed output and teaching output.
//
//  - after training (but also during it), you can associate the kernel
//    to TTree's, so that the outputs of the kernel are visible by the
//    TTree, to use for example mytree->Draw("v1:OutputU[0]"),
//    OutputU[i] being the standard name given to the values of outputs.
//    The tools are provided by the class TNNUtils
//
//////////////////////////////////////////////////////////////////////////////
//  example  of macros in order to use the classes : nn.C and nngrap.C :
//////////////////////////////////////////////////////////////////////////////
//   //////////////////// macro nn.C >>>>>>>>>>>>>>>>>>>
//   
//   {
//   //////////// this is a demo macro for NeuNet Classes.
//   //////////// its name is nn.C, use before the macro prodtree.C
//   //////////// to produce the trees needed by this macro.
//   //////////// THIS example consists of 2 "processes" : signal and background.
//   //////////// 5 variables are used to define the processes, these variables
//   //////////// have gaussian distribution (see the plots), and the NN
//   //////////// discriminates using these variables : v1 v2 v3 v4 v5. 
//   //////////// 
//   //////////// => the kernel has 5 input units, I have choosen 8 hidden units,
//   //////////// and 1 output unit : 1 -> it's signal, 0 -> it's background. 
//   
//   
// //load the ROOT shared libraries (new since v2.23)
//  gSystem->Load("$ROOTSYS/lib/libProof.so");
//  gSystem->Load("$ROOTSYS/lib/libTree.so");
//  gSystem->Load("$ROOTSYS/lib/libTreePlayer.so");
//  gSystem->Load("$ROOTSYS/lib/libTreeViewer.so");
// //load the NN shared library
//     gSystem->Load("~rnenwein/root/NN/libNeuNet.so");     
//   // file containing the "signal"
//     TFile f1("NNsignal.root"); 
//   // we get the tree     
//     TTree *t1=(TTree*)f1.Get("NNsignal");  
//    
//   // file containing the "background"  
//     TFile f2("NNbackg.root");    
//   // we get the tree 
//     TTree *t2=(TTree*)f2.Get("NNbackg"); 
//    
//     gROOT.cd();  
//    
//   // allocation of the kernel : here we have one hidden layer with 8 units,
//   // replace "8" by "3:4:5" to have 3 hidden 
//   // layers with 3, 4 and 5 units for example.
//     TNNKernel *k= new TNNKernel("Test NN",5,"8",1); 
//   
//     k->SetLearnParam(0.2); // the learning parameter (<1)
//     k->SetInitParam(-2,2); // bounds for the initialisation of weights
//     
//   // set the use of biases in the kernel (default)  
//   // to inhibit it type k->SetUseBiases(0);
//     k->SetUseBiases();     
//   
//     k->Init(); // initialisation of the kernel
//     k->PrintS(); // printing of network structure
//
//     TNNTree valid;  // declaration of validation tree
//     TNNTree train;  // declaration of training tree
//     
//   // to deal with valid and train trees, we need files (as buffers)
//     valid.SetFile("testvalid.root"); 
//     train.SetFile("testtrain.root");
//     
//   //  we will get some variables from the trees t1 and t2, in order
//   //  to build the valid and train trees, we use for that TNNFormula's.
//   //  first, we set a tree structure recognized by the formula's :
//     valid.SetFormulaTree(t1);
//     train.SetFormulaTree(t1);
//     
//   //  second, we define the formula's : here we have 5 input neurons, so
//   //  the formula has 5 parts, we could also use ::sqrt(v1) for example ...  
//     valid.SetInFormula("v1:v2:v3:v4:v5");
//     train.SetInFormula("v1:v2:v3:v4:v5");
//     
//   // clips the values computed by the input formula's (the default) (in [0,1])
//   // if you don't want to do that, type   SetClip(0) :
//     train.SetClip();
//     valid.SetClip(); 
//     
//   // set some cuts for the filling of the valid and train tree's
//   // the default is "", so no cut, in my examples there are stupid cuts
//     train.SetCut("v1>-1.&&v4>-1.");
//     valid.SetCut("v1>-1.&&v4>-1.");   
//    
//     
//   // at the end we can fill the valid and train trees, the formula's are
//   // used for input computation, and the second field in AddTree is the values
//   // of the output units, here we have only one, for more we should
//   // put : "1:0:0" for example, or "0.5:0.5" ...
//     train.AddTree(t1,"1",0,3999);
//     valid.AddTree(t1,"1",4000,5000);
//     train.AddTree(t2,"0",0,3999);
//     valid.AddTree(t2,"0",4000,5000);  
//     
//   // REMARK 1: we need sometimes to use output values different for
//   // EACH event (e.g. for a fit). In this case we can define an output
//   // formula which will get values in tree's t1 and t2, and will compute
//   // the outputs (as the input formula) :
//   // the methods are :  SetOutFormula(Text_t *formula)
//   // and   train.AddTree(t1,0,3000);  WITHOUT THE SECOND PARAMETER
//   // (and SetOutClip(Bool_t trueForClip=1) for clipping)
//   
//   // REMARK 2: SetCut is active for input and output
//   
//   
//   // methods to see what we have done :
//     train->Infos();
//     valid->Infos();
//     
//   
//   ////////////////////////////////////////////////////////////////
//   //  for results exploitation :
//   ///////////////////////////////////////////////////////////////  
//   // we use the  TNNUtils class : an object tu1 is associated to t1 and
//   // k : in this way t1 recognizes the  outputs of the NN, with the name :
//   // OutputU[i], and you can draw with the standard method Draw().
//   // The principle is the following:
//   // a new branch is created for t1, but in another file.
//     
//     TNNUtils *tu1=new TNNUtils();
//     tu1->SetTree(t1);                            // the tree
//     tu1->SetNewBranchFile("testnewbr1.root"); // the file
//     tu1->SetFormula("v1:v2:v3:v4:v5");        // the formula to compute the NN inputs
//     tu1->SetKernel(k);                           // the NN kernel
//    
//     TNNUtils *tu2=new TNNUtils();        // idem
//     tu2->SetTree(t2);
//     tu2->SetNewBranchFile("testnewbr2.root");
//     tu2->SetFormula("v1:v2:v3:v4:v5");
//     tu2->SetKernel(k);  
//     
//   // histos to plot signal and background :
//     
//     TH1F h1tr("histo1 train","NN output",50,0.,1.);
//     h1tr.SetFillColor(45);    
//     TH1F h2tr("histo2 train","NN output",50,0.,1.);  
//     
//     TH1F h1va("histo1 valid","NN output",50,0.,1.);   
//     TH1F h2va("histo2 valid","NN output",50,0.,1.);
//     
//   // canevas to plot histos
//     
//     chist = new TCanvas("chist","NN output",600,100,600,350);
//     chist->SetFillColor(42);
//     
//   // canevas to plot curves
//     
//     ccurves = new TCanvas("ccurves","NN output",600,500,600,350);
//     ccurves->SetFillColor(40);
//   
//   ////////////////////////////////////////////////////////////////
//   // the beginning of the work :
//   // we first associate the train tree to the kernel
//   
//     k->SetTrainTree(&train);
//     printf(" nbr evts for training : %i \n",k->GetNTrainEvents());
//     
//   // we then associate the valid tree to the kernel
//   
//     k->SetValidTree(&valid);
//     printf(" nbr evts for validation : %i \n",k->GetNValidEvents());
//     
//   // we will use a canevas to plot the output errors during training :
//     TNNControlE conte;
//     
//   // here we train on 70 cycles, with a plot of error each 5 cycles :
//   
//     k->TrainNCycles(&conte,5,70);
//   
//   ///////////////////////////////////////////////////////////////
//   // now the kernel is trained
//   // you can export it in an ascii file with
//   // k->Export("name_of_file")
//   // you can import one another with
//   // k->Import("name_of_file")
//   // in this case the structure of the NN k will be adapted
//   // to the new numbers
//   ////////////////////////////////////////////////////////////////
//   
//   //  results exploitation : call the macro nngrap.C
//      printf("now you can call the macro nngrap.C : .x nngrap.C\n");
//   
//   }
//   
//
//
//
//   //////////////////// macro nngrap.C >>>>>>>>>>>>>>>>>>>
//   {
//   //////////// this is a demo macro for NeuNet Classes.
//   //////////// its name is nngrap.C, use before the macro 
//   //////////// nn.C 
//   ////////////////////////////////////////////////////////////////
//   //  results exploitation 
//   ///////////////////////////////////////////////////////////////  
//   // IMPORTANT : the following line fill the new branch, and you
//   // have to do that if you train more the kernel
//   
//     tu1->FillNB();tu2->FillNB();
//   
//   /////////////////////////////////////////////////////
//   ///// now it is standard ROOT :  
//     chist->cd();chist->Clear();
//     t1->Draw("OutputU[0]>>histo1 train","","",4000,0); // training event
//     t2->Draw("OutputU[0]>>histo2 train","","",4000,0);
//     t1->Draw("OutputU[0]>>histo1 valid","","",1000,4000); // others events
//     t2->Draw("OutputU[0]>>histo2 valid","","",1000,4000);  
//     h1va.Sumw2();
//     h2va.Sumw2();
//     h1tr.Scale(100./h1tr.Integral()); // rescale --> 100 events per histo
//     h2tr.Scale(100./h2tr.Integral());
//     h1va.Scale(100./h1va.Integral());
//     h2va.Scale(100./h2va.Integral());
//     Float_t maxihtr=(Float_t)TMath::Max(h1tr.GetMaximum(),h2tr.GetMaximum());
//     Float_t maxihva=(Float_t)TMath::Max(h1va.GetMaximum(),h2va.GetMaximum());
//     maxihtr=TMath::Max(maxihtr,maxihva);
//     h1tr.SetMaximum(maxihtr+::sqrt(maxihtr));
//    
//     h1tr.Draw();h2tr.Draw("same");
//     h1va.Draw("esame");h2va.Draw("esame");
//   
//     ccurves->cd();ccurves->Clear();
//   // rejection versus efficiency graph :		
//     tu1->XY(tu1->HIntegral(&h1tr,1),tu1->HIntegral(&h2tr,0),2)->Draw("ALP");
//     tu1->XY(tu1->HIntegral(&h1va,1),tu1->HIntegral(&h2va,0),3)->Draw("LP");
//     
//     TText xleg(0.4,0.02,"signal efficiency");xleg.SetNDC();xleg.Draw();   
//     TText yleg(0.05,0.2,"background rejection");yleg.SetNDC();yleg.SetTextAngle(90);yleg.Draw();
//   
//     		    	      
//   }
//   
//   
//   
//////////////////////////////////  end of the macros ////////////////////////// 
//
//
//
//
//
// 
//////////////////////////////////////////////////////////////////

#include "NeuNet.h"

///////////////////////////////////////////////////////////////////
//
//   TNNFormula
//
///////////////////////////////////////////////////////////////////

ClassImp(TNNFormula)


TNNFormula::TNNFormula(Text_t *name, Text_t *formula, Text_t *cut, TTree *tree):TNamed(name,"NN Formula")
{
// constructor  
  fNValues=0;
  fTTCut=0;
  fTTFormula=0;
  fClip=1;
  fTree=0;
  fFormula=0;
  fCut=0;
  fRefresh=0;
  SetTree(tree);
  SetFormula(formula);
  SetCut(cut);
}


TNNFormula::~TNNFormula() 
{
// destructor  
  Int_t i;    
  
  for(i=0;i<fNValues;i++) delete fTTFormula[i];
  if(fNValues){delete [] fTTFormula;fTTFormula=0;}  
  if(fFormula){delete fFormula;fFormula=0;}  
  if(fCut){delete fCut;fCut=0;}
  if(fTTCut){delete fTTCut;fTTCut=0;}      
}  

void TNNFormula::SetFormula(Text_t *formula)
{
  if(!fTree){printf("A Tree must be set before !\n");return;}
  if(!RMBlanks(formula)){printf("EMPTY INPUT FORMULA !\n");return;}
  if(!fRefresh)
  {
     if(fFormula){delete fFormula;fFormula=0;}
     fFormula=new TStringLong(formula);
  }
  Int_t i,j;
  TString *oneUnit;
  
  for(i=0;i<fNValues;i++)delete fTTFormula[i];
  if(fNValues){delete [] fTTFormula;fTTFormula=0;}
  
  fNValues=1;
  for (i=0;formula[i];i++)if(formula[i]==':')fNValues++;   
  
  fTTFormula = new TTreeFormula*[fNValues];  
  
  oneUnit=new TString();
  j=0;
  for (i=0;formula[i];i++)
  {
    if (formula[i]!=':')
      oneUnit->Append(formula[i]);
    else
    {
      fTTFormula[j] = new TTreeFormula("Input",oneUnit->Data(),fTree);
      delete oneUnit;oneUnit=new TString();
      j++;   
    }
  }  
  fTTFormula[j] =  new TTreeFormula("Input",oneUnit->Data(),fTree);
  delete oneUnit; 
  
  printf("Formula =  >>%s<< with %3i values\n",formula,fNValues);
}  

void TNNFormula::SetCut(Text_t *cutarg)
{
  if(!fTree){printf("A Tree must be set before !\n");return;}
  Text_t cut[500];
  strcpy(cut,cutarg);  
  if(!fRefresh)
  {
     if(fCut){delete fCut;fCut=0;}
     fCut=new TStringLong(cut);
  }
  if (fTTCut){delete  fTTCut; fTTCut=0;}
  if (RMBlanks(cut))
    fTTCut = new TTreeFormula("NNFormula Selection",cut,fTree);

  printf("Cut applied in TNNFormula set to  >>%s<< \n",cut);  
  
}  

void TNNFormula::SetTree(TTree *tree)
{
// set the current TTree, and update formula and cut if there are any
  
  if(!tree){printf("No Tree !\n");return;}
  
  Int_t i;
  fTree=tree;
  for (i=0;i<fNValues;i++) fTTFormula[i]->SetTree(fTree);
  if(fTTCut)fTTCut->SetTree(fTree);
}

Bool_t TNNFormula::Find(Int_t iEvent, Float_t *values)
{
// Finds and Selects with TNNFormula  cut, and returns the array values.  
// this function gets event iEvent in tree pointed by tree.
// if the cut isn't satisfied, Find returns FALSE.
  
  if(!fTTFormula){printf("Empty input string !\n");return 0;}
  Int_t i;   
  
// fill the array 

//  fTree->LoadTree(fTree->GetEventNumber(iEvent));// load event
  fTree->LoadTree(iEvent);// load event
  
  for (i=0;i<fNValues;i++) 
    values[i]=Clip(fTTFormula[i]->EvalInstance(0));

// return true or false according to the fTTCUt value
 
  if (fTTCut) 
    return (Bool_t) fTTCut->EvalInstance(0); 
  else
    return (Bool_t) 1;  
}

void TNNFormula::Find(Int_t iEvent)
{
// Finds and Selects with TNNFormula  cut, and prints values.  
// this function gets event iEvent in tree pointed by tree.
  
  if(!fTTFormula){printf("Empty input string !\n");return;}
  Int_t i;   

//  fTree->LoadTree(fTree->GetEventNumber(iEvent));// load event
  fTree->LoadTree(iEvent);// load event

  for (i=0;i<fNValues;i++) printf("%6.2f ",Clip(fTTFormula[i]->EvalInstance(0)));
  if(fTTCut)
    if(fTTCut->EvalInstance(0)) printf(" SELECTED\n"); else printf(" NOT SELECTED\n");
  else printf("\n");						       
}

Int_t TNNFormula::RMBlanks(Text_t *str)
{
// remove blanks in a string  
  if(!strlen(str)) return 0;
  TStringLong temp(str);
  Int_t posi;
  while((posi=(Int_t)temp.Index(" "))!=-1){temp.Remove(posi,1);}
  strcpy(str,temp.Data());
  return strlen(str);
}

void TNNFormula::SetClip(Bool_t trueForClip)
{
  fClip=trueForClip;
}

Float_t TNNFormula::Clip(Float_t x)
{
  if (!fClip) return x;
  if (x<0)   return 0.;
  if (x>1)   return 1.;
  return x;
}

//////////////////////////////////////////////////////////////////
//
//  Neural Network class TNNTree 
//
//////////////////////////////////////////////////////////////////

ClassImp(TNNTree)

TNNTree::TNNTree(Text_t *name):TNamed(name,"Neural Network")
{
// constructor  
  fTree=0;
  fNTrees=0;  
  fFile=0;
  fInfos=0;
  fFName=0;
  fInput=0;
  fOutput=0;
  fNInput=0;
  fNOutput=0;
}  
  
TNNTree::~TNNTree() 
{
// destructor  
  DeleteTree();
  if (fFName) delete [] fFName;
}  


void TNNTree::AddTree(TTree *tree, Int_t begin, Int_t end)
{
// function to add a piece of a given Tree in fTree  
// this function uses the input and output TNNFormula's  
// you must have called before :
// SetFile(Text_t *fName)  to choose a file for saving the tree
// SetFormulaTree(TTree *tree) to define formula's
// SetInFormula(Text_t *formula)  
// SetOutFormula(Text_t *formula)  
  
  if(!fFName){printf("NO FILE SELECTED !\n");return;}
  if(!tree){printf("Bad pointer on TTree !\n");return;} 
  if(fFormula.Length()==0){printf("EMPTY INPUT FORMULA, GIVE AN INPUT FORMULA FIRST !\n");return;}
  if(!fFormula.GetNValues()){printf("NO INPUT UNIT, GIVE AN INPUT FORMULA FIRST !\n");return;}
  if(fOutFormula.Length()==0){printf("EMPTY OUTPUT FORMULA, GIVE AN OUTPUT FORMULA FIRST !\n");return;}
  if(!fOutFormula.GetNValues()){printf("NO OUTPUT UNIT, GIVE AN OUTPUT FORMULA FIRST !\n");return;}
  
  SetFormulaTree(tree);RefreshInFormula();RefreshOutFormula();
  
  if(fTree&&fFormula.GetNValues()!=fNInput) DeleteTree();
  if(fTree&&fOutFormula.GetNValues()!=fNOutput) DeleteTree();
  
  fNInput=fFormula.GetNValues();
  fNOutput=fOutFormula.GetNValues();
  CheckRange(&begin,&end,(Int_t)(tree->GetEntries())-1);
  
  Int_t i,nSelected,range;
   
  if (!fTree) CreateTree(); // create the Tree
    
  fFormula.SetTree(tree);
  fOutFormula.SetTree(tree);
  nSelected=(Int_t)(fTree->GetEntries());
  for (i=begin;i<=end;i++)if(fFormula.Find(i,fInput)){fOutFormula.Find(i,fOutput);fTree->Fill();}
  fFile->Write();
  nSelected=(Int_t)(fTree->GetEntries())-nSelected;
  range=end-begin+1;
  
  fInfos[fNTrees]=new Text_t[strlen(tree->GetName())+60];
  sprintf(fInfos[fNTrees],"%s ,range  [%7i,%7i]=%7i, %7i selected",tree->GetName(),begin,end,range,nSelected);
  printf("%s \n",fInfos[fNTrees]);
  fNTrees++; 
}


void TNNTree::AddTree(TTree *tree, Text_t *out, Int_t begin, Int_t end)
{
// function to add a piece of a given Tree in fTree  
// this function uses the input TNNFormula only  
// you must have called before :
// SetFile(Text_t *fName)  to choose a file for saving the tree
// SetFormulaTree(TTree *tree) to define input formula
// SetInFormula(Text_t *formula)  
// and the values of outputs are in the string out : "0.1:0.6:0.2" 
  
  if(!fFName){printf("NO FILE SELECTED !\n");return;}
  if(!tree){printf("Bad pointer on TTree !\n");return;} 
  if(fFormula.Length()==0){printf("EMPTY INPUT FORMULA, GIVE AN INPUT FORMULA FIRST !\n");return;}
  if(!fFormula.GetNValues()){printf("NO INPUT UNIT, GIVE AN INPUT FORMULA FIRST !\n");return;}
  SetFormulaTree(tree);RefreshInFormula();
  if(fTree&&fFormula.GetNValues()!=fNInput) DeleteTree();
  Int_t newNO=NumberOut(out);
  if(fTree&&newNO!=fNOutput) DeleteTree();
  
  fNInput=fFormula.GetNValues();
  fNOutput=newNO;
  CheckRange(&begin,&end,(Int_t)(tree->GetEntries())-1);
  
  Int_t i,nSelected,range;
   
  if (!fTree) CreateTree(); // create the Tree
    
  fFormula.SetTree(tree);
  Decode(out);
  nSelected=(Int_t)(fTree->GetEntries());
  for (i=begin;i<=end;i++)if(fFormula.Find(i,fInput))fTree->Fill();
  fFile->Write();
  nSelected=(Int_t)(fTree->GetEntries())-nSelected;
  range=end-begin+1;
  
  fInfos[fNTrees]=new Text_t[strlen(tree->GetName())+60];
  sprintf(fInfos[fNTrees],"%s ,range  [%7i,%7i]=%7i, %7i selected",tree->GetName(),begin,end,range,nSelected);
  printf("%s \n",fInfos[fNTrees]);
  fNTrees++; 
}

void TNNTree::Infos() 
{
// prints infos about fTree
  
  Int_t i;
  if(fFName)printf("%s \n",fFName);
  for (i=0;i<fNTrees;i++)printf("%3i : %s \n",i,fInfos[i]); 
}

void TNNTree::SetFile(Text_t *namearg)
{
// sets the file associated with fTree 
// example : "/home/someone/rootfiles/tree.root"
  
  if(fTree){printf("File already set and used !\n");return;}
  Text_t name[500];
  strcpy(name,namearg);
  if(!fFName) fFName=new Text_t[500];
  RMBlanks(name);
  strcpy(fFName,name);
  printf("File to record Tree set to  >>%s<< \n",fFName);
}

void TNNTree::CheckRange(Int_t *begin, Int_t *end, Int_t indexMax)
{
  Int_t temp;

  if (*begin<0) *begin=0;
  if (*end<0) *end=0;
  if (*begin>indexMax) *begin=indexMax;
  if (*end>indexMax) *end=indexMax;

  if (*begin>*end)
  {
     temp=*begin;
     *begin=*end;
     *end=temp;
  }
}

Int_t TNNTree::RMBlanks(Text_t *str)
{
// remove blanks in a string  
  if(!strlen(str)) return 0;
  TStringLong temp(str);
  Int_t posi;
  while((posi=(Int_t)temp.Index(" "))!=-1){temp.Remove(posi,1);}
  strcpy(str,temp.Data());
  return strlen(str);
}

void TNNTree::GetEvent(Float_t *input, Float_t *output, Int_t iEvent)
{
// Make a copy in input and output, these adresses are filled
  if(!fTree){printf("No Tree !\n");return;}
  Int_t i;
  fTree->GetEvent(iEvent);
  for(i=0;i<fNInput;i++)input[i]=fInput[i];
  for(i=0;i<fNOutput;i++)output[i]=fOutput[i];
}

void TNNTree::DeleteTree()
{
// delete the tree, its file and its buffers  
  if(!fTree) return;
  Int_t i;
  delete fTree;fTree=0; 
  fFile->Close(); fFile->Delete(); fFile=0;
  for(i=0;i<fNTrees;i++) delete [] fInfos[i];
  delete [] fInfos;
  if (fInput) delete [] fInput;  
  if (fOutput) delete [] fOutput; 
  fNTrees=0;
  fNInput=0;
  fNOutput=0;
}  


void TNNTree::CreateTree()
{
  Text_t *varnamei,*varnameo,*nunits;

  fFile = new TFile(fFName,"RECREATE","Neural Network");
     
  nunits=new Text_t[4];
  sprintf(nunits,"%i",fNInput);
  varnamei=new Text_t[13];
  strcpy(varnamei,"");
  strcat(varnamei,"InputU[");strcat(varnamei,nunits);strcat(varnamei,"]/F");
  strcpy(nunits,"");
  sprintf(nunits,"%i",fNOutput);
  varnameo=new Text_t[14];
  strcpy(varnameo,"");
  strcat(varnameo,"OutputU[");strcat(varnameo,nunits);strcat(varnameo,"]/F");
  delete [] nunits;  
        
  fInput=new Float_t[fNInput];
  fOutput=new Float_t[fNOutput];
  fFile->cd();
  fTree = new TTree(fFName,"Neural Network");
  fTree->Branch("InputU",fInput,varnamei); 
  fTree->Branch("OutputU",fOutput,varnameo);
  delete [] varnamei;delete [] varnameo;
  gROOT->cd();
    
  fInfos=new Text_t*[200]; // up to 200 Trees Added
}

Int_t TNNTree::NumberOut(Text_t *ttext)
{
  Int_t i,n;
  Text_t text[200];
  strcpy(text,ttext);
  n=1;
  for (i=0;text[i];i++)if(text[i]==':')n++;
  return n;
}

void TNNTree::Decode(Text_t *ttext)
{
  Int_t i,j;
  TString *number;
  Text_t text[200];
  strcpy(text,ttext);
 
  j=0;
  for (i=0;i<fNOutput;i++)
  {
    number=new TString();
    while(text[j]&&(text[j]!=':')){number->Append(text[j]);j++;}
    j++;
    sscanf(number->Data(),"%f",&fOutput[i]);
    delete number;
  }
}

//////////////////////////////////////////////////////////////////
//
//  TNNKernel 
//  Feed-Forward Neural Network 
//
//////////////////////////////////////////////////////////////////

ClassImp(TNNKernel)



TNNKernel::TNNKernel(Text_t *name, Int_t nInput, Text_t *hidden, Int_t nOutput):TNamed(name,"Neural Network")
{
// constructor

  fValues=0;
  fErrors=0;
  fBiases=0;
  fNUnits=0;
  fW=0;
  fValidTree=0;
  fArrayOut=0;
  fArrayIn=0;
  fTeach=0;
  fEventsList=0;
  fNWeights=0;
  fDW=0;
  fDB=0;
  fNHiddL=0;
  
  AllocateVW(nInput,hidden,nOutput);
    
  fUseBiases=1.;
  fLearnParam=0.2;
  fFlatSE=0.;
  fMu=0.;
  fLowerInitWeight=-1.;
  fUpperInitWeight=1.;

  fNTrainEvents=0;
  fNValidEvents=0;
  fNTrainCycles=0;

  TDatime temps;
  fRandom.SetSeed(temps.Convert());
  printf("First Random Seed = %i\n",fRandom.GetSeed());
  printf("Neural Network is created : \n");
//  PrintS();

}

TNNKernel::TNNKernel()
{
// constructor witn no parameter 
  fValues=0;
  fErrors=0;
  fBiases=0;
  fNUnits=0;
  fW=0;
  fValidTree=0;
  fArrayOut=0;
  fArrayIn=0;
  fTeach=0;
  fEventsList=0;
  fNWeights=0;
  fDW=0;
  fDB=0;
  

  fUseBiases=1.;
  fLearnParam=0.2;
  fFlatSE=0.;
  fMu=0.;
  fLowerInitWeight=-1.;
  fUpperInitWeight=1.;
  fNHiddL=0;

  fNTrainEvents=0;
  fNValidEvents=0;
  fNTrainCycles=0;

  TDatime temps;
  fRandom.SetSeed(temps.Convert());
  printf("First Random Seed = %i\n",fRandom.GetSeed());
}


TNNKernel::~TNNKernel() 
{
// destructor  
  
  DeleteArray();
  FreeVW();
  if(fEventsList) delete [] fEventsList;
}  


void TNNKernel::SetHidden(Text_t *ttext)
{
  Int_t i,j;
  TString *number;
  Text_t text[100];
  strcpy(text,ttext);

  fNHiddL=1;
  for (i=0;text[i];i++)if(text[i]==':')fNHiddL++;
  if (fNUnits) delete [] fNUnits;
  fNUnits = new Int_t[fNHiddL+2];

  j=0;
  for (i=1;i<=fNHiddL;i++)
  {
    number=new TString();
    while(text[j]&&(text[j]!=':')){number->Append(text[j]);j++;}
    j++;
    sscanf(number->Data(),"%i",&fNUnits[i]);  
    delete number;
//    printf("%i \n",fNUnits[i]); 
  }

}


void TNNKernel::FreeVW()
{
  Int_t i,l;

  // free of values
  
  if (fValues)
  {
    for (i=0;i<fNHiddL+2;i++)
      {delete [] fValues[i]; delete [] fErrors[i]; delete [] fBiases[i];delete [] fDB[i];} 
    delete [] fValues; delete [] fErrors; delete [] fBiases;delete [] fDB;
    fValues=0;
  }
  
  // free of teaching
  
  if (fTeach) 
  {
    delete [] fTeach;
    fTeach=0;
  }
  
  // free of weights
  
  if (fW)
  {
    for (i=0;i<fNHiddL+1;i++)
    {
      for(l=0;l<fNUnits[i];l++){delete [] fW[i][l];delete [] fDW[i][l];}  
      delete [] fW[i];delete [] fDW[i];
    }    
    fW=0;
  }
  
  // free of units
  
  if (fNUnits){ delete [] fNUnits; fNUnits=0;}
}

void TNNKernel::AllocateVW(Int_t nInput, Text_t *hidden, Int_t nOutput)
{
  Int_t i,l;
  
  if(fW){printf("free memory first !\n");return;}

  SetHidden(hidden);
  fNUnits[0]=nInput;
  fNUnits[fNHiddL+1]=nOutput;
  
  // allocation of values
  
  fValues = new Float_t*[fNHiddL+2];
  fErrors = new Double_t*[fNHiddL+2];
  fBiases = new Double_t*[fNHiddL+2];
  fDB = new Double_t*[fNHiddL+2];

  for (i=0;i<fNHiddL+2;i++)
  {
    fValues[i]=new Float_t[fNUnits[i]]; 
    fErrors[i]=new Double_t[fNUnits[i]]; 
    fBiases[i]=new Double_t[fNUnits[i]]; 
    fDB[i]=new Double_t[fNUnits[i]]; 
  }
  
  // allocation of teaching
  
  fTeach=new Float_t[fNUnits[fNHiddL+1]]; 
    
  // allocation of weights
  
  fW=new Double_t**[fNHiddL+1];
  fDW=new Double_t**[fNHiddL+1];
  
  for (i=0;i<fNHiddL+1;i++)
  {
    fW[i]=new Double_t*[fNUnits[i]];
    fDW[i]=new Double_t*[fNUnits[i]];
    for (l=0;l<fNUnits[i];l++)
    {
      fW[i][l]=new Double_t[fNUnits[i+1]];  
      fDW[i][l]=new Double_t[fNUnits[i+1]]; 
    }
  }
  
}

void TNNKernel::SetKernel(Int_t nInput, Text_t *hidden, Int_t nOutput)
{  
   FreeVW();
   AllocateVW(nInput,hidden,nOutput);
}

void TNNKernel::SetLearnParam(Double_t learnParam,Double_t fse,Double_t mu)
{
// Sets the learning parameters :
// the main learning parameter is around 0.2 (in ]0,1])
// fse is for flat spot elimination, with values in [0,0.25], often 0.1
// mu is for backprop momentum, values in [0,1]
  fLearnParam=fabs(learnParam);
  fFlatSE=fabs(fse);
  fMu=fabs(mu);

  if (fLearnParam>1.0) printf("Warning : %6.2f is not an usual value\n",fLearnParam);
  if (fLearnParam==0.0) printf("Warning : 0 is a stupid value\n");
  printf("Learning Parameter set to : %6.2f\n",fLearnParam);
  printf("Flat Spot elimination value  set to : %6.2f\n",fFlatSE);
  printf("Momentum set to : %6.2f\n",fMu);
}
 
void TNNKernel::SetInitParam(Float_t lowerInitWeight, Float_t upperInitWeight)
{
// Sets the initialisation parameters : max and min weights 
  Float_t temp;

  fLowerInitWeight=lowerInitWeight;
  fUpperInitWeight=upperInitWeight;
  if (fLowerInitWeight>fUpperInitWeight)
  {
    temp=fUpperInitWeight;
    fUpperInitWeight=fLowerInitWeight;
    fLowerInitWeight=temp;
  } 
  if (fLowerInitWeight==fUpperInitWeight)printf("Warning : the weights initialisation bounds are equal !\n");
  printf("Init Parameters set to :\n");
  printf(" --> Lower bound = %6.2f\n",fLowerInitWeight);
  printf(" --> Upper bound = %6.2f\n",fUpperInitWeight);

}


Float_t TNNKernel::Alea()
{
  return fLowerInitWeight+fRandom.Rndm()*(fUpperInitWeight-fLowerInitWeight);
}

void TNNKernel::Init()
{
// initialisation of  biases and weights.  
// the init parameters can be changed by :
// SetInitParam(Float_t lowerInitWeight, Float_t upperInitWeight)
// The default is -1 and 1
  
  Int_t i,l,c;
  
  if(!fW){printf("allocate memory first !\n");return;}
  
  // init of weights
  
  for (i=0;i<fNHiddL+1;i++)
    for (l=0;l<fNUnits[i];l++)
      for (c=0;c<fNUnits[i+1];c++) fW[i][l][c]=(Double_t)Alea();

  for(i=0;i<fNHiddL+1;i++)for(l=0;l<fNUnits[i];l++)for(c=0;c<fNUnits[i+1];c++)
      fDW[i][l][c]=0.;       
  
  // init of biases
  
  for (i=0;i<fNHiddL+2;i++)
    for (l=0;l<fNUnits[i];l++) fBiases[i][l]=(Double_t)(Alea())*fUseBiases;

  for(i=0;i<fNHiddL+2;i++)for(l=0;l<fNUnits[i];l++)fDB[i][l]=0.;


  fNTrainCycles=0;
  printf("Initialisation done\n");
}

void TNNKernel::PrintS()
{
// prints structure of network on screen
  Int_t i,l,c;
  
  if(!fW){printf("no unit !\n");return;} 
  
  printf("+++++++++ Neural Network %s ++++++++++++\n",GetName());
  for(i=0;i<fNHiddL+2;i++)printf("Layer %1i contains %2i units\n",i,fNUnits[i]);

  if(fUseBiases)printf(">>>>>>> Biases USED");else printf(">>>>>>>Biases DUMMY");

  printf("\n ----------   Biases   ---------- \n");
  Int_t maxl=0;
  for(i=0;i<fNHiddL+2;i++)if(fNUnits[i]>=maxl)maxl=fNUnits[i];
  for(i=0;i<fNHiddL+2;i++)printf("    %1i   | ",i);printf("\n");
  for(i=0;i<fNHiddL+2;i++)printf("--------|-");printf("\n");
  for(l=0;l<maxl;l++)
  {
    for(i=0;i<fNHiddL+2;i++)
      if(l<fNUnits[i])printf("%6.2f  | ",fBiases[i][l]);else printf("        | ");
    printf("\n");
  }


  printf("\n    ----------   Weights ----------- \n");
  for(i=0;i<fNHiddL+1;i++)
  {
    printf(" From  %1i  to  %1i  : \n",i,i+1);
    printf("%2i |",i);for(l=0;l<fNUnits[i];l++)printf("  %3i |",l);printf("\n");
    printf("===|");for(l=0;l<fNUnits[i];l++)printf("-------");printf("\n");
    printf("%2i |",i+1);for(l=0;l<fNUnits[i];l++)printf("-------");printf("\n");
    for(c=0;c<fNUnits[i+1];c++)
    { 
       printf("%2i |",c);
       for(l=0;l<fNUnits[i];l++)printf("%6.2f|",fW[i][l][c]);
       printf("\n");
    }     
    printf("\n");
  }  

  printf("\n");
  printf("Learning parameter = %6.2f\n",fLearnParam);
  printf("Flat Spot elimination value = %6.2f\n",fFlatSE);
  printf("Momentum = %6.2f\n",fMu);
  printf("Lower initialisation weight = %6.2f\n",fLowerInitWeight);
  printf("Upper initialisation weight = %6.2f\n",fUpperInitWeight);
  printf("Number of events for training   = %5i\n",fNTrainEvents);
  printf("Number of events for validation = %5i\n",fNValidEvents);
  printf("Number of cycles done = %3i\n",fNTrainCycles);
  printf("+++++++++++++++++++++++++++++++++++++++++++++++\n");

}

void TNNKernel::Forward()
{
// general function to propagate the input activation 
//  The input activation array must be filled  
  Int_t i,l,c;
  Double_t sum;

  if(!fW){printf("no unit !\n");return;}  
  
  for (i=0;i<fNHiddL+1;i++)  
    for (c=0;c<fNUnits[i+1];c++)
    {
      sum=0.; 
      for(l=0;l<fNUnits[i];l++)sum+=fW[i][l][c]*(Double_t)fValues[i][l];
      fValues[i+1][c]=(Float_t)Sigmoide(sum+fBiases[i+1][c]*fUseBiases);
    }
}

void TNNKernel::LearnBackward()
{
// gradient retropropagation (updates of biases and weights)  

  if(fNTrainEvents<1){printf("No event to train !!!\n");return;}
  if(!fW){printf("no unit !\n");return;}

  Int_t i,l,c;
  Double_t delta;
  
// weights
  
  for (i=0;i<fNHiddL+1;i++)  
    for (l=0;l<fNUnits[i];l++)
      for(c=0;c<fNUnits[i+1];c++)
      {
        delta=fLearnParam*fErrors[i+1][c]*(Double_t)fValues[i][l]+fMu*fDW[i][l][c];
        fW[i][l][c]+=delta;
        fDW[i][l][c]=delta;
      }
// biases
  if(((Bool_t)fUseBiases))
  {
    for (i=1;i<fNHiddL+2;i++)  
      for (l=0;l<fNUnits[i];l++)
      {
        delta=fLearnParam*fErrors[i][l]+fMu*fDB[i][l];
        fBiases[i][l]+=delta;
        fDB[i][l]=delta;
      }
  }
}

Double_t TNNKernel::Error()
{
// function to compute the errors between forward propagation and teaching.  
// this error is = |teaching-computed| summed on NN outputs and divided by their number.  
  Int_t i,l,c;
  Double_t sum,error=0,errorOneUnit;
  if(!fW){printf("no unit !\n");return 0;}    
  
//  Error on Output Units

  for(l=0;l<fNUnits[fNHiddL+1];l++)
  {
    errorOneUnit=(Double_t)(fTeach[l]-fValues[fNHiddL+1][l]);
    error+=fabs(errorOneUnit);
    fErrors[fNHiddL+1][l]=errorOneUnit*(SigPrim(fValues[fNHiddL+1][l])+fFlatSE);
  }
  error=error/(Double_t)fNUnits[fNHiddL+1];

//  Error on Hidden Units

  for(i=fNHiddL;i==1;i--)
  {  
    for(l=0;l<fNUnits[i];l++)
    {
      sum=0.;
      for(c=0;c<fNUnits[i+1];c++) sum+=fW[i][l][c]*fErrors[i+1][c];
      fErrors[i][l]=sum*(SigPrim((Double_t)fValues[i][l])+fFlatSE);
    }  
  }
  
  return error;
}

Double_t TNNKernel::ErrorO()
{
// function to compute the errors between forward propagation and teaching.  
// this error is = |teaching-computed| summed on NN outputs and divided by their number.  
//  Error on Output Units
  
  Int_t l;
  Double_t error=0;
  if(!fW){printf("no unit !\n");return 0;}    
  for(l=0;l<fNUnits[fNHiddL+1];l++)
    error+=fabs((Double_t)(fTeach[l]-fValues[fNHiddL+1][l]));

  error=error/(Double_t)fNUnits[fNHiddL+1];  
  
  return error;
  
}  

Double_t TNNKernel::TrainOneCycle()
{
// one loop on internal events = one cycle.  
// takes each event from internal array in an order fixed by an array ( fEventsList ).
// It is necessary to call the method Mix() before each call to this function
// in order to change the presentation order.
// The learning is done by this function.
// The private variable  fNTrainCycles is incremented.

  if(fNTrainEvents<1){printf("No event to train !!!\n");return 0.;}
  if(!fW){printf("no unit !\n");return 0.;}

  Int_t i;
  Double_t error=0.;

  for(i=0;i<fNTrainEvents;i++)
  {  
    GetArrayEvt(fEventsList[i]); 
    Forward();
    error+=Error();
    LearnBackward();
  }
 
  fNTrainCycles++;
  error=error/(Double_t)fNTrainEvents;
//  printf("cycle %i : E_t = %6.4f ",fNTrainCycles,error);

  return error;
}

Double_t TNNKernel::Valid()
{
// one loop on valid events.  
// takes each event from validation tree.
// the events are passed trough the kernel, and a mean output
// error is computed.

  if(fNValidEvents<1) return 0.;
  
// we will now pass all the validation events through the kernel, and
// compute the mean error on output 
  Double_t error=0.;
  for (Int_t j=0;j<fNValidEvents;j++)
  {
      fValidTree->GetEvent(GetInputAdr(),GetTeachAdr(),j);
      error+=GoThrough(); // forward propagation and error on one event	
  }
  error=error/(Double_t)fNValidEvents; // mean
  return error;
}

void TNNKernel::TrainNCycles(TNNControlE *conte, Int_t period, Int_t nCycles)
{
// method to train on N cycles, with mixing and plot of errors
// on the controller conte.

  if(!conte){printf("no controller !\n");return;}
  Float_t errt,errv;
  for(Int_t i=0;i<nCycles;i++)
  {
    Mix();
    errt=(Float_t)TrainOneCycle();
    errv=(Float_t)Valid();
    printf("cycle %3i > train : %7.3f",fNTrainCycles,errt);
    if(fNValidEvents)printf(" and valid : %7.3f \n",errv);else printf("\n");
    if(!(i%period)||i==(nCycles-1))
    {  
       conte->AddTP(fNTrainCycles,errt); // add Train Point
       conte->AddVP(fNTrainCycles,errv); // add Valid Point
       conte->UpdateG();  // update graphics
    }     
    
  }
  
}

void TNNKernel::Export(Text_t *fileName)
{
// Put the structure in a file
// WARNING : the weights and biases are stored with 4 digits
// in decimal part.    
// Learning parameters are not stored
  Int_t i,l,c;
  
  if(!fW){printf("no unit !\n");return;} 
  
  FILE *file;
  file=fopen(fileName,"w");

  fprintf(file,"%3i\n",fNHiddL);
  for(i=0;i<fNHiddL+2;i++)fprintf(file,"%3i\n",fNUnits[i]);

  for(i=0;i<fNHiddL+2;i++)
    for(l=0;l<fNUnits[i];l++)fprintf(file,"%8.4f\n",fBiases[i][l]);

  for(i=0;i<fNHiddL+1;i++)
    for(l=0;l<fNUnits[i];l++)
      for(c=0;c<fNUnits[i+1];c++)fprintf(file,"%8.4f\n",fW[i][l][c]);
  
  fprintf(file,"%5i\n",fNTrainCycles);  
  fprintf(file,"%2.0f\n",fUseBiases); 
    
  fclose(file);   
}

void TNNKernel::Import(Text_t *fileName)
{
// Get the structure from a file
// WARNING : the weights and biases are stored with 4 digits
// in decimal part.
// Learning parameteres are not stored.  
  Int_t i,l,c,newI,newHL,newO;
  Text_t hidden[100],piece[5];
  FILE *file;
  file=fopen(fileName,"r");
  
  fscanf(file,"%3i",&newHL);
  fscanf(file,"%3i",&newI); 
  strcpy(hidden,"");
  for(i=1;i<newHL;i++)
    {fscanf(file,"%s",piece);strcat(hidden,piece);strcat(hidden,":");} 
  fscanf(file,"%s",piece);strcat(hidden,piece);
  fscanf(file,"%3i",&newO); 
  
  printf("New NN set to : %3i  %s  %3i \n",newI,hidden,newO);
  FreeVW();			  
  AllocateVW(newI,hidden,newO);
  Float_t tmpfl;
  for(i=0;i<fNHiddL+2;i++)
    for(l=0;l<fNUnits[i];l++){fDB[i][l]=0.;fscanf(file,"%f",&tmpfl);*(fBiases[i]+l)=(Double_t)tmpfl;}

  for(i=0;i<fNHiddL+1;i++)
    for(l=0;l<fNUnits[i];l++)
      for(c=0;c<fNUnits[i+1];c++){fDW[i][l][c]=0.;fscanf(file,"%f",&tmpfl);*(fW[i][l]+c)=(Double_t)tmpfl;}
      

  fscanf(file,"%5i",&fNTrainCycles);  
  fscanf(file,"%f",&tmpfl);fUseBiases=(Double_t)tmpfl;  
    
  fclose(file);   
}

void TNNKernel::Mix()
{
// mix the events before learning. VERY IMPORTANT.
// is has to be used before  TrainOneCycle() , 
// IT IS NOT used by TrainOneCycle() , you have to do the call yourself
  
  Int_t i,i1,i2;
  Int_t temp;
  for (i=0;i<3*fNTrainEvents;i++)
  {
     i1=(Int_t)(fRandom.Rndm()*(Float_t)fNTrainEvents);
     i2=(Int_t)(fRandom.Rndm()*(Float_t)fNTrainEvents);
     temp=fEventsList[i1];
     fEventsList[i1]=fEventsList[i2];
     fEventsList[i2]=temp;
  }

//  for (i=0;i<fNTrainEvents;i++)printf("%i \n",fEventsList[i]);  
//  printf("Mixed ... ");
}

void TNNKernel::SetArraySize(Int_t size)
{
  DeleteArray();
  if (fEventsList) delete [] fEventsList;
  if(!size)return;
  Int_t i;
  fNTrainEvents=size;  
  fArrayIn  = new Float_t*[fNTrainEvents];
  for (i=0;i<fNTrainEvents;i++) fArrayIn[i] = new Float_t[fNUnits[0]];

  fArrayOut = new Float_t*[fNTrainEvents];  
  for (i=0;i<fNTrainEvents;i++) fArrayOut[i] = new Float_t[fNUnits[fNHiddL+1]];
  
  fEventsList = new Int_t[fNTrainEvents];
  for (i=0;i<fNTrainEvents;i++)fEventsList[i]=i;
}

void TNNKernel::DeleteArray()
{
  Int_t i; 

  if(fArrayIn) 
  {
    for (i=0;i<fNTrainEvents;i++)delete [] fArrayIn[i];
    delete [] fArrayIn;
    fArrayIn=0;
  }

  if(fArrayOut) 
  {
    for (i=0;i<fNTrainEvents;i++)delete [] fArrayOut[i];
    delete [] fArrayOut;
    fArrayOut=0;
  }
  
}

void TNNKernel::SetTrainTree(TNNTree *t)
{
// method to associate a TNNTree to the kernel :
// the events of the tree will be transferred in the internal
// array of the kernel.

  if(!t){printf("no tree !\n");return;}
  Int_t i;
  
//allocation  
  
  SetArraySize((Int_t)(t->GetTree()->GetEntries()));
  printf(" nbr evts for training : %i \n",GetNTrainEvents());  
    
// loop  
// the methods GetInputAdr() and GetTeachAdr()
// return the adresses of arrays in kernel, and the method
// GetEvent fills these adresses with event i of the train tree t
// the method Fill(i) translates the filled arrays in the internal array
   
   for (i=0;i<(Int_t)(t->GetTree()->GetEntries());i++)
   {
     t->GetEvent(GetInputAdr(),GetTeachAdr(),i);
     Fill(i);  
   }

}

void TNNKernel::SetValidTree(TNNTree *t)
{
// method to associate a TNNTree to the kernel :
// a link will be done between the tree and the kernel.
// it is not necessary to keep these events in the kernel

  if(!t){printf("no tree !\n");return;}
  fValidTree=t;
  fNValidEvents=(Int_t)(t->GetTree()->GetEntries());
}

//////////////////////////////////////////////////////////////////
//
//  TNNControlE 
//  tool to plot output error for training and validation
//  in a canvas
//
//////////////////////////////////////////////////////////////////


ClassImp(TNNControlE)


TNNControlE::TNNControlE():TCanvas("NN Errors","NN Errors",10,10,550,430)
{ 
// constructor  
  SetFillColor(41);
  SetGridx();
  SetGridy();
  GetFrame()->SetFillColor(21);
  GetFrame()->SetBorderSize(12);
  
  fXT = new Float_t[50];
  fYT = new Float_t[50];
  fXV = new Float_t[50];
  fYV = new Float_t[50];
  
  fGraphT = new TGraph();
  fGraphT->SetFillColor(19);
  fGraphT->SetLineColor(3);
  fGraphT->SetLineWidth(4);
  fGraphT->SetMarkerColor(3);
  fGraphT->SetMarkerStyle(21);
  fGraphT->SetTitle("training");

  fGraphV = new TGraph();
  fGraphV->SetFillColor(19);
  fGraphV->SetLineColor(5);
  fGraphV->SetLineWidth(4);
  fGraphV->SetMarkerColor(5);
  fGraphV->SetMarkerStyle(20);
  fGraphV->SetTitle("validation");  
  
  fNT=0;
  fNV=0;
  
}

TNNControlE::~TNNControlE() 
{
// destructor      
  if(fGraphV){fGraphV->Delete();fGraphV=0;}
  if(fGraphT){fGraphT->Delete();fGraphT=0;}  
  delete [] fXT;
  delete [] fYT;
  delete [] fXV;
  delete [] fYV;
}  

void TNNControlE::UpdateG()
{
// update graphs  
  cd();Clear();

  if(!fNT)return;     
  fGraphT->DrawGraph(fNT,fXT,fYT,"ALP");  
  
  if (fNV)
  {
    fGraphV->DrawGraph(fNV,fXV,fYV,"LP");
    DrawT("Validation",0.7,0.7,0.,5);
  }
  DrawT("Training",0.7, 0.8, 0., 3);
  DrawT("Number of cycles",0.35, 0.015, 0., 2);
  DrawT("NN Output Error",0.03,0.35,90.,2);  
  Update(); 
}

void TNNControlE::AddTP(Int_t n,Float_t e) 
{  
// add a point in train curve : n in x and e (error) in y  
  fNT++;
  fXT[fNT-1]=(Float_t)n;fYT[fNT-1]=e;
  if((fNT%50))return;
//  if fNT is a multiple of 50 (50, 100, ...), the array is full and we
//  prepare a bigger one (+50)  
  Int_t i;
  Float_t *x=new Float_t[fNT],*y=new Float_t[fNT];

  for(i=0;i<fNT;i++){x[i]=fXT[i];y[i]=fYT[i];}
  delete [] fXT;fXT = new Float_t[fNT+50];
  delete [] fYT;fYT = new Float_t[fNT+50];
  for(i=0;i<fNT;i++)  {fXT[i]=x[i];fYT[i]=y[i];}  
  
  delete [] x;delete [] y;
}


void TNNControlE::AddVP(Int_t n,Float_t e) 
{  
// add a point in valid curve : n in x and e (error) in y 
  fNV++;
  fXV[fNV-1]=(Float_t)n;fYV[fNV-1]=e;
  if((fNV%50))return;
//  if fNV is a multiple of 50 (50, 100, ...), the array is full and we
//  prepare a bigger one (+50)  
  Int_t i;
  Float_t *x=new Float_t[fNV],*y=new Float_t[fNV];

  for(i=0;i<fNV;i++){x[i]=fXV[i];y[i]=fYV[i];}
  delete [] fXV;fXV = new Float_t[fNV+50];
  delete [] fYV;fYV = new Float_t[fNV+50];
  for(i=0;i<fNV;i++)  {fXV[i]=x[i];fYV[i]=y[i];}  
  
  delete [] x;delete [] y;
}

//////////////////////////////////////////////////////////////////
//
//  TNNUtils 
//
//////////////////////////////////////////////////////////////////

ClassImp(TNNUtils)

// destructor
TNNUtils::~TNNUtils() 
{
   //if(fB) delete fB; //AAPSUAIDE
}  


Int_t TNNUtils::UpdateNewBranch()
{
// create or update the new branch for the tree.
// create in case of no branch,
// update if the kernel has changed (different number of output units or 
// different output units).
// this function is private and called by FillNB()
  
  if(!fT){printf("no tree associated!\n");return 0;}
  if(!fK){printf("no kernel associated!\n");return 0;}
  if(!fFName){printf("no file associated to contain the new branch!\n");return 0;}
  if(fOAdr==(ULong_t)(fK->GetOutputAdr())&&fNOut==fK->GetNOutput())return 1;
  
  if(fB) delete fB;
    
  Text_t *varname,*noutunits;
     
  noutunits=new Text_t[4];sprintf(noutunits,"%i",fK->GetNOutput());
  varname=new Text_t[14];strcpy(varname,"");
  strcat(varname,"OutputU[");strcat(varname,noutunits);strcat(varname,"]/F");
  delete [] noutunits;
  fB=(fT->Branch("OutputU",fK->GetOutputAdr(),varname));
  delete [] varname;  
  fB->SetFile(fFName);
  fOAdr=(ULong_t)fK->GetOutputAdr();
  fNOut=fK->GetNOutput();
  return 1;
}
  
           
Int_t TNNUtils::FillNB()
{  
// function to call to fill the new branch.
// The tree, kernel, and formula must have been specified before with
// SetTree(TTree *t), SetKernel(TNNKernel *k), SetFormula(Text_t *form).  
  
  if(!UpdateNewBranch())return 0;
  if(fForm.GetNValues()!=fK->GetNInput()){printf("input mismatch\n");return 0;}

  fB->Reset();
  for(Int_t i=0;i<(Int_t)fT->GetEntries();i++)
  {
    fForm.Find(i,fK->GetInputAdr());
    fK->GoThrough();
    fB->Fill();
  } 
  return 1;
}  


TH1F* TNNUtils::HIntegral(TH1F *hOrig, Int_t efficiency, Text_t *name, Text_t *title)
{
// method to compute the running integral of an histogram :
// if efficiency is set to 1 (default) : for the bin i, the sum is the one of i and next bins
// ---> we compute an efficiency when a cut is applied on the variable (var > cut).
// if efficiency is set to 0  : for the bin i, the sum is the one of i and previous bins
// ---> we compute a rejection  when a cut is applied on the variable (var > cut).
    
  if (!hOrig){printf("No input histo!\n");return 0;}
  Int_t i,nBins;
  nBins=hOrig->GetNbinsX();
  
  TH1F *histo= new TH1F(name,title,nBins,0.,1.);
  if(efficiency)
    for(i=0;i<=nBins+1;i++)histo->SetBinContent(i,hOrig->Integral(i,nBins+1)); 
  else
  {
    for(i=1;i<=nBins+1;i++)histo->SetBinContent(i,hOrig->Integral(0,i-1));
    histo->SetBinContent(0,0);
  }
  return histo;
}  

TGraph* TNNUtils::XY(TH1F *hX, TH1F *hY, Int_t color)
{  
// method to do a graph from 2 histos with the same number of bins :
// we can with this method do rejection-efficiency curves ...
// ex : util.XY(util.HIntegral(h1,1),util.HIntegral(h2,0))->Draw("ALP") 
  
  if (!hX||!hY){printf("Missing input histo!\n");return 0;}
  Int_t nBX=hX->GetNbinsX();
  Int_t nBY=hY->GetNbinsX();
  if(nBX!=nBY){printf("histos don't martch!\n");return 0;}

  Float_t *X=new Float_t[nBX],*Y=new Float_t[nBY];
  for(Int_t i=1;i<=nBX;i++)
  {
    X[i-1]=hX->GetBinContent(i);
    Y[i-1]=hY->GetBinContent(i);
  }    
  
  TGraph* g=new TGraph(nBX,X,Y);
  g->SetLineColor(color);
  g->SetLineWidth(4);
  return g;
  
}

////////////////////////////////////// end //////////////////////////////////////////////
