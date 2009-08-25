//////////////////////////////////////////////////////////////////
//
//  Neural Network classes :
//  TNNFormula 
//  TNNTree
//  TNNKernel
//  TNNControlE
//  TNNUtils
//                             J.P. Ernenwein (rnenwein@in2p3.fr)
//////////////////////////////////////////////////////////////////

////////////// some include files ... /////////////////////////////
#ifndef ROOT_TNamed
#include "TNamed.h"
#endif
#ifndef ROOT_TROOT
#include "TROOT.h"
#endif
#ifndef ROOT_TTree
#include "TTree.h"
#endif
#ifndef ROOT_TString
#include "TString.h"
#endif
#ifndef ROOT_TFormula
#include "TFormula.h"
#endif
#ifndef ROOT_TTreeFormula
#include "TTreeFormula.h"
#endif
#ifndef ROOT_TCanvas
#include "TCanvas.h"
#endif
#include "TFrame.h"
#include "TStringLong.h"
#include "TH1.h"
#include "TGraph.h"
#include "TAxis.h"
#include "TFile.h"
#include "TText.h"
#include "TDatime.h"
#include "TRandom.h"
#include "TPad.h"
#include "math.h"
#include "stdlib.h"


////////////////////////////////////////////// the classes /////////////////////////////////////

class TNNFormula : public TNamed 
{

 private:
  Int_t  fNValues;        // number of values
  TTree *fTree;           // current Tree on which values are computed
  TTreeFormula *fTTCut;   // Tree formula containing the cut expression
  TTreeFormula **fTTFormula; //! array of TTreeFormula for input units
  Bool_t fClip; // flag to clip or not the values (1 = clip, 0 = no clip)
  TStringLong* fFormula; //! formula stored for refresh purpose
  TStringLong* fCut; //! cut formula stored for refresh purpose
  Bool_t fRefresh; // refresh flag
  Int_t RMBlanks(Text_t *str);
  Float_t Clip(Float_t x);

 public:
  TNNFormula(){fNValues=0;fClip=1;fTree=0;fTTCut=0;fTTFormula=0;fFormula=0;fCut=0;fRefresh=0;};
  TNNFormula(Text_t *name, Text_t *formula, Text_t *cut,  TTree *tree);
  virtual ~TNNFormula();
  virtual Bool_t Find(Int_t iEvent, Float_t *values);  
  virtual void  Find(Int_t iEvent=0);
  virtual Int_t GetNValues(){return fNValues;};
  virtual void SetTree(TTree *tree); 
  virtual void SetFormula(Text_t *formula); 
  virtual void Refresh()
  {
    fRefresh=1;
    if(fFormula)SetFormula((Text_t*)(fFormula->Data()));
    if(fCut)SetCut((Text_t*)(fCut->Data()));
    fRefresh=0;
  };
  virtual Int_t Length(){if(fFormula)return fFormula->Length();else return 0;};
  virtual void SetClip(Bool_t trueForClip=1);  
  virtual void SetCut(Text_t *cutarg=""); 


  ClassDef(TNNFormula,0)

};

///////////////////////////////////////////////////////////////////////////////

class TNNTree : public TNamed
{

 private:

  TTree *fTree;    // Tree 
  Int_t fNTrees;   // number of TTrees in fTree
  TFile *fFile;   //  fTree File
  TNNFormula fFormula; // Input Formula
  TNNFormula fOutFormula; // Output Formula
  Text_t **fInfos; //! array of informations 
  Text_t *fFName;  //! name of the fTree File
  Float_t *fInput; //! array of input values (first branch)
  Float_t *fOutput; //! array of output values  (second branch)  
  Int_t fNInput; // number of input values
  Int_t fNOutput; // number of output values

  void CheckRange(Int_t *begin, Int_t *end, Int_t indexMax);
  Int_t RMBlanks(Text_t *str);
  void CreateTree();
  Int_t NumberOut(Text_t *ttext);
  void Decode(Text_t *ttext);
  
  virtual void RefreshInFormula(){fFormula.Refresh();};
  virtual void RefreshOutFormula(){fOutFormula.Refresh();};
    
 public:
  TNNTree()
    {fTree=0;fNTrees=0;fFile=0;fInfos=0;fFName=0;fInput=0;fOutput=0;fNInput=0;fNOutput=0;};
  TNNTree(Text_t *name); 
  virtual ~TNNTree(); 
  virtual void AddTree(TTree *tree, Int_t begin=0, Int_t end=1000000);
  virtual void AddTree(TTree *tree, Text_t *out, Int_t begin=0, Int_t end=1000000);
  virtual void Infos();
  virtual TTree* GetTree(){return fTree;};
  virtual void SetFormulaTree(TTree *tree)
  {
     fFormula.SetTree(tree);
     fOutFormula.SetTree(tree);
  };
  virtual void SetInFormula(Text_t *formula){fFormula.SetFormula(formula);};
  virtual void SetOutFormula(Text_t *formula){fOutFormula.SetFormula(formula);};
  virtual void SetCut(Text_t *cut=""){fFormula.SetCut(cut);};
  virtual void SetClip(Bool_t trueForClip=1){fFormula.SetClip(trueForClip);};
  virtual void SetOutClip(Bool_t trueForClip=1){fOutFormula.SetClip(trueForClip);};
  virtual void GetEvent(Float_t *input, Float_t *output, Int_t iEvent=0);  
  virtual Int_t GetNInput(){return fNInput;};
  virtual Int_t GetNOutput(){return fNOutput;};
  virtual void SetFile(Text_t *namearg);
  virtual void DeleteTree();
  
  ClassDef(TNNTree,0)

};

////////////////////////////////////////////////////////////////////////////////////

class TNNControlE : public TCanvas
{

 private:
  Float_t * fXT; //! array of train errors
  Float_t * fYT; //!
  Float_t * fXV; //! array of valid errors
  Float_t * fYV; //!
  Int_t  fNT;  // number of components of train array
  Int_t  fNV;  // number of components of valid array
  TGraph *fGraphV; // graph for train
  TGraph *fGraphT; // graph for valid

  
 public:
  TNNControlE();
  virtual ~TNNControlE(); // destructor
  virtual void AddTP(Int_t n, Float_t e);
  virtual void AddVP(Int_t n, Float_t e);
  virtual void UpdateG();
  virtual void ResetT()
    {delete [] fXT;delete [] fYT;fXT=new Float_t[50];fYT=new Float_t[50];fNT=0;};
  virtual void ResetV()
    {delete [] fXV;delete [] fYV;fXV=new Float_t[50];fYV=new Float_t[50];fNV=0;};
  virtual Float_t* GetXT(){return fXT;};
  virtual Float_t* GetYT(){return fYT;};
  virtual Float_t* GetXV(){return fXV;};
  virtual Float_t* GetYV(){return fYV;};  
  virtual Int_t GetNT(){return fNT;};
  virtual Int_t GetNV(){return fNV;};    
  virtual void DrawT(const Text_t *text, Float_t x, Float_t y, Float_t angle=0., Int_t color=1)
  {
    TText *tText= new TText(x,y,text);
    tText->SetNDC(kTRUE);
    tText->SetTextColor(color);
    tText->SetTextAngle(angle);
    tText->Draw();
  }

  ClassDef(TNNControlE,0)

};

/////////////////////////////////////////////////////////////////////////////////////

class TNNKernel : public TNamed 
{

 private:
  Int_t  fNHiddL;  // number of hidden layers
  Float_t **fValues;     //! array of activations 
  Double_t **fErrors;    //! array of errors
  Double_t **fBiases;    //! array of biases
  Int_t *fNUnits;    //! array of units numbers
  Double_t ***fW;    //! array of weights
   
  Int_t  fNTrainEvents;  // number of events for training 
  Int_t  fNValidEvents;  // number of events for validation
  TNNTree *fValidTree;   // validation tree 
  Double_t fLearnParam;   // learning parameter
  Float_t fLowerInitWeight;  // minimum weight for initialisation
  Float_t fUpperInitWeight;  // maximum weight for initialisation
  Float_t **fArrayOut;  //! Internal Array with teaching  values for output units
  Float_t *fTeach; //! array of teaching values for outputs
  Float_t **fArrayIn; //! Internal Array with values of input units
  Int_t *fEventsList; //! array of events numbers, for mixing
  Int_t fNTrainCycles; // Number of training cycles done
  Double_t fUseBiases;  // flag for use of biases or not (1=use, 0=no use)
  TRandom fRandom; // Random object used in initialisation and mixing
  Int_t fNWeights; // number of weights in neural network
  Double_t fMu; // backpropagation momentum parameter
  Double_t fFlatSE; // Flat Spot elimination paramater
  Double_t ***fDW;  //! array of delta weights used by backprop momentum
  Double_t **fDB;   //! array of delta biases  used by backprop momentum
  
  void GetArrayEvt(Int_t iEvent)
  {
    Int_t l;      
    for(l=0;l<fNUnits[0];l++)fValues[0][l]=fArrayIn[iEvent][l];
    for(l=0;l<fNUnits[fNHiddL+1];l++)fTeach[l]=fArrayOut[iEvent][l];
  };
  void LearnBackward();   // gradient retropropagation (updates of biases and weights)
  void Forward(); // do a simple forward propagation
  Double_t Error();// compute the error between forward propagation and teaching
  void Error(const char*,const char*,...) const {;}//WarnOff
  Double_t ErrorO();// compute the error between forward propagation and teaching
  void FreeVW();  
  void AllocateVW(Int_t nInput, Text_t *hidden, Int_t nOutput);  
  void SetHidden(Text_t *ttext); 
  Float_t Alea();  
  void DeleteArray();  
  
 protected: 
  virtual Double_t Sigmoide(Double_t x)
  {  
    if(x> 10.) return 0.99999; // probability MUST be < 1
    if(x<-10.) return 0.;
    return (1./(1.+exp(-x)));
  };
  virtual Double_t SigPrim(Double_t x){return (x*(1.-x));};   
  
 public:
  TNNKernel();
  TNNKernel(Text_t *name, Int_t nInput=5, Text_t *hidden="6:7:8", Int_t nOutput=4); 
  virtual ~TNNKernel(); // destructor
  virtual void SetKernel(Int_t nInput, Text_t *hidden, Int_t nOutput);
  virtual void SetLearnParam(Double_t learnParam=0.2,Double_t fse=0.,Double_t mu=0.);
  virtual void SetInitParam(Float_t lowerInitWeight=-1., Float_t upperInitWeight=1.);
  virtual void Init();   // init biases and weights
  virtual void PrintS(); // print structure of network
  virtual void Mix();    // mix the events before learning
  virtual Double_t TrainOneCycle();  // one loop on internal events = one cycle
  virtual void ResetCycles(){fNTrainCycles=0;};
  virtual void Export(Text_t *fileName="exportNN.dat");
  virtual void Import(Text_t *fileName="exportNN.dat");  
  virtual void SetUseBiases(Bool_t trueForUse=1){fUseBiases=(Double_t)trueForUse;};
  virtual void SetRandomSeed(UInt_t seed=0){fRandom.SetSeed(seed);};
  virtual UInt_t GetRandomSeed(){return fRandom.GetSeed();};    
  virtual Bool_t IsTrained(){return fNTrainCycles;};
  virtual Int_t GetNTrainCycles(){return fNTrainCycles;};
  virtual Int_t GetNTrainEvents(){return fNTrainEvents;};
  virtual Int_t GetNValidEvents(){return fNValidEvents;};
  virtual void SetArraySize(Int_t s=0);
  virtual void Fill(Int_t iev=0)
  {
    Int_t i;
    for(i=0;i<fNUnits[0];i++)fArrayIn[iev][i]=fValues[0][i];
    for(i=0;i<fNUnits[fNHiddL+1];i++)fArrayOut[iev][i]=fTeach[i];
  }
  virtual Float_t* GetInputAdr(){return fValues[0];};
  virtual void     SetInput(Float_t v,Int_t i){fValues[0][i]=v;};
  virtual Int_t    GetNInput(){return fNUnits[0];};
  virtual Int_t    GetNOutput(){return fNUnits[fNHiddL+1];};
  virtual Float_t  GetOutput(Int_t unit=0){return fValues[fNHiddL+1][unit];};
  virtual Float_t* GetOutputAdr(){return fValues[fNHiddL+1];};
  virtual Float_t* GetTeachAdr(){return fTeach;};
  virtual void     SetTeach(Float_t v,Int_t i){fTeach[i]=v;};
  virtual Double_t  GoThrough(){Forward();return ErrorO();};
  virtual Float_t  GetSumO()
  {
    Int_t i; Float_t s=0.;
    for(i=0;i<fNUnits[fNHiddL+1];i++)s+=fValues[fNHiddL+1][i];
    return s;
  };
  virtual void SetTrainTree(TNNTree *t);  
  virtual void SetValidTree(TNNTree *t);  
  virtual Double_t Valid();
  virtual void TrainNCycles(TNNControlE *conte, Int_t period=5, Int_t nCycles=10);
  virtual Int_t GetNWeights()
  {
     if(!fNUnits)return 0;
     Int_t n=0;
     for(Int_t i=0;i<fNHiddL+1;i++)
     {
       n+=fNUnits[i]*fNUnits[i+1];
     }
     return n;
  };

  ClassDef(TNNKernel,0)

};

///////////////////////////////////////////////////////////////////////////////////


class TNNUtils : public TNamed
{

 private:
  TTree *fT;       // TTree associated 
  TNNKernel *fK;   // associated kernel
  Text_t fFName[400];  // file name for the new branch
  TFile fTF;       // Tfile for the new branch
  TBranch *fB;     // new branch
  ULong_t fOAdr;   // adress of output units of the kernel
  Int_t  fNOut;    // number of output units of the kernel
  TNNFormula fForm;// formula to compute output
  
  Int_t UpdateNewBranch();
  
 public:
  TNNUtils() {fK=0;fNOut=0;fT=0;fB=0;};  
  virtual ~TNNUtils(); // destructor
  virtual void SetTree(TTree *t){fT=t;};
  virtual void SetKernel(TNNKernel *k){fK=k;};
  virtual void SetNewBranchFile(Text_t *fname){strcpy(fFName,fname);};
  virtual void SetFormula(Text_t *form, Bool_t clip=1)
  {
     if(!fT){printf("no tree associated!\n");return;}
     fForm.SetTree(fT);fForm.SetFormula(form);fForm.SetClip(clip);
  };
  virtual Int_t FillNB();
  virtual TH1F* HIntegral(TH1F *hOrig, Int_t efficiency=1, Text_t *name="Integral", Text_t *title="Integral");
  virtual TGraph* XY(TH1F *hX, TH1F *hY, Int_t color=1);
  ClassDef(TNNUtils,0)

};

////////////////////////////////////// end ///////////////////////////////////////

