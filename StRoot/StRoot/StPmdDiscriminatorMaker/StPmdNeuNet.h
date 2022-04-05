/*!
 * \class StPmdNeuNet
 */
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
/*
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
//#ifndef ROOT_TFormula
//#include "TFormula.h"
//#endif
//#ifndef ROOT_TTreeFormula
//#include "TTreeFormula.h"
//#endif
#ifndef ROOT_TCanvas
#include "TCanvas.h"
#endif
*/
#include "TFrame.h"
#include "TStringLong.h"
#include "TFile.h"
#include "TText.h"
#include "TDatime.h"
#include "TRandom.h"
#include "TPad.h"
#include "math.h"
#include "stdlib.h"
#include "Stiostream.h"
#include "StPmdDiscriminatorMaker.h"

////////////////////////////////////////////// the classes /////////////////////////////////////

class StPmdNeuNet : public TNamed
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
//  TNNTree *fValidTree;   // validation tree
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
  Double_t ErrorO();// compute the error between forward propagation and teaching
  void Error(const char*, const char*, ...) const{}//WarnOff
  void FreeVW();
  void ZeroAll();
  void AllocateVW(Int_t nInput, const Text_t *hidden, Int_t nOutput);
  void SetHidden(const Text_t *ttext);
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
  StPmdDiscriminatorMaker * m_DiscMaker;

 public:
  StPmdNeuNet();
  StPmdNeuNet(const Text_t *name, Int_t nInput=5, const Text_t *hidden="6:7:8", Int_t nOutput=4);
  void setDiscMaker(StPmdDiscriminatorMaker*);
  virtual ~StPmdNeuNet(); // destructor
  virtual void SetKernel(Int_t nInput, const Text_t *hidden, Int_t nOutput);
  virtual void SetLearnParam(Double_t learnParam=0.2,Double_t fse=0.,Double_t mu=0.);
  virtual void SetInitParam(Float_t lowerInitWeight=-1., Float_t upperInitWeight=1.);
  virtual void Init();   // init biases and weights
  virtual void PrintS(); // print structure of network
  virtual void Mix();    // mix the events before learning
  virtual Double_t TrainOneCycle();  // one loop on internal events = one cycle
  virtual void ResetCycles(){fNTrainCycles=0;};
  virtual void Export(const Text_t *fileName="exportNN.dat");
  virtual void Import(const Text_t *fileName="exportNN.dat");
  virtual void SetUseBiases(Bool_t trueForUse=1){fUseBiases=(Double_t)trueForUse;};
  virtual void SetRandomSeed(UInt_t seed=0){fRandom.SetSeed(seed);};
  virtual UInt_t GetRandomSeed(){return fRandom.GetSeed();};
  virtual Bool_t IsTrained(){return fNTrainCycles;};
  virtual Int_t GetNTrainCycles(){return fNTrainCycles;};
  virtual Int_t GetNTrainEvents(){return fNTrainEvents;};
  virtual void SetNTrainEvents(Int_t nevt){fNTrainEvents = nevt;};
  virtual Int_t GetNValidEvents(){return fNValidEvents;};
  virtual void SetArraySize(Int_t s=0);
  virtual void FillArray(Int_t,Int_t,Float_t);
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
  virtual void     fillArrayOut(Float_t v,Int_t i,Int_t l){fArrayOut[i][l]=v;};
  virtual Double_t  GoThrough(){Forward();return ErrorO();};
  virtual Float_t  GetSumO()
  {
    Int_t i; Float_t s=0.;
    for(i=0;i<fNUnits[fNHiddL+1];i++)s+=fValues[fNHiddL+1][i];
    return s;
  };

  void PrintTrain()
  {
	  cout<<"Units** "<<fNUnits[fNHiddL+1]<<endl;

    Int_t l;
    for(l=0;l<fNUnits[fNHiddL+1];l++){
	    cout<<"teach "<<fTeach[l]<<"Value "<<fValues[fNHiddL+1][l]<<endl;
    }
  }

  //  virtual void SetTrainTree(TNNTree *t);
//  virtual void SetValidTree(TNNTree *t);
  virtual Double_t Valid();
//  virtual void TrainNCycles(TNNControlE *conte, Int_t period=5, Int_t nCycles=10);
  virtual void TrainNCycles(Int_t nCycles=10);
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

  virtual Double_t ApplyWeights(Float_t*,Float_t*);  // one loop on internal events = one cycle

  ClassDef(StPmdNeuNet,1)

};

inline void StPmdNeuNet::setDiscMaker(StPmdDiscriminatorMaker* disc){m_DiscMaker=disc;}
