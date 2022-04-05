

#include "StPmdNeuNet.h"
#include<StMessMgr.h>

//  StPmdNeuNet 
//  Feed-Forward Neural Network 
//
//////////////////////////////////////////////////////////////////
ClassImp(StPmdNeuNet)

Float_t Teach[200000]={0.};
Float_t Value[200000]={0.};


/// Constructor 
StPmdNeuNet::StPmdNeuNet(const Text_t *name, Int_t nInput, const Text_t *hidden, Int_t nOutput):TNamed(name,"Neural Network")
{
  ZeroAll();
  AllocateVW(nInput,hidden,nOutput);
    
  fUseBiases=1.;
  fLearnParam=0.2;
  fFlatSE=0.;
  fMu=0.;
  fLowerInitWeight=-1.;
  fUpperInitWeight=1.;

  fNTrainEvents=10;   //default value was 0

  fNTrainCycles=100;  //default value was 0

  TDatime temps;
  fRandom.SetSeed(temps.Convert());
  gMessMgr->Info()<<"StPmdNeuNet::StPmdNeuNet: First Random Seed = "<<fRandom.GetSeed();
  gMessMgr->Info()<<"StPmdNeuNet::StPmdNeuNet: Neural Network is created";

}


/// Constructor with no parameter . Purpose ??
StPmdNeuNet::StPmdNeuNet()
{
  ZeroAll();
  fUseBiases=1.;
  fLearnParam=0.2;
  fFlatSE=0.;
  fMu=0.;
  fLowerInitWeight=-1.;
  fUpperInitWeight=1.;
  fNHiddL=0;

  fNTrainEvents=10;
  fNTrainCycles=100;

  TDatime temps;
  fRandom.SetSeed(temps.Convert());
  gMessMgr->Info()<<"StPmdNeuNet::StPmdNeuNet: First Random Seed = "<<fRandom.GetSeed();
}



// Destructor
StPmdNeuNet::~StPmdNeuNet() 
{
  // destructor  
  gMessMgr->Info()<<"StPmdNeuNet::~StPmdNeuNet : we are done ";
  DeleteArray(); 
  FreeVW();
  if(fEventsList) delete [] fEventsList;
}  


/// Zero all arrays later used. Common for both constructors
void StPmdNeuNet::ZeroAll()
{
  fValues     = 0;
  fErrors     = 0;
  fBiases     = 0;
  fNUnits     = 0;
  fW          = 0;

  fArrayIn    = 0;
  fArrayOut   = 0;
  fTeach      = 0;
  fEventsList = 0;

  fDW         = 0;
  fDB         = 0;

}


void StPmdNeuNet::SetHidden(const Text_t *ttext)
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


void StPmdNeuNet::FreeVW()
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

void StPmdNeuNet::AllocateVW(Int_t nInput, const Text_t *hidden, Int_t nOutput)
{
  Int_t i,l;
  
  if(fW){
    gMessMgr->Info()<<"StPmdNeuNet::AllocateVW: free memory first ";
    return;
  }

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

void StPmdNeuNet::SetKernel(Int_t nInput, const Text_t *hidden, Int_t nOutput)
{  
   FreeVW();
   AllocateVW(nInput,hidden,nOutput);
}


/*! 
 * Sets the learning parameters :
 * the main learning parameter is around 0.2 (in ]0,1])
 * fse is for flat spot elimination, with values in [0,0.25], often 0.1
 * mu is for backprop momentum, values in [0,1]
 */
void StPmdNeuNet::SetLearnParam(Double_t learnParam,Double_t fse,Double_t mu)
{
  fLearnParam=fabs(learnParam);
  fFlatSE=fabs(fse);
  fMu=fabs(mu);

    gMessMgr->Info()<<"StPmdNeuNet::AllocateVW: free memory first ";
  if (fLearnParam>1.0)  gMessMgr->Info()<<"StPmdNeuNet::SetLearnParam: Warning : is not an usual value "<<fLearnParam;
  if (fLearnParam==0.0) gMessMgr->Info()<<"StPmdNeuNet::SetLearnParam: Warning : 0 is a stupid value";
  gMessMgr->Info()<<"StPmdNeuNet::SetLearnParam: Learning Parameter set to : "<<fLearnParam;
  gMessMgr->Info()<<"StPmdNeuNet::SetLearnParam: Flat Spot elimination value  set to :"<<fFlatSE;
  gMessMgr->Info()<<"StPmdNeuNet::SetLearnParam: Momentum set to : "<<fMu;
}


/// Sets the initialisation parameters : max and min weights  
void StPmdNeuNet::SetInitParam(Float_t lowerInitWeight, Float_t upperInitWeight)
{
  Float_t temp;

  fLowerInitWeight=lowerInitWeight;
  fUpperInitWeight=upperInitWeight;
  if (fLowerInitWeight>fUpperInitWeight)
  {
    temp=fUpperInitWeight;
    fUpperInitWeight=fLowerInitWeight;
    fLowerInitWeight=temp;
  } 
  if (fLowerInitWeight==fUpperInitWeight)
    gMessMgr->Info()<<"StPmdNeuNet::SetInitParam: Warning : the weights initialisation bounds are equal ";
  gMessMgr->Info()<<"StPmdNeuNet::SetInitParam: Init Parameters set to ";
  gMessMgr->Info()<<"StPmdNeuNet::SetInitParam: --> Lower bound = "<<fLowerInitWeight;
  gMessMgr->Info()<<"StPmdNeuNet::SetInitParam: --> Upper bound = "<<fUpperInitWeight;

}


Float_t StPmdNeuNet::Alea()
{
  return fLowerInitWeight+fRandom.Rndm()*(fUpperInitWeight-fLowerInitWeight);
}



/*! initialisation of  biases and weights.  
 * the init parameters can be changed by :
 * SetInitParam(Float_t lowerInitWeight, Float_t upperInitWeight)
 * The default is -1 and 1
 */
void StPmdNeuNet::Init()
{  
  Int_t i,l,c;
  
  if(!fW){gMessMgr->Info()<<"StPmdNeuNet::Init: allocate memory first";return;}
  
  // init of weights
  
  for (i=0;i<fNHiddL+1;i++)
    for (l=0;l<fNUnits[i];l++)
      for (c=0;c<fNUnits[i+1];c++) fW[i][l][c]=(Double_t)Alea();

  for(i=0;i<fNHiddL+1;i++)for(l=0;l<fNUnits[i];l++)for(c=0;c<fNUnits[i+1];c++)
      fDW[i][l][c]=0.;       
  
  // init of biases
  
  for (i=1;i<fNHiddL+2;i++)
    for (l=0;l<fNUnits[i];l++) fBiases[i][l]=(Double_t)(Alea())*fUseBiases;

  for(i=1;i<fNHiddL+2;i++)for(l=0;l<fNUnits[i];l++)fDB[i][l]=0.;


  fNTrainCycles=0;
  gMessMgr->Info()<<"StPmdNeuNet::Init: Initialisation done";
}


/// prints structure of network on screen
void StPmdNeuNet::PrintS()
{
  Int_t i,l,c;
  
  if(!fW){gMessMgr->Info()<<"StPmdNeuNet::PrintS: no unit ";return;} 
  
  gMessMgr->Info()<<"StPmdNeuNet::PrintS: +++++++++ Neural Network ++++++++++++"<<GetName();
  for(i=0;i<fNHiddL+2;i++)gMessMgr->Info()<<"StPmdNeuNet::PrintS: Layer contains units"<<i<<" "<<fNUnits[i];

  if(fUseBiases)gMessMgr->Info()<<"StPmdNeuNet::PrintS: >>>>>>> Biases USED";
  else          gMessMgr->Info()<<"StPmdNeuNet::PrintS: >>>>>>>Biases DUMMY";

  gMessMgr->Info()<<"StPmdNeuNet::PrintS: ----------   Biases   ---------- ";
  Int_t maxl=0;
  for(i=0;i<fNHiddL+2;i++)if(fNUnits[i]>=maxl)maxl=fNUnits[i];
  for(i=0;i<fNHiddL+2;i++)gMessMgr->Info()<<"      | "<<i;
  for(i=0;i<fNHiddL+2;i++)gMessMgr->Info()<<"--------|-";
  for(l=0;l<maxl;l++)
  {
    for(i=0;i<fNHiddL+2;i++)
      if(l<fNUnits[i])gMessMgr->Info()<<"StPmdNeuNet::PrintS:  | "<<fBiases[i][l];
      else gMessMgr->Info()<<"        | "; 
  }


  gMessMgr->Info()<<"StPmdNeuNet::PrintS:    ----------   Weights ----------- ";
  for(i=0;i<fNHiddL+1;i++)
  {
    gMessMgr->Info()<<"StPmdNeuNet::PrintS:  From  "<<i<<" to " <<i+1;
   gMessMgr->Info()<<"StPmdNeuNet::PrintS: "<<i;for(l=0;l<fNUnits[i];l++)gMessMgr->Info()<<"  |"<<l;
    gMessMgr->Info()<<"StPmdNeuNet::PrintS: ===|";for(l=0;l<fNUnits[i];l++)gMessMgr->Info()<<"-------";
     gMessMgr->Info()<<"StPmdNeuNet::PrintS:  |"<<i+1; for(l=0;l<fNUnits[i];l++)gMessMgr->Info()<<"-------";
    for(c=0;c<fNUnits[i+1];c++)
    { 
       gMessMgr->Info()<<"StPmdNeuNet::PrintS: |"<<c;
       for(l=0;l<fNUnits[i];l++)gMessMgr->Info()<<"|"<<fW[i][l][c];
    }     
  }  

  gMessMgr->Info()<<"StPmdNeuNet::PrintS: Learning parameter = "<<fLearnParam;
 gMessMgr->Info()<<"StPmdNeuNet::PrintS: Flat Spot elimination value = "<<fFlatSE;
  gMessMgr->Info()<<"StPmdNeuNet::PrintS: Momentum = "<<fMu;
  gMessMgr->Info()<<"StPmdNeuNet::PrintS: Lower initialisation weight = "<<fLowerInitWeight;
  gMessMgr->Info()<<"StPmdNeuNet::PrintS: Upper initialisation weight = "<<fUpperInitWeight;
  gMessMgr->Info()<<"StPmdNeuNet::PrintS: Number of events for training   = "<<fNTrainEvents;
  gMessMgr->Info()<<"StPmdNeuNet::PrintS: Number of events for validation = "<<fNValidEvents;
  gMessMgr->Info()<<"StPmdNeuNet::PrintS: Number of cycles done = "<<fNTrainCycles;
  gMessMgr->Info()<<"StPmdNeuNet::PrintS: +++++++++++++++++++++++++++++++++++++++++++++++";

}

/*! 
 * general function to propagate the input activation 
 * The input activation array must be filled  
 */
void StPmdNeuNet::Forward()
{
  Int_t i,l,c;
  Double_t sum;
  //  cout<<"Valid forward called "<<endl;
  if(!fW){ 
    gMessMgr->Info()<<"StPmdNeuNet::Forward no unit !";
    return;
  }  
  
  for (i=0;i<fNHiddL+1;i++)  
    for (c=0;c<fNUnits[i+1];c++)
      {
	sum=0.; 
	for(l=0;l<fNUnits[i];l++)sum+=fW[i][l][c]*(Double_t)fValues[i][l];
	fValues[i+1][c]=(Float_t)Sigmoide(sum+fBiases[i+1][c]*fUseBiases);
      }
}



/// gradient retropropagation (updates of biases and weights)  
void StPmdNeuNet::LearnBackward()
{
  if(fNTrainEvents<1){gMessMgr->Info()<<"StPmdNeuNet::LearnBackward: No event to train !!!";return;}
  if(!fW){gMessMgr->Info()<<"StPmdNeuNet::LearnBackward: no unit !";return;}

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


/*!
 * function to compute the errors between forward propagation and teaching.  
 * this error is = |teaching-computed| summed on NN outputs and divided by their number.  
 */
Double_t StPmdNeuNet::Error()
{

  Int_t i,l,c;
  Double_t sum,error=0,errorOneUnit;
  if(!fW){gMessMgr->Info()<<"StPmdNeuNet::Error: no unit !";return 0;}    
  
  //  Error on Output Units

  for(l=0;l<fNUnits[fNHiddL+1];l++)
  {
    errorOneUnit=(Double_t)(fTeach[l]-fValues[fNHiddL+1][l]);
    //    cout<<"teach "<<fTeach[l]<<"Value "<<fValues[fNHiddL+1][l]<<endl;

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


/*!
 * function to compute the errors between forward propagation and teaching.  
 * this error is = |teaching-computed| summed on NN outputs and divided by their number.  
 *  Error on Output Units
 */
Double_t StPmdNeuNet::ErrorO()
{

  
//  cout<<"Error0 called "<<endl;
  Int_t l;
  Double_t error=0;
  if(!fW){gMessMgr->Info()<<"StPmdNeuNet::ErrorO: no unit !";return 0;}    
  for(l=0;l<fNUnits[fNHiddL+1];l++)
    error+=fabs((Double_t)(fTeach[l]-fValues[fNHiddL+1][l]));
  error=error/(Double_t)fNUnits[fNHiddL+1];  
  return error;
  
}  


/*!
 * one loop on internal events = one cycle.  
 * takes each event from internal array in an order fixed by an array ( fEventsList ).
 * It is necessary to call the method Mix() before each call to this function
 * in order to change the presentation order.
 * The learning is done by this function.
 * The private variable  fNTrainCycles is incremented.
 */
Double_t StPmdNeuNet::TrainOneCycle()
{
  if(fNTrainEvents<1){gMessMgr->Info()<<"StPmdNeuNet::TrainOneCycle: No event to train !!!";return 0.;}
  if(!fW){gMessMgr->Info()<<"StPmdNeuNet::TrainOneCycle: no unit !";return 0.;}


  Int_t i;
  Double_t error=0.;

  for(i=0;i<fNTrainEvents;i++)
  {  
    GetArrayEvt(fEventsList[i]); 
    Forward();
    for(Int_t l=0;l<fNUnits[fNHiddL+1];l++)
      {
	Teach[i]=fTeach[l];
	Value[i]=fValues[fNHiddL+1][l];
	
	//  cout<<"evt  "<<i<<"teach **"<<fTeach[l]<<"favle "<<fValues[fNHiddL+1][l]<<endl;
      }

    error+=Error();
    LearnBackward();
  

  }
 
  fNTrainCycles++;
  error=error/(Double_t)fNTrainEvents;
  gMessMgr->Info()<<"StPmdNeuNet::TrainOneCycle: cycle  : E_t =  "<<fNTrainCycles<<" "<<error;

  return error;
}


/*!
 * one loop on valid events.  
 * takes each event from validation tree.
 * the events are passed trough the kernel, and a mean output
 * error is computed.
 */
Double_t StPmdNeuNet::Valid()
{
  if(fNValidEvents<1) return 0.;
 

  // we will now pass all the validation events through the kernel, and
  // compute the mean error on output 
  Double_t error=0.;

  for (Int_t j=0;j<fNValidEvents;j++)
    {
      error+=GoThrough(); // forward propagation and error on one event	
    }
  error=error/(Double_t)fNValidEvents; // mean
  return error;
}


/*!
 * method to train on N cycles, with mixing and plot of errors
 * on the controller conte.
 */
void StPmdNeuNet::TrainNCycles(Int_t nCycles)
{
  //sub  if(!conte){gMessMgr->Info()<<"no controller !";return;}
  Float_t errt,errv;
  for(Int_t i=0;i<nCycles;i++)
  {
    Mix();
    errt=(Float_t)TrainOneCycle();
    errv=(Float_t)Valid();
    gMessMgr->Info()<<"StPmdNeuNet::TrainNCycles: cycle  > train : "<<fNTrainCycles<<" "<<errt;
    if(fNValidEvents)gMessMgr->Info()<<"StPmdNeuNet::TrainNCycles: and valid : ";
    else gMessMgr->Info()<<("  ");

  }
  
}


/*! 
 * Put the structure in a file
 * WARNING : the weights and biases are stored with 4 digits
 * in decimal part.      
 * Learning parameters are not stored
 */
void StPmdNeuNet::Export(const Text_t *fileName)
{
  Int_t i,l,c;
  
  if(!fW){gMessMgr->Info()<<"StPmdNeuNet::Export: no unit !";return;} 
  
  FILE  *file=0;
  file = fopen(fileName,"w");
  if ( ! file){
    gMessMgr->Info()<<"StPmdNeuNet::Export: ERROR Cannot open  for write  "<<fileName;
    return;
  }

  fprintf(file,"%8i\n",fNTrainEvents);
  for(l=0;l<fNTrainEvents;l++)fprintf(file,"%8.4f %8.4f\n",Teach[l],Value[l]);

  //////////////////////
  m_DiscMaker->mNNoutput->Fill(Value[l]);

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


/*!
 * Get the structure from a file
 * WARNING : the weights and biases are stored with 4 digits
 * in decimal part.
 * Learning parameteres are not stored.  
 */
void StPmdNeuNet::Import(const Text_t *fileName)
{
  Int_t i,l,c,newI,newHL,newO;
  Text_t hidden[100],piece[5];
  FILE *file=0;
  file = fopen(fileName,"r");
  
  if ( ! file){
    gMessMgr->Info()<<"StPmdNeuNet::Import: ERROR Cannot open  for read"<<fileName;
    return;
  }

  fscanf(file,"%3i",&newHL);
  fscanf(file,"%3i",&newI); 
  strcpy(hidden,"");
  for(i=1;i<newHL;i++)
    {fscanf(file,"%s",piece);strcat(hidden,piece);strcat(hidden,":");} 
  fscanf(file,"%s",piece);strcat(hidden,piece);
  fscanf(file,"%3i",&newO); 
  
  gMessMgr->Info()<<"StPmdNeuNet::Import: New NN set to : "<<newI<<" "<<hidden<<" "<<newO;
  FreeVW();			  

  gMessMgr->Info()<<"StPmdNeuNet::Import: Allocating";
  AllocateVW(newI,hidden,newO);

  gMessMgr->Info()<<"StPmdNeuNet::Import: Filling fDB+fscanf()";
  Float_t tmpfl;
  for(i=0;i<fNHiddL+2;i++)
    for(l=0;l<fNUnits[i];l++){fDB[i][l]=0.;fscanf(file,"%f",&tmpfl);*(fBiases[i]+l)=(Double_t)tmpfl;}

  for(i=0;i<fNHiddL+1;i++)
    for(l=0;l<fNUnits[i];l++)
      for(c=0;c<fNUnits[i+1];c++){
	fDW[i][l][c]=0.;fscanf(file,"%f",&tmpfl);*(fW[i][l]+c)=(Double_t)tmpfl;
	//      cout<<"Nhidd "<<i<<"Nunit "<<l<<"unit_next "<<c<<"wei "<<fW[i][l][c]<<endl;     
      }
  

  fscanf(file,"%5i",&fNTrainCycles);  
  fscanf(file,"%f",&tmpfl);fUseBiases=(Double_t)tmpfl;  
  fclose(file);   
  gMessMgr->Info()<<"StPmdNeuNet::Import: Done";
}


/*! mix the events before learning. VERY IMPORTANT.
 * is has to be used before  TrainOneCycle() , 
 * IT IS NOT used by TrainOneCycle() , you have to do the call yourself
 */
void StPmdNeuNet::Mix()
{
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


void StPmdNeuNet::SetArraySize(Int_t size)
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

void StPmdNeuNet::DeleteArray()
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

/*
void StPmdNeuNet::SetTrainTree(TNNTree *t)
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

void StPmdNeuNet::SetValidTree(TNNTree *t)
{
// method to associate a TNNTree to the kernel :
// a link will be done between the tree and the kernel.
// it is not necessary to keep these events in the kernel

  if(!t){printf("no tree !\n");return;}
  fValidTree=t;
  fNValidEvents=(Int_t)(t->GetTree()->GetEntries());
}

*/

void StPmdNeuNet::FillArray(Int_t iev,Int_t iunit,Float_t value)
{
  //cout<<"inside fillarray**"<<iev<<" "<<iunit<<" "<<value<<endl;

  fArrayIn[iev][iunit]=value;
}



/*!
 * one loop on internal events = one cycle.  
 * takes each event from internal array in an order fixed by an array ( fEventsList ).
 * It is necessary to call the method Mix() before each call to this function
 * in order to change the presentation order.
 * The learning is done by this function.
 * The private variable  fNTrainCycles is incremented.
 */
Double_t StPmdNeuNet::ApplyWeights(Float_t *Teach,Float_t *Value)
{
  if(fNTrainEvents<1){gMessMgr->Info()<<"StPmdNeuNet::ApplyWeights: No event to train !!!";return 0.;}
  if(!fW){gMessMgr->Info()<<"StPmdNeuNet::ApplyWeights: no unit !";return 0.;}
  FILE *file1;
  file1=fopen("testout","w");

  Int_t i;
  Double_t error=0.;

  for(i=0;i<fNTrainEvents;i++)
  {  
    GetArrayEvt(fEventsList[i]); 
    Forward();
    for(Int_t l=0;l<fNUnits[fNHiddL+1];l++)
      {
	Teach[i]=fTeach[l];
	Value[i]=fValues[fNHiddL+1][l];
	
	
	//	  cout<<"evt  "<<i<<"teach **"<<fTeach[l]<<"favle "<<fValues[fNHiddL+1][l]<<endl;
      }
    fprintf(file1,"%d %8.4f %8.4f\n",i,Teach[i],Value[i]);

    error+=Error();
    //    LearnBackward();
  
    
  }
 
  fNTrainCycles++;
  error=error/(Double_t)fNTrainEvents;
  // printf("cycle %i : E_t = %6.4f ",fNTrainCycles,error);

  return error;
}
