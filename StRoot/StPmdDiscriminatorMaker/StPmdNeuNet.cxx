

#include "StPmdNeuNet.h"

//  StPmdNeuNet 
//  Feed-Forward Neural Network 
//
//////////////////////////////////////////////////////////////////
ClassImp(StPmdNeuNet)

	Float_t Teach[200000]={0.};
	Float_t Value[200000]={0.};


StPmdNeuNet::StPmdNeuNet(Text_t *name, Int_t nInput, Text_t *hidden, Int_t nOutput):TNamed(name,"Neural Network")
{
// constructor
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
  printf("First Random Seed = %i\n",fRandom.GetSeed());
  printf("Neural Network is created : \n");
//  PrintS();

}

StPmdNeuNet::StPmdNeuNet()
{
// constructor witn no parameter 
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
  printf("First Random Seed = %i\n",fRandom.GetSeed());
}


StPmdNeuNet::~StPmdNeuNet() 
{
// destructor  
  
  DeleteArray();
  FreeVW();
  if(fEventsList) delete [] fEventsList;
}  


void StPmdNeuNet::SetHidden(Text_t *ttext)
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

void StPmdNeuNet::AllocateVW(Int_t nInput, Text_t *hidden, Int_t nOutput)
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

void StPmdNeuNet::SetKernel(Int_t nInput, Text_t *hidden, Int_t nOutput)
{  
   FreeVW();
   AllocateVW(nInput,hidden,nOutput);
}

void StPmdNeuNet::SetLearnParam(Double_t learnParam,Double_t fse,Double_t mu)
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
 
void StPmdNeuNet::SetInitParam(Float_t lowerInitWeight, Float_t upperInitWeight)
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


Float_t StPmdNeuNet::Alea()
{
  return fLowerInitWeight+fRandom.Rndm()*(fUpperInitWeight-fLowerInitWeight);
}

void StPmdNeuNet::Init()
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
  
  for (i=1;i<fNHiddL+2;i++)
    for (l=0;l<fNUnits[i];l++) fBiases[i][l]=(Double_t)(Alea())*fUseBiases;

  for(i=1;i<fNHiddL+2;i++)for(l=0;l<fNUnits[i];l++)fDB[i][l]=0.;


  fNTrainCycles=0;
  printf("Initialisation done\n");
}

void StPmdNeuNet::PrintS()
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

void StPmdNeuNet::Forward()
{
// general function to propagate the input activation 
//  The input activation array must be filled  
  Int_t i,l,c;
  Double_t sum;
  //  cout<<"Valid forward called "<<endl;
  if(!fW){printf("no unit !\n");return;}  
  
for (i=0;i<fNHiddL+1;i++)  
    for (c=0;c<fNUnits[i+1];c++)
    {
      sum=0.; 
      for(l=0;l<fNUnits[i];l++)sum+=fW[i][l][c]*(Double_t)fValues[i][l];
      fValues[i+1][c]=(Float_t)Sigmoide(sum+fBiases[i+1][c]*fUseBiases);
    }
}

void StPmdNeuNet::LearnBackward()
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

Double_t StPmdNeuNet::Error()
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

Double_t StPmdNeuNet::ErrorO()
{
// function to compute the errors between forward propagation and teaching.  
// this error is = |teaching-computed| summed on NN outputs and divided by their number.  
//  Error on Output Units
  
//  cout<<"Error0 called "<<endl;
  Int_t l;
  Double_t error=0;
  if(!fW){printf("no unit !\n");return 0;}    
  for(l=0;l<fNUnits[fNHiddL+1];l++)
    error+=fabs((Double_t)(fTeach[l]-fValues[fNHiddL+1][l]));
  error=error/(Double_t)fNUnits[fNHiddL+1];  
  return error;
  
}  

Double_t StPmdNeuNet::TrainOneCycle()
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
  printf("cycle %i : E_t = %6.4f ",fNTrainCycles,error);

  return error;
}

Double_t StPmdNeuNet::Valid()
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
      error+=GoThrough(); // forward propagation and error on one event	
    }
  error=error/(Double_t)fNValidEvents; // mean
  return error;
}


void StPmdNeuNet::TrainNCycles(Int_t nCycles)
{
// method to train on N cycles, with mixing and plot of errors
// on the controller conte.

//sub  if(!conte){printf("no controller !\n");return;}
  Float_t errt,errv;
  for(Int_t i=0;i<nCycles;i++)
  {
    Mix();
    errt=(Float_t)TrainOneCycle();
    errv=(Float_t)Valid();
    printf("cycle %3i > train : %7.3f",fNTrainCycles,errt);
    if(fNValidEvents)printf(" and valid : %7.3f \n",errv);else printf("\n");

  }
  
}

void StPmdNeuNet::Export(Text_t *fileName)
{
// Put the structure in a file
// WARNING : the weights and biases are stored with 4 digits
// in decimal part.    
// Learning parameters are not stored
  Int_t i,l,c;
  
  if(!fW){printf("no unit !\n");return;} 
  
  FILE *file;
  file=fopen(fileName,"w");

    fprintf(file,"%8i\n",fNTrainEvents);
    for(l=0;l<fNTrainEvents;l++)fprintf(file,"%8.4f %8.4f\n",Teach[l],Value[l]);

    //////////////////////
    cout<<"discMaker  "<<m_DiscMaker<<endl;
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

void StPmdNeuNet::Import(Text_t *fileName)
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
      for(c=0;c<fNUnits[i+1];c++){
      fDW[i][l][c]=0.;fscanf(file,"%f",&tmpfl);*(fW[i][l]+c)=(Double_t)tmpfl;
      //      cout<<"Nhidd "<<i<<"Nunit "<<l<<"unit_next "<<c<<"wei "<<fW[i][l][c]<<endl;     
}
      

  fscanf(file,"%5i",&fNTrainCycles);  
  fscanf(file,"%f",&tmpfl);fUseBiases=(Double_t)tmpfl;  
    
  fclose(file);   
}

void StPmdNeuNet::Mix()
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

void StPmdNeuNet::SetArraySize(Int_t size)
{
  DeleteArray();
  if (fEventsList) delete [] fEventsList;
  if(!size)return;
  Int_t i;
  fNTrainEvents=size;  
  fArrayIn  = new Float_t*[fNTrainEvents];
  for (i=0;i<fNTrainEvents;i++) fArrayIn[i] = new Float_t[fNUnits[0]];

  cout<<"array size "<<fNUnits[0]<<endl;

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


Double_t StPmdNeuNet::ApplyWeights(Float_t *Teach,Float_t *Value)
{
// one loop on internal events = one cycle.  
// takes each event from internal array in an order fixed by an array ( fEventsList ).
// It is necessary to call the method Mix() before each call to this function
// in order to change the presentation order.
// The learning is done by this function.
// The private variable  fNTrainCycles is incremented.

  if(fNTrainEvents<1){printf("No event to train !!!\n");return 0.;}
  if(!fW){printf("no unit !\n");return 0.;}
  FILE *file1;
  file1=fopen("testout","w");

  Int_t i;
  Double_t error=0.;

  for(i=0;i<fNTrainEvents;i++)
  {  
    GetArrayEvt(fEventsList[i]); 
    Forward();
//	  cout<<"fNUnits "<<fNUnits[fNHiddL+1]<<endl;
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
