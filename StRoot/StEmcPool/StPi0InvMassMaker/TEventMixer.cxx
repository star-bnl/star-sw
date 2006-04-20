#include "TEventMixer.h"
#include <math.h>
#include "stdlib.h"
#include <iostream>
using namespace std;
// StRoot
#include "St_DataSet.h"
#include "St_DataSetIter.h"
//#include "StMessMgr.h"


#include "TMixer.h"


//*-*-* ROOT includes

#include "TROOT.h"
#include "TRandom.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TBranch.h"
#include "TObjArray.h"
#include "TObject.h"
#include "TString.h"
#include "TArrayF.h"

ClassImp(TEventMixer)
    
TEventMixer::TEventMixer() 
{
  Init(); 
}

TEventMixer::~TEventMixer() { /* noop */ }
 
 



 
void TEventMixer::Init()
{
    fNumMixVariable = 0; 
    fnbin1          = 0;               
    fmin1           = 0;                  
    fmax1           = 0;                  
    fnbin2          = 0;                 
    fmin2           = 0;                 
    fmax2           = 0;                  
    fnbin3          = 0;                   
    fmin3           = 0;                  
    fmax3           = 0;              

    fpositionx      = 0;
    fpositiony      = 0;
    fpositionz      = 0;


    fEventList = new TObjArray();
}




void TEventMixer::SetMixVariable(Int_t nummixed,
                            Int_t nbin1, Double_t min1, Double_t max1,
                            Int_t nbin2, Double_t min2, Double_t max2,
                            Int_t nbin3, Double_t min3, Double_t max3)
{  
   fnummixed = nummixed;
   fnbin1 = nbin1;
   fmin1  = min1;
   fmax1  = max1;
   fnbin2 = nbin2;
   fmin2  = min2;
   fmax2  = max2;
   fnbin3 = nbin3;
   fmin3  = min3;
   fmax3  = max3;
   cout<< "TEventMixer: mixing variables are set " << endl;
   cout<<"1.Binning  nbin="<<fnbin1<<" min="<<fmin1<<" max="<<fmax1<< endl;
   cout<<"2.Binning  nbin="<<fnbin2<<" min="<<fmin2<<" max="<<fmax2<< endl;
   cout<<"3.Binning  nbin="<<fnbin3<<" min="<<fmin3<<" max="<<fmax3<< endl;
 
   cout<< "TEventMixer: create mixing classes " <<endl;    
   for (Int_t i=0; i<fnbin1;i++)
   {
     for (Int_t ii=0; ii<fnbin2;ii++)
     {
       for (Int_t iii=0; iii<fnbin3;iii++)
       {
	 fMixerList[i][ii][iii] = new TMixer(fnummixed); 
         cout<<"fMixerList["<<i<<"]["<<ii<<"]["<<iii<<"]" << endl; 
         fNumLastEvent[i][ii][iii]=0;
         fNumEventInMix[i][ii][iii]=0;
      
       }
     }
   }
 
   // for the first events which do not get mixed used at the end       
   for (Int_t i=0; i<fnbin1;i++)
   {
     for (Int_t ii=0; ii<fnbin2;ii++)
     {
       for (Int_t iii=0; iii<fnbin3;iii++)
       {
	 fMixerListFirstEvents[i][ii][iii] = new TMixer(fnummixed);     
       }
     }
   }
      

}



void TEventMixer::AddEvent(TObjArray *list1,TObjArray *list2,Double_t x, Double_t y,Double_t z)
{

  fpositiony =0;
  fpositionz =0;

  // position - min / binsize
  fpositionx =  Int_t ( (x - fmin1) / ((fmax1 - fmin1)/fnbin1) );
  if(y != 0)
  fpositiony =  Int_t ( (y - fmin2) / ((fmax2 - fmin2)/fnbin2) );
  if(z!= 0)
  fpositionz =  Int_t ( (z - fmin3) / ((fmax3 - fmin3)/fnbin3) );

  //cout<<" vertex positon "<<VertexZPos<<" mixer position "<<fposition<<endl;
 
  // fill first events in separat list   
  if(fMixerList[fpositionx][fpositiony][fpositionz]->IsReady() != 1) 
  {  
    fMixerListFirstEvents[fpositionx][fpositiony][fpositionz]->AddEvent(list1,list2); 
    fNumEventInMix[fpositionx][fpositiony][fpositionz]++;
  }

  // fill mixer list 
  fMixerList[fpositionx][fpositiony][fpositionz]->AddEvent(list1,list2);
	  
  fmixcounter= 0;
  


}


Int_t TEventMixer::IsItReady()
{

  return fMixerList[fpositionx][fpositiony][fpositionz]->IsReady();  
}


void  TEventMixer::GoToEvent(Int_t event1, Int_t event2)
{   
  
  fMixerList[fpositionx][fpositiony][fpositionz]->GetEvent(event1,event2);   
  fPartList1 = fMixerList[fpositionx][fpositiony][fpositionz]->GetPart1List();
  fPartList2 = fMixerList[fpositionx][fpositiony][fpositionz]->GetPart2List();   
}



Int_t TEventMixer::AddLastEvent(Int_t bin1,Int_t bin2, Int_t bin3)
{

  cout<< "TEventMixer::AddLastEvent Bin["<<bin1<<"]["<<bin2<<"]["<<bin3<<"]" << endl;

  Int_t  fillevent =0;
 
  fpositionx =  bin1;
  fpositiony =  bin2;
  fpositionz =  bin3;

  Int_t event = fNumLastEvent[fpositionx][fpositiony][fpositionz];
 
  if(fMixerListFirstEvents[fpositionx][fpositiony][fpositionz]->IsReady()==1)
  {
      fMixerListFirstEvents[fpositionx][fpositiony][fpositionz]->GetEvent(event,0);
      fList1 = fMixerListFirstEvents[fpositionx][fpositiony][fpositionz]->GetPart1List(); 

      fMixerListFirstEvents[fpositionx][fpositiony][fpositionz]->GetEvent(0,event);
      fList2 = fMixerListFirstEvents[fpositionx][fpositiony][fpositionz]->GetPart2List(); 

     fMixerList[fpositionx][fpositiony][fpositionz]->AddEvent(fList1,fList2);
     fillevent =1;

    

  }



  fNumLastEvent[fpositionx][fpositiony][fpositionz]++; 
  return  fillevent;         
}



void TEventMixer::PrintMixer()
{

 for (Int_t i=0; i<fnbin1;i++)
   {
     for (Int_t ii=0; ii<fnbin2;ii++)
     {
       for (Int_t iii=0; iii<fnbin3;iii++)
       {
        
         cout<<"fMixerList["<<i<<"]["<<ii<<"]["<<iii<<"]" << endl; 
         for(Int_t ev=0; ev<  fNumEventInMix[i][ii][iii] ;ev++){
	   fMixerList[i][ii][iii]->GetEvent(ev,ev);   
           fPartList1 = fMixerList[i][ii][iii]->GetPart1List();
           fPartList2 = fMixerList[i][ii][iii]->GetPart2List(); 
           cout<<" event "<< ev << " list 1 " <<GetPartList1()->GetEntries()<< " list 2 "<< GetPartList2()->GetEntries()<<endl;
	 }
       }
     }
   }
        
}

void TEventMixer::PrintMixerFirstEvent()
{

 for (Int_t i=0; i<fnbin1;i++)
   {
     for (Int_t ii=0; ii<fnbin2;ii++)
     {
       for (Int_t iii=0; iii<fnbin3;iii++)
       {
         
         cout<<"fMixerListFirstEvents["<<i<<"]["<<ii<<"]["<<iii<<"]" << endl; 
         for(Int_t ev=0; ev<  fNumEventInMix[i][ii][iii];ev++){
	  
          fMixerListFirstEvents[i][ii][iii]->GetEvent(ev,ev);   
          fPartList1 = fMixerListFirstEvents[i][ii][iii]->GetPart1List();
          fPartList2 = fMixerListFirstEvents[i][ii][iii]->GetPart2List();   
         
	  cout<<" event "<< ev << " list 1 " <<GetPartList1()->GetEntries()<< " list 2 "<< GetPartList2()->GetEntries()<<endl;
	 }
       }
     }
   }
        
}
