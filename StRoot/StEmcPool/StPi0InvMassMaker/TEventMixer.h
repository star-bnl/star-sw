#ifndef _TEventMixer_INCLUDED_
#define _TEventMixer_INCLUDED_

#include "TObject.h"
#include "TH1.h"
#include "TH2.h"
#include "TObjArray.h"
#include "TClonesArray.h"
#include "TCutG.h"
#include "TString.h"
#include "TArrayF.h"


#include "TMixer.h"

//Forward Declarations
class TFile;
class TH1F;

class TEventMixer : public TObject {

public:
    TEventMixer();   
    virtual ~TEventMixer();
     
    void SetMixVariable(Int_t nummixed,
                         Int_t nbin1, Double_t min1, Double_t max1,
                         Int_t nbin2=1, Double_t min2=0, Double_t max2=0,
                         Int_t nbin3=1, Double_t min3=0, Double_t max3=0);

    void      AddEvent(TObjArray *list1,TObjArray *list2,Double_t x,Double_t y=0.0,Double_t z=0.0);
    void      GoToEvent(Int_t event1, Int_t event2);

    Int_t     IsItReady();
  
    TObjArray *GetPartList1()                  {return fPartList1;}
    TObjArray *GetPartList2()                  {return fPartList2;}

    Int_t     AddLastEvent(Int_t bin1,Int_t bin2, Int_t bin3);
 
    void SetNumEvent(Int_t x)                          {fNumEvent = x;}     
    void SetNumMixedEvent(Int_t x)                     {fnummixed = x;}
  
    void SetBinX(Int_t x)                              {fpositionx = x;}
    void SetBinY(Int_t y)                              {fpositiony = y;}
    void SetBinZ(Int_t z)                              {fpositionz = z;}

    void  Init();
   

    void PrintMixer();
    void PrintMixerFirstEvent();

    Int_t  GetNumMixedEvent()                        {return fnummixed;} 
    Int_t  GetNum1Bin()                              {return fnbin1;} 
    Int_t  GetNum2Bin()                              {return fnbin2;} 
    Int_t  GetNum3Bin()                              {return fnbin3;} 
    Int_t  GetNumEventInMix(Int_t x, Int_t y, Int_t z)     {return fNumEventInMix[x][y][z];}

    //TObjArray *GetHistoListTrack()                 {return fTTrackAna->GetHistoList();} 
    //TObjArray *GetHistoListProton()                {return anaproton->GetHistoList();} 
    //TObjArray *GetHistoListKaon()                  {return anakaon->GetHistoList();} 
  
    ClassDef(TEventMixer,1) // Event mixer for TObjArray

    protected:
    
    Int_t startkaon;               //! 
    Int_t startproton;             //!
    Int_t fNumEvent;               //!  
  
    Int_t     fNumMixVariable;     //!
    Int_t     fnbin1;              //!
    Double_t  fmin1;               //!
    Double_t  fmax1;               //!
    Int_t     fnbin2;              //!
    Double_t  fmin2;               //!
    Double_t  fmax2;               //!
    Int_t     fnbin3;              //!
    Double_t  fmin3;               //!
    Double_t  fmax3;               //!

    TMixer *fMixerList[20][20][20]; 
    TMixer *fMixerListFirstEvents[20][20][20];       
    Int_t  fNumLastEvent[20][20][20];  //! number of mixed events 
    Int_t  fNumEventInMix[20][20][20];  //! number of mixed events 


    Int_t  fpositionx;      //!
    Int_t  fpositiony;      //!
    Int_t  fpositionz;      //!

    Int_t  fmixcounter;     //!
    Int_t  fstartproton;    //!
    Int_t  fstartkaon;      //!  

    TObjArray  *fEventList; //! 2 track lists a an event from mixer
    TObjArray *fPartList1;
    TObjArray *fPartList2;

    //TTreeMixer       *fTMixer;         //! mixing class
    TObjArray        *MixingList;      //!

    TObjArray        *fList1;      //!
    TObjArray        *fList2;      //!



    //TTreeMixer       *Mixed1;           //!
    //TTreeMixer       *Mixed2;           //!

   
    Int_t    fnummixed;  //! number of mixed events 
   
  
    TObjArray  *mixedprotonlist;     //! protontracklist
    TObjArray  *mixedkaonlist;       //!  kaontracklist
    
  
    	
	};
#endif
