////////////////////////////////////////////////////////
//  Mixing tool 
/////////////////////////  Author R.Ganz April 99 ///////
//
//  You can store lists of Particles (TObjArrays) in this
//  mixer. It will organize the memory, and tell you
//  when it has collected enough statisic to produce
//  mixed events.
//  Optional you can have one or two lists(default) of
//  particles stored
/////////////////////////////////////////////////////////  

#include "StEmcPoint.h"
#include "TMixer.h"

ClassImp(TMixer)
 
/*
 $Log: TMixer.cxx,v $
 Revision 1.1  2006/04/20 11:42:26  amischke
 automatically CVS:  CVS: Committing in . CVS:  CVS: Added Files: CVS:  StPi0InvMassMaker.cxx StPi0InvMassMaker.h TEventMixer.cxx CVS:
 TEventMixer.h TMixer.cxx TMixer.h CVS: ----------------------------------------------------------------------

 Revision 1.1  2005/01/06 14:47:14  amischke
 classes for the event mixing

 * Revision 1.3  1999/11/23  13:54:08  cblume
 * Remove files
 *
*/

  TMixer::TMixer()
{
  Reset();
}

TMixer::TMixer(Int_t n, Int_t nl)
{
  ////////////////////////////////////////
  //  use this constructor to initialize the mixer
  //   n    number of events to be stored
  //   nl   number of lists to be kept (default 2)
  ////////////////////////////////////////////
    
  Reset();
  nMaxEvents = n;
  fNumList =nl;
  fFlagInvers =0;
  fPart1Pool = new TObjArray(n);
  fPart1List = new TObjArray();
  fPart2List = new TObjArray();
  if (fNumList>1)
  {
      fPart2Pool = new TObjArray(n);
  };
}


void TMixer::Reset()
{
  nEvent=0;
  nCurrent=0;
  fIsReadyFlag=0;
  fVerbose=0;
  nMaxEvents = 0;
  fPart1Pool = NULL;
  fPart2Pool = NULL;
  fPart1List = NULL;
  fPart2List = NULL;
  fNumList =2;
}

void TMixer::AddEvent(TObjArray *P1list, TObjArray *P2list)
{
  ///////////////////////////////////////////
  // Add one (2nd left empty) or two lists to the mixer
  // If initialize number of events is eceeded
  // ReadyFlag is set and first(oldest) list(event)
  // will be removed
  ///////////////////////////////////////////// 
  

  Int_t I;
  StEmcPoint *Part;
  StEmcPoint *PartHelp;
 
// delete first part lists
  if (fIsReadyFlag==1)  
    {
      TObjArray *PartH =  (TObjArray*)fPart1Pool->At(0);
      for (I=0; I< PartH->GetEntries() ;I++)
      {
	StEmcPoint *dummy = (StEmcPoint *)PartH->At(I);
	delete dummy;
      }
      ((TObjArray*)PartH)->Clear();
      ((TObjArray*)PartH)->Delete();
      delete PartH; 
      fPart1Pool->RemoveAt(0);      
      fPart1Pool->Compress();
      if (fVerbose) {printf(" <I> TMixer: Deleted Part 1 list n");};
    };

  if (fIsReadyFlag==1 && fNumList==2)  
    {
      TObjArray *PartH =  (TObjArray*)fPart2Pool->At(0);
      for (I=0; I< PartH->GetEntries() ;I++)
	{
	  StEmcPoint *dummy = (StEmcPoint *)PartH->At(I);
	  delete dummy;
	}

      ((TObjArray*)PartH)->Clear();
      ((TObjArray*)PartH)->Delete();
      delete PartH;
      fPart2Pool->RemoveAt(0);       
      fPart2Pool->Compress();
      if (fVerbose) {printf(" <I> TMixer: Deleted Part 2 list n");};
    };

// add new  part lists at the end

 
  TObjArray* a = new TObjArray();
  for (I=0; I< P1list->GetEntries(); I++)
    {	
      PartHelp =  (StEmcPoint *)P1list->At(I); 
      if( fFlagInvers ==1){
	// Part = new TParticle();
	// Part->CopyParticleInvers(*PartHelp);
      }       
      else{
	Part   =   new StEmcPoint(*PartHelp);	  
      }
      a->Add((TObject*)Part);
    }
  fPart1Pool->Add(a);  
  if (fNumList==2) 
    { 
      TObjArray* b = new TObjArray();
      for (I=0; I< P2list->GetEntries() ;I++)
        {
	  PartHelp =  (StEmcPoint *)P2list->At(I);	  
	  Part   =   new StEmcPoint(*PartHelp);
	  b->Add((TObject*)Part);
	  
        }
      fPart2Pool->Add(b);
      
    };
  nEvent++;
  
  if (nEvent == nMaxEvents)  
    { 
      fIsReadyFlag=1;
      if (fVerbose) 
        {printf(" <I> TMixer is ready now n");};
    };
  if (nCurrent<nMaxEvents)  { nCurrent++;}; 
  if (fVerbose) 
    {printf(" <I> TMixer: Event %d   Current %d added  n",nEvent,nCurrent);};
    
}


void TMixer::GetEvent(Int_t Evt1, Int_t Evt2)
{ ////////////////////////////////////////////////////
  // Sets particle List 1 and 2 to lists of
  // Evt1 and Evt2 respectivly
  //
  // Eventmixing mode:
  // If Evt1<0 or >Max. Event Number will be assigned randomly to Evt1
  // and Part1List is assigned accordingly    
  // If Evt2<0 or >Max. Event Number will be assigned randomly to Evt2
  // and Part2List is assigned accordingly    
  /////////////////////////////////////////////////////////
  fEvent1 = Evt1;
  fEvent2 = Evt2;
  
  while  (!CheckBound(fEvent1))   
    {fEvent1 = (int)(gRandom->Rndm(nEvent)*((float)nCurrent));};

  //while  (!CheckBound(fEvent2) ||(fEvent1==fEvent2)
  while  (!CheckBound(fEvent2) )
    {fEvent2 = (int)(gRandom->Rndm(nEvent)*((float)nCurrent));};


  if (fVerbose) 
    {printf(" <I> TMixer: Mixed Event %d  and %d   n",fEvent1,fEvent2);};   
}

TObjArray* TMixer::GetPart1List()
{ ////////////////////////////////////////////////
  // Retrieve list 1 from event #Evt1 assigned in GetEvent
  ////////////////////////////////////////////////
  StEmcPoint *track;
  if (fPart1List) fPart1List->Delete();
  
  TIter NextParticle((TObjArray*)fPart1Pool->At(fEvent1));
  while( (track = (StEmcPoint *) NextParticle()) != 0)
    {
      fPart1List->Add((TObject*)track);            
    }
  
  if(fVerbose)
    {
      printf(" <I> Mixing: Returns Part1List with %d particles. n", fPart1List->GetEntries());
    }
  
  return fPart1List;
}

TObjArray* TMixer::GetPart2List()
{ ////////////////////////////////////////////////
  // Retrieve list 2  from event#Evt2 assigned in GetEvent
  ////////////////////////////////////////////////

  StEmcPoint *track;
  if (fPart2List) fPart2List->Delete();
  
  if (fNumList==2)
    {
      TIter NextParticle((TObjArray*)fPart2Pool->At(fEvent2));
      while( (track = (StEmcPoint *) NextParticle()) != 0)
        {
          fPart2List->Add((TObject*)track);            
        }
    }
  else
    {
      TIter NextParticle((TObjArray*)fPart1Pool->At(fEvent2));      
      while( (track = (StEmcPoint *) NextParticle()) != 0)
        {
          fPart2List->Add((TObject*)track);            
        }
    }
  if(fVerbose)
    {
      printf(" <I> Mixing: Returns Part2List with %d particles. n", fPart2List->GetEntries());
    }
  return fPart2List;
}


