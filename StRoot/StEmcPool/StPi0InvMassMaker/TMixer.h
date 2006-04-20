#ifndef _TMixer_INCLUDED_
#define _TMixer_INCLUDED_

/*
$Log: TMixer.h,v $
Revision 1.2  2006/04/20 13:47:15  amischke
bug fixed.

Revision 1.1  2006/04/20 11:42:26  amischke

automatically CVS:  CVS: Committing in . CVS:  CVS: Added Files: CVS:  StPi0InvMassMaker.cxx StPi0InvMassMaker.h TEventMixer.cxx CVS:
TEventMixer.h TMixer.cxx TMixer.h CVS: ----------------------------------------------------------------------

Revision 1.1  2005/01/06 14:47:14  amischke
classes for the event mixing

 * Revision 1.3  1999/11/23  13:52:15  cblume
 * Remove files
 *
*/

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* ROOT includes */
#include "TClass.h"
#include "TKey.h"
#include "TRandom.h"
#include "TObject.h"
#include "TObjArray.h"
#include "TClonesArray.h"
#include "TROOT.h"
#include "TCanvas.h"
#include "TNtuple.h"
#include "TBranch.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "TRandom.h"
#include "TSystem.h"



class StEmcPoint;

class TMixer : public TObject 
{
  
protected:
  Int_t nMaxEvents;                    //! Maximum number of event
  Int_t nEvent;                        //! Total Number of events processed 
  Int_t nCurrent;                      //! Degree of filling  ( from 0 to nMaxEvents)
    Int_t fIsReadyFlag;                //! Flag signals nCurrent == nMaxEvents
  Int_t fNumList;                      //! Number of diefferent particle lists
          
  Int_t fVerbose;
  Int_t fFlagInvers;

  TObjArray *fPart1Pool;               //! Particle 1 Pool
  TObjArray *fPart2Pool;               //! Particle 2 Pool 

  TObjArray *fPart1List;               //! Current Pacticle 1 List
  TObjArray *fPart2List;               //! Current Pacticle 2 List

  Int_t fEvent1;                       //! EventNumber for first P. list
  Int_t fEvent2;                       //! EventNumber for second P. list

    //StTrack *Part1;                   //! for copy tracks
    //StTrack *Part2;                   //! for copy tracks

    
  
public: 
  TMixer();

  TMixer(Int_t n, Int_t nl=2);
  
  Int_t CheckBound(Int_t m) { return (( 0<=m && m < nCurrent) ? 1 : 0 );}
  
  Int_t IsReady() {return fIsReadyFlag;};

  void SetNumList(Int_t n = 2) {fNumList=n;};
  
  void SetVerbose(Int_t n = 1) {fVerbose=n;};
  
  void SetInvers()             {fFlagInvers =1;};

  void Reset();
      
  void AddEvent(TObjArray *P1list, TObjArray *P2list=NULL);
    
  void GetEvent(Int_t Evt1=-1, Int_t Evt2=-1);

  TObjArray* GetPart1List();  
  TObjArray* GetPart2List();

  ClassDef(TMixer,1)   // Event mixer for list of T49ParticleRoot

} ; 
#endif

