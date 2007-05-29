//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StChallenger                                                         //
//                                                                      //
// Loads the appropriate shared libraries                               //
// for the StChallenger interface to the Grand Challenge services       //
//                                                                      //
// Alexandre V. Vaniachine <AVVaniachine@lbl.gov>                       //
//////////////////////////////////////////////////////////////////////////
#include <Stiostream.h>

#include "TROOT.h"
#include "TClass.h"
#include "StChallenger.h"
#include "StMessMgr.h" 

 ClassImp(StChallenger)
//_____________________________________________________________________________
StChallenger* StChallenger::Challenge(){
  StChallenger  *serv = 0;

  if (gROOT->LoadClass("libOB","libOB")) return 0;
  if (gROOT->LoadClass("libGCAClient","libGCAClient")) return 0;
  if (gROOT->LoadClass("libChallenger","libChallenger")) return 0;

  TClass *c = gROOT->GetClass("Challenger");
  if (!c) {
    LOG_ERROR <<"failed to GetClass"<<endm;      
    return 0;
  }

  serv = (StChallenger *) c->New();
  if(!serv)
    LOG_ERROR<<"failed to make new Challenger"<<endm;

  return serv;
 }
//_____________________________________________________________________________
