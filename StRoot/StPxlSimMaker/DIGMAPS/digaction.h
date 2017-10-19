#ifndef MAPS_DIGACTION_H
#define MAPS_DIGACTION_H

#include <TNamed.h>
#include <TList.h>
#include <TGraph.h>
#include "Riostream.h"
#include "vector"

// ROOT classes
#include "TString.h"
#include "TObject.h"
#include "TVector.h"
#include "TFile.h"
#include "TSystem.h"
#include "TRandom.h"
#include "TH1.h"
#include "TH2.h"
#include "TObjArray.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClonesArray.h"
using namespace std;

//Int_t GlobalSeed = 1;

class DIGInitialize;
class DIGPlane;
class DIGADC;
class DIGBeam;
class DIGTransport;
class DIGParticle;
class DIGAction;
class DIGEvent;
class DIGMAPS;
//==============================================================================

class DIGAction : public TObject {
 public:
  DIGAction();
  DIGAction(Char_t *Doit, Char_t *Model);
  //  DIGPlane(char *name,char *title);
  virtual ~DIGAction();
  void SetDoit(Char_t *Doit);
  void SetModel(Char_t *Model);
  void PrintInfo();
  Char_t *GetDoit(){return fDoit;}
  Char_t *GetModel(){return fModel;}

 protected:
  enum       {tpsz = 200};
  Char_t  fDoit[tpsz]; // "foresee" , "train" , "plot"
  Char_t fModel[tpsz]; // "basic"
  /*
 Char_t    tWeightFileName[200];
  Int_t     tRunIndex = GetRunPar().Number;
  sprintf(tWeightFileName,"config/inf%d.root",tRunIndex); //was DTDIR

   fWeightFileName = tWeightFileName;

  const char*   GetWeightFileName() {return fWeightFileName;}

  struct ActionParameter_t {
    enum       {actionnum = 200};
    Char_t Doit[actionnum];
    Char_t Model[actionnum];

  } ActionParameter; 
  ActionParameter_t& GetActionPar()         {return ActionParameter;}

*/
  ClassDef(DIGAction,1);
};





//==============================================================================

#endif


