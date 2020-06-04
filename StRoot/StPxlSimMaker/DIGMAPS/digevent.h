#ifndef MAPS_DIGEVENT_H
#define MAPS_DIGEVENT_H

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

#include "digparticle.h"
#include "digcluster.h"
#include "digreadoutmap.h"
#include "digplane.h"


using namespace std;

class DIGInitialize;
class DIGPlane;
class DIGADC;
class DIGBeam;
class DIGTransport;
class DIGParticle;
class DIGCluster;
class DIGAction;
class DIGMAPS;

//==============================================================================
class DIGEvent : public TObject {
 public:
  DIGEvent();
  DIGEvent(DIGEvent & adigevent);
  // DIGEvent(Float_t EntryX, Float_t EntryY, Float_t ExitX, Float_t ExitY, Float_t Energy_deposited);
  virtual ~DIGEvent();
  void             Clear(const Option_t * /*option*/ = "");
    //void             Clear(const Option_t *);
  void PrintInfo();
  TClonesArray *GetParticle(){return fDIGParticleArray;}
  TClonesArray *GetCluster(){return fDIGClusterArray;}
  Int_t         GetNParticles(){return fNParticles;}
  Int_t         GetNClusters(){return fNClusters;}
  Int_t         GetConfigurationNumber(){return fConfigurationNumber;}

  DIGReadoutmap *GetReadoutmap(){return fDIGReadoutmap;}

  void          SetNParticles(Int_t Nparticles);
  void          SetNClusters(Int_t NClusters);
  void          SetConfigurationNumber(Int_t ConfigurationNumber);

  void  AddParticle(DIGParticle& particle);
  void  AddCluster(DIGCluster& cluster);
  void  BuildTrueClusters(DIGPlane *myDIGPlane);


 protected:
  Int_t            fConfigurationNumber;
  Int_t            fNParticles;
  Int_t            fNClusters;
  TClonesArray       *fDIGParticleArray;
  TClonesArray       *fDIGClusterArray;
  DIGReadoutmap *fDIGReadoutmap;

  

  
  ClassDef(DIGEvent,1);
};

//==============================================================================


#endif
