#ifndef __AgUStep_h__
#define __AgUStep_h__

#include "TNamed.h"
#include "TTree.h"
#include "TFile.h"
#include "TRefArray.h"
#include "TGiant3.h"


// // class AgUTrackStep : public TObject
// {
// public:
//   Int_t    idTruth; // Track id
//   Double_t x0,x1; 
//   Double_t y0,y1;
//   Double_t z0,z1;
//   Double_t dEstep, adEstep;
//   ClassDef(AgUTrackStep,1);  
// };

class Track;
class Step;
class Event;

class Event : public TObject
{
public:
  Event();
  ~Event(){ Clear(); }

  Int_t     idEvent;
  Int_t     nTracks;
  Int_t     nSteps;
  //TObjArray tracks;
  TClonesArray *tracks;
  TClonesArray *steps;

  Track *AddTrack();
  Step  *AddStep();

  void   Clear( const Option_t *opts="" );

  ClassDef(Event,1);
};

class Track : public TObject
{
public:
  Track();
  ~Track(){ Clear(); }

  Int_t idTruth;     // ID truth for the track
  Float_t eta;       // initial eta
  Float_t phi;       // initial phi
  Float_t x, y, z;   // track 1st position
  Float_t px, py, pz; // track momentum
  Float_t mass, charge;
  Int_t nSteps;      // number of tracking steps
  //TObjArray steps;   // track history
  TRefArray steps;//[nSteps]

  Step *AddStep();
  void  Clear(const Option_t *opts="" );

  ClassDef(Track,1);
};

class Step;

class Step : public TObject
{
public:
  Step();

  void Clear( const Option_t *opts="" );

  Int_t idStep; // Number of tracking steps in this track
  Int_t idTruth; // idTruth of the parent track
  Float_t x,y,z,r; // Position of step;
  Int_t state; // Tracking state (inwvol)
  Float_t dEstep; // energy lost in this step [GeV]
  Float_t adEstep; // accumulated energy lost by this point [GeV]
  Int_t   nStep; // accumulated number of steps to this point
  Float_t step; // step size [cm]
  Float_t dens; // density of material in this tracking step
  Float_t A, Z; // A and Z of material in this tracking step
  UShort_t vnums[15]; // Volume numbers
  UShort_t cnums[15]; // Copy numbers
  Int_t    isvol;     // Sensitive volume flag

  TString path();
  TString volume();

  ClassDef(Step,1);
};


class AgUStep : public TNamed
{
public:
  AgUStep(); 
 ~AgUStep(){ };

  static AgUStep *Instance();
  void operator()();

  void Finish();

  /// Initialize stepping routine.  Opens TFile and creates TTree
  void Init( const Char_t *filename="" );

  static Float_t rmin;// =   0.0;
  static Float_t rmax;//= 200.0;
  static Float_t zmin;//=-200;
  static Float_t zmax;//= 200.0;
  static Int_t   verbose;// = 0;
  static Int_t   mnTruth;//=0;
  static Int_t   mxTruth;//=-1;

private:
protected:

  static AgUStep *sInstance;
  TTree *mTree;
  TFile *mFile;

public:
  Event *mEvent; // Current event 
  Track *mTrack; // Current track
  
  static TGiant3   *geant3; //!
  static Quest_t   *cquest; //! 
  static Gclink_t  *clink; //! 
  static Gcflag_t  *cflag; //! 
  static Gcvolu_t  *cvolu; //! 
  static Gcnum_t   *cnum; //! 
  static Gcsets_t  *csets; //!
  static Gckine_t  *ckine; //!
  static Gcking_t  *cking; //!
  static Gctrak_t  *ctrak; //!
  static Gcmate_t  *cmate; //!
  static Gccuts_t  *ccuts; //!
  static Gcphys_t  *cphys; //!
  static Gctmed_t  *ctmed; //!
  static Int_t      nlev; //!

private:
protected:  
  Int_t   idEvent;  // current event number
  Int_t   idTruth;  // current track number
  Float_t aDeStep;  // accumulated energy loss
  Int_t   nStep;    // accumulated number of steps
  Float_t aStep;    // accumulated path length

  Float_t vect0[7]; // previous step

  Int_t oldEvent;

  ClassDef(AgUStep,1);

};

#endif
