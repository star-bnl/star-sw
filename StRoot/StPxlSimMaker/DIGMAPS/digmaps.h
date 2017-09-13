#ifndef MAPS_DIGMAPS_H
#define MAPS_DIGMAPS_H

///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//     DIGMAPS                                                                               //
//                                                                                           //
//    Main Class of DIGMAPS                                                                  //
// Contains pointers to all other classes and to the root tree                               //
//   units are all in micrometer.                                                            //
//       ROOT->ProcessLine(".L MAPS_digitiser.cxx+");                                        //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////

#include <TNamed.h>
#include <TList.h>
#include <TGraph.h>
#include <TCanvas.h>
#include "Riostream.h"
#include "vector"
#include <TRandom3.h>

// ROOT classes
#include "TStopwatch.h"
#include "TString.h"
#include "TObject.h"
#include "TVector.h"
#include "TFile.h"
#include "TSystem.h"
#include "TRandom.h"
#include "TProfile.h"
#include "TH1.h"
#include "TH2.h"
#include "TObjArray.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClonesArray.h"

#include "dighistograms.h"


using namespace std;

class DIGInitialize;
class DIGPlane;
class DIGADC;
class DIGBeam;
class DIGTransport;
class DIGParticle;
class DIGAction;
class DIGEvent;
class DIGMAPS;
class DIGHistograms;
class DIGResult;

static const Double_t PI=3.14159265358979312;
extern Int_t GlobalSeed;
// DIGInitialize test("read","test","/home/abesson/SAVE/ILCSOFT/DIGITISEUR/","input.txt")
class DIGMAPS : public TNamed, public DIGHistograms {
 public:
  DIGMAPS();
  DIGMAPS(char *name,char *title, TString aCP, TString aCFN,  TString action);
  DIGMAPS(char *name,char *title, TString aCP, TString aCFN,  TString outp, TString outf, TString action );
  virtual ~DIGMAPS();
  void         Run() ;
  void         RunConfiguration(Int_t configcounter, Int_t BeamNumber, Int_t PlaneNumber, Int_t ADCNumber, Int_t TransportNumber);
  void         ReadTree(TString StringTree) ;
  void         PrintConfigurations();
  void         ChargeGeneration(DIGParticle &aDIGParticle, Float_t StartingSegmentSize, 
				Float_t MaximumSegmentSize,
				Float_t MaximumChargePerSegment);

  void         ActionPlot();
  void         InspectEvent(Int_t EventNumber);
  void         PlotAConfiguration(Int_t confignumber, Bool_t newcanvas);
  void         PlotDigitalConfiguration(Bool_t newcanvas, Int_t *configlist, Int_t Nconfig);

  void         SetConfigPath(TString aCP) ;
  void         SetConfigFileName(TString aCFN) ;
  void         SetConfigPathAndFileName() ;
  void         SetConfigPathAndFileName(TString aCP,TString aCFN) ;
  void         SetOutputPath(TString outp) ;
  void         SetOutputFileName(TString outf) ;
  void         SetOutputPathAndFileName() ;
  void         SetOutputPathAndFileName(TString outp,TString outf) ;
  void         SetIsOutputfile(Bool_t IsOutputfile);
  void         SetNumberOfConfigurations(Int_t NumberOfConfiguration); 
  void         SetfAction(TString action) ;
  void         SetAction(DIGAction *aDIGAction) ;

  TString         GetConfigPath() ;
  TString         GetConfigFileName() ;
  TString         GetConfigPathAndFileName() ;
  TString         GetOutputPath()             {return fOutputPath ;}
  TString         GetOutputFileName()         {return fOutputFileName; } 
  TString         GetOutputPathAndFileName()  {return fOutputPathAndFileName;}
  Bool_t           GetIsOutputfile()  {return fIsOutputfile;}
  DIGInitialize   *GetDIGInitialize(){     return fDIGInitialize; }
  Int_t            GetPlanesN()                               { return  fPlanesN;      } 
  DIGPlane        *GetPlane(Int_t aPlaneNumber);
  Int_t            GetADCN()                               { return  fADCN;      } 
  DIGADC          *GetADC(Int_t anADCNumber);
  Int_t            GetTransportN()                               { return  fTransportN;      } 
  DIGTransport    *GetTransport(Int_t aTransportNumber);
  Int_t            GetBeamN()                               { return  fBeamN;      } 
  DIGBeam         *GetBeam(Int_t aBeamNumber);
  DIGEvent        *GetEvent(){ return  fdigevent;}

  DIGResult       *GetResult(Int_t aResultNumber){ return (DIGResult*)fDIGResultArray->At(aResultNumber);}


  //DIGHistograms   *GetHistograms() { return   fdighistograms;}
  Int_t            GetNumberOfConfigurations()                               { return  fNumberOfConfigurations;      } 
  DIGAction           *GetAction(){return fDIGAction;}
  TString         GetfAction(){return fAction;}

  Int_t            PoissonLaw(Float_t Lambda);
  Double_t         LandauLaw(Double_t mean, Double_t sigma);
  Double_t         GaussianLaw(Double_t mean, Double_t sigma); 
  Double_t         Lorentz2D(Double_t *x, Double_t *par);




  TStopwatch timer1;

 protected:

  
   ClassDef(DIGMAPS,1);
   TString      fConfigPath;                 // name of the configuration path
   TString      fConfigFileName;             // name of the configuration file
   TString      fConfigPathAndFileName;      // both path and file name appended

   Bool_t       fIsOutputfile;
   TString      fOutputPath;                 // name of the configuration path
   TString      fOutputFileName;             // name of the configuration file
   TString      fOutputPathAndFileName;      // both path and file name appended
   TString      fAction;

   DIGInitialize        *fDIGInitialize;                           // pointer to configuration
   //  DIGPlane        *fDIGPlane;                           // pointer to DIGPlanes
   Int_t            fPlanesN;                  // number of planes 
   TObjArray       *fDIGPlaneArray;	       // pointer to array of planes			    
   Int_t            fADCN;                     // number of ADCs
   TObjArray       *fDIGADCArray;	       // pointer to array of ADCs
   Int_t            fTransportN;                     // number of charge transport models
   TObjArray       *fDIGTransportArray;	       // pointer to array of models
   Int_t            fBeamN;                    // number of beams
   TObjArray       *fDIGBeamArray;	       // pointer to array of beams
   Int_t fNumberOfConfigurations; // number of configurations to study = N planes x N ADCs x  N charge transport models x N beams
   DIGAction *fDIGAction;

   TObjArray       *fDIGResultArray;	       // pointer to array of results


   ofstream     fOutputFileStream;

   TFile *ffile;
   TTree *ftree;
   DIGBeam *fdigbeam;
   DIGPlane *fdigplane;
   DIGADC *fdigadc;
   DIGTransport *fdigtransport;
   DIGEvent *fdigevent;
   DIGResult *fdigresult;
   //DIGHistograms *fdighistograms;
 
   TCanvas *MainCanvas1; //canvas 1
   TCanvas *MainCanvas2;
   TCanvas *MainCanvas3;
   TCanvas *MainCanvas4;
   TCanvas *MainCanvas5;
   //TRandom3 *r3;

   Int_t ColorChosen;
   
};



//==============================================================================
#endif
