#ifndef MAPS_DIGINITIALIZE_H
#define MAPS_DIGINITIALIZE_H

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

class DIGInitialize;
class DIGPlane;
class DIGADC;
class DIGBeam;
class DIGTransport;
class DIGParticle;
class DIGAction;
class DIGEvent;
class DIGMAPS;

class DIGInitialize : public TNamed {
 public:
  DIGInitialize();
  DIGInitialize(char *name,char *title, TString aCP, TString aCFN, TString action);
  virtual ~DIGInitialize();
  void         SetConfigPath(TString aCP) ;
  void         SetConfigFileName(TString aCFN) ;
  void         SetConfigPathAndFileName() ;
  void         SetConfigPathAndFileName(TString aCP,TString aCFN) ;
  TString         GetConfigPath() ;
  TString         GetConfigFileName() ;
  TString         GetConfigPathAndFileName() ;

  void         nextItem(Char_t delimiter);
  //  void         read_r3(DR3 &arg);
  void         read_item(Int_t &arg);
  void         read_item(Float_t &arg);
  void         read_item(Double_t &arg);
  void         read_item(Bool_t &arg);


  void PrintInfo();

  struct ActionParameter_t {
    enum       {actionnum = 200};
    Char_t Doit[actionnum];
    Char_t Model[actionnum];

  } ActionParameter; 
  ActionParameter_t& GetActionPar()         {return ActionParameter;}


  struct BeamParameter_t {
    enum       {beamnum = 50};
    Int_t RunNumber; //You can set a run number to compare several configurations.
    Int_t NumberOfEvents; //number of events to run per configuration
    Float_t ParticleDensity; //number of particles per mm^2 per event
    Int_t NAngles;
    Float_t ThetaIncidentDeg[beamnum];
    Float_t PhiIncidentDeg[beamnum];
    Int_t BeamOption;

  } BeamParameter; 
  BeamParameter_t& GetBeamPar()         {return BeamParameter;}

  struct PlaneParameter_t {
    enum       {planenum = 100};
    Int_t NADC;  

    Int_t NTransport;  // <20

    //Int_t NPitch;
    Int_t NGeom;
    Float_t PitchX[planenum];
    Float_t PitchY[planenum];
    //Int_t Nepitaxial;
    Float_t EpitaxialThickness[planenum];
    //Int_t NNoise;
    Float_t NoiseElectrons[planenum];    
    Int_t NPixelsX;
    Int_t NPixelsY;  
    Int_t NTemperature;
    Float_t Temperature[planenum];
    Float_t IonizationEnergy;
    Float_t SegmentSize;
    Float_t MaximumSegmentSize;
    Float_t MaximumChargePerSegment;
    Float_t DiffusionMaximumRangeInX;
    Float_t DiffusionMaximumRangeInY;
    Float_t ReflexionCoefficient;
    Float_t BasicModel_SigmaTenMicrons;

  }PlaneParameter;
  PlaneParameter_t& GetPlanePar()         {return PlaneParameter;}

  struct TransportParameter_t {
    //enum       {transportnum = 20};
    Int_t ChargeModel;
    Float_t RangeLimit_InPitchUnit;
    Float_t Lorentz2DModel_Cp0;
    Float_t Lorentz2DModel_Cp1;
    Float_t   Gauss2DModel_sigma1_Cp0;
    Float_t   Gauss2DModel_sigma1_Cp1;
    Float_t   Gauss2DModel_sigma2_Cp0;
    Float_t   Gauss2DModel_sigma2_Cp1;
    Float_t   Gauss2DModel_weight;

    Float_t   LorGaussModel_Norm1_Cp0;
    Float_t   LorGaussModel_Norm1_Cp1;
    Float_t   LorGaussModel_Norm1_Cp2;
    Float_t   LorGaussModel_sigma_Cp0;
    Float_t   LorGaussModel_sigma_Cp1;
    Float_t   LorGaussModel_C_Cp0;
    Float_t   LorGaussModel_C_Cp1;
    Float_t   LorGaussModel_Norm_Cp0;
    Float_t   LorGaussModel_Norm_Cp1;

    Float_t   lorlorgausModel_Norm1_Cp0 ;
    Float_t   lorlorgausModel_Norm1_Cp1 ;
    Float_t   lorlorgausModel_x01_Cp0 ;
    Float_t   lorlorgausModel_x01_Cp1 ;
    Float_t   lorlorgausModel_sigmax1_Cp0 ;
    Float_t   lorlorgausModel_sigmax1_Cp1 ;
    Float_t   lorlorgausModel_y01_Cp0 ;
    Float_t   lorlorgausModel_y01_Cp1 ;
    Float_t   lorlorgausModel_sigmay1_Cp0 ;
    Float_t   lorlorgausModel_sigmay1_Cp1 ;
    Float_t   lorlorgausModel_Gamma_Cp0 ;
    Float_t   lorlorgausModel_Gamma_Cp1 ;
    Float_t   lorlorgausModel_x0_Cp0 ;
    Float_t   lorlorgausModel_x0_Cp1 ;
    Float_t   lorlorgausModel_y0_Cp0 ;
    Float_t   lorlorgausModel_y0_Cp1 ;
    Float_t   lorlorgausModel_norm_Cp0 ;
    Float_t   lorlorgausModel_norm_Cp1 ;
    Float_t   lorlorgausModel_normgaus2_Cp0 ;
    Float_t   lorlorgausModel_normgaus2_Cp1 ;
    Float_t   lorlorgausModel_sigma2_Cp0 ;
    Float_t   lorlorgausModel_sigma2_Cp1 ;

    Float_t   l1dimgauslor_Norm_g_1st;
    Float_t   l1dimgauslor_x0_g_1st;
    Float_t   l1dimgauslor_sigma_g_1st;
    Float_t   l1dimgauslor_Gamma_lor_1st;
    Float_t   l1dimgauslor_x0_lor_1st;
    Float_t   l1dimgauslor_norm_lor_1st;
    Float_t   l1dimgauslor_Norm_g_2nd;
    Float_t   l1dimgauslor_x0_g_2nd;
    Float_t   l1dimgauslor_sigma_g_2nd;
    Float_t   l1dimgauslor_Gamma_lor_2nd;
    Float_t   l1dimgauslor_x0_lor_2nd;
    Float_t   l1dimgauslor_norm_lor_2nd;


  //par[0] Norm_1
  //par[1] x0_1
  //par[2] sigma_x_1
  //par[3] y0_1
  //par[4] sigma_y_1
  // par[5] = Gamma
  // par[6] = x0
  // par[7] = y0
  // par[8] = norm
  // par[9] = normgaus2
  // par[10] = sigma_2

  };

  TransportParameter_t *pTransportParameter;
  TransportParameter_t& GetTransportPar(Int_t aPN)         {return pTransportParameter[aPN];}


  struct ADCParameter_t {
    enum       {adcnum = 4096}; //12bits maximum
    Int_t Nbits; //Nbits
    Int_t NThresholds; // actually (2^Nbits)-1
    Bool_t ADC_linear; // 1 if the ADC has a linear gain, 0 otherwise.
    Float_t LSB; //if ADC is linear, threshold of the least significant bit (in Signal/Noise units)
    Float_t Electron_Conversion; // if ADC is linear, the conversion factor for 1 bit beyond the first (ADC count/(Signal/Noise))
    Float_t ADC_thresholds[adcnum]; //if ADC is NOT linear, All thresholds (in Signal/Noise units)

    //Int_t NumberOfPlanes;
    //    Int_t      FileCountOut;         // maximum number of files possible
  };

  ADCParameter_t  *pADCParameter;//! don''t put in streamer
  ADCParameter_t& GetADCPar(Int_t aPN) {return pADCParameter[aPN];} 

 
  //---to add:
  /*
    Temperature
    fluence (neq(1MeV)/cm2)
    radiation dose (krad)
    diodes number/surface
    epi resistivity
    CCE, transport, poisson law used
    delta.
    electronic Noise
    nombre de segments
    attenuation length
    reflexion coeeficient
    ionisation energiemaximum range of diffusion
    maximum length of a segment
    maximum charge of a segment
    gain
    suppression de zero
    fake rate. pixels chauds.
  */

 protected:
  
  TString      fConfigPath;                 // name of the configuration path
  TString      fConfigFileName;             // name of the configuration file
  TString      fConfigPathAndFileName;      // both path and file name appended
  ifstream     fConfigFileStream;
  
 
  ClassDef(DIGInitialize,1);
  
};
//==============================================================================
#endif
