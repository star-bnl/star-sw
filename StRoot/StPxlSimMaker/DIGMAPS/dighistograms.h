#ifndef MAPS_DIGHISTO_H
#define MAPS_DIGHISTO_H

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
#include "TProfile.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3F.h"
#include "TObjArray.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClonesArray.h"
#include "TCanvas.h"
#include "TGraphErrors.h"
#include "TProfile.h"
#include "Riostream.h"
#include "TSystem.h"
#include "TInterpreter.h"






using namespace std;

class DIGInitialize;
class DIGPlane;
class DIGADC;
class DIGBeam;
class DIGTransport;
class DIGParticle;
class DIGAction;
class DIGEvent;
//class DIGMAPS;
//==============================================================================
class DIGHistograms  {
  //class DIGHistograms {
 public:
  DIGHistograms();
  DIGHistograms(Int_t myNumberOfConfigurations);
  DIGHistograms(DIGHistograms& adighisto);
  virtual ~DIGHistograms();
  void    Clear(const Option_t * /*option*/ = "");
  void PrintInfo();

  Int_t GetNumberOfConfigurations(){return fNumberOfConfigs;}
  
  void SetNumberOfConfigurations(Int_t myNumberOfConfigurations);
  void BookHistograms(Int_t myNumberOfConfigurations);
  void PlotHistograms(Int_t myNumberOfConfigurations);
  //  friend class DIGMAPS;

  TH1F* AutoZoom(TH1F* H,Option_t* aType="all", Int_t EntryMin=0);
  TH2F* AutoZoom(TH2F* H,Option_t* aType="all", Int_t EntryMin=0);

 protected:




 //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 // Histogram list
 //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


 //----------------------
 // general histograms
 //----------------------
  TH1F *h1_NumberOfEventsPerConfiguration;
  TH1F *h1_NumberOfConfigurations;

 //----------------------
 // cluster histograms
 //----------------------
 Int_t fNumberOfConfigs; // number of configurations to study = nb of planes x nb of ADCs x nb  beams

 TObjArray *Ar_h1_multiplicity_with_threshold_01;
 TObjArray *Ar_h1_multiplicity_with_threshold_02;
 TObjArray *Ar_h1_multiplicity_with_threshold_03;
 TObjArray *Ar_h1_multiplicity_with_threshold_04;
 TObjArray *Ar_h1_multiplicity_with_threshold_05;
 TObjArray *Ar_h1_multiplicity_with_threshold_06;
 TObjArray *Ar_h1_multiplicity_with_threshold_07;
 TObjArray *Ar_h1_multiplicity_with_threshold_08;
 TObjArray *Ar_h1_multiplicity_with_threshold_09;
 TObjArray *Ar_h1_multiplicity_with_threshold_10;

 TObjArray *Ar_h1_multiplicity_with_threshold_15;
 TObjArray *Ar_h1_multiplicity_with_threshold_20;
 TObjArray *Ar_h1_multiplicity_with_threshold_25;
 TObjArray *Ar_h1_multiplicity_with_threshold_30;
 TObjArray *Ar_h1_multiplicity_with_threshold_40;


 TObjArray *Ar_h1_Cluster_SeedDigitalCharge;
 TObjArray *Ar_h1_Cluster_TotalDigitalCharge;
 TObjArray *Ar_h1_Cluster_9x9DigitalCharge;
 TObjArray *Ar_h1_Cluster_1stCrownDigitalCharge;
 TObjArray *Ar_h1_Cluster_2ndCrownDigitalCharge;
 TObjArray *Ar_h1_Cluster_4NeighboursDigitalCharge;
 TObjArray *Ar_h1_Cluster_SeedOverTotalDigitalCharge;
 TObjArray *Ar_h1_Cluster_1stCrownOverTotalDigitalCharge;
 TObjArray *Ar_h1_Cluster_2ndCrownOverTotalDigitalCharge;
 TObjArray *Ar_h1_Cluster_4NeighboursOverTotalDigitalCharge;
 

 TObjArray *Ar_h1_Particle_TotalAnalogCharge;
 TObjArray *Ar_h1_Particle_TotalDigitalCharge;
 TObjArray *Ar_h1_Particle_Energy_deposited;
 TObjArray *Ar_h2_Particle_EnergyDeposited_vs_TotalAnalogCharge;
 TObjArray *Ar_h2_Particle_TotalDigitalCharge_vs_TotalAnalogCharge;

 TObjArray *Ar_h1_test;

 TObjArray *Ar_h1_Resolution_ResidualX_CoG_true;
 TObjArray *Ar_h1_Resolution_ResidualY_CoG_true;
 TObjArray *Ar_h1_Resolution_Residualdist_CoG_true;
 TObjArray *Ar_h2_Resolution_TruePosition;
 TObjArray *Ar_h2_Resolution_TruePosition_modulo;
 TObjArray *Ar_h2_Resolution_CoG;
 TObjArray *Ar_h2_Resolution_CoG_modulo;

 TObjArray *Ar_Pr_Charge_Q_over_Qtot_vs_distance;
 TObjArray *Ar_h2_Charge_Q_over_Qtot_vs_distance;


 TObjArray *Ar_h1_ADC_LSB;

 TObjArray *Ar_h1_Efficiency_ideal;



  ClassDef(DIGHistograms,1);
};



//==============================================================================

#endif
