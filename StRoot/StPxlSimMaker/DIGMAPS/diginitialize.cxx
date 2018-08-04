///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//   DIGInitialize                                                                           //
//                                                                                           //
//   Class performing the initialization (reads the input data card and store it)            //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include <diginitialize.h>

#include <TROOT.h> // for gROOT object
#include <TMath.h>
#include <TMatrixD.h>
#include <TCanvas.h>
#include <TGraph.h>
#include <TAxis.h>
#include <TRandom3.h>
#include <TFile.h>
#include <TTree.h>
#include <TBranch.h>
#include <TClonesArray.h>
#include <TString.h>

#include <stdio.h>
#include <string.h>

using namespace std;

//==============================================================================
//==============================================================================
ClassImp(DIGInitialize)

DIGInitialize::DIGInitialize()  
  : TNamed("DIG Initialize","Initialize")
{
  //
  // default constructor
  //

}

//______________________________________________________________________________
//  

DIGInitialize::DIGInitialize(char *name, char *title, TString aCP, TString aCFN, TString action)  : TNamed(name,title)
{

// --- open config file:

  
  SetConfigPathAndFileName(aCP,aCFN); 
  printf(" Reading Setup from %s\n", fConfigPathAndFileName.Data());

  fConfigFileStream.open(fConfigPathAndFileName);
  Bool_t    answer=fConfigFileStream.fail();
  while( answer ) {
    cout << "enter correct file name \n";
    cin >> fConfigFileName;
    fConfigPathAndFileName = fConfigPath + fConfigFileName;
    printf(" Reading Setup from %s\n", fConfigPathAndFileName.Data());
    fConfigFileStream.open(fConfigPathAndFileName);
    answer=fConfigFileStream.fail();
  }

  // Char_t char_buffer; 

  //----------------------------------------------
  //--------------------READ Action PARAMETERS
  //----------------------------------------------
  Char_t c; 
  Int_t k;

  /*
  nextItem(':');
  nextItem('"'); k = 0;
  do {
    fConfigFileStream >> c;
    if ((c != '"') && (k < ActionParameter.actionnum)) {
      ActionParameter.Doit[k] = c;
      k++;
    }
  } while (c != '"');
  */
  //string copy to char:
  ActionParameter.Doit[action.Sizeof()]=0;
  string temp;
  temp = action;
  memcpy(ActionParameter.Doit,temp.c_str() ,action.Sizeof());

  nextItem(':');
  nextItem('"'); k = 0;
  do {
    fConfigFileStream >> c;
    if ((c != '"') && (k < ActionParameter.actionnum)) {
      ActionParameter.Model[k] = c;
      k++;
    }
  } while (c != '"');
  



  //----------------------------------------------
  //--------------------READ BEAM PARAMETERS
  //----------------------------------------------

  read_item(BeamParameter.RunNumber);
  read_item(BeamParameter.NumberOfEvents);
  read_item(BeamParameter.BeamOption);
  read_item(BeamParameter.ParticleDensity);
  //read_item(BeamParameter.NumberOfPlanes);
  read_item(BeamParameter.NAngles);
  nextItem(':');
  for (Int_t j = 0; j < BeamParameter.NAngles; j++) {
    fConfigFileStream >> BeamParameter.ThetaIncidentDeg[j];
  }
  nextItem(':');
  for (Int_t j = 0; j < BeamParameter.NAngles; j++) {
    fConfigFileStream >> BeamParameter.PhiIncidentDeg[j];
  }
  // printf(" Reading Setup from %s\n", fConfigPathAndFileName.Data());

  //----------------------------------------------
  //--------------------READ PLANE PARAMETERS
  //----------------------------------------------

  read_item(PlaneParameter.NGeom);

  nextItem(':');
  for (Int_t j = 0; j < PlaneParameter.NGeom; j++) {
    fConfigFileStream >> PlaneParameter.PitchX[j];
  }

  nextItem(':');
  for (Int_t j = 0; j < PlaneParameter.NGeom; j++) {
    fConfigFileStream >> PlaneParameter.PitchY[j];
  }

  nextItem(':');
  for (Int_t j = 0; j < PlaneParameter.NGeom; j++) {
    fConfigFileStream >> PlaneParameter.NoiseElectrons[j];
  }

  nextItem(':');
  for (Int_t j = 0; j < PlaneParameter.NGeom; j++) {
    fConfigFileStream >> PlaneParameter.EpitaxialThickness[j];
  }


  read_item(PlaneParameter.NPixelsX);
  read_item(PlaneParameter.NPixelsY);

  read_item(PlaneParameter.NTemperature);

  nextItem(':');
  for (Int_t j = 0; j < PlaneParameter.NTemperature; j++) {
    fConfigFileStream >> PlaneParameter.Temperature[j];
  }

  read_item(PlaneParameter.IonizationEnergy);
  read_item(PlaneParameter.SegmentSize);
  read_item(PlaneParameter.MaximumSegmentSize);
  read_item(PlaneParameter.MaximumChargePerSegment);
  read_item(PlaneParameter.DiffusionMaximumRangeInX);
  read_item(PlaneParameter.DiffusionMaximumRangeInY);
  read_item(PlaneParameter.ReflexionCoefficient);
  read_item(PlaneParameter.BasicModel_SigmaTenMicrons);

  //----------------------------------------------
  //--------------------READ TRANSPORT PARAMETERS
  //----------------------------------------------

  read_item(PlaneParameter.NTransport);
  pTransportParameter  = new TransportParameter_t[PlaneParameter.NTransport];

  //---loop over all TRANSPORT models
  for (Int_t p = 0; p < PlaneParameter.NTransport; p++) {
    read_item(pTransportParameter[p].ChargeModel);
    read_item(pTransportParameter[p].RangeLimit_InPitchUnit);
    if(pTransportParameter[p].ChargeModel==1){
      read_item(pTransportParameter[p].Lorentz2DModel_Cp0);
      read_item(pTransportParameter[p].Lorentz2DModel_Cp1);
    }
    if(pTransportParameter[p].ChargeModel==2){
      read_item(pTransportParameter[p].Gauss2DModel_sigma1_Cp0);
      read_item(pTransportParameter[p].Gauss2DModel_sigma1_Cp1);
      read_item(pTransportParameter[p].Gauss2DModel_sigma2_Cp0);
      read_item(pTransportParameter[p].Gauss2DModel_sigma2_Cp1);
      read_item(pTransportParameter[p].Gauss2DModel_weight);
    }
    if(pTransportParameter[p].ChargeModel==3){
      read_item(pTransportParameter[p].LorGaussModel_Norm1_Cp0);
      read_item(pTransportParameter[p].LorGaussModel_Norm1_Cp1);
      read_item(pTransportParameter[p].LorGaussModel_Norm1_Cp2);
      read_item(pTransportParameter[p].LorGaussModel_sigma_Cp0);
      read_item(pTransportParameter[p].LorGaussModel_sigma_Cp1);
      read_item(pTransportParameter[p].LorGaussModel_C_Cp0);
      read_item(pTransportParameter[p].LorGaussModel_C_Cp1);
      read_item(pTransportParameter[p].LorGaussModel_Norm_Cp0);
      read_item(pTransportParameter[p].LorGaussModel_Norm_Cp1);
    }
    if(pTransportParameter[p].ChargeModel==4){
      read_item(pTransportParameter[p].lorlorgausModel_Norm1_Cp0) ;
      read_item(pTransportParameter[p].lorlorgausModel_Norm1_Cp1) ;
      read_item(pTransportParameter[p].lorlorgausModel_x01_Cp0) ;
      read_item(pTransportParameter[p].lorlorgausModel_x01_Cp1) ;
      read_item(pTransportParameter[p].lorlorgausModel_sigmax1_Cp0) ;
      read_item(pTransportParameter[p].lorlorgausModel_sigmax1_Cp1) ;
      read_item(pTransportParameter[p].lorlorgausModel_y01_Cp0) ;
      read_item(pTransportParameter[p].lorlorgausModel_y01_Cp1) ;
      read_item(pTransportParameter[p].lorlorgausModel_sigmay1_Cp0) ;
      read_item(pTransportParameter[p].lorlorgausModel_sigmay1_Cp1) ;
      read_item(pTransportParameter[p].lorlorgausModel_Gamma_Cp0) ;
      read_item(pTransportParameter[p].lorlorgausModel_Gamma_Cp1) ;
      read_item(pTransportParameter[p].lorlorgausModel_x0_Cp0) ;
      read_item(pTransportParameter[p].lorlorgausModel_x0_Cp1) ;
      read_item(pTransportParameter[p].lorlorgausModel_y0_Cp0) ;
      read_item(pTransportParameter[p].lorlorgausModel_y0_Cp1) ;
      read_item(pTransportParameter[p].lorlorgausModel_norm_Cp0) ;
      read_item(pTransportParameter[p].lorlorgausModel_norm_Cp1) ;
      read_item(pTransportParameter[p].lorlorgausModel_normgaus2_Cp0) ;
      read_item(pTransportParameter[p].lorlorgausModel_normgaus2_Cp1) ;
      read_item(pTransportParameter[p].lorlorgausModel_sigma2_Cp0) ;
      read_item(pTransportParameter[p].lorlorgausModel_sigma2_Cp1) ;
    }
    if(pTransportParameter[p].ChargeModel==5){
      read_item(pTransportParameter[p].l1dimgauslor_Norm_g_1st);
      read_item(pTransportParameter[p].l1dimgauslor_x0_g_1st);
      read_item(pTransportParameter[p].l1dimgauslor_sigma_g_1st);
      read_item(pTransportParameter[p].l1dimgauslor_Gamma_lor_1st);
      read_item(pTransportParameter[p].l1dimgauslor_x0_lor_1st);
      read_item(pTransportParameter[p].l1dimgauslor_norm_lor_1st);
      read_item(pTransportParameter[p].l1dimgauslor_Norm_g_2nd);
      read_item(pTransportParameter[p].l1dimgauslor_x0_g_2nd);
      read_item(pTransportParameter[p].l1dimgauslor_sigma_g_2nd);
      read_item(pTransportParameter[p].l1dimgauslor_Gamma_lor_2nd);
      read_item(pTransportParameter[p].l1dimgauslor_x0_lor_2nd);
      read_item(pTransportParameter[p].l1dimgauslor_norm_lor_2nd);
    }
  }
  //----------------------------------------------
  //READ ADC PARAMETERS
  //----------------------------------------------
 
  read_item(PlaneParameter.NADC);
  pADCParameter  = new ADCParameter_t[PlaneParameter.NADC];

  //---loop over all ADCs
  for (Int_t p = 0; p < PlaneParameter.NADC; p++) {
    //---read number of bits of the ADC:
    read_item(pADCParameter[p].Nbits);
    //----compute number of thresholds needed:
    pADCParameter[p].NThresholds = int(TMath::Power(2.0,pADCParameter[p].Nbits) - 1); 
    read_item(pADCParameter[p].ADC_linear);
    if(pADCParameter[p].ADC_linear){ //-------linear case (enter LSB and step size)
      read_item(pADCParameter[p].LSB);
      read_item(pADCParameter[p].Electron_Conversion);      
      nextItem(':');
      for (Int_t j = 0; j < pADCParameter[p].NThresholds; j++) {
	pADCParameter[p].ADC_thresholds[j] = pADCParameter[p].LSB+float(j)* (pADCParameter[p].Electron_Conversion);
      } 
    }else{    //non linear case (read each value instead)
      nextItem(':');
      nextItem(':');     
      nextItem(':');     
      for (Int_t j = 0; j < pADCParameter[p].NThresholds; j++) {
	fConfigFileStream >> pADCParameter[p].ADC_thresholds[j];
      } 
      pADCParameter[p].LSB=pADCParameter[p].ADC_thresholds[0];     
      pADCParameter[p].Electron_Conversion = -999; // dummy value since not needed.
    }
  }


  PrintInfo();

  //----------------------------------------------
  //Configure
  //----------------------------------------------




}
//______________________________________________________________________________
//  


DIGInitialize::~DIGInitialize() { // 
  // virtual destructor
  //
  //  delete fLayers;
}



//______________________________________________________________________________
//  
void  DIGInitialize::SetConfigPath(TString aCP) 
{ 
  fConfigPath = aCP;  
}


//______________________________________________________________________________
//  
void DIGInitialize::SetConfigFileName(TString aCFN) 
{ 
  fConfigFileName = aCFN;     
} 

//______________________________________________________________________________
//  
void DIGInitialize::SetConfigPathAndFileName() 
{ 
  fConfigPathAndFileName = fConfigPath + fConfigFileName;     
} 

//______________________________________________________________________________
//  
void DIGInitialize::SetConfigPathAndFileName(TString aCP,TString aCFN) 
{ 
  fConfigPath = aCP;  
  fConfigFileName = aCFN;     
  fConfigPathAndFileName = fConfigPath + fConfigFileName;  
} 
//______________________________________________________________________________
//  
TString  DIGInitialize::GetConfigPath() 
{ 
  return fConfigPath;  
}
//______________________________________________________________________________
//  
TString DIGInitialize::GetConfigFileName() 
{ 
  return fConfigFileName;     
} 
//______________________________________________________________________________
//  
TString DIGInitialize::GetConfigPathAndFileName() 
{ 
  return fConfigPathAndFileName;
} 
//______________________________________________________________________________
//  

void DIGInitialize::PrintInfo() {

  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<"---------Action configuration------------- "<<endl;
  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<" Action: "<<ActionParameter.Doit <<endl;
  //printf("%s\n",ActionParameter.Doit);
  std::cout<<" Model: "<<ActionParameter.Model << endl;
  //printf("%s\n",ActionParameter.Model);
  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<"---------General configuration------------- "<<endl;
  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<"-- RunNumber: "<<BeamParameter.RunNumber<<endl;
  std::cout<<"-- NumberOfEvents: "<<BeamParameter.NumberOfEvents<<endl;
  std::cout<<"-- BeamOption: "<<BeamParameter.BeamOption<<endl;
  std::cout<<"-- ParticleDensity: "<<BeamParameter.ParticleDensity<<endl;
  std::cout<<"--Number of different Angles: "<<BeamParameter.NAngles<<endl;
  for (Int_t j = 0; j < BeamParameter.NAngles; j++) {
    std::cout<<" Theta(deg): "<<BeamParameter.ThetaIncidentDeg[j]<<" Phi(deg): "<<BeamParameter.PhiIncidentDeg[j]<<endl;
  }
  //std::cout<<" NumberOfPlanes: "<<BeamParameter.NumberOfPlanes<<endl;
  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<"----------Plane configuration-------------- "<<endl;
  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<"--Number of different Geometries: "<<PlaneParameter.NGeom<<endl;
  for (Int_t j = 0; j < PlaneParameter.NGeom ; j++) {
    std::cout<<" PitchX: "<<PlaneParameter.PitchX[j]<<" PitchY: "<<PlaneParameter.PitchY[j]<<endl;
    std::cout<<" Epitaxial_thickness: "<<PlaneParameter.EpitaxialThickness[j]<<endl;
    std::cout<<" Noise: "<<PlaneParameter.NoiseElectrons[j]<<endl;
  }

  std::cout<<"--Number pixels in X and Y: "<<PlaneParameter.NPixelsX<<" x "<<PlaneParameter.NPixelsY<<endl;

  std::cout<<"--Number different temperatures "<<PlaneParameter.NTemperature<<endl;
  for (Int_t j = 0; j < PlaneParameter.NTemperature; j++) {
    std::cout<<" Temperature: "<<PlaneParameter.Temperature[j]<<endl;
  }
  std::cout<<"--IonizationEnergy "<< PlaneParameter.IonizationEnergy<<endl;
  std::cout<<"--SegmentSize  "<<PlaneParameter.SegmentSize<<endl;
  std::cout<<"--MaximumSegmentSize  "<<PlaneParameter.MaximumSegmentSize<<endl;
  std::cout<<"--MaximumChargePerSegment  "<<PlaneParameter.MaximumChargePerSegment<<endl;
  std::cout<<"--DiffusionMaximumRangeInX  "<<PlaneParameter.DiffusionMaximumRangeInX<<endl;
  std::cout<<"--DiffusionMaximumRangeInY  "<<PlaneParameter.DiffusionMaximumRangeInY<<endl;
  std::cout<<"--ReflexionCoefficient  "<<PlaneParameter.ReflexionCoefficient<<endl;
  std::cout<<"--BasicModel_SigmaTenMicrons  "<<PlaneParameter.BasicModel_SigmaTenMicrons<<endl;
  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<"----------Transport configuration-------------- "<<endl;
  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<"-- Number of Transport models = "<<PlaneParameter.NTransport<<endl;
  for (Int_t p = 0; p < PlaneParameter.NTransport; p++) {
    std::cout<<"-> Transport Model number "<<p<<endl;
    std::cout<<"--ChargeModel "<<pTransportParameter[p].ChargeModel<<endl;
    std::cout<<"--RangeLimit_InPitchUnit "<<pTransportParameter[p].RangeLimit_InPitchUnit<<endl;
    if(pTransportParameter[p].ChargeModel==1){
      std::cout<<"--Lorentz2DModel_Cp0 "<<pTransportParameter[p].Lorentz2DModel_Cp0<<endl;
      std::cout<<"--Lorentz2DModel_Cp1 "<<pTransportParameter[p].Lorentz2DModel_Cp1<<endl;
    }
    if(pTransportParameter[p].ChargeModel==2){
      std::cout<<"--Gauss2DModel_sigma1_Cp0 "<<pTransportParameter[p].Gauss2DModel_sigma1_Cp0<<endl;
      std::cout<<"--Gauss2DModel_sigma1_Cp1 "<<pTransportParameter[p].Gauss2DModel_sigma1_Cp1<<endl;
      std::cout<<"--Gauss2DModel_sigma2_Cp0 "<<pTransportParameter[p].Gauss2DModel_sigma2_Cp0<<endl;
      std::cout<<"--Gauss2DModel_sigma2_Cp1 "<<pTransportParameter[p].Gauss2DModel_sigma2_Cp1<<endl;
      std::cout<<"--Gauss2DModel_weight "<<pTransportParameter[p].Gauss2DModel_weight<<endl;
    }
    if(pTransportParameter[p].ChargeModel==3){
      std::cout<<"--LorGaussModel_Norm1_Cp0 "<<pTransportParameter[p].LorGaussModel_Norm1_Cp0<<endl;
      std::cout<<"--LorGaussModel_Norm1_Cp1 "<<pTransportParameter[p].LorGaussModel_Norm1_Cp1<<endl;
      std::cout<<"--LorGaussModel_Norm1_Cp2 "<<pTransportParameter[p].LorGaussModel_Norm1_Cp2<<endl;
      std::cout<<"--LorGaussModel_sigma_Cp0 "<<pTransportParameter[p].LorGaussModel_sigma_Cp0<<endl;
      std::cout<<"--LorGaussModel_sigma_Cp1 "<<pTransportParameter[p].LorGaussModel_sigma_Cp1<<endl;
      std::cout<<"--LorGaussModel_C_Cp0 "<<pTransportParameter[p].LorGaussModel_C_Cp0<<endl;
      std::cout<<"--LorGaussModel_C_Cp1 "<<pTransportParameter[p].LorGaussModel_C_Cp1<<endl;
      std::cout<<"--LorGaussModel_Norm_Cp0 "<<pTransportParameter[p].LorGaussModel_Norm_Cp0<<endl;
      std::cout<<"--LorGaussModel_Norm_Cp1 "<<pTransportParameter[p].LorGaussModel_Norm_Cp1<<endl;
    }
    if(pTransportParameter[p].ChargeModel==4){
     std::cout<<"--lorlorgausModel_Norm1_Cp0 "<<pTransportParameter[p].lorlorgausModel_Norm1_Cp0<<endl;
     std::cout<<"--lorlorgausModel_Norm1_Cp1  "<<pTransportParameter[p].lorlorgausModel_Norm1_Cp1<<endl;
     std::cout<<"--lorlorgausModel_x01_Cp0 "<<pTransportParameter[p].lorlorgausModel_x01_Cp0<<endl;
     std::cout<<"--lorlorgausModel_x01_Cp1 "<<pTransportParameter[p].lorlorgausModel_x01_Cp1<<endl;
     std::cout<<"--lorlorgausModel_sigmax1_Cp0 "<<pTransportParameter[p].lorlorgausModel_sigmax1_Cp0<<endl;
     std::cout<<"--lorlorgausModel_sigmax1_Cp1 "<<pTransportParameter[p].lorlorgausModel_sigmax1_Cp1<<endl;
     std::cout<<"--lorlorgausModel_y01_Cp0 "<<pTransportParameter[p].lorlorgausModel_y01_Cp0<<endl;
     std::cout<<"--lorlorgausModel_y01_Cp1 "<<pTransportParameter[p].lorlorgausModel_y01_Cp1<<endl;
     std::cout<<"--lorlorgausModel_sigmay1_Cp0 "<<pTransportParameter[p].lorlorgausModel_sigmay1_Cp0<<endl;
     std::cout<<"--lorlorgausModel_sigmay1_Cp1 "<<pTransportParameter[p].lorlorgausModel_sigmay1_Cp1<<endl;
     std::cout<<"--lorlorgausModel_Gamma_Cp0 "<<pTransportParameter[p].lorlorgausModel_Gamma_Cp0<<endl;
     std::cout<<"--lorlorgausModel_Gamma_Cp1 "<<pTransportParameter[p].lorlorgausModel_Gamma_Cp1<<endl;
     std::cout<<"--lorlorgausModel_x0_Cp0 "<<pTransportParameter[p].lorlorgausModel_x0_Cp0<<endl;
     std::cout<<"--lorlorgausModel_x0_Cp1 "<<pTransportParameter[p].lorlorgausModel_x0_Cp1<<endl;
     std::cout<<"--lorlorgausModel_y0_Cp0 "<<pTransportParameter[p].lorlorgausModel_y0_Cp0<<endl;
     std::cout<<"--lorlorgausModel_y0_Cp1 "<<pTransportParameter[p].lorlorgausModel_y0_Cp1<<endl;
     std::cout<<"--lorlorgausModel_norm_Cp0 "<<pTransportParameter[p].lorlorgausModel_norm_Cp0<<endl;
     std::cout<<"--lorlorgausModel_norm_Cp1 "<<pTransportParameter[p].lorlorgausModel_norm_Cp1<<endl;
     std::cout<<"--lorlorgausModel_normgaus2_Cp0 "<<pTransportParameter[p].lorlorgausModel_normgaus2_Cp0<<endl;
     std::cout<<"--lorlorgausModel_normgaus2_Cp1 "<<pTransportParameter[p].lorlorgausModel_normgaus2_Cp1<<endl;
     std::cout<<"--lorlorgausModel_sigma2_Cp0 "<<pTransportParameter[p].lorlorgausModel_sigma2_Cp0<<endl;
     std::cout<<"--lorlorgausModel_sigma2_Cp1 "<<pTransportParameter[p].lorlorgausModel_sigma2_Cp1<<endl;
    }
    if(pTransportParameter[p].ChargeModel==5){
      std::cout<<"--l1dimgauslor_Norm_g_1st "<<pTransportParameter[p].l1dimgauslor_Norm_g_1st<<endl;
      std::cout<<"--l1dimgauslor_x0_g_1st "<<pTransportParameter[p].l1dimgauslor_x0_g_1st<<endl;
      std::cout<<"--l1dimgauslor_sigma_g_1st "<<pTransportParameter[p].l1dimgauslor_sigma_g_1st<<endl;
      std::cout<<"--l1dimgauslor_Gamma_lor_1st "<<pTransportParameter[p].l1dimgauslor_Gamma_lor_1st<<endl;
      std::cout<<"--l1dimgauslor_x0_lor_1st "<<pTransportParameter[p].l1dimgauslor_x0_lor_1st<<endl;
      std::cout<<"--l1dimgauslor_norm_lor_1st "<<pTransportParameter[p].l1dimgauslor_norm_lor_1st<<endl;
      std::cout<<"--l1dimgauslor_Norm_g_2nd "<<pTransportParameter[p].l1dimgauslor_Norm_g_2nd<<endl;
      std::cout<<"--l1dimgauslor_x0_g_2nd "<<pTransportParameter[p].l1dimgauslor_x0_g_2nd<<endl;
      std::cout<<"--l1dimgauslor_sigma_g_2nd "<<pTransportParameter[p].l1dimgauslor_sigma_g_2nd<<endl;
      std::cout<<"--l1dimgauslor_Gamma_lor_2nd "<<pTransportParameter[p].l1dimgauslor_Gamma_lor_2nd<<endl;
      std::cout<<"--l1dimgauslor_x0_lor_2nd "<<pTransportParameter[p].l1dimgauslor_x0_lor_2nd<<endl;
      std::cout<<"--l1dimgauslor_norm_lor_2nd "<<pTransportParameter[p].l1dimgauslor_norm_lor_2nd<<endl;
    }
  }
  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<"-----------ADC configuration---------------  "<<endl;
  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<"-- Number of ADCs = "<<PlaneParameter.NADC<<endl;
  for (Int_t p = 0; p < PlaneParameter.NADC; p++) {
    std::cout<<"-> ADC "<<p<<endl;
    std::cout<<"Nbits= "<<pADCParameter[p].Nbits<<"; ADC_linear="<<pADCParameter[p].ADC_linear<<endl;
    std::cout<<"NThresholds= "<<pADCParameter[p].NThresholds<<"; LSB= "<<pADCParameter[p].LSB <<endl;
    std::cout<<"Electron_Conversion= "<<pADCParameter[p].Electron_Conversion<<endl;
    Int_t Nthtoprint = pADCParameter[p].NThresholds;
    if(Nthtoprint < 33){
      for (Int_t j = 0; j < pADCParameter[p].NThresholds; j++) {
	std::cout<<"thresholds["<<j<<"]="<<pADCParameter[p].ADC_thresholds[j]<<endl;
      } 
    }else{
      Nthtoprint = 32;
      for (Int_t j = 0; j < Nthtoprint; j++) {
	std::cout<<"thresholds["<<j<<"]="<<pADCParameter[p].ADC_thresholds[j]<<endl;
      } 
      std::cout<<" etc. "<<endl;  
    }
  }

  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<"------------------------------------------- "<<endl;

  std::cout<<" "<<endl;
  std::cout<<"------------------------------------------- "<<endl;
  std::cout<<"----------END OF READ OUT FILE------------- "<<endl;
  std::cout<<"------------------------------------------- "<<endl;
}

//______________________________________________________________________________
//  
void DIGInitialize::nextItem(Char_t delimiter)
{
  Char_t c;
  do {
    fConfigFileStream >> c;
  } while (c != delimiter);  
}


//______________________________________________________________________________
//  
void DIGInitialize::read_item(Int_t &arg)
{
  nextItem(':');
  fConfigFileStream >> arg;
}

//______________________________________________________________________________
//  
void DIGInitialize::read_item(Float_t &arg)
{

  // reads values from configuration file

  nextItem(':');
  fConfigFileStream >> arg;
}
//______________________________________________________________________________
//  
void DIGInitialize::read_item(Double_t &arg)
{

  // reads values from configuration file

  nextItem(':');
  fConfigFileStream >> arg;
}
//______________________________________________________________________________
//  
void DIGInitialize::read_item(Bool_t &arg)
{

  // reads values from configuration file

  nextItem(':');
  fConfigFileStream >> arg;
}
//______________________________________________________________________________
//  
