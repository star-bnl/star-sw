///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//     DIGMAPS                                                                               //
//                                                                                           //
//    Main Class of DIGMAPS                                                                  //
// Contains pointers to all other classes and to the root tree                               //
//   units are all in micrometer.                                                            //
//                                                                                           //
//     contains:                                                                             //
//    - Run() function (loop on all configurations)                                          //
//    - ActionPlot() function (plot a configuration)                                         //
//    - RunConfiguration() loop on all events for a given configuration                      //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include <digmaps.h>
#include <TStopwatch.h>
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
#include <TPaveStats.h>
#include <string.h>

#include "digresult.h"
#include "digaction.h"
#include "digadc.h"
#include "digbeam.h"
#include "digtransport.h"
#include "digevent.h"
#include "diginitialize.h"
#include "digparticle.h"
#include "digcluster.h"
#include "digplane.h"
#include "dighistograms.h"


extern Int_t GlobalSeed = 1;

//==============================================================================
ClassImp(DIGMAPS)
DIGMAPS::DIGMAPS()  
  : TNamed("DIGMAPS","DIGMAPS title")
{
  //
  // default constructor
  //
}
//______________________________________________________________________________
//  

DIGMAPS::DIGMAPS(char *name, char *title, TString aCP, TString aCFN,  TString outp, TString outf, TString action )  : TNamed(name,title)
{
  timer1.Start();
  time_t seconds;
  seconds = time(NULL); 
  gRandom->SetSeed(seconds);
  GlobalSeed=seconds;
  ColorChosen=1;

  SetConfigPathAndFileName(aCP,aCFN); 
  SetIsOutputfile(1);
  SetOutputPathAndFileName(outp,outf) ;
  SetfAction(action);
  std::cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<endl;
  std::cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<endl;
  std::cout<<"++                                                           ++"<<endl;
  std::cout<<"++                                                           ++"<<endl;
  std::cout<<"++                   WELCOME to DIGMAPS                      ++"<<endl;
  std::cout<<"++                                                           ++"<<endl;
  std::cout<<"++                                                           ++"<<endl;
  std::cout<<"++         Version: 1.01    Date: March 29th 2011            ++"<<endl;
  std::cout<<"++         Author: Auguste Besson abesson@in2p3.fr           ++"<<endl;
  std::cout<<"++                   (WITH OUTPUT FILE)                      ++"<<endl;
  std::cout<<"++                                                           ++"<<endl;
  std::cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<endl;
  std::cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<endl;
  fDIGPlaneArray=0;
  fDIGADCArray=0;
  fDIGBeamArray=0;
  fDIGTransportArray=0;
  fDIGResultArray=0;
  ffile=0;
  ftree=0;
  fDIGInitialize=0;
  fdigbeam=0;
  fdigplane=0;
  fdigadc=0;
  fdigresult=0;

  Run();
  timer1.Stop();
  cout<<" REAL TIME = "<<timer1.RealTime()<<" ; CPU TIME = "<<timer1.CpuTime()<<endl;

}
//______________________________________________________________________________
//  

DIGMAPS::DIGMAPS(char *name, char *title, TString aCP, TString aCFN , TString action )  : TNamed(name,title)
{
  timer1.Start();

  time_t seconds;
  seconds = time(NULL); 
  gRandom->SetSeed(seconds);
  GlobalSeed=seconds;
  ColorChosen=1;

  SetConfigPathAndFileName(aCP,aCFN); 
  SetIsOutputfile(0);
  SetfAction(action);
  std::cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<endl;
  std::cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<endl;
  std::cout<<"++                                                           ++"<<endl;
  std::cout<<"++                                                           ++"<<endl;
  std::cout<<"++                   WELCOME to DIGMAPS                      ++"<<endl;
  std::cout<<"++                                                           ++"<<endl;
  std::cout<<"++                                                           ++"<<endl;
  std::cout<<"++         Version: 1.01    Date: March 29th 2011            ++"<<endl;
  std::cout<<"++         Author: Auguste Besson abesson@in2p3.fr           ++"<<endl;
  std::cout<<"++               (WITHOUT OUTPUT FILE)                       ++"<<endl;
  std::cout<<"++                                                           ++"<<endl;
  std::cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<endl;
  std::cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<endl;
  fDIGPlaneArray=0;
  fDIGADCArray=0;
  fDIGBeamArray=0;
  fDIGTransportArray=0;
  fDIGResultArray=0;
  ffile=0;
  ftree=0;
  fDIGInitialize=0;
  fdigbeam=0;
  fdigplane=0;
  fdigadc=0;
  fdigresult=0;

  Run();
  timer1.Stop();

  cout<<" REAL TIME = "<<timer1.RealTime()<<" ; CPU TIME = "<<timer1.CpuTime()<<endl;
}
//______________________________________________________________________________
//  
DIGMAPS::~DIGMAPS() { // 
  // virtual destructor
  //
}
//______________________________________________________________________________
//  
void  DIGMAPS::Run() 
{ 

  //--------read configuration
  fDIGInitialize   = new DIGInitialize("read","test",fConfigPath,fConfigFileName,fAction);
  //print output in file:
  std::streambuf* OldBuf=0;  
  std::cout<<"config file = "<<GetConfigPathAndFileName()<<endl;
  if(GetIsOutputfile()==1){
    std::cout<<"output file = "<<GetOutputPathAndFileName()<<endl;
    fOutputFileStream.open(fOutputPathAndFileName, ios::out);
    
    OldBuf = std::cout.rdbuf(fOutputFileStream.rdbuf());
    fDIGInitialize->PrintInfo();
  }else{
    std::cout<<"NO output file "<<endl;;
  }
  //---------------------------
 
  //--------Read action

  Char_t *myDoit =0;  //= (fDIGInitialize->GetActionPar()).Doit;
  Char_t *myModel=0; // = (fDIGInitialize->GetActionPar()).Model;
  myDoit = strdup((fDIGInitialize->GetActionPar()).Doit);
  myModel  = strdup((fDIGInitialize->GetActionPar()).Model);

  DIGAction *aDIGAction =0;
  aDIGAction = new DIGAction(myDoit,myModel);
  SetAction(aDIGAction);

  std::cout<<" DIGMAPS:Run:  Action ="<<myDoit<<" ; Model = "<<myModel<<endl;
 
  Int_t myAction = 0;
  Char_t action[200] = "foresee";
  // strlen(action);
  if(!strncmp( action, myDoit,strlen(action))  ){
    cout<<" WE CAN DO THAT !"<<endl;
    myAction=1;
  }
  Char_t action2[200] = "plot";
  if(!strncmp( action2, myDoit,strlen(action2))  ){
    cout<<" WE CAN PLOT THAT !"<<endl;
    myAction=2;
  }

  //---compute number of Beams to create
  Int_t TotalNumberOfBeams = 0;
  TotalNumberOfBeams  = (fDIGInitialize->GetBeamPar().NAngles);
  std::cout<<" DIGMAPS:Run TotalNumberOfBeams = "<<TotalNumberOfBeams<<endl;
  //---------------------------

  //---compute number of planes to create.
  Int_t TotalNumberOfPlanes = 0;
  TotalNumberOfPlanes = (fDIGInitialize->GetPlanePar().NGeom)
    * (fDIGInitialize->GetPlanePar().NTemperature);
  std::cout<<" DIGMAPS:Run TotalNumberOfPlanes = "<<TotalNumberOfPlanes<<endl;
  //---------------------------

  //---compute number of ADCs to create
  Int_t TotalNumberOfADCs = 0;
  TotalNumberOfADCs  = (fDIGInitialize->GetPlanePar().NADC);

  std::cout<<" DIGMAPS:Run TotalNumberOfADCs = "<<TotalNumberOfADCs<<endl;
  //---------------------------
 //---compute number of transport model to create
  Int_t TotalNumberOfTransports = 0;
  TotalNumberOfTransports  = (fDIGInitialize->GetPlanePar().NTransport);

  std::cout<<" DIGMAPS:Run TotalNumberOfTransports = "<<TotalNumberOfTransports<<endl;
 



  //-------create all Beams
  fBeamN = TotalNumberOfBeams;
  fDIGBeamArray  = new TObjArray(fBeamN);
  DIGBeam *aDIGBeam=0;              // a pointer to an ADC
  //---------------------------
  
  //-------create all planes
  fPlanesN = TotalNumberOfPlanes;
  fDIGPlaneArray  = new TObjArray(fPlanesN);
  DIGPlane *aDIGPlane=0;              // a pointer to a plane
  //---------------------------

  //-------create all ADCs
  fADCN = TotalNumberOfADCs;
  fDIGADCArray  = new TObjArray(fADCN);
  DIGADC *aDIGADC=0;              // a pointer to an ADC
  //---------------------------


  //-------create all ADCs
  fTransportN = TotalNumberOfTransports;
  fDIGTransportArray = new TObjArray(fTransportN);
  DIGTransport *aDIGTransport=0;              // a pointer to a transport model
  //---------------------------

  //----loop on all beams
  for (Int_t ibeam = 0; ibeam < (fDIGInitialize->GetBeamPar().NAngles) ; ibeam++){
    aDIGBeam = new  DIGBeam(fDIGInitialize->GetBeamPar().RunNumber,
			    fDIGInitialize->GetBeamPar().NumberOfEvents,			    
			    fDIGInitialize->GetBeamPar().ParticleDensity,
			    fDIGInitialize->GetBeamPar().ThetaIncidentDeg[ibeam],
			    fDIGInitialize->GetBeamPar().PhiIncidentDeg[ibeam],
			    fDIGInitialize->GetBeamPar().BeamOption);
    fDIGBeamArray->Add(aDIGBeam);
  }
  //---------------------------

  //----loop on all beams
  /*
    for (Int_t ibeam = 0; ibeam < TotalNumberOfBeams ; ibeam++){
    ((DIGBeam*)fDIGBeamArray->At(ibeam))->PrintInfo();
    GetBeam(ibeam)->PrintInfo();
    }
  */
  //----loop on all ADCs
  for (Int_t iADC = 0; iADC < (fDIGInitialize->GetPlanePar().NADC) ; iADC++){
    // std::cout<<" "<<iADC<<" "<<(fDIGInitialize->GetPlanePar().NADC)<<endl;
    //read ADC parameters
    aDIGADC = new DIGADC();
    aDIGADC->SetNbits(fDIGInitialize->GetADCPar(iADC).Nbits);
    //std::cout<<"N bits "<<fDIGInitialize->GetADCPar(iADC).Nbits<<" "<<aDIGADC->GetNbits()<<endl;
    aDIGADC->SetNThresholds(fDIGInitialize->GetADCPar(iADC).NThresholds);
    //std::cout<<"NThresholds "<<fDIGInitialize->GetADCPar(iADC).NThresholds<<" "<<aDIGADC->GetNThresholds()<<endl;
    aDIGADC->SetADC_linear(fDIGInitialize->GetADCPar(iADC).ADC_linear);
    //std::cout<<"ADC_linear "<<fDIGInitialize->GetADCPar(iADC).ADC_linear<<" "<<aDIGADC->GetADC_linear()<<endl;
    aDIGADC->SetLSB(fDIGInitialize->GetADCPar(iADC).LSB);
    //std::cout<<"LSB "<<fDIGInitialize->GetADCPar(iADC).LSB<<" "<<aDIGADC->GetLSB() <<endl;
    aDIGADC->SetElectron_Conversion(fDIGInitialize->GetADCPar(iADC).Electron_Conversion);
    //std::cout<<"electron_Conversion "<<fDIGInitialize->GetADCPar(iADC).Electron_Conversion<<" "<<aDIGADC->GetElectron_Conversion() <<endl;
    //for (Int_t i = 0; i <aDIGADC->GetNThresholds()  ; i++){
    //  std::cout<<"tADC_thresholds n"<<i<<" ="<<fDIGInitialize->GetADCPar(iADC).ADC_thresholds[i]<<endl;
    //}
    aDIGADC->SetADC_thresholds( (fDIGInitialize->GetADCPar(iADC).ADC_thresholds),fDIGInitialize->GetADCPar(iADC).NThresholds);
    //std::cout<<"tADC_thresholds " <<endl;
    // aDIGADC->PrintInfo();
    fDIGADCArray->Add(aDIGADC);
  }
   
  //  std::cout<<"2nd loop on ADCs"<<endl;
  /*
    for (Int_t iADC = 0; iADC < (fDIGInitialize->GetPlanePar().NADC) ; iADC++){
    ((DIGADC*)fDIGADCArray->At(iADC))->PrintInfo();
    }
  */
  //---------------------------
 //----loop on all transport models
  for (Int_t itrans = 0; itrans < (fDIGInitialize->GetPlanePar().NTransport) ; itrans++){    
    aDIGTransport = new DIGTransport();
    aDIGTransport->SetChargeModel(fDIGInitialize->GetTransportPar(itrans).ChargeModel );
    aDIGTransport->SetRangeLimit_InPitchUnit(fDIGInitialize->GetTransportPar(itrans).RangeLimit_InPitchUnit);
    if(aDIGTransport->GetChargeModel()==1){
      aDIGTransport->SetLorentz2DModel_Cp0(fDIGInitialize->GetTransportPar(itrans).Lorentz2DModel_Cp0);
      aDIGTransport->SetLorentz2DModel_Cp1(fDIGInitialize->GetTransportPar(itrans).Lorentz2DModel_Cp1);
    }
    if(aDIGTransport->GetChargeModel()==2){
      aDIGTransport->SetGauss2DModel_sigma1_Cp0(fDIGInitialize->GetTransportPar(itrans).Gauss2DModel_sigma1_Cp0);
      aDIGTransport->SetGauss2DModel_sigma1_Cp1(fDIGInitialize->GetTransportPar(itrans).Gauss2DModel_sigma1_Cp1);
      aDIGTransport->SetGauss2DModel_sigma2_Cp0(fDIGInitialize->GetTransportPar(itrans).Gauss2DModel_sigma2_Cp0);
      aDIGTransport->SetGauss2DModel_sigma2_Cp1(fDIGInitialize->GetTransportPar(itrans).Gauss2DModel_sigma2_Cp1);
      aDIGTransport->SetGauss2DModel_weight(fDIGInitialize->GetTransportPar(itrans).Gauss2DModel_weight);
    }
    if(aDIGTransport->GetChargeModel()==3){
      aDIGTransport->SetLorGaussModel_Norm1_Cp0(fDIGInitialize->GetTransportPar(itrans).LorGaussModel_Norm1_Cp0);
      aDIGTransport->SetLorGaussModel_Norm1_Cp1(fDIGInitialize->GetTransportPar(itrans).LorGaussModel_Norm1_Cp1);
      aDIGTransport->SetLorGaussModel_Norm1_Cp2(fDIGInitialize->GetTransportPar(itrans).LorGaussModel_Norm1_Cp2);
      aDIGTransport->SetLorGaussModel_sigma_Cp0(fDIGInitialize->GetTransportPar(itrans).LorGaussModel_sigma_Cp0);
      aDIGTransport->SetLorGaussModel_sigma_Cp1(fDIGInitialize->GetTransportPar(itrans).LorGaussModel_sigma_Cp1);
      aDIGTransport->SetLorGaussModel_C_Cp0(fDIGInitialize->GetTransportPar(itrans).LorGaussModel_C_Cp0);
      aDIGTransport->SetLorGaussModel_C_Cp1(fDIGInitialize->GetTransportPar(itrans).LorGaussModel_C_Cp1);
      aDIGTransport->SetLorGaussModel_Norm_Cp0(fDIGInitialize->GetTransportPar(itrans).LorGaussModel_Norm_Cp0);
      aDIGTransport->SetLorGaussModel_Norm_Cp1(fDIGInitialize->GetTransportPar(itrans).LorGaussModel_Norm_Cp1);
    }
   
    if(aDIGTransport->GetChargeModel()==4){
      aDIGTransport->SetlorlorgausModel_Norm1_Cp0(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_Norm1_Cp0);
      aDIGTransport->SetlorlorgausModel_Norm1_Cp1(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_Norm1_Cp1);
      aDIGTransport->SetlorlorgausModel_x01_Cp0(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_x01_Cp0);
      aDIGTransport->SetlorlorgausModel_x01_Cp1(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_x01_Cp1);
      aDIGTransport->SetlorlorgausModel_sigmax1_Cp0(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_sigmax1_Cp0);
      aDIGTransport->SetlorlorgausModel_sigmax1_Cp1(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_sigmax1_Cp1);
      aDIGTransport->SetlorlorgausModel_y01_Cp0(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_y01_Cp0);
      aDIGTransport->SetlorlorgausModel_y01_Cp1(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_y01_Cp1);
      aDIGTransport->SetlorlorgausModel_sigmay1_Cp0(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_sigmay1_Cp0);
      aDIGTransport->SetlorlorgausModel_sigmay1_Cp1(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_sigmay1_Cp1);
      aDIGTransport->SetlorlorgausModel_Gamma_Cp0(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_Gamma_Cp0);
      aDIGTransport->SetlorlorgausModel_Gamma_Cp1(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_Gamma_Cp1);
      aDIGTransport->SetlorlorgausModel_x0_Cp0(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_x0_Cp0);
      aDIGTransport->SetlorlorgausModel_x0_Cp1(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_x0_Cp1);
      aDIGTransport->SetlorlorgausModel_y0_Cp0(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_y0_Cp0);
      aDIGTransport->SetlorlorgausModel_y0_Cp1(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_y0_Cp1);
      aDIGTransport->SetlorlorgausModel_norm_Cp0(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_norm_Cp0);
      aDIGTransport->SetlorlorgausModel_norm_Cp1(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_norm_Cp1);
      aDIGTransport->SetlorlorgausModel_normgaus2_Cp0(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_normgaus2_Cp0);
      aDIGTransport->SetlorlorgausModel_normgaus2_Cp1(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_normgaus2_Cp1);
      aDIGTransport->SetlorlorgausModel_sigma2_Cp0(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_sigma2_Cp0);
      aDIGTransport->SetlorlorgausModel_sigma2_Cp1(fDIGInitialize->GetTransportPar(itrans).lorlorgausModel_sigma2_Cp1);   
    }
    if(aDIGTransport->GetChargeModel()==5){
      aDIGTransport->Setf1dimgauslor_Norm_g_1st(fDIGInitialize->GetTransportPar(itrans).l1dimgauslor_Norm_g_1st); 
      aDIGTransport->Setf1dimgauslor_x0_g_1st(fDIGInitialize->GetTransportPar(itrans).l1dimgauslor_x0_g_1st);    
      aDIGTransport->Setf1dimgauslor_sigma_g_1st(fDIGInitialize->GetTransportPar(itrans).l1dimgauslor_sigma_g_1st);   
      aDIGTransport->Setf1dimgauslor_Gamma_lor_1st(fDIGInitialize->GetTransportPar(itrans).l1dimgauslor_Gamma_lor_1st); 
      aDIGTransport->Setf1dimgauslor_x0_lor_1st(fDIGInitialize->GetTransportPar(itrans).l1dimgauslor_x0_lor_1st);
      aDIGTransport->Setf1dimgauslor_norm_lor_1st(fDIGInitialize->GetTransportPar(itrans).l1dimgauslor_norm_lor_1st); 
      aDIGTransport->Setf1dimgauslor_Norm_g_2nd(fDIGInitialize->GetTransportPar(itrans).l1dimgauslor_Norm_g_2nd); 
      aDIGTransport->Setf1dimgauslor_x0_g_2nd(fDIGInitialize->GetTransportPar(itrans).l1dimgauslor_x0_g_2nd); 
      aDIGTransport->Setf1dimgauslor_sigma_g_2nd(fDIGInitialize->GetTransportPar(itrans).l1dimgauslor_sigma_g_2nd); 
      aDIGTransport->Setf1dimgauslor_Gamma_lor_2nd(fDIGInitialize->GetTransportPar(itrans).l1dimgauslor_Gamma_lor_2nd);
      aDIGTransport->Setf1dimgauslor_x0_lor_2nd(fDIGInitialize->GetTransportPar(itrans).l1dimgauslor_x0_lor_2nd); 
      aDIGTransport->Setf1dimgauslor_norm_lor_2nd(fDIGInitialize->GetTransportPar(itrans).l1dimgauslor_norm_lor_2nd); 
    }
    fDIGTransportArray->Add(aDIGTransport);
  }

  for (Int_t igeom = 0; igeom < (fDIGInitialize->GetPlanePar().NGeom) ; igeom++){    
    for (Int_t itemp = 0; itemp < (fDIGInitialize->GetPlanePar().NTemperature) ; itemp++){
      aDIGPlane  = new DIGPlane();
      aDIGPlane->SetPitch((fDIGInitialize->GetPlanePar().PitchX[igeom]),(fDIGInitialize->GetPlanePar().PitchY[igeom]));
      aDIGPlane->SetNpixels((fDIGInitialize->GetPlanePar().NPixelsX),(fDIGInitialize->GetPlanePar().NPixelsY));
      Float_t tempDimensionX = (fDIGInitialize->GetPlanePar().NPixelsX)*(fDIGInitialize->GetPlanePar().PitchX[igeom]);
      Float_t tempDimensionY = (fDIGInitialize->GetPlanePar().NPixelsY)*(fDIGInitialize->GetPlanePar().PitchY[igeom]);
      Float_t tempDimensionZ = (fDIGInitialize->GetPlanePar().EpitaxialThickness[igeom]);
      aDIGPlane->SetDimensions(tempDimensionX,tempDimensionY,tempDimensionZ);
      aDIGPlane->SetNoiseElectrons((fDIGInitialize->GetPlanePar().NoiseElectrons[igeom]));
      aDIGPlane->SetTemperature((fDIGInitialize->GetPlanePar().Temperature[itemp]));
      aDIGPlane->SetIonizationEnergy((fDIGInitialize->GetPlanePar().IonizationEnergy));
      aDIGPlane->SetSegmentSize((fDIGInitialize->GetPlanePar().SegmentSize));
      aDIGPlane->SetMaximumSegmentSize((fDIGInitialize->GetPlanePar().MaximumSegmentSize));
      aDIGPlane->SetMaximumChargePerSegment((fDIGInitialize->GetPlanePar().MaximumChargePerSegment));
      aDIGPlane->SetDiffusionMaximumRange((fDIGInitialize->GetPlanePar().DiffusionMaximumRangeInX),
					  (fDIGInitialize->GetPlanePar().DiffusionMaximumRangeInY));
      aDIGPlane->SetReflexionCoefficient((fDIGInitialize->GetPlanePar().ReflexionCoefficient));
      aDIGPlane->SetBasicModel_SigmaTenMicrons((fDIGInitialize->GetPlanePar().BasicModel_SigmaTenMicrons));

      fDIGPlaneArray->Add(aDIGPlane);
    }
  }

  
  //------------compute total number of configurations
  Int_t mynumberofconfigurations = TotalNumberOfBeams* TotalNumberOfPlanes *TotalNumberOfADCs*TotalNumberOfTransports;
  SetNumberOfConfigurations(mynumberofconfigurations);

  //---------------------------
  //----create the array of results:
  fDIGResultArray  = new TObjArray(mynumberofconfigurations); 
  DIGResult *aDIGResult=0;
  for (Int_t ires = 0; ires < mynumberofconfigurations ; ires++){    
    aDIGResult = new DIGResult();
    aDIGResult->SetIdealEfficiency(0.0);
    fDIGResultArray->Add(aDIGResult);
  }
  //---------------------------


  //---------------------------
  if(myAction==1) {
    //---------------------------
    

    //----------create tree:
    //----------create tree with all classes.
    //TFile myfile("tree.root","RECREATE");
    //TTree mytree("mytree","DIGMAPS tree");
   
    ffile = new TFile("tree.root","RECREATE");
    ftree = new TTree("mytree","DIGMAPS tree");

    fdigbeam = new DIGBeam();
    ftree->Branch("beam_branch","DIGBeam",&fdigbeam,32000,1);

    fdigplane = new DIGPlane();
    ftree->Branch("plane_branch","DIGPlane",&fdigplane,32000,1);

    fdigadc = new DIGADC();
    ftree->Branch("adc_branch","DIGADC",&fdigadc,32000,1);
  
    fdigtransport = new DIGTransport();
    ftree->Branch("transport_branch","DIGTransport",&fdigtransport,32000,1);

    fdigevent = new DIGEvent();
    ftree->Branch("event_branch","DIGEvent",&fdigevent,32000,99);

    
    //---------------------------
    Int_t iconfigcounter =0;
    //-------------------------------------------------------------------------------------
    //--------------Loop on all configurations.
    //-------------------------------------------------------------------------------------
    for (Int_t ibeam = 0; ibeam < TotalNumberOfBeams ; ibeam++){    
      for (Int_t iplane = 0; iplane < TotalNumberOfPlanes ; iplane++){    
	for (Int_t iadc = 0; iadc < TotalNumberOfADCs ; iadc++){    
	  for (Int_t itransport = 0; itransport < TotalNumberOfTransports ; itransport++){    
	  std::cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<endl;
	  std::cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<endl;
	  std::cout<<"                                                               "<<endl;
	  std::cout<<"        Configuration number " <<iconfigcounter+1 <<" / "<< mynumberofconfigurations<<endl;
	  std::cout<<"         ibeam "<<ibeam <<endl;
	  std::cout<<"         iplane "<<iplane <<endl;
	  std::cout<<"         iadc "<<iadc <<endl;
	  std::cout<<"         itransport "<< itransport<<endl;
	  std::cout<<"                                                               "<<endl;
	  std::cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<endl;
	  std::cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<endl;
	  RunConfiguration(iconfigcounter,ibeam,iplane,iadc,itransport);
	  iconfigcounter++;
	  }
	}
      }
    }
    ffile->Write();
    delete fdigevent;
    delete fdigbeam;
    delete fdigplane;
    delete fdigtransport;
    delete fdigadc;
    //-------------------------------------------------------------------------------------
  }// end Action ==1

  if(myAction==2){
    cout<<"Time to read the tree ! "<<endl;
    ActionPlot();
  }

 
  PrintConfigurations();

  std::cout.rdbuf(OldBuf);
  fOutputFileStream.close();

  PrintConfigurations();


  cout<<" ... END"<<endl;

  /*  time_t seconds;
      seconds = time(NULL);
      cout<<"TIME "<< seconds<<endl;
  */
}
//______________________________________________________________________________
//  
void  DIGMAPS::ActionPlot()
{


  //-------------------------------------------
  //---------Declare histograms
  //-------------------------------------------

  DIGHistograms::BookHistograms(GetNumberOfConfigurations());


  //-------------------------------------------
  //---------Open tree
  //-------------------------------------------
  
  cout<<"Time to read the tree ! "<<endl;
  ffile = new TFile("tree.root","READ");
  //   ftree = new TTree("mytree","DIGMAPS tree");
  ftree=(TTree*) ffile->Get("mytree");

  DIGBeam *mydigBeam = new DIGBeam();
  TBranch *branchdigBeam = ftree->GetBranch("beam_branch");
  branchdigBeam->SetAddress(&mydigBeam);

  DIGPlane *mydigPlane = new DIGPlane();
  TBranch *branchdigPlane= ftree->GetBranch("plane_branch");
  branchdigPlane->SetAddress(&mydigPlane);

  DIGADC *mydigADC = new DIGADC();
  TBranch *branchdigADC= ftree->GetBranch("adc_branch");
  branchdigADC->SetAddress(&mydigADC);

  DIGTransport *mydigTransport = new DIGTransport();
  TBranch *branchdigTransport= ftree->GetBranch("transport_branch");
  branchdigTransport->SetAddress(&mydigTransport);


    
  DIGEvent *mydigEvent = new DIGEvent();
  TBranch *branchdigEvent= ftree->GetBranch("event_branch");
  branchdigEvent->SetAddress(&mydigEvent);
    
  Int_t NEVENT = (Int_t) ftree->GetEntries() ;

  Int_t MinEvent = 0;
  Int_t MaxEvent = NEVENT;


  Int_t Old_configuration = -1;
  Int_t Current_configuration = 0;

  Float_t IdealEfficiency=-1;
  Int_t TotalNumberOfParticles = 0;
  Int_t TotalNumberOfClusters = 0;


  // Int_t Former_configuration =0;
  // **********************************************************************************
  // ********* MAIN LOOP ***************
  // **********************************************************************************
  for ( Int_t ievt=MinEvent ; ievt<MaxEvent ; ievt++ ) {
    ftree->GetEvent(ievt);
    cout<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Event ievt "<<ievt<<endl;
    //cout<<" DIGBEAM  TEST:: GetThetaIncidentDeg "<<mydigBeam->GetThetaIncidentDeg()<<endl;
    //cout<<" DIGPLANE TEST:: GetPitchX "<<mydigPlane->GetPitchX() <<endl;
    //cout<<" DIGADC   TEST:: GetNbits() "<<mydigADC->GetNbits() <<endl;

    //-------------------------------------------
    //---------get the configuration and initialize if it's a new one.
    //-------------------------------------------
    
    Current_configuration = mydigEvent->GetConfigurationNumber();


    if(Current_configuration!=Old_configuration){ 
      IdealEfficiency=-1;
      TotalNumberOfParticles = 0;
      TotalNumberOfClusters = 0;
      Old_configuration=Current_configuration;
    }

    //-------------------------------------------
    //---------Get transport model and plot it
    //-------------------------------------------

    //mydigTransport->GetChargeModel()

    // if model == 1 then store tels parametres.

    // create a function with such parametersxs  
    // On a deja: Double_t DIGMAPS::Lorentz2D(Double_t *x, Double_t *par){
    // copier la 2xgaus2d et la lor+gauss

    // en fait il faudrait stocker une funtion plutot qu'un histo.

    // creer des histo 2d charge vs distance.

    //loop sur tous les clusters, acces a tous les pixels du cluster, acces a leur charge digitise et leur position, 
    //acces a la position de la trace, calcul de la distance
    //remplissage de l'histo 2d.

    //-------------------------------------------
    //---------loop on ADC thresholds
    //-------------------------------------------
    for ( Int_t i=0 ; i<mydigADC->GetNThresholds() ; i++ ) {
      //      cout<<"threshold "<<i<<" "<<mydigADC->GetADC_thresholds()[i]<<endl;
    }   
    ((TH1F*)  Ar_h1_ADC_LSB->At(Current_configuration))->Fill( mydigADC->GetLSB() );

    //-------------------------------------------
    //---------Print info
    //-------------------------------------------
    mydigEvent->PrintInfo();

    //-------------------------------------------
    //----------get particles of the event
    //-------------------------------------------
    TClonesArray       *myDIGParticleArray = mydigEvent->GetParticle();
    DIGParticle* TheParticle =0;
    Int_t NbOfParticles = myDIGParticleArray->GetLast()+1   ;
    TotalNumberOfParticles+=NbOfParticles;
    Int_t j=0;
    vector< Bool_t > ClusterAssociated;
    ClusterAssociated.resize(mydigEvent->GetNClusters());
    for (Int_t i = 0; i < mydigEvent->GetNClusters() ; i++){
      ClusterAssociated[i]=false;
    }


    while(j<NbOfParticles) {
      TheParticle=(DIGParticle*) myDIGParticleArray->At(j) ;
      //TheParticle->PrintInfo();
      cout<<" DIGITAL CHARGE "<<TheParticle->GetTotalDigitalCharge()<<endl;


      ((TH1F*) Ar_h1_Particle_TotalAnalogCharge->At(Current_configuration))->Fill( TheParticle->GetTotalAnalogCharge() );
      ((TH1F*) Ar_h1_Particle_TotalDigitalCharge->At(Current_configuration))->Fill( TheParticle->GetTotalDigitalCharge() );
      ((TH1F*) Ar_h1_Particle_Energy_deposited->At(Current_configuration))->Fill( TheParticle->GetEnergy_deposited() );

      
      ((TH2F*) Ar_h2_Particle_EnergyDeposited_vs_TotalAnalogCharge->At(Current_configuration))
	->Fill(TheParticle->GetTotalAnalogCharge(),TheParticle->GetEnergy_deposited());    
      ((TH2F*) Ar_h2_Particle_TotalDigitalCharge_vs_TotalAnalogCharge->At(Current_configuration))
      	->Fill(TheParticle->GetTotalAnalogCharge(),TheParticle->GetTotalDigitalCharge());
  

      //--------------loop on clusters and associate each particle to one cluster
      TClonesArray       *myDIGClusterArray = mydigEvent->GetCluster();
      DIGCluster* TheCluster =0;
      Int_t NbOfCluster = myDIGClusterArray->GetLast()+1   ;
      TotalNumberOfClusters+=NbOfCluster;

      Int_t k=0;
      
      Float_t trackHitdistance = 100000000.0;
      Int_t theclusterNumber = 0;
      Double_t ClusterCoGX = 0.0;
      Double_t ClusterCoGY = 0.0;
      Double_t theClusterCoGX = 0.0;
      Double_t theClusterCoGY = 0.0;
      Float_t TrackX = (TheParticle->GetEntryX() + TheParticle->GetExitX())/2.0;
      Float_t TrackY = (TheParticle->GetEntryY() + TheParticle->GetExitY())/2.0;
      
      while(k<NbOfCluster) {
	TheCluster=(DIGCluster*) myDIGClusterArray->At(k) ;
	ClusterCoGX = TheCluster->GetXposition_CoG();
	ClusterCoGY = TheCluster->GetYposition_CoG();
	if(TheCluster->GetTotalCharge()>0.){
	  Double_t currentdist = TMath::Sqrt((TrackX-ClusterCoGX)*(TrackX-ClusterCoGX) + (TrackY-ClusterCoGY)*(TrackY-ClusterCoGY));
	  if((currentdist<trackHitdistance)&&(ClusterAssociated[k]==false)){
	    theclusterNumber = k;
	    trackHitdistance = currentdist;
	    theClusterCoGX = ClusterCoGX;
	    theClusterCoGY = ClusterCoGY;
	  }	
	}
	k++;
      }
      cout<<" DIGITAL CHARGE 2 "<<TheParticle->GetTotalDigitalCharge()<<endl;
      
      //--------------end loop on clusters
      //--------------loop on the pixels of the cluster:
      if(TheParticle->GetTotalDigitalCharge()>0){
	TheCluster = (DIGCluster*) myDIGClusterArray->At(theclusterNumber);
	Float_t ClusterTotalCharge = TheCluster->GetTotalCharge();
	/*
	  for (Int_t ip = 0; ip < TheCluster->GetNpixels() ; ip++){
	  ClusterTotalCharge += TheCluster->GetDigitalCharge()[ip];
	  }
	*/
	cout<<" DIGITAL CHARGE 2.1 "<<TheParticle->GetTotalDigitalCharge()<<endl;

	for (Int_t ip = 0; ip < TheCluster->GetNpixels() ; ip++){
	  Int_t Xpixnum;
	  Int_t Ypixnum;
	  Float_t Xpix;
	  Float_t Ypix;
	  //       TrackX TrackY = particle position.
	  //      TheCluster->GetPixelMap()[ip];
	  //	TheCluster->GetDigitalCharge()[ip];
	  TheCluster->GetXYPixelNumber(Xpixnum,Ypixnum,mydigPlane,TheCluster->GetPixelMap()[ip]);
	  Xpix=(float(Xpixnum)+0.5) * mydigPlane->GetPitchX();
	  Ypix=(float(Ypixnum)+0.5) * mydigPlane->GetPitchY();
	  cout<<"-------------> "<<TrackX<<" "<<Xpix<<" "<<TrackY<<" "<<Ypix<<endl;
	  Float_t trackpixeldistance = TMath::Sqrt((TrackX-Xpix)*(TrackX-Xpix) + (TrackY-Ypix)*(TrackY-Ypix));
	  Float_t thepixelcharge = TheCluster->GetDigitalCharge()[ip];
	  Float_t thepixelchargeRelative = thepixelcharge/ClusterTotalCharge;
	  if(ClusterTotalCharge!=0.0){
	    ((TH2F*) Ar_h2_Charge_Q_over_Qtot_vs_distance->At(Current_configuration))
	      ->Fill(trackpixeldistance,thepixelchargeRelative); 
	    ((TProfile*) Ar_Pr_Charge_Q_over_Qtot_vs_distance->At(Current_configuration))
	      ->Fill(trackpixeldistance,thepixelchargeRelative); 
	  }
	}
      }
      //--------------end loop on pixels
      Float_t distancemax = 5.0*TMath::Sqrt(mydigPlane->GetPitchX()*mydigPlane->GetPitchX()+mydigPlane->GetPitchY()*mydigPlane->GetPitchY());
      cout<<" DIGITAL CHARGE 3 "<<TheParticle->GetTotalDigitalCharge()<<endl;

      if(TheParticle->GetTotalDigitalCharge()>0.){
	if(trackHitdistance < distancemax ){
	  ClusterAssociated[theclusterNumber]=true;
	}
	
	((TH1F*) Ar_h1_Resolution_ResidualX_CoG_true->At(Current_configuration))->Fill(TrackX - theClusterCoGX);
	((TH1F*) Ar_h1_Resolution_ResidualY_CoG_true->At(Current_configuration))->Fill(TrackY - theClusterCoGY);
	((TH1F*) Ar_h1_Resolution_Residualdist_CoG_true->At(Current_configuration))->Fill(trackHitdistance);
	((TH2F*) Ar_h2_Resolution_TruePosition->At(Current_configuration))->Fill(TrackX,TrackY);
	((TH2F*) Ar_h2_Resolution_TruePosition_modulo->At(Current_configuration))
	  ->Fill(TrackX-int(TrackX)+int(TrackX)%(int(mydigPlane->GetPitchX())) ,TrackY-int(TrackY)+int(TrackY)%(int(mydigPlane->GetPitchY()))   );
	((TH2F*) Ar_h2_Resolution_CoG->At(Current_configuration))->Fill(ClusterCoGX,ClusterCoGY);
	((TH2F*) Ar_h2_Resolution_CoG_modulo->At(Current_configuration))
	  ->Fill(ClusterCoGX-int(ClusterCoGX)+int(ClusterCoGX)%(int(mydigPlane->GetPitchX())),ClusterCoGY-int(ClusterCoGY)+int(ClusterCoGY)%(int(mydigPlane->GetPitchY())));
	
      }
      cout<<" DIGITAL CHARGE 4 "<<TheParticle->GetTotalDigitalCharge()<<endl;

      j++;
    } //----end loop on particles
    if(mydigEvent->GetNParticles() !=myDIGParticleArray->GetLast()+1){
      cout<< "DIGEVENT TEST:: WARNING PROBLEM IN PARTICLE RECORDING"<<endl;
      cout<<mydigEvent->GetNParticles()<<" != "<<myDIGParticleArray->GetLast()+1<<endl;
    }else{
      //cout<<mydigEvent->GetNParticles()<<" === "<<myDIGParticleArray->GetLast()+1<<endl;
    }

    //-------------------------------------------
    //----------get clusters of the event
    //-------------------------------------------
    TClonesArray       *myDIGClusterArray = mydigEvent->GetCluster();
    DIGCluster* TheCluster =0;
    Int_t NbOfCluster = myDIGClusterArray->GetLast()+1   ;
    Int_t k=0;

    if(mydigEvent->GetNClusters() !=myDIGClusterArray->GetLast()+1){
      cout<< "DIGEVENT TEST:: WARNING PROBLEM IN CLUSTERS RECORDING"<<endl;
      cout<<mydigEvent->GetNClusters()<<" != "<<myDIGClusterArray->GetLast()+1<<endl;
    }else{
      //cout<<mydigEvent->GetNClusters()<<" === "<<myDIGClusterArray->GetLast()+1<<endl;
    }

    while(k<NbOfCluster) {
      TheCluster=(DIGCluster*) myDIGClusterArray->At(k) ;
      TheCluster->PrintInfo();
      
      ((TH1F*) Ar_h1_multiplicity_with_threshold_01->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(1));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_02->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(2));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_03->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(3));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_04->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(4));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_05->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(5));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_06->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(6));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_07->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(7));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_08->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(8));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_09->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(9));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_10->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(10));

      ((TH1F*) Ar_h1_multiplicity_with_threshold_15->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(15));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_20->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(20));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_25->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(25));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_30->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(30));
      ((TH1F*) Ar_h1_multiplicity_with_threshold_40->At(Current_configuration))->Fill(TheCluster->GetMultiplicity(40));
 
      ((TH1F*) Ar_h1_Cluster_SeedDigitalCharge->At(Current_configuration))->Fill(float(TheCluster->GetDigitalCharge()[TheCluster->GetSeedPixelIndex()]) );
      ((TH1F*) Ar_h1_Cluster_TotalDigitalCharge->At(Current_configuration))->Fill(  TheCluster->GetTotalCharge() );
      ((TH1F*) Ar_h1_Cluster_9x9DigitalCharge->At(Current_configuration))
	->Fill( TheCluster->GetDigitalCharge()[TheCluster->GetSeedPixelIndex()]+  TheCluster->Get1stCrownCharge(mydigPlane) );
      ((TH1F*) Ar_h1_Cluster_1stCrownDigitalCharge->At(Current_configuration))->Fill(TheCluster->Get1stCrownCharge(mydigPlane));
      ((TH1F*) Ar_h1_Cluster_2ndCrownDigitalCharge->At(Current_configuration))->Fill(TheCluster->Get2ndCrownCharge(mydigPlane));
      ((TH1F*) Ar_h1_Cluster_4NeighboursDigitalCharge->At(Current_configuration))->Fill(TheCluster->Get4NeigboursCharge(mydigPlane));



      ((TH1F*)  Ar_h1_test->At(Current_configuration))
	->Fill( (TheCluster->Get1stCrownCharge(mydigPlane) +TheCluster->Get2ndCrownCharge(mydigPlane)
		 +TheCluster->GetDigitalCharge()[TheCluster->GetSeedPixelIndex()]));


      if( TheCluster->GetTotalCharge()!=0){
	((TH1F*) Ar_h1_Cluster_SeedOverTotalDigitalCharge->At(Current_configuration))
	  ->Fill(TheCluster->GetDigitalCharge()[TheCluster->GetSeedPixelIndex()] / float(TheCluster->GetTotalCharge()) );
	((TH1F*) Ar_h1_Cluster_1stCrownOverTotalDigitalCharge->At(Current_configuration))
	  ->Fill(TheCluster->Get1stCrownCharge(mydigPlane)/ float(TheCluster->GetTotalCharge()) );
	((TH1F*) Ar_h1_Cluster_2ndCrownOverTotalDigitalCharge->At(Current_configuration))
	  ->Fill(TheCluster->Get2ndCrownCharge(mydigPlane)/ float(TheCluster->GetTotalCharge()) );
	((TH1F*) Ar_h1_Cluster_4NeighboursOverTotalDigitalCharge->At(Current_configuration))
	  ->Fill(TheCluster->Get4NeigboursCharge(mydigPlane)/ float(TheCluster->GetTotalCharge()) );
      }

      k++;
    }

    //-------------------------------------------
    //---------Get read out map of the event
    //-------------------------------------------

    DIGReadoutmap *myDIGReadoutmap = mydigEvent->GetReadoutmap();
    //myDIGReadoutmap->PrintInfo();
    Int_t Nx = mydigPlane->GetNpixelsX();
    Int_t Ny = mydigPlane->GetNpixelsY();
    myDIGReadoutmap->PrintOuput(Nx,Ny);

    //-------------------------------------------
    //---------compute current efficiency
    //-------------------------------------------

    if(TotalNumberOfParticles!=0){
      IdealEfficiency = float(TotalNumberOfClusters)/float(TotalNumberOfParticles);
      GetResult(Current_configuration)->SetConfigNumber(Current_configuration); 
      GetResult(Current_configuration)->SetIdealEfficiency(IdealEfficiency);
    }




  }
  // **********************************************************************************
  // ********* END OF MAIN LOOP ***************
  // **********************************************************************************

  //-------------------------------------------------------------------------------------
  //--------------Compute results of the current configuration
  //-------------------------------------------------------------------------------------
  for ( Int_t ic=0 ; ic<GetNumberOfConfigurations() ; ic++ ) {

    ((TH1F*) Ar_h1_Efficiency_ideal->At(ic))->Fill(GetResult(ic)->GetIdealEfficiency());

  }

}
//______________________________________________________________________________
//  
void  DIGMAPS::PlotAConfiguration(Int_t confignumber, Bool_t newcanvas)
{


  if(newcanvas==1){
    MainCanvas1 = new TCanvas("MainCanvas1","MainCanvas1",450,10,1000,950);
    MainCanvas1->Divide(4,4);
    MainCanvas1->Update();
    MainCanvas2 = new TCanvas("MainCanvas2","MainCanvas2",550,10,1000,950);
    MainCanvas2->Divide(2,4);
    MainCanvas2->Update();
    MainCanvas3 = new TCanvas("MainCanvas3","MainCanvas3",650,10,1000,950);
    MainCanvas3->Divide(2,4);
    MainCanvas3->Update();
    MainCanvas4 = new TCanvas("MainCanvas4","MainCanvas4",750,10,1000,950);
    MainCanvas4->Divide(2,4);
    MainCanvas4->Update();
    ColorChosen =1;

  }else{
    ColorChosen++;
    if(ColorChosen==2){ColorChosen++;}
    //   ColorChosen++;
  }

  Float_t integ ;
  Float_t AxisRange = 0.4;
  //------------MainCanvas1
  MainCanvas1->cd(1);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_01->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_01->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_01->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_01->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(2);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_02->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_02->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_02->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_02->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_02->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_02->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(3);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_03->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_03->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_03->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_03->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_03->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_03->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(4);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_04->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_04->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_04->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_04->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_04->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_04->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(5);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_05->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_05->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_05->At(confignumber)))->SetAxisRange(0,AxisRange,"Y");
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_05->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_05->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_05->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(6);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_06->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_06->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_06->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_06->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_06->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_06->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(7);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_07->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_07->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_07->At(confignumber)))->SetAxisRange(0,AxisRange,"Y");
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_07->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_07->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_07->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(8);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_08->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_08->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_08->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_08->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_08->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_08->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(9);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_09->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_09->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_09->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_09->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_09->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_09->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(10);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_10->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_10->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_10->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_10->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_10->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_10->At(confignumber))->Draw("same");
  }

  MainCanvas1->cd(11);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_15->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_15->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_15->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_15->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_15->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_15->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(12);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_20->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_20->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_20->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_20->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_20->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_20->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(13);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_25->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_25->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_25->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_25->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_25->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_25->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(14);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_30->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_30->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_30->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_30->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_30->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_30->At(confignumber))->Draw("same");
  }
  MainCanvas1->cd(15);
  integ =((TH1F*)Ar_h1_multiplicity_with_threshold_40->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_40->At(confignumber))->Scale(1/integ);}
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_40->At(confignumber)))->SetAxisRange(0,AxisRange,"Y"); 
  ((TH1F*)(Ar_h1_multiplicity_with_threshold_40->At(confignumber)))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    (Ar_h1_multiplicity_with_threshold_40->At(confignumber))->Draw();
  }else{
    (Ar_h1_multiplicity_with_threshold_40->At(confignumber))->Draw("same");
  }


  //------------MainCanvas2
  MainCanvas2->cd(1);
  ((TH1F*)Ar_h1_Cluster_SeedDigitalCharge->At(confignumber))->Draw("HIST");
  ((TH1F*)Ar_h1_Cluster_SeedDigitalCharge->At(confignumber))->Sumw2();
  integ = ((TH1F*)Ar_h1_Cluster_SeedDigitalCharge->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_Cluster_SeedDigitalCharge->At(confignumber))->Scale(1/integ);}
  ((TH1F*)Ar_h1_Cluster_SeedDigitalCharge->At(confignumber))->Fit("landau","","same");
 
  MainCanvas2->cd(2);
  ((TH1F*)Ar_h1_Cluster_TotalDigitalCharge->At(confignumber))->Draw("HIST");
  ((TH1F*)Ar_h1_Cluster_TotalDigitalCharge->At(confignumber))->Sumw2();
  integ = ((TH1F*)Ar_h1_Cluster_TotalDigitalCharge->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_Cluster_TotalDigitalCharge->At(confignumber))->Scale(1/integ);}
   ((TH1F*)Ar_h1_Cluster_TotalDigitalCharge->At(confignumber))->Fit("landau","","same");

  MainCanvas2->cd(3);
  ((TH1F*)Ar_h1_Particle_TotalAnalogCharge->At(confignumber))->Draw("HIST");
  ((TH1F*)Ar_h1_Particle_TotalAnalogCharge->At(confignumber))->Sumw2();
  integ = ((TH1F*)Ar_h1_Particle_TotalAnalogCharge->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_Particle_TotalAnalogCharge->At(confignumber))->Scale(1/integ);}
  ((TH1F*)Ar_h1_Particle_TotalAnalogCharge->At(confignumber))->Fit("landau","","same");

  MainCanvas2->cd(4);
  ((TH1F*)Ar_h1_Particle_TotalDigitalCharge->At(confignumber))->Draw("HIST");
  ((TH1F*)Ar_h1_Particle_TotalDigitalCharge->At(confignumber))->Sumw2();
  integ = ((TH1F*)Ar_h1_Particle_TotalDigitalCharge->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_Particle_TotalDigitalCharge->At(confignumber))->Scale(1/integ);}
  ((TH1F*)Ar_h1_Particle_TotalDigitalCharge->At(confignumber))->Fit("landau","","same");

  MainCanvas2->cd(5);
  ((TH1F*)Ar_h1_Particle_Energy_deposited->At(confignumber))->Draw("HIST");
  ((TH1F*)Ar_h1_Particle_Energy_deposited->At(confignumber))->Sumw2();
  integ = ((TH1F*)Ar_h1_Particle_Energy_deposited->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_Particle_Energy_deposited->At(confignumber))->Scale(1/integ);}
 ((TH1F*)Ar_h1_Particle_Energy_deposited->At(confignumber))->Fit("landau","","same");

  MainCanvas2->cd(6);
  ((TH2F*)Ar_h2_Particle_EnergyDeposited_vs_TotalAnalogCharge->At(confignumber))->Draw("colz");

  MainCanvas2->cd(7);    
  ((TH2F*)Ar_h2_Particle_TotalDigitalCharge_vs_TotalAnalogCharge->At(confignumber))->Draw("colz");

  //------------MainCanvas3
  MainCanvas3->cd(1);  
  ((TH1F*)(Ar_h1_Cluster_9x9DigitalCharge->At(confignumber)))->SetLineColor(ColorChosen);
  ((TH1F*)Ar_h1_Cluster_9x9DigitalCharge->At(confignumber))->Draw();
  MainCanvas3->cd(2);    
  ((TH1F*)Ar_h1_Cluster_1stCrownDigitalCharge->At(confignumber))->SetLineColor(ColorChosen);
  if(newcanvas==1){
     ((TH1F*)Ar_h1_Cluster_1stCrownDigitalCharge->At(confignumber))->Draw();
  }else{
     ((TH1F*)Ar_h1_Cluster_1stCrownDigitalCharge->At(confignumber))->Draw("same");
  }
 
  MainCanvas3->cd(3); 
  ((TH2F*)Ar_h2_Charge_Q_over_Qtot_vs_distance->At(confignumber))->SetMarkerSize(1.0);
  ((TH2F*)Ar_h2_Charge_Q_over_Qtot_vs_distance->At(confignumber))->SetMarkerStyle(1);
  ((TH2F*)Ar_h2_Charge_Q_over_Qtot_vs_distance->At(confignumber))->SetMarkerColor(ColorChosen);
  ((TH2F*)Ar_h2_Charge_Q_over_Qtot_vs_distance->At(confignumber))->SetLineColor(ColorChosen);
  if(newcanvas==1){
      ((TH2F*)Ar_h2_Charge_Q_over_Qtot_vs_distance->At(confignumber))->Draw();
  }else{
      ((TH2F*)Ar_h2_Charge_Q_over_Qtot_vs_distance->At(confignumber))->Draw("same");
  }

  MainCanvas3->cd(4);   
  ((TProfile*)Ar_Pr_Charge_Q_over_Qtot_vs_distance->At(confignumber))->SetMarkerColor(ColorChosen);
  ((TProfile*)Ar_Pr_Charge_Q_over_Qtot_vs_distance->At(confignumber))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    ((TProfile*)Ar_Pr_Charge_Q_over_Qtot_vs_distance->At(confignumber))->Draw();
  }else{
    ((TProfile*)Ar_Pr_Charge_Q_over_Qtot_vs_distance->At(confignumber))->Draw("same");
  }
 
  /*
  if(newcanvas==1){
     ((TH1F*)Ar_h1_Cluster_4NeighboursDigitalCharge->At(confignumber))->Draw();
  }else{
     ((TH1F*)Ar_h1_Cluster_4NeighboursDigitalCharge->At(confignumber))->Draw("same");
       }*/
 
  MainCanvas3->cd(5);  
  integ = ((TH1F*)Ar_h1_Cluster_SeedOverTotalDigitalCharge->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_Cluster_SeedOverTotalDigitalCharge->At(confignumber))->Scale(1/integ);}
  ((TH1F*)Ar_h1_Cluster_SeedOverTotalDigitalCharge->At(confignumber))->SetAxisRange(0,0.07,"Y"); 
  ((TH1F*)Ar_h1_Cluster_SeedOverTotalDigitalCharge->At(confignumber))->SetLineColor(ColorChosen);
  if(newcanvas==1){
      ((TH1F*)Ar_h1_Cluster_SeedOverTotalDigitalCharge->At(confignumber))->Draw();
  }else{
      ((TH1F*)Ar_h1_Cluster_SeedOverTotalDigitalCharge->At(confignumber))->Draw("same");
  }

  MainCanvas3->cd(6);    
  integ = ((TH1F*)Ar_h1_Cluster_1stCrownOverTotalDigitalCharge->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_Cluster_1stCrownOverTotalDigitalCharge->At(confignumber))->Scale(1/integ);}
  ((TH1F*)Ar_h1_Cluster_1stCrownOverTotalDigitalCharge->At(confignumber))->SetAxisRange(0,0.09,"Y"); 
  ((TH1F*)Ar_h1_Cluster_1stCrownOverTotalDigitalCharge->At(confignumber))->SetLineColor(ColorChosen);
  if(newcanvas==1){
     ((TH1F*)Ar_h1_Cluster_1stCrownOverTotalDigitalCharge->At(confignumber))->Draw();
  }else{
     ((TH1F*)Ar_h1_Cluster_1stCrownOverTotalDigitalCharge->At(confignumber))->Draw("same");
  }
 
  MainCanvas3->cd(7); 
  integ = ((TH1F*)Ar_h1_Cluster_2ndCrownOverTotalDigitalCharge->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_Cluster_2ndCrownOverTotalDigitalCharge->At(confignumber))->Scale(1/integ);}   
  ((TH1F*)Ar_h1_Cluster_2ndCrownOverTotalDigitalCharge->At(confignumber))->SetAxisRange(0,0.10,"Y"); 
  ((TH1F*)Ar_h1_Cluster_2ndCrownOverTotalDigitalCharge->At(confignumber))->SetLineColor(ColorChosen);
  if(newcanvas==1){
     ((TH1F*)Ar_h1_Cluster_2ndCrownOverTotalDigitalCharge->At(confignumber))->Draw();
  }else{
     ((TH1F*)Ar_h1_Cluster_2ndCrownOverTotalDigitalCharge->At(confignumber))->Draw("same");
  }
 
  MainCanvas3->cd(8);  
  integ = ((TH1F*)Ar_h1_Cluster_4NeighboursOverTotalDigitalCharge->At(confignumber))->Integral();
  if(integ!=0.0){((TH1F*)Ar_h1_Cluster_4NeighboursOverTotalDigitalCharge->At(confignumber))->Scale(1/integ);}     
  ((TH1F*)Ar_h1_Cluster_4NeighboursOverTotalDigitalCharge->At(confignumber))->SetAxisRange(0,0.12,"Y"); 
  ((TH1F*)Ar_h1_Cluster_4NeighboursOverTotalDigitalCharge->At(confignumber))->SetLineColor(ColorChosen);
  if(newcanvas==1){
    ((TH1F*)Ar_h1_Cluster_4NeighboursOverTotalDigitalCharge->At(confignumber))->Draw();
  }else{
    ((TH1F*)Ar_h1_Cluster_4NeighboursOverTotalDigitalCharge->At(confignumber))->Draw("same");
  }
  

  //------------MainCanvas4
  MainCanvas4->cd(1);    
  ((TH1F*)Ar_h1_Resolution_ResidualX_CoG_true->At(confignumber))->Draw();
  ((TH1F*)Ar_h1_Resolution_ResidualX_CoG_true->At(confignumber))->Fit("gaus","","same");
  MainCanvas4->cd(2);    
  ((TH1F*)Ar_h1_Resolution_ResidualY_CoG_true->At(confignumber))->Draw();
  ((TH1F*)Ar_h1_Resolution_ResidualY_CoG_true->At(confignumber))->Fit("gaus","","same");
  MainCanvas4->cd(3);    
  ((TH1F*)Ar_h1_Resolution_Residualdist_CoG_true->At(confignumber))->Draw();
  MainCanvas4->cd(4);    
  ((TH2F*)Ar_h2_Resolution_TruePosition->At(confignumber))->Draw("colz");
  AutoZoom((TH2F*)Ar_h2_Resolution_TruePosition->At(confignumber))->Draw("colz");
  MainCanvas4->cd(5);    
  ((TH2F*)Ar_h2_Resolution_TruePosition_modulo->At(confignumber))->Draw("colz");
  AutoZoom((TH2F*)Ar_h2_Resolution_TruePosition_modulo->At(confignumber))->Draw("colz");
  MainCanvas4->cd(6);    
  ((TH2F*)Ar_h2_Resolution_CoG->At(confignumber))->Draw("colz");
  AutoZoom((TH2F*)Ar_h2_Resolution_CoG->At(confignumber))->Draw("colz");
  MainCanvas4->cd(7);    
  ((TH2F*)Ar_h2_Resolution_CoG_modulo->At(confignumber))->Draw("colz");
  AutoZoom(((TH2F*)Ar_h2_Resolution_CoG_modulo->At(confignumber)))->Draw("colz");
  MainCanvas4->cd(8);    
  

  //------------
  MainCanvas1->Update();
  MainCanvas2->Update();
  MainCanvas3->Update();
  MainCanvas4->Update();

}
//______________________________________________________________________________
//  
//  Double_t         Lorentz2D(Double_t *x, Double_t *par);
//  Double_t DIGMAPS::Lorentz2D(Double_t *x, Double_t *par){ 
//x[0] = x
    //x[1] = y
    // par[0] = Gamma
    // par[1] = x0
//.Sizeof() 
// sizeof(configlist)


void  DIGMAPS::PlotDigitalConfiguration(Bool_t newcanvas, Int_t *configlist, Int_t Nconfigs)
{
  //Int_t Nconfigs = sizeof(configlist)/sizeof(configlist[0]);
  //cout<<configlist[0]<<" "<< configlist[1]<<endl;
  //cout<<" NCONFIGS "<<Nconfigs<<" "<< sizeof(configlist) <<" "<<sizeof(configlist[0]) <<endl;
  Float_t integ ;
  if(newcanvas==1){
    MainCanvas1 = new TCanvas("MainCanvas1","MainCanvas1",450,10,1000,950);
    MainCanvas2 = new TCanvas("MainCanvas2","MainCanvas2",550,10,1000,950);
    MainCanvas3 = new TCanvas("MainCanvas3","MainCanvas3",650,10,1000,950);
    MainCanvas4 = new TCanvas("MainCanvas4","MainCanvas4",750,10,1000,950);
    MainCanvas5 = new TCanvas("MainCanvas5","MainCanvas5",750,10,1000,950);
    MainCanvas5->Divide(2,4);
    MainCanvas5->Update();

    if(Nconfigs<=4){
      MainCanvas1->Divide(2,2);
      MainCanvas2->Divide(2,2);
      MainCanvas3->Divide(2,2);
      MainCanvas4->Divide(2,2);
    }else if(Nconfigs<=6){
      MainCanvas1->Divide(2,3);
      MainCanvas2->Divide(2,3);
      MainCanvas3->Divide(2,3);
      MainCanvas4->Divide(2,3);
    }else if(Nconfigs<=9){
      MainCanvas1->Divide(3,3);
      MainCanvas2->Divide(3,3);
      MainCanvas3->Divide(3,3);
      MainCanvas4->Divide(3,3);
    }else if(Nconfigs<=12){
      MainCanvas1->Divide(3,4);
      MainCanvas2->Divide(3,4);
      MainCanvas3->Divide(3,4);
      MainCanvas4->Divide(3,4);
    }else if(Nconfigs<=16){
      MainCanvas1->Divide(4,4);
      MainCanvas2->Divide(4,4);
      MainCanvas3->Divide(4,4);
      MainCanvas4->Divide(4,4);
    }else if(Nconfigs<=25){
      MainCanvas1->Divide(5,5);
      MainCanvas2->Divide(5,5);
      MainCanvas3->Divide(5,5);
      MainCanvas4->Divide(5,5);
    }else{
      cout<<" WARNING : Config list too big, please reduce the number of configs you want to plot"<<endl;
      MainCanvas1->Divide(5,5);
      MainCanvas2->Divide(5,5);
      MainCanvas3->Divide(5,5);
      MainCanvas4->Divide(5,5);
    }
    MainCanvas1->Update();
    MainCanvas2->Update();
    MainCanvas3->Update();   
    MainCanvas4->Update();
    ColorChosen =1;
  }else{
    ColorChosen++;
    if(ColorChosen==2){ColorChosen++;}
    if(ColorChosen==5){ColorChosen++;}
    //   ColorChosen++;
  }
  Int_t maxNconfigs = 25;
  if(Nconfigs>maxNconfigs){Nconfigs=maxNconfigs;}
 
  //------------MainCanvas1
  Int_t currentpad = 1;
  for (Int_t i=0 ; i<Nconfigs ; i++){
    MainCanvas1->cd(currentpad);
    integ =((TH1F*)Ar_h1_multiplicity_with_threshold_01->At(configlist[i]))->Integral();
    if(integ!=0.0){((TH1F*)Ar_h1_multiplicity_with_threshold_01->At(configlist[i]))->Scale(1/integ);}
    ((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i])))->SetAxisRange(0,0.6,"Y"); 
    ((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i])))->SetLineColor(ColorChosen);
    ((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i])))->SetFillStyle(0);
    ((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i])))->SetFillColor(1);
    ((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i])))->SetMarkerColor(ColorChosen);
    ((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i])))->SetMarkerStyle(2);
    ((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i])))->SetMarkerSize(1);    
    ((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i])))->SetLineStyle(1);
    ((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i])))->SetLineWidth(1);
    if(newcanvas==1){
      (Ar_h1_multiplicity_with_threshold_01->At(configlist[i]))->Draw("lp");
       gPad->Update();
       MainCanvas1->Update();
       TPaveStats *st = (TPaveStats*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i]))->FindObject("stats");
       st->SetName("stat2");
       //MainCanvas1->Modified();
    }else{
      (Ar_h1_multiplicity_with_threshold_01->At(configlist[i]))->Draw("lp sames");
      gPad->Update();
      MainCanvas1->Update();
      TPaveStats *st = (TPaveStats*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i]))->FindObject("stats");
      st->SetName("stat2");
      //MainCanvas1->Modified();
    }    
    // cout<<" MULTI "<<i<<" "<<((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i])))->GetMean()
    //	<<" "<<((TH1F*)(Ar_h1_multiplicity_with_threshold_01->At(configlist[i])))->GetMeanError()<<endl;

    currentpad++;
  }
  //------------MainCanvas2
  currentpad = 1;
  for (Int_t i=0 ; i<Nconfigs ; i++){
    MainCanvas2->cd(currentpad);
    if(newcanvas==1){
      (Ar_h1_ADC_LSB->At(configlist[i]))->Draw();
      gPad->Update();
    }else{
      (Ar_h1_ADC_LSB->At(configlist[i]))->Draw("same");
      gPad->Update();
    }
    currentpad++;
  }

  //------------MainCanvas3
  currentpad = 1;
  for (Int_t i=0 ; i<Nconfigs ; i++){
    MainCanvas3->cd(currentpad);
    if(newcanvas==1){
      ( Ar_h1_Efficiency_ideal->At(configlist[i]))->Draw();
      gPad->Update();
      MainCanvas3->Update();
    }else{
      ( Ar_h1_Efficiency_ideal->At(configlist[i]))->Draw("sames");
      gPad->Update();
      MainCanvas3->Update();
    }
    currentpad++;
  }

  //------------MainCanvas4
  currentpad = 1;
  for (Int_t i=0 ; i<Nconfigs ; i++){
    MainCanvas4->cd(currentpad);
    if(newcanvas==1){
      ((TH1F*)Ar_h1_Resolution_ResidualX_CoG_true->At(configlist[i]))->Draw();
      ((TH1F*)Ar_h1_Resolution_ResidualX_CoG_true->At(configlist[i]))->Fit("gaus","","same");
      gPad->Update();
      MainCanvas4->Update();
    }else{
      ((TH1F*)Ar_h1_Resolution_ResidualX_CoG_true->At(configlist[i]))->Draw("same");
      ((TH1F*)Ar_h1_Resolution_ResidualX_CoG_true->At(configlist[i]))->Fit("gaus","","same");
      gPad->Update();
      MainCanvas4->Update();
    }
    currentpad++;
  }





  MainCanvas1->Update();
  MainCanvas2->Update();
  MainCanvas3->Update();
  MainCanvas4->Update();

  //------------Compute efficiency, average multiplicity, resolution, fake rate...

  //TClonesArray       *myDIGResultArray = mydigEvent->GetParticle();
  //DIGResult* theresult = 0;
  // theresult = (DIGResult*)GetResult(configlist[i])->At(configlist[i]);  
  //TheParticle=(DIGParticle*) myDIGParticleArray->At(j) ;

  for (Int_t i=0 ; i<Nconfigs ; i++){
    cout<<" i config[i] config eff"<<i<<" "<<configlist[i]<<" "<<GetResult(configlist[i])->GetConfigNumber()<<" "<<GetResult(configlist[i])->GetIdealEfficiency()<<endl;
  }

}
//______________________________________________________________________________
//  
void  DIGMAPS::PrintConfigurations()
{
  //  fNumberOfConfigurations
    Int_t    iconfigcounter = 0;
    Int_t TotalNumberOfBeams = fBeamN;
    Int_t TotalNumberOfPlanes = fPlanesN;
    Int_t TotalNumberOfADCs =fADCN;
    Int_t TotalNumberOfTransports =fTransportN;
    std::cout<<"************************************************************** "<<endl;
    std::cout<<"           List of all configurations "<<endl;
    std::cout<<"num |Thet|Phi |Pit x X y|epi |Nois|Temp|Nbits|lin|LSB  |gain   |Model|"<<endl;
    for (Int_t ibeam = 0; ibeam < TotalNumberOfBeams ; ibeam++){    
      for (Int_t iplane = 0; iplane < TotalNumberOfPlanes ; iplane++){    
	for (Int_t iadc = 0; iadc < TotalNumberOfADCs ; iadc++){    
	  for (Int_t itransport = 0; itransport < TotalNumberOfTransports ; itransport++){    

	    Char_t charac[800];
	    sprintf(charac,"%4d|%4.1f|%4.1f|%4.1fX%4.1f|%4.1f|%4.1f|%4.1f|%2d   |%d  |%5.2f|%7.2f|%d    |\n",
		    iconfigcounter,
		    GetBeam(ibeam)->GetThetaIncidentDeg(),
		    GetBeam(ibeam)->GetPhiIncidentDeg(),
		    GetPlane(iplane)->GetPitchX(),
		    GetPlane(iplane)->GetPitchY(),
		    GetPlane(iplane)->GetZdimension(),
		    GetPlane(iplane)->GetNoiseElectrons(),
		    GetPlane(iplane)->GetTemperature(),
		    GetADC(iadc)->GetNbits(),
		    GetADC(iadc)->GetADC_linear(),
		    GetADC(iadc)->GetLSB(),
		    GetADC(iadc)->GetElectron_Conversion(),
		    GetTransport(itransport)->GetChargeModel())
	      ;	    
	    std::cout<<charac;
	    iconfigcounter++;
	  }
	}
      }
    }
    std::cout<<"num |Thet|Phi |Pit x X y|epi |Nois|Temp|Nbits|lin|LSB  |gain   |Model|"<<endl;
    std::cout<<"************************************************************** "<<endl;


}
//______________________________________________________________________________
//  
void  DIGMAPS::InspectEvent(Int_t EventNumber)
{
  DIGAction *myAction = GetAction();
  Char_t *myDoit = myAction->GetDoit();  
  Int_t myIntAction = 0;
  Char_t action2[200] = "plot";
  if(!strncmp( action2, myDoit,strlen(action2))  ){
    cout<<" WE CAN PLOT THAT !"<<endl;
    myIntAction=2;
  }else{
    cout<<" No action plot done before !"<<endl;
    return;
  }

  cout<<" Event number "<<EventNumber <<"is observed"<<endl;


  delete myAction;
  delete myDoit;

}
//______________________________________________________________________________
//  
void  DIGMAPS::RunConfiguration(Int_t configcounter, Int_t BeamNumber, Int_t PlaneNumber, Int_t ADCNumber, Int_t TransportNumber) 
{ 
  //compute visible surface.
  //compute active length, xlength, Y length, output position.

  //----compute density parameters
  //total surface in um^2
  Float_t totalsurface = (GetPlane(PlaneNumber)-> GetXdimension()) *  (GetPlane(PlaneNumber)-> GetYdimension());
  //  Compute Lambda value of the Poisson law for a given surface:
  Float_t Lambda_poisson = totalsurface * 0.000001 *  (GetBeam(BeamNumber)->GetParticleDensity());
  Double_t xran,yran,zran;


  GlobalSeed++;
  TRandom3 *r3 = new TRandom3(GlobalSeed);

  Int_t NumberOfEvents = GetBeam(BeamNumber)->GetNumberOfEvents();

  //-------------------------------------------------------------------------------------
  //--------------Loop on events.
  //-------------------------------------------------------------------------------------
  DIGParticle *fdigparticle=0;
  //DIGReadoutmap *fdigreadoutmap=0;
  for (Int_t iEvent = 0; iEvent < NumberOfEvents ; iEvent++){ 
    fdigevent = new DIGEvent();
    //---generate incident particles.
    Int_t numberofparticles = 0;
    if(GetBeam(BeamNumber)->GetBeamOption()==1){
      numberofparticles = PoissonLaw( Lambda_poisson);
    }else{
      numberofparticles=1;
    }

    std::cout<<"-----event "<<iEvent<<" with "<<numberofparticles<<" particles produced"<<endl;
    //---------------------------------------------
    //--------------Loop on  particles
    //---------------------------------------------
    for (Int_t ipart = 0; ipart < numberofparticles ; ipart++){ 
      
      //---------Energy deposition generation
      //-------------------
      //-----------Model 1: 
      /*
      //Energy in e-:(800 e- on ten microns)
      Float_t Energy = LandauLaw(800.0,180.0); 
      while(Energy>20000){
	cout<<"Energy too high -> Energy regenerated"<<Energy<<endl;
	Energy = LandauLaw(800.0,180.0);
      }
      //scale Energy with epitaxial thickness
      //Energy = Energy * GetPlane(PlaneNumber)->GetZdimension() / 10.0;
      //scale with incident angle
      Float_t mtotallentgh = (GetPlane(PlaneNumber)->GetZdimension())
	/ TMath::Cos((GetBeam(BeamNumber)->GetThetaIncidentDeg())*PI/180.0);
      //rescale energy (e-) with effective pitch:
      Energy = Energy * mtotallentgh / 10.0; 
      */
      //-------------------
      //-----------Model 2:
      //scale with incident angle
      Float_t mtotallentgh = (GetPlane(PlaneNumber)->GetZdimension())
	/ TMath::Cos((GetBeam(BeamNumber)->GetThetaIncidentDeg())*PI/180.0);
      Float_t EnergyMPV = 800.0 * mtotallentgh / 10.0;  
      Float_t EnergySIGMA = 180.0 * mtotallentgh / 10.0;  
      Float_t Energy = LandauLaw(EnergyMPV,EnergySIGMA); 
      while(Energy>20000){
	cout<<"Energy too high -> Energy regenerated"<<Energy<<endl;
	Energy = LandauLaw(EnergyMPV,EnergySIGMA);
      }
      //-------------------------
      //---------compute random entry point:
      if(GetBeam(BeamNumber)->GetBeamOption()==1){
	xran = r3->Rndm()*(GetPlane(PlaneNumber)-> GetXdimension());
	yran = r3->Rndm()*(GetPlane(PlaneNumber)-> GetYdimension());
      }else{
	xran = (float(GetPlane(PlaneNumber)->GetNpixelsX()/2) + r3->Rndm() )* (GetPlane(PlaneNumber)->GetPitchX());
 	yran = (float(GetPlane(PlaneNumber)->GetNpixelsY()/2) + r3->Rndm() )* (GetPlane(PlaneNumber)->GetPitchY());
     }
      zran=0.0;
      Float_t thetapos = GetBeam(BeamNumber)->GetThetaIncidentDeg();
      Float_t phipos = GetBeam(BeamNumber)->GetPhiIncidentDeg();//r3->Rndm()*360.0;
      Float_t thetaposrad = thetapos*PI/180.0;
      Float_t phiposrad = phipos*PI/180.0;
      //compute exit position given the incident angle:
      Float_t totalXlength=(GetPlane(PlaneNumber)->GetZdimension()) *TMath::Tan(thetaposrad)*TMath::Cos(phiposrad);
      Float_t totalYlength=(GetPlane(PlaneNumber)->GetZdimension()) *TMath::Tan(thetaposrad)*TMath::Sin(phiposrad);
      Float_t outputXpos=xran+totalXlength;
      Float_t outputYpos=yran+totalYlength;
      Float_t outputZpos=(GetPlane(PlaneNumber)->GetZdimension());
      
      fdigparticle = new DIGParticle(xran,yran,zran,outputXpos,outputYpos,outputZpos,Energy);
      //---------charge generation
      fdigparticle->ComputeChargeDeposition(GetPlane(PlaneNumber)->GetSegmentSize(), 
		       GetPlane(PlaneNumber)->GetMaximumSegmentSize(),
		       GetPlane(PlaneNumber)->GetMaximumChargePerSegment());
      //---------charge transport
      fdigparticle->ComputeChargeTransport(GetPlane(PlaneNumber),GetTransport(TransportNumber));
      //---------random noise (should be removed if one wants to avoid double noise on double hit pixels)
      fdigparticle->AddRandomNoise(GetPlane(PlaneNumber));
      //---------ADC (stored only for reference)
      fdigparticle->AnalogToDigitalconversion(GetADC(ADCNumber), GetPlane(PlaneNumber) );
      //fdigparticle->PrintInfo();
      //---------Add particle to fDIGParticleArray:
      //cout<<" TESTT 0 1 "<<ipart<<endl;
      fdigevent->AddParticle(*fdigparticle);
      std::vector<Float_t> chargevector;
      std::vector<Int_t> pixmapvector;
      chargevector = fdigparticle->GetAnalogCharge();
      pixmapvector = fdigparticle->GetPixelMap();
      for (Int_t ipix=0 ; ipix<fdigparticle->GetNpixels() ; ipix++){
	(fdigevent->GetReadoutmap())->UpdatePixel(chargevector[ipix], pixmapvector[ipix]);	
      }
      //fdigparticle->Clear();
      delete fdigparticle;      
    }
    //---------------------------------------------
    //----------end loop on particles
    //---------------------------------------------
    // fdigevent->PrintInfo();
 
    //---------Build readout map:
    (fdigevent->GetReadoutmap())->AnalogToDigitalconversion(GetADC(ADCNumber), GetPlane(PlaneNumber));
     //---------Build clusters:
    fdigevent->BuildTrueClusters(GetPlane(PlaneNumber));
    fdigevent->SetConfigurationNumber(configcounter);
    //---------Print Info on event if needed
    //fdigevent->PrintInfo();
    //---------P
    TClonesArray *particules   = fdigevent->GetParticle();
    if( fdigevent->GetNParticles() != (particules->GetLast()+1)){
      cout<< "DIGMAPS 4::PrintInfo WARNING PROBLEM IN PARTICLES RECORDING"<<endl;
    }
    //--------- store event.
    fdigbeam  = (DIGBeam*)GetBeam(BeamNumber)->Clone();
    fdigplane = (DIGPlane*)GetPlane(PlaneNumber)->Clone();
    fdigadc   = (DIGADC*)GetADC(ADCNumber)->Clone();
    fdigtransport   = (DIGTransport*)GetTransport(TransportNumber)->Clone();
    ftree->Fill();

  } //end loop on events
  
 

  delete r3;
}
//______________________________________________________________________________
//  
void  DIGMAPS::ChargeGeneration(DIGParticle& aDIGParticle, Float_t StartingSegmentSize, Float_t MaximumSegmentSize,
					  Float_t MaximumChargePerSegment) 
{ 
  aDIGParticle.ComputeChargeDeposition(StartingSegmentSize, MaximumSegmentSize,MaximumChargePerSegment) ;
}
//______________________________________________________________________________
//  
Int_t  DIGMAPS::PoissonLaw(Float_t Lambda) 
{ 
  Int_t n;
  GlobalSeed++;
  TRandom3 *r3 = new TRandom3(GlobalSeed);
  //Float_t smear = r3->Rndm(245);
  n = r3->Poisson(Lambda);

  //  TMath::PoissonI(x,Lambda)
  delete r3;
  return n;
    
}
//______________________________________________________________________________
//  
Double_t  DIGMAPS::LandauLaw(Double_t mean, Double_t sigma) 
{ 
  Double_t x;
  GlobalSeed++;
  TRandom3 *r3 = new TRandom3(GlobalSeed);
  //Float_t smear = r3->Rndm(245);
  x = r3->Landau(mean,sigma);
  //  TMath::PoissonI(x,Lambda)
  delete r3;
  return x;
}
//______________________________________________________________________________
//  
Double_t  DIGMAPS::GaussianLaw(Double_t mean, Double_t sigma) 
{ 
  Double_t x;
  GlobalSeed++;
  TRandom3 *r3 = new TRandom3(GlobalSeed);
  //Float_t smear = r3->Rndm(245);
  x = r3->Gaus(mean,sigma);
  //  TMath::PoissonI(x,Lambda)
  delete r3;
  return x;
}
//_______________________________________________________________________________________
//
  Double_t DIGMAPS::Lorentz2D(Double_t *x, Double_t *par){ 
    //x[0] = x
    //x[1] = y
    // par[0] = Gamma
    // par[1] = x0
    // par[2] = y0
    // par[3] = norm
  if(par[0]>0){
    Double_t Pi = 3.141592653;
    return par[3]*par[0]/Pi/((x[0]-par[1])*(x[0]-par[1])+(x[1]-par[2])*(x[1]-par[2])+par[0]*par[0]) ; 
  }else{
    return 0;
  }
}
//______________________________________________________________________________
//  
void  DIGMAPS::SetfAction(TString action)
{
  fAction = action;
}

//______________________________________________________________________________
//  
void  DIGMAPS::SetIsOutputfile(Bool_t IsOutputfile)
{
  fIsOutputfile = IsOutputfile;
}
//______________________________________________________________________________
//  
void  DIGMAPS::SetAction(DIGAction *aDIGAction)
{ 
  fDIGAction = aDIGAction;
}
//______________________________________________________________________________
//  
void  DIGMAPS::SetNumberOfConfigurations(Int_t NumberOfConfiguration)
{ 
  fNumberOfConfigurations = NumberOfConfiguration;
}
//______________________________________________________________________________
//  
void  DIGMAPS::SetConfigPath(TString aCP) 
{ 
  fConfigPath = aCP;  
}


//______________________________________________________________________________
//  
void DIGMAPS::SetConfigFileName(TString aCFN) 
{ 
  fConfigFileName = aCFN;     
} 

//______________________________________________________________________________
//  
void DIGMAPS::SetConfigPathAndFileName() 
{ 
  fConfigPathAndFileName = fConfigPath + fConfigFileName;     
} 

//______________________________________________________________________________
//  
void DIGMAPS::SetConfigPathAndFileName(TString aCP,TString aCFN) 
{ 
  fConfigPath = aCP;  
  fConfigFileName = aCFN;     
  fConfigPathAndFileName = fConfigPath + fConfigFileName;  
} 
//______________________________________________________________________________
//  
TString  DIGMAPS::GetConfigPath() 
{ 
  return fConfigPath;  
}
//______________________________________________________________________________
//  
TString DIGMAPS::GetConfigFileName() 
{ 
  return fConfigFileName;     
} 
//______________________________________________________________________________
//  
TString DIGMAPS::GetConfigPathAndFileName() 
{ 
  return fConfigPathAndFileName;
} 







//______________________________________________________________________________
//  
void  DIGMAPS::SetOutputPath(TString outp) 
{ 
  fOutputPath = outp;  
 
}


//______________________________________________________________________________
//  
void DIGMAPS::SetOutputFileName(TString outf) 
{ 
  fOutputFileName = outf;     
} 

//______________________________________________________________________________
//  
void DIGMAPS::SetOutputPathAndFileName() 
{ 
  fOutputPathAndFileName = fOutputPath + fOutputFileName;     
} 

//______________________________________________________________________________
//  
void DIGMAPS::SetOutputPathAndFileName(TString outp,TString outf) 
{ 
  fOutputPath = outp;  
  fOutputFileName = outf;     
  fOutputPathAndFileName = fOutputPath + fOutputFileName;  
} 




//______________________________________________________________________________
//  
DIGPlane* DIGMAPS::GetPlane(Int_t aPlaneNumber){
  return (DIGPlane*)fDIGPlaneArray->At(aPlaneNumber);
}
//______________________________________________________________________________
//  
DIGADC* DIGMAPS::GetADC(Int_t anADCNumber){
  return (DIGADC*)fDIGADCArray->At(anADCNumber);
}
//______________________________________________________________________________
//  
DIGTransport* DIGMAPS::GetTransport(Int_t aTransportNumber){
  return (DIGTransport*)fDIGTransportArray->At(aTransportNumber);
}
//______________________________________________________________________________
//  
DIGBeam* DIGMAPS::GetBeam(Int_t aBeamNumber){
  return (DIGBeam*)fDIGBeamArray->At(aBeamNumber);
}
//______________________________________________________________________________
//  
void   DIGMAPS::ReadTree(TString StringTree){

  cout<<StringTree<<endl;

}

