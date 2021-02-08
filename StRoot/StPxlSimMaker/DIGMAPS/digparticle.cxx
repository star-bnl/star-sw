///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//                                                                                           //
//     DIGParticle                                                                           //
//                                                                                           //
//     particle class                                                                        //
//       contains: -entry and exit point of the particle into the plane                      //
//                 -segment list (Charge, position) of the track                             //
//                 -pixel list (number, charge) where charge has been collected              //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include <digparticle.h>

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
#include <TF2.h>
#include <TProfile2D.h>
#include <TH2.h>


#include "digplane.h"
#include "digadc.h"
#include "digtransport.h"

using namespace std;

extern Int_t GlobalSeed;

//_______________________________________________________________________________________
//
  Double_t Lorentz2D(Double_t *x, Double_t *par){ 
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
//_______________________________________________________________________________________
//
Double_t SumGaus2D(Double_t *x, Double_t *par){
  //par[0] Norm_1
  //par[1] x0_1
  //par[2] sigma_x_1
  //par[3] y0_1
  //par[4] sigma_y_1
  //par[5] Norm_2
  //par[6] x0_2
  //par[7] sigma_x_2
  //par[8] y0_2
  //par[9] sigma_y_2
  if((par[2]!=0.0) && (par[4]!=0.0) && (par[7]!=0.0) && (par[9]!=0.0)){
    double rx = (x[0]-par[1])/par[2];
    double ry = (x[1]-par[3])/par[4];
    double rx2 = (x[0]-par[6])/par[7];
    double ry2 = (x[1]-par[8])/par[9];
    return par[0]*( TMath::Exp(-(rx*rx+ry*ry)/2.0)+par[5]*TMath::Exp(-(rx2*rx2+ry2*ry2)/2.0) ) ;
  }else{
    return par[0]+par[1]+par[2]+par[3]+par[4]+par[5]+par[6]+par[7]+par[8]+par[9];
    //return 0;
  }
}
//_______________________________________________________________________________________
//
Double_t SumGausLorentz2D(Double_t *x, Double_t *par){
  //par[0] Norm_1
  //par[1] x0_1
  //par[2] sigma_x_1
  //par[3] y0_1
  //par[4] sigma_y_1
  // par[5] = Gamma
  // par[6] = x0
  // par[7] = y0
  // par[8] = norm
  //0 0.146571+-0 0+-0 7.55129+-0 0+-0 5.94133+-0 3.2357+-0 0+-0 0+-0 27.3458+-0 chi2 / NDF = 3126.16 / 10009 = 0.312335
  //1 0.1459+-0 0+-0 14.7913+-0 0+-0 11.7393+-0 6.86661+-0 0+-0 0+-0 58.7235+-0 chi2 / NDF = 7458.34 / 10009 = 0.745163
  //2 0.149045+-0 0+-0 22.2327+-0 0+-0 17.6162+-0 9.20632+-0 0+-0 0+-0 90.0768+-0 chi2 / NDF = 12700.5 / 10009 = 1.26891
  //3 0.168693+-0 0+-0 27.9107+-0 0+-0 21.8062+-0 12.9946+-0 0+-0 0+-0 102.019+-0 chi2 / NDF = 8275.98 / 10009 = 0.826854
  Double_t Pi = 3.141592653;
  if((par[2]!=0.0) && (par[4]!=0.0) ){
    double rx = (x[0]-par[1])/par[2];
    double ry = (x[1]-par[3])/par[4];
    return par[0]*( TMath::Exp(-(rx*rx+ry*ry)/2.0)
    		    +par[8]*par[5]/Pi/((x[0]-par[6])*(x[0]-par[6])+(x[1]-par[7])*(x[1]-par[7])+par[5]*par[5])
    		    );
  }else{
    return 0;  
  }
}
//_______________________________________________________________________________________
//
Double_t SumGausLorentz2Dnew(Double_t *x, Double_t *par){
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

  /*   for (int j=0;j<10; j++) {
       cout<<"par " <<j<<" "<<par[j]<<endl;
       }
  */
  Double_t Pi = 3.141592653;
  if((par[2]!=0.0) && (par[4]!=0.0) ){
    double rx = (x[0]-par[1])/par[2];
    double ry = (x[1]-par[3])/par[4];

    double rx2 = (x[0])/par[10];
    double ry2 = (x[1])/par[10];


    if( TMath::Sqrt(x[0]*x[0]+x[1]*x[1])<200.0 ){
          return par[0]*( TMath::Exp(-(rx*rx+ry*ry)/2.0)
    		    +par[8]*par[5]/Pi/((x[0]-par[6])*(x[0]-par[6])+(x[1]-par[7])*(x[1]-par[7])+par[5]*par[5])
		    +par[9]*TMath::Exp(-(rx2*rx2+ry2*ry2)/2.0)
    		    );
    }else{
      return par[0]*( TMath::Exp(-(rx*rx+ry*ry)/2.0)
    		    +par[8]*par[5]/Pi/((x[0]-par[6])*(x[0]-par[6])+(x[1]-par[7])*(x[1]-par[7])+par[5]*par[5])
		    
    		    );
    }
  }else{
    return 0;
 
  }
}
//_______________________________________________________________________________________
//
Double_t SumGausLorentz1D(Double_t *x, Double_t *par){

  //par[0] Norm_g
  //par[1] x0_g
  //par[2] sigma_g
  // par[3] = Gamma_lor
  // par[4] = x0_lor
  // par[5] = norm_lor



  /*   for (int j=0;j<10; j++) {
       cout<<"par " <<j<<" "<<par[j]<<endl;
       }
  */
  Double_t Pi = 3.141592653;
  if((par[2]!=0.0) ){
    Double_t rx = (x[0]-par[1])/par[2];
    Double_t tempoutput;
    tempoutput= par[0]*( TMath::Exp(-(rx*rx)/2.0)
		    +par[5]*par[3]/Pi/ ((x[0]-par[4])*(x[0]-par[4]) +par[3]*par[3])
    		    );
    //  cout<<"SumGausLorentz1D " <<tempoutput<<endl;
    return tempoutput;
   
  }else{
    return 0;
  }
}
//==============================================================================
ClassImp(DIGParticle)
DIGParticle::DIGParticle()  
{
  //
  // default constructor
  //

}
//______________________________________________________________________________
//  
DIGParticle::DIGParticle(Float_t EntryX, Float_t EntryY, Float_t EntryZ, 
			 Float_t ExitX, Float_t ExitY, Float_t ExitZ, Float_t Energy_deposited)
{
  fEntryX = EntryX;
  fEntryY = EntryY;
  fEntryZ = EntryZ;
  fExitX = ExitX ;
  fExitY = ExitY ;
  fExitZ = ExitZ ;
  fEnergy_deposited = Energy_deposited;
  fNSegment =0;
  fSegmentX.clear();
  fSegmentY.clear();
  fSegmentZ.clear();
  fSegmentCharge.clear();

  fNpixels=0;
  fPixelMap.clear();
  fAnalogChargeMap.clear();
  fDigitalChargeMap.clear();//not used ?
 
}
//______________________________________________________________________________
//  
DIGParticle::DIGParticle(DIGParticle & adigparticle)  : TObject()
{
  fEntryX = adigparticle.GetEntryX();
  fEntryY = adigparticle.GetEntryY();
  fEntryZ = adigparticle.GetEntryZ();
  fExitX = adigparticle.GetExitX();
  fExitY = adigparticle.GetExitY();
  fExitZ = adigparticle.GetExitZ();
  fEnergy_deposited = adigparticle.GetEnergy_deposited();
  fNSegment =adigparticle.GetNSegment();
  fSegmentX.resize(fNSegment);
  fSegmentY.resize(fNSegment);
  fSegmentZ.resize(fNSegment);
  fSegmentCharge.resize(fNSegment);
  // a modifier:
  for (Int_t i=0 ; i<fNSegment ; i++){
    fSegmentX[i] = adigparticle.GetSegmentX()[i];
    fSegmentY[i] = adigparticle.GetSegmentY()[i];
    fSegmentZ[i] = adigparticle.GetSegmentZ()[i];
    fSegmentCharge[i] = adigparticle.GetSegmentCharge()[i];
  }
  fNpixels = adigparticle.GetNpixels();
  fPixelMap.resize(fNpixels);
  fAnalogChargeMap.resize(fNpixels);
  fDigitalChargeMap.resize(fNpixels);
  for (Int_t i=0 ; i<fNpixels ; i++){
    fPixelMap[i] = adigparticle.GetPixelMap()[i];
    fAnalogChargeMap[i] = adigparticle.GetAnalogCharge()[i];
    fDigitalChargeMap[i] = adigparticle.GetDigitalCharge()[i];
  }
}
//______________________________________________________________________________
//  


DIGParticle::~DIGParticle() { // 
  // virtual destructor
  //
  //  delete fLayers;
}
//______________________________________________________________________________
//  
void DIGParticle::Clear(const Option_t *) 
{

}
//______________________________________________________________________________
//  
void DIGParticle::ComputeChargeDeposition(Float_t StartingSegmentSize, Float_t MaximumSegmentSize,
					  Float_t MaximumChargePerSegment) 
{
  Float_t SegmentSize = StartingSegmentSize;
  Float_t TotalLength = TMath::Sqrt((fExitX-fEntryX)*(fExitX-fEntryX)
				    +(fExitY-fEntryY)*(fExitY-fEntryY)
				    +(fExitZ-fEntryZ)*(fExitZ-fEntryZ));

  Float_t ChargePerSegment = 0.0;
  if(SegmentSize<0.0){
    SegmentSize=0.001;
  }
  fNSegment = int(TotalLength*1.000001/SegmentSize) ;
  if(fNSegment<1){
    fNSegment=1;
  }
  SegmentSize = TotalLength/float(fNSegment);
  ChargePerSegment = fEnergy_deposited / float(fNSegment);

  while((SegmentSize>MaximumSegmentSize)&&(ChargePerSegment > MaximumChargePerSegment)){
    Int_t newNSegment = int(fNSegment *1.1);
    if(newNSegment==fNSegment){fNSegment++;}
    SegmentSize = TotalLength/float(fNSegment);
    ChargePerSegment = fEnergy_deposited / float(fNSegment);
  }
  Float_t xstep = fExitX-fEntryX;
  Float_t ystep = fExitY-fEntryY;
  Float_t zstep = fExitZ-fEntryZ;


  for (Int_t i=0 ; i<fNSegment ; i++){
    fSegmentX.push_back(fEntryX + (float(i+0.5)* xstep/float(fNSegment)) );
    fSegmentY.push_back(fEntryY + (float(i+0.5)* ystep/float(fNSegment)) );
    fSegmentZ.push_back(fEntryZ + (float(i+0.5)* zstep/float(fNSegment)) );
    fSegmentCharge.push_back(ChargePerSegment );
  }
    

}
//______________________________________________________________________________
//  

void DIGParticle::PrintInfo() {
  std::cout<<"---------Particle properties------------- "<<endl;
  std::cout<<"fEntryX fEntryY fEntryZ"<<endl;
  std::cout<<fEntryX<<" "<< fEntryY<<" "<<fEntryZ<<endl;
  std::cout<<"fExitX fExitY fExitZ fEnergy_deposited"<<endl;
  std::cout<<fExitX <<" "<<fExitY <<" "<<fExitZ <<" "<<fEnergy_deposited <<endl;
  std::cout<<fNSegment<<" Segments X Y Z Charge"<<endl;
  /*  for (Int_t i=0 ; i<fNSegment ; i++){
    std::cout<<i<<" "<< fSegmentX[i]<<" "<<fSegmentY[i] <<" "<<fSegmentZ[i] <<" "<< fSegmentCharge[i]<<endl;
    }*/
  cout<<" size vectors "<<  fPixelMap.size()<<" "<<fAnalogChargeMap.size()<<" "<<fDigitalChargeMap.size()<<endl;
  std::cout<<fNpixels<<" fNpixels map analog digital "<<endl;
  for (Int_t i=0 ; i<fNpixels ; i++){
    std::cout<<i<<" "<<fPixelMap[i]<<" "<<fAnalogChargeMap[i]<<" "<<fDigitalChargeMap[i]<<endl;
  }
}
//______________________________________________________________________________
//   
void DIGParticle::ComputeChargeTransport(DIGPlane *aDIGPlane,DIGTransport *aDIGTransport){

  Float_t pitchX =     aDIGPlane->GetPitchX();
  Float_t pitchY =     aDIGPlane->GetPitchY();

  Float_t lorentz2Dmodel_Cp0 = aDIGTransport->GetLorentz2DModel_Cp0();
  Float_t lorentz2Dmodel_Cp1 = aDIGTransport->GetLorentz2DModel_Cp1();
  Float_t rangelimit_inpitchunit = aDIGTransport->GetRangeLimit_InPitchUnit();

  Int_t ChargeModel = aDIGTransport->GetChargeModel();

  Float_t Gauss2DModel_sigma1_Cp0 = aDIGTransport->GetGauss2DModel_sigma1_Cp0();
  Float_t Gauss2DModel_sigma1_Cp1 = aDIGTransport->GetGauss2DModel_sigma1_Cp1();
  Float_t Gauss2DModel_sigma2_Cp0 = aDIGTransport->GetGauss2DModel_sigma2_Cp0();
  Float_t Gauss2DModel_sigma2_Cp1 = aDIGTransport->GetGauss2DModel_sigma2_Cp1();
  Float_t Gauss2DModel_weight = aDIGTransport->GetGauss2DModel_weight();

  Float_t LorGaussModel_Norm1_Cp0  = aDIGTransport->GetLorGaussModel_Norm1_Cp0();
  Float_t LorGaussModel_Norm1_Cp1 = aDIGTransport->GetLorGaussModel_Norm1_Cp1();
  Float_t LorGaussModel_Norm1_Cp2 = aDIGTransport->GetLorGaussModel_Norm1_Cp2();
  Float_t LorGaussModel_sigma_Cp0 = aDIGTransport->GetLorGaussModel_sigma_Cp0();
  Float_t LorGaussModel_sigma_Cp1 = aDIGTransport->GetLorGaussModel_sigma_Cp1();
  Float_t LorGaussModel_C_Cp0 = aDIGTransport->GetLorGaussModel_C_Cp0();
  Float_t LorGaussModel_C_Cp1 = aDIGTransport->GetLorGaussModel_C_Cp1();
  Float_t LorGaussModel_Norm_Cp0 = aDIGTransport->GetLorGaussModel_Norm_Cp0();
  Float_t LorGaussModel_Norm_Cp1 = aDIGTransport->GetLorGaussModel_Norm_Cp1();

  Float_t   lorlorgausModel_Norm1_Cp0 = aDIGTransport->GetlorlorgausModel_Norm1_Cp0() ;
  Float_t   lorlorgausModel_Norm1_Cp1 = aDIGTransport->GetlorlorgausModel_Norm1_Cp1() ;
  Float_t   lorlorgausModel_x01_Cp0 = aDIGTransport->GetlorlorgausModel_x01_Cp0() ;
  Float_t   lorlorgausModel_x01_Cp1 = aDIGTransport->GetlorlorgausModel_x01_Cp1() ;
  Float_t   lorlorgausModel_sigmax1_Cp0  = aDIGTransport->GetlorlorgausModel_sigmax1_Cp0();
  Float_t   lorlorgausModel_sigmax1_Cp1  = aDIGTransport->GetlorlorgausModel_sigmax1_Cp1();
  Float_t   lorlorgausModel_y01_Cp0  = aDIGTransport->GetlorlorgausModel_y01_Cp0();
  Float_t   lorlorgausModel_y01_Cp1 = aDIGTransport->GetlorlorgausModel_y01_Cp1() ;
  Float_t   lorlorgausModel_sigmay1_Cp0  = aDIGTransport->GetlorlorgausModel_sigmay1_Cp0();
  Float_t   lorlorgausModel_sigmay1_Cp1  = aDIGTransport->GetlorlorgausModel_sigmay1_Cp1();
  Float_t   lorlorgausModel_Gamma_Cp0 = aDIGTransport->GetlorlorgausModel_Gamma_Cp0() ;
  Float_t   lorlorgausModel_Gamma_Cp1 = aDIGTransport->GetlorlorgausModel_Gamma_Cp1() ;
  Float_t   lorlorgausModel_x0_Cp0 = aDIGTransport->GetlorlorgausModel_x0_Cp0() ;
  Float_t   lorlorgausModel_x0_Cp1 = aDIGTransport->GetlorlorgausModel_x0_Cp1() ;
  Float_t   lorlorgausModel_y0_Cp0 = aDIGTransport->GetlorlorgausModel_y0_Cp0() ;
  Float_t   lorlorgausModel_y0_Cp1 = aDIGTransport->GetlorlorgausModel_y0_Cp1() ;
  Float_t   lorlorgausModel_norm_Cp0 = aDIGTransport->GetlorlorgausModel_norm_Cp0() ;
  Float_t   lorlorgausModel_norm_Cp1 = aDIGTransport->GetlorlorgausModel_norm_Cp1() ;
  Float_t   lorlorgausModel_normgaus2_Cp0  = aDIGTransport->GetlorlorgausModel_normgaus2_Cp0();
  Float_t   lorlorgausModel_normgaus2_Cp1 = aDIGTransport->GetlorlorgausModel_normgaus2_Cp1() ;
  Float_t   lorlorgausModel_sigma2_Cp0 = aDIGTransport->GetlorlorgausModel_sigma2_Cp0() ;
  Float_t   lorlorgausModel_sigma2_Cp1 = aDIGTransport->GetlorlorgausModel_sigma2_Cp1() ;

  Float_t   l1dimgauslor_Norm_g_1st =    aDIGTransport->Getf1dimgauslor_Norm_g_1st();	    
  Float_t   l1dimgauslor_x0_g_1st = 	 aDIGTransport->Getf1dimgauslor_x0_g_1st();	   
  Float_t   l1dimgauslor_sigma_g_1st = 	 aDIGTransport->Getf1dimgauslor_sigma_g_1st();   
  Float_t   l1dimgauslor_Gamma_lor_1st = aDIGTransport->Getf1dimgauslor_Gamma_lor_1st(); 
  Float_t   l1dimgauslor_x0_lor_1st = 	 aDIGTransport->Getf1dimgauslor_x0_lor_1st();	   
  Float_t   l1dimgauslor_norm_lor_1st =  aDIGTransport->Getf1dimgauslor_norm_lor_1st();  
  Float_t   l1dimgauslor_Norm_g_2nd = 	 aDIGTransport->Getf1dimgauslor_Norm_g_2nd();	   
  Float_t   l1dimgauslor_x0_g_2nd = 	 aDIGTransport->Getf1dimgauslor_x0_g_2nd();	   
  Float_t   l1dimgauslor_sigma_g_2nd = 	 aDIGTransport->Getf1dimgauslor_sigma_g_2nd();   
  Float_t   l1dimgauslor_Gamma_lor_2nd = aDIGTransport->Getf1dimgauslor_Gamma_lor_2nd(); 
  Float_t   l1dimgauslor_x0_lor_2nd = 	 aDIGTransport->Getf1dimgauslor_x0_lor_2nd();	   
  Float_t   l1dimgauslor_norm_lor_2nd =  aDIGTransport->Getf1dimgauslor_norm_lor_2nd();  


  GlobalSeed++;
  TRandom3 *r3 = new TRandom3(GlobalSeed);
  r3->SetSeed(GlobalSeed);

  TF2 *mymodel2D=0;
  TF1 *mymodel1D_1st=0;
  TF1 *mymodel1D_2nd=0;
  //-----------------------------------------------------
  // chose model
  //-----------------------------------------------------
  if(ChargeModel==1){
    //-----------------------------------------------------
    // model Lorentz
    //-----------------------------------------------------

    Double_t Cvalue=  lorentz2Dmodel_Cp0 + pitchX * lorentz2Dmodel_Cp1 ;

    mymodel2D = new TF2("funlor2d",Lorentz2D,-rangelimit_inpitchunit*pitchX,rangelimit_inpitchunit*pitchX,
			  -rangelimit_inpitchunit*pitchY,rangelimit_inpitchunit*pitchY,4);
    mymodel2D->SetParNames("C","X_{0}","Y_{0}","Norm");
    Double_t params1[] = {Cvalue,0.,0.,1.0};
    mymodel2D->SetParameters(params1);

 }else if(ChargeModel==2){
    //-----------------------------------------------------
    // 2xGaussian model 
    //-----------------------------------------------------
    //Double_t SumGaus2D(Double_t *x, Double_t *par){
    //par[0] Norm_1
    //par[1] x0_1
    //par[2] sigma_x_1
    //par[3] y0_1
    //par[4] sigma_y_1
    //par[5] Norm_2
    //par[6] x0_2
    //par[7] sigma_x_2
    //par[8] y0_2
    //par[9] sigma_y_2
    Double_t Gsigma1 = Gauss2DModel_sigma1_Cp0  + pitchX * Gauss2DModel_sigma1_Cp1 ;
    Double_t Gsigma2 = Gauss2DModel_sigma2_Cp0  + pitchX * Gauss2DModel_sigma2_Cp1 ;

    mymodel2D = new TF2("funlor2d",SumGaus2D,-rangelimit_inpitchunit*pitchX,rangelimit_inpitchunit*pitchX,
			  -rangelimit_inpitchunit*pitchY,rangelimit_inpitchunit*pitchY,10);
    mymodel2D->SetParNames("Norm_1","x0_1","sigma_x_1","y0_1","sigma_y_1","Norm_2","x0_2","sigma_x_2","y0_2","sigma_y_2");
    Double_t params1[] = {1.0,0.0,Gsigma1,0.0,Gsigma1,Gauss2DModel_weight,0.0,Gsigma2,0.0,Gsigma2,0.0};
    mymodel2D->SetParameters(params1);
  }else if(ChargeModel==3){
    //-----------------------------------------------------
    // Lorentz + Gauss model 
    //-----------------------------------------------------
    //par[0] Norm_1
    //par[1] x0_1
    //par[2] sigma_x_1
    //par[3] y0_1
    //par[4] sigma_y_1
    // par[5] = Gamma
    // par[6] = x0
    // par[7] = y0
    // par[8] = norm    
    Double_t Norm1Value =  LorGaussModel_Norm1_Cp0 + pitchX * LorGaussModel_Norm1_Cp1 + pitchX *pitchX * LorGaussModel_Norm1_Cp2;
    Double_t sigmaValue = LorGaussModel_sigma_Cp0 + pitchX *LorGaussModel_sigma_Cp1;
    Double_t Cvalue= LorGaussModel_C_Cp0 + pitchX *LorGaussModel_C_Cp1;
    Double_t NormValue =LorGaussModel_Norm_Cp0 + pitchX * LorGaussModel_Norm_Cp1;
    mymodel2D = new TF2("funlor2d",SumGausLorentz2D,-rangelimit_inpitchunit*pitchX,rangelimit_inpitchunit*pitchX,
			  -rangelimit_inpitchunit*pitchY,rangelimit_inpitchunit*pitchY,9);
    mymodel2D->SetParNames("Norm_1","x0_1","sigma_x_1","y0_1","sigma_y_1","Gamma","x0","y0","norm");
    Double_t params1[] = {Norm1Value,0.0,sigmaValue,0.0,sigmaValue,Cvalue,0.0,0.0,NormValue};
    mymodel2D->SetParameters(params1);
  }else if(ChargeModel==4){
   //-----------------------------------------------------
    // Lorentz + Gauss + Gauss model 
    //-----------------------------------------------------
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
    /*
      SumGausLorentz2Dnew
    */
    Double_t Norm1Value = lorlorgausModel_Norm1_Cp0  + pitchX * lorlorgausModel_Norm1_Cp1;
    Double_t x0_1Value =  lorlorgausModel_x01_Cp0 + pitchX * lorlorgausModel_x01_Cp1;
    Double_t sigma_x_1Value =  lorlorgausModel_sigmax1_Cp0 + pitchX * lorlorgausModel_sigmax1_Cp1;
    Double_t y0_1Value = lorlorgausModel_y01_Cp0 + pitchX * lorlorgausModel_y01_Cp1;
    Double_t sigma_y_1Value = lorlorgausModel_sigmay1_Cp0  + pitchX * lorlorgausModel_sigmay1_Cp1;
    Double_t GammaValue = lorlorgausModel_Gamma_Cp0 + pitchX * lorlorgausModel_Gamma_Cp1;
    Double_t x0Value = lorlorgausModel_x0_Cp0  + pitchX * lorlorgausModel_x0_Cp1;
    Double_t y0Value = lorlorgausModel_y0_Cp0 + pitchX * lorlorgausModel_y0_Cp1;
    Double_t normValue = lorlorgausModel_norm_Cp0 + pitchX * lorlorgausModel_norm_Cp1;
    Double_t normgaus2Value = lorlorgausModel_normgaus2_Cp0 + pitchX * lorlorgausModel_normgaus2_Cp1;
    Double_t sigma_2Value = lorlorgausModel_sigma2_Cp0 + pitchX * lorlorgausModel_sigma2_Cp1;
     mymodel2D = new TF2("funlor2d",SumGausLorentz2Dnew,-rangelimit_inpitchunit*pitchX,rangelimit_inpitchunit*pitchX,
			  -rangelimit_inpitchunit*pitchY,rangelimit_inpitchunit*pitchY,11);
     mymodel2D->SetParNames("Norm_1","x0_1","sigma_x_1","y0_1","sigma_y_1","Gamma","x0","y0","norm","normgaus2","sigma_2");
     Double_t params1[] = {Norm1Value,x0_1Value,sigma_x_1Value,y0_1Value,
			   sigma_y_1Value,GammaValue,x0Value,y0Value,normValue,normgaus2Value,sigma_2Value};
    mymodel2D->SetParameters(params1);
  }else if(ChargeModel==5){

    //-----------------------------------------------------
    // 1 dimension model with 2 function for each squares around the hit
    //-----------------------------------------------------
    // "Norm_g","x0_g","sigma_g","Gamma_lor","x0_lor","norm_lor"
    Double_t Norm_g_1st     =l1dimgauslor_Norm_g_1st ; 	 
    Double_t x0_g_1st       =l1dimgauslor_x0_g_1st ; 	  
    Double_t sigma_g_1st    =l1dimgauslor_sigma_g_1st ; 	 
    Double_t Gamma_lor_1st  =l1dimgauslor_Gamma_lor_1st ; 
    Double_t x0_lor_1st     =l1dimgauslor_x0_lor_1st ; 	 
    Double_t norm_lor_1st   =l1dimgauslor_norm_lor_1st ;  
    Double_t Norm_g_2nd     =l1dimgauslor_Norm_g_2nd ; 	 
    Double_t x0_g_2nd       =l1dimgauslor_x0_g_2nd ; 	  
    Double_t sigma_g_2nd    =l1dimgauslor_sigma_g_2nd ; 	 
    Double_t Gamma_lor_2nd  =l1dimgauslor_Gamma_lor_2nd ; 
    Double_t x0_lor_2nd     =l1dimgauslor_x0_lor_2nd ; 	 
    Double_t norm_lor_2nd   =l1dimgauslor_norm_lor_2nd ;  
    
    
    mymodel1D_1st = new TF1("namemodel1D_1st",SumGausLorentz1D,0,4.0*pitchX,6);
    mymodel1D_2nd = new TF1("namemodel1D_2nd",SumGausLorentz1D,0,4.0*pitchX,6);
    mymodel1D_1st->SetParNames("Norm_g","x0_g","sigma_g","Gamma_lor","x0_lor","norm_lor");
    mymodel1D_2nd->SetParNames("Norm_g","x0_g","sigma_g","Gamma_lor","x0_lor","norm_lor");
    Double_t params1[] = {Norm_g_1st,x0_g_1st,sigma_g_1st, Gamma_lor_1st,x0_lor_1st,norm_lor_1st};
    Double_t params2[] = {Norm_g_2nd,x0_g_2nd,sigma_g_2nd, Gamma_lor_2nd,x0_lor_2nd,norm_lor_2nd};
    mymodel1D_1st->SetParameters(params1);
    mymodel1D_2nd->SetParameters(params2);
    
    //std::cout<<" LIST OF PARAMETERS 1st "<<Norm_g_1st<<" "<<x0_g_1st<<" "<<sigma_g_1st<<" "<<Gamma_lor_1st<<" "<<x0_lor_1st<<" "<<norm_lor_1st<<endl;
    //std::cout<<" LIST OF PARAMETERS 2nd "<<Norm_g_2nd<<" "<<x0_g_2nd<<" "<<sigma_g_2nd<<" "<<Gamma_lor_2nd<<" "<<x0_lor_2nd<<" "<<norm_lor_2nd<<endl;
    //std::cout<<" function proba "<<mymodel1D_1st->Eval(0)<<" "<<mymodel1D_1st->Eval(15.0)<<mymodel1D_1st->Eval(38.)<<mymodel1D_1st->Eval(70.)<<endl;
    //std::cout<<" function proba "<<mymodel1D_2nd->Eval(0)<<" "<<mymodel1D_2nd->Eval(15.0)<<mymodel1D_2nd->Eval(38.)<<mymodel1D_2nd->Eval(70.)<<endl;

  }else{
    Double_t Gsigma1 = Gauss2DModel_sigma1_Cp0  + pitchX * Gauss2DModel_sigma1_Cp1 ;
    Double_t Gsigma2 = Gauss2DModel_sigma2_Cp0  + pitchX * Gauss2DModel_sigma2_Cp1 ;

    mymodel2D = new TF2("funlor2d",SumGaus2D,-rangelimit_inpitchunit*pitchX,rangelimit_inpitchunit*pitchX,
			  -rangelimit_inpitchunit*pitchY,rangelimit_inpitchunit*pitchY,10);
    mymodel2D->SetParNames("Norm_1","x0_1","sigma_x_1","y0_1","sigma_y_1","Norm_2","x0_2","sigma_x_2","y0_2","sigma_y_2");
    Double_t params1[] = {1.0,0.0,Gsigma1,0.0,Gsigma1,Gauss2DModel_weight,0.0,Gsigma2,0.0,Gsigma2,0.0};
    mymodel2D->SetParameters(params1);
 
  }


    //---------------------------------------------------------------------
    // method A: lorentz function define a probability density function which gives where the charge is collected.
    //---------------------------------------------------------------------
    /*
      Float_t Xdimension =     aDIGPlane->GetXdimension();
      Float_t Ydimension =     aDIGPlane->GetYdimension();
  
      for (Int_t i=0 ; i<fNSegment ; i++){
      //std::cout<<" i="<<i<<" x="<< fSegmentX[i]<<" y="<<fSegmentY[i] <<" z="<<fSegmentZ[i] <<" c="<< fSegmentCharge[i]<<endl;
      Double_t xrandom;
      Double_t yrandom;
      gRandom = new TRandom3;
      GlobalSeed++;
      gRandom->SetSeed(GlobalSeed);
      mylorentz2D->GetRandom2(xrandom,yrandom);
    
      Double_t xpos_aftertransport = fSegmentX[i] + xrandom;
      Double_t ypos_aftertransport = fSegmentY[i] + yrandom;
      //if charge reaches a x,y position outside the size of the detector, consider that the charge is lost.
      if((xpos_aftertransport>=0.0)
      &&(xpos_aftertransport<=Xdimension)
      &&(ypos_aftertransport>=0.0)
      &&(ypos_aftertransport<=Ydimension)){
      Int_t PixelReached =  GetPixelNumber(aDIGPlane, xpos_aftertransport ,  ypos_aftertransport );
      UpdatePixel(fSegmentCharge[i],PixelReached);	  
      }else{
      //  Int_t PixelReached =  GetPixelNumber(aDIGPlane, xpos_aftertransport ,  ypos_aftertransport );
      //     Int_t xnpos, ynpos;
      //      GetXYPixelNumber(xnpos,ynpos, aDIGPlane, PixelReached);
      //      Float_t  xdpos, ydpos;    
      //      GetXYPixelCenter(xdpos,ydpos, aDIGPlane, PixelReached);
      //      std::cout<<"charge outside DIMENSIONS "<<Xdimension <<" x "<<Ydimension <<endl;
      //      std::cout<<" after ranx="<< xrandom<<" rany="<<yrandom<<" x="<<xpos_aftertransport<<" y="<<ypos_aftertransport
      //      <<" n="<<PixelReached<<" xn= "<<xnpos 
      //      <<" yn="<<ynpos <<" xd="<<xdpos<<" yd="<<ydpos<<endl;
      
      }
      }
    */	       
    //---------------------------------------------------------------------
    // method B: more realistic model. The lorentz function allows to compute 25 probabilities (5x5 cluster)
    // the  random number is generated to see where the segment charge is deposited.
    //---------------------------------------------------------------------

    for (Int_t i=0 ; i<fNSegment ; i++){      
    
      Int_t PixelReached =  GetPixelNumber(aDIGPlane, fSegmentX[i], fSegmentY[i]);
      Float_t  xdpos, ydpos;
      GetXYPixelCenter(xdpos, ydpos, aDIGPlane, PixelReached);
      Int_t xpixn,ypixn;
      GetXYPixelNumber( xpixn,ypixn, aDIGPlane, PixelReached);

      Float_t TotalProba=0.0;
      const Int_t Npix=25;
      Float_t PixelposX[Npix];
      Float_t PixelposY[Npix];
      Float_t Pixelproba[Npix];
      Int_t Pixelnumber[Npix];
      for (Int_t j=0 ; j<Npix ; j++){
	PixelposX[j]=0.0;
	PixelposY[j]=0.0;
	Pixelproba[j]=0.0;
	Pixelnumber[j]=0;
      }
    
      for (Int_t j=0 ; j<Npix ; j++){
	Int_t xi = j%5;
	Int_t yi = j/5;     
	Float_t xeval = -(fSegmentX[i]-(xpixn+0.5)*pitchX) + (float(xi)-2.0)*pitchX ;
	Float_t yeval = -(fSegmentY[i]-(ypixn+0.5)*pitchY) + (float(yi)-2.0)*pitchY ;
	Float_t xydist = TMath::Sqrt(xeval*xeval+yeval*yeval);
	// force Lorentz function to be centered in the seed pixel for tests:
	//Float_t xeval = (float(xi)-2.0)*pitchX ;
	//Float_t yeval = (float(yi)-2.0)*pitchY ;

	if((ChargeModel==1)||(ChargeModel==2)||(ChargeModel==3)||(ChargeModel==4)){
	  Pixelproba[j]=mymodel2D->Eval(xeval,yeval);
	}else if(ChargeModel==5){
	  //find if pixel is one of the four in the first square around the hit:	  
	  if((fabs(xeval)<=pitchX)&&(fabs(yeval)<=pitchY)){
	    Pixelproba[j]=mymodel1D_1st->Eval(xydist);
	  }else{
	    Pixelproba[j]=mymodel1D_2nd->Eval(xydist);
	  }
	}
	TotalProba+=Pixelproba[j];
	PixelposX[j]= xdpos + (float(xi)-2.0)*pitchX;
	PixelposY[j]= ydpos + (float(yi)-2.0)*pitchY;
	if(  (PixelposX[j]<=0.0)||
	     (PixelposX[j]>=aDIGPlane->GetXdimension())||
	     (PixelposY[j]<=0.0)||
	     (PixelposY[j]>=aDIGPlane->GetYdimension())){
	  Pixelnumber[j] = -1;
	}else{
	  Pixelnumber[j] = GetPixelNumber(aDIGPlane, PixelposX[j], PixelposY[j]);
	}
	//std::cout<<"j ChargeModel  PIXEL PROBA / TOT / xydist " <<j<<" "<<ChargeModel<<" "<<Pixelproba[j]<<" / "<<TotalProba<<" "<<xydist<<endl;
	//		 <<mymodel1D_1st->Eval(xydist)<<"  "<< mymodel1D_2nd->Eval(xydist) <<endl;
     }
    
      //for (Int_t j=0 ; j<Npix ; j++){
      //	std::cout<<" PIXEL PROBA " <<j<<" "<<Pixelproba[j]<<" / "<<TotalProba<<endl;
      //  }

      Double_t rando = r3->Rndm()*TotalProba;
      Bool_t reached = false;
      Double_t incrprob = 0.0;
      Int_t icounter=0;
      while((!reached)&&(icounter<Npix)){
	incrprob+=Pixelproba[icounter];
	if(incrprob>rando){
	  reached=true;
	}else{
	  icounter++;
	}
      }

      if(Pixelnumber[icounter]>=0){
	UpdatePixel(fSegmentCharge[i],Pixelnumber[icounter]);
      }
    }
  delete r3;
}
//______________________________________________________________________________
//   

void DIGParticle::AddPixel(Float_t AnalogCharge, Int_t PixelNumber){
  fNpixels++;
  fPixelMap.push_back(PixelNumber);
  fAnalogChargeMap.push_back(AnalogCharge);
  fDigitalChargeMap.push_back(0);
}

//______________________________________________________________________________
//   
void DIGParticle::UpdatePixel(Float_t AnalogCharge, Int_t PixelNumber){
  Bool_t found = false;
  Int_t i=0;
  while((!found)&&(i<fNpixels)){
    if(PixelNumber==fPixelMap[i]){     
      found=true;
      fAnalogChargeMap[i]+=AnalogCharge;
    }
    i++;
  }
  if(!found){
    AddPixel(AnalogCharge, PixelNumber);
  } 

}
//______________________________________________________________________________
//   
void DIGParticle::AddRandomNoise(DIGPlane *myDIGPlane){
  Double_t Noisefix = myDIGPlane->GetNoiseElectrons();
  Double_t Noise;
  for (Int_t i = 0; i < fNpixels ; i++){
    Noise =   GaussianLaw(0.0, Noisefix);
    fAnalogChargeMap[i]+=Noise;
  }
}
//______________________________________________________________________________
//   
void DIGParticle::AnalogToDigitalconversion(DIGADC *myDIGADC, DIGPlane *myDIGPlane ){
  Float_t Noisefix = myDIGPlane->GetNoiseElectrons();
  if(Noisefix<=0.0){
    std::cout <<"<---- DIGParticle::AnalogToDigitalconversion --->"<<endl;
    std::cout<<"WARNING negative or null Noise is not physical, please correct the input file"<<endl; 
    Noisefix = 1.0;
  }
  Float_t *myADCthreshold = 0;
  myADCthreshold = myDIGADC->GetADC_thresholds();
  for (Int_t i = 0; i < fNpixels ; i++){
    if (fAnalogChargeMap[i]<=0.0){
      fDigitalChargeMap[i]=0;
    }else{
      Bool_t thresholdfound = false;
      Int_t ithres = 0;      
      fDigitalChargeMap[i]=0;
      while((thresholdfound==false)&&(ithres< (myDIGADC->GetNThresholds()) )){
	if( (fAnalogChargeMap[i]/Noisefix) < myADCthreshold[ithres] ){
	  thresholdfound = true;
	}else{
	  fDigitalChargeMap[i]++;
	  ithres++;
	}
      }
    }
  }
}
//______________________________________________________________________________
//   
Int_t DIGParticle::GetPixelNumber(DIGPlane *myDIGPlane,  Float_t Xpos, Float_t Ypos){
  Int_t XPixelnumber = int(Xpos/(myDIGPlane->GetPitchX()));
  Int_t YPixelnumber = int(Ypos/(myDIGPlane->GetPitchY()));
  Int_t PixelNumber = XPixelnumber + (myDIGPlane->GetNpixelsX())*YPixelnumber;
 
  if(
     (Xpos<0.0)||
     (Xpos>myDIGPlane->GetXdimension())||
     (Ypos<0.0)||
     (Ypos>myDIGPlane->GetYdimension())){
//    cout<<" DIGParticle::GetPixelNumber WARNING  charge is going outside the plane limits: x/y([0,"<<myDIGPlane->GetXdimension()<<"],[0,"<<myDIGPlane->GetYdimension()<<"])="<<Xpos<<"/"<<Ypos<<endl;
    return 0;
  }else{
    return PixelNumber;
  }
}
//______________________________________________________________________________
//   
void DIGParticle::GetXYPixelNumber(Int_t &Xpix, Int_t &Ypix, DIGPlane *myDIGPlane, Int_t PixelNumber){

  Xpix = PixelNumber%(myDIGPlane->GetNpixelsX());
  Ypix = PixelNumber/(myDIGPlane->GetNpixelsX());

}
//______________________________________________________________________________
//   
void DIGParticle::GetXYPixelCenter(Float_t &Xpix, Float_t &Ypix, DIGPlane *myDIGPlane, Int_t PixelNumber){

  Xpix = (myDIGPlane->GetPitchY()) * (0.5+PixelNumber%(myDIGPlane->GetNpixelsX()));
  Ypix = (myDIGPlane->GetPitchX()) * (0.5+PixelNumber/(myDIGPlane->GetNpixelsX()));

}
//______________________________________________________________________________
//  
Double_t  DIGParticle::GaussianLaw(Double_t mean, Double_t sigma) 
{ 
  Double_t x;
  TRandom3 *r3 = new TRandom3(0);
  GlobalSeed++;
  r3->SetSeed(GlobalSeed);

  x = r3->Gaus(mean,sigma);

  delete r3;
  return x;
}
//______________________________________________________________________________
//  
Float_t DIGParticle::GetTotalAnalogCharge(){
  Float_t TotalCharge = 0;
  for (Int_t i=0 ; i<fNpixels ; i++){
    TotalCharge+=fAnalogChargeMap[i];
  }
  return TotalCharge;
}
//______________________________________________________________________________
//  
Int_t DIGParticle::GetTotalDigitalCharge(){
  Int_t TotalCharge = 0;
  for (Int_t i=0 ; i<fNpixels ; i++){
    TotalCharge+=fDigitalChargeMap[i];
  }
  return TotalCharge;
}
//==============================================================================
