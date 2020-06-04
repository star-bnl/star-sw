///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//    DIGHistograms                                                                          //
//                                                                                           //
//      histograms list stored in TObjArray                                                  //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include <dighistograms.h>


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

//include other classes.h:


using namespace std;

//==============================================================================
ClassImp(DIGHistograms)
//______________________________________________________________________________
//  
DIGHistograms::DIGHistograms()  
{
  //
  // default constructor
  //
}  
//______________________________________________________________________________
//  
DIGHistograms::DIGHistograms(Int_t myNumberOfConfigurations)  
{
  fNumberOfConfigs = myNumberOfConfigurations;
}  
//______________________________________________________________________________
//  
DIGHistograms::~DIGHistograms() {  
  //
  // virtual destructor
  //
}
//______________________________________________________________________________
//  
DIGHistograms::DIGHistograms(DIGHistograms & adighisto)
{
  fNumberOfConfigs = adighisto.GetNumberOfConfigurations();

}
//______________________________________________________________________________
//  
void DIGHistograms::Clear(const Option_t *) 
{
  //  delete pointers.  fDIGParticleArray->Clear("C");

}
//______________________________________________________________________________
//  
void DIGHistograms::PrintInfo() {
  std::cout<<"---------DIGHistograms properties------------- "<<endl;
  //std::cout<<"fEntryX fEntryY fExitX fExitY fEnergy_deposited"<<endl;
  // std::cout<<fEntryX<<" "<< fEntryY<<" "<<fExitX <<" "<<fExitY <<" "<<fEnergy_deposited <<endl;
}
//______________________________________________________________________________
//  
void DIGHistograms::SetNumberOfConfigurations(Int_t myNumberOfConfigurations){
  fNumberOfConfigs=myNumberOfConfigurations;
}  
//______________________________________________________________________________
//  
void DIGHistograms::BookHistograms(Int_t myNumberOfConfigurations){

  SetNumberOfConfigurations(myNumberOfConfigurations);
  //-----------multiplicity
  Ar_h1_multiplicity_with_threshold_01 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_02 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_03 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_04 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_05 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_06 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_07 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_08 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_09 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_10 = new TObjArray(fNumberOfConfigs);

  Ar_h1_multiplicity_with_threshold_15 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_20 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_25 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_30 = new TObjArray(fNumberOfConfigs);
  Ar_h1_multiplicity_with_threshold_40 = new TObjArray(fNumberOfConfigs);




  //-----------cluster
  Ar_h1_Cluster_SeedDigitalCharge                 = new TObjArray(fNumberOfConfigs);
  Ar_h1_Cluster_TotalDigitalCharge                = new TObjArray(fNumberOfConfigs);
  Ar_h1_Cluster_9x9DigitalCharge                  = new TObjArray(fNumberOfConfigs);
  Ar_h1_Cluster_1stCrownDigitalCharge             = new TObjArray(fNumberOfConfigs);
  Ar_h1_Cluster_2ndCrownDigitalCharge             = new TObjArray(fNumberOfConfigs);
  Ar_h1_Cluster_4NeighboursDigitalCharge          = new TObjArray(fNumberOfConfigs);
  Ar_h1_Cluster_SeedOverTotalDigitalCharge        = new TObjArray(fNumberOfConfigs);
  Ar_h1_Cluster_1stCrownOverTotalDigitalCharge    = new TObjArray(fNumberOfConfigs);
  Ar_h1_Cluster_2ndCrownOverTotalDigitalCharge    = new TObjArray(fNumberOfConfigs);
  Ar_h1_Cluster_4NeighboursOverTotalDigitalCharge = new TObjArray(fNumberOfConfigs);

  Ar_h1_test = new TObjArray(fNumberOfConfigs);

  //-----------particle
  Ar_h1_Particle_TotalAnalogCharge     = new TObjArray(fNumberOfConfigs);
  Ar_h1_Particle_TotalDigitalCharge    = new TObjArray(fNumberOfConfigs);
  Ar_h1_Particle_Energy_deposited      = new TObjArray(fNumberOfConfigs);
  Ar_h2_Particle_EnergyDeposited_vs_TotalAnalogCharge     = new TObjArray(fNumberOfConfigs);
  Ar_h2_Particle_TotalDigitalCharge_vs_TotalAnalogCharge  = new TObjArray(fNumberOfConfigs);

  //-----------Resolution
  Ar_h1_Resolution_ResidualX_CoG_true = new TObjArray(fNumberOfConfigs);
  Ar_h1_Resolution_ResidualY_CoG_true = new TObjArray(fNumberOfConfigs);
  Ar_h1_Resolution_Residualdist_CoG_true = new TObjArray(fNumberOfConfigs);
  Ar_h2_Resolution_TruePosition = new TObjArray(fNumberOfConfigs);
  Ar_h2_Resolution_TruePosition_modulo = new TObjArray(fNumberOfConfigs);
  Ar_h2_Resolution_CoG = new TObjArray(fNumberOfConfigs);
  Ar_h2_Resolution_CoG_modulo = new TObjArray(fNumberOfConfigs);


  Ar_Pr_Charge_Q_over_Qtot_vs_distance = new TObjArray(fNumberOfConfigs);
  Ar_h2_Charge_Q_over_Qtot_vs_distance = new TObjArray(fNumberOfConfigs);

  Ar_h1_ADC_LSB = new TObjArray(fNumberOfConfigs);
  Ar_h1_Efficiency_ideal  = new TObjArray(fNumberOfConfigs);




  TProfile *prtemp = 0;
  TH1F *h1temp = 0;
  TH2F *h2temp = 0;
  Char_t titre[150] ;
  // sprintf(titre," Charge in %d pixels",i+1); 
  Int_t Nbin_mult = 27;

  for (Int_t i = 0; i < fNumberOfConfigs ; i++){  

    //-----------multiplicity
    sprintf(titre,"multiplicity_with_threshold_01_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_01->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_02_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_02->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_03_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_03->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_04_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_04->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_05_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_05->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_06_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_06->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_07_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_07->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_08_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_08->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_09_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_09->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_10_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_10->Add(h1temp);

    sprintf(titre,"multiplicity_with_threshold_15_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_15->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_20_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_20->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_25_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_25->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_30_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_30->Add(h1temp);
    sprintf(titre,"multiplicity_with_threshold_40_conf%d",i);
    h1temp= new TH1F(titre,titre,Nbin_mult,0,Nbin_mult);
    Ar_h1_multiplicity_with_threshold_40->Add(h1temp);



    //-----------cluster
    sprintf(titre,"Cluster_SeedDigitalCharge_conf%d",i);
    h1temp= new TH1F(titre,titre,1000,0,2000);
    Ar_h1_Cluster_SeedDigitalCharge->Add(h1temp);
    sprintf(titre,"Cluster_TotalDigitalCharge_conf%d",i);
    h1temp= new TH1F(titre,titre,1000,0,2000);
    Ar_h1_Cluster_TotalDigitalCharge->Add(h1temp);
    sprintf(titre,"Ar_h1_Cluster_9x9DigitalCharge_conf%d",i);                  
    h1temp= new TH1F(titre,titre,1000,0,2000);
    Ar_h1_Cluster_9x9DigitalCharge->Add(h1temp);
    sprintf(titre,"Ar_h1_Cluster_1stCrownDigitalCharge_conf%d",i);            
    h1temp= new TH1F(titre,titre,1000,0,2000);
    Ar_h1_Cluster_1stCrownDigitalCharge->Add(h1temp);
    sprintf(titre," Ar_h1_Cluster_2ndCrownDigitalCharge_conf%d",i);             
    h1temp= new TH1F(titre,titre,1000,0,2000);
    Ar_h1_Cluster_2ndCrownDigitalCharge->Add(h1temp);
    sprintf(titre,"Ar_h1_Cluster_4NeighboursDigitalCharge_conf%d",i);          
    h1temp= new TH1F(titre,titre,1000,0,2000);
    Ar_h1_Cluster_4NeighboursDigitalCharge->Add(h1temp);
    sprintf(titre," Ar_h1_Cluster_SeedOverTotalDigitalCharge_conf%d",i);        
    h1temp= new TH1F(titre,titre,110,0.0,1.1);
    Ar_h1_Cluster_SeedOverTotalDigitalCharge->Add(h1temp);
    sprintf(titre,"Ar_h1_Cluster_1stCrownOverTotalDigitalCharge_conf%d",i);    
    h1temp= new TH1F(titre,titre,110,0.0,1.1);
    Ar_h1_Cluster_1stCrownOverTotalDigitalCharge->Add(h1temp);
    sprintf(titre," Ar_h1_Cluster_2ndCrownOverTotalDigitalCharge_conf%d",i);    
    h1temp= new TH1F(titre,titre,110,0.0,1.1);
    Ar_h1_Cluster_2ndCrownOverTotalDigitalCharge->Add(h1temp);
    sprintf(titre,"Ar_h1_Cluster_4NeighboursOverTotalDigitalCharge_conf%d",i); 
    h1temp= new TH1F(titre,titre,110,0.0,1.1);
    Ar_h1_Cluster_4NeighboursOverTotalDigitalCharge->Add(h1temp);
    sprintf(titre,"Ar_h1_test%d",i); 
    h1temp= new TH1F(titre,titre,1000,0,2000);
    Ar_h1_test->Add(h1temp);

    sprintf(titre,"Ar_Pr_Charge_Q_over_Qtot_vs_distance%d",i);
    prtemp= new TProfile(titre,titre,100,0,200,0.0,1.1);
    Ar_Pr_Charge_Q_over_Qtot_vs_distance->Add(prtemp);
    sprintf(titre,"Ar_h2_Charge_Q_over_Qtot_vs_distance%d",i);
    h2temp= new TH2F(titre,titre,400,0,200,110,0.0,1.1);
    Ar_h2_Charge_Q_over_Qtot_vs_distance->Add(h2temp);

 
    //-----------particle
    sprintf(titre,"Particle_TotalAnalogCharge_conf%d",i);
    h1temp= new TH1F(titre,titre,600,0,6000);
    Ar_h1_Particle_TotalAnalogCharge->Add(h1temp);
    sprintf(titre,"Particle_TotalDigitalCharge_conf%d",i);
    h1temp= new TH1F(titre,titre,1000,0,2000);
    Ar_h1_Particle_TotalDigitalCharge->Add(h1temp);
    sprintf(titre,"Particle_Energy_deposited_conf%d",i);
    h1temp= new TH1F(titre,titre,600,0,6000);
    Ar_h1_Particle_Energy_deposited->Add(h1temp);

    sprintf(titre,"Particle_EnergyDeposited_vs_TotalAnalogCharge_conf%d",i);
    h2temp= new TH2F(titre,titre,150,0,3000,150,0,3000);
    Ar_h2_Particle_EnergyDeposited_vs_TotalAnalogCharge->Add(h2temp);
    sprintf(titre,"Particle_TotalDigitalCharge_vs_TotalAnalogCharge_conf%d",i);
    h2temp= new TH2F(titre,titre,300,0,3000,500,0,1000);
    Ar_h2_Particle_TotalDigitalCharge_vs_TotalAnalogCharge->Add(h2temp);

   //-----------Resolution
    sprintf(titre,"Ar_h1_Resolution_ResidualX_CoG_true_conf%d",i);
    h1temp= new TH1F(titre,titre,400,-40,40);
    Ar_h1_Resolution_ResidualX_CoG_true->Add(h1temp);
    sprintf(titre,"Ar_h1_Resolution_ResidualY_CoG_true_conf%d",i);
    h1temp= new TH1F(titre,titre,400,-40,40);
    Ar_h1_Resolution_ResidualY_CoG_true->Add(h1temp);
    sprintf(titre,"Ar_h1_Resolution_Residualdist_CoG_true_conf%d",i);
    h1temp= new TH1F(titre,titre,400,0,80);
    Ar_h1_Resolution_Residualdist_CoG_true->Add(h1temp);
    sprintf(titre,"Ar_h2_Resolution_TruePosition_conf%d",i);
    h2temp= new TH2F(titre,titre,400,0,2000,400,0,2000);
    Ar_h2_Resolution_TruePosition->Add(h2temp);
    sprintf(titre,"Ar_h2_Resolution_TruePosition_modulo_conf%d",i);
    h2temp= new TH2F(titre,titre,200,0,200,200,0,200);
    Ar_h2_Resolution_TruePosition_modulo->Add(h2temp);
    sprintf(titre,"Ar_h2_Resolution_CoG_conf%d",i);
    h2temp= new TH2F(titre,titre,400,0,2000,400,0,2000);
    Ar_h2_Resolution_CoG->Add(h2temp);
    sprintf(titre,"Ar_h2_Resolution_CoG_modulo_conf%d",i);
    h2temp= new TH2F(titre,titre,200,0,200,200,0,200);
    Ar_h2_Resolution_CoG_modulo->Add(h2temp);

   //-----------ADC
    sprintf(titre,"Ar_h1_ADC_LSB_conf%d",i);
    h1temp= new TH1F(titre,titre,200,0,20);
    Ar_h1_ADC_LSB->Add(h1temp);
    
    //----------Efficiency
    sprintf(titre,"Ar_h1_Efficiency_ideal_conf%d",i);
    h1temp= new TH1F(titre,titre,10001,0,1.0001);
    Ar_h1_Efficiency_ideal->Add(h1temp);



  }

  sprintf(titre,"NumberOfEventsPerConfiguration");
  h1_NumberOfEventsPerConfiguration= new TH1F(titre,titre,100,0,100000);
  sprintf(titre,"NumberOfConfigurations");
  h1_NumberOfConfigurations= new TH1F(titre,titre,10000,0,10000);;

    




}

//______________________________________________________________________________
//  
void DIGHistograms::PlotHistograms(Int_t myNumberOfConfigurations){
  cout<<" number of configurations "<<myNumberOfConfigurations<<endl;

}
//______________________________________________________________________________
//
TH1F* DIGHistograms::AutoZoom(TH1F* H,Option_t* aType, Int_t EntryMin/*=0*/)
{
  Int_t shift = (Int_t)(H->GetNbinsX()/50.);
  
  TString opt = aType;
  opt.ToLower();
  
  int min =0;
  int max = H->GetNbinsX();
  int New_min = min;
  int New_max = max;

  if (opt.Contains("all")) opt = TString("min,max");
  if (opt.Contains("sym")) opt = TString("min,sym");
  
  if (opt.Contains("min"))
    {
      for  (New_min=min; New_min<=max;New_min++)   
	{Stat_t c = H->GetBinContent(New_min);  if (c>EntryMin) break;}
    }
 
  else if (opt.Contains("max"))
    {
      for (New_max=max;New_max>=min;New_max--) 
	{Stat_t c = H->GetBinContent(New_max);  if (c>EntryMin) break;}
    }

  else if (opt.Contains("calib"))
    {
      Stat_t t = 0;
      for (New_max=max;New_max>=min;New_max--)
        {Stat_t c = H->GetBinContent(New_max);  if (c>EntryMin) break;}
      
      Stat_t  EntryMax=H->GetEntries()-H->GetEntries()/100;
      
      for  (New_min=min; New_min<=max;New_min++)
        {t+=H->GetBinContent(New_min); if (t>EntryMax) break;}
    }
  
  if (opt.Contains("sym"))New_max = H->FindBin(-1*H->GetXaxis()->GetBinCenter(New_min));
  
  H->GetXaxis()->SetRange(New_min - shift  , New_max + shift);  
  return H;
}


//_______________________________________________________________________________________
//
TH2F* DIGHistograms::AutoZoom(TH2F* H,Option_t* aType/*="all"*/, Int_t EntryMin/*=0*/)
{

  Int_t shiftX = (Int_t)(H->GetNbinsX()/30.);
  Int_t shiftY = (Int_t)(H->GetNbinsY()/30.); 
  
  TString opt = aType;
  opt.ToLower();
 
  int minX =0;
  int maxX = H->GetNbinsX();
  int New_minX = minX;
  int New_maxX = maxX;
  
  int minY =0;
  int maxY = H->GetNbinsY();
  int New_minY = minY;
  int New_maxY = maxY;
  
  if (opt.Contains("all")) opt = TString("minx,maxx,miny,maxy");
  
  if (opt.Contains("maxx"))
    {
      for  (New_maxX = maxX;New_maxX >=minX; New_maxX--)
	{  Stat_t c = 0;
	for  (int i_y = maxY; i_y >= minY;i_y--)
	  { c = H->GetBinContent(New_maxX,i_y);  if (c>EntryMin) break;}
	if (c>EntryMin) break;
	}
    }
  
  if (opt.Contains("maxy"))
    {
      
      for  (New_maxY = maxY;New_maxY >=minY;New_maxY--) 
	{  Stat_t c = 0;
	for  (int i_x=maxX; i_x>=minX;i_x--)   
	  { c = H->GetBinContent(i_x, New_maxY );  if (c>EntryMin) break;}
	if (c>EntryMin) break;
	}
 
    }
  
  if (opt.Contains("minx"))
    {
     
      for  (New_minX = minX;New_minX <=maxX; New_minX++)
	{  Stat_t c = 0;
	for  (int i_y = minY; i_y <= maxY;i_y++)
	  { c = H->GetBinContent(New_minX,i_y);  if (c>EntryMin) break;}
	if (c>EntryMin) break;
	}
    }
  
  

if (opt.Contains("miny"))
    {
      for  (New_minY = minY;New_minY <=maxY;New_minY++) 
	{  Stat_t c = 0;
	for  (int i_x=minX; i_x<=maxX;i_x++)   
	  { c = H->GetBinContent(i_x, New_minY );  if (c>EntryMin) break;}
	if (c>EntryMin) break;
	}      
    }
  

  H->GetXaxis()->SetRange(New_minX - shiftX  , New_maxX + shiftX);  
  H->GetYaxis()->SetRange(New_minY - shiftY  , New_maxY + shiftY);  
   
  return H;
}
//______________________________________________________________________________
//  
//______________________________________________________________________________
//  
