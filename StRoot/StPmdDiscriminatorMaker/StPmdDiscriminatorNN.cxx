/****************************************************
 *
 * $Id: StPmdDiscriminatorNN.cxx,v 1.4 2007/04/26 04:13:46 perev Exp $
 *
 * Author: Subhasis Chattopadhyay
 *
 ******************************************************
 *
 * Description: Class for discrimination through energy
 * cut is defined.
 *
 ******************************************************
 *
 * $Log: StPmdDiscriminatorNN.cxx,v $
 * Revision 1.4  2007/04/26 04:13:46  perev
 * Remove StBFChain dependency
 *
 * Revision 1.3  2003/09/02 17:58:48  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2003/08/04 18:53:44  perev
 * warnOff
 *
 * Revision 1.1  2003/05/29 13:21:05  subhasis
 * NN discriminator
 *
 *
 ******************************************************/

#include<Stiostream.h>
#include"Stiostream.h"
#include<assert.h>
#include<math.h>
#include"TROOT.h"
#include<TRandom.h>
#include<TBrowser.h>
#include<TPad.h>
#include<StMessMgr.h>
#include<TFile.h>

#include "StPmdUtil/StPmdGeom.h"
#include "StPmdUtil/StPmdDetector.h"
#include "StPmdDiscriminatorMaker.h"
#include "StPmdDiscriminatorNN.h"
#include "StPmdUtil/StPmdClusterCollection.h"
#include "StPmdUtil/StPmdCluster.h"
#include "StPmdNeuNet.h" 
#include "StEventTypes.h" 
#include "StNNCluster.h" 

ClassImp(StPmdDiscriminatorNN)
  ofstream fileo("test_NN",ios::app);   

Int_t npmdvalue=0;
Int_t ncpvvalue=0;
Int_t Trained=0;
Int_t NTrain=0;

StPmdDiscriminatorNN::StPmdDiscriminatorNN(StPmdCl cl_con)
{
  mClContainer=cl_con;
  mApplyFlagNN=0;
  cout<<"inside discNN, size**"<<cl_con.size()<<" "<<mClContainer.size()<<endl;
}


StPmdDiscriminatorNN::StPmdDiscriminatorNN(StPmdDetector* pmd_det, StPmdDetector* cpv_det)
{
  m_PmdDet=pmd_det;
  m_CpvDet=cpv_det;
}

StPmdDiscriminatorNN::~StPmdDiscriminatorNN()
{
  //destructor
}

void StPmdDiscriminatorNN::Discriminate()
{
  StPmdNeuNet *sneu=new StPmdNeuNet("for PMD",4,"4",1);
  sneu->setDiscMaker(m_DiscMaker);
  sneu->SetLearnParam(0.2); // the learning parameter (<1)
  sneu->SetInitParam(-2,2); // bounds for the initialisation of weights
  sneu ->SetUseBiases();
  sneu->Init(); // initialisation of the kernel
  sneu->PrintS(); // printing of network structure
  Int_t NNSize=mClContainer.size();
  sneu->SetNTrainEvents(NNSize);
  sneu->SetArraySize(NNSize);
  cout<<"nTrainevts  "<<sneu->GetNTrainEvents()<<endl;
  Input(sneu);
  cout<<"No of INputs **"<<NTrain<<"NNFlag  "<<mApplyFlagNN<<endl;
  
  if(mApplyFlagNN!=1){
    if(Trained!=1){
      sneu->TrainNCycles(100);
      Trained=1;
      sneu->Export("NNTrain.out"); // printing of network structure
    }
  }
  
  cout<<" NNTrain to be imported "<<endl;
  sneu->Import("NNTrain.out"); // printing of network structure
  if(mApplyFlagNN==1)
    {
      Float_t Teach[20000];
      Float_t Value[20000];
      for(Int_t i=0;i<20000;i++){Teach[i]=999; Value[i]=0;}
      
      cout<<"NN Valid() Called "<<endl;
      sneu->ApplyWeights(Teach,Value);
      for(Int_t i=0;i<20000;i++){
	if(Teach[i]!=999)fileo<<Teach[i]<<" "<<Value[i]<<endl;
      }
    }
}

void StPmdDiscriminatorNN::Input(StPmdNeuNet* sneu)
{
  TFile * file=new TFile("nninput.root","RECREATE");
  m_NNedep_ph=new TH1F("nnedp_ph","(ph) PMD edep",100,0.,1000.);
  m_NNncell_ph=new TH1F("nn_ncell_ph","(ph) PMD ncell",100,0.,20.);
  m_NNsigma_ph=new TH1F("nn_sigma_ph","(ph) PMD sigma",100,0.,20.);
  m_NNedep_cpv_ph=new TH1F("nnedp_cpv_ph","(ph) CPV edep",100,0.,100.);
  m_NNedep_had=new TH1F("nnedp_had","(had) PMD edep",100,0.,1000.);
  m_NNncell_had=new TH1F("nn_ncell_had","(had) PMD ncell",100,0.,20.);
  m_NNsigma_had=new TH1F("nn_sigma_had","(had) PMD sigma",100,0.,20.);
  m_NNedep_cpv_had=new TH1F("nnedp_cpv_had","(had) CPV edep",100,0.,100.);
  
  
  Int_t totno = 0,totcpvno = 0;
  Float_t aveEnergy = 0., aveNcell = 0., aveSigma = 0.,aveCpvEnergy = 0;
  Float_t totEnergy = 0., totNcell = 0., totSigma = 0.,totCpvEnergy = 0;
  //  cout<<"CONTAINER SIZE ********* "<<mClContainer.size()<<endl;
  for(UInt_t i=0;i<mClContainer.size();i++)
    {
      StPhmdCluster *cl1=(StPhmdCluster*)(mClContainer[i]->PmdCluster());
      StPhmdCluster *cl2=(StPhmdCluster*)(mClContainer[i]->CpvCluster());
      ///////////////////////////////////////////////
      
      totEnergy = totEnergy + cl1->energy();
      totNcell = totNcell +  cl1->numberOfCells();
      totSigma = totSigma + cl1->sigma();
      totno++;
      if(cl2){
	totCpvEnergy = totCpvEnergy + cl2->energy();
	totcpvno++;
      }
    }
  aveEnergy = totEnergy/totno;
  aveNcell =  totNcell/totno;
  aveSigma = totSigma/totno;
  aveCpvEnergy = totCpvEnergy/totcpvno;
  
  if(mApplyFlagNN==1){
    // Put avergaes by hand
    
    /*Single particle
      aveEnergy =0.000253643 ;
      aveNcell =2.54512;
      aveSigma =0.467905;
      aveCpvEnergy =1.8775e-05;
    */
    // AuAu
    aveEnergy =0.000157085 ;
    aveNcell =2.20981;
    aveSigma =0.476074;
    aveCpvEnergy =1.26994e-05;
    
  }
  
  fileo<<" AveEnergy "<<aveEnergy<<" AveNcell "<<aveNcell<<" aveSigma "<<aveSigma<<" aveCpvEne "<<aveCpvEnergy<<" TotNo "<<totno<<" TotCPvNo "<<totcpvno<<endl; 
  
  Float_t outEnergy,outNcell,outSigma,outCpvEnergy;
  
  for(UInt_t i=0;i<mClContainer.size();i++)
    {
      StPhmdCluster *cl1=(StPhmdCluster*)(mClContainer[i]->PmdCluster());
      StPhmdCluster *cl2=(StPhmdCluster*)(mClContainer[i]->CpvCluster());
      Float_t target;
      if(cl1->mcPid()==1)target=1.;
      if(cl1->mcPid()==8)target=0.;
      sneu->fillArrayOut(target,i,0);
      
//VP      Int_t sm=cl1->module();
      Float_t energy=cl1->energy();
      InputRange(energy,aveEnergy,outEnergy);
      sneu->FillArray(i,0,outEnergy);
      
      Int_t ncell=cl1->numberOfCells();
      InputRange(ncell,aveNcell,outNcell);

      sneu->FillArray(i,1,outNcell);
      
      Float_t sigma=cl1->sigma();
      InputRange(sigma,aveSigma,outSigma);

      sneu->FillArray(i,2,outSigma);
      
      Float_t cpv_energy=0.;
      if(cl2){
	cpv_energy=cl2->energy();
	InputRange(cpv_energy,aveCpvEnergy,outCpvEnergy);

	sneu->FillArray(i,3,outCpvEnergy);
      }

      if(target==1)m_NNncell_ph->Fill(Float_t(ncell));
      if(target==0)m_NNncell_had->Fill(Float_t(ncell));
      if(target==1)m_NNsigma_ph->Fill(sigma);
      if(target==0)m_NNsigma_had->Fill(sigma);
      if(target==1&&cpv_energy>0)m_NNedep_cpv_ph->Fill(cpv_energy);
      if(target==0&&cpv_energy>0)m_NNedep_cpv_had->Fill(cpv_energy);
      if(target==1)m_NNedep_ph->Fill(energy);
      if(target==0)m_NNedep_had->Fill(energy);
      NTrain++;
    }
  cout<<"In Input **"<<NTrain<<endl;
  m_NNedep_ph->Write();
  m_NNncell_ph->Write();
  m_NNsigma_ph->Write();
  m_NNedep_cpv_ph->Write();
  
  m_NNedep_had->Write();
  m_NNncell_had->Write();
  m_NNsigma_had->Write();
  m_NNedep_cpv_had->Write();
  file->Close();
}

void StPmdDiscriminatorNN::setFormula()
{
  cout<<"In the Neural Network"<<npmdvalue<<ncpvvalue<<endl;	
}

//function used for scaling the input variables to (-1,1)

Float_t StPmdDiscriminatorNN::InputRange(Float_t Input,Float_t aveInput, Float_t& Output)
{
  Float_t fx;
  Float_t ax = 1.;
  if(aveInput !=0.){
    if((Input/aveInput)<10.){
      fx = (2./(1. + exp(-ax*Input/aveInput))) - 1;
    }
    else
      {
	fx = (2./(1. + exp(-10.))) - 1.;
      }
    Output = 1 - 2. * fx;
  }
  return Output;
}
	
      
