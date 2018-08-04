///////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                           //
//     DIGEvent                                                                              //
//                                                                                           //
//        Event class                                                                        //
//        -> particle list, cluster list and digital output of the plane ( fDIGReadoutmap)   //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
//                                                                                           //
///////////////////////////////////////////////////////////////////////////////////////////////
#include <digevent.h>

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

#include "digparticle.h"
#include "digcluster.h"
#include "digreadoutmap.h"
#include "digplane.h"

using namespace std;

//==============================================================================
ClassImp(DIGEvent)
DIGEvent::DIGEvent()  
{
  //
  // default constructor
  //
  fNParticles=0;
  fDIGParticleArray=0;
  fDIGParticleArray  = new TClonesArray("DIGParticle", 10);
  fNClusters=0;
  fDIGClusterArray=0;
  fDIGClusterArray  = new TClonesArray("DIGCluster", 10);
  fDIGReadoutmap= new DIGReadoutmap();
}
//______________________________________________________________________________
//  
DIGEvent::DIGEvent(DIGEvent & adigevent)  : TObject()
{

  //----copy the particle array
  TClonesArray *particleArray2 = adigevent.GetParticle();
  Int_t npart =adigevent.GetNParticles();
  for (Int_t ipart = 0; ipart < npart ; ipart++){
    DIGParticle *papa = 0;
    papa = (DIGParticle*)particleArray2->At(ipart);
    AddParticle(*papa);
    delete papa;
  }
  //----copy the cluster array
  TClonesArray *clusterArray2 = adigevent.GetCluster();
  Int_t nclus =adigevent.GetNClusters();
  for (Int_t iclus = 0; iclus < nclus ; iclus++){
    DIGCluster *clus = 0;
    clus = (DIGCluster*)clusterArray2->At(iclus);
    AddCluster(*clus);
    delete clus;
  }


  //----copy the read out map object:
  DIGReadoutmap &adigreadout = *(adigevent.GetReadoutmap());
  fDIGReadoutmap= new DIGReadoutmap(adigreadout);


  fConfigurationNumber =adigevent.GetConfigurationNumber();
}
//______________________________________________________________________________
//  
DIGEvent::~DIGEvent() { // 
  // virtual destructor
  //
  //  delete fLayers;
  fDIGParticleArray->Clear("C");
  delete fDIGReadoutmap;
  //  fDIGReadoutmap=0;
  fNParticles=0;

  fDIGClusterArray->Clear("C");
  fNClusters=0;

}
//______________________________________________________________________________
//  
void DIGEvent::Clear(const Option_t *) 
{
  fDIGParticleArray->Clear("C");
  delete fDIGReadoutmap;
  fNParticles=0;
  fDIGClusterArray->Clear("C");
  fNClusters=0;
}

//______________________________________________________________________________
//  

void DIGEvent::PrintInfo() {
  std::cout<<"---------------------------Event properties------------- "<<endl;
  TClonesArray *particules   = GetParticle();
  DIGParticle *apart;
  std::cout<<" number of particles "<<fNParticles<<" "<<particules->GetLast()+1<<endl;
  for (Int_t i=0 ; i<(particules->GetLast()+1) ; i++){
    apart = (DIGParticle*)particules->At(i);
    apart->PrintInfo();
  }
  if( fNParticles != (particules->GetLast()+1)){
    cout<< "DIGEvent::PrintInfo WARNING PROBLEM IN PARTICLES RECORDING "<<fNParticles<<" != "<<particules->GetLast()+1<<endl;
  }

  TClonesArray *clusters   = GetCluster();
  DIGCluster *acluster;
  std::cout<<" number of clusters "<<fNClusters<<" "<<clusters->GetLast()+1<<endl;
  for (Int_t i=0 ; i<(clusters->GetLast()+1) ; i++){
    acluster = (DIGCluster*)clusters->At(i);
    acluster->PrintInfo();
  }
  if( fNClusters != (clusters->GetLast()+1)){
    cout<< "DIGEvent::PrintInfo WARNING PROBLEM IN CLUSTERS RECORDING "<<fNClusters<<" != "<<clusters->GetLast()+1<<endl;
  }

  GetReadoutmap()->PrintInfo();
}
//______________________________________________________________________________
//   
void  DIGEvent::SetNParticles(Int_t Nparticles){
  fNParticles = Nparticles;
}
//______________________________________________________________________________
//   
void DIGEvent::SetNClusters(Int_t NClusters){
  fNClusters=NClusters;
}
//______________________________________________________________________________
//   
void DIGEvent::SetConfigurationNumber(Int_t ConfigurationNumber){
  fConfigurationNumber = ConfigurationNumber;
}
//______________________________________________________________________________
//   
void DIGEvent::AddParticle(DIGParticle& particle)
{
  // fNParticles++;
   TClonesArray &particleArray = *fDIGParticleArray;
   new(particleArray[fNParticles++]) DIGParticle(particle); // utilise le copieur



}
//______________________________________________________________________________
//   
void DIGEvent::AddCluster(DIGCluster& cluster)
{
   TClonesArray &clusterArray = *fDIGClusterArray;
   new(clusterArray[fNClusters++]) DIGCluster(cluster); // utilise le copieur
}
//______________________________________________________________________________
//   
void DIGEvent::BuildTrueClusters(DIGPlane *myDIGPlane){
  // this method builds "monte carlo truth" clusters.
  // In others words, it takes the generated charge from the particles to build the clusters.
  // Its purpose is to compare other algorithms with ideal clustering reconstruction.

  TClonesArray *particules   = GetParticle();
  if( GetNParticles() != (particules->GetLast()+1)){
    cout<< "DIGEvent::BuildTrueClusters   WARNING PROBLEM IN PARTICLES RECORDING"<<endl;
  }



  DIGParticle *myparticle = 0;
  TClonesArray *particleArray = GetParticle();    
  DIGCluster *mycluster=0;
  for (Int_t i = 0; i < fNParticles ; i++){
    myparticle = (DIGParticle*)particleArray->At(i);
    //myparticle->PrintInfo();
    mycluster =  new DIGCluster();
    for (Int_t npix = 0; npix <  myparticle->GetNpixels() ; npix++){      
      if(myparticle->GetDigitalCharge()[npix]>0){
	mycluster->AddPixel(myparticle->GetDigitalCharge()[npix],myparticle->GetPixelMap()[npix]);
      }
    }
    mycluster->Compute_CoG(myDIGPlane);
    mycluster->Compute_SeedPixel(myDIGPlane);

    if(mycluster->GetNpixels()>0){
      //mycluster->PrintInfo();
    AddCluster(*mycluster); 
    }
  }

  TClonesArray *aclusterArray = GetCluster();    
  if(GetNClusters() != aclusterArray->GetLast()+1){
    cout<<" TEST cluster number "<<GetNClusters()<<" "<<aclusterArray->GetLast()+1<<endl;
  }
  delete mycluster;
}
//______________________________________________________________________________
//   

