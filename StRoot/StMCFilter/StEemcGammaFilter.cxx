// $Id: StEemcGammaFilter.cxx,v 1.2 2010/08/09 21:52:21 seluzhen Exp $

#include <cstdlib>
#include <cmath>
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

#include "StMCFilter/StGenParticle.h"
#include "StMCFilter/StEemcGammaFilter.h"

#include "TLorentzVector.h"

// IMPORTANT IMPORTANT IMPORTANT
// Defining the static instance of user filter provides creating this
// class during the loading of library. Afterward GEANT could select 
// the needed filter by name. 

static StEemcGammaFilter eemcGammaFilter;


//////////////////////////////////////////////
//Defining the constants for endcap filtering/
//////////////////////////////////////////////
//the radius of cone in eta-phi space for clustering
const double StEemcGammaFilter::mConeRadius = 0.22;
 
//minimum energy of a gamma candidate seed track
const double StEemcGammaFilter::mSeedThreshold = 3.8; //2.4; //2.6;
  
//minimum energy of a gamma candidate cluster
const double StEemcGammaFilter::mClusterThreshold = 5.0; //3.3; //3.6;

//minimum eta of a gamma candidate
const double StEemcGammaFilter::mEtaLow = 0.95;

//maximum eta of a gamma candidate
const double StEemcGammaFilter::mEtaHigh =2.1;

//maximum vertex of gamma candidate
const double StEemcGammaFilter::mMaxVertex = 120.0;

//depth of the EEMC SMD 
const double StEemcGammaFilter:: mCalDepth = 279.5; 

//factor that hadronic energy is decreased to model
//response of EEMC to hardons
// const double StEemcGammaFilter:: mHadronScale = 0.4;
const double StEemcGammaFilter:: mHadronScale = 1.0;

//minimum energy of event tracks that are kept
//for clustering
const double StEemcGammaFilter:: mMinPartEnergy = 0.00001;


bool operator>(const TLorentzVector& v1, const TLorentzVector& v2) 
{
  return v1.E() > v2.E();
}


//////////////////////////////////////////////////
//                  Constructor                 //
//////////////////////////////////////////////////

StEemcGammaFilter::StEemcGammaFilter(): StMCFilter("eemcGammaFilter.1.00") 
{

  //some options for testing
  //sets a printout level. 0 = min, 1=medium and 2=max
  mPrintLevel = 0;
  //Fliter mode
  //0 - test mode (accept all events)
  //1 - reject events at Pythia level
//   mFilterMode = 1;
  mFilterMode = 1;

  if (!mFilterMode)
    cout<<"StEemcGammaFilter:: running the TEST mode (accepting all events). Set mFilterMode=1 to actually reject events"<< endl;

  cout<<"StEemcGammaFilter::"
	  <<" mConeRadius "<<mConeRadius
	  <<" mSeedThreshold "<< mSeedThreshold
	  <<" mClusterThreshold "<< mClusterThreshold
	  <<" mEtaLow "<<mEtaLow
	  <<" mEtaHigh "<<mEtaHigh
	  <<" mMaxVertex "<<mMaxVertex
	  <<endl;
  cout<<"StEemcGammaFilter::"
	  <<" mCalDepth " << mCalDepth
	  <<" mMinPartEnergy " <<mMinPartEnergy
	  <<" mHadronScale " <<mHadronScale
	  <<" mFilterMode " <<mFilterMode
	  <<" mPrintLevel "<<mPrintLevel
	  <<endl;
	

}


//////////////////////////////////////////////////
//    Filter events immediately after vertex    //
//////////////////////////////////////////////////

int StEemcGammaFilter::RejectGT(const StGenParticleMaster &ptl) const
{
  
  //vectors of vectors to hold tracks
  //tracks in detector coordinates
  vector<TLorentzVector> seedTracksDet;
  vector<TLorentzVector> eventTracksDet;
 
  //particle coordinates
//   vector<TLorentzVector> eventTracks;

  float zVertex = 0;
  int acceptFlag= 0;
  
  const StGenParticle* track = 0;
  // Loop over particles
  for(int i = 0; i < ptl.Size(); ++i)
    {
    
      track = ptl(i);
      if(!track) continue;

      // Skip any intermediate particles
      if(track->GetStatusCode() != 1) continue;

      int id = track->GetPdgCode();

      //get momentum 4-vector and vertex
      double p[4] = {0, 0, 0, 0};
      double v[3] = {0, 0, 0};
	
      track->Momentum(p);
      track->Vertex(v);
      zVertex = v[2];
      //can reject for vertex now
      if(fabs(v[2])>mMaxVertex)
	{
	  if(mPrintLevel>=1) cout<<"Rejecting event with extreme vertex of "<<v[2]<<endl;
	  if (mFilterMode==0 || mPrintLevel>=1)
	    if (acceptFlag==0) { cout<<"StEemcGammaFilter::RejectGT() - Reject!"<<endl; acceptFlag = -1;}
	  if (mFilterMode) return mFilterMode;
	}


      //fill a lorentz vector with particle kinematics
      TLorentzVector particleV(p[0],p[1],p[2],p[3]);;
   
      if(mPrintLevel>=2)
	{
	  cout<<"Particle vector px py pz E Et:\n";
	  cout<<particleV.Px()<<" "<<particleV.Py()<<" "<<particleV.Pz()
		   <<" "<<particleV.E()<<" "<<particleV.Et()<<endl;
	}

      //When moving to detector coordinates we want
      //the r of detector vector normalized to the smd
      //and we add on the vertex
      double scale = (mCalDepth-v[2]) / fabs(p[2]);
      for(unsigned int j = 0; j < 3; ++j) p[j] = p[j] * scale + v[j];
    
      TVector3 positionV(p[0],p[1],p[2]);
      //Let's re-normalize this to the momentum
      positionV.SetMag(particleV.P());
	
      TLorentzVector detectorV(positionV,p[3]);
     

      if(mPrintLevel>=2)
	{
	  cout<<"Detector vector px py pz E Et:\n";
	  cout<<detectorV.Px()<<" "<<detectorV.Py()<<" "<<detectorV.Pz()
		   <<" "<<detectorV.E()<<" "<<detectorV.Et()<<endl;
	}

      // To mimic the response of the calorimeters to hadrons, 
      // decrease energy deposited by some factor
      // for all particles except for
      // photons (22), neutral pions (111), eta (221), electrons (11)
      // antiprotons (-2212), and antineutrons (-2112)
      // note: charged pions are 211

      bool hadronFlag = abs(id) != 22 && abs(id) != 111 && abs(id) != 221 && abs(id) != 11;
      //we keep anti-baryons at full energy with guess that they 
      //annhilated and deposit all their energy
      hadronFlag &= id != -2212 && id != -2112;
      

      if(hadronFlag){
	particleV*=mHadronScale;
	detectorV*=mHadronScale;
	if(mPrintLevel>=2)
	  cout<<"Particle with id "<< id<<" is a hadron -  E was "<<track->Energy()<<" and is now "<<detectorV.Energy()<<endl;
      }

	
      /*
	If we had a track that's low in energy let's throw it out
      */

      if(particleV.E()<mMinPartEnergy){
	if(mPrintLevel>=1)
	  cout<<"Throwing out track with energy "<<particleV.E()
		  <<" from particle with id "<<id<<endl;
	continue;
      }


      	
      // Ignore tracks outside of the fiducial volume
      if(detectorV.Eta() < mEtaLow || detectorV.Eta() > mEtaHigh) continue;
       
      //store seed tracks with energy above threshold
      //Gamma maker uses particle tracks (corrected for vertex) to measure momentum
      //but we want detector eta and phi for clustering so save both
      if(particleV.Energy() > mSeedThreshold) 
	{
	  seedTracksDet.push_back(detectorV);
	  if(mPrintLevel>=1)
	    {
	      cout<<"Seed track stored with E "<<particleV.Energy()
		      <<"  et "<<particleV.Et()
		      <<" detector eta "<<detectorV.Eta()
		      <<" and detector phi "<<detectorV.Phi()<<endl;
	      cout<<"StStRoot/StMCFilter/StGenParticle Print :"<<endl;
	      track->Print();
	    }
	}
	
      // Store all tracks
      eventTracksDet.push_back(detectorV);
//       eventTracks.push_back(particleV);

    }

  // Search for clusters around each seed
 

  
     
  if(seedTracksDet.empty()){
    if(mPrintLevel>=1)
      cout<<"Did not find any fiducial seed tracks passing the energy threshold.\n";
    if (mFilterMode==0 || mPrintLevel>=1)
    if(mPrintLevel>=1) {if (acceptFlag==0)  cout<<"StEemcGammaFilter::RejectGT() - Reject!"<<endl;}
      acceptFlag += -10;
    if(mPrintLevel>=1)
    {
      cout << "StEemcGammaFilter:: max clusters: seed: " << acceptFlag << " " << zVertex << " " 
      << 0.0 <<" "<< 0.0<<" "<<0.0<<" "<<0.0<<" "<<0.0
      << " :Et: "
      << 0.0 <<" "<< 0.0<<" "<<0.0<<" "<<0.0<<" "<<0.0<< endl;
    }
     return mFilterMode;
  }
   
//   if(eventTracksDet.size()!=eventTracks.size())
//     cout<<"Did not save detector and particle variables for all event tracks!\n";
    

  /*
    This is a very simple way to cluster.
    I loop over the seed tracks and include tracks in
    eta and phi (detector Eta and Phi) space with R<R_max.
    Using a vector sum to add tracks, then get transverse
    energy at the end when all tracks are added.

  */
      
  //sorting the seed tracks in terms of energy makes it
  //more likely to find cluster in first few loops
  sort(seedTracksDet.begin(),seedTracksDet.end(),greater<TLorentzVector>());

  TLorentzVector maxSeedCluster(0.,0.,0.,0.);
  TLorentzVector maxEtCluster(0.,0.,0.,0.);
  float maxEtClusterValue = 0;
  float maxSeedClusterSeedEnergy = 0;
  float maxEtClusterSeedEnergy = 0;

  for(unsigned int iseed = 0; iseed<seedTracksDet.size(); iseed++)
    {
      if(mPrintLevel>=1)
	{
	  cout<<"Begin looping seed track with energy "<<seedTracksDet[iseed].Energy()
	      <<" detector eta "<<seedTracksDet[iseed].Eta()
	      <<" and detector phi "<<seedTracksDet[iseed].Phi()<<endl;
	}

      TLorentzVector clusterV(0.,0.,0.,0.);

//       if(eventTracksDet.empty())continue;
	
      for(unsigned int ievent = 0; ievent<eventTracksDet.size(); ievent++)
	{
	    
	  //use detector variables for clustering in eta-phi space
	  double dEta = seedTracksDet[iseed].Eta()-eventTracksDet[ievent].Eta();
	  double dPhi = acos( cos( seedTracksDet[iseed].Phi() - eventTracksDet[ievent].Phi()) );
	  double R = sqrt( dEta * dEta + dPhi * dPhi );
	    
	    
	  //use particle variables for momentum
	  TLorentzVector &trackV = eventTracksDet[ievent];
	 
	    
	  if(R <= mConeRadius) {
	    clusterV+=trackV;
	    if(mPrintLevel>=1)
	      {
		cout<<"Track accepted with energy "<<trackV.Energy()
			<<" et "<<trackV.Et()
			<<" dEta  "<<dEta
			<<" dPhi  "<<dPhi
			<<" R  "<<R<<endl;
		cout<<"EtSum now "<<clusterV.Et()<<endl;
	      }
	  }//within our maximum radius
	}//all event tracks looped
	if (clusterV.Et() > maxEtClusterValue ) // cluster with maximum Et
	{
	  maxEtClusterValue = clusterV.Et();
	  maxEtCluster = clusterV;
	  maxEtClusterSeedEnergy = seedTracksDet[iseed].Energy();
	}
	if (iseed==0) // cluster with maximum seed particle
	{
	  maxSeedCluster = clusterV;
	  maxSeedClusterSeedEnergy = seedTracksDet[iseed].Energy();
	}

      
      // If a cluster was found above threshold, 
      // let the event through the filter
      if(clusterV.Et() > mClusterThreshold) 
	{
	  if(mPrintLevel>=1)
	    cout<<"Cluster accepted with et "<<clusterV.Et()
		<<" eta "<<clusterV.Eta()<< " and phi "<<clusterV.Phi()<<endl;
// 	  if(mPrintLevel>=1)
	  if (mFilterMode==0 || mPrintLevel>=1)
	    if (acceptFlag==0)
	    {
	      if(mPrintLevel>=1) cout << "StEemcGammaFilter::RejectGT() - Accept!" << endl;
	      acceptFlag= 1;
	    }
	  if (mFilterMode) return 0;
	}
	
    }
    
  // If no clusters were found, abort the event
//   if(mPrintLevel>=1)
  if (mFilterMode==0 || mPrintLevel>=1)
  {if (acceptFlag==0) if(mPrintLevel>=1) cout << "StEemcGammaFilter::RejectGT() - Reject!" << endl;}
  if (acceptFlag==0) acceptFlag = -100;
    if(mPrintLevel>=1)
    {
      cout << "StEemcGammaFilter:: max clusters: seed: " << acceptFlag << " " << zVertex << " " 
      << maxSeedClusterSeedEnergy<<" "<< maxSeedCluster.E()<<" "<<maxSeedCluster.Et()<<" "<<maxSeedCluster.Eta()<<" "<<maxSeedCluster.Phi()
      << " :Et: "
      << maxEtClusterSeedEnergy<<" "<<maxEtCluster.E()<<" "<<maxEtCluster.Et()<<" "<<maxEtCluster.Eta()<<" "<<maxEtCluster.Phi()
      << endl;
    }
  
  return mFilterMode;
}

// $Log: StEemcGammaFilter.cxx,v $
// Revision 1.2  2010/08/09 21:52:21  seluzhen
// updated comment field
//

