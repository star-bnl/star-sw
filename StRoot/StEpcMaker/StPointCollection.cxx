
//
// $id$
//
// $Log: StPointCollection.cxx,v $
// Revision 1.2  2000/07/03 02:07:45  perev
// StEvent: vector<TObject*>
//
// Revision 1.1  2000/05/15 21:18:33  subhasis
// initial version
//
// Pi0 Candidate Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay , Jan 2000
//
//////////////////////////////////////////////////////////////////////////
//                                   
// StPointCollection
//

//
//////////////////////////////////////////////////////////////////////////
#include "StPointCollection.h"
#include "StPi0Candidate.h"
#include "emc_def.h"
#include "StChain.h"
#include "StThreeVector.hh"
#include "StHelix.hh"
#include "St_emc_Maker/StEmcGeom.h"


// copied from StEpcMaker
#include <iostream.h>
#include <math.h>
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StEpcMaker.h"
#include "StThreeVector.hh"
#include "StHelix.hh"
#include "SystemOfUnits.h"
#include "St_emc_Maker/StEmcGeom.h"
#include "St_emc_Maker/StEmcHitCollection.h"
#include <stdlib.h>
#include <string.h>
#include <vector>
#include "StThreeVectorD.hh"
#include "StHelixD.hh"
#include "StPhysicalHelixD.hh"
#include "TMath.h"
#include "StDetectorDefinitions.h"
#include "Stypes.h"
#include "math_constants.h"

//For StEvent
#include "St_ObjectSet.h"
#include "StEmcCollection.h"
#include "StEmcDetector.h"
#include "StEmcModule.h"
#include "StEmcRawHit.h"
#include "StEmcClusterCollection.h"
#include "StEmcCluster.h"
#include "StEmcPoint.h"
#include "StEnumerations.h"  

#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)

// declaring cernlib routine (mathlib, H301) assndx to be used for matching.
#define    assndx  F77_NAME(assndx,ASSNDX)
extern "C" {void type_of_call assndx ( Int_t &, Float_t *, Int_t &, Int_t &,
 Int_t &,Int_t *, Float_t &,Int_t *,Int_t &); }


ClassImp(StPointCollection)


  St_tpt_track *tpctrack;
  tpt_track_st *trackStaf;
  StEmcGeom *BemcGeomIn;
  StEmcGeom *BemcGeomOut;

const TString detname[] = {"Bemc", "Bsmde", "Bsmdp"};
// Extern for sorted emc-smd
  StMatchVec matchlist_bemc[Epc::nModule][Epc::nPhiBin];
  StMatchVec matchlist_bsmde[Epc::nModule][Epc::nPhiBin];
  StMatchVec matchlist_bsmdp[Epc::nModule][Epc::nPhiBin];
  FloatVector HitTrackEta;
  FloatVector HitTrackPhi;
  FloatVector HitTrackMom;

//_____________________________________________________________________________
StPointCollection::StPointCollection():St_DataSet("Default")
{
  SetTitle("EmcPoints");
}
//_____________________________________________________________________________
StPointCollection::StPointCollection(const Char_t *Name):St_DataSet(Name)
{
  SetTitle("EmcPoints");
}
//_____________________________________________________________________________
StPointCollection::~StPointCollection(){
/*
*/
}
//_____________________________________________________________________________
Int_t 
  StPointCollection::findPoints(StBemcPreClusterCollection* hit0,
                                StBsmdePreClusterCollection *hit1,
                                StBsmdpPreClusterCollection *hit2,
                                StEmcHitCollection *hits,
                                StEmcHitCollection *hit_e,
                                StEmcHitCollection *hit_p,
                                St_dst_track *globtrk)


{
//Sort BEMC, SMDe, SMDp, PRS clusters according to location 

  findCandidates(hit0,
                 hit1,
                 hit2,
                 hits,
                 hit_e,
                 hit_p);

// Getting BemcGeom to obtain radius etc

   BemcGeomIn  = new StEmcGeom("bemc");
   BemcGeomOut = new StEmcGeom("bemc");                                        

// Sorting global tracks
  int iRes = 0;
  long NGlbTrk = globtrk->GetNRows();
 
    if(NGlbTrk!=0)iRes = GlobSort(globtrk);

    //track check array for checking if the track is matched
     Int_t *Trcheck = new Int_t[HitTrackEta.size()];
  for(UInt_t i1=0;i1<HitTrackEta.size();i1++){
      Trcheck[i1]=0;
   }

// MATCHING****************

           for(Int_t im=0;im<Epc::nModule;im++){
            for(Int_t is=0;is<Epc::nPhiBin;is++){

  if(matchlist_bemc[im][is].size()>0){
  Int_t testp=GetEmcPoint(matchlist_bemc[im][is],
                         matchlist_bsmde[im][is],
                         matchlist_bsmdp[im][is],
                         HitTrackEta,
                         HitTrackPhi,
                         HitTrackMom,
                         Trcheck);
  if(testp!=0){
      cout<<" GetEmcPoint not successful for "<<im<<" "<<is<<endl;
      return kStWarn;
     }
  }
	    }
	   }
delete [] Trcheck;

return kStOK;
}
//_____________________________________________________________________________
Int_t StPointCollection::addPoints(Float_t *hid)
{
StPi0Candidate *pnts = new StPi0Candidate(hid);
mPoints.Add(pnts);
mNPoints++;
return kStOK;
}
//_____________________________________________________________________________
void 
 StPointCollection::findCandidates(StBemcPreClusterCollection* hit0,
                                   StBsmdePreClusterCollection *hit1,
                                   StBsmdpPreClusterCollection *hit2,
                                   StEmcHitCollection *hits,
                                   StEmcHitCollection *hit_e,
                                   StEmcHitCollection *hit_p)
{
 
   for(Int_t i1=0;i1<Epc::nModule;i1++){
     for(Int_t i2=0;i2<Epc::nPhiBin;i2++){
	 matchlist_bemc[i1][i2].clear();
	 matchlist_bsmde[i1][i2].clear();
	 matchlist_bsmdp[i1][i2].clear();
     }
   }

       Int_t id,check,m,e,s;
//  cast clus1 over bemcclustercollection
   if(hit0>0){
    StBemcPreClusterCollection *clus1 = (StBemcPreClusterCollection*)hit0;
// create an iterator
             TIter next(clus1->Clusters());
//loop for emc clusters
    if(clus1->Nclusters()>0){
    for(Int_t i1=0; i1<clus1->Nclusters(); i1++){
	// check the eta,phi and sigma values of that cluster
             StEmcPreCluster *cl1;
             cl1=(StBsmdePreCluster*)next();
	     Float_t eta_emc=cl1->Eta(); 
	     //Get the module number
//
       Int_t emc_module;
       Int_t emc_hits_id = cl1->ID(0);
       id  = hits->HitId(emc_hits_id);
       check = hits->getBin(id,m,e,s);
       if(check == 0){
          emc_module=m;
       Int_t emc_phi_bin=Int_t(TMath::Abs(eta_emc*10));
       //keeping the cluster very close to phibin boundry to the previous bin
       //
       if(emc_phi_bin>0){
	 if((TMath::Abs(eta_emc*10)-Float_t(emc_phi_bin))<=0.01){
	   emc_phi_bin--;
	 }
       }
       if(emc_phi_bin>9){emc_phi_bin=9;}

//copy cl1 pointer to StMatchvec
	matchlist_bemc[emc_module-1][emc_phi_bin].push_back(cl1);

       }
       else{
	 printf("**** Bad id %i m %i",id,emc_module);
       }
	} // END of EMC cluster loop
       }
     }

//    cast clus2 on bsmdecollection
//
   if(hit1>0){
    StBsmdePreClusterCollection *clus2 = (StBsmdePreClusterCollection*)hit1;
             TIter next2(clus2->Clusters());
// loop over bsmde
//
// Get pointer for Bsmde hit collection
//
    if(clus2->Nclusters()>0){
      for(Int_t i2=0; i2<clus2->Nclusters(); i2++){

	// check the phi value of that cluster
             StEmcPreCluster *cl2;
             cl2=(StBsmdePreCluster*)next2();
	     Float_t eta_eta=cl2->Eta(); 

       Int_t id_e,check_e,m_e,e_e,s_e;
       Int_t smde_module;
       Int_t smde_hits_id = cl2->ID(0);
       id_e  = hit_e->HitId(smde_hits_id);
       check_e = hit_e->getBin(id_e,m_e,e_e,s_e);
       if(check_e == 0){
          smde_module=m_e;
       Int_t smde_phi_bin=Int_t(TMath::Abs(eta_eta*10));
       //       keeping the cluster very close to phibin boundry to the previous bin
       
       if(smde_phi_bin>0){
       	 if((TMath::Abs(eta_eta*10)-Float_t(smde_phi_bin))<=0.01){
       	   smde_phi_bin--;
       	 }
       }
       if(smde_phi_bin>9){smde_phi_bin=9;}

//record the phi-bin sorted pointers

	matchlist_bsmde[smde_module-1][smde_phi_bin].push_back(cl2);

       }
       else{
	 printf("**** Bad id %i m %i",id,smde_module);
       }
	} // End of SMDE cluster loop
        }
     }
// Cast clus3 over bsmdp
//
    if(hit2>0){         
    StBsmdpPreClusterCollection *clus3 = (StBsmdpPreClusterCollection*)hit2;
             TIter next3(clus3->Clusters());
//
//loop over smdp clusters
//
// Get pointer for Bsmde hit collection
//
    if(clus3->Nclusters()>0){
        for(Int_t i3=0; i3<clus3->Nclusters(); i3++){
	     StEmcPreCluster *cl3;
	     cl3=(StBsmdpPreCluster*)next3();
	     Float_t eta_phi=cl3->Eta();

       Int_t id_p,check_p,m_p,e_p,s_p;
       Int_t smdp_module;
       Int_t smdp_hits_id = cl3->ID(0);
       id_p  = hit_p->HitId(smdp_hits_id);
       check_p = hit_p->getBin(id_p,m_p,e_p,s_p);
       if(check == 0){
          smdp_module=m_p;
       Int_t smdp_phi_bin;
	smdp_phi_bin=Int_t(TMath::Abs(eta_phi*10));
       if(smdp_phi_bin>9){smdp_phi_bin=9;}
//record the phi-bin sorted pointers
	matchlist_bsmdp[smdp_module-1][smdp_phi_bin].push_back(cl3);

       }
       else{
	 printf("**** Bad id %i m %i",id,smdp_module);
       }
	} // End of SMDP cluster loop
       }
     }
}
//--------------------------------------------------------------------------
Int_t
  StPointCollection::GetEmcPoint(const StMatchVec mvec,
                                 const StMatchVec evec,
                                 const StMatchVec pvec,
                                 const FloatVector E_tvec,
                                 const FloatVector P_tvec,
                                 const FloatVector M_tvec,
                                 Int_t *Trcheck)

{
     Int_t mode=1;
     Int_t na=0,ma=0,ida=Epc::nMaxNoOfClinBin;
     Float_t ep[Epc::nMaxNoOfClinBin][Epc::nMaxNoOfClinBin]; 
     Int_t k[Epc::nMaxNoOfClinBin];
     Float_t smin;
     Int_t iw[Epc::nMaxNoOfClinBin][Epc::nMaxNoOfClinBin];
     Int_t idw =Epc::nMaxNoOfClinBin;
     Int_t k_track[Epc::nMaxNoOfClinBin][Epc::nMaxNoOfClinBin];
     Float_t totAvg=0.;
     Float_t EmcTot;

        na=evec.size();
        ma=pvec.size();
	UInt_t it;
  Float_t PointMember[9];
  {
     for (Int_t iF=0;iF<9;iF++){PointMember[iF]=0.0;}
  }
// will be taken as track pointer later
	Float_t TrackMom[Epc::nMaxNoOfClinBin][Epc::nMaxNoOfClinBin];
	Float_t DeltaEta[Epc::nMaxNoOfClinBin][Epc::nMaxNoOfClinBin];
	Float_t DeltaPhi[Epc::nMaxNoOfClinBin][Epc::nMaxNoOfClinBin];

     for (Int_t iF=0;iF<Epc::nMaxNoOfClinBin;iF++){
       k[iF]=0;
        for (Int_t iL=0;iL<Epc::nMaxNoOfClinBin;iL++){
         TrackMom[iF][iL]=0.0;
         DeltaEta[iF][iL]=0.0;
         DeltaPhi[iF][iL]=0.0;
         ep[iF][iL]=0.0;
         iw[iF][iL]=0;
         k_track[iF][iL]=0;
        }
     }
     //
     
     Int_t Category;

     if(evec.size()==0 && pvec.size()==0){
       Category=0;
       na=mvec.size();
       ma=mvec.size();
     }
     if(evec.size()>0 && pvec.size()==0){
       Category=1;
       na=evec.size();
       ma=mvec.size();
     }
     if(evec.size()==0 && pvec.size()>0){
       Category=2;
       na=mvec.size();
       ma=pvec.size();
     }
     if(evec.size()>0 && pvec.size()>0){
       Category=3;
       na=evec.size();
       ma=pvec.size();
     }
     //
     EmcTot=0.0;
  if(mvec.size()>0){
     for (UInt_t ims=0;ims<mvec.size();ims++){
     StEmcPreCluster *cl0;
     cl0=(StEmcPreCluster*)mvec[ims];
     Float_t emen=cl0->Energy();
        EmcTot+=emen;
     }
   }
  //
  for(Int_t ie=0;ie<na;ie++){
    StEmcPreCluster *cl1;
    for(Int_t ip=0;ip<ma;ip++){
      StEmcPreCluster *cl2;
      switch (Category) {
      case 0:
	cl1 = (StEmcPreCluster*)mvec[ie];
	cl2 = (StEmcPreCluster*)mvec[ip];
	break;
      case 1:
	cl1 = (StEmcPreCluster*)evec[ie];
	cl2 = (StEmcPreCluster*)mvec[ip];
	break;
      case 2:
	cl1 = (StEmcPreCluster*)mvec[ie];
	cl2 = (StEmcPreCluster*)pvec[ip];
	break;
      case 3:
	cl1 = (StEmcPreCluster*)evec[ie];
	cl2 = (StEmcPreCluster*)pvec[ip];
	break;
      }
      
// track matching
     Float_t PhitoMatch=cl2->Phi();
     Float_t EtatoMatch=cl1->Eta();
     Int_t MatchFlag=0;
     if(E_tvec.size()>0){
     for (it=0;it<E_tvec.size();it++){
       if(MatchFlag!=1){
       if(Trcheck[it]!=1){
       Float_t EtaTrack=E_tvec[it];
       Float_t PhiTrack=P_tvec[it];
       if(TMath::Abs(EtatoMatch-EtaTrack)<=StEpcCut::DeltaEta() && TMath::Abs(PhitoMatch-PhiTrack)<=StEpcCut::DeltaPhi()){
	 Trcheck[it]=1;
	 TrackMom[ie][ip]=M_tvec[it];
	 DeltaEta[ie][ip]=EtatoMatch-EtaTrack;
	 DeltaPhi[ie][ip]=PhitoMatch-PhiTrack;
	 k_track[ie][ip]=it+1;
	 MatchFlag=1;
        }
       }
       }
     }//it loop
     }// etrack size check

          Float_t diff=TMath::Abs((cl1->Energy())-(cl2->Energy()));
          Float_t summ= (cl1->Energy())+(cl2->Energy());
    	  ep[ip][ie]=diff/summ;

    }
  }

 assndx(mode,&ep[0][0],na,ma,ida,k,smin,&iw[0][0],idw);

     switch (Category) {
     case 0:
      {for(Int_t i1=0;i1<na;i1++){
       if((k[i1]-1)>=0){
	 StEmcPreCluster *cl1;
	 cl1 = (StEmcPreCluster*)mvec[i1];
	 Float_t avg_en = cl1->Energy();
	 totAvg += avg_en;
       }
      }}
       break;
     case 1:
      {for(Int_t i1=0;i1<na;i1++){
       if((k[i1]-1)>=0){
	 StEmcPreCluster *cl1;
	 cl1 = (StEmcPreCluster*)evec[i1];
	 Float_t avg_en = cl1->Energy();
	 totAvg += avg_en;
       }
      }}
       break;
     case 2:
      {for(Int_t i1=0;i1<na;i1++){
       if((k[i1]-1)>=0){
	 StEmcPreCluster *cl1;
	 cl1 = (StEmcPreCluster*)mvec[i1];
	 Float_t avg_en = cl1->Energy();
	 totAvg += avg_en;
       }
      }}
       break;
     case 3:
      {for(Int_t i1=0;i1<na;i1++){
       if((k[i1]-1)>=0){
	 StEmcPreCluster *cl1;
	 cl1 = (StEmcPreCluster*)evec[i1];
	 StEmcPreCluster *cl2;
	 cl2 = (StEmcPreCluster*)pvec[k[i1]-1];
	 Float_t avg_en = (cl1->Energy()+cl2->Energy())/2.;
	 totAvg += avg_en;
       }
       }}
       break;
 }


 for(Int_t i1=0;i1<na;i1++){

  Float_t PointEnergy=0.;
  Float_t PointEta=0.;
  Float_t PointSigEta=0.;
  Float_t PointPhi=0.;
  Float_t PointSigPhi=0.;
  Float_t PointEnergyinDet[4]={0.,0.,0.,0.};
  Float_t PointSizeinDet[4]={0.,0.,0.,0.};
 
     switch (Category){
     case 0:
       if((k[i1]-1)>=0){
	 StEmcPreCluster *cl1;
	 cl1 = (StEmcPreCluster*)mvec[i1];
	 PointEta=cl1->Eta();
	 PointSigEta=cl1->SigmaEta();
	 PointPhi=cl1->Phi();
	 PointSigPhi=cl1->SigmaPhi();
	 PointEnergy=cl1->Energy();
	 PointEnergyinDet[0]=cl1->Energy();
	 PointSizeinDet[0]=cl1->SigmaEta();
	 }
       break;
     case 1:
       if((k[i1]-1)>=0){
	 StEmcPreCluster *cl1;
	 cl1 = (StEmcPreCluster*)evec[i1];
	 StEmcPreCluster *cl2;
	 cl2 = (StEmcPreCluster*)mvec[k[i1]-1];
	 PointEta=cl1->Eta();
	 PointSigEta=cl1->SigmaEta();
	 PointPhi=cl2->Phi();
	 PointSigPhi=cl2->SigmaPhi();
	 PointEnergy=cl1->Energy()*(EmcTot/totAvg);
	 PointEnergyinDet[0]=cl2->Energy();
	 PointEnergyinDet[2]=cl1->Energy();
	 PointSizeinDet[0]=cl2->SigmaEta();
	 PointSizeinDet[2]=cl1->SigmaEta();
	 }
       break;
     case 2:
       if((k[i1]-1)>=0){
	 StEmcPreCluster *cl1;
	 cl1 = (StEmcPreCluster*)mvec[i1];
	 StEmcPreCluster *cl2;
	 cl2 = (StEmcPreCluster*)pvec[k[i1]-1];
	 PointEta=cl1->Eta();
	 PointSigEta=cl1->SigmaEta();
	 PointPhi=cl2->Phi();
	 PointSigPhi=cl2->SigmaPhi();
	 PointEnergy=cl1->Energy()*(EmcTot/totAvg);
	 PointEnergyinDet[0]=cl1->Energy();
	 PointEnergyinDet[3]=cl2->Energy();
	 PointSizeinDet[0]=cl1->SigmaEta();
	 PointSizeinDet[3]=cl2->SigmaPhi();
	 }
       break;
     case 3:
       if((k[i1]-1)>=0){
	 StEmcPreCluster *cl1;
	 cl1 = (StEmcPreCluster*)evec[i1];
	 StEmcPreCluster *cl2;
	 cl2 = (StEmcPreCluster*)pvec[k[i1]-1];
	 PointEta=cl1->Eta();
	 PointSigEta=cl1->SigmaEta();
	 PointPhi=cl2->Phi();
	 PointSigPhi=cl2->SigmaPhi();
	 PointEnergy=((cl1->Energy()+cl2->Energy())/2.)*(EmcTot/totAvg);
	 PointEnergyinDet[0]=EmcTot;
	 PointEnergyinDet[2]=cl1->Energy();
	 PointEnergyinDet[3]=cl2->Energy();
	 PointSizeinDet[2]=cl1->SigmaEta();
	 PointSizeinDet[3]=cl2->SigmaPhi();
	 }
       break;
     }
     if((k[i1]-1)>=0){
	PointMember[0]=PointEta;
	PointMember[1]=PointPhi;
	PointMember[2]=PointSigEta;
	PointMember[3]=PointSigPhi;
	PointMember[4]=PointEnergy;
	if(k_track[i1][k[i1]-1]>0){
	PointMember[5]=TrackMom[i1][k[i1]-1];
	PointMember[6]=DeltaEta[i1][k[i1]-1];
	PointMember[7]=DeltaPhi[i1][k[i1]-1];
	PointMember[8]=(Float_t)Category;
	}
	Int_t testadd = addPoints(PointMember);
	if(testadd==1)cout<<" addPoints not O.K"<<endl;
	// Point in StEvent

	Float_t xp,yp,zp;
	//Location of Point

	xp=(StEpcCut::RAD_SMD_E())*cos(PointPhi);
	yp=(StEpcCut::RAD_SMD_E())*sin(PointPhi);
	zp=(StEpcCut::RAD_SMD_E())*sinh(PointEta);
	StThreeVectorF PointPosition(xp*centimeter, yp*centimeter, zp*centimeter);

	//Error in location of Point
	xp=0.0;
	yp=0.0;
	zp=0.0;
	StThreeVectorF ErrorPosition(xp*centimeter, yp*centimeter, zp*centimeter);

	// Size of Point

	StThreeVectorF size(PointSigEta,PointSigPhi,0.0);

	// Chisquare
	//	Float_t ChiSquare = 0.0;
	//I am filling this chisquare with track mom now, so that it can be used for pi0 study , later on we need to do something so that deltaeta, deltaphican be stored.

	Float_t ChiSquare=TrackMom[i1][k[i1]-1];
        
	//Energy In Detector
	Float_t EnergyInDetector[4];
	{for(Int_t i=0;i<4;i++){EnergyInDetector[i]=PointEnergyinDet[i];}}
	Float_t SizeAtDetector[4];
	{for(Int_t i=0;i<4;i++){SizeAtDetector[i]=PointSizeinDet[i];}}

	StEmcPoint *point = new StEmcPoint();
	point->setPosition(PointPosition);
	point->setPositionError(ErrorPosition);
	point->setSize(size);
	point->setChiSquare(ChiSquare);
        //Energy of Point
	point->setEnergy(PointEnergy);
	for(Int_t i=0;i<4;i++){
	  StDetectorId id=static_cast<StDetectorId>(i+kBarrelEmcTowerId);
	  point->setEnergyInDetector(id,EnergyInDetector[i]);
	  point->setSizeAtDetector(id,SizeAtDetector[i]);
	}
	  mPointsReal.Add(point);
	  mNPointsReal++;
     }
 }
 return kStOK;
}

//------------------------------------------------------------------------

Int_t 
   StPointCollection::GlobSort( const St_dst_track *track) const 
{

	 HitTrackEta.clear();
	 HitTrackPhi.clear();
	 HitTrackMom.clear();

  // Constants
  double RIN            = 238.0;    // From Alexei
  double ROUT           =248.0;    // From Alexei
  RIN=BemcGeomIn->Radius();
  // Parameters
  double Rmincut      = 4.0;
  long   MinTrkPoints = 10;

  // Get BField from gufld(,) 
//  cout<<"Trying to Get the BField the old way..."<<endl;
  float x[3] = {0,0,0};
  float b[3];
  gufld(x,b);
  double bfield = 0.1*b[2]; //This is now Tesla.

  //	  cout<"Trying to Get the BField the new way..."<<endl;
  //          float y[3] = {0,0,0};
  //          float c[3];
  //          StMagF *mymag = new StMagF();
  //          mymag->Field(y,c);
  //          cout<<"New field in KGauss: "<<c[2]<<endl;
// counter for abnormal tracks
//

#ifdef ST_NO_TEMPLATE_DEF_ARGS
  vector<long,allocator<long> > index;
  vector<StPhysicalHelixD,allocator<StPhysicalHelixD> > clvec,helices;
  vector<double,allocator<double> > TrkLength,sigma;
#else
  vector<long > index;
  vector<StPhysicalHelixD > clvec,helices;
  vector<double > TrkLength,sigma;
#endif
  index.clear();
  double spath,h;
  double x0,y0,z0;
  double ptinv,psi,tanl;
  double px,py,pz;
  StThreeVectorD XVertex(0.,0.,0.);

  long Ntrk = track->GetNRows();
  if( Ntrk <= 0 ){
    cout<<"GlobSort: Event contains "<<Ntrk<<" global tracks. ";
    cout<<"No track for Matching"<<endl;
    return kStWarn; 
  }
    cout<<"To go for Ntrack "<<Ntrk<<endl;

  long i_non_tpc=0;
  dst_track_st *glb_track_pointer = track->GetTable();

  for (long l=0; l<Ntrk; l++){

    // First point on Helix
    x0 = glb_track_pointer->r0*cos(C_RAD_PER_DEG*glb_track_pointer->phi0);
    y0 = glb_track_pointer->r0*sin(C_RAD_PER_DEG*glb_track_pointer->phi0);
    z0 = glb_track_pointer->z0;
    StThreeVectorD origin(x0*centimeter, y0*centimeter, z0*centimeter);

    // Helicity / Sense of Curvatutre
    h  = 1.0;  if( bfield*glb_track_pointer->icharge > 0.0 )h=-1.0;
    double qtrk = 1.0; if( h*bfield > 0.0)qtrk=-1.0;

    // Track direction at first point
    ptinv  = glb_track_pointer->invpt;
    tanl   = glb_track_pointer->tanl;
    psi    = (C_PI/180.0)*glb_track_pointer->psi; if(psi<0.0){psi=psi+2.*C_PI;}

    px   = (1./ptinv)*cos(psi);
    py   = (1./ptinv)*sin(psi);
    pz   = (1./ptinv)*tanl;
    StThreeVectorD MomFstPt(px*GeV, py*GeV, pz*GeV);
    
    StPhysicalHelixD TrkHlx(MomFstPt, origin, bfield*tesla, qtrk);

    long NPnts = glb_track_pointer->n_point;
    if(NPnts > MinTrkPoints){
      helices.push_back(TrkHlx);
      double ltk = glb_track_pointer->length;
      TrkLength.push_back(ltk);
      long trk_id = glb_track_pointer->id;
      index.push_back(trk_id);
      if( glb_track_pointer->det_id != kTpcIdentifier )i_non_tpc=1;
    }

    glb_track_pointer++;

  }


  //Currently, use only pure tpc tracks
  //  if( i_non_tpc == 1 ){
  //    cout<<"This event contains non-tpc tracks - GlobSort currently only works for pure tpc tracks"<<endl;
  //    return kStWarn;
  //  }

#ifdef ST_NO_TEMPLATE_DEF_ARGS
  vector<StPhysicalHelixD,allocator<StPhysicalHelixD> >::iterator ihlx=helices.begin();
  vector<double,allocator<double> >::iterator ihelp=TrkLength.begin();
  vector<long,allocator<long> >::iterator i_index=index.begin();
#else
  vector<StPhysicalHelixD >::iterator ihlx=helices.begin();
  vector<double >::iterator ihelp=TrkLength.begin();
  vector<long >::iterator i_index=index.begin();
#endif  
  while( ihlx != helices.end()){
    StPhysicalHelixD trk = *ihlx;
    double xorigin = 0.0; double yorigin = 0.0;
    spath = trk.pathLength(xorigin, yorigin);
    StThreeVectorD XMinVec = trk.at(spath);
    double x_m = XMinVec.x(), y_m = XMinVec.y();
    double dmin = sqrt(x_m*x_m + y_m*y_m);
    if( dmin > Rmincut ){
      helices.erase(ihlx);
      TrkLength.erase(ihelp);
      index.erase(i_index);
    }
    else{
      ihlx++; ihelp++; i_index++;
    }
  }

  // Do the Multiple Scattering
  for(unsigned int jj=0; jj < helices.size(); jj++){
//    double lpath_tot = 0.0;
    double xo = 0.0; double yo = 0.0;
    spath = helices[jj].pathLength(xo, yo);
    double s=0.0;
    double R1St = sqrt( helices[jj].x(s)*helices[jj].x(s) + helices[jj].y(s)*helices[jj].y(s) );
    if( R1St > RIN ){cout<<"GlobSort: ERROR: Radius of First point > EmcInnerradius!! R1St= "<<R1St<<endl; return kStWarn;}

    // Find Coordinates of Intersect with Emc Inner radius
    if( R1St < RIN ){
      double ifcpath=0.0;
      pairD  SIfc; 
      SIfc = helices[jj].pathLength(RIN);


//This portion was used at the begining
//      if(SIfc.first > 0.0 && SIfc.first>helices[jj].period())
//                                  SIfc.first -= helices[jj].period();
//      if(SIfc.second > 0.0 && SIfc.second>helices[jj].period())
//                                  SIfc.second -= helices[jj].period();
//
// After Thomas's mail regarding this this portion was used
      ifcpath =  (SIfc.first < 0 || SIfc.second < 0) 
          ? max(SIfc.first, SIfc.second) : min(SIfc.first, SIfc.second); 
//
//
//                                                     
//      if(SIfc.second>0 && SIfc.first>0){
//      if( SIfc.second > SIfc.first){
//        ifcpath = SIfc.first;
//      }
//      else{
//        ifcpath = SIfc.second;
//      }
//      }
//	if(SIfc.first>0 && SIfc.second<0)ifcpath=SIfc.first;
//	if(SIfc.first<0 && SIfc.second>0)ifcpath=SIfc.second;
	Int_t PhiBinI,imodI;
      // Find momentum at this point
      StThreeVectorD pmom;
      pmom = helices[jj].momentumAt(ifcpath, bfield*tesla);
      Float_t Mom = sqrt(pmom.x()*pmom.x()+pmom.y()*pmom.y()+pmom.z()*pmom.z());
//----------------------------------------------------------------------------
      StThreeVectorD xpos;
      xpos = helices[jj].at(ifcpath);
// Calculate eta and phi at the point of intersection
	   Float_t Phi_hit=atan2(xpos.y(),xpos.x());
	   double rr=sqrt(xpos.x()*xpos.x()+xpos.y()*xpos.y());
	   Float_t theta_hit=atan(rr/fabs(xpos.z()));
	   Float_t eta_hit=-log(tan(theta_hit/2.));
	   if(xpos.z()<0)eta_hit=0.-eta_hit;

	   Int_t imod,HitPhiBin;
           Int_t ebin,pbin;
	   imod=0;
	   //
  if(TMath::Abs(eta_hit)<1.0){
	   Int_t & imd =imod;
           Int_t testb=BemcGeomIn->getBin(Phi_hit,eta_hit,imd,ebin,pbin);
	   if(testb==0)imod=imd;
	   if(testb==0){ 
	   HitPhiBin=Int_t(TMath::Abs(eta_hit*10));
              if(HitPhiBin>0){
       	 if((TMath::Abs(eta_hit*10)-Float_t(HitPhiBin))<=0.01){
       	   HitPhiBin--;
       	 }
       }
	   if(HitPhiBin>9){HitPhiBin=9;}
	   imodI=imod;
	   PhiBinI=HitPhiBin;
	   HitTrackEta.push_back(eta_hit);
	   HitTrackPhi.push_back(Phi_hit);
	   HitTrackMom.push_back(Mom);

           }
  }

           // Getting the intersection on the outer surface of emc
           //
      ifcpath=0.0;
      SIfc = helices[jj].pathLength(ROUT);

// This portion was used before Thomas's mail on this
//
//      if(SIfc.first > 0.0 && SIfc.first>helices[jj].period())
//                                  SIfc.first-=helices[jj].period();
//      if(SIfc.second > 0.0 && SIfc.second>helices[jj].period())
//                                  SIfc.second-=helices[jj].period();

      ifcpath =  (SIfc.first < 0 || SIfc.second < 0) 
          ? max(SIfc.first, SIfc.second) : min(SIfc.first, SIfc.second); 
//
//      if(SIfc.second>0 && SIfc.first>0){
//      if( SIfc.second > SIfc.first){
//        ifcpath = SIfc.first;
//      }
//      else{
//        ifcpath = SIfc.second;
//      }
//      }
//	if(SIfc.first>0 && SIfc.second<0)ifcpath=SIfc.first;
//	if(SIfc.first<0 && SIfc.second>0)ifcpath=SIfc.second;

      // Find momentum at this point
//      StThreeVectorD pmom;
      pmom = helices[jj].momentumAt(ifcpath, bfield*tesla);
      Mom = sqrt(pmom.x()*pmom.x()+pmom.y()*pmom.y()+pmom.z()*pmom.z());
//-----------------------------------------------------------------
      StThreeVectorD xposO;
      xposO = helices[jj].at(ifcpath);
// Calculate eta and phi at the point of intersection
	   Phi_hit=atan2(xposO.y(),xposO.x());
	   rr=sqrt(xposO.x()*xposO.x()+xposO.y()*xposO.y());
	   theta_hit=atan(rr/fabs(xposO.z()));
	   eta_hit=-log(tan(theta_hit/2.));
	   if(xposO.z()<0)eta_hit=0.-eta_hit;
	   if(TMath::Abs(eta_hit)<1.0){   
	   Int_t & imd =imod;
           Int_t testb=BemcGeomIn->getBin(Phi_hit,eta_hit,imd,ebin,pbin);
	   if(testb==0){ 
	   HitPhiBin=Int_t(TMath::Abs(eta_hit*10));
            if(HitPhiBin>0){
       	 if((TMath::Abs(eta_hit*10)-Float_t(HitPhiBin))<=0.01){
       	   HitPhiBin=HitPhiBin-1;
       	 }
       }
	   if(HitPhiBin>9){HitPhiBin=9;}
	   if(imod!=imodI || PhiBinI != HitPhiBin){
	   HitTrackEta.push_back(eta_hit);
	   HitTrackPhi.push_back(Phi_hit);
	   HitTrackMom.push_back(Mom);
	   }
           }
	   }

    }
  }
  return kStOK;
}
//-------------------------------------------------------
