
//
// $id$
//
// $Log: StPointCollection.cxx,v $
// Revision 1.3  2000/08/29 20:33:04  subhasis
// Modified to accept input from StEvent and writing output to StEvent for Emc
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
/////////////////////////////////////////////////
/////////////////////////
#include "StPointCollection.h"
#include "StPi0Candidate.h"
#include "emc_def.h"

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
#include "StEvent/StEvent.h"
#include "St_ObjectSet.h"
#include "StEvent/StEmcCollection.h"
#include "StEvent/StEmcDetector.h"
#include "StEvent/StEmcModule.h"
#include "StEvent/StEmcRawHit.h"
#include "StEvent/StEmcClusterCollection.h"
#include "StEvent/StEmcCluster.h"
#include "StEvent/StEmcPoint.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StEventTypes.h"  

#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)

// declaring cernlib routine (mathlib, H301) assndx to be used for matching.
#define    assndx  F77_NAME(assndx,ASSNDX)
extern "C" {void type_of_call assndx ( Int_t &, Float_t (*)[], Int_t &, Int_t &,
 Int_t &,Int_t *, Float_t &,Int_t (*)[],Int_t &); }


ClassImp(StPointCollection)


  StEmcGeom *BemcGeomIn;
  StEmcGeom *BemcGeomOut;

const TString detname[] = {"Bemc", "Bsmde", "Bsmdp"};
// Extern for sorted emc-smd
  StMatchVecClus matchlist_bemc_clus[Epc::nModule][Epc::nPhiBin];
  StMatchVecClus matchlist_bprs_clus[Epc::nModule][Epc::nPhiBin];
  StMatchVecClus matchlist_bsmde_clus[Epc::nModule][Epc::nPhiBin];
  StMatchVecClus matchlist_bsmdp_clus[Epc::nModule][Epc::nPhiBin];

  FloatVector HitTrackEta;
  FloatVector HitTrackPhi;
  FloatVector HitTrackMom;
  StTrackVec  HitTrackPointer;

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

//*************** FIND EMC POINTS **********************************

Int_t 
  StPointCollection::findEmcPoints(StEmcClusterCollection* Bemccluster,
                                StEmcClusterCollection *Bprscluster,
                                StEmcClusterCollection *Bsmdecluster,
                                StEmcClusterCollection *Bsmdpcluster,
                                StTrackVec& TrackToFit)
{
//Sort BEMC, SMDe, SMDp, PRS clusters according to location 

                    PointCalc(Bemccluster,
                              Bprscluster,
                              Bsmdecluster,
                              Bsmdpcluster);

// Getting BemcGeom to obtain radius etc

   BemcGeomIn  = new StEmcGeom("bemc");
   BemcGeomOut = new StEmcGeom("bemc");                                        

  if(TrackToFit.size()>0){
    cout<<" Taking Tracks from StEvent for track matching**"<<endl;
    TrackSort(TrackToFit);
  }

    //track check array for checking if the track is matched
     Int_t *Trcheck = new Int_t[HitTrackEta.size()];
  for(UInt_t i1=0;i1<HitTrackEta.size();i1++){
      Trcheck[i1]=0;
   }

// MATCHING****************

           for(Int_t im=0;im<Epc::nModule;im++){
            for(Int_t is=0;is<Epc::nPhiBin;is++){

  if(matchlist_bemc_clus[im][is].size()>0){
  Int_t testp=GetEmcPointEvent(matchlist_bemc_clus[im][is],
                         matchlist_bsmde_clus[im][is],
                         matchlist_bsmdp_clus[im][is],
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
//--------------------------------------------------------------------------
Int_t
  StPointCollection::GetEmcPointEvent(const StMatchVecClus mvec,
                                 const StMatchVecClus evec,
                                 const StMatchVecClus pvec,
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
     for (Int_t iF=0;iF<9;iF++){PointMember[iF]=0.0;}

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
     StEmcCluster *cl0;
     cl0=(StEmcCluster*)mvec[ims];
     Float_t emen=cl0->energy();
        EmcTot+=emen;
     }
   }
  //
  for(Int_t ie=0;ie<na;ie++){
    StEmcCluster *cl1;
    for(Int_t ip=0;ip<ma;ip++){
      StEmcCluster *cl2;
      switch (Category) {
      case 0:
	cl1 = (StEmcCluster*)mvec[ie];
	cl2 = (StEmcCluster*)mvec[ip];
	break;
      case 1:
	cl1 = (StEmcCluster*)evec[ie];
	cl2 = (StEmcCluster*)mvec[ip];
	break;
      case 2:
	cl1 = (StEmcCluster*)mvec[ie];
	cl2 = (StEmcCluster*)pvec[ip];
	break;
      case 3:
	cl1 = (StEmcCluster*)evec[ie];
	cl2 = (StEmcCluster*)pvec[ip];
	break;
      }

      
// track matching
      Int_t Trmatch=0;
     Float_t PhitoMatch=cl2->phi();
     Float_t EtatoMatch=cl1->eta();
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
	 Trmatch++;
        }
       }
       }
     }//it loop
     }// etrack size check

          Float_t diff=TMath::Abs((cl1->energy())-(cl2->energy()));
          Float_t summ= (cl1->energy())+(cl2->energy());
    	  ep[ip][ie]=diff/summ;

    }
  }

 assndx(mode,ep,na,ma,ida,k,smin,iw,idw);

     switch (Category) {
     case 0:
       for(Int_t i1=0;i1<na;i1++){
       if((k[i1]-1)>=0){
	 StEmcCluster *cl1;
	 cl1 = (StEmcCluster*)mvec[i1];
	 Float_t avg_en = cl1->energy();
	 totAvg += avg_en;
       }
       }
       break;
     case 1:
       for(Int_t i1=0;i1<na;i1++){
       if((k[i1]-1)>=0){
	 StEmcCluster *cl1;
	 cl1 = (StEmcCluster*)evec[i1];
	 Float_t avg_en = cl1->energy();
	 totAvg += avg_en;
       }
       }
       break;
     case 2:
       for(Int_t i1=0;i1<na;i1++){
       if((k[i1]-1)>=0){
	 StEmcCluster *cl1;
	 cl1 = (StEmcCluster*)mvec[i1];
	 Float_t avg_en = cl1->energy();
	 totAvg += avg_en;
       }
       }
       break;
     case 3:
       for(Int_t i1=0;i1<na;i1++){
       if((k[i1]-1)>=0){
	 StEmcCluster *cl1;
	 cl1 = (StEmcCluster*)evec[i1];
	 StEmcCluster *cl2;
	 cl2 = (StEmcCluster*)pvec[k[i1]-1];
	 Float_t avg_en = (cl1->energy()+cl2->energy())/2.;
	 totAvg += avg_en;
       }
       }
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
	 StEmcCluster *cl1;
	 cl1 = (StEmcCluster*)mvec[i1];
	 PointEta=cl1->eta();
	 PointSigEta=cl1->sigmaEta();
	 PointPhi=cl1->phi();
	 PointSigPhi=cl1->sigmaPhi();
	 PointEnergy=cl1->energy();
	 PointEnergyinDet[0]=cl1->energy();
	 PointSizeinDet[0]=cl1->sigmaEta();
	 }
       break;
     case 1:
       if((k[i1]-1)>=0){
	 StEmcCluster *cl1;
	 cl1 = (StEmcCluster*)evec[i1];
	 StEmcCluster *cl2;
	 cl2 = (StEmcCluster*)mvec[k[i1]-1];
	 PointEta=cl1->eta();
	 PointSigEta=cl1->sigmaEta();
	 PointPhi=cl2->phi();
	 PointSigPhi=cl2->sigmaPhi();
	 PointEnergy=cl1->energy()*(EmcTot/totAvg);
	 PointEnergyinDet[0]=cl2->energy();
	 PointEnergyinDet[2]=cl1->energy();
	 PointSizeinDet[0]=cl2->sigmaEta();
	 PointSizeinDet[2]=cl1->sigmaEta();
	 }
       break;
     case 2:
       if((k[i1]-1)>=0){
	 StEmcCluster *cl1;
	 cl1 = (StEmcCluster*)mvec[i1];
	 StEmcCluster *cl2;
	 cl2 = (StEmcCluster*)pvec[k[i1]-1];
	 PointEta=cl1->eta();
	 PointSigEta=cl1->sigmaEta();
	 PointPhi=cl2->phi();
	 PointSigPhi=cl2->sigmaPhi();
	 PointEnergy=cl1->energy()*(EmcTot/totAvg);
	 PointEnergyinDet[0]=cl1->energy();
	 PointEnergyinDet[3]=cl2->energy();
	 PointSizeinDet[0]=cl1->sigmaEta();
	 PointSizeinDet[3]=cl2->sigmaPhi();
	 }
       break;
     case 3:
       if((k[i1]-1)>=0){
	 StEmcCluster *cl1;
	 cl1 = (StEmcCluster*)evec[i1];
	 StEmcCluster *cl2;
	 cl2 = (StEmcCluster*)pvec[k[i1]-1];
	 PointEta=cl1->eta();
	 PointSigEta=cl1->sigmaEta();
	 PointPhi=cl2->phi();
	 PointSigPhi=cl2->sigmaPhi();
	 PointEnergy=((cl1->energy()+cl2->energy())/2.)*(EmcTot/totAvg);
	 PointEnergyinDet[0]=EmcTot;
	 PointEnergyinDet[2]=cl1->energy();
	 PointEnergyinDet[3]=cl2->energy();
	 PointSizeinDet[2]=cl1->sigmaEta();
	 PointSizeinDet[3]=cl2->sigmaPhi();
	 }
       break;
     }
     if((k[i1]-1)>=0){
	PointMember[0]=PointEta;
	//      Float_t tempeta=((StEmcCluster*)mvec[0])->eta();
	PointMember[1]=PointPhi;
	PointMember[2]=PointSigEta;
	PointMember[3]=PointSigPhi;
	PointMember[4]=PointEnergy;
	PointMember[8]=(Float_t)Category;
	if(k_track[i1][k[i1]-1]>0){
	PointMember[5]=TrackMom[i1][k[i1]-1];
	PointMember[6]=DeltaEta[i1][k[i1]-1];
	PointMember[7]=DeltaPhi[i1][k[i1]-1];
	}
	if(Category==3){Int_t testadd = addPoints(PointMember);
	if(testadd==1)cout<<" addPoints not O.K"<<endl;}
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
	for(Int_t i=0;i<4;i++){EnergyInDetector[i]=PointEnergyinDet[i];}
	Float_t SizeAtDetector[4];
	for(Int_t i=0;i<4;i++){SizeAtDetector[i]=PointSizeinDet[i];}

	StEmcPoint *point = new StEmcPoint();
	point->setPosition(PointPosition);
	point->setPositionError(ErrorPosition);
	point->setSize(size);
	point->setChiSquare(ChiSquare);
        //Energy of Point
	point->setEnergy(PointEnergy);
	// Pointer to matched track "TrackPointer[k_track[i1][k[i1]-1]-1]"
        
	if(k_track[i1][k[i1]-1]>0){
	  //Set track pointer here
	  point->addTrack(HitTrackPointer[k_track[i1][k[i1]-1]-1]);
	}
	else{
	  //set ponter to zero
	  point->addTrack(NULL);
	}

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

//-------------------------------------------------------
Int_t 
   StPointCollection::TrackSort( const StTrackVec & TrackToFit) const
{
  //  cout<<" Inside TrackSort*** size "<<TrackToFit.size()<<endl;

  double spath;
  //  double x0,y0,z0;
  //  double ptinv,psi,tanl;
  //  double px,py,pz;
  StThreeVectorD XVertex(0.,0.,0.);

	 HitTrackEta.clear();
	 HitTrackPhi.clear();
	 HitTrackMom.clear();
	 HitTrackPointer.clear();

  // Constants
  double RIN            = 238.0;    // From Alexei, should be replaced by 
                                    // SMD radius
  //  double ROUT           =248.0;    // From Alexei
  RIN=BemcGeomIn->Radius();
  // Parameters
  //  double Rmincut      = 4.0;
  //  long   MinTrkPoints = 10;

  // Get BField from gufld(,) 
//  cout<<"Trying to Get the BField the old way..."<<endl;
  float x[3] = {0,0,0};
  float b[3];
  gufld(x,b);
  double bfield = 0.1*b[2]; //This is now Tesla.

#ifdef ST_NO_TEMPLATE_DEF_ARGS
  vector<StPhysicalHelixD,allocator<StPhysicalHelixD> > helices;
#else
  vector<StPhysicalHelixD > helices;
#endif


  helices.clear();
  // Do the Multiple Scattering
  for(unsigned int jj=0; jj < TrackToFit.size(); jj++){
StPhysicalHelixD  Helix = TrackToFit[jj]->geometry()->helix();
helices.push_back(Helix);
  }

  //  cout<<" HELIX FILLED ***Size **"<<helices.size()<<endl;


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


      ifcpath =  (SIfc.first < 0 || SIfc.second < 0) 
          ? max(SIfc.first, SIfc.second) : min(SIfc.first, SIfc.second); 
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
	   HitTrackPointer.push_back(TrackToFit[jj]);

           }
  }

  /*
           // Getting the intersection on the outer surface of emc
           //
      ifcpath=0.0;
      SIfc = helices[jj].pathLength(ROUT);

      ifcpath =  (SIfc.first < 0 || SIfc.second < 0) 
          ? max(SIfc.first, SIfc.second) : min(SIfc.first, SIfc.second); 
//
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
	   
  */

    }
  }
  //  cout<<" END OF TRACKSORT*** size**"<<HitTrackEta.size()<<" "<<HitTrackPhi.size()<<endl;

  return kStOK;
}
//-----------------------------------------------------------------------
void
 StPointCollection::PointCalc(StEmcClusterCollection* Bemccluster,
             StEmcClusterCollection* Bprscluster,
             StEmcClusterCollection* Bsmdecluster,
	     StEmcClusterCollection* Bsmdpcluster)
{
  cout<<" I am inside PointCalc***"<<endl;
   for(Int_t i1=0;i1<Epc::nModule;i1++){
     for(Int_t i2=0;i2<Epc::nPhiBin;i2++){
	 matchlist_bemc_clus[i1][i2].clear();
	 matchlist_bprs_clus[i1][i2].clear();
	 matchlist_bsmde_clus[i1][i2].clear();
	 matchlist_bsmdp_clus[i1][i2].clear();
     }
   }
   StEmcGeom* GeomIn  = new StEmcGeom("bemc");

  //BEMC

  if(Bemccluster){
Int_t Ncluster0=Bemccluster->numberOfClusters();
          if(Ncluster0>0){
          const StSPtrVecEmcCluster& emcclusters= Bemccluster->clusters();
	  for(UInt_t i=0;i<emcclusters.size();i++){
             StEmcCluster *cl1=(StEmcCluster*)emcclusters[i];
	     Float_t eta_emc=cl1->eta(); 
	     Float_t phi_emc=cl1->phi(); 
	     //Get the module number
	     Int_t ebin,pbin;
	     Int_t emc_module=0;
	     Int_t & imd =emc_module;
           Int_t testb=GeomIn->getBin(phi_emc,eta_emc,imd,ebin,pbin);
	   if(testb==0)emc_module=imd;
	   if(testb==0){
       Int_t emc_phi_bin=Int_t(TMath::Abs(eta_emc*10));
       //keeping the cluster very close to phibin boundry to the previous bin
       //
       //       cout<<" EMC module no, phibin**"<<emc_module<<" "<<emc_phi_bin<<endl;
       if(emc_phi_bin>0){
	 if((TMath::Abs(eta_emc*10)-Float_t(emc_phi_bin))<=0.01){
	   emc_phi_bin--;
	 }
       }
       if(emc_phi_bin>9){emc_phi_bin=9;}
//copy cl1 pointer to StMatchvec
	matchlist_bemc_clus[emc_module-1][emc_phi_bin].push_back(cl1);
	  }
	  }
	  }
  }
  //PRS
  if(Bprscluster){
Int_t Ncluster1=Bprscluster->numberOfClusters();
          if(Ncluster1>0){
          const StSPtrVecEmcCluster& emcclusters= Bprscluster->clusters();
	  for(UInt_t i=0;i<emcclusters.size();i++){
             StEmcCluster *cl2=(StEmcCluster*)emcclusters[i];
	     Float_t eta_emc=cl2->eta(); 
	     Float_t phi_emc=cl2->phi(); 
	     //Get the module number
	     Int_t ebin,pbin;
	     Int_t emc_module=0;
	     Int_t emc_phi_bin=0;
	     Int_t & imd =emc_module;
           Int_t testb=GeomIn->getBin(phi_emc,eta_emc,imd,ebin,pbin);
	   if(testb==0)emc_module=imd;
	   if(testb==0){
         emc_phi_bin=Int_t(TMath::Abs(eta_emc*10));
       //keeping the cluster very close to phibin boundry to the previous bin
       //
       if(emc_phi_bin>0){
	 if((TMath::Abs(eta_emc*10)-Float_t(emc_phi_bin))<=0.01){
	   emc_phi_bin--;
	 }
       }
       if(emc_phi_bin>9){emc_phi_bin=9;}
//copy cl1 pointer to StMatchvec
       matchlist_bprs_clus[emc_module-1][emc_phi_bin].push_back(cl2);
	  }
	  }
	  }
  }

  //BSMD_ETA

  if(Bsmdecluster){
Int_t Ncluster2=Bsmdecluster->numberOfClusters();
          if(Ncluster2>0){
          const StSPtrVecEmcCluster& emcclusters= Bsmdecluster->clusters();
	  for(UInt_t i=0;i<emcclusters.size();i++){
             StEmcCluster *cl3=(StEmcCluster*)emcclusters[i];
	     Float_t eta_emc=cl3->eta(); 
	     Float_t phi_emc=cl3->phi(); 
	     //Get the module number
	     Int_t ebin,pbin;
	     Int_t emc_module=0;
	     Int_t emc_phi_bin=0;
	     Int_t & imd =emc_module;
           Int_t testb=GeomIn->getBin(phi_emc,eta_emc,imd,ebin,pbin);
	   if(testb==0)emc_module=imd;
	   if(testb==0){
           emc_phi_bin=Int_t(TMath::Abs(eta_emc*10));
       //keeping the cluster very close to phibin boundry to the previous bin
       //
	   //       cout<<" SMDE module no, phibin**"<<emc_module<<" "<<emc_phi_bin<<endl;
       if(emc_phi_bin>0){
	 if((TMath::Abs(eta_emc*10)-Float_t(emc_phi_bin))<=0.01){
	   emc_phi_bin--;
	 }
       }
       if(emc_phi_bin>9){emc_phi_bin=9;}
//copy cl1 pointer to StMatchvec
	matchlist_bsmde_clus[emc_module-1][emc_phi_bin].push_back(cl3);
	  }
	  }
	  }
  }

  // BSMDP

  if(Bsmdpcluster){
Int_t Ncluster3=Bsmdpcluster->numberOfClusters();
          if(Ncluster3>0){
          const StSPtrVecEmcCluster& emcclusters= Bsmdpcluster->clusters();
	  for(UInt_t i=0;i<emcclusters.size();i++){
             StEmcCluster *cl4=(StEmcCluster*)emcclusters[i];
	     Float_t eta_emc=cl4->eta(); 
	     Float_t phi_emc=cl4->phi(); 
	     //Get the module number
	     Int_t ebin,pbin;
	     Int_t emc_module=0;
	     Int_t & imd =emc_module;
           Int_t testb=GeomIn->getBin(phi_emc,eta_emc,imd,ebin,pbin);
	   if(testb==0)emc_module=imd;
	   if(testb==0){
       Int_t emc_phi_bin=Int_t(TMath::Abs(eta_emc*10));
       //keeping the cluster very close to phibin boundry to the previous bin
       //
       //       cout<<" SMDP module no, phibin**"<<emc_module<<" "<<emc_phi_bin<<endl;
       if(emc_phi_bin>0){
	 if((TMath::Abs(eta_emc*10)-Float_t(emc_phi_bin))<=0.01){
	   emc_phi_bin--;
	 }
       }
       if(emc_phi_bin>9){emc_phi_bin=9;}
//copy cl1 pointer to StMatchvec
	matchlist_bsmdp_clus[emc_module-1][emc_phi_bin].push_back(cl4);
	  }
	  }

	  }
  }
  delete GeomIn;
  //  return kStOK;
}


