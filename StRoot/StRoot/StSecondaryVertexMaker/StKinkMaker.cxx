/*!
 * \class  StKinkMaker
 * \brief  Class to find kink secondary vertices
 * \author Camelia Mironov, KSU
 * \date   Jan,2004
 *
 *
 */

#include <iostream>
#include <cstdlib>
#include <cstring>
#include "StMemStat.h"
#include "StKinkMaker.h"
#include "StKinkLocalTrack.hh"
#include "StTrackGeometry.h"
#include "StV0FinderMaker.h"
#include "StEvent/StEventTypes.h"
#include "StEvent.h"
#include "TMath.h"
#include "StTrack.h"
#include "tables/St_tkf_tkfpar_Table.h"
#include "StMessMgr.h"

#include "math_constants.h"
#include "phys_constants.h"
#include "TVector2.h"
#include "StThreeVectorF.hh"
#include "TObjArray.h"
#include "SystemOfUnits.h"


#if !defined(ST_NO_NAMESPACES)
using namespace units;
#endif

const double kaonMass= M_KAON_PLUS;
const double pionMass= M_PION_PLUS;
const double muonMass= M_MUON_PLUS;
const double pi0Mass= M_PION_0;

const double kaonToMuonQ= 0.236;
const double kaonToPionQ= 0.205;
const double pionToMuonQ= 0.030;
  
const int MAXNUMOFTRACKS= 10000;
const int SIZETRKIDCHECK= 1000;


//=======================================================================
  StKinkMaker::StKinkMaker(const char *name):StMaker(name),m_tkfpar(0)
{
  mTrack1     = 0;          
  mTrack2     = 0;   
  mGlobalTrks = 0;
  mParentTrackCandidate=0;             
  mDaughterTrackCandidate=0;
  mDaughterTrackUnic=0;
  mUseTracker = kTrackerUseBOTH;
  event       = 0;
  kinkVertex  = 0;
  mBfield     = -2.;   // arbitrary value, normalized after ...neat (gene)
}
//=======================================================================
StKinkMaker::~StKinkMaker(){
}

//======================================================================

/*!

  Init() will aslo initialize the track type to select on. 
  See SetTrackerUsage(). Note that the steering of how to call
  SetTrackerUsage() is done through StBFChain and the maker m_Mode
  mechanism. Refer to SetTrackerUsage() method for more explaination.

*/
Int_t StKinkMaker::Init(){

  // m_Mode -> SetTrackerUsage()
  if      (m_Mode == 1) SetTrackerUsage(kTrackerUseTPT);
  else if (m_Mode == 2) SetTrackerUsage(kTrackerUseITTF);
  else if (m_Mode == 3) SetTrackerUsage(kTrackerUseBOTH);

 
  return StMaker::Init();
}
//=============================================================================
Int_t StKinkMaker::InitRun(int runumber) {
  m_tkfpar = (St_tkf_tkfpar*) GetDataBase("Calibrations/tracker/tkf_tkfpar");
  if (!m_tkfpar) {
    gMessMgr->Error(
      "StKinkMaker::InitRun() : could not find tkf_tkfpar in database.");
    return kStErr;
  }
  return StMaker::InitRun(runumber);
 }
//=============================================================================
Int_t StKinkMaker::Make(){//called for each event


  //******* variables
  unsigned short nNodes,i,j;
  int cutLast=0;
  StKinkLocalTrack* tempTrack;
  TObjArray trackArray(MAXNUMOFTRACKS);trackArray.SetOwner();
 
  tkf_tkfpar_st *tkfpar = m_tkfpar->GetTable();
  gMessMgr->Info()<<"StKinkMaker: impact parameter"<<tkfpar->impactCut<<endm;
  StThreeVectorF p,start,end;

  //******* GET 
  //### event
  event = (StEvent*)GetInputDS("StEvent");
  if (!event){
    gMessMgr->Warning("StKinkMaker:no StEvent;skip event");
    return kStWarn;
  }

  //### primary vertex
  StPrimaryVertex* vrtx=event->primaryVertex();
  if (!vrtx){
    gMessMgr->Warning("StKinkMaker:no primary vertex;skip event");
    return kStWarn;
  }
  mEventVertex = vrtx->position(); //StThreeVectorD
 
  //### global tracks to use
  StSPtrVecTrackNode& theNodes = event->trackNodes();
  nNodes = theNodes.size();
  mGlobalTrks=0;
  for (i=0;i<nNodes;i++){
    int nj = theNodes[i]->entries(global);
    for (j=0; j<nj;j++){
      StTrack* trk = theNodes[i]->track(global,j);
      if( !acceptTrack(trk) )continue;

     //******* find the magnetic field
      StTrackGeometry* trkGeom = trk->geometry();
      if(!trkGeom) continue; //ignore the track if it has no geometry 

      if(!mGlobalTrks)//calculate mBfield only for the first global trk from the event
        {

         StThreeVectorD p11 = trk->geometry()->momentum();
         StThreeVectorD p22 = trk->geometry()->helix().momentum(mBfield);

	 if      (fabs(p22.x()) > 1.e-20) mBfield *= p11.x()/p22.x();
	 else if (fabs(p22.y()) > 1.e-20) mBfield *= p11.y()/p22.y();
	 else continue;

         if (fabs(mBfield) < 1.e-20) return kStWarn;
	}
	mGlobalTrks++;
      //### cut: fiducial volume

      Float_t trkStartRadius2D = trk->geometry()->origin().perp();
      Float_t trkEndRadius2D = trk->outerGeometry()->origin().perp();
      if (trkStartRadius2D < tkfpar->vertexRMin2D && //cut [133,179]
            trkEndRadius2D > tkfpar->vertexRMax2D ) 
	{ continue;}
      if (trkStartRadius2D < tkfpar->vertexRMax2D ||
	    trkEndRadius2D > tkfpar->vertexRMin2D ){
	tempTrack = new StKinkLocalTrack(trk);
	trackArray.Add(tempTrack);//keep the track if ok

      }
      }//for each track in the node
  }   //for each node
  //***************************************************************************************
  // cout<<" magnetic field="<<mBfield<<endl;
  //******* erase existing kinks; because everything runs in chain, the tkf makes its own kinks
  StSPtrVecKinkVertex& kinkVertices = event->kinkVertices();
  /* StMemStat memStat("kinkVertices");
  kinkVertices.getEntries();
  cout<<"memory used"<<  memStat.Used();
  */
  kinkVertices.erase(kinkVertices.begin(),kinkVertices.end());
  // kinkVertices.getEntries();
  //cout<<"memory used"<< memStat.Used();
  //  StSPtrVecKinkVertex kinkVertices2;
  //kinkVertices = kinkVertices2;

//******* sorts by the trkStartRadius2D==>the potential parent trk, with 
      //smaller radius will be ahead of the potential daughters
  if (trackArray.GetEntries() > 0) trackArray.Sort();
  Int_t   kinkCandidate=0;
  Int_t cutPPt=0,cutPImpact=0,initial=0;
  
  int ni=trackArray.GetEntries();
  for (i=0;i<ni;i++){//parent
    initial++;
    mTrack1 = (StKinkLocalTrack*)trackArray.At(i);
    mParentTrackCandidate = mTrack1->trackBack();
    StTrackGeometry* myParentGeometry1 = mParentTrackCandidate->geometry(); //first point
    StTrackGeometry* myParentGeometry11 = mParentTrackCandidate->outerGeometry();//last point
    StPhysicalHelixD parentHelix = myParentGeometry1->helix();
    double parentPtot = myParentGeometry11->momentum().mag();
    double parentPt = myParentGeometry11->momentum().perp();
              
    //### cut pT parent minimum = 0.2 Gev/c
    if (myParentGeometry1->momentum().perp() < tkfpar->parentPtMin ) continue;
    cutPPt++;        

    //### cut : impact parameter parent
    mParentImpact = parentHelix.distance(mEventVertex);
    if (mParentImpact > tkfpar->impactCut ) continue;
    cutPImpact++;
    Int_t foundDaughters = 0;
    int nj = trackArray.GetLast()+1;
    for (j=i+1;j<nj;j++ ){//daughter
      mTrack2 = (StKinkLocalTrack*)trackArray.At(j);
      mDaughterTrackCandidate = mTrack2->trackBack();
      StTrackGeometry* myDaughterGeometry1 = mDaughterTrackCandidate->geometry();//first point
      StPhysicalHelixD daughterHelix = myDaughterGeometry1->helix();
      double daughterPtot = myDaughterGeometry1->momentum().mag();
      double daughterPt = myDaughterGeometry1->momentum().perp();

      //### cut: same charge
      if (myParentGeometry1->charge() != myDaughterGeometry1->charge() ) continue;
          
      //### cut:low lim fiducial volume = 133cm 
      if (myDaughterGeometry1->origin().perp() < tkfpar->vertexRMin2D ) continue; 
      
      //### cut:daughter impact parameter >2cm
      mDaughterImpact = daughterHelix.distance(mEventVertex);
      if (mDaughterImpact < mParentImpact) continue;
                
      //### cut:last_pt-first_pt < 14 (radial) and < 20 (z)
      if (fabs(myDaughterGeometry1->origin().perp() - myParentGeometry11->origin().perp())
	    > tkfpar->parentLastDaughterStart2D ) continue; 
      if (fabs(myDaughterGeometry1->origin().z() - myParentGeometry11->origin().z())
	    > tkfpar->parentLastDaughterStartZ ) continue;
      cutLast++;
       	  

      //##############################################################
      //2D method (gets 3D aproximation after two 2D solution      //
      //    ***** START DETERMINATION 2D DCA .... good luck        //
     //###############################################################
      //kink vertex; look at the intersection pts in 2d of the 2helices
      pairD paths,path2,paths1;                             // helix pathLength vars;
      TVector2 r1,r2,xc1 ,xc2,tmp2V;                  // for 2D helices projections
      StThreeVectorD x1,x2,p1,p2,x1i,x2j ;//position and momentum at 2d dca
      double separation,dxc;    // helix circle params
      double dca_12,dca_12tmp; // 2D dca ... helpers
      double rad_1,rad_2;//radii of the projected helices
      double distanceOne,distanceTwo;
  
      //### parent
      xc1.Set(parentHelix.xcenter(),parentHelix.ycenter());//center of parent helix circle
      r1.Set(parentHelix.origin().x(),parentHelix.origin().y());//first pt of the helix
      r1 -= xc1;//distance
      rad_1 = r1.Mod();//sqrt(x*x+y*y) length of the radius
      //### daughter
      xc2.Set(daughterHelix.xcenter(),daughterHelix.ycenter());//center of daughter helix circle
      r2.Set(daughterHelix.origin().x(),daughterHelix.origin().y());//first pt on the helix
      r2 -= xc2;//distance
      rad_2 = r2.Mod();//sqrt(x*x+y*y) length of the radius

      tmp2V = xc1 - xc2;//distance between centers
      dxc = tmp2V.Mod();//length =O1O2
      separation = dxc - (rad_1+rad_2);//O1O2-(r1+r2)           
     
      distanceOne = 0;
      dca_12 = -9999;
      dca_12tmp = -9999;
      if (dxc==0)continue;  
      if (separation < 0){
	if (dxc <= TMath::Abs(rad_1-rad_2)){//one helix circle, completely inside the other
	    double why = xc1.Y()-((rad_1+rad_2+dxc)*0.5 * (xc1.Y()-xc2.Y()))/dxc;
	    double ecs = xc1.X()-((rad_1+rad_2+dxc)*0.5 * (xc1.X()-xc2.X()))/dxc;
            paths.first  = parentHelix.pathLength(ecs,why);
            paths.second = daughterHelix.pathLength(ecs,why);
	    x1 = parentHelix.at(paths.first);
	    x2 = daughterHelix.at(paths.second);
	    dca_12=x1.z()-x2.z();
	    }else {//2 intersection points=>2 solution, process those who aren't nan
	           //paths containes the path lengths for this solution with
		    //that for track 1 stores in 'first', and the track2 stored in 'second'
		  path2 = parentHelix.pathLength(rad_2,xc2.X(),xc2.Y());
		  if (!std::isnan(path2.first)){
                      paths.first = path2.first;
		      x1 = parentHelix.at(paths.first);
		      paths.second = daughterHelix.pathLength(x1.x(),x1.y());
                      x2 = daughterHelix.at(paths.second);
                      dca_12 = x1.z()-x2.z();
		      distanceOne = (x1-myDaughterGeometry1->origin()).mag();
		  }
		  if ((!std::isnan(path2.second))&&(path2.second!=path2.first))
		       { paths1.first = path2.second;
		         x1i = parentHelix.at(paths1.first);
		         paths1.second = daughterHelix.pathLength(x1i.x(),x1i.y());
                         x2j = daughterHelix.at(paths1.second);
                         distanceTwo = (x2j-myDaughterGeometry1->origin()).mag();
                         dca_12tmp = x1i.z()-x2j.z();

			 if( distanceTwo < distanceOne){

	              //second solution is better
	                 dca_12 = dca_12tmp;
	                 x1 = x1i;
	                 x2 = x2j;
	                 paths = paths1;
		      }//distance
		  
                   }else if (std::isnan(path2.second))//no solution
		      { continue;}
	    }
	    
       }else if(separation==0){
            double why = xc1.Y()-(rad_1 * (xc1.Y()-xc2.Y()))/dxc;
	    double ecs = xc1.X()-(rad_1 * (xc1.X()-xc2.X()))/dxc;
            paths.first  = parentHelix.pathLength(ecs,why);
            paths.second = daughterHelix.pathLength(ecs,why);
	    x1 = parentHelix.at(paths.first);
	    x2 = daughterHelix.at(paths.second);
	    dca_12=x1.z()-x2.z();
       }else if ((separation < tkfpar->dcaParentDaughterMax)){
	      //helix circles are close, but not overlapping
	      //find dca to point halfway between circle centers
	          tmp2V = (xc1 + xc2) * 0.5;
	          paths.first  = parentHelix.pathLength(tmp2V.X(),tmp2V.Y());
                  paths.second = daughterHelix.pathLength(tmp2V.X(),tmp2V.Y());
	          x1 = parentHelix.at(paths.first);
	          x2 = daughterHelix.at(paths.second);
		  dca_12=x1.z()-x2.z();
		  //   cout << "3rd case :xtarget = " << x1.x() << "\ty target =" << x1.y()<<endl;
		  }else {continue ;}//helix circle too far apart 
     
//### cut: kink vertex in fiducial volume [133,179]
      if ((x1.perp() > tkfpar->vertexRMax2D)|| (x1.perp() < tkfpar->vertexRMin2D) )  continue;
      if ((x2.perp() > tkfpar->vertexRMax2D)|| (x2.perp() < tkfpar->vertexRMin2D) ) continue;
//### cut: projectPointZDiff (2cm)
     if (fabs(dca_12) > tkfpar->projectPointZDiff) continue;
    
//at this point, dca_12 can be used in 3D calculation of the dca
     //###############################################################
     //DONE @D DCA < START 3D DCA FINDING#                         //
     //##############################################################

          //### momentum of the 2 tracks , at the inters pt
     mParentMoment = parentHelix.momentumAt(paths.first, mBfield);
     mDaughterMoment = daughterHelix.momentumAt(paths.second, mBfield);
	  
          //### cut: decay angle > 1
      mDecayAngle = (1./degree) * mParentMoment.angle(mDaughterMoment);
      if (mDecayAngle<tkfpar->thetaMin) continue;
  
//### 3D dca between parent and daughter (comes out square)
     StThreeVectorD temp3V;
     double cos12,sin2_12,t1,t2;                     // 3D dca calculation vars
     double temp;
  
     p1 = mParentMoment/parentPtot;
     p2 = mDaughterMoment/daughterPtot;

     cos12 = p1.dot(p2);
     sin2_12 = (1.0 - cos12)*(1.- cos12);
     if( sin2_12){
        temp = dca_12/sin2_12;
        t1 = (-p1.z()+p2.z()*cos12)*temp;
        t2 = ( p2.z()-p1.z()*cos12)*temp;

        temp = rad_1*(parentPtot/parentPt);
        temp *= sin(t1/temp);
        x1i = x1 + p1.pseudoProduct(temp,temp,t1);

        temp = rad_2*(daughterPtot/daughterPt);
        temp *= sin(t2/temp);
        x2j = x2 + p2.pseudoProduct(temp,temp,t2);

        dca_12tmp = (x1i - x2j).mag2();
        dca_12 *= dca_12;

        if( dca_12tmp < dca_12){
            paths.first  = parentHelix.pathLength(x1i.x(),x1i.y());
            paths.second = daughterHelix.pathLength(x2j.x(),x2j.y());
            x1i =  parentHelix.at(paths.first);
            x2j =  daughterHelix.at(paths.second);
	      dca_12tmp = (x1i - x2j).mag2();
            if( dca_12tmp < dca_12){
	        x1 = x1i;
                x2 = x2j;
        	mParentMoment = parentHelix.momentumAt(paths.first, mBfield);
	        mDaughterMoment = daughterHelix.momentumAt(paths.second,mBfield);
        	dca_12 = dca_12tmp;
		}
	}
     }
     

  //############ end 3D DCA CALCUALTION
   mDca = sqrt(dca_12);


  //###cut: 3D dca < .5
  if (mDca>(tkfpar->dcaParentDaughterMax) ) continue;
  // cout << " dca is = " << mDca;	 
  //cutDca++;

  mKinkVertex = (x1 + x2) * 0.5;//vertex position
  Float_t dx,dy;
  dx = mKinkVertex.x() - myParentGeometry11->origin().x();
  dy = mKinkVertex.y() - myParentGeometry11->origin().y();
  Float_t distanceKinkParent2D   = sqrt(dx*dx+dy*dy );
  dx = mKinkVertex.x() - myDaughterGeometry1->origin().x();
  dy = mKinkVertex.y() - myDaughterGeometry1->origin().y();
  Float_t distanceKinkDaughter2D = sqrt(dx*dx+dy*dy );
  Float_t distanceKinkParentZ    = mKinkVertex.z() - myParentGeometry11->origin().z();
  Float_t distanceKinkDaughterZ  = mKinkVertex.z() - myDaughterGeometry1->origin().z();
	  
  //cut: distance kink track :radial<14, z<20
  if (distanceKinkParent2D > tkfpar->distanceKinkParent2D ) continue; 
  if (distanceKinkDaughter2D > tkfpar->distanceKinkDaughter2D ) continue; 
  if (distanceKinkParentZ > tkfpar->distanceKinkParentZ ) continue; 
  if (distanceKinkDaughterZ > tkfpar->distanceKinkDaughterZ ) continue; 
  foundDaughters++;
  mDaughterTrackUnic = myDaughterGeometry1;
   }//loop j.. daughters
  if(foundDaughters!=1)continue;//if found more than 1 daugthers for one parent track,ignore it

  kinkCandidate++;		  
  FillEvent(mDaughterTrackUnic,myParentGeometry11);
  kinkVertices.push_back(kinkVertex);
  //  cout<<"foundDaugthers"  <<foundDaughters<<endl;

 }//loop i .. parents
  // trackArray.Delete(); 
  gMessMgr->Info() << "StKinkMaker:: Found " << kinkCandidate << " kink candidates " << endm;

  //========================================================================
  // Look for kinks in which 2 different parents are sharing the same daughter
  // mark the coresonding  kink vertices by changing the decayAngle value
  // the new decay angle is [old*100+99]
  // make the vertex found zombie ???!!!

  if(kinkVertices.size()>1) Crop();
  return kStOK; 
}
	
//######################### DONE!!! ###############################################
     

/// Event filling
void StKinkMaker::FillEvent(StTrackGeometry *myDaughterGeometry1,StTrackGeometry *myParentGeometry11){

  kinkVertex = new StKinkVertex();
  StThreeVectorF pMomMinusdMom = mParentMoment - mDaughterMoment;
  Float_t  deltaKaonMuon = fabs(sqrt(mParentMoment.mag2() + kaonMass*kaonMass)   -
				sqrt(mDaughterMoment.mag2() + muonMass*muonMass) - 
				pMomMinusdMom.mag());
  Float_t  deltaKaonPion = fabs(sqrt(mParentMoment.mag2() + kaonMass*kaonMass)   -
				sqrt(mDaughterMoment.mag2() + pionMass*pionMass) - 
				sqrt(pMomMinusdMom.mag2() + pi0Mass*pi0Mass));
  Float_t  deltaPionMuon = fabs(sqrt(mParentMoment.mag2() + pionMass*pionMass)   -
				sqrt(mDaughterMoment.mag2() + muonMass*muonMass) - 
				pMomMinusdMom.mag());  
  //K-/+=>pi+/- + pi0 
  if ((deltaKaonPion < deltaKaonMuon) && (deltaKaonPion < deltaPionMuon) ){
      Float_t asinArg = (mDaughterMoment.mag() / kaonToPionQ) * sin(mDecayAngle*degree);//sin(theta_cm)
      if (fabs(asinArg) < 1. ) {
	  kinkVertex->setDecayAngleCM(float((1./degree) * asin(asinArg)));
	} 
      else {
	  kinkVertex->setDecayAngleCM(999.) ;
	}
      if( myParentGeometry11->charge() > 0 ){ //parent K+
	  kinkVertex->setGeantIdDaughter(8);//daughter is pi+
	  kinkVertex->setGeantIdParent(11);
	}
      else {// parent K-
      	  kinkVertex->setGeantIdDaughter(9);//daughter pi-
	  kinkVertex->setGeantIdParent(12);
	}
    } //K-/+=>mu+/- + (anti)neutrino
  else if ((deltaKaonMuon < deltaKaonPion) && (deltaKaonMuon < deltaPionMuon) ) {
      Float_t asinArg = (mDaughterMoment.mag() / kaonToMuonQ) * sin(mDecayAngle*degree);
      if (fabs(asinArg) < 1. ) {
	  kinkVertex->setDecayAngleCM( (1./degree) * asin(asinArg) );
	} 
      else {
	  kinkVertex->setDecayAngleCM( 999.) ;
	}
      if( myParentGeometry11->charge() > 0 ){ //parent K+
	  kinkVertex->setGeantIdDaughter(5);//daughter mu +
	  kinkVertex->setGeantIdParent(11);
	} 
      else {//parent K-
	  kinkVertex->setGeantIdDaughter(6);//daughter mu-
	  kinkVertex->setGeantIdParent(12);
	}   
    } else { //pi+/-=>mu+/- + (anti)neutrino
	Float_t asinArg = (mDaughterMoment.mag() / pionToMuonQ) * sin(mDecayAngle*degree);
	if( fabs(asinArg) < 1. ) {
            kinkVertex->setDecayAngleCM( (1./degree) * asin(asinArg) );  
	  } 
	else {
            kinkVertex->setDecayAngleCM( 999.) ;
	  }
	if (myParentGeometry11->charge() > 0 ){ //parent pi+
	    kinkVertex->setGeantIdDaughter(5);//daughter mu+
	    kinkVertex->setGeantIdParent(8);
	  } 
	else {//parent pi-
	    kinkVertex->setGeantIdDaughter(6);//daughter mu-
	    kinkVertex->setGeantIdParent(9);
	  }   
      }
 
  kinkVertex->setDcaParentDaughter(mDca);
  kinkVertex->setDcaDaughterPrimaryVertex(mDaughterImpact);//dca daughter from event vertex
  kinkVertex->setDcaParentPrimaryVertex(mParentImpact);//dca parent from event vertex
  
  //distance last point parent - first pt daughter
  kinkVertex->setHitDistanceParentDaughter( (myDaughterGeometry1->origin() - myParentGeometry11->origin()).mag() );
	
  //distance last point parent primary vertex
  kinkVertex->setHitDistanceParentVertex(sqrt( pow(mKinkVertex.x() - myParentGeometry11->origin().x(), 2) +
			  pow(mKinkVertex.y() - myParentGeometry11->origin().y(), 2) +
			  pow(mKinkVertex.z() - myParentGeometry11->origin().z(), 2)) );

  //set dE-delta enrgy for different deay hypotheses
  kinkVertex->setdE(1,deltaKaonMuon);//dE for K->mu + (anti)neutrino
  kinkVertex->setdE(2,deltaKaonPion);//dE for K->pi+pi0
  kinkVertex->setdE(3,deltaPionMuon);//dE for pi->mu +(anti)neutrino
  
  //set momentum
  kinkVertex->setParentMomentum(mParentMoment);
  kinkVertex->setDaughterMomentum(mDaughterMoment);
  kinkVertex->setParent(mParentTrackCandidate);

  //set dcay angle	  
  kinkVertex->setDecayAngle(mDecayAngle);

  //add daughter
  kinkVertex->addDaughter(mDaughterTrackCandidate);
 
  //set kinkVertex position
  kinkVertex->setPosition(mKinkVertex);	
}


/*!
  Track acceptance filter
  fittingMethod() as defined in pams/global/inc/StTrackMethod.h 
  (values themselves are defined in pams/global/inc/StTrackDefinitions.h)
  while mUseTracker is defined as SetTrackerUsage(). Note that TPT
  tracks are set as kHelix3DIdentifier BUT, we also found that there are
  some cases (untraced) where the mEncodedMethod is reset to 0 (the only tracks
  having kHelix3DIdentifier being tracks with SVT points). So, we had to revert
  to a non-equality for safety purposes i.e. kTrackerUseTPT will consider any 
  tracks not being kITKalmanFitId (sad).
*/
/*!
  Julien : I change this on Jerome's request : now TPT tracks will only be kHelix3DIdentifier... for safety purposes too ;-))
*/
bool StKinkMaker::acceptTrack(StTrack *trk)
{
  //cout << "DEBUG Track [" << trk->fittingMethod() << "] [" << trk->flag() << "] FitIds [" 
  //     << kKalmanFitId << "] [" << kITKalmanFitId << "] selector [" << mUseTracker << "]" << endl;

  // cut on flag
  if (trk->flag() <= 0) return false;

  // on fittingMethod() 
  if  (  
       //( trk->fittingMethod() == kHelix3DIdentifier  && (mUseTracker == kTrackerUseTPT  || mUseTracker == kTrackerUseBOTH) ) ||
  	 ( trk->fittingMethod() != kITKalmanFitId  && (mUseTracker == kTrackerUseTPT  || mUseTracker == kTrackerUseBOTH) ) ||
  	 ( trk->fittingMethod() == kITKalmanFitId  && (mUseTracker == kTrackerUseITTF || mUseTracker == kTrackerUseBOTH) ) ){
    return true;
  } else {
    return false;
  }

}


/*!

  Sets the tracker track-selection flag according to convention defined 
  enumeration in StV0FinderMaker.h . One should not confuse this value
  with the value of the controlling parameter m_Mode setting calling
  this method as follow

  m_Mode=0 00 Leaves mUseTracker as default i.e. kTrackerUseBOTH (see ctor)
  m_Mode=1 01 sets mUseTracker to kTrackerUseTPT
  m_Mode=2 10 Sets mUseTracker to kTrackerUseITTF
  m_Mode=3 11 Sets mUseTracker to kTrackerUseBOTH (bitmask)

  See Init() for how this is set. The m_Mode convention would allow for 
  easier extension to yet another track-selection flags.
     
*/
void StKinkMaker::SetTrackerUsage(Int_t opt)
{
  mUseTracker=opt;
  gMessMgr->Info() << "StKinkMaker::SetTrackerUsage : Setting option to " << mUseTracker << endm;
}
		
void StKinkMaker::Crop(){
  // Loop over kinks and remove (makeZombie() )+ change the decay angle for those that have one daughter sharing two different parents
  gMessMgr->Info()<<"StKinkMaker::Crop() : Starting ...."<<endm;
  event = (StEvent *)GetInputDS("StEvent");
  StSPtrVecKinkVertex& kinkVertices = event->kinkVertices();
  StKinkVertex* kinkVtxPtr1 = 0;//new StKinkVertex();
  StKinkVertex* kinkVtxPtr2 = 0;// new StKinkVertex();
  StTrack* daughterTrk1 = 0;
  StTrack* daughterTrk2 = 0;
  int iKinks = kinkVertices.size();
  ///////////////////////////////////////////////////////////////////////
  // take pairs of consecutive kink vertices and check for same track key() for daugther
  for(Int_t ikv1=0; ikv1<iKinks-1; ikv1++) {//first kink vertex from teh container
    kinkVtxPtr1 = kinkVertices[ikv1];
    daughterTrk1=kinkVtxPtr1->daughter();
    //  gMessMgr->Info()<<"###########StKinkMaker:daughter1->key()"<<daughterTrk1->key()<<endm;                  
    for(Int_t ikv2=ikv1+1; ikv2<iKinks; ikv2++) {//next kink vertex in the container

      kinkVtxPtr2 = kinkVertices[ikv2]; 
      daughterTrk2=kinkVtxPtr2->daughter();
    //  gMessMgr->Info()<<"###########StKinkMaker: daughter2->key()"<<daughterTrk2->key()<<endm;  
      if(daughterTrk1->key() == daughterTrk2->key()) {
	float decAngKinkVtx1 = kinkVtxPtr1->decayAngle();
        float decAngKinkVtx2 = kinkVtxPtr2->decayAngle(); 
        kinkVtxPtr1->setDecayAngle(decAngKinkVtx1*100+99);
	kinkVtxPtr2->setDecayAngle(decAngKinkVtx2*100+99);        
      }

    }//second vertex loop
  }//first vtx loop

  ///////////////////////////////////////////////////////////////////////
  // make Zombie the kink vertices with problems
 //  for(int ikv=iKinks-1;ikv>=0;ikv--){
//     kinkVertex = kinkVertices[ikv];
//     if( (kinkVertex->geantIdDaughter()%100) == 99){

//       kinkVertex->makeZombie();
//       //   gMessMgr->Info()<<"###########StKinkMaker: AFTER "<<kinkVertex->decayAngle()%100<<endm;  
//       iKinks--;
//     }
//   }

//   gMessMgr->Info()<<"StKinkMaker::Crop() : saving "<< iKinks << " Kink Candidates" <<endm;


}
