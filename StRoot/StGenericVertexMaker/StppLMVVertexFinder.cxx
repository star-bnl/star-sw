/************************************************************
 *
 * $Id: StppLMVVertexFinder.cxx,v 1.9 2004/08/18 20:08:04 balewski Exp $
 *
 * Author: Jan Balewski
 ************************************************************
 *
 * Description: 
 *
 ************************************************************/

#include <StEventTypes.h>
#include <StEnumerations.h>
#include <StGlobals.hh>
#include <SystemOfUnits.h>
#include <StMessMgr.h>
#include <cmath>
#include "math_constants.h"

#include "tables/St_g2t_vertex_Table.h" // tmp for Dz(vertex)
#include "StMaker.h"

#include "StppLMVVertexFinder.h"
#include "StGenericVertexMaker.h"

#if 1
#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)
#endif

StppLMVVertexFinder::StppLMVVertexFinder() {
  gMessMgr->Info() << "StppLMVVertexFinder::StppLMVVertexFinder is in use." << endm;

    mBeamHelix=0;
    mExternalSeedPresent = false;
    mVertexConstrain = false;
    mRequireCTB = false;
    mUseITTF = false;
    mTotEve=0;
    mdxdz=mdydz=mX0=mY0=0;


    //jan default cuts
    mMaxTrkDcaRxy=2.0;// was 3.9
    mMinTrkPt=0.2; // was 0.2
    mMinNumberOfFitPointsOnTrack = 15; // was 10 
    mMaxZrange=70; // for tracks
    mDVtxMax=4.0;  // max sigma multipl between tracks and current vertex, used for tracks rejection
    mMinMatchTr=2; // minimal # of tracks matched to CTB // was 1
    mBLequivNtr=20; // equivalent # of tracks for BeamLine
    mMatchCtbMax_eta=mCtbEtaSeg/2.+0.02;
    mMatchCtbMax_phi=mCtbPhiSeg/2.+C_PI*0./180.;

}

//==========================================================
//==========================================================
StppLMVVertexFinder::~StppLMVVertexFinder() {
   delete mBeamHelix;mBeamHelix=0;
}

//==========================================================
//==========================================================
bool StppLMVVertexFinder::fit(StEvent* event) {
  //
  //  Reset vertex
  //
  mFitError = mFitResult = StThreeVectorD(777,888,999);
  mCtbHits.clear();
  mPrimCand.clear();

  n1=0,n2=0,n3=0, n4=0,n5=0,n6=0;
  mStatus = -888;

  mTotEve++;
  eveID=event->id();

  // Get BField from gufld(,) 
  float x[3] = {0,0,0};
  float b[3];
  gufld(x,b);
  mBfield=  0.1*b[2]; //This is now Tesla.
  
  //printf("mBfield=%f tesla=%e b2=%f\n",mBfield,tesla,b[2]);
  gMessMgr->Info() << "ppLMV5:: mBfield/Tesla=" << mBfield << " b2=" << b[2] << endm;
 
  setFlagBase(); // what is that ? JB
  changeCuts();

  gMessMgr->Message("","I") << "ppLMV5::cuts"
			    <<"\n CtbThres_ch (real)="<<mCtbThres_ch
    			    <<"\n CtbThres_mev (M-C)="<<mCtbThres_mev
			    <<"\n MinNumberOfFitPointsOnTrack="<<mMinNumberOfFitPointsOnTrack
			    <<"\n MaxTrkDcaRxy/cm="<<mMaxTrkDcaRxy
			    <<"\n MinTrkPt GeV/c ="<<mMinTrkPt
			    <<"\n MatchCtbMax_eta="<<mMatchCtbMax_eta
			    <<"\n MatchCtbMax_phi/deg="<<mMatchCtbMax_phi/3.1416*180
    			    <<"\n BeamLequivNtr="<<mBLequivNtr
    			    <<"\n DVtxMax (dz/sig)="<<mDVtxMax
    			    <<"\n MinMatchTr ="<< mMinMatchTr
    			    <<"\n MaxZrange (cm) ="<< mMaxZrange
    			    <<"\n RequireCTB="<<mRequireCTB
    			    <<"\n VertexConstrain="<<mVertexConstrain
    //			    <<"\n  ="<<
			    <<endm; 
  
  if(!mRequireCTB){
    gMessMgr->Warning() << "StppLMVVertexFinder::fit() does not work w/o RequireCTB=true, sorry , quit" << endm;
    return false;
  }   

  // get CTB info, does not  work for embeding 
  if(event->runId()<100000){
    St_DataSet *gds=mDumMaker->GetDataSet("geant");
    collectCTBhitsMC(gds);  // use M-C
  }  else {
    StTriggerData *trgD=event->triggerData ();
    collectCTBhitsData(trgD); // use real data
  }

  if(mCtbHits.size()<=0){
    gMessMgr->Warning() << "StppLMVVertexFinder::fit() no valid CTB hits found, quit" << endm;
    return false;
  }   
   
  //  Loop all global tracks (TPC) and store the
  //  refering helices and their estimated DCA
  //  resolution in  vectors.
    
  StSPtrVecTrackNode& nodes = event->trackNodes();
  for (unsigned int k=0; k<nodes.size(); k++) {
    StTrack* g = nodes[k]->track(global);
    float sigma=0;
    if( !matchTrack2CTB(g,sigma)) continue;;	
    JHelix x;
    x.helix=g->geometry()->helix();
    x.sigma=sigma;
    mPrimCand.push_back(x);
    // printf("added tr %d w/ sigma=%f\n",mPrimCand.size(),sigma);
         
  } // end of track selection

  //printf(", now n1=%d n2=%d n3=%d n4=%d  n6=%d\n",n1,n2,n3,n4,n6);
  gMessMgr->Debug() << ", now n1=" << n1 << " n2=" << n2 << " n3=" << n3 << " n4=" << n4 << "n6=" << n6 << endm;


  //
  //  In case there are no tracks left we better quit
  //
  if (mPrimCand.size() < mMinMatchTr){
    gMessMgr->Warning() << "StppLMVVertexFinder::fit() only "<< mPrimCand.size() << "tracks to fit - too few, quit ppLMV" << endm;
    return false;
  }
  gMessMgr->Info() << "StppLMVVertexFinder::fit() size of helix vector: " << mPrimCand.size() << endm;
 
  // use beam line constraint or not
  if(mVertexConstrain ) {

    float sigMin=100000; // pick large weight for this track 
    for( uint j=0;j<mPrimCand.size();j++) {
      float sig=mPrimCand[j].sigma;
      if(sigMin>sig) sigMin=sig;
    }
    float sigma=sigMin/::sqrt(mBLequivNtr); //<== assigne relative weight
    JHelix x;
    x.helix=*mBeamHelix;
    x.sigma=sigma;
    mPrimCand.push_back(x);
    //printf("WARN ppLMV: nominal beam line added with sigma=%f, now nTrack=%d \n",sigma, mPrimCand.size());
    gMessMgr->Warning() << "ppLMV: nominal beam line added with sigma=" << sigma 
			<< ", now nTrack=" << mPrimCand.size() << endm;

  } // end of beamLine


  //printf("PrimCand  before ppLMV for eveID=%d tracks at first point\n",eveID);
  gMessMgr->Debug() << "PrimCand  before ppLMV for eveID=" << eveID << "tracks at first point" << endm;
  for( uint j=0;j<mPrimCand.size();j++) {
    StThreeVectorD p=mPrimCand[j].helix.momentum(mBfield*tesla);
    printf("j=%d  sig=%f pT=%f eta=%f phi/deg=%f\n",j,mPrimCand[j].sigma,p.perp(),p.pseudoRapidity(), p.phi()/C_PI*180);
    
  }

  //  ----------  D O   F I N D    V E R T E X
  int ret=ppLMV5();
  if(ret==false) {
    gMessMgr->Debug() << "ppLMV did not found vertex"<< endm;
    return false;
  }
  
  //printf("Prim tr used by ppLMV for nTotEve=%d, tracks at vertex\n",mTotEve);
  gMessMgr->Info() << "Prim tr used by ppLMV for eveID=" << eveID 
		    << ", tracks at vertex" << endm;

  for( uint j=0;j<mPrimCand.size();j++) {
    double spath = mPrimCand[j].helix.pathLength(mFitResult.x(),mFitResult.y());
    StThreeVectorD p=mPrimCand[j].helix.momentumAt(spath,mBfield*tesla);
    printf("j=%d  sig=%f pT=%f eta=%f phi/deg=%f cur=%f p=%f charg=%d spath=%f\n",j,mPrimCand[j].sigma,p.perp(),p.pseudoRapidity(), p.phi()/C_PI*180,mPrimCand[j].helix.curvature(), p.mag(),mPrimCand[j].helix.charge(mBfield*tesla),spath);
  }
	 
  gMessMgr->Message("","I") << "Prim ppLMV Vertex at " <<  mFitResult<<endm;
  return true;
} 


//======================================================
//======================================================
void
StppLMVVertexFinder::printInfo(ostream& os) const
{
    os << "StppLMVVertexFinder - Fit Statistics:" << endl;
    os << "fitted vertex ........................ " << mFitResult << endl;
    os << "position errors ...................... " << mFitError << endl;
    os << "# of used tracks + beamL.............. " << mPrimCand.size() << endl;
    os << "min # of fit points for tracks ....... " << mMinNumberOfFitPointsOnTrack << endl;
}


//======================================================
//======================================================
void StppLMVVertexFinder::UseVertexConstraint(double x0, double y0, double dxdz, double dydz, double weight) {
  mVertexConstrain = true;
  mX0 = x0;
  mY0 = y0;
  mdxdz = dxdz;
  mdydz = dydz;
  mWeight = weight;
  gMessMgr->Info() << "StppLMVVertexFinder::Using Constrained Vertex" << endm;
  gMessMgr->Info() << "x origin = " << mX0 << endm;
  gMessMgr->Info() << "y origin = " << mY0 << endm;
  gMessMgr->Info() << "slope dxdz = " << mdxdz << endm;
  gMessMgr->Info() << "slope dydz = " << mdydz << endm;
  gMessMgr->Info() << "NOT used (JB) weight in fit = " << weight <<  endm;
  StThreeVectorD origin(mX0,mY0,0.0);
  double pt  = 88889999;   
  double nxy=::sqrt(mdxdz*mdxdz +  mdydz*mdydz);
    if(nxy<1.e-5){ // beam line _MUST_ be tilted
      gMessMgr->Warning() << "StppLMVVertexFinder:: Beam line must be tilted!" << endm;
      nxy=mdxdz=1.e-5; 
    }
    double p0=pt/nxy;  
    double px   = p0*mdxdz;
    double py   = p0*mdydz;
    double pz   = p0; // approximation: nx,ny<<0
    StThreeVectorD MomFstPt(px*GeV, py*GeV, pz*GeV);
    delete mBeamHelix;
    mBeamHelix = new StPhysicalHelixD(MomFstPt,origin,0.5*tesla,1.);
    mExternalSeedPresent = false;
}




//==========================================================
//==========================================================
bool StppLMVVertexFinder::matchTrack2CTB (StTrack* track, float & sigma) {
  /* upgrade:
     - used dE/dX for strag
     - use track length  
     - make clear()
     - check for total energy per slat for  geant
  */

  sigma=0;
  const double Rctb=213.6; // (cm) radius of the CTB 
  uint nPoss=0, nFitP=0, nSvtP=0;

  if (!track) return false; // it should never happen
  if(!finite(track->geometry()->helix().curvature())){
    gMessMgr->Warning() << "StppLMVVertexFinder::matchTrack2CTB: helix.curvature not finite, fix tracker, ppLMV aborting" << endm;
    mCtbHits.clear();
    mPrimCand.clear();    
    return false;
  }

  if( track->flag()<0) return false;
  if( track->topologyMap().trackFtpc() )return false;
  if( mUseITTF && track->fittingMethod()!=kITKalmanFitId ) return false;
  if( !mUseITTF && track->fittingMethod()==kITKalmanFitId ) return false;

  n1++;	

  StPhysicalHelixD TrkHlxIn=track->geometry()->helix();

  //           check Rxy_min condition  close to beam    
  double spath = TrkHlxIn.pathLength(mX0, mY0 );
  StThreeVectorD posDCA = TrkHlxIn.at(spath);
  //  cout<<" DCA Position: "<<posDCA<<endl;
  double x_m = posDCA.x(), y_m = posDCA.y();
  double dmin = ::sqrt(x_m*x_m + y_m*y_m);
  if( dmin > mMaxTrkDcaRxy ) return false;
  if (fabs(posDCA.z()) >mMaxZrange) return false;
  n2++;
  
  nFitP = track->detectorInfo()->numberOfPoints(kTpcId);
  nSvtP = track->detectorInfo()->numberOfPoints(kSvtId);
  nPoss=track->numberOfPossiblePoints(kTpcId);

  if(  nFitP <= mMinNumberOfFitPointsOnTrack) return false;
  n3++;

  if(track->geometry()->momentum().perp() <mMinTrkPt ) return false;
  n4++;

  //  cout<<"\n\n DCA to beam at 0x0: "<<posDCA<<endl;
       
  //Find momentum direction at vertex point
  StThreeVectorD pmom = TrkHlxIn.momentumAt(spath,mBfield*tesla );
  double beta = pmom.mag()/::sqrt(pmom.mag()*pmom.mag()+0.139*0.139); //Assume pion 

  // old formula from TPT chain
  // float strag=0.0136/beta/pmom.mag()*fabs(spath); 

  // ppLMV face-lift : attenuated weights 
  float pmomM=pmom.mag();
  if (pmomM >4 )pmomM=4; //inhibit domination of high pT tracks
  float spathL=fabs(spath); // reduce advantage of SVT matched tracks
  if(spathL<40) spathL=40;
  float strag=0.0136/beta/pmomM*spathL; 
  if(fabs(mBfield)<0.01) strag=0.0136*spathL; // no field case, pT makes no sense

  //  printf("stragling=%f %f p=%f %f nFp=%d nPp=%d\n",strag,beta,pmom.mag(),spath, track->fitTraits().numberOfFitPoints(),nPoss );
  

  if ( !track->outerGeometry() )  return false;
  StPhysicalHelixD TrkHlxOut=track->outerGeometry()->helix();
  pairD  d2 = TrkHlxOut.pathLength(Rctb);
  //  printf(" path 1=%f, 2=%f, period=%f, R=%f\n",d2.first ,d2.second,TrkHlx.period(),1./TrkHlxOut.curvature());
  
  // assert(d2.first<0); // propagate backwards
  //assert(d2.second>0); // propagate forwards
  
  if(d2.first>=0 || d2.second<=0) {
    n5++;
    gMessMgr->Message("","W") << "ppLMV::matchTrack2CTB ()"  
      " unexpected solution for track crossing CTB, track="<<track <<endm;
    return false;
  }
  
  
  StThreeVectorD posCTB = TrkHlxOut.at(d2.second);
  //  double xmagn = ::sqrt( posCTB.x()*posCTB.x() + posCTB.y()*posCTB.y() );//tmp, out
  // printf(" punch2 x,y,z=%.1f, %.1f, %.1f, Rxy=%.1f\n",posCTB.x(),posCTB.y(),posCTB.z(),xmagn);
  
  float phi=atan2(posCTB.y(),posCTB.x());
  if(phi<0) phi+=2*C_PI;// now phi is [0,2Pi] as for CTB slats
  
  // printf("posCTB.z()=%f posDCA.z()=%f\n",posCTB.z(),posDCA.z());

  uint ih;
  for(ih=0;ih<mCtbHits.size();ih++) {// loop over CTB hits
 
   // match to CTB slats in phi
    float del_phi=phi-mCtbHits[ih].phi;
    if(del_phi>C_PI) del_phi-=2*C_PI;
    if(del_phi<-C_PI) del_phi+=2*C_PI;
    //printf("phiRad trk=%f  CTB=%f del=%f\n",phi,mCtbHits[ih].phi,del_phi);
    //printf("match ih=%d del_phi=%f/deg\n",ih, del_phi/C_PI*180);
    if(fabs(del_phi) >mMatchCtbMax_phi) continue;
    
    // match to CTB slats in eta
    float eta=posCTB.pseudoRapidity();
    float del_eta=eta-mCtbHits[ih].eta;
    //printf("  match ih=%d del_eta=%f\n",ih, del_eta);
    if(fabs(del_eta) >mMatchCtbMax_eta) continue;
    
    // printf("  CTB match OK:  del_eta=%.2f, del_phi/deg=%.1f \n", del_eta,del_phi/C_PI*180);
    sigma=strag;
    n6++;    
    printf("add tr %d w/ sigma=%f p/GeV=%f spath/cm=%f nFitP=%d nPoss=%d nSVT=%d\n",mPrimCand.size(),sigma,pmom.mag(),spath,nFitP, nPoss,nSvtP);
    
    return true;
  }
  
  // match not found
  return false;
}

//==========================================================
//==========================================================
bool  StppLMVVertexFinder::ppLMV5() { 
  //  ----------  D O   F I N D    V E R T E X

  int totTr=mPrimCand.size();
  uint minTr=mMinMatchTr;
  if(mVertexConstrain) minTr++;
  //printf("passed %d tracks match to CTB,  BeamLine=%d\n",totTr,mVertexConstrain );
  gMessMgr->Debug() << "passed " << totTr << " tracks match to CTB,  BeamLine=" << mVertexConstrain << endm;
   
  double xo=0.0,yo=0.0;
  xo=mX0;
  yo=mY0; 
 
  //Do the actual vertex fitting, continue until good
  double A11=0.0,A12=0.0,A13=0.0,A21=0.0,A22=0.0,A23=0.0;
  double A31=0.0,A32=0.0,A33=0.0; // Matrix Elements
  double C11=0.0,C12=0.0,C13=0.0,C21=0.0,C22=0.0,C23=0.0;
  double C31=0.0,C32=0.0,C33=0.0; // C = A^-1
  int done = 0;
  int vertIter=0;
  double chi2=0;
  StThreeVectorD XVertex(999.,888.,777.);
  while( done != 1 ){
    vertIter++;
    // Check that there at least are 2 tracks
    if( mPrimCand.size() < minTr ){
      gMessMgr->Warning()  << "ppLMV5: below  "<<minTr<<" track remains. No vertex found." << endm;
      return false;
    }
  
    // Begin by doing a fit
    A11=0.0,A12=0.0,A13=0.0,A21=0.0,A22=0.0,A23=0.0;
    A31=0.0,A32=0.0,A33=0.0; // Matrix Elements
    C11=0.0,C12=0.0,C13=0.0,C21=0.0,C22=0.0,C23=0.0;
    C31=0.0,C32=0.0,C33=0.0; // C = A^-1
    double b1=0.0,b2=0.0,b3=0.0;

    // Compute matrix A and vector b
    for(unsigned int itr=0; itr < mPrimCand.size(); itr++){ 

      double spath = mPrimCand[itr].helix.pathLength(xo,yo);
      StThreeVectorD XClosest =  mPrimCand[itr].helix.at(spath);
      StThreeVectorD XMomAtClosest =  mPrimCand[itr].helix.momentumAt(spath,mBfield*tesla );
      double xp   = XClosest.x(); double yp= XClosest.y(); double zp= XClosest.z();  
      // printf("itr=%d  DCA x=%f y=%f z=%f  sig=%f\n",itr,xp,yp,zp,mPrimCand[itr].sigma);
      
      double xhat = XMomAtClosest.x()/XMomAtClosest.mag();
      double yhat = XMomAtClosest.y()/XMomAtClosest.mag();
      double zhat = XMomAtClosest.z()/XMomAtClosest.mag();
      float sig=mPrimCand[itr].sigma;
      A11=A11+(yhat*yhat+zhat*zhat)/sig;
      A12=A12-(xhat*yhat)/sig;
      A13=A13-(xhat*zhat)/sig;
      A22=A22+(xhat*xhat+zhat*zhat)/sig;
      A23=A23-(yhat*zhat)/sig;
      A33=A33+(xhat*xhat+yhat*yhat)/sig;
      b1=b1 + ( (yhat*yhat+zhat*zhat)*xp - xhat*yhat*yp - xhat*zhat*zp )/sig;
      b2=b2 + ( (xhat*xhat+zhat*zhat)*yp - xhat*yhat*xp - yhat*zhat*zp )/sig;
      b3=b3 + ( (xhat*xhat+yhat*yhat)*zp - xhat*zhat*xp - yhat*zhat*yp )/sig;
    }
    A21 = A12; A31=A13; A32=A23;

    // Invert A
    double detA =   A11*A22*A33 + A12*A23*A31 + A13*A21*A32;
    detA = detA   - A31*A22*A13 - A32*A23*A11 - A33*A21*A12;
    //    cout<<"Determinant= "<<detA<<endl;
    //    cout<<"A11,A12,A13: "<<A11<<" "<<A12<<" "<<A13<<endl;
    //    cout<<"A21,A22,A23: "<<A21<<" "<<A22<<" "<<A23<<endl;
    //    cout<<"A31,A32,A33: "<<A31<<" "<<A32<<" "<<A33<<endl;
    //    cout<<"b1,b2,b3 "<<b1<<" "<<b2<<" "<<b3<<endl;
    C11=(A22*A33-A23*A32)/detA; C12=(A13*A32-A12*A33)/detA; C13=(A12*A23-A13*A22)/detA;
    C21=C12;                    C22=(A11*A33-A13*A31)/detA; C23=(A13*A21-A11*A23)/detA;
    C31=C13;                    C32=C23;                    C33=(A11*A22-A12*A21)/detA;

    // Find Vertex Position
    double Xv = C11*b1 + C12*b2 + C13*b3;
    double Yv = C21*b1 + C22*b2 + C23*b3;
    double Zv = C31*b1 + C32*b2 + C33*b3;
    XVertex.setX(Xv); XVertex.setY(Yv); XVertex.setZ(Zv);
    gMessMgr->Debug() <<vertIter<<"  Vertex Position   : "<<XVertex.x()<<" "<<XVertex.y()<<" "<<XVertex.z()<<endm;
    gMessMgr->Debug() <<"Error in Position : "<<::sqrt(C11)<<" "<<::sqrt(C22)<<" "<<::sqrt(C33)<<endm;
    

    // Check if the fit is any good
    // Loop over tracks again to get Chi2 and check each track's deviation
    
    double dmax=0.0;
    vector<JHelix>::iterator itehlx, end, ikeep;
    end=mPrimCand.end();
    if(mVertexConstrain )end--; // to preserve beamL

    for( itehlx=mPrimCand.begin(); itehlx <end;  itehlx++) {
      StPhysicalHelixD hlx = (*itehlx).helix;
      double sig = (*itehlx).sigma;
      double spath = hlx.pathLength(XVertex.x(),XVertex.y()); 
      StThreeVectorD XHel = hlx.at(spath);
      double d=(XHel.x()-XVertex.x())*(XHel.x()-XVertex.x());
      d = d+(XHel.y()-XVertex.y())*(XHel.y()-XVertex.y());
      d = d+(XHel.z()-XVertex.z())*(XHel.z()-XVertex.z());
      d = ::sqrt(d);
      chi2 = chi2 + (d*d)/(sig*sig);
      double drel = d;  // do not use sig during track rejection ??? JB
      printf(" DCA x=%f y=%f z=%f d=%f drel=%f dmax=%f sig=%f\n",XHel.x(),XHel.y(),XHel.z(),d,drel,dmax,sig);
      if( drel > dmax ){// save  track that deviates the most from vertex to be discarded later
        dmax = drel;
        ikeep = itehlx;
      }
    }

    if( dmax > mDVtxMax ){
      gMessMgr->Info() << "Removing a track! dmax=" << dmax << endm;
      mPrimCand.erase(ikeep);
      done=0;
    }
    else{
      done=1;
    }
  } // End While Loop
  
  //  double  chi2pdof = chi2/(mPrimCand.size()-1);

  mFitResult=XVertex;
  mFitError.setX(sqrt(C11));   mFitError.setY(sqrt(C22));   mFitError.setZ(sqrt(C33));

  gMessMgr->Debug() <<"ppLMV5: Primary Vertex found!\nVert position: "<<XVertex<<", accepted tracks "<<mPrimCand.size()<<" of "<<totTr<<" eveID="<<eveID<<endm;
  printf("##V %6d %d %f %f %f    %d %d %d\n",eveID,mTotEve,XVertex.x(),XVertex.y(),XVertex.z(),NCtbSlats(),n1,NCtbMatches());
      

  // get geant vertex
  St_DataSet *gds=mDumMaker->GetDataSet("geant");
  if(gds) {
    St_g2t_vertex  *g2t_ver=( St_g2t_vertex *)gds->Find("g2t_vertex");
    if(g2t_ver) {
      // --------------  A C C E S S    G E A N T   V E R T E X  (for histo)
      g2t_vertex_st *GVER= g2t_ver->GetTable();
      //  printf("GVER add=%d\n",GVER);
      // hPiFi[10]->Fill(GVER->ge_x[2]-rZver);
      // hPiFi[11]->Fill(GVER->ge_x[0]-rXver);
      // hPiFi[12]->Fill(GVER->ge_x[1]-rYver);
      printf("Z Geant-found=%.2f, dx=%.2f, dy=%.2f nCtbSl=%d n1=%d eveID=%d\n",GVER->ge_x[2]-XVertex.z(),GVER->ge_x[0]-XVertex.x(),GVER->ge_x[1]-XVertex.y(),NCtbSlats(),n1,eveID);
      printf("##G %6d %d %f %f    %d %d %d\n",eveID,mTotEve,GVER->ge_x[2],XVertex.z(),NCtbSlats(),n1,NCtbMatches());
      
    }
  }
  
  gMessMgr->Debug() << "StppLMVVertexFinder::ppLMV5() ends" << endm;
  return true;
}


//==========================================================
//==========================================================
int  StppLMVVertexFinder::NCtbMatches() { 
  int nTr=mPrimCand.size();
  if(mVertexConstrain ) nTr--; // drop beam line
  return nTr;
}

//==========================================================
//==========================================================
int  StppLMVVertexFinder::NCtbSlats() { 
  return mCtbHits.size();
}


//==========================================================
//==========================================================
void  StppLMVVertexFinder::changeCuts(){
  StGenericVertexMaker *mk=(StGenericVertexMaker *)mDumMaker->GetMaker("GenericVertex");
  int mode2=mk->GetMode2();

  printf("ccc m_mode2=%d\n",mode2);
  switch(mode2) {
  case 'a': mMaxTrkDcaRxy=1.5; break;
  case 'b': mMaxTrkDcaRxy=2.5; break;
  case 'c':mMinTrkPt=0.3; break;
  case 'd':mMinTrkPt=0.4; break;
  case 'e':mMinNumberOfFitPointsOnTrack =10;break; // do nothing
  case 'f': mMinNumberOfFitPointsOnTrack =20; break; // do nothing
  case 'g':  mMatchCtbMax_phi=mCtbPhiSeg/2.+C_PI*0.5/180.;break; // do nothing
  case 'h': mMatchCtbMax_phi=mCtbPhiSeg/2.+C_PI*1./180.;break; break; // do nothing
  case 'i':mMaxZrange=150; break; // do nothing
  case 'j':mMaxZrange=50; break; // do nothing
  default: break; // do nothing

  }

  

}

/*
 * $Log: StppLMVVertexFinder.cxx,v $
 * Revision 1.9  2004/08/18 20:08:04  balewski
 * outerGeometry()->helix() used by ppLMV for extrapolation to CTB
 *
 * Revision 1.8  2004/08/06 21:00:01  balewski
 * now should be fine for 2004 pp200 data,
 * high pT & small path cat-off included
 *  still no nFit/nPoss cut
 *
 * Revision 1.7  2004/08/06 04:49:14  balewski
 * that would be it
 *
 * Revision 1.6  2004/08/05 22:08:04  balewski
 * toward working point
 *
 * Revision 1.5  2004/08/04 21:57:56  balewski
 * toward smarter ppLMV5
 *
 * Revision 1.4  2004/07/29 00:44:30  balewski
 * ppLMV() returns false on error
 *
 * Revision 1.3  2004/07/24 02:57:40  balewski
 * clean up of ppLMV, CTB-util separated
 *
 * Revision 1.2  2004/07/23 01:01:33  jeromel
 * See .h + changed printf() / cout to gMessMgr + removed a ew asserts
 *
 * Revision 1.1  2004/07/21 01:53:18  balewski
 * first
 *
 **************************************************************************/

