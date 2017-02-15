/************************************************************
 *
 * $Id: StppLMVVertexFinder.cxx,v 1.32 2017/02/15 15:30:18 smirnovd Exp $
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

#include "tables/St_g2t_vertex_Table.h" // tmp for Dz(vertex)
#include "StMaker.h"

#include "StppLMVVertexFinder.h"
#include "StGenericVertexMaker.h"

//==========================================================
//==========================================================
StppLMVVertexFinder::StppLMVVertexFinder() {
  LOG_INFO << "StppLMVVertexFinder::StppLMVVertexFinder is in use." << endm;
  mBeamHelix           = 0;
  mVertexConstrain     = false;
  mTotEve              = 0;
  mMode                = 1;
}

//==========================================================
//==========================================================
void 
StppLMVVertexFinder::Init() {

  //jan default cuts
  if (mMode==0){
    LOG_INFO << "The  ppLMV4 cuts have been activated" << endm; 
    mMaxTrkDcaRxy    = 3.9;
    mMinTrkPt        = 0.2;
    mMinNumberOfFitPointsOnTrack = 15; // a typo (=10) was here before , JB
    mMaxZrange       = 180;            // for tracks;  typo (=250) was here before , JB
    mDVtxMax         = 4.0;            // max sigma multipl between tracks and current vertex, used for tracks rejection
    mMinMatchTr      = 1;              // minimal # of tracks matched to CTB 
    mBLequivNtr      = 100;            // equivalent # of tracks for BeamLine
    mMatchCtbMax_eta = mCtbEtaSeg/2.+0.02;
    mMatchCtbMax_phi = mCtbPhiSeg/2.+M_PI*1./180.;
  } else {
    LOG_INFO << "The  ppLMV5 cuts have been activated" << endm; 
    mMaxTrkDcaRxy    = 2.0;            
    mMinTrkPt        = 0.2;            
    mMinNumberOfFitPointsOnTrack = 15; 
    mMaxZrange       = 150;            
    mDVtxMax         = 4.0;            
    mMinMatchTr      = 2;              
    mBLequivNtr      = 20;             
    mMatchCtbMax_eta = mCtbEtaSeg/2.+0.02;
    mMatchCtbMax_phi = mCtbPhiSeg/2.+M_PI*0./180.;
  }
}

//==========================================================
//==========================================================
void 
StppLMVVertexFinder::Clear(){  //  Reset vertex
  StGenericVertexFinder::Clear();
  mCtbHits.clear();
  mPrimCand.clear();
  LOG_INFO << "StppLMVVertexFinder::Clear() ..." << endm;
}

//==========================================================
//==========================================================
void 
StppLMVVertexFinder::addFakeVerex(float z){
  StPrimaryVertex primV;
  Float_t cov[6] = {1,0.0,2,0.0,0.0,3  }; 
  StThreeVectorD XVertex(0.1,0.2,z);
  primV.setPosition(XVertex);
  primV.setCovariantMatrix(cov); 
  primV.setVertexFinderId(pplmvVertexFinder);
  primV.setFlag(1); 
  primV.setRanking(444);
  //..... add vertex to the list
  addVertex(primV);
}

//==========================================================
//==========================================================
StppLMVVertexFinder::~StppLMVVertexFinder() {
   delete mBeamHelix;mBeamHelix=0;
}

//==========================================================
//==========================================================
int 
StppLMVVertexFinder::fit(StEvent* event) {
  LOG_INFO << "StppLMVVertexFinder::fit() START ..." << endm;
 //

  n1=0,n2=0,n3=0, n4=0,n5=0,n6=0;
  //  mStatus = -888;

  mTotEve++;
  eveID=event->id();

  mBfield = event->runInfo()->magneticField(); 
  LOG_INFO << Form("ppLMV5:: mBfield[Tesla]=%e ",mBfield)<<endm;
  

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
    			    <<"\n VertexConstrain="<<mVertexConstrain
    //			    <<"\n  ="<<
			    <<endm; 
   
  // get CTB info, does not  work for embeding 
  if(event->runId()<100000){
    St_DataSet *gds=StMaker::GetChain()->GetDataSet("geant");
    collectCTBhitsMC(gds);  // use M-C
  }  else {
    StTriggerData *trgD=event->triggerData ();
    collectCTBhitsData(trgD); // use real data
  }
  // printCtb(); //4ppv

  if(mCtbHits.size()<=0){
    LOG_WARN << "StppLMVVertexFinder::fit() no valid CTB hits found, quit" << endm;
    return 0;
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

  //  printf(", now n1=%d n2=%d n3=%d n4=%d  n6=%d\n",n1,n2,n3,n4,n6); //tmp
  LOG_DEBUG << ", now n1=" << n1 << " n2=" << n2 << " n3=" << n3 << " n4=" << n4 << "n6=" << n6 << endm;


  //
  //  In case there are no tracks left we better quit
  //
  if (mPrimCand.size() < mMinMatchTr){
    LOG_WARN << "StppLMVVertexFinder::fit() only "<< mPrimCand.size() << "tracks to fit - too few, quit ppLMV" << endm;
    return 0;
  }
  LOG_INFO << "StppLMVVertexFinder::fit() size of helix vector: " << mPrimCand.size() << endm;
 
  // use beam line constraint or not
  if(mVertexConstrain ) {

    float sigMin=100000; // pick large weight for this track 
    for( unsigned int j=0;j<mPrimCand.size();j++) {
      float sig=mPrimCand[j].sigma;
      if(sigMin>sig) sigMin=sig;
    }
    float sigma=sigMin/::sqrt(mBLequivNtr); //<== assigne relative weight
    JHelix x;
    x.helix=*mBeamHelix;
    x.sigma=sigma;
    mPrimCand.push_back(x);
    //printf("WARN ppLMV: nominal beam line added with sigma=%f, now nTrack=%d \n",sigma, mPrimCand.size());
    LOG_WARN << "ppLMV: nominal beam line added with sigma=" << sigma 
			<< ", now nTrack=" << mPrimCand.size() << endm;

  } // end of beamLine


  //printf("PrimCand  before ppLMV for eveID=%d tracks at first point\n",eveID);
  LOG_DEBUG << "PrimCand  before ppLMV for eveID=" << eveID << "tracks at first point" << endm;

  //tmp - lits all matched tracks
  for( unsigned int j=0;j<mPrimCand.size();j++) {
    StThreeVectorD p=mPrimCand[j].helix.momentum(mBfield*tesla);
    printf("j=%d  sig=%f pT=%f eta=%f phi/deg=%f\n",j,mPrimCand[j].sigma,p.perp(),p.pseudoRapidity(), p.phi()/M_PI*180);
  } 

  //  ----------  D O   F I N D    V E R T E X
  int ret=ppLMV5();
  if(ret==false) {
    LOG_DEBUG << "ppLMV did not found vertex"<< endm;
    return 0;
  }
  

  LOG_INFO << "Prim tr used by ppLMV for eveID=" << eveID 
		   << ", tracks at vertex=" << mPrimCand.size()<<endm;

  assert(size());
  for( unsigned int j=0;j<mPrimCand.size();j++) {
    double spath = mPrimCand[j].helix.pathLength( getVertex(0)->position().x(),getVertex(0)->position().y());
    StThreeVectorD p=mPrimCand[j].helix.momentumAt(spath,mBfield*tesla);
    // printf("j=%d  sig=%f pT=%f eta=%f phi/deg=%f cur=%f p=%f charg=%d spath=%f\n",j,mPrimCand[j].sigma,p.perp(),p.pseudoRapidity(), p.phi()/M_PI*180,mPrimCand[j].helix.curvature(), p.mag(),mPrimCand[j].helix.charge(mBfield*tesla),spath);
  }
	 
  gMessMgr->Message("","I") << "Prim ppLMV Vertex at " << getVertex(0)->position()<<endm;
  return size();
} 


//======================================================
//======================================================
void
StppLMVVertexFinder::printInfo(ostream& os) const
{
    os << "StppLMVVertexFinder - Fit Statistics:" << endl;
    os << "found prim vertices ................ " <<size() << endl;
    os << "no more info printed at the moment, Jan"<<endl;
 }


//======================================================
//======================================================
void 
StppLMVVertexFinder::UseVertexConstraint() {
  mVertexConstrain = true;
  double mX0 = mBeamline.x0;
  double mY0 = mBeamline.y0;
  double mdxdz = mBeamline.dxdz;
  double mdydz = mBeamline.dydz;
  LOG_INFO << "StppLMVVertexFinder::Using Constrained Vertex" << endm;
  StThreeVectorD origin(mX0,mY0,0.0);
  double pt  = 88889999;   
  double nxy=::sqrt(mdxdz*mdxdz +  mdydz*mdydz);
  if(nxy<1.e-5){ // beam line _MUST_ be tilted
    LOG_WARN << "StppLMVVertexFinder:: Beam line must be tilted!" << endm;
    nxy=mdxdz=1.e-5; 
  }
  double p0=pt/nxy;  
  double px   = p0*mdxdz;
  double py   = p0*mdydz;
  double pz   = p0; // approximation: nx,ny<<0
  StThreeVectorD MomFstPt(px*GeV, py*GeV, pz*GeV);
  delete mBeamHelix;
  mBeamHelix = new StPhysicalHelixD(MomFstPt,origin,0.5*tesla,1.);
}




//==========================================================
//==========================================================
bool 
StppLMVVertexFinder::matchTrack2CTB (StTrack* track, float & sigma) {
  /* upgrade:
     - used dE/dX for strag
     - use track length  
     - make clear()
     - check for total energy per slat for  geant
  */

  sigma=0;
  const double Rctb=213.6; // (cm) radius of the CTB 
  unsigned int nFitP=0;

  if (!track) return false; // it should never happen
  if(!finite(track->geometry()->helix().curvature())){
    LOG_WARN << "StppLMVVertexFinder::matchTrack2CTB: helix.curvature not finite, fix tracker, ppLMV aborting" << endm;
    mCtbHits.clear();
    mPrimCand.clear();    
    return false;
  }

  if( track->flag()<0) return false;
  if( track->topologyMap().trackFtpc() )return false;

  n1++;	

  StPhysicalHelixD TrkHlxIn=track->geometry()->helix();

  //           check Rxy_min condition  close to beam    
  double spath = TrkHlxIn.pathLength(mBeamline.x0, mBeamline.y0);
  StThreeVectorD posDCA = TrkHlxIn.at(spath);
  //  cout<<" DCA Position: "<<posDCA<<endl;
  double x_m = posDCA.x(), y_m = posDCA.y();
  double dmin = ::sqrt(x_m*x_m + y_m*y_m);
  if( dmin > mMaxTrkDcaRxy ) return false;
  if (fabs(posDCA.z()) >mMaxZrange) return false;
  n2++;
  
  nFitP = track->detectorInfo()->numberOfPoints(kTpcId);

  if(  nFitP <= mMinNumberOfFitPointsOnTrack) return false;
  n3++;

  if(track->geometry()->momentum().perp() <mMinTrkPt ) return false;
  n4++;

  //cout<<"\n\n DCA to beam at xy=00: "<<posDCA<<endl;
       
  //Find momentum direction at vertex point
  StThreeVectorD pmom = TrkHlxIn.momentumAt(spath,mBfield*tesla );
  double beta = pmom.mag()/::sqrt(pmom.mag()*pmom.mag()+0.139*0.139); //Assume pion 

  // old formula from TPT chain
  // float strag=0.0136/beta/pmom.mag()*fabs(spath); 

  // ppLMV face-lift : attenuated weights 
  float pmomM=pmom.mag();
  float spathL=fabs(spath); // reduce advantage of SVT matched tracks

  if(mMode ==1){
    // This is mode 1 (ppLMV5)
    if (pmomM >4 ) pmomM=4; //inhibit domination of high pT tracks
    if( spathL<40) spathL=40;
  } else {
    // This is mode 0 (ppLMV4)
    // ignore SVT contribution , ~aproximately
    if( spathL<60) spathL=60;
  } 

  float strag=0.0136/beta/pmomM*spathL; 
  if(fabs(mBfield)<0.01) strag=0.0136*spathL; // no field case, pT makes no sense

  //  printf("stragling=%f %f p=%f %f nFp=%d nPp=%d\n",strag,beta,pmom.mag(),spath, track->fitTraits().numberOfFitPoints(), track->numberOfPossiblePoints(kTpcId) );
  

  if ( !track->outerGeometry() )  return false;
  StPhysicalHelixD TrkHlxOut=track->outerGeometry()->helix();
  pairD  d2 = TrkHlxOut.pathLength(Rctb);
  //  printf(" path 1=%f, 2=%f, period=%f, R=%f\n",d2.first ,d2.second,TrkHlx.period(),1./TrkHlxOut.curvature());
  
  // assert(d2.first<0); // propagate backwards
  //assert(d2.second>0); // propagate forwards
  
  if(d2.first>=0 || d2.second<=0) {
    n5++;
    gMessMgr->Message("","W") << "ppLMV::matchTrack2CTB ()"  
      " unexpected solution for track crossing CTB, TrkHlxOut="<< TrkHlxOut<<endm;
    return false;
  }
  
  
  StThreeVectorD posCTB = TrkHlxOut.at(d2.second);
  //  double xmagn = ::sqrt( posCTB.x()*posCTB.x() + posCTB.y()*posCTB.y() );//tmp, out
  // printf(" punch2 x,y,z=%.1f, %.1f, %.1f, Rxy=%.1f\n",posCTB.x(),posCTB.y(),posCTB.z(),xmagn);
  
  float phi=atan2(posCTB.y(),posCTB.x());
  if(phi<0) phi+=2*M_PI;// now phi is [0,2Pi] as for CTB slats
  
  // printf("posCTB.z()=%f posDCA.z()=%f\n",posCTB.z(),posDCA.z());

  unsigned int ih;
  for(ih=0;ih<mCtbHits.size();ih++) {// loop over CTB hits
 
   // match to CTB slats in phi
    float del_phi=phi-mCtbHits[ih].phi;
    if(del_phi>M_PI) del_phi-=2*M_PI;
    if(del_phi<-M_PI) del_phi+=2*M_PI;
    //printf("phiRad trk=%f  CTB=%f del=%f\n",phi,mCtbHits[ih].phi,del_phi);
    //printf("match ih=%d del_phi=%f/deg\n",ih, del_phi/M_PI*180);
    if(fabs(del_phi) >mMatchCtbMax_phi) continue;
    
    // match to CTB slats in eta
    float eta=posCTB.pseudoRapidity();
    float del_eta=eta-mCtbHits[ih].eta;
    //printf("  match ih=%d del_eta=%f\n",ih, del_eta);
    if(fabs(del_eta) >mMatchCtbMax_eta) continue;
    
    // printf("  CTB match OK:  del_eta=%.2f, del_phi/deg=%.1f \n", del_eta,del_phi/M_PI*180);
    sigma=strag;
    n6++;    
    //  printf("add tr %d w/ sigma=%f p/GeV=%f spath/cm=%f nFitP=%d nSVT=%d\n",mPrimCand.size(),sigma,pmom.mag(),spath,nFitP, track->detectorInfo()->numberOfPoints(kSvtId) );
    
    return true;
  }
  
  // match not found
  return false;
}

//==========================================================
//==========================================================
bool  
StppLMVVertexFinder::ppLMV5() { 
  //  ----------  D O   F I N D    V E R T E X
  if(mMode == 0) LOG_WARN<<"ppLMV4 cuts have been activated"<<endm; 

  int totTr=mPrimCand.size();
  unsigned int minTr=mMinMatchTr;
  if(mVertexConstrain) minTr++;
  //printf("passed %d tracks match to CTB,  BeamLine=%d\n",totTr,mVertexConstrain );
  LOG_DEBUG << "passed " << totTr << " tracks match to CTB,  BeamLine=" << mVertexConstrain << endm;
   
  double xo = mBeamline.x0;
  double yo = mBeamline.y0;
 
  //Do the actual vertex fitting, continue until good
  double A11=0.0,A12=0.0,A13=0.0;
  double A21=0.0,A22=0.0,A23=0.0;
  double A31=0.0,A32=0.0,A33=0.0; // Matrix Elements

  double C11=0.0,C12=0.0,C13=0.0;
  double C21=0.0,C22=0.0,C23=0.0;
  double C31=0.0,C32=0.0,C33=0.0; // C = A^-1

  int done = 0;
  int vertIter=0;
  double chi2=0;
  StThreeVectorD XVertex(999.,888.,777.);
  while( done != 1 ){
    vertIter++;
    // Check that there at least are 2 tracks
    if( mPrimCand.size() < minTr ){
      LOG_WARN  << "ppLMV5: below  "<<minTr<<" track remains. No vertex found." << endm;
      return false;
    }
  
    // Begin by doing a fit
    A11=0.0,A12=0.0,A13=0.0;
    A21=0.0,A22=0.0,A23=0.0;
    A31=0.0,A32=0.0,A33=0.0; // Matrix Elements

    C11=0.0,C12=0.0,C13=0.0;
    C21=0.0,C22=0.0,C23=0.0;
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
    LOG_DEBUG <<vertIter<<"  Vertex Position   : "<<XVertex.x()<<" "<<XVertex.y()<<" "<<XVertex.z()<<endm;
    LOG_DEBUG <<"Error in Position : "<<::sqrt(C11)<<" "<<::sqrt(C22)<<" "<<::sqrt(C33)<<endm;
    

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
      //printf(" DCA x=%f y=%f z=%f d=%f drel=%f dmax=%f sig=%f\n",XHel.x(),XHel.y(),XHel.z(),d,drel,dmax,sig);
      if( drel > dmax ){// save  track that deviates the most from vertex to be discarded later
        dmax = drel;
        ikeep = itehlx;
      }
    } 

    if( dmax > mDVtxMax ){
      LOG_INFO << "Removing a track! dmax=" << dmax << endm;
      mPrimCand.erase(ikeep);
      done=0;
    }
    else{
      done=1;
    }
  } // End While Loop
  
  //  double  chi2pdof = chi2/(mPrimCand.size()-1);

  StPrimaryVertex primV;                    //  Initial comment - cxx,?,cyy,?,?,czz
  Float_t cov[6] = {(Float_t) C11, 0.0,     //  m(1,1)          m(1,2)==m(2,1)
		    (Float_t) C22, 0.0,     //  m(2,2)          m(1,3)==m(3,1)
		    0.0, (Float_t) C33  };  //  m(2,3)==m(3,2)  m(3,3)
  
  primV.setPosition(XVertex);
  primV.setCovariantMatrix(cov); 
  primV.setVertexFinderId(pplmvVertexFinder);
  primV.setNumTracksUsedInFinder(mPrimCand.size());
  primV.setNumMatchesWithCTB(mPrimCand.size());// the same, only matched tracks are used

  /*  I do not understand if this elaborate scheme is applicable to multiple prim vertices for pp
      http://www.star.bnl.gov/STAR/html/all_l/html/dst_vertex_flags.html
      provide '1' as was
  */
  primV.setFlag(1); 
  primV.setRanking(555);

  //..... add vertex to the list
  addVertex(primV);

#if 0
  /*
    The fake vertex will never take precedence over the real one in 
    the muDst analysi since its rank is lower (if default ranking is used).
    But may pick a random match if luminosity (i.e. pileup is high).
    Unless the true vertex is not found. Then we would have a fake 
    vertex w/o prim tracks.
   Jan B.
  */
  if(eveID%2) addFakeVerex(XVertex.z()+20);
#endif

#if 0  ///old
  mFitResult=XVertex;
  mFitError.setX(sqrt(C11));   mFitError.setY(sqrt(C22));   mFitError.setZ(sqrt(C33));
#endif


  LOG_DEBUG <<"ppLMV5: Primary Vertex found!\nVert position: "<<XVertex<<", accepted tracks "<<mPrimCand.size()<<" of "<<totTr<<" eveID="<<eveID<<endm;
  printf("##V %6d %d %f %f %f    %d %d %d\n",eveID,mTotEve,XVertex.x(),XVertex.y(),XVertex.z(),mCtbHits.size(),n1,NCtbMatches());


  // get geant vertex
  St_DataSet *gds=StMaker::GetChain()->GetDataSet("geant");
  if(gds) {
    St_g2t_vertex  *g2t_ver=( St_g2t_vertex *)gds->Find("g2t_vertex");
    if(g2t_ver) {
      // --------------  A C C E S S    G E A N T   V E R T E X  (for histo)
      g2t_vertex_st *GVER= g2t_ver->GetTable();
      //  printf("GVER add=%d\n",GVER);
      // hPiFi[10]->Fill(GVER->ge_x[2]-rZver);
      // hPiFi[11]->Fill(GVER->ge_x[0]-rXver);
      // hPiFi[12]->Fill(GVER->ge_x[1]-rYver);
      printf("Z Geant-found=%.2f, dx=%.2f, dy=%.2f nCtbSl=%d n1=%d eveID=%d\n",GVER->ge_x[2]-XVertex.z(),GVER->ge_x[0]-XVertex.x(),GVER->ge_x[1]-XVertex.y(),mCtbHits.size(),n1,eveID);
      printf("##G %6d %d %f %f    %d %d %d\n",eveID,mTotEve,GVER->ge_x[2],XVertex.z(),mCtbHits.size(),n1,NCtbMatches());
      
    }
  }
  
  LOG_DEBUG << "StppLMVVertexFinder::ppLMV5() ends" << endm;
  return true;
}


//==========================================================
//==========================================================
int  StppLMVVertexFinder::NCtbMatches() { 
  int nTr=mPrimCand.size();
  if(mVertexConstrain ) nTr--; // drop beam line
  return nTr;
}


/*
 * $Log: StppLMVVertexFinder.cxx,v $
 * Revision 1.32  2017/02/15 15:30:18  smirnovd
 * Refactoring design flaws by getting rid of static members
 *
 * For details see commits on master branch edbe287d..8166aa1e
 *
 * - StMinuitVertexFinder: Move static fit functions to base class
 * - StMinuitVertexFinder: Use equivalent base class fit function for Beamline3D fits
 * - StMinuitVertexFinder: Get rid of static mWidthScale
 *     in favor of equivalent local variables.
 *     To do: The scale should come from the database where it is actually already
 *     defined. See Calibrations::rhic::vertexSeed::weight in DB
 * - Converted functions from static to member + adjustments
 * - Introduced self static pointer to vertex finder implementations
 * - This is required by TMinuit relying on static fit functions.
 * - StMinuitVertexFinder: Use common fit function type
 * - Renamed Chi2AtVertex to virtual CalcChi2DCAs
 *     The virtuality allows to keep backward compatibility with the previous
 *     implementation of 1D fit with (forced) beamline in StMinuitVertexFinder.
 * - Convert static sDCAs to mDCAs
 * - Convert static sBeamline to mBeamline
 *
 * Revision 1.31  2017/02/14 22:00:40  smirnovd
 * Squashed commit of the following clean-up changes:
 *
 * See master branch for details.
 *
 * - Remove commented code for debugging
 * - Removed extra validation; it is done at construction
 * - No need to include header for apple OS
 * - Removed pointless assert
 * - Use standard portable type name
 * - Remove unused header math_constants.h
 * - StMinuitVertexFinder: Remove abandoned member function
 *
 * Revision 1.30  2017/01/06 21:01:48  smirnovd
 * Use pi constant from standard library, s/C_PI/M_PI/
 *
 * Revision 1.29  2017/01/03 22:17:36  smirnovd
 * [Stylistic] Changed public addVertex() to accept references
 *
 * Avoid unnecessary gymnastics with pointers
 *
 * Revision 1.28  2016/08/18 17:46:14  smirnovd
 * Squashed commit of the following refactoring changes:
 *
 * Date:   Wed Jul 27 18:31:18 2016 -0400
 *
 *     Removed unused arguments in UseVertexConstraint()
 *
 *     In StiPPVertexFinder and StvPPVertexFinder this method does nothing
 *
 * Date:   Wed Jul 27 16:47:58 2016 -0400
 *
 *     Make old UseVertexConstraint private virtual and call it from its public replacement in the base class
 *
 *     also mark methods as private explicitly
 *
 * Date:   Wed Jul 27 16:52:02 2016 -0400
 *
 *     Removed unused private data member mWeight
 *
 * Date:   Wed Jul 27 16:50:42 2016 -0400
 *
 *     Prefer base class static beamline parameters rather than this class private members
 *
 * Date:   Wed Jul 27 16:21:49 2016 -0400
 *
 *     StPPVertexFinder: Got rid of unused private beamline parameters
 *
 *     The equivalent measurements are available from the base class
 *     StGenericVertexFinder
 *
 * Date:   Wed Jul 27 16:19:19 2016 -0400
 *
 *     StPPVertexFinder: For beamline position use equivalent static methods from parent class
 *
 * Date:   Wed Jul 27 16:05:50 2016 -0400
 *
 *     StGenericVertexMaker: Assigning once is enough
 *
 * Date:   Mon Aug 15 10:43:49 2016 -0400
 *
 *     StGenericVertexFinder: Print out beamline parameters
 *
 *     Print beamline values as extracted from the database before any modification.
 *
 * Date:   Wed Jul 6 15:33:02 2016 -0400
 *
 *     Stylistic changes and minor refactoring
 *
 *     Whitespace and comments for improved readability
 *     s/track/stiKalmanTrack/
 *
 * Date:   Wed Jul 6 15:28:16 2016 -0400
 *
 *     StPPVertexFinder: Switched to cleaner c++11 range loop syntax
 *
 * Date:   Wed Jul 6 15:22:14 2016 -0400
 *
 *     StPPVertexFinder: Minor c++ refactoring
 *
 *     - Removed unused counter
 *     - c-style array to std::array
 *
 * Date:   Wed Jul 6 15:20:11 2016 -0400
 *
 *     Deleted commented out code
 *
 *     Removed unused #include's StMinuitVertexFinder
 *
 * Revision 1.27  2016/04/28 18:17:38  smirnovd
 * [Cosmetic] Whitespace, doxygen, comments, and readability changes only
 *
 * Revision 1.26  2016/04/15 19:24:14  smirnovd
 * Got rid of unused variables reported by compiler
 *
 * Revision 1.25  2014/07/28 18:07:59  jeromel
 * Cst for C++11 and added comment for cov elements as per StVertex
 *
 * Revision 1.24  2010/01/26 21:01:49  fisyak
 * Clean up, switch from bit mask to attributes
 *
 * Revision 1.23  2006/05/05 18:35:39  balewski
 * block the fake second prim vertex
 *
 * Revision 1.22  2006/05/04 20:01:30  jeromel
 * Switched to logger
 *
 * Revision 1.21  2006/05/02 13:49:21  balewski
 * replace gufld() with  mBfield = event->runInfo()->magneticField();
 *
 * Revision 1.20  2005/07/19 21:56:58  perev
 * MultiVertex
 *
 * Revision 1.19  2005/06/21 02:16:36  balewski
 * multiple prim vertices are stored in StEvent
 *
 * Revision 1.18  2005/03/11 22:23:53  balewski
 * towards PPV
 *
 * Revision 1.17  2005/03/09 19:24:18  balewski
 * preparation for PPV vertex finder
 *
 * Revision 1.16  2004/12/16 17:01:36  balewski
 * 2 cuts in ppLMV4 were slightly off what was in the TPT version
 *
 * Revision 1.15  2004/12/13 20:39:58  fisyak
 * Add initaition of StGenericVertexFinder variables, replace mDumMaker by StMaker::GetChain() method
 *
 * Revision 1.14  2004/09/13 15:41:31  balewski
 * fix bug in ppLMV4/5 switch
 *
 * Revision 1.13  2004/09/03 14:24:15  jeromel
 * Fixed inverted comment
 *
 * Revision 1.12  2004/09/03 00:09:08  jeromel
 * Modified code to Implement Init() and SetMode() and allow passing a switch
 * to chose the vertex finder from within the same code implementation. Was
 * needed for ppLMV (one implementation, two algorithm)
 *
 * Revision 1.11  2004/09/01 18:45:01  balewski
 * ppLMV5/4 switch added
 *
 * Revision 1.10  2004/08/25 15:20:30  balewski
 * Z-vertex range increased to +/- 150 cm
 *
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

