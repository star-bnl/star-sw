/***************************************************************************
 *
 * $Id: StppLMVVertexFinder.cxx,v 1.1 2004/07/21 01:53:18 balewski Exp $
 *
 * Author: Thomas Ullrich, Feb 2002
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************/

#include <StEventTypes.h>
#include <StEnumerations.h>
#include <StGlobals.hh>
#include <SystemOfUnits.h>
#include <StMessMgr.h>
#include <cmath>
#include "math_constants.h"

#include "tables/St_g2t_vertex_Table.h" // tmp for Dz(vertex)
#include "StMaker.h"


#include "tables/St_g2t_ctf_hit_Table.h"
#include "St_trg_Maker/St_trg_Maker.h"
#include <StTriggerData.h>

// This needs cleanup of the mapping code
extern void cts_get_ctb_indexes(long, long &, long &);

/*!
 * cts_get_ctb_indexes() is defined in $STAR/pams/ctf/cts/cts.cc
 */


#include "StppLMVVertexFinder.h"

#if 1
#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)
#endif

StppLMVVertexFinder::StppLMVVertexFinder() {
    mBeamHelix=0;
    mStatus = 0;
    mExternalSeedPresent = false;
    mVertexConstrain = false;
    requireCTB = false;
    use_ITTF = false;

    //jan

    mCtbEtaSeg=0.5;   mCtbPhiSeg=C_PI/30; // NEVER chang this two , JB
    
    mCtbThres_ch=2 ;  //to reject slats with low ADC for real data
    mCtbThres_mev=1; //to reject slats with low dE for M-C data, CTB clibration: 2 MeV==5 ADC
    mMaxTrkDcaRxy=3.9;
    mMinTrkPt=0.2;
    mMinNumberOfFitPointsOnTrack = 10; 
    mMatchCtbMax_eta=mCtbEtaSeg/2.+0.02;
    mMatchCtbMax_phi=mCtbPhiSeg/2.+C_PI*1./180.;
    mDVtxMax=4.0;  // max sigma multipl between tracks and current vertex, used for tracks rejection
    mBLequivNtr=20; // equivalent # of tracks for BeamLine
    mTotEve=0;
    mDumMaker = new StMaker();

}


 StppLMVVertexFinder::~StppLMVVertexFinder() {
   delete mBeamHelix;mBeamHelix=0;
 }

//==========================================================
//==========================================================
bool StppLMVVertexFinder::fit(StEvent* event) {
  //
  //  Reset vertex
  //
  mFitError = mFitResult = StThreeVectorD(0,0,0);
  mCtbHits.clear();
  mPrimCand.clear();
  n1=0,n2=0,n3=0, n4=0,n5=0,n6=0;
  mTotEve++;
  eveID=event->id();

  // Get BField from gufld(,) 
  //  cout<<"Trying to Get the BField the old way..."<<endl;
  float x[3] = {0,0,0};
  float b[3];
  gufld(x,b);
  mBfield=  0.1*b[2]; //This is now Tesla.
  printf("mBfield=%f tesla=%e b2=%f\n",mBfield,tesla,b[2]);
 
  ///Flagging
  if(use_ITTF){
    setFlagBase(8000);
  }else{
    setFlagBase(1000);
  }

  gMessMgr->Message("","I") << "ppLMV:: mCtbThres_ch="<<mCtbThres_ch
			    <<"\n MinNumberOfFitPointsOnTrack="<<mMinNumberOfFitPointsOnTrack
			    <<"\n MaxTrkDcaRxy="<<mMaxTrkDcaRxy
			    <<"\n MinTrkPt="<<mMinTrkPt
			    <<"\n MatchCtbMax_eta="<<mMatchCtbMax_eta
			    <<"\n MatchCtbMax_phi="<<mMatchCtbMax_phi
    			    <<"\n BLequivNtr="<<mBLequivNtr
    			    <<"\n DVtxMax="<<mDVtxMax
    			    <<"\n Ignored: mRequireCTB="<<mRequireCTB
    			    <<"\n mVertexConstrain="<<mVertexConstrain
    			    <<"\n CtbThres_mev="<<mCtbThres_mev
    //			    <<"\n  ="<<
			    <<endm; 
  

  // get CTB info, does not  work for embeding 
  if(event->runId()<100000) 
    collectCTBhitsMC();  // use M-C
  else
    collectCTBhitsData(event); // use real data


  if(mCtbHits.size()<=0){
    gMessMgr->Warning() << "StppLMVVertexFinder::fit: no valid CTB hits found, quit" << endm;
    mStatus = -1;
    return false;
  }   
  
  //  collectCTBhits3(); // alterantive access to CTB for DATA
  
  gMessMgr->Message("","I") << mCtbHits.size() << " CTB slats accepted"<<endm;
  
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
  printf(", now n1=%d n2=%d n3=%d n4=%d  n6=%d\n",n1,n2,n3,n4,n6);


  //
  //  In case there are no tracks left we better quit
  //
  if (mPrimCand.empty()) {
    gMessMgr->Warning() << "StppLMVVertexFinder::fit: no tracks to fit." << endm;
    mStatus = -1;
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
    printf("WARN ppLMV: nominal beam line added with sigma=%f, now nTrack=%d \n",sigma, mPrimCand.size());
  } // end of beamLine


  printf("PrimCand  before ppLMV for eveID=%d tracks at first point\n",eveID);
  for( uint j=0;j<mPrimCand.size();j++) {
    StThreeVectorD p=mPrimCand[j].helix.momentum(mBfield*tesla);
    printf("j=%d  sig=%f pT=%f eta=%f phi/deg=%f\n",j,mPrimCand[j].sigma,p.perp(),p.pseudoRapidity(), p.phi()/C_PI*180);
  }

  //  ----------  D O   F I N D    V E R T E X
  ppLMV4();

  printf("Prim tr used by ppLMV for nTotEve=%d, tracks at vertex\n",mTotEve);
  for( uint j=0;j<mPrimCand.size();j++) {
    double spath = mPrimCand[j].helix.pathLength(mFitResult.x(),mFitResult.y());
    StThreeVectorD p=mPrimCand[j].helix.momentumAt(spath,mBfield*tesla);
    printf("j=%d  sig=%f pT=%f eta=%f phi/deg=%f cur=%f p=%f charg=%d spath=%f\n",j,mPrimCand[j].sigma,p.perp(),p.pseudoRapidity(), p.phi()/C_PI*180,mPrimCand[j].helix.curvature(), p.mag(),mPrimCand[j].helix.charge(mBfield*tesla),spath);
  }
	 
    return true;
} 


StThreeVectorD
StppLMVVertexFinder::result() const {return mFitResult;}

StThreeVectorD
StppLMVVertexFinder::error() const {return mFitError;}

int
StppLMVVertexFinder::status() const {return mStatus;}

void
StppLMVVertexFinder::setExternalSeed(const StThreeVectorD& s)
{
    mExternalSeedPresent = true;
    mExternalSeed = s;
}

void
StppLMVVertexFinder::setPrintLevel(int level) { ;}//mMinuit->SetPrintLevel(level);}

//======================================================
//======================================================
void
StppLMVVertexFinder::printInfo(ostream& os) const
{
    os << "StppLMVVertexFinder - Fit Statistics:" << endl;
    os << "fitted vertex ........................ " << mFitResult << endl;
    os << "position errors ...................... " << mFitError << endl;
    os << "# of used tracks + beamL.............. " << mPrimCand.size() << endl;
    os << "minimum found (FMIN) ................. " << mFmin << endl;
    os << "estimated distance to minimum (FEDM) . " << mFedm << endl;
    os << "parameter uncertainties (ERRDEF) ..... " << mErrdef << endl;
    os << "# of parameters (NPARI) .............. " << mNpari << endl;
    os << "# of max parameters (NPARX) .......... " << mNparx << endl;
    os << "goodness of covariance matrix (ISTAT)  " << mStatus << endl;
    os << "min # of fit points for tracks ....... " << mMinNumberOfFitPointsOnTrack << endl;
    os << "final potential width scale .......... " << mWidthScale << endl;
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
void StppLMVVertexFinder::NoVertexConstraint() {mVertexConstrain = false; gMessMgr->Info() << "StppLMVVertexFinder::No Vertex Constraint" << endm;}

int  StppLMVVertexFinder::NCtbMatches() { return mCtbHits.size() ;}
void StppLMVVertexFinder::SetFitPointsCut(int fitpoints) {mMinNumberOfFitPointsOnTrack = fitpoints;return;}


//==========================================================
//==========================================================
bool StppLMVVertexFinder::matchTrack2CTB (StTrack* track, float & sigma) {
  /* upgrade:
     - used dE/dX for strag
     - saturate sig(p)
  */

  sigma=0;
  const double Rctb=213.6; // (cm) radius of the CTB 

  assert(track);
  assert(finite(track->geometry()->helix().curvature()));

  if( track->flag()<0) return false;
  if( track->topologyMap().trackFtpc() )return false;
  if( use_ITTF && track->fittingMethod()!=kITKalmanFitId ) return false;
  if( !use_ITTF && track->fittingMethod()==kITKalmanFitId ) return false;

  n1++;	

  StPhysicalHelixD TrkHlx=track->geometry()->helix();

  //           check Rxy_min condition  close to beam    
  double xorigin = 0.0; double yorigin = 0.0;
  double spath = TrkHlx.pathLength(xorigin, yorigin);
  StThreeVectorD posDCA = TrkHlx.at(spath);
  //  cout<<" DCA Position: "<<posDCA<<endl;
  double x_m = posDCA.x(), y_m = posDCA.y();
  double dmin = ::sqrt(x_m*x_m + y_m*y_m);
  if( dmin > mMaxTrkDcaRxy ) return false;
  n2++;
  
  if( track->fitTraits().numberOfFitPoints() <= mMinNumberOfFitPointsOnTrack) return false;
  n3++;

  if(track->geometry()->momentum().perp() <mMinTrkPt ) return false;
  n4++;

  //  cout<<"\n\n DCA to beam at 0x0: "<<posDCA<<endl;
       
  //Find momentum direction at vertex point
  StThreeVectorD pmom = TrkHlx.momentumAt(spath,mBfield*tesla );
  double beta = pmom.mag()/::sqrt(pmom.mag()*pmom.mag()+0.139*0.139); //Assume pion 
  float strag=0.0136/beta/pmom.mag()*fabs(spath);
  if(fabs(mBfield)<0.01) strag=0.0136*fabs(spath);
  // printf("stragling=%f %f p=%f %f nFp=%d\n",strag,beta,pmom.mag(),spath, track->fitTraits().numberOfFitPoints() );
  
  pairD  d2;
  d2 = TrkHlx.pathLength(Rctb);
  //  printf(" path 1=%f, 2=%f, period=%f, R=%f\n",d2.first ,d2.second,TrkHlx.period(),1./TrkHlx.curvature());
  
  // assert(d2.first<0); // propagate backwards
  //assert(d2.second>0); // propagate forwards
  
  if(d2.first>=0 || d2.second<=0) {
    n5++;
    gMessMgr->Message("","W") << "ppLMV::matchTrack2CTB ()"  
      " unexpected solution for track crossing CTB, track="<<track <<endm;
    return false;
  }
  
  
  StThreeVectorD posCTB = TrkHlx.at(d2.second);
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
    printf("add tr %d w/ sigma=%f p/GeV=%f spath/cm=%f nFitP=%d\n",mPrimCand.size(),sigma,pmom.mag(),spath,track->fitTraits().numberOfFitPoints());
    
    return true;
  }
  
  // match not found
  return false;
}

//==========================================================
//==========================================================
bool  StppLMVVertexFinder::ppLMV4() {  //  ----------  D O   F I N D    V E R T E X

  int totTr=mPrimCand.size();
  printf("passed %d tracks match to CTB,  BeamLine=%d\n",totTr,mVertexConstrain );
   
  double xo=0.0,yo=0.0;
  if(mVertexConstrain) {// use the beam line as a starting point
    xo=mX0;
    yo=mY0;
  }
  
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
    if( mPrimCand.size() <= 1 ){
      cout<<"ppLMV4: Fewer than 2 track remains. No vertex found."<<endl;
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
    cout<<vertIter<<"  Vertex Position   : "<<XVertex.x()<<" "<<XVertex.y()<<" "<<XVertex.z()<<endl;
    cout<<"Error in Position : "<<::sqrt(C11)<<" "<<::sqrt(C22)<<" "<<::sqrt(C33)<<endl;
    

    // Check if the fit is any good
    // Loop over tracks again to get Chi2 and check each track's deviation
    
    double dmax=0.0;
    vector<JHelix>::iterator itehlx, end, ikeep;
    end=mPrimCand.end();
    if(mVertexConstrain )end--; // to preserve beamL

    for( itehlx=mPrimCand.begin(); itehlx <end;  itehlx++) {

      //    while(itehlx < mPrimCand.end()-1){
      //if( (*itehlx).helix.curvature()<10) {itehlx++; continue;}// beamLine track
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
      //  itehlx++;
    }

    if( dmax > mDVtxMax ){
      printf("Removing a track! dmax=%f \n",dmax);
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

  cout<<"ppLMV4: Primary Vertex found!\nVert position: "<<XVertex<<", accepted tracks "<<mPrimCand.size()<<" of "<<totTr<<" eveID="<<eveID<<endl;

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
      printf("Z Geant-found=%.2f, dx=%.2f, dy=%.2f nCtbSl=%d n1=%d eveID=%d\n",GVER->ge_x[2]-XVertex.z(),GVER->ge_x[0]-XVertex.x(),GVER->ge_x[1]-XVertex.y(),mCtbHits.size(),n1,eveID);
      printf("##v %6d %d %f %f   %f %f   %f %f   %d %d\n",eveID,mTotEve,GVER->ge_x[2],XVertex.z(),GVER->ge_x[0],XVertex.x(),GVER->ge_x[1],XVertex.y(),mCtbHits.size(),n1);
      
    }
  }
  
  printf("end of ppLMV4\n");
  return true;
}


//==========================================================
//==========================================================
void  StppLMVVertexFinder::collectCTBhitsData(StEvent* event){
  // returns true if one or more valid CTB hits is found.  
  gMessMgr->Info() << "StppLMVVertexFinder::fit() pick real CTB hits" << endm;

  // access CTB from Akio's Maker
  StTriggerData *trgD=event->triggerData ();
  if(!trgD){
    gMessMgr->Warning() << "StppLMVVertexFinder::fit() no trigData in Data" << endm;
    return ;
  }

  assert(trgD);
  for (UInt_t slat = 0; slat < 2; slat++) 
    for (UInt_t tray = 0; tray < 120; tray++) {
      ctbHit curHit;
      curHit.adc = trgD->ctbTraySlat(tray,slat,0);
      if(curHit.adc<mCtbThres_ch) continue;
      // printf("B sl=%3d tr=%3d  %4f\n",slat,tray, curHit.adc );
      ctb_get_slat_from_data(slat,tray, curHit.phi, curHit.eta);
      mCtbHits.push_back(curHit);
    }

  // old method , run just for cross check, delete later, JB

  StTriggerDetectorCollection* trigCol = event->triggerDetectorCollection();
  if(!trigCol){
    gMessMgr->Warning() << "StppLMVVertexFinder::fit() no trigCol in Data" << endm;
    return ;
  }

  StCtbTriggerDetector* ctbDet = &(trigCol->ctb());
  for (UInt_t slat = 0; slat < ctbDet->numberOfSlats(); slat++) 
    for (UInt_t tray = 0; tray < ctbDet->numberOfTrays(); tray++) {
      ctbHit curHit;
      curHit.adc = ctbDet->mips(tray,slat,0);
      if(curHit.adc<mCtbThres_ch) continue;
      printf("A sl=%3d tr=%3d  %4f\n",slat,tray, curHit.adc );
    }
  
  for (UInt_t slat = 0; slat < ctbDet->numberOfSlats(); slat++) 
    for (UInt_t tray = 0; tray < ctbDet->numberOfTrays(); tray++) {
      if(trgD->ctbTraySlat(tray,slat,0)!=ctbDet->mips(tray,slat,0)) printf("JAN-ERROR %d %d \n",slat,tray);
    }
  
  return ;
}


//==========================================================
//==========================================================
bool  StppLMVVertexFinder::collectCTBhitsMC(){// M-C CTB
  // CTB clibration: 2 MeV==5 ADC
  // RETUN: true if GEANT table with hits exist (even if empty)

  St_DataSet *gds=mDumMaker->GetDataSet("geant");
  if(gds==0) return false;
  
  // -------------- E X T R A C T    C T B   H I T S   --------------------
  //access the CTB data  from GEANT
  St_g2t_ctf_hit *g2t_ctb_hit = (St_g2t_ctf_hit *) gds->Find("g2t_ctb_hit");
  if(g2t_ctb_hit == 0){
    cout << "No CTB Hits in MC table for this event" << endl;
    cout << "g2t_ctb_hit = " << g2t_ctb_hit << endl;
    return false;
  }
  
  gMessMgr->Message("","I") <<" use GEANT CTB hits,  ADC's  with 2 MeV=> 5 ADC, thr/MeV="<<mCtbThres_mev<<endm;
  g2t_ctf_hit_st *ctb_hit = NULL;
  
  //printf("All GEANT CTB hits=%d\n",(int)g2t_ctb_hit->GetNRows());
  
  if (g2t_ctb_hit->GetNRows() == 0)    gMessMgr->Message("","I") <<" Empty geant/ctb data set "<<endm;

  ctb_hit = g2t_ctb_hit->GetTable();
  assert(ctb_hit);
  int i;
  for (i = 0; i < g2t_ctb_hit->GetNRows(); i++,ctb_hit++){
    float de_mev=ctb_hit->de*1000.;
    // printf("CTB Hit i=%d  de/MeV=%f parent=%d\n",i,de_mev ,ctb_hit->track_p);
    if(de_mev <mCtbThres_mev)  continue; // ignore double hits per CTB slat

    long iPhi,iEta;
    cts_get_ctb_indexes(ctb_hit->volume_id,iPhi,iEta);
    iPhi--; iEta--; // change range to [0,N-1]
    printf("iPhi=%d,iEta=%d de/MeV=%f \n",(int)iPhi,(int)iEta,de_mev);
    assert(iPhi >= 0 && iPhi<60 && iEta>=0 && iEta<4);
    //printf("ctb_indexes , hit=%d, vol_id=%d, iPhi=%d, iEta=%d, de/MeV=%f\n",i,(int)ctb_hit->volume_id,(int)iPhi,(int)iEta );
    
    ctbHit curHit;
    curHit.adc=(int)de_mev*2.5 ;
    curHit.phi=iPhi*mCtbPhiSeg;
    curHit.eta=iEta*mCtbEtaSeg -0.75;
    mCtbHits.push_back(curHit);
    
  }// end of loop over CTB hits
  
  
  gMessMgr->Message("","I") << mCtbHits.size() << " CTB slats accepted from M-C data"<<endm;
  
  return  true;
}


//==========================================================
//==========================================================
void  StppLMVVertexFinder::ctb_get_slat_from_data(int slat, int tray, float & phiRad, float &eta) {
  float phiZero1 = 72 ; // magic lines from Pablo & Herb
  float phiZero2 = 108 ;
  float deltaPhi = 6 ;
  // tray=0,1
  // slat=0,...,119

  int iz ;
  float phi ;

  if ( tray < 60 )  {
    phi = phiZero1 - tray * deltaPhi ;
    iz = 0 ;
  }  else {
    phi = phiZero2 + (tray-60) * deltaPhi ;
    iz = 1 ;
  }
  if ( phi <   0. ) phi += 360 ;
  if ( phi > 360. ) phi -= 360 ;

  phiRad=phi/180*C_PI;
  eta=(1-2*iz)*(1+2*slat)*0.25;
  printf("CTB hit: slat=%d, tray=%d,  phiDeg=%f/deg, eta=%f\n",slat,tray,phi,eta);
  
}


/*
 * $Log: StppLMVVertexFinder.cxx,v $
 * Revision 1.1  2004/07/21 01:53:18  balewski
 * first
 *
 **************************************************************************/

