/***************************************************************************
 *
 * $Id: StEstProjection.cxx,v 1.1 2000/12/07 11:14:21 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the track projections
 *
 ***************************************************************************
 *
 * $Log: StEstProjection.cxx,v $
 * Revision 1.1  2000/12/07 11:14:21  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StEstMaker.h"

int StEstMaker::Preprojection(StEstBranch& branch, int slayer) {
  
  StHelix*              helix = branch.GetHelix();
  StThreeVector<double> vect1; 
  pair<double,double>   s;      // intersection

  StEstWafer*         pNeighbour;

  double    sd;      // path length
  int       phi;     // phi bin
  int       z;       // z bin
  long      il;
  long      jl;
  int       ns;      
  int       ret = 0;
  int       nneighbours = mParams[mPass]->nneighbours[slayer];
  int       nwaf;

  const int     FREE     = 0;
  const int     OCCUPIED = 1; 

  mPreprojNumber=0;


  if(mParams[mPass]->debug>4){
    cout<<"     branch  = "<<&branch<<endl;
    cout<<"     slayer  = "<<slayer<<endl;
  }
  
  switch(slayer){
  case 0:
  case 1:
  case 2:
    ns=2;
    break;
  case 3:
    ns=1;
    break;
  default:
    ret = 4;
    cerr<<"ERROR in Preprojection !!!"<<endl;
    cerr<<" slay<1 or slay>4 !!!"<<endl;
    cerr<<"     slay = "<<slayer;
    cerr<<"     branch* = "<<long(&branch)<<endl;
    return(ret);
  }

  for(;ns>0;ns--){ //loop over layers
    
    // intersection of helix and cylinder
    s = helix->pathLength(mParams[mPass]->lrad[slayer][ns-1]);  

    // we expect small negative values for the two solutions.
    // In some cases, one or two big positive values are return by pathLength
    // In such case, we need to subtract a period.
    if (s.first>0) {
      s.first=s.first-helix->period();
    }
    if (s.second>0) {
      s.second=s.second-helix->period();
    }
    if(fabs(s.first)<fabs(s.second))
    //    if(s.first>s.second)
      sd=s.first;
    else
      sd=s.second;

    if(fabs(sd)>=1000){
//       if(mParams[mPass]->debug>0){
// 	cout<<"       Path Length > 10m !!! : s.f="<<s.first
// 	    <<" s.s="<<s.second<<" phase="<<helix->phase()
// 	    <<" period="<<helix->period()
// 	    <<" track_id="<<branch.mTrack->mTPCTrack->GetId()
// 	    <<" layer="<<slayer
// 	    <<" ns="<<ns<<endl;
//       }
      ret = 1;
    }
    else{
      vect1 = helix->at(sd);
      phi=floor((atan2(vect1.y(), vect1.x())+M_PI)*RAD_TO_DEG/mParams[mPass]->phibin);
      z=floor(vect1.z()/mParams[mPass]->zbin)+mParams[mPass]->nzbins/2; 

      //last chance for track
      // need to be verified

      if(z==-1)
	z++;
      if(z==mParams[mPass]->nzbins)
      z--;
      
      if(z>=0 && z<mParams[mPass]->nzbins && phi>=0 && phi<mParams[mPass]->nphibins) {
	for(jl=0; jl<mIndexGeom->getNWaf(phi,z,slayer); jl++){
	  if(mPreprojNumber>=MAXFINDWAF){
	    cout<<"       WARNING!!! too many wafers. "<<endl;
	    ret = 2;
	    goto PREPROJ_FINISH;
	  }
	  if(mIndexGeom->getWafTab(phi,z,slayer)[jl]->mPreprojection==FREE){
	    mPreprojTable[mPreprojNumber++]=mIndexGeom->getWafTab(phi,z,slayer)[jl];
	    mIndexGeom->getWafTab(phi,z,slayer)[jl]->mPreprojection=OCCUPIED;
	  }
	} // end of for(jl...
      } // end of if(z...
    } // end of if(fabs(sd)>=1000)...
  } // end of for(;ns...

  while(nneighbours>0){
    nwaf=mPreprojNumber;
    for(il=0; il<nwaf; il++){
      for(jl=0; jl<8; jl++){
	if(mPreprojNumber>=MAXFINDWAF){
	  if(mParams[mPass]->debug>0)
	    cout<<"      WARNING!!! too many neighbours. "<<endl;
	  ret=3;
	  goto PREPROJ_FINISH;
	}
	pNeighbour=mPreprojTable[il]->neighbour[jl];
	if(pNeighbour==NULL)
	  continue;  // empty bin
	if(pNeighbour->mPreprojection==FREE){
	  mPreprojTable[mPreprojNumber++]=pNeighbour;
	  pNeighbour->mPreprojection=OCCUPIED;
	}
      }
    }
    nneighbours--;
  } // end of while(nneighbours...

 PREPROJ_FINISH:;
  for(jl=0; jl<mPreprojNumber; jl++) {
    if(mParams[0]->debug>4) {
      cout << "detector ="<<mPreprojTable[jl]->mId<<"  nhits ="<<mPreprojTable[jl]->GetNHits()<<" srodek (center)="<<*(mPreprojTable[jl]->GetX())<<"  direction = "<<*(mPreprojTable[jl]->GetN())<<endl;
    }
    mPreprojTable[jl]->mPreprojection=FREE;
  }
    
  if(mParams[mPass]->debug>2){
    cout << "    Wafers found: "<<mPreprojNumber<<endl;
    cout<<"**** Preprojection STOP ****"<<endl;    
  }

  return(ret); // 0=O.K.  1=no wafers 2=too many wafers 3=too many neighbours
} // end of Preprojection method




int StEstMaker::Projection(StEstBranch* branch, long slay) {


  StEstWafer* tmpl1;
  StHelix*      helix = branch->GetHelix();
  StEstHit*   tmpl1a;
  StEstHit*   tmpl2a;
  StEstHit*   hit;
  long il,jl, kl;
  long realslay;
  double sd, dist, tmpd1, tmpd2, tmpd1b, tmpd2b, tmpd1c, tmpd2c;

  StThreeVector<double> vect1;
  StThreeVector<double> vect2;
  StThreeVector<double> vect3;

  if(mParams[0]->debug>4) {
    StThreeVector<double> *vect0 = new StThreeVector<double>(0,0,0);
    cout << "  dist from (0,0,0)="<<helix->distance(*vect0)<<endl;
    cout << "  pathlength to (0,0,0)="<<helix->pathLength(*vect0)<<endl;
    delete vect0;
  }

  //initialization of mProjOut structure:
  for (il=0;il<MAXHITPROJ;il++) {
    mProjOut.dist[il]  = 666;
    mProjOut.distw[il] = 666;
    mProjOut.distl[il] = 666;
    mProjOut.hit[il] = NULL;
  }
  mProjOut.nhit = 0;
  mProjOut.nwaf = 0;
  
  // Projection & hit information
  for(il=0; il<mPreprojNumber; il++) {
    vect2.setX(mPreprojTable[il]->GetX()->x());
    vect2.setY(mPreprojTable[il]->GetX()->y());
    vect2.setZ(mPreprojTable[il]->GetX()->z());
    vect3.setX(mPreprojTable[il]->GetN()->x());
    vect3.setY(mPreprojTable[il]->GetN()->y());
    vect3.setZ(mPreprojTable[il]->GetN()->z());
    
    sd=helix->pathLength(vect2, vect3);
    
    if(mParams[mPass]->debug>4) {
      cout<<"  helix->pathLength="<<sd<<endl;
    }
    if(sd<1000) {
      vect1=helix->at(sd);
      
      for(jl=0; jl<mPreprojTable[il]->GetNHits(); jl++) {
	if (mPreprojTable[il]->GetHit(jl)->GetFlag()>0) continue; //hit cannot be taken into account
	hit=mPreprojTable[il]->GetHit(jl);
	dist=sqrt((vect1.x() - hit->GetGlobX()->x())*(vect1.x() - hit->GetGlobX()->x()) + 
		  (vect1.y() - hit->GetGlobX()->y())*(vect1.y() - hit->GetGlobX()->y()) +
		  (vect1.z() - hit->GetGlobX()->z())*(vect1.z() - hit->GetGlobX()->z()));
	
	tmpd1b=fabs(vect1.z() - hit->GetGlobX()->z()); //distl
	tmpd1c=sqrt((vect1.x() - hit->GetGlobX()->x())*(vect1.x() - hit->GetGlobX()->x()) + 
		    (vect1.y() - hit->GetGlobX()->y())*(vect1.y() - hit->GetGlobX()->y())); //distw
	


	realslay=3-branch->GetNHits();
	//	realslay=slay;
	if(tmpd1b>mParams[mPass]->geomcutl[realslay] || tmpd1c>mParams[mPass]->geomcutw[realslay])
	  continue;  // geometrical cut
	tmpl1  = mPreprojTable[il];	    
	tmpl1a = hit;
	tmpd1  = dist;
	if(mProjOut.nhit < MAXHITPROJ) 
	  mProjOut.nhit++;
	else
	  cerr <<  "mProjOut.nhit==MAXHITPROJ" <<endl;
	if(mParams[mPass]->debug>4) 
	  cout << "--- HIT ADDED TO PROJ TABLE ---  "<<mProjOut.nhit<<endl;
	
	// do pomyslenia :
	for(kl=0; kl<mProjOut.nhit; kl++) // sorting
	  //	      if(mProjOut.distl[kl]>tmpd1) {
	  if(mProjOut.dist[kl]>tmpd1) {
	    //		tmpl2=mProjOut.hwafer[kl];
	    tmpl2a = mProjOut.hit[kl];
	    tmpd2  = mProjOut.dist[kl];
	    tmpd2b = mProjOut.distl[kl];
	    tmpd2c = mProjOut.distw[kl];
	    //		mProjOut.hwafer[kl]=tmpl1;
	    mProjOut.hit[kl]   = tmpl1a;
	    mProjOut.dist[kl]  = tmpd1;
	    mProjOut.distl[kl] = tmpd1b;
	    mProjOut.distw[kl] = tmpd1c;
	    //		tmpl1=tmpl2;
	    tmpl1a = tmpl2a;
	    tmpd1  = tmpd2;
	    tmpd1b = tmpd2b;
	    tmpd1c = tmpd2c;
	  }
	if(mParams[mPass]->debug>4) 
	  for(kl=0; kl<mProjOut.nhit; kl++) {
	    cout << "hit nr = "<<kl<<"\tdist = "<<mProjOut.dist[kl]<<endl;
	  }
	
      } // end of for(jl=0; jl<mPreprojTable[il]->GetNHits; jl++)
    } // end of if(sd<1000)
  } //end of for(il=0; il<mProjOut.nwaf; il++)
  // end of projection & hit information

  if(mParams[0]->debug>2)
    cout<<"Projection **** STOP ****"<<endl;
 
  return(1);

};


