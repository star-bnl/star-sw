/***************************************************************************
 *
 * $Id: StEstProjection.cxx,v 1.12 2009/08/25 18:54:13 fine Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the track projections
 *
 ***************************************************************************
 *
 * $Log: StEstProjection.cxx,v $
 * Revision 1.12  2009/08/25 18:54:13  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 1.11  2004/01/15 00:25:30  fisyak
 * unsigned int => size_t
 *
 * Revision 1.10  2003/09/02 17:58:04  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.9  2003/04/30 20:36:54  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.8  2002/04/30 22:49:19  caines
 * Make est work with shifted SVT geom, change search radii to 1cm
 *
 * Revision 1.7  2001/07/15 20:31:30  caines
 * Fixes from Insure++ debugging
 *
 * Revision 1.6  2001/04/25 17:31:10  perev
 * HPcorrs
 *
 * Revision 1.5  2001/03/02 16:10:26  lmartin
 * geometrical cuts assigned to the layers and not to the number of hits in the segment.
 *
 * Revision 1.4  2001/02/23 13:31:11  lmartin
 * cout replaced by gMessMgr.
 *
 * Revision 1.3  2001/01/31 16:54:40  lmartin
 * mParams[]->debug replaced by mDebug.
 * phi and z params for StEstIndexGeom removed from StEstParams.
 *
 * Revision 1.2  2001/01/25 18:00:09  lmartin
 * Methods declared as StEstTracker methods.
 * RAD_TO_DEG variable replaced by C_DEG_PER_RAD.
 *
 * Revision 1.1  2000/12/07 11:14:21  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StMessMgr.h"
#include "StEstTracker.h"
#include "StEstParams.hh"
#include "math_constants.h"
#include "StHelix.hh"
#include "Infrastructure/StEstBranch.hh"
#include "Infrastructure/StEstWafer.hh"
#include "Infrastructure/StEstHit.hh"
#include "StMatrix.hh"


// matrix for SVT rotation and translation (alignment)
StMatrix<double> mSvtToGlobalRotation(3,3,1); // (3X3)
StMatrix<double> mGlobalToSvtRotation(3,3,1); // (3X3)
StThreeVector<double> mSvtPositionInGlobal; 

void StEstTracker::AlignmentInfo() {

  // fill in rotation and translation matrix
  double phi = 0.0;  
  double theta = -0.000381;
  double psi = -0.000156;

  mGlobalToSvtRotation(1,1) = cos(theta)*cos(phi);
  mGlobalToSvtRotation(1,2) = cos(theta)*sin(phi);
  mGlobalToSvtRotation(1,3) = -sin(theta);
  mGlobalToSvtRotation(2,1) = sin(psi)*sin(theta)*cos(phi)-cos(psi)*sin(phi);
  mGlobalToSvtRotation(2,2) = sin(psi)*sin(theta)*sin(phi) + cos(psi)*cos(phi);
  mGlobalToSvtRotation(2,3) = cos(theta)*sin(psi);
  mGlobalToSvtRotation(3,1) = cos(psi)*sin(theta)*cos(phi)+sin(psi)*sin(phi);
  mGlobalToSvtRotation(3,2) = cos(psi)*sin(theta)*sin(phi)-sin(psi)*cos(phi);
  mGlobalToSvtRotation(3,3) = cos(theta)*cos(psi);

  size_t ierr;
  mSvtToGlobalRotation = mGlobalToSvtRotation.inverse(ierr);
  if (ierr!=0){ 
    cerr << "StSvtCoordinateTransform::Cant invert rotation matrix" << endl;
    cout << "Global to SVT rotation matrix:" << mGlobalToSvtRotation << endl;
    cout << "SVT to global rotation matrix:" << mSvtToGlobalRotation << endl;
  }

  mSvtPositionInGlobal.setX(-0.276);
  mSvtPositionInGlobal.setY(-0.082);
  mSvtPositionInGlobal.setZ(-0.192);

}

int StEstTracker::Preprojection(StEstBranch *branch, int slayer) {
  
  StHelix*              helix = branch->GetHelix();
  StThreeVector<double> vect1; 
  pair<double,double>   s;      // intersection

  StEstWafer*         pNeighbour;

  double    sd;      // path length
  int       phi;     // phi bin
  int       z;       // z bin
  int      il,jl;
  int       ns;      
  int       ret = 0;
  int       nneighbours = mParams[mPass]->nneighbours[slayer];
  int       nwaf;

  const int     FREE     = 0;
  const int     OCCUPIED = 1; 

  mPreprojNumber=0;


  if(mDebugLevel>4){
    gMessMgr->Info()<<"     branch  = "<<&branch<<endm;
    gMessMgr->Info()<<"     slayer  = "<<slayer<<endm;
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
    LOG_ERROR <<"ERROR in Preprojection !!! slay<1 or slay>4 !!!"<<endm;
    LOG_ERROR <<"     slay = "<<slayer<<"     branch* = "<<&branch<<endm;
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
      ret = 1;
    }
    else{
      //vect1 = helix->at(sd);
      vect1 = mSvtToGlobalRotation*helix->at(sd)+mSvtPositionInGlobal;
      phi=(int)floor((atan2(vect1.y(), vect1.x())+M_PI)*C_DEG_PER_RAD/mPhiBin);
      z=(int)floor(vect1.z()/mZBin)+mNZBins/2; 

      //last chance for track
      // need to be verified

      if(z==-1)
	z++;
      if(z==mNZBins)
      z--;
      
      if(z>=0 && z<mNZBins && phi>=0 && phi<mNPhiBins) {
	for(jl=0; jl<mIndexGeom->getNWaf(phi,z,slayer); jl++){
	  if(mPreprojNumber>=MAXFINDWAF){
	    gMessMgr->Warning()<<"Preprojection : too many wafers. "<<endm;
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
	  if(mDebugLevel>0) 
	    gMessMgr->Warning()<<"Prepojection : too many neighbours. mPreprojNumber="<<mPreprojNumber<<endm;
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
    if(mDebugLevel>4)
      gMessMgr->Info()<<"wafer ="<<mPreprojTable[jl]->mId<<" nhits ="<<mPreprojTable[jl]->GetNHits()<<endm;
    mPreprojTable[jl]->mPreprojection=FREE;
  }
    
  if(mDebugLevel>2){
    gMessMgr->Info()<<"    Wafers found: "<<mPreprojNumber<<endm;
    gMessMgr->Info()<<"**** Preprojection STOP ****"<<endm;
  }

  return(ret); // 0=O.K.  1=no wafers 2=too many wafers 3=too many neighbours
} // end of Preprojection method


// int StEstTracker::Preprojection(StEstBranch& branch, int slayer) {
  
//   StHelix*              helix = branch.GetHelix();
//   StThreeVector<double> vect1; 
//   pair<double,double>   s;      // intersection

//   StEstWafer*         pNeighbour;

//   double    sd;      // path length
//   int       phi;     // phi bin
//   int       z;       // z bin
//   int      il,jl;
//   int       ns;      
//   int       ret = 0;
//   int       nneighbours = mParams[mPass]->nneighbours[slayer];
//   int       nwaf;

//   const int     FREE     = 0;
//   const int     OCCUPIED = 1; 

//   mPreprojNumber=0;


//   if(mDebugLevel>4){
//     gMessMgr->Info()<<"     branch  = "<<&branch<<endm;
//     gMessMgr->Info()<<"     slayer  = "<<slayer<<endm;
//   }
  
//   switch(slayer){
//   case 0:
//   case 1:
//   case 2:
//     ns=2;
//     break;
//   case 3:
//     ns=1;
//     break;
//   default:
//     ret = 4;
//     gMessMgr->Error()<<"ERROR in Preprojection !!! slay<1 or slay>4 !!!"<<endm;
//     gMessMgr->Error()<<"     slay = "<<slayer<<"     branch* = "<<int(&branch)<<endm;
//     return(ret);
//   }

//   for(;ns>0;ns--){ //loop over layers
    
//     // intersection of helix and cylinder
//     s = helix->pathLength(mParams[mPass]->lrad[slayer][ns-1]);  

//     // we expect small negative values for the two solutions.
//     // In some cases, one or two big positive values are return by pathLength
//     // In such case, we need to subtract a period.
//     if (s.first>0) {
//       s.first=s.first-helix->period();
//     }
//     if (s.second>0) {
//       s.second=s.second-helix->period();
//     }
//     if(fabs(s.first)<fabs(s.second))
//     //    if(s.first>s.second)
//       sd=s.first;
//     else
//       sd=s.second;

//     if(fabs(sd)>=1000){
//       ret = 1;
//     }
//     else{
//       vect1 = helix->at(sd);
//       phi=floor((atan2(vect1.y(), vect1.x())+M_PI)*C_DEG_PER_RAD/mPhiBin);
//       z=floor(vect1.z()/mZBin)+mNZBins/2; 

//       //last chance for track
//       // need to be verified

//       if(z==-1)
// 	z++;
//       if(z==mNZBins)
//       z--;
      
//       if(z>=0 && z<mNZBins && phi>=0 && phi<mNPhiBins) {
// 	for(jl=0; jl<mIndexGeom->getNWaf(phi,z,slayer); jl++){
// 	  if(mPreprojNumber>=MAXFINDWAF){
// 	    gMessMgr->Warning()<<"Preprojection : too many wafers. "<<endm;
// 	    ret = 2;
// 	    goto PREPROJ_FINISH;
// 	  }
// 	  if(mIndexGeom->getWafTab(phi,z,slayer)[jl]->mPreprojection==FREE){
// 	    mPreprojTable[mPreprojNumber++]=mIndexGeom->getWafTab(phi,z,slayer)[jl];
// 	    mIndexGeom->getWafTab(phi,z,slayer)[jl]->mPreprojection=OCCUPIED;
// 	  }
// 	} // end of for(jl...
//       } // end of if(z...
//     } // end of if(fabs(sd)>=1000)...
//   } // end of for(;ns...

//   while(nneighbours>0){
//     nwaf=mPreprojNumber;
//     for(il=0; il<nwaf; il++){
//       for(jl=0; jl<8; jl++){
// 	if(mPreprojNumber>=MAXFINDWAF){
// 	  if(mDebugLevel>0) 
// 	    gMessMgr->Warning()<<"Prepojection : too many neighbours. mPreprojNumber="<<mPreprojNumber<<endm;
// 	  ret=3;
// 	  goto PREPROJ_FINISH;
// 	}
// 	pNeighbour=mPreprojTable[il]->neighbour[jl];
// 	if(pNeighbour==NULL)
// 	  continue;  // empty bin
// 	if(pNeighbour->mPreprojection==FREE){
// 	  mPreprojTable[mPreprojNumber++]=pNeighbour;
// 	  pNeighbour->mPreprojection=OCCUPIED;
// 	}
//       }
//     }
//     nneighbours--;
//   } // end of while(nneighbours...

//  PREPROJ_FINISH:;
//   for(jl=0; jl<mPreprojNumber; jl++) {
//     if(mDebugLevel>4)
//       gMessMgr->Info()<<"wafer ="<<mPreprojTable[jl]->mId<<" nhits ="<<mPreprojTable[jl]->GetNHits()<<endm;
//     mPreprojTable[jl]->mPreprojection=FREE;
//   }
    
//   if(mDebugLevel>2){
//     gMessMgr->Info()<<"    Wafers found: "<<mPreprojNumber<<endm;
//     gMessMgr->Info()<<"**** Preprojection STOP ****"<<endm;
//   }

//   return(ret); // 0=O.K.  1=no wafers 2=too many wafers 3=too many neighbours
// } // end of Preprojection method




int StEstTracker::Projection(StEstBranch* branch, int slay) {


  StEstWafer* tmpl1;
  StHelix*      helix = branch->GetHelix();
  StEstHit*   tmpl1a;
  StEstHit*   tmpl2a;
  StEstHit*   hit;
  int il,jl, kl;
  int realslay;
  double sd, dist, tmpd1, tmpd2, tmpd1b, tmpd2b, tmpd1c, tmpd2c;

  StThreeVector<double> vect1;
  StThreeVector<double> vect2;
  StThreeVector<double> vect3;

  if(mDebugLevel>4) {
    StThreeVector<double> *vect0 = new StThreeVector<double>(0,0,0);
    gMessMgr->Info()<<"  dist from (0,0,0)="<<helix->distance(*vect0)<<endm;
    gMessMgr->Info()<<"  pathlength to (0,0,0)="<<helix->pathLength(*vect0)<<endm;
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
    
    if(mDebugLevel>4) {
      gMessMgr->Info()<<"  helix->pathLength="<<sd<<endm;
    }
    if(sd<1000) {
      vect1=helix->at(sd);
      
      for(jl=0; jl<mPreprojTable[il]->GetNHits(); jl++) {
	if (mPreprojTable[il]->GetHit(jl)->GetFlag()>0) continue; //hit cannot be taken into account
	hit=mPreprojTable[il]->GetHit(jl);
	dist=::sqrt((vect1.x() - hit->GetGlobX()->x())*(vect1.x() - hit->GetGlobX()->x()) + 
		  (vect1.y() - hit->GetGlobX()->y())*(vect1.y() - hit->GetGlobX()->y()) +
		  (vect1.z() - hit->GetGlobX()->z())*(vect1.z() - hit->GetGlobX()->z()));
	
	tmpd1b=fabs(vect1.z() - hit->GetGlobX()->z()); //distl
	tmpd1c=::sqrt((vect1.x() - hit->GetGlobX()->x())*(vect1.x() - hit->GetGlobX()->x()) + 
		    (vect1.y() - hit->GetGlobX()->y())*(vect1.y() - hit->GetGlobX()->y())); //distw
	


	//	realslay=3-branch->GetNHits();
	realslay=slay;
	if(tmpd1b>mParams[mPass]->geomcutl[realslay] || tmpd1c>mParams[mPass]->geomcutw[realslay])
	  continue;  // geometrical cut
	tmpl1  = mPreprojTable[il];	    
	tmpl1a = hit;
	tmpd1  = dist;
	if(mProjOut.nhit < MAXHITPROJ) 
	  mProjOut.nhit++;
	else
	  gMessMgr->Warning()<<"Projection : mProjOut.nhit==MAXHITPROJ"<<endm;
	if(mDebugLevel>4) 
	  gMessMgr->Info()<<"--- HIT ADDED TO PROJ TABLE ---  "<<mProjOut.nhit<<endm;
	
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
	if(mDebugLevel>4) 
	  for(kl=0; kl<mProjOut.nhit; kl++) {
	    gMessMgr->Info()<<"hit nr = "<<kl<<"\tdist = "<<mProjOut.dist[kl]<<endm;
	  }
	
      } // end of for(jl=0; jl<mPreprojTable[il]->GetNHits; jl++)
    } // end of if(sd<1000)
  } //end of for(il=0; il<mProjOut.nwaf; il++)
  // end of projection & hit information

  if(mDebugLevel>2)
    gMessMgr->Info()<<"Projection **** STOP ****"<<endm;
 
  return(1);

};


