/***************************************************************************
 *
 * $Id: StEstRefitBranch.cxx,v 1.8 2004/01/26 22:47:17 caines Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the track refitting
 *
 ***************************************************************************
 *
 * $Log: StEstRefitBranch.cxx,v $
 * Revision 1.8  2004/01/26 22:47:17  caines
 * Take out prolific print statement
 *
 * Revision 1.7  2003/04/30 20:36:54  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.6  2003/04/14 18:31:35  munhoz
 * allow 1 hit tracks
 *
 * Revision 1.5  2002/11/21 23:02:48  caines
 * Fix helicity initialization for TPC tracks and no longer use assumed vertex if one isnt there
 *
 * Revision 1.4  2001/07/15 20:31:31  caines
 * Fixes from Insure++ debugging
 *
 * Revision 1.3  2001/02/23 13:46:12  lmartin
 * Two arguments (hittmp,exclhit) of the RefitBranch method removed.
 *
 * Revision 1.2  2001/01/25 18:02:18  lmartin
 * Method renamed and declared as StEstTracker method.
 * gtrk deleted at the end of the refit.
 *
 * Revision 1.1  2000/12/07 11:14:22  lmartin
 * First CVS commit
 *
 **************************************************************************/
// memory leak fixed for gtrk. gtrk is delete at the end of the method which 
// still impose a creation at each call.
// Final fix ? : gtrk as a data member of StEstMaker and initialized outside
// RefitBranch. 

#include "StEstTracker.h"
#include "StEstParams.hh"
#include "Infrastructure/StEstWafer.hh"
#include "Infrastructure/StEstBranch.hh"
#include "Infrastructure/StEstHit.hh"
#include "Infrastructure/StEstTrack.hh"
#include "Infrastructure/StEstTPCTrack.hh"

#include "StarCallf77.h"
#include "table_header.h"
#include "tables/St_egr_egrpar_Table.h"

// egr fortran routines
#define egr_cross_fact F77_NAME(egr_cross_fact,EGR_CROSS_FACT)
extern "C" {int type_of_call egr_cross_fact_(int&,float &,float &,float &,float &,float &,float &);}
#define egr_helix_fit F77_NAME(egr_helix_fit,EGR_HELIX_FIT)
extern "C" {int type_of_call egr_helix_fit_(int&,StEstGtrk*,float*,float*,float*,float*,float*,int&, table_head_st*,egr_egrpar_st*,float(*)[3]);}
#define egr_find_outlier F77_NAME(egr_find_outlier,EGR_FIND_OUTLIER)
extern "C" {int type_of_call egr_find_outlier_(int&,StEstGtrk*,float*,float*,float*,float*,float*,int&,int&, table_head_st*,egr_egrpar_st*);}


int StEstTracker::RefitBranch(StEstBranch *br, int usevertex, int *fitstatus) {

  // details on the fit status (called flag in this method)
  // flag = -1 not enough hits for the fit
  // flag = -2 not enough hits survive the first half
  // flag =  1 the fit is successful ( the reduced circ. and linear chisq are below the proba.)
  // flag = -3 one outlier has been identified, the iteration continues
  // flag = -6 no outlier can be isolated to reduce the chisq
  // flag = -4 the helix fit failed.
  // details on the fit parameters gtrk[0].p[x]
  // p[0] = 1 
  // p[1] = sgn*rr : signed radius
  // p[2] = acent : x center of the circle
  // p[3] = bcent : y center of the circle
  // p[4] = phi0 : azimuthal angle of the origin of the helix 
  // p[5] = z0 origin in z of the helix
  // p[6] = tanl : slope of the track in z-path
  // p[7] = chit : circular chisq 
  // p[8] = chil : linear chisq

  int i,max,ient,ient1,isvt;
  int j,iFirstGood;
  int RowNextToLast,RowLast;
  float as,bs,covar [3] [3];
    
  int ncir,row_in_gtrk,ibad;
  float  xcir[220]={0};
  float  ycir[220]={0};
  float  zcir[220]={0};
  float  wlin[220]={0};
  float  wcir[220]={0};
  float dx,dy,dz;
  float cf/*VPunused ,xold,yold,theta*/;
  int good_hit[220],enough,ntry;
  int pnt[220];
  int iret;
  gtrk = new StEstGtrk[1];
  row_in_gtrk=1;

  // initialize the gtrk structure
  gtrk[0].nhit = 0;
  gtrk[0].nfit = 0;
  gtrk[0].ntpc = 0;
  gtrk[0].nmax = 0;
  gtrk[0].flag = 0;
  for (i=0;i<220;i++) {
    gtrk[0].ipnt[i]=0;
    gtrk[0].pos[i]=0;
    gtrk[0].det[i]=0;
  }
  for (i=0;i<9;i++) gtrk[0].p[i]=0;

  StEstTrack *track = br->mTrack;           // track object

  StEstTPCTrack *tpctr = track->mTPCTrack;  // TPCtrack object

  egr_egrpar_st*   egrpar      = m_egr_egrpar->GetTable();

  // load the tpc information 
  // In StEstInit the tpc hits are sorted from the smallest radius to the biggest radius
  // the opposite of the order from the tpc tracking
  max = tpctr->mNHits;  
  if (max > mParams[mPass]->maxtpchits) max = mParams[mPass]->maxtpchits;
  for (i=0;i<max;i++) {
    gtrk[0].pos[i] = i;
    gtrk[0].det[i] = 1;
    gtrk[0].ipnt[i] = tpctr->mHitIndex[i];   
    gtrk[0].nhit = gtrk[0].nhit + 1;
    gtrk[0].ntpc = gtrk[0].ntpc + 1;
  }
  

  // load the svt information (from the SSD to the first SVT layer)
  ient = gtrk[0].nhit;
  for(i=0;i<br->GetNHits();i++) {
    gtrk[0].det[ient] = 2;
    gtrk[0].ipnt[ient] = i;
    gtrk[0].pos[ient] = ient;
    gtrk[0].nhit++;
    ient++;
  }
  // load the vertex information
  if(usevertex==1) {
    ient = gtrk[0].nhit;
    gtrk[0].ipnt[ient] = -999;
    gtrk[0].pos[ient] = ient;
    gtrk[0].det[ient] = 3;
    gtrk[0].nhit++;
  }

  // At the stage the grtrk array is filled in this order :
  // gtrk : 0   ....   40  ...   41 ... 44   ...  45
  //          tpc hits           SVT hits     main vertex
  //      rmin  ...   rmax      SSD ...SVT1

  
  // initialize the good points
  for (i=0;i<gtrk[0].nhit;i++) {
    good_hit[i]=1;
  }

  // prune singletons (tpc hits)
//     rowNextToLast = mod(hit(trk(ipt).ipnt(trk(ipt).nhit-1)).row,100)
//     rowLast = mod(hit(trk(ipt).ipnt(trk(ipt).nhit)).row,100)
//     if(abs(rowNextToLast-rowLast).gt.2) good_hit(trk(ipt).nhit)=.false.
  RowNextToLast = tpctr->row[gtrk[0].ipnt[1]]%100;
  RowLast = tpctr->row[gtrk[0].ipnt[0]]%100;
  if (abs(RowNextToLast-RowLast)>2) good_hit[0]=0;


  ntry = 1;
  enough = 0;
  while (enough==0 && ntry<=egrpar[0].mxtry) {
    ncir = 0;
    //fill Xcir arrays with the vertex
    if(usevertex==1 && good_hit[gtrk[0].nhit-1]){
      dx=0.01;
      dy=0.01;
      dz=0.05;
      xcir[0]  = mVertex->mXG->x();
      ycir[0]  = mVertex->mXG->y();
      zcir[0]  = mVertex->mXG->z();
      wcir[0]  = 1./(dx*dx+dy*dy);
      wlin[0]  = 1./(dz*dz);
      //    wcir[0]  = 1250.; // record as 0.2 in z and 0.02 in xy (cm) 
      //    wlin[0]  = 25.0;
      pnt[0] = gtrk[0].nhit-1;
      ncir++;
    }
    
    // Fill Xcir arrays with the SVT/SSD hits
    // We need a reverse loop to fill in increasing radius
    for (i=gtrk[0].ntpc+br->GetNHits()-1;i>=gtrk[0].ntpc;i--) {
      if (good_hit[i]>=0) {
	isvt=gtrk[0].ipnt[i];
	xcir[ncir]=br->GetHit(isvt)->GetGlobX()->x();
	ycir[ncir]=br->GetHit(isvt)->GetGlobX()->y();
	zcir[ncir]=br->GetHit(isvt)->GetGlobX()->z();
	if (br->GetHit(isvt)->GetWafer()->GetLayer()<3) {
	  dx = br->GetHit(isvt)->GetGlobE()->x();
	  dy = br->GetHit(isvt)->GetGlobE()->y();
	  dz = br->GetHit(isvt)->GetGlobE()->z();
//	  cout << "EST-INFO : dx ="<<dx<<"  dy="<<dy<<"  dz="<<dz<<endl;
// 	  dx = 0.001;
// 	  dy = 0.001;
// 	  dz = 0.001;
	  //      dx = 0.002;
	  //      dy = 0.002;
	  //      dz = 0.002;
	}
	else {
	  //      dx = 0.002;
	  //      dy = 0.002;
	  //      dz = 0.08;
	  dx = 0.001;
	  dy = 0.001;
	  dz = 0.001;
	}      
	wcir[ncir]  = 1./(dx*dx+dy*dy);
	wlin[ncir]  = 1./(dz*dz);
	pnt[ncir]=i;
	ncir++;
      }
    }
    // Fill Xcir arrays with the TPC hits
    // We take them as they are (sorted already)
    if (gtrk[0].ntpc>0) {
      cf =-1;
      ient1 = -1;
      for (i=0;i<gtrk[0].ntpc;i++) {
	ient=gtrk[0].ipnt[i];
	//	if (ient>=0 && tpctr->mHitFlag[ient]!=0) good_hit[i]=0;
	if (good_hit[i]==1) {
	  xcir[ncir]=tpctr->mR[ient]->x();
	  ycir[ncir]=tpctr->mR[ient]->y();
	  zcir[ncir]=tpctr->mR[ient]->z();
	  wcir[ncir]=1./(tpctr->mdR[ient]->x()*tpctr->mdR[ient]->x()+
			 tpctr->mdR[ient]->y()*tpctr->mdR[ient]->y());
	  //  iret= egr_cross_fact(tpctr->row[ient],xcir[ncir],ycir[ncir],
	  // 			       xold,yold,cf,theta);
	  //if (ient1==-1) ient1=0;
	  //  else if (ient1==0) {
	   //    ient1=1;
	  //   wcir[ncir-1]=wcir[ncir-1]/cf;
	  // }
	  //  xold=xcir[ncir];
	  //  yold=ycir[ncir];
	  //  wcir[ncir]=wcir[ncir]/cf;
	  wlin[ncir]=1.0/(tpctr->mdR[ient]->z()*tpctr->mdR[ient]->z());
	  if (tpctr->mHitFlag[ient]!=0) {
	    wcir[ncir]=0.01*wcir[ncir];
	    wlin[ncir]=0.01*wlin[ncir];
	  }
	  pnt[ncir]=i;
	  ncir++;
	}
      }
    }
    gtrk[0].nfit=ncir;
    
    // enough hits for the fit
    if (ncir<=egrpar[0].minfit) {
      gtrk[0].flag=-1;
      ntry = egrpar[0].mxtry+1;
    }
    else {
      // get the first half circle
      bs=0;
      j=1;
      as=(xcir[0]-xcir[1])*(xcir[0]-xcir[1])+
	(ycir[0]-ycir[1])*(ycir[0]-ycir[1]);
      while (bs<as && j<ncir-1) {
	j++;
	bs=as;
	as=(xcir[0]-xcir[j])*(xcir[0]-xcir[j])+
	(ycir[0]-ycir[j])*(ycir[0]-ycir[j]);
      }
      if (bs>as) ncir=j;
      gtrk[0].nfit = ncir;
      // check if enough hits survive the first half
      if (ncir<=egrpar[0].minfit) {
	gtrk[0].flag=-2;
	ntry=egrpar[0].mxtry+1;
      }
      else {
	// call the fitting routine
 	iret = egr_helix_fit(row_in_gtrk,gtrk,xcir,ycir,zcir,wcir,wlin,ncir,m_egrpar_h,egrpar,covar);
	if (iret==1) {
	  if ((gtrk[0].p[7]/float(ncir-3)<egrpar[0].prob[0] || 
	       egrpar[0].prob[0]<0) &&
	      (gtrk[0].p[8]/float(ncir-2)<egrpar[0].prob[1] ||
	       egrpar[0].prob[0]<0)) {
	    enough=1;
	    gtrk[0].flag=1;
	  }
	  else {
	   if (egrpar[0].mxtry>1) {
	     iret=egr_find_outlier(row_in_gtrk,gtrk,xcir,ycir,zcir,wcir,wlin,ncir,ibad,m_egrpar_h,egrpar);
	     if (iret==1) {
	       good_hit[pnt[ibad-1]]=0;
	       gtrk[0].flag=-3;
	       ntry=ntry+1;
	     }
	     else {
	       gtrk[0].flag=-6;
	       ntry=egrpar[0].mxtry+1;
	     }
	   }
	   else {
	     enough=1;
	     gtrk[0].flag=1;
	   }
	 }
	}
	else {
	  gtrk[0].flag=-4; // error from the helix fit
	  ntry=egrpar[0].mxtry+1;
	}
      }
    }
  }
  if (enough==0) {
    *fitstatus=gtrk[0].flag;
    br->mLastFitStatus=0;
    delete[] gtrk;
    // fit failed - return -1 (previously gtrk[0].flag)
    return(-1);
  }
  else {
    if (ntry>1)
      for (i=0;i<gtrk[0].nhit;i++) 
	if (good_hit[i]==0) {
	  gtrk[0].pos[i]=-99;
	}
  }

  // helix prameters
  double c,x0,y0,z0,dip,phase,chi2_lin,chi2_cir;
  int h;
  
  c=1./fabs(gtrk[0].p[1]);   //curvature
  x0= fabs(gtrk[0].p[1])*cos(gtrk[0].p[4])+ gtrk[0].p[2]; // x of origin point
  y0= fabs(gtrk[0].p[1])*sin(gtrk[0].p[4])+ gtrk[0].p[3]; // y of origin point
  z0=gtrk[0].p[5];                                       // z of origin point
  dip=atan(gtrk[0].p[6]);        // dip angle
  phase=gtrk[0].p[4];            // phase
  if(gtrk[0].p[1]>0) h=-1;
  else h=1;                     // helicity
  chi2_lin=gtrk[0].p[8];///(float)(gtrk[0].nfit-2);       // chi square linear
  chi2_cir=gtrk[0].p[7];///(float)(gtrk[0].nfit-3);       // chi square circle

  StThreeVector<double> orig(x0,y0,z0);
  StHelix *helix = new StHelix(c,dip,phase,orig,h);

  // if the main vertex is in the fit and considered has a good hit
  // we should change the helix origin back to the nearest real 
  // and flagged as good tpc hit.
  if (usevertex==1 && good_hit[0]==1) {
    iFirstGood=1;
    while (good_hit[pnt[iFirstGood]]==0) iFirstGood++;
    StThreeVector<double> TrueOrig(xcir[iFirstGood],ycir[iFirstGood],zcir[iFirstGood]); 
    double shift_s;
    shift_s=helix->pathLength(TrueOrig);
    helix->moveOrigin(shift_s);

    
  }  
      

  br->SetHelix(helix);
  br->SetChiSq(chi2_cir+chi2_lin); // ??? sprawdzic ???
  br->SetChiSqCir(chi2_cir); // ??? sprawdzic ???
  br->SetChiSqLin(chi2_lin); // ??? sprawdzic ???
  br->SetNFit(gtrk[0].nfit);
  br->mLastFitStatus=gtrk[0].flag;
  *fitstatus=gtrk[0].flag;
  delete [] gtrk;
  return(1);
  
    
  }
