/***************************************************************************
 *
 * $Id: StEstInit.cxx,v 1.4 2001/01/31 16:45:25 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the Initialization 
 *
 ***************************************************************************
 *
 * $Log: StEstInit.cxx,v $
 * Revision 1.4  2001/01/31 16:45:25  lmartin
 * mParams[]->debug replaced by mDebug.
 * phi and z params for StEstIndexGeom remove from StEstParams.
 *
 * Revision 1.3  2001/01/28 22:18:27  lmartin
 * References to the data member mEvalTrack of StEstHit object removed.
 *
 * Revision 1.2  2001/01/25 17:49:09  lmartin
 * Method Setup removed
 * Methods defined as public methods of StEstTracker class
 * Input tables passed as method arguments rather than instantiated into the methods
 *
 * Revision 1.1  2000/12/07 11:14:21  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StThreeVectorD.hh"
#include "math_constants.h"
#include "phys_constants.h"
#include "StEstTracker.h"
#include "StHelix.hh"
#include "StThreeVector.hh"
#include "StEstParams.hh"
#include "Infrastructure/StEstWafer.hh"
#include "Infrastructure/StEstBranch.hh"
#include "Infrastructure/StEstHit.hh"
#include "Infrastructure/StEstTrack.hh"
#include "Infrastructure/StEstTPCTrack.hh"
#include "tables/St_dst_vertex_Table.h"
#include "tables/St_svg_geom_Table.h"
#include "tables/St_svg_shape_Table.h"
#include "tables/St_svg_config_Table.h"
#include "tables/St_scs_spt_Table.h"
#include "tables/St_tpt_track_Table.h"
#include "tables/St_tcl_tphit_Table.h"
#include "tables/St_tte_eval_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"

#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)


int StEstTracker::VertexSetup(St_dst_vertex *preVertex){

  cout<<"StEstTracker::VertexSetup"<<endl;
  StThreeVectorD* xg = new StThreeVectorD(0,0,0);
  StThreeVectorD* xl = new StThreeVectorD(0,0,0);

  //  St_dst_vertex  *preVertex = (St_dst_vertex *)GetDataSet("preVertex/.data/preVertex"); 
  
  if( preVertex){
    dst_vertex_st *preVtxPtr = preVertex->GetTable();
    
    for (int i=0; i <preVertex->GetNRows(); i++,preVtxPtr++) {
      
      if(preVtxPtr->iflag == 101) {
	xg->setX(preVtxPtr->x);
	xg->setY(preVtxPtr->y);
	xg->setZ(preVtxPtr->z);
      }
    }
  }
  else cout<<"StEstTracker::VertexSetup : Prevertex not found. Main vertex set to (0,0,0)"<<endl;
  mVertex = new StEstHit(9999,xg,xl,1,1,mIndexWaf[0]);
  
  return kStOK;
}

int StEstTracker::BranchInit(){

  int i;
  StEstBranch *branch;

  cout<<"StEstTracker::BranchInit starting"<<endl;
  mNTrack = mNTPCTrack;
  mTrack = new StEstTrack*[mNTPCTrack];
  if(!mTrack){
    cerr<<"ERROR!!! not enough memory"<<endl;
    cerr<<"StEstMaker::Init mTrack = new StEstTrack*[ "<< mNTPCTrack<<"];"<<endl;
    return 1;
  }
  
  for (i=0;i<mNTPCTrack;i++) {
    mTrack[i] = new StEstTrack(mParams[0]->maxbranches,mTPCTrack[i]);
    if (mTrack[i]==NULL)
      cerr << "ERROR StEstMaker::Init mTrack[i]==NULL" <<endl;
    else {
      if(mDebugLevel>3)
	cout<<" New track has been created #"<<i<<endl;
      branch = new StEstBranch(NULL, long(mParams[0]->maxsvthits));
      if (branch==NULL)
	cerr << "ERROR StEstMaker::Init branch==NULL" << endl;
      else {
	// branch initialization
	// we should copy first the tpc helix before attaching 
	// the helix to the branch
	StHelix *helix = new StHelix(*mTPCTrack[i]->GetHelix());
	branch->SetHelix(helix);
	branch->JoinTrack(mTrack[i],0);
	branch->SetDebugLevel(0);
	if(mDebugLevel>3)
	  cout<<" Branch added"<<endl;
	StThreeVector<double> a(mVertex->mXG->z(),mVertex->mXG->y(),mVertex->mXG->z());
  	if (mTPCTrack[i]->GetFlag()>=0) {
	  // First the branch is refitted without the tpc hits only.
	  // The fit results (chisq,chisql,chisqc,ndofl,ndofc) are
	  // assigned to the tpc track
	  int fitstatus;
	  fitstatus=0;
 	  if (mTPCTrack[i]->GetHelix()->distance(a)<3.) 
	    RefitBranch(branch,NULL,-1,1,&fitstatus);
	  else
	    RefitBranch(branch,NULL,-1,0,&fitstatus);
	if(mDebugLevel>3)
	  cout<<" Branch refitted"<<endl;
	  //	  branch->mTrack->mTPCTrack->mChiSq = branch->GetChiSq();
	  //	  branch->mTrack->mTPCTrack->mChiSqCir = branch->GetChiSqCir();
	  //	  branch->mTrack->mTPCTrack->mChiSqLin = branch->GetChiSqLin();
	  // here we copy the new helix and attach it to the track
	  StHelix *helix_for_track = new StHelix(*branch->GetHelix());
	  mTrack[i]->SetHelix(helix_for_track);
	}
      }
    }
  }

  cout<<"StEstTracker::BranchInit stopped"<<endl;
  return kStOK;
}
  

int StEstTracker::SVTInit(St_svg_geom*   Stsvggeom,
			  St_svg_shape*   Stsvgshape,
			  St_svg_config*   Stsvgconf,
			  St_scs_spt*   Stscsspt)
{

  if (mDebugLevel>2) 
    cout << "SVTInit **** START ****"<<endl;  

  long il, jl, kl, maxl, lay, shape;
  long	lay2, lad, waf, maxlad, minlad, maxwaf, minwaf;
  long zmin, zmax, pmin, pmax;
  double phi0, dphi, r;

  StThreeVectorD *xx;
  StThreeVectorD *nn;
    
  svg_geom_st*   svggeom;
  svg_config_st*   svgconf;
  scs_spt_st*   scsspt;
  svg_shape_st*    svgshape;

  if (mDebugLevel>2) 
    cout << "SVTInit **** Getting data from tables ***"<<endl;  


  svggeom   = Stsvggeom->GetTable();
  mNWafers=Stsvggeom->GetNRows();
  svgshape   = Stsvgshape->GetTable();
  svgconf   = Stsvgconf->GetTable();
  // Now get hits
  scsspt   = Stscsspt->GetTable();


  if (mDebugLevel>0) 
    cout << "SVTInit **** Creating "<<mNWafers<<" wafers ****"<<endl;  
  mIndexGeom = new StEstIndexGeom(mNPhiBins,mNZBins);
  mIndexWaf  =  new StEstWafer*[mNWafers];
  if(!mIndexWaf){
    cerr<<"ERROR!!! not enough memory"<<endl;
    cerr<<"StEstMaker::SVTInit mIndexWaf = new StEstWafer*["<<mNWafers<<"];"<<endl;
    return 1;
  }

  if (mDebugLevel>2) 
    cout << "SVTInit **** Loop over the wafers **** : "<<mNWafers<<endl;  

  // We scan once the scs_spt table the count the number of hits in each 
  // wafer and allocate suited memory space for the hits in the wafer object.
  long int HitPerWafer[9000]={0,}; 
  for (il=0;il<Stscsspt->GetNRows();il++) {
    if( scsspt[il].flag < 4){
      HitPerWafer[scsspt[il].id_wafer]++; 
    }
  }
  for(il=0; il<mNWafers; il++){ // loop over the wafers
    
    mWafId2IndexWaf[svggeom[il].id]=il;

    xx = new StThreeVectorD(svggeom[il].x[0],svggeom[il].x[1],svggeom[il].x[2]);
    nn = new StThreeVectorD(svggeom[il].n[0],svggeom[il].n[1],svggeom[il].n[2]);

    shape=svggeom[il].id_shape-1;
    mIndexWaf[il]  =  new StEstWafer(svggeom[il].id, HitPerWafer[svggeom[il].id], xx, nn, shape);
    if(!mIndexWaf[il] || !xx || !nn){
      cout<<"ERROR!!! not enough memory"<<endl;
      cout<<"mIndexWaf["<<il<<"] = new StEstWafer("<<svggeom[il].id<<", 200);"<<endl;
      return 1;
    }

    // fill object mIndexGeom
    lay=mIndexWaf[il]->GetLayer();

    zmin = (long)floor((mIndexWaf[il]->GetX()->z() - svgshape[shape].shape[1])/mZBin) + mNZBins/2;
    zmax = (long)floor((mIndexWaf[il]->GetX()->z() + svgshape[shape].shape[1])/mZBin) + mNZBins/2;
    phi0 = ( atan2(mIndexWaf[il]->GetX()->y(),mIndexWaf[il]->GetX()->x()) + M_PI)*C_DEG_PER_RAD;
    r = sqrt(mIndexWaf[il]->GetX()->x() * mIndexWaf[il]->GetX()->x() + 
	     mIndexWaf[il]->GetX()->y() * mIndexWaf[il]->GetX()->y());
    dphi=atan(svgshape[shape].shape[0]/r)*C_DEG_PER_RAD;
    pmin= (long)floor((phi0-dphi)/mPhiBin);
    if(pmin<0)
      pmin+=mNPhiBins;
    pmax= (long)floor((phi0+dphi)/mPhiBin);
    if(pmax>=mNPhiBins) 
      pmax-=mNPhiBins;
    
    jl=pmin-1;
    do{
      jl++;
      if(jl>=mNPhiBins)
	jl=0;
      for(kl=zmin; kl<=zmax; kl++)
	mIndexGeom->setWafTab(jl,kl,lay,mIndexWaf[il]);
    }while(jl!=pmax);
    
    if (mDebugLevel>3) 
      if ((il%10)==0)
	cout << "Wafer #"<<svggeom[il].id<<" coord: "<<mIndexWaf[il]->GetX()->x()<<"  "<<mIndexWaf[il]->GetX()->y()<<"  "<<mIndexWaf[il]->GetX()->z()<<endl;  

  } // end of for(il=0; il<mNWafers; il++)
  
  
  // neighbours wafers
  // 08/01/00 change to match the new svt ladder numbering scheme.
  // the first svt sub-layer now contains only ladders 2,4,6,8 (before 1,2,3,4)
  // the second sub-layer contains only the ladders 1,3,5,7 (before 1,2,3,4)
  if (mDebugLevel>2) 
    cout << "SVTInit **** Finding neighbouring wafers ****"<<endl;  

  for (il=0; il<mNWafers; il++) {
    lay=(int)svggeom[il].id/1000;
    if(lay==8)
      lay=7;
    waf=(int)(svggeom[il].id-lay*1000)/100;
    lad=svggeom[il].id-lay*1000-waf*100;
    maxwaf=waf+1;
    minwaf=waf-1;
    switch(lay)
      {
      case 1:		// inner layers
      case 3:
      case 5:
	//	maxlad=lad;
	maxlad=lad+1;
	minlad=lad-1;
	lay2=lay+1;
	break;
      case 2:		// outer layers
      case 4:
      case 6:
	maxlad=lad+1;
	//	minlad=lad;
	minlad=lad-1;
	lay2=lay-1;
	break;
      case 7:		// SSD
	maxlad=lad+1;
	minlad=lad-1;
	lay2=lay;

	break;
      }	

    //    if(minlad<1)
    //      minlad=svgconf->n_ladder[lay2-1];
    if(minlad<1) {
      if (lay<7)
        minlad=svgconf->n_ladder[lay2-1]+svgconf->n_ladder[lay-1];
      else
        minlad=svgconf->n_ladder[lay-1];
    }
    //    if(maxlad>svgconf->n_ladder[lay2-1])
    //      maxlad=1;
    if (lay<7) {
      if(maxlad>svgconf->n_ladder[lay2-1]+svgconf->n_ladder[lay-1]) maxlad=1;
    }
    else {
      if(maxlad>svgconf->n_ladder[lay-1]) maxlad=1;
    }

    if(maxwaf<=svgconf->n_wafer[lay-1])
      {
	mIndexWaf[il]->neighbour[0]=mIndexWaf[mWafId2IndexWaf[lay2*1000+maxwaf*100+maxlad]];
	mIndexWaf[il]->neighbour[1]=mIndexWaf[mWafId2IndexWaf[lay*1000+maxwaf*100+lad]];
	mIndexWaf[il]->neighbour[2]=mIndexWaf[mWafId2IndexWaf[lay2*1000+maxwaf*100+minlad]];
      }
    else
      {
	mIndexWaf[il]->neighbour[0]=NULL;
	mIndexWaf[il]->neighbour[1]=NULL; //end of detector
	mIndexWaf[il]->neighbour[2]=NULL;
      }
    mIndexWaf[il]->neighbour[3]=mIndexWaf[mWafId2IndexWaf[lay2*1000+waf*100+maxlad]];
    mIndexWaf[il]->neighbour[4]=mIndexWaf[mWafId2IndexWaf[lay2*1000+waf*100+minlad]];
    if(minwaf>0)
      {
	mIndexWaf[il]->neighbour[5]=mIndexWaf[mWafId2IndexWaf[lay2*1000+minwaf*100+maxlad]];
	mIndexWaf[il]->neighbour[6]=mIndexWaf[mWafId2IndexWaf[lay*1000+minwaf*100+lad]];
	mIndexWaf[il]->neighbour[7]=mIndexWaf[mWafId2IndexWaf[lay2*1000+minwaf*100+minlad]];
      }
    else
      {
	mIndexWaf[il]->neighbour[5]=NULL;
	mIndexWaf[il]->neighbour[6]=NULL; //end of detector
	mIndexWaf[il]->neighbour[7]=NULL;

      }
  } // end of for(il=0; il<mNWafers; il++)
  

  // get SVT hits for each wafer

  if (mDebugLevel>2) 
    cout << "SVTInit **** Creating Hit objects ****"<<endl;  


  maxwaf=0;
  maxl=Stscsspt->GetNRows();
  mNSvtHit = maxl;

  mSvtHit  =  new StEstHit*[maxl];
  if(!mSvtHit){
    cerr<<"ERROR!!! not enougth memory"<<endl;
    cerr<<"StEstMaker::SVTInit mSvtHit = new StEstHit*["<<maxl<<"];"<<endl;
    return 1;
  }

  StThreeVectorD *xg;
  StThreeVectorD *xl;
  // we have to determine the maximum allowed use of a given hit
  // For a given branch it is limited to ntotbranch*max(nbranch(lay=3),nbranch(lay=2)..)
  long ExtremeBranching=mParams[0]->nbranch[3]*mParams[0]->onoff[3];
  for (il=2;il>=0;il--)
    if (mParams[0]->nbranch[il]*mParams[0]->onoff[il]>ExtremeBranching) 
      ExtremeBranching=mParams[0]->nbranch[il]*mParams[0]->onoff[il];
  ExtremeBranching=ExtremeBranching*mParams[0]->ntotbranch[0];
  cout<<"mSvtInit : ExtremeBranching = "<<ExtremeBranching<<endl;
  int ill=0;
  for(il=0; il<maxl; il++) //loop over SVT hits
    {
      if( scsspt[il].flag < 4){
	jl=mWafId2IndexWaf[scsspt[il].id_wafer];
	lay = mIndexWaf[jl]->GetLayer();
	
	xg = new StThreeVectorD(scsspt[il].x[0],scsspt[il].x[1],scsspt[il].x[2]);
	xl = new StThreeVectorD(scsspt[il].xl[0],scsspt[il].xl[1],0);
	
	//create a hit object
	// the total number of allowed use of the hit is 
	mSvtHit[ill] = new StEstHit(scsspt[il].id,xg,xl,ExtremeBranching,mParams[0]->share[lay],mIndexWaf[jl]);
	
	if(!mSvtHit[ill]){
	  cerr<<"ERROR!!! not enougth memory"<<endl;
	  cerr<<"StEstMaker::SVTInit mSvtHit["<<ill<<"] = new StEstHit(...);"<<endl;
	  return 1;
	}
	
	//add a hit to the wafer
	if (mIndexWaf[jl]->AddHit(mSvtHit[ill])==1) 
	  cerr << "ERROR!!! StEstMaker::SVTInit Too many hits on wafer #"<<scsspt[il].id_wafer<<endl;
	if (mIndexWaf[jl]->GetNHits()>maxwaf)
	  maxwaf = mIndexWaf[jl]->GetNHits();
	ill++;
      }
    }

  if(mDebugLevel>0)
    cout << "SVTInit **** Maximum number of hits per wafer=" << maxwaf << " ****" <<endl;
  
  if(mDebugLevel>0)
    cout << "SVTInit *** STOP ***"<<endl;  
  
  return 0;
}
 
int StEstTracker::TPCInit(St_tpt_track* Sttptrack,
			  St_tcl_tphit* Sttphit){

  if(mDebugLevel>1)
    cout << "TPCInit *** START ***"<<endl;  

  long il, jl, kl, maxl;
  long MaxIndex, MaxTPCTrack;
  long RowToFill;
  double c,dip,phase;
  int h;
  double r0[10000];  //!!!!!

  float x[3] = {0,0,0};
  float b[3];
  gufld(x,b);

  cout << " Using a field of " << b[2] << endl;

  // reading tables with TPC data

  tpt_track_st*    tptrack;
  tptrack      = Sttptrack->GetTable();
  tcl_tphit_st*    tphit;
  tphit        = Sttphit->GetTable();

  // first loop to determine the number (MaxTPCTrack) of good TPC tracks 
  // (flag>0) and the maximum id value (MaxIndex) of those tracks.
  MaxIndex =0;
  MaxTPCTrack = 0;
  for (il=0;il<Sttptrack->GetNRows();il++) {
    if (tptrack[il].flag>0) MaxTPCTrack++;
    if (tptrack[il].id>MaxIndex) MaxIndex=tptrack[il].id;
  }
  cout<<"number of tpc tracks : "<<MaxTPCTrack<<endl;
  cout<<"maximum tpc index : "<<MaxIndex+1<<endl;

  //we allocated the memory for the TPCTrack objects 
  mNTPCTrack   = MaxTPCTrack;
  mTPCTrack = new StEstTPCTrack*[MaxTPCTrack];
  if(!mTPCTrack) {
    cerr<<"ERROR!!! not enough memory"<<endl;
    cerr<<"StEstMaker::TPCInit mTPCTrack = new StEstTPCTrack*["<<mNTPCTrack<<"];"<<endl;
    return 1;
  }
  
  // we allocated the memory for the index table 
  // we need +1 since the id value run from 1 to XXX
  mTptIndex = new long[MaxIndex+1]; 
  if(!mTptIndex) {
    cerr<<"ERROR!!! not enough memory"<<endl;
    cerr<<"StEstMaker::TPCInit mTptIndex = new long["<<MaxIndex<<"];"<<endl;
    return 1;
  }

  for (il=0;il<MaxIndex+1;il++)
    mTptIndex[il] = -1;

  if(mDebugLevel>2)
    cout << "TPCInit **** Creating TPC tracks ****"<<endl;  

  StHelix *hel;
  StThreeVector<double> orig;
  StThreeVectorD *xhit, *xdhit;
  RowToFill=0;
  for (il=0;il<Sttptrack->GetNRows();il++) {
    if (tptrack[il].flag>0) {
      c = tptrack[il].invp*C_D_CURVATURE*b[2];
      orig.setX(tptrack[il].r0*cos((double)tptrack[il].phi0/C_DEG_PER_RAD));
      orig.setY(tptrack[il].r0*sin((double)tptrack[il].phi0/C_DEG_PER_RAD));
      orig.setZ(tptrack[il].z0);
      
      dip = atan(tptrack[il].tanl);
      h = ((b[2] * tptrack[il].q) > 0 ? -1 : 1);
      phase = (tptrack[il].psi)/C_DEG_PER_RAD-h*M_PI_2;

      hel = new StHelix(c,dip,phase,orig,h);
      if(!hel) {
	cerr<<"ERROR!!! not enough memory"<<endl;
	cerr<<"StEstMaker::TPCInit hel = new StHelix(c,dip,phase,orig,h);"<<endl;
	return 1;
      }
      
      mTPCTrack[RowToFill] = new StEstTPCTrack(tptrack[il].id, mParams[0]->maxtpchits, hel, 1/tptrack[il].invp);
      
      r0[RowToFill] = tptrack[il].r0; // !!!!
      
      if(!mTPCTrack[RowToFill]) {
	cerr<<"ERROR!!! not enough memory"<<endl;
	cerr<<"StEstMaker::TPCInit mTPCTrack["<<RowToFill<<"] = new StEstTPCTrack("<<il 
	    <<",mParams[0]->maxtpchits,hel);"<<endl;
	return 1;
      }
      
      mTPCTrack[RowToFill]->SetFlag(tptrack[il].flag);
      mTPCTrack[RowToFill]->mChiSqLin = tptrack[il].chisq[1];
      mTPCTrack[RowToFill]->mChiSqCir = tptrack[il].chisq[0];
      
      
      mTptIndex[tptrack[il].id]=RowToFill;
      RowToFill++;
    } 
  }

  // loop filing the St_TPCTrack with the TPC hits
  // flip the order of the tpc hits....
  if(mDebugLevel>2)
    cout << "*** INIT - Filling the TPC tracks with the hits ***"<<endl;  
  maxl=Sttphit->GetNRows();
  for(il=0; il<maxl; il++) {
    jl=tphit[il].track/1000;
    kl=tphit[il].track-1000*jl;
    jl=mTptIndex[jl];    
    if(jl!=-1) {
      kl=mParams[0]->maxtpchits-kl; 
      if(kl<mParams[0]->maxtpchits && kl>=0) {
	xhit = new StThreeVectorD(tphit[il].x,tphit[il].y,tphit[il].z);
	xdhit = new StThreeVectorD(tphit[il].dx,tphit[il].dy,tphit[il].dz);
	if(!mTPCTrack[jl]->AddHit(kl,xhit,xdhit,tphit[il].row,tphit[il].flag)) {
	  cout<<"ERROR!!! StEstMaker::TPCInit too many hits for TPCTrack["<<jl<<"] = " \
	      <<mTPCTrack[jl]->GetNHits()<<endl;
	  return 1;
	}
      }
    }
  }

  double rrr;

  for (il=0;il<mNTPCTrack;il++) {
    // sorting TPC hits on the radius
    mTPCTrack[il]->SortHits();
    if (mTPCTrack[il]->GetFlag()>0 && mTPCTrack[il]->GetNHits()>0) {
      rrr = sqrt(mTPCTrack[il]->mR[mTPCTrack[il]->mHitIndex[0]]->x()*mTPCTrack[il]->mR[mTPCTrack[il]->mHitIndex[0]]->x()
		 + mTPCTrack[il]->mR[mTPCTrack[il]->mHitIndex[0]]->y()*mTPCTrack[il]->mR[mTPCTrack[il]->mHitIndex[0]]->y());
      mTPCTrack[il]->SetR(rrr);
      if (fabs(r0[il] - rrr)>30) {
	cout << "HUGE DIFF= "<<fabs(r0[il] - rrr)<<" id= "<<mTPCTrack[il]->mId<<" pt= "<<mTPCTrack[il]->mPt<<" hit.r0= "<<rrr<<" tpc.r0= "<<r0[il]<<endl;
      }
    }
  }

  if(mDebugLevel>2)
    cout << "TPCInit **** STOP ***"<<endl;  
  
  return 0;	
}


int StEstTracker::SetupMc(St_scs_spt* Stscsspt,
			  St_tte_eval* Stevaltrk, 
			  St_g2t_track* Stg2ttrack,
			  St_g2t_vertex* Stg2tvertex){

  int i, j, IsolatedSvtHits;
  int Parent_p;


  IsolatedSvtHits=0;


  // EvalInit start
  if(mDebugLevel>2) cout << "EvalInit start" <<endl;

  // translation table (mcid -> est_id)
  Eval_id_mctrk2est_Track = new long[mNTPCTrack*10];
  if(!Eval_id_mctrk2est_Track){
    cerr<<"ERROR!!! not enough memory"<<endl;
    cerr<<"StEstMaker::Init Eval_id_mctrk2est_Track = new long["<< mNTPCTrack*10<<"];"<<endl;
    return 1;
  }

  //  int* mctest = new int[mNTPCTrack*10];

  for (i=0;i<mNTPCTrack*10;i++) {
    Eval_id_mctrk2est_Track[i]=-1;
    //    mctest[i] = 0;
  }

  
  tte_eval_st*   evaltrk      = Stevaltrk->GetTable();
  g2t_track_st*   g2t_track      = Stg2ttrack->GetTable();
  g2t_vertex_st*   g2t_vertex      = Stg2tvertex->GetTable();

  for(i=0; i<Stevaltrk->GetNRows(); i++) {
    if(evaltrk[i].mtrk<mNTPCTrack*10) {
      //      mctest[evaltrk[i].mtrk]++;
      // filling translation table; index is mcid, content is id of TPC track
      Eval_id_mctrk2est_Track[evaltrk[i].mtrk] = mTptIndex[evaltrk[i].rtrk];
      if (mTptIndex[evaltrk[i].rtrk]!=-1) {
	mTPCTrack[mTptIndex[evaltrk[i].rtrk]]->mMcId=evaltrk[i].mtrk;
	mTPCTrack[mTptIndex[evaltrk[i].rtrk]]->mPid=evaltrk[i].pid;
	mTPCTrack[mTptIndex[evaltrk[i].rtrk]]->mVid=evaltrk[i].vid;
	if (evaltrk[i].vid==1) mTPCTrack[mTptIndex[evaltrk[i].rtrk]]->mType=1;
	if (evaltrk[i].vid!=1) {
	  mTPCTrack[mTptIndex[evaltrk[i].rtrk]]->mType=2;
	  if (mTPCTrack[mTptIndex[evaltrk[i].rtrk]]->mPid==5 || mTPCTrack[mTptIndex[evaltrk[i].rtrk]]->mPid==6) {
	    for (j=0; j<Stg2tvertex->GetNRows();j++) 
	      if (g2t_vertex[j].id==mTPCTrack[mTptIndex[evaltrk[i].rtrk]]->mVid)
		Parent_p=g2t_vertex[j].parent_p;
	    for (j=0; j<Stg2ttrack->GetNRows();j++) {
	      if (g2t_track[j].id==Parent_p) {
		mTPCTrack[mTptIndex[evaltrk[i].rtrk]]->mParentPid=g2t_track[j].ge_pid;
		if(mTPCTrack[mTptIndex[evaltrk[i].rtrk]]->mParentPid==8 || mTPCTrack[mTptIndex[evaltrk[i].rtrk]]->mParentPid==9) 
		  mTPCTrack[mTptIndex[evaltrk[i].rtrk]]->mParentMcId=g2t_track[j].id;
	      }
	    }
	  }
	}
      }
    }
    else
      cerr << "ERROR StEstMaker::Init mctrk[i].mcid>=mNTPCTrack*10" << endl;
  }

  Eval_mchits = new StEstHit**[mNTPCTrack];
  if(!Eval_mchits){
    cerr<<"ERROR!!! not enougth memory"<<endl;
    cerr<<"StEstMaker::Init Eval_mchits = new StEstHit**["<< mNTPCTrack<<"];"<<endl;
    return 1;
  }

  for(i=0; i<mNTPCTrack; i++) {
    Eval_mchits[i] = new StEstHit*[10];
      if(!Eval_mchits[i]){
	cerr<<"ERROR!!! not enougth memory"<<endl;
	cerr<<"StEstMaker::Init Eval_mchits["<<i<<"] = new StEstHit*[10];"<<endl;
	return 1;
      }
      else {
	for(j=0; j<10; j++) Eval_mchits[i][j]=NULL;
      }
  }
  
  scs_spt_st*     scsspt;
  scsspt      = Stscsspt->GetTable();
  int HitLayer;
  int ChargedParent;
  // filling Eval_mchits table (for evaluation)
  // only the hits coming from a layer on are loaded
  // something to fix here : j goes to 8 but a space of 10 is allocated.
  for(i=0; i<Stscsspt->GetNRows(); i++) {
    HitLayer=(scsspt[i].id_wafer/1000)-1;
    switch (HitLayer)
      {
      case 1 :
      case 3 :
      case 5 :
      case 7 :
	HitLayer=HitLayer-1;
	break;
      }
    HitLayer=HitLayer/2;
    if(mParams[0]->onoff[HitLayer]==1) {
      if(Eval_id_mctrk2est_Track[scsspt[i].id_mctrack]!=-1 &&
	 mParams[0]->onoff[HitLayer]==1)
	for(j=0; j<8; j++) {
	  if(Eval_mchits[Eval_id_mctrk2est_Track[scsspt[i].id_mctrack]][j]==NULL){
	    Eval_mchits[Eval_id_mctrk2est_Track[scsspt[i].id_mctrack]][j]=mSvtHit[i];
	    break;
	  }
	}
      else {
	ChargedParent=-1;
	for (j=0;j<mNTPCTrack;j++) 
	  if (mTPCTrack[j]->mParentMcId==scsspt[i].id_mctrack) ChargedParent=j;
	
	if (ChargedParent>-1) {
	  for(j=0; j<8; j++) {
	    if(Eval_mchits[Eval_id_mctrk2est_Track[mTPCTrack[ChargedParent]->GetMcId()]][j]==NULL){
	      Eval_mchits[Eval_id_mctrk2est_Track[mTPCTrack[ChargedParent]->GetMcId()]][j]=mSvtHit[i];
	      break;
	    }
	  }	
	}
	else IsolatedSvtHits++;
      }
    }
  }
  
  if(mDebugLevel>0) cout << "Number of SVT/SSD hits without TPC tracks :"<<IsolatedSvtHits<<endl;
  if(mDebugLevel>0) cout << "EvalInit STOP" <<endl;

  //EvalInit STOP

  if(mDebugLevel>3) { //to check TPC flag 
    for (i=0;i<mNTPCTrack;i++) {
      j=0;
      while (Eval_mchits[i][j] !=NULL)
	j++;
      cout<<"i = "<<i<<"  nTPCHits = "<<mTPCTrack[i]->GetNHits()<<"  flag = "<<mTPCTrack[i]->GetFlag()<<"  nSVTHits = "<<j<<endl;
    }
  }
  return 0;
}
 
 









