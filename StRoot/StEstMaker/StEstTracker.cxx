/***************************************************************************
 *
 * $Id: StEstTracker.cxx,v 1.11 2002/01/31 21:10:00 caines Exp $ 
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Main methods for StEstTracker
 *
 ***************************************************************************
 *
 * $Log: StEstTracker.cxx,v $
 * Revision 1.11  2002/01/31 21:10:00  caines
 * Open est cuts up
 *
 * Revision 1.10  2001/07/15 20:31:31  caines
 * Fixes from Insure++ debugging
 *
 * Revision 1.9  2001/04/25 17:33:02  perev
 * HPcorrs
 *
 * Revision 1.8  2001/04/25 15:06:50  lmartin
 * Tracking parameters set to specific values for the 2nd superpass.
 *
 * Revision 1.7  2001/03/19 16:09:03  lmartin
 * GetVertexZ method added.
 *
 * Revision 1.6  2001/03/02 16:23:49  lmartin
 * New method CumulEval to save the tracking performances.
 *
 * Revision 1.5  2001/02/23 13:46:13  lmartin
 * Two arguments (hittmp,exclhit) of the RefitBranch method removed.
 *
 * Revision 1.4  2001/02/22 16:33:30  lmartin
 * most of the cout replaced by gMessMgr
 *
 * Revision 1.3  2001/02/01 13:00:26  lmartin
 * Correction of the cvs keywords to get the log in the file header.
 *
 **************************************************************************/
#include "StMessMgr.h"
#include "StEstTracker.h"
#include "StEstParams.hh"
#include "Infrastructure/StEstWafer.hh"
#include "Infrastructure/StEstBranch.hh"
#include "Infrastructure/StEstHit.hh"
#include "Infrastructure/StEstTrack.hh"
#include "Infrastructure/StEstTPCTrack.hh"

ClassImp(StEstTracker)

StEstTracker::StEstTracker(int npass,
			   int nsuperpass,
			   int idealtracking,
			   int debuglevel,
			   StEstParams** params,
			   StEstSegments** segments,
			   St_egr_egrpar* egr_egrpar,
			   table_head_st* egrpar_h) {
  mNPass=npass;
  mPass=0;
  mNSuperPass=nsuperpass;
  mSuperPass=0;
  mIdealTracking=idealtracking;
  mDebugLevel=debuglevel;
  mParams=params;
  mSegments=segments;
  m_egr_egrpar=egr_egrpar;
  m_egrpar_h=egrpar_h;
  mPhiBin=10;
  mNPhiBins=36;
  mZBin=4;
  mNZBins=18;
  mNIdealPrim=0;
  mNIdealSeco=0;
  mNGoodPrim=0;
  mNGoodSeco=0;
  mNBadPrim=0;
  mNBadSeco=0;
}

StEstTracker::~StEstTracker() {

}

Int_t StEstTracker::DoTracking() {
  int TrackDeadBeforeSelection,TrackDeadAfterSelection;
  int TrackDeadBeforeSegment,TrackDeadAfterSegment;
  int TrackDeadBeforeBest,TrackDeadAfterBest;
  int TrackDeadBeforeRemoveSharing,TrackDeadAfterRemoveSharing;
  int NTrackPresented,NTrackFormed;
  int NTrackPresentedGood;
  int OneIsGood;
  int i,j;
  int ihita[4],ihitaold[4];
  int ihitb[4],ihitbshared[4];


  if(mDebugLevel>0)
    gMessMgr->Info()<<"StEstTracker::DoTracking() ****START****"<<endm;
  if(mIdealTracking==1){
    BuildIdealBranches();
    BuildFindableBranches();
  }

  if (!mTrack) return 1;
  int slay,onoffmatrix,nminhit;
  
  for (i=0;i<4;i++) ihita[i]=0;
  for (i=0;i<mNSvtHit;i++) {
    if (mSvtHit[i]->GetFlag()==0) ihita[mSvtHit[i]->GetWafer()->GetLayer()]++;
  }

  gMessMgr->Info()<<"Hit density (0123) :\t"<<ihita[0]<<"\t\t"<<ihita[1]<<"\t\t"<<ihita[2]<<"\t\t"<<ihita[3]<<endm;
  for(mSuperPass=0; mSuperPass<mNSuperPass; mSuperPass++) {
    if (mSuperPass>0) {
      for (mPass=0;mPass<mNPass;mPass++) {
	cout<< " Setting params here" << endl;
	mParams[mPass]->geomcutl[2] = 5.;
	mParams[mPass]->geomcutl[1] = 5.;
	mParams[mPass]->geomcutl[0] = 5.;
	mParams[mPass]->geomcutw[2] = 5.;
	mParams[mPass]->geomcutw[1] = 5.;
	mParams[mPass]->geomcutw[0] = 5.;
      }
    }
    FlagTPCTracksSP(mSuperPass);
    onoffmatrix=0;
    for (j=0;j<4;j++) 
      if (mSegments[mSuperPass]->slay[j]==2 && mParams[0]->onoff[j]!=0) 
	onoffmatrix |= ((1<<j));
    nminhit=mSegments[mSuperPass]->minhits;
    for (mPass=0;mPass<mNPass;mPass++) {
      gMessMgr->Info()<<"Super = "<<mSuperPass<<" Pass = "<<mPass<<endm;
      // locally mark the tracks which should be considered
      NTrackPresented=0;
      NTrackPresentedGood=0;
      for (i=0;i<mNTrack;i++) {
	mTrack[i]->SetDoIt(1);
	if(mTrack[i]->mTPCTrack->mPt<=mParams[mPass]->ptmin || 
	   mTrack[i]->mTPCTrack->mPt>mParams[mPass]->ptmax ||
	   mTrack[i]->mTPCTrack->GetFlag()<0 || 
	   mTrack[i]->mTPCTrack->GetFlagSP()<0 || 
	   mTrack[i]->GetFlag()!=0) mTrack[i]->SetDoIt(0);
	else {
	  NTrackPresented++;
	  if (onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
	      mTrack[i]->GetIdealPattern()>=nminhit)
	  NTrackPresentedGood++;
	}
      }
      // Calling the Tracking method
      for (slay=3;slay>=0;slay--) {
	if(mParams[mPass]->onoff[slay])  Tracking(slay);

	if (mIdealTracking==1 && mDebugLevel>0) {
	  TrackDeadBeforeSegment=0;
	  for (i=0;i<mNTrack;i++) 
	    if (mTrack[i]->GetDoIt()==1) {
	      OneIsGood=0;
	      for (j=0;j<mTrack[i]->GetNBranches();j++)
		if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	      if (OneIsGood==0 && 
		  onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
		  mTrack[i]->GetIdealPattern()>=nminhit) {
		TrackDeadBeforeSegment++;
	      }
	    }
	}
	// Removing dead branches
	ChooseSegment(mSuperPass,slay);
	
	if (mIdealTracking==1 && mDebugLevel>0) {
	  TrackDeadAfterSegment=0;
	  for (i=0;i<mNTrack;i++) 
	    if (mTrack[i]->GetDoIt()==1) {
	      OneIsGood=0;
	      for (j=0;j<mTrack[i]->GetNBranches();j++)
		if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	      if (OneIsGood==0 && 
		  onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
		  mTrack[i]->GetIdealPattern()>=nminhit) {
		TrackDeadAfterSegment++;
	      }
	    }
	  gMessMgr->Info()<<"slay "<<slay<<" TrackDead (b/a) ChooseSegment = "<<TrackDeadBeforeSegment<<"  "<<TrackDeadAfterSegment<<endm;
	}
	TrackDeadBeforeSelection=0;
	TrackDeadAfterSelection=0;
	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1) {
	    if (mIdealTracking==1 && mDebugLevel>0) {
	      OneIsGood=0;
	      for (j=0;j<mTrack[i]->GetNBranches();j++)
		if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	      if (OneIsGood==0 && 
		  onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
		  mTrack[i]->GetIdealPattern()>=nminhit)
		TrackDeadBeforeSelection++;
	    }
	    // We keep only N branches for each track
	    ChooseBestNBranches(mTrack[i], slay);
	    if (mIdealTracking==1 && mDebugLevel>0) {
	    OneIsGood=0;
	    for (j=0;j<mTrack[i]->GetNBranches();j++)
	      if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	    if (OneIsGood==0 && 
		onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
		mTrack[i]->GetIdealPattern()>=nminhit)
	      TrackDeadAfterSelection++;
	    }
	  }
	if (mIdealTracking==1 && mDebugLevel>0)
	gMessMgr->Info()<<"slay "<<slay<<" TrackDead (b/a) ChooseBestNBranches = "<<TrackDeadBeforeSelection<<"  "<<TrackDeadAfterSelection<<endm;
	
      }// for (slay=3..... 

      TrackDeadBeforeBest=0;
      if (mIdealTracking==1 && mDebugLevel>0) {
	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1) {
	    OneIsGood=0;
	    for (j=0;j<mTrack[i]->GetNBranches();j++)
	      if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	    if (OneIsGood==0 && 
		onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
		mTrack[i]->GetIdealPattern()>=nminhit)
	      TrackDeadBeforeBest++;
	  }
      }
      // choosing the best branch for each track and killing the others
      for (i=0;i<mNTrack;i++) {
	if(mTrack[i]->GetDoIt()==1){
	  ChooseBestBranch(mTrack[i], mSuperPass);    
	}
      }
      TrackDeadAfterBest=0;
      if (mIdealTracking==1 && mDebugLevel>0) {
	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1) {
	    OneIsGood=0;
	    for (j=0;j<mTrack[i]->GetNBranches();j++)
	      if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	    if (OneIsGood==0 && 
		onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
		mTrack[i]->GetIdealPattern()>=nminhit)
	      TrackDeadAfterBest++;
	  }
	gMessMgr->Info()<<"--> TrackDead (b/a) ChooseBest = "<<TrackDeadBeforeBest<<"  "<<TrackDeadAfterBest<<endm;
      }
      
      NTrackFormed=0;
	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1 && mTrack[i]->GetFlag()==1) NTrackFormed++;
      gMessMgr->Info()<<"Number of tracks presented/should be formed/formed during the pass : "
	  <<NTrackPresented<<"/"<<NTrackPresentedGood<<"/"<<NTrackFormed<<endm;

      for (i=0;i<4;i++) {
	ihitaold[i]=ihita[i];
	ihita[i]=0;
      }
      for (i=0;i<mNSvtHit;i++) {
	if (mSvtHit[i]->GetFlag()==0) ihita[mSvtHit[i]->GetWafer()->GetLayer()]++;
      }
      gMessMgr->Info()<<"Hit density (0123) :"
		      <<"\t"<<ihita[0]<<"\t"<<ihitaold[0]-ihita[0]
		      <<"\t"<<ihita[1]<<"\t"<<ihitaold[1]-ihita[1]
		      <<"\t"<<ihita[2]<<"\t"<<ihitaold[2]-ihita[2]
		      <<"\t"<<ihita[3]<<"\t"<<ihitaold[3]-ihita[3]<<endm;
      // We need to flag the track process in the current pass 
      // in order to be considered by the method applied (FinishFlag...)
      // at the end of the superpasses.
      	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1)  mTrack[i]->SetDone(1);
    } // for (mPass=0.......
    mPass--;
    // studing the hit sharing.
    gMessMgr->Info()<<"Studying the hit sharing"<<endm;
    for (i=0;i<4;i++) {
      ihitb[i]=0;
      ihitbshared[i]=0;
    }
    for (i=0;i<mNSvtHit;i++)       
      if (mSvtHit[i]->GetNBranch()!=0) {
	ihitb[mSvtHit[i]->GetWafer()->GetLayer()]++;
	if (mSvtHit[i]->GetNShare()>1) 
	  ihitbshared[mSvtHit[i]->GetWafer()->GetLayer()]++;
      }
    
    gMessMgr->Info()<<"Hits used (0123) :"
		    <<"\t"<<ihitb[0]
		    <<"\t"<<ihitb[1]
		    <<"\t"<<ihitb[2]
		    <<"\t"<<ihitb[3]<<endm;
    gMessMgr->Info()<<"Hits shared (0123) :"
		    <<"\t"<<ihitbshared[0]
		    <<"\t"<<ihitbshared[1]
		    <<"\t"<<ihitbshared[2]
		    <<"\t"<<ihitbshared[3]<<endm;
    
    // choosing best branches for each hit
    TrackDeadBeforeRemoveSharing=0;
    for (i=0;i<mNTrack;i++) 
      if (mTrack[i]->GetDone()==1) {
	OneIsGood=0;
	for (j=0;j<mTrack[i]->GetNBranches();j++)
	  if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	if (OneIsGood==0 && 
	    onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
	    mTrack[i]->GetIdealPattern()>=nminhit)
	  TrackDeadBeforeRemoveSharing++;
      }
    RemoveHitSharing2();
    
    TrackDeadAfterRemoveSharing=0;
    for (i=0;i<mNTrack;i++) 
      if (mTrack[i]->GetDone()==1) {
	OneIsGood=0;
	for (j=0;j<mTrack[i]->GetNBranches();j++)
	  if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	if (OneIsGood==0 && 
	    onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
	    mTrack[i]->GetIdealPattern()>=nminhit)
	  TrackDeadAfterRemoveSharing++;
      }
    gMessMgr->Info()<<"--> TrackDead (b/a) RemoveSharing = "<<TrackDeadBeforeRemoveSharing<<"  "<<TrackDeadAfterRemoveSharing<<endm;
    

    // flagging the tracks which we assume as found
    FinishFlag();
    // reinitialize the branch helix of the track which are dropped
    ReInitializeHelix();

    // Doing the evaluation for the superpass.
    if(mDebugLevel>=0 && mIdealTracking==1) Eval(onoffmatrix,nminhit);
    //      Eval(0,1);
  }// for(mSuperPass...

  if(mDebugLevel>0)
    gMessMgr->Info()<<"StEstTracker::DoTracking() ****STOP****"<<endm;
  
  return kStOK;
} 


void StEstTracker::BuildIdealBranches() {

  int nseg,matrix,l[4],idealnhits;
  int iret,flaglog[8];
  int fitstatus;
  int i,j,slay,mcid,nsvthit;
  int IsolatedTPCTracks,AssociatedTPCTracks;
  double dist,distw,distl,sd;
  double dca;
  StEstBranch *branch;
  StThreeVector<double> Proj;
  StThreeVector<double> XWaf;
  StThreeVector<double> NWaf;


  if (mDebugLevel>0) gMessMgr->Info()<<"StEstTracker::BuildIdealBranches Starting"<<endm;
  IsolatedTPCTracks=0;
  AssociatedTPCTracks=0;
  for (i=0;i<8;i++) flaglog[i]=0;
  for (i=0;i<mNTrack;i++) {
    if (mTrack[i]->mTPCTrack->GetFlag()>0) {
      if (mTrack[i]->mTPCTrack->mPt>mParams[mNPass-1]->ptmin && mTrack[i]->mTPCTrack->mPt<mParams[0]->ptmax) {
	mcid = Eval_id_mctrk2est_Track[mTrack[i]->mTPCTrack->GetMcId()];
	nseg=0;
	for (j=0;j<mNTrack;j++)
	  if (mTrack[i]->mTPCTrack->GetFlag()>0 && Eval_id_mctrk2est_Track[mTrack[j]->mTPCTrack->GetMcId()]==mcid)
	    nseg++;
	// get the ideal branch pattern= sum(2^layer)
	// and the ideal number of hits (counting one/layer)
	j=0;
	matrix=0;
	l[0]=0;
	l[1]=0;
	l[2]=0;
	l[3]=0;
	idealnhits=0;
	while (Eval_mchits[mcid][j]!=NULL && j<10) {
	  matrix= matrix | ((1<<Eval_mchits[mcid][j]->GetWafer()->GetLayer()));
	  if (l[Eval_mchits[mcid][j]->GetWafer()->GetLayer()]==0) 
	    l[Eval_mchits[mcid][j]->GetWafer()->GetLayer()]++;
	  j++;
	}
	nsvthit=j;
	
	for (j=0;j<4;j++) idealnhits=idealnhits+l[j];
	
	if (matrix==0) IsolatedTPCTracks++;
	if (matrix!=0) AssociatedTPCTracks++;
	mTrack[i]->SetIdealPattern(matrix);
	mTrack[i]->SetIdealNHits(idealnhits);

	branch = new StEstBranch(NULL, int(mParams[0]->maxsvthits));
	if (branch==NULL)
	  gMessMgr->Error()<<"ERROR StEstTracker::BuildIdealBranches branch==NULL"<<endm;
	else { // the perfect branch has been created
	  branch->JoinTrack(mTrack[i],1);
	  StThreeVector<double> temp(mVertex->GetGlobX()->x(),
				     mVertex->GetGlobX()->y(),
				     mVertex->GetGlobX()->z());
	  dca=mTrack[i]->mTPCTrack->GetHelix()->distance(temp); 
	  // copy of the tpc helix.
	  StHelix *helix_for_idealbranch = new StHelix(*mTrack[i]->mTPCTrack->GetHelix());
	  branch->SetHelix(helix_for_idealbranch);


      	  if (dca<3.) {
   	    iret=RefitBranch(branch,1.,&fitstatus);
	    if (iret!=1)  flaglog[7]++;
	    else {
	      if (fitstatus==1) flaglog[0]++;
	      else flaglog[abs(fitstatus)]++;
	    }
	  }  
    	  else {
    	    iret=RefitBranch(branch,0.,&fitstatus);
	    if (iret!=1)  flaglog[7]++;
	    else {
	      if (fitstatus==1) flaglog[0]++;
	      else flaglog[abs(fitstatus)]++;
	    }
	  }
	  for (slay=3;slay>=0;slay--) {
	    if(mParams[mPass]->onoff[slay]) {
	      j=0;
	      while (Eval_mchits[mcid][j]!=NULL && j<10) {      
		if (Eval_mchits[mcid][j]->GetWafer()->GetLayer()==slay){
		  XWaf.setX(Eval_mchits[mcid][j]->GetWafer()->GetX()->x());
		  XWaf.setY(Eval_mchits[mcid][j]->GetWafer()->GetX()->y());
		  XWaf.setZ(Eval_mchits[mcid][j]->GetWafer()->GetX()->z());
		  NWaf.setX(Eval_mchits[mcid][j]->GetWafer()->GetN()->x());
		  NWaf.setY(Eval_mchits[mcid][j]->GetWafer()->GetN()->y());
		  NWaf.setZ(Eval_mchits[mcid][j]->GetWafer()->GetN()->z());
		  sd=branch->GetHelix()->pathLength(XWaf,NWaf);
		  if (sd<1000) {
		    Proj=branch->GetHelix()->at(sd);
		    dist=sqrt((Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x())*
			      (Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x()) + 
			      (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y())*
			      (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y()) +
			      (Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z())*
			      (Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z()));
		    distw=sqrt((Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x())*
			       (Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x()) + 
			       (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y())*
			       (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y()));
		    distl=sqrt((Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z())*
			       (Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z()));
		    
		    branch->AddHit(Eval_mchits[mcid][j],dist);
		    iret=RefitBranch(branch,0,&fitstatus);
		    if (iret!=1)  flaglog[7]++;
		    else {
		      if (fitstatus==1) flaglog[0]++;
		      else flaglog[abs(fitstatus)]++;
		    }
		  }
		}
		j++;
	      }
	    }
	  }
	}
      }
    }
  }
  if (mDebugLevel>0) gMessMgr->Info()<<"Number of TPC Tracks without SVT/SSD hits :"<<IsolatedTPCTracks<<endm;
  if (mDebugLevel>0) gMessMgr->Info()<<"Number of TPC Tracks with    SVT/SSD hits :"<<AssociatedTPCTracks<<endm;
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstTracker::BuildIdealBranches Finished"<<endm;
}


void StEstTracker::BuildFindableBranches() {

  int matrix;
  int iret,flaglog[8];
  int fitstatus;
  int i,j,slay,mcid;
  int CorrectPass;
  double dist,distw,distl,sd;
  double dca;
  StEstBranch *branch;
  StThreeVector<double> Proj;
  StThreeVector<double> XWaf;
  StThreeVector<double> NWaf;

  if (mDebugLevel>0) gMessMgr->Info()<<"StEstTracker::BuildFindableBranches Starting"<<endm;
  for (i=0;i<8;i++) flaglog[i]=0;
  for (i=0;i<mNTrack;i++) {
    if (mTrack[i]->mTPCTrack->GetFlag()>0) {
      if (mTrack[i]->mTPCTrack->mPt>mParams[mNPass-1]->ptmin && mTrack[i]->mTPCTrack->mPt<mParams[0]->ptmax) {

	CorrectPass=mNPass-1;
 	for (j=0;j<mNPass;j++)
 	  if (mTrack[i]->mTPCTrack->mPt>mParams[j]->ptmin && 
 	      mTrack[i]->mTPCTrack->mPt<mParams[j]->ptmax) CorrectPass=j;
	mcid = Eval_id_mctrk2est_Track[mTrack[i]->mTPCTrack->GetMcId()];
	branch = new StEstBranch(NULL, int(mParams[0]->maxsvthits));
	if (branch==NULL)
	  gMessMgr->Error()<<"ERROR StEstTracker::BuildFindableBranches branch==NULL"<<endm;
	else { // the perfect branch has been created
	  branch->JoinTrack(mTrack[i],2);
	  StThreeVector<double> temp(mVertex->GetGlobX()->x(),
				     mVertex->GetGlobX()->y(),
				     mVertex->GetGlobX()->z());
	  dca=mTrack[i]->mTPCTrack->GetHelix()->distance(temp); 
	  // copy of the tpc helix.
	  StHelix *helix_for_findablebranch = new StHelix(*mTrack[i]->mTPCTrack->GetHelix());
	  branch->SetHelix(helix_for_findablebranch);


      	  if (dca<3.) {
   	    iret=RefitBranch(branch,1.,&fitstatus);
	    if (iret!=1)  flaglog[7]++;
	    else {
	      if (fitstatus==1) flaglog[0]++;
	      else flaglog[abs(fitstatus)]++;
	    }
	  }	  
    	  else {
    	    iret=RefitBranch(branch,0.,&fitstatus);
	    if (iret!=1)  flaglog[7]++;
	    else {
	      if (fitstatus==1) flaglog[0]++;
	      else flaglog[abs(fitstatus)]++;
	    }
	  }
	  for (slay=3;slay>=0;slay--) {
	    if(mParams[mPass]->onoff[slay]) {
	      j=0;
	      while (Eval_mchits[mcid][j]!=NULL && j<10) {      
		if (Eval_mchits[mcid][j]->GetWafer()->GetLayer()==slay){
		  XWaf.setX(Eval_mchits[mcid][j]->GetWafer()->GetX()->x());
		  XWaf.setY(Eval_mchits[mcid][j]->GetWafer()->GetX()->y());
		  XWaf.setZ(Eval_mchits[mcid][j]->GetWafer()->GetX()->z());
		  NWaf.setX(Eval_mchits[mcid][j]->GetWafer()->GetN()->x());
		  NWaf.setY(Eval_mchits[mcid][j]->GetWafer()->GetN()->y());
		  NWaf.setZ(Eval_mchits[mcid][j]->GetWafer()->GetN()->z());
		  sd=branch->GetHelix()->pathLength(XWaf,NWaf);

		  if (sd<1000) {
		    Proj=branch->GetHelix()->at(sd);
		    dist=sqrt((Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x())*
			      (Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x()) + 
			      (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y())*
			      (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y()) +
			      (Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z())*
			      (Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z()));
		    distw=sqrt((Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x())*
			       (Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x()) + 
			       (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y())*
			       (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y()));
		    distl=sqrt((Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z())*
			       (Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z()));
		    if (distl<mParams[CorrectPass]->geomcutl[slay] && 
			distw<mParams[CorrectPass]->geomcutw[slay]) {
		      branch->AddHit(Eval_mchits[mcid][j],dist);
		      iret=RefitBranch(branch,0,&fitstatus);
		      if (iret!=1)  flaglog[7]++;
		      else {
			if (fitstatus==1) flaglog[0]++;
			else flaglog[abs(fitstatus)]++;
		      }
		    }
		  }
		  else dist=999.;
		}
		j++;
	      }

	    }
	  }
	}
	matrix=0;
	for (j=0;j<branch->GetNHits();j++) 
	  matrix= matrix | ((1<<branch->GetHit(j)->GetWafer()->GetLayer()));
	mTrack[i]->SetFindablePattern(matrix);
	mTrack[i]->SetFindableNHits(branch->GetNHits());
      }
    }
  }
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstTracker::BuildFindableBranches Finished"<<endm;
}


void StEstTracker::PrintTrackDetails(int trackid) {

  int i,lm,lm2;

  for (i=0;i<mNTrack;i++) {
    if (mTrack[i]->mTPCTrack->mId==trackid) {
      cout<<"------------------------------------------------------------"<<endl;
      cout<<"Track id "<<trackid
	  <<" mcid="<<mTrack[i]->mTPCTrack->GetMcId()
	  <<" type="<<mTrack[i]->mTPCTrack->mType
	  <<" Pt(tpc)="<<mTrack[i]->mTPCTrack->mPt
	  <<" Nhits(tpc)="<<mTrack[i]->mTPCTrack->GetNHits()
	  <<" R(tpc)="<<mTrack[i]->mTPCTrack->GetR()<<endl;
      cout<<"Flag= "<<mTrack[i]->GetFlag()
	  <<" Pid= "<<mTrack[i]->mTPCTrack->GetPid()
	  <<" ParentPid= "<<mTrack[i]->mTPCTrack->mParentPid 
	  <<" ParentMcId= "<<mTrack[i]->mTPCTrack->mParentMcId<<endl; 
      cout<<"mcid shared with track id : ";
      if (mTrack[i]->mTPCTrack->GetMcId()!=0) 
	for (lm=0;lm<mNTPCTrack;lm++) {
	  if (mTPCTrack[lm]->GetMcId()==mTrack[i]->mTPCTrack->GetMcId()&&mTPCTrack[lm]->GetId()!=trackid)
	    cout<<mTPCTrack[lm]->GetId()
		<<" (pt="<<mTPCTrack[lm]->mPt
		<<" nhits="<<mTPCTrack[lm]->GetNHits()
		<<" r="<<mTPCTrack[lm]->GetR()<<") ";
	}
      cout<<endl;
      cout<<"Ideal Pattern : "<<mTrack[i]->GetIdealPattern()<<" IdealNHits : "<<mTrack[i]->GetIdealNHits()<<endl;
      if (mTrack[i]->mIdealBranch) {
	cout<<"Ideal branch : "<<mTrack[i]->mIdealBranch->GetNHits()<<" hit_id (dist) : ";
	for (lm=0;lm<mTrack[i]->mIdealBranch->GetNHits();lm++) 
	  cout<<mTrack[i]->mIdealBranch->GetHit(lm)->GetId()<<" ("
	      <<mTrack[i]->mIdealBranch->GetDist(lm)<<") ";
	cout<<endl;
	cout<<"Ideal branch :  wafer_id for the hits : ";
	for (lm=0;lm<mTrack[i]->mIdealBranch->GetNHits();lm++) 
	  cout<<mTrack[i]->mIdealBranch->GetHit(lm)->GetWafer()->GetId()<<" ";
	cout<<endl;
      }
      else cout<<"No Ideal branch"<<endl;
      cout<<"Findable Pattern : "<<mTrack[i]->GetFindablePattern()
	  <<" FindableNHits : "<<mTrack[i]->GetFindableNHits()<<endl;
      if (mTrack[i]->mFindableBranch) {
	cout<<"Findable branch : "<<mTrack[i]->mFindableBranch->GetNHits()<<" hit_id wafer_id sharing flag dist : "<<endl;
	for (lm=0;lm<mTrack[i]->mFindableBranch->GetNHits();lm++) {
	  cout<<"\t"<<mTrack[i]->mFindableBranch->GetHit(lm)->GetId()
	      <<"\t"<<mTrack[i]->mFindableBranch->GetHit(lm)->GetWafer()->GetId()
	      <<"\t"<<mTrack[i]->mFindableBranch->GetHit(lm)->GetNShare()
	      <<"\t"<<mTrack[i]->mFindableBranch->GetHit(lm)->GetFlag()
	      <<"\t"<<mTrack[i]->mFindableBranch->GetDist(lm);
	  for (lm2=0;lm2<mTrack[i]->mFindableBranch->GetHit(lm)->GetNBranch();lm2++)
	    cout<<" "<<mTrack[i]->mFindableBranch->GetHit(lm)->GetBranch(lm2)->GetTrack()->mTPCTrack->GetId();
	  cout<<endl;
	}
      }
      else cout<<"No Findable branch"<<endl;
      cout<<"From the tracking : Nbranches="<<mTrack[i]->GetNBranches()<<endl;
      for (lm=0;lm<mTrack[i]->GetNBranches();lm++) {
	cout<<"br,good,goodold,nhit,nfit,fitstatus,chisq,hit_id,waf_id,dist : "<<lm
	    <<" "<<mTrack[i]->GetBranch(lm)->GetIsGood()
	    <<" "<<mTrack[i]->GetBranch(lm)->GetIsGoodOld()
	    <<" "<<mTrack[i]->GetBranch(lm)->GetNHits()
	    <<" "<<mTrack[i]->GetBranch(lm)->GetNFit()
	    <<" "<<mTrack[i]->GetBranch(lm)->mLastFitStatus
	    <<" "<<mTrack[i]->GetBranch(lm)->GetChiSq()<<endl;
        for (lm2=0;lm2<mTrack[i]->GetBranch(lm)->GetNHits();lm2++) {
          cout<<mTrack[i]->GetBranch(lm)->GetHit(lm2)->mId<<"\t"
	      <<mTrack[i]->GetBranch(lm)->GetHit(lm2)->GetWafer()->GetId()<<"\t"
	      <<mTrack[i]->GetBranch(lm)->GetDist(lm2)
	      <<" ("<<mTrack[i]->GetBranch(lm)->GetHit(lm2)->GetNBranch()<<") "<<endl;
        }
      }
      cout<<"------------------------------------------------------------"<<endl;
    }
  }
}

void StEstTracker::CleanUp(){

  // 03/01/01 lm
  // the Track destructor is first called which delete the TPCTrack
  // release the hits from the branch and delete the branches.
  // the IndexGeom is reset and followed by the wafer deletions.
  int i,j,NHitStillAttached,NBranchPossessing;
  if (mDebugLevel>0) gMessMgr->Info()<<"StEstTracker::CleanUp : Starting"<<endm;
  if (mDebugLevel>0) gMessMgr->Info()<<" Before mNSvtHit="<<mNSvtHit<<endm;
  NHitStillAttached=0;
  NBranchPossessing=0;
  for (i=0;i<mNSvtHit;i++) {
    if (mSvtHit[i]->mNBranch!=0) {
      NHitStillAttached=NHitStillAttached+1;
      NBranchPossessing=NBranchPossessing+mSvtHit[i]->mNBranch;
      for (j=0;j<mSvtHit[i]->mNBranch;j++)
	if(!mSvtHit[i]->mBranch[j]) gMessMgr->Error()<<"Error fake branch"<<endm;
    }
  }
  if (mDebugLevel>0) gMessMgr->Info()<<" Number of hits attached : "<<NHitStillAttached<<endm;
  if (mDebugLevel>0) gMessMgr->Info()<<" Number of branchess possessing the hits : "<<NBranchPossessing<<endm;
  if (mDebugLevel>0) gMessMgr->Info()<<"CleanUp : deleting the "<<mNTrack<<" Tracks"<<endm;
  for (i=0;i<mNTrack;i++) {
    if (mTrack[i]->mNBranch<1) gMessMgr->Error()<<"Warning, this track does not have any branch"<<endm;
    mTrack[i]->StEstTrackDestructor();
    delete mTrack[i];
  }
  delete [] mTrack;
  delete [] mTPCTrack;
  mIndexGeom->ResetWafTab();
  delete mIndexGeom;
  // Delete the Eval_mchits array.
  if (mIdealTracking==1) {
    for (i=0;i<mNTPCTrack;i++)
      for (j=0; j<10; j++) Eval_mchits[i][j]=NULL;
    for (i=0;i<mNTPCTrack;i++)
      delete [] Eval_mchits[i];
    delete [] Eval_mchits;
  }

  for (i=0;i<mNSvtHit;i++) mSvtHit[i]->DetachFromWafer();
  for (i=0;i<mNSvtHit;i++) 
    delete mSvtHit[i];
  delete [] mSvtHit;
  delete mVertex;

  for (i=0;i<mNWafers;i++) delete mIndexWaf[i];
  delete [] mIndexWaf;

  delete [] mTptIndex;
  delete [] Eval_id_mctrk2est_Track;

  if (mDebugLevel>0) gMessMgr->Info()<<" CleanUp : Done"<<endm;

}

void StEstTracker::CumulEval(int* CumulIdealPrim,
			     int* CumulIdealSeco,
			     int* CumulGoodPrim,
			     int* CumulGoodSeco,
			     int* CumulBadPrim,
			     int* CumulBadSeco,
			     int* CumulEvents){
  *CumulIdealPrim=*CumulIdealPrim+mNIdealPrim;
  *CumulIdealSeco=*CumulIdealSeco+mNIdealSeco;
  *CumulGoodPrim=*CumulGoodPrim+mNGoodPrim;
  *CumulGoodSeco=*CumulGoodSeco+mNGoodSeco;
  *CumulBadPrim=*CumulBadPrim+mNBadPrim;
  *CumulBadSeco=*CumulBadSeco+mNBadSeco;
  *CumulEvents=*CumulEvents+1;
}

float StEstTracker::GetVertexZ() {
  return mVertex->GetGlobX()->z();
}
