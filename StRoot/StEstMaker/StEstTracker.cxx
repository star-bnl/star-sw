/***************************************************************************
 *
 *  
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Main methods for StEstTracker
 *
 ***************************************************************************
 *
 * 
 **************************************************************************/
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
  cout<<"-------------------------------------------------------------> StEstTracker Constructor "<<endl;
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

  
  cout<<"mNPass="<<mNPass<<endl;
  cout<<"mNSuperPass="<<mNSuperPass<<endl;
  cout<<"mIdealTracking="<<mIdealTracking<<endl;
  cout<<"mDebugLevel="<<mDebugLevel<<endl;

}

StEstTracker::~StEstTracker() {
  cout<<"-------------------------------------------------------------> StEstTracker Destructor "<<endl;
}
Int_t StEstTracker::DoTracking() {

  cout<<"-------------------------------------------------------------> StEstTracker DoTracking "<<endl;
  long TrackDeadBeforeSelection,TrackDeadAfterSelection;
  long TrackDeadBeforeSegment,TrackDeadAfterSegment;
  long TrackDeadBeforeBest,TrackDeadAfterBest;
  long TrackDeadBeforeRemoveSharing,TrackDeadAfterRemoveSharing;
  long NTrackPresented,NTrackFormed;
  long NTrackPresentedGood;
  int OneIsGood;
  long i,j;
  long ihita[4],ihitaold[4];
  long ihitb[4],ihitbshared[4];


  if(mParams[mPass]->debug>0)
    cout<<"StEstTracker::DoTracking() ****START****"<<endl;
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
  cout<<"Hit density (0123) :";
  cout<<"\t"<<ihita[0]<<"\t\t"<<ihita[1]<<"\t\t"<<ihita[2]<<"\t\t"<<ihita[3]<<endl;
  for(mSuperPass=0; mSuperPass<mNSuperPass; mSuperPass++) {
    FlagTPCTracksSP(mSuperPass);
    onoffmatrix=0;
    for (j=0;j<4;j++) 
      if (mSegments[mSuperPass]->slay[j]==2 && mParams[0]->onoff[j]!=0) 
	onoffmatrix |= int(pow(2,j));
    nminhit=mSegments[mSuperPass]->minhits;
    for (mPass=0;mPass<mNPass;mPass++) {
      cout<<"Super = "<<mSuperPass<<" Pass = "<<mPass<<endl;
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

	if (mIdealTracking==1) {
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
	
	if (mIdealTracking==1) {
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
	  cout<<"slay "<<slay<<" TrackDead (b/a) ChooseSegment = "<<TrackDeadBeforeSegment<<"  "<<TrackDeadAfterSegment<<endl;
	}
	TrackDeadBeforeSelection=0;
	TrackDeadAfterSelection=0;
	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1) {
	    if (mIdealTracking==1) {
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
	    if (mIdealTracking==1) {
	    OneIsGood=0;
	    for (j=0;j<mTrack[i]->GetNBranches();j++)
	      if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	    if (OneIsGood==0 && 
		onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
		mTrack[i]->GetIdealPattern()>=nminhit)
	      TrackDeadAfterSelection++;
	    }
	  }
	if (mIdealTracking==1)
	cout<<"slay "<<slay<<" TrackDead (b/a) ChooseBestNBranches = "<<TrackDeadBeforeSelection<<"  "<<TrackDeadAfterSelection<<endl;
	
      }// for (slay=3..... 

      TrackDeadBeforeBest=0;
      if (mIdealTracking==1) {
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
      if (mIdealTracking==1) {
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
	cout<<"--> TrackDead (b/a) ChooseBest = "<<TrackDeadBeforeBest<<"  "<<TrackDeadAfterBest<<endl;
      }
      
      NTrackFormed=0;
	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1 && mTrack[i]->GetFlag()==1) NTrackFormed++;
      cout<<"Number of tracks presented/should be formed/formed during the pass : "
	  <<NTrackPresented<<"/"<<NTrackPresentedGood<<"/"<<NTrackFormed<<endl;

      for (i=0;i<4;i++) {
	ihitaold[i]=ihita[i];
	ihita[i]=0;
      }
      for (i=0;i<mNSvtHit;i++) {
	if (mSvtHit[i]->GetFlag()==0) ihita[mSvtHit[i]->GetWafer()->GetLayer()]++;
      }
      cout<<"Hit density (0123) :";
      cout<<"\t"<<ihita[0]<<"\t"<<ihitaold[0]-ihita[0];
      cout<<"\t"<<ihita[1]<<"\t"<<ihitaold[1]-ihita[1];
      cout<<"\t"<<ihita[2]<<"\t"<<ihitaold[2]-ihita[2];
      cout<<"\t"<<ihita[3]<<"\t"<<ihitaold[3]-ihita[3]<<endl;
      // We need to flag the track process in the current pass 
      // in order to be considered by the method applied (FinishFlag...)
      // at the end of the superpasses.
      	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1)  mTrack[i]->SetDone(1);
    } // for (mPass=0.......
    mPass--;
    // studing the hit sharing.
    cout<<"Studying the hit sharing"<<endl;
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
    
    cout<<"Hits used (0123) :";
    cout<<"\t"<<ihitb[0];
    cout<<"\t"<<ihitb[1];
    cout<<"\t"<<ihitb[2];
    cout<<"\t"<<ihitb[3]<<endl;
    cout<<"Hits shared (0123) :";
    cout<<"\t"<<ihitbshared[0];
    cout<<"\t"<<ihitbshared[1];
    cout<<"\t"<<ihitbshared[2];
    cout<<"\t"<<ihitbshared[3]<<endl;
    
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
    RemoveHitSharing();
    
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
    cout<<"--> TrackDead (b/a) RemoveSharing = "<<TrackDeadBeforeRemoveSharing<<"  "<<TrackDeadAfterRemoveSharing<<endl;
    

    // flagging the tracks which we assume as found
    FinishFlag();
    // reinitialize the branch helix of the track which are dropped
    ReInitializeHelix();

    // Doing the evaluation for the superpass.
    if(mParams[0]->debug>0 && mIdealTracking==1) Eval(onoffmatrix,nminhit);
    //      Eval(0,1);
  }// for(mSuperPass...

  if(mParams[0]->debug>0)
    cout<<"StEstTracker::DoTracking() ****STOP****"<<endl;
  
  return kStOK;
} 


void StEstTracker::BuildIdealBranches() {

  int nseg,matrix,l[4],idealnhits;
  int iret,flaglog[8];
  int fitstatus;
  long i,j,slay,mcid,nsvthit;
  long slay_found;
  long IsolatedTPCTracks,AssociatedTPCTracks;
  double dist,distw,distl,sd;
  double dca;
  StEstBranch *branch;
  StThreeVector<double> Proj;
  StThreeVector<double> XWaf;
  StThreeVector<double> NWaf;


  if (mParams[0]->debug>0) cout<<"StEstTracker::BuildIdealBranches Starting"<<endl;
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
	  matrix= matrix | int(pow(2,Eval_mchits[mcid][j]->GetWafer()->GetLayer()));
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

	branch = new StEstBranch(NULL, long(mParams[0]->maxsvthits));
	if (branch==NULL)
	  cerr << "ERROR StEstTracker::BuildIdealBranches branch==NULL" << endl;
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
   	    iret=RefitBranch(branch,NULL,-1,1.,&fitstatus);
	    if (iret!=1)  flaglog[7]++;
	    else {
	      if (fitstatus==1) flaglog[0]++;
	      else flaglog[abs(fitstatus)]++;
	    }
	  }  
    	  else {
    	    iret=RefitBranch(branch,NULL,-1,0.,&fitstatus);
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
		    iret=RefitBranch(branch,NULL,-1,0,&fitstatus);
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
  cout<<"Number of TPC Tracks without SVT/SSD hits :"<<IsolatedTPCTracks<<endl;
  cout<<"Number of TPC Tracks with    SVT/SSD hits :"<<AssociatedTPCTracks<<endl;
  cout<<"Fit status : ";
  for (i=0;i<8;i++) cout<<flaglog[i]<<" ";
  cout<<endl;
  if (mParams[0]->debug>0) cout<<"StEstTracker::BuildIdealBranches Finished"<<endl;
}


void StEstTracker::BuildFindableBranches() {

  int matrix;
  int iret,flaglog[8];
  int fitstatus;
  long i,j,slay,mcid;
  long CorrectPass;
  double dist,distw,distl,sd;
  double dca;
  StEstBranch *branch;
  StThreeVector<double> Proj;
  StThreeVector<double> XWaf;
  StThreeVector<double> NWaf;

  if (mParams[0]->debug>0) cout<<"StEstTracker::BuildFindableBranches Starting"<<endl;
  for (i=0;i<8;i++) flaglog[i]=0;
  for (i=0;i<mNTrack;i++) {
    if (mTrack[i]->mTPCTrack->GetFlag()>0) {
      if (mTrack[i]->mTPCTrack->mPt>mParams[mNPass-1]->ptmin && mTrack[i]->mTPCTrack->mPt<mParams[0]->ptmax) {

	CorrectPass=mNPass-1;
 	for (j=0;j<mNPass;j++)
 	  if (mTrack[i]->mTPCTrack->mPt>mParams[j]->ptmin && 
 	      mTrack[i]->mTPCTrack->mPt<mParams[j]->ptmax) CorrectPass=j;
	mcid = Eval_id_mctrk2est_Track[mTrack[i]->mTPCTrack->GetMcId()];
	branch = new StEstBranch(NULL, long(mParams[0]->maxsvthits));
	if (branch==NULL)
	  cerr << "ERROR StEstTracker::BuildFindableBranches branch==NULL" << endl;
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
   	    iret=RefitBranch(branch,NULL,-1,1.,&fitstatus);
	    if (iret!=1)  flaglog[7]++;
	    else {
	      if (fitstatus==1) flaglog[0]++;
	      else flaglog[abs(fitstatus)]++;
	    }
	  }	  
    	  else {
    	    iret=RefitBranch(branch,NULL,-1,0.,&fitstatus);
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
		      iret=RefitBranch(branch,NULL,-1,0,&fitstatus);
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
	  matrix= matrix | int(pow(2,branch->GetHit(j)->GetWafer()->GetLayer()));
	mTrack[i]->SetFindablePattern(matrix);
	mTrack[i]->SetFindableNHits(branch->GetNHits());
      }
    }
  }
  cout<<"End of Build Findable Branches"<<endl;
  cout<<"Fit status : ";
  for (i=0;i<8;i++) cout<<flaglog[i]<<" ";
  cout<<endl;
  if (mParams[0]->debug>0) cout<<"StEstTracker::BuildFindableBranches Finished"<<endl;
}


void StEstTracker::PrintTrackDetails(int trackid) {

  long i,lm,lm2;

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
  long i,j,NHitStillAttached,NBranchPossessing;
  cout<<" CleanUp : Starting"<<endl;
  cout<<" Before mNSvtHit="<<mNSvtHit<<endl;
  NHitStillAttached=0;
  NBranchPossessing=0;
  for (i=0;i<mNSvtHit;i++) {
    if (mSvtHit[i]->mNBranch!=0) {
      NHitStillAttached=NHitStillAttached+1;
      NBranchPossessing=NBranchPossessing+mSvtHit[i]->mNBranch;
      for (j=0;j<mSvtHit[i]->mNBranch;j++)
	if(!mSvtHit[i]->mBranch[j]) cout<<"Error fake branch"<<endl;
    }
  }
  cout<<" Number of hits attached : "<<NHitStillAttached<<endl;
  cout<<" Number of branchess possessing the hits : "<<NBranchPossessing<<endl;
  cout<<"CleanUp : deleting the "<<mNTrack<<" Tracks"<<endl;
  for (i=0;i<mNTrack;i++) {
    if (mTrack[i]->mNBranch<1) cout<<"Warning, this track does not have any branch"<<endl;
    delete mTrack[i];
  }
  delete [] mTrack;
  cout<<"mTrack deleted"<<endl;
  delete [] mTPCTrack;
  cout<<"mTPCTrack deleted"<<endl;
  mIndexGeom->ResetWafTab();
  delete mIndexGeom;
  cout<<"mIndexGeom deleted"<<endl;
  // Delete the Eval_mchits array.
  if (mIdealTracking==1) {
    for (i=0;i<mNTPCTrack;i++)
      for (j=0; j<10; j++) Eval_mchits[i][j]=NULL;
    for (i=0;i<mNTPCTrack;i++)
      delete [] Eval_mchits[i];
    delete [] Eval_mchits;
    cout<<"Eval_mchits deleted"<<endl;
  }
  for (i=0;i<mNSvtHit;i++) mSvtHit[i]->DetachFromWafer();
  cout<<"Hits detached from the wafers"<<endl;
  for (i=0;i<mNSvtHit;i++) {
    delete mSvtHit[i];
  }
  cout<<"Hits deleted"<<endl;
  delete [] mSvtHit;
  cout<<"mSvtHit deleted"<<endl;
  delete mVertex;
  cout<<"mVertex deleted"<<endl;
  for (i=0;i<mNWafers;i++) delete mIndexWaf[i];
  delete [] mIndexWaf;
  cout<<"mIndexWaf deleted"<<endl;
  delete [] mTptIndex;
  cout<<"mTptIndex deleted"<<endl;
  delete [] Eval_id_mctrk2est_Track;
  cout<<"Eval_id_mctrk2est deleted"<<endl;
  cout<<" CleanUp : Done"<<endl;

}
