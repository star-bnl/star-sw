/***************************************************************************
 *
 * $Id: StEstEval.cxx,v 1.1 2000/12/07 11:14:21 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the Evaluation
 *
 ***************************************************************************
 *
 * $Log: StEstEval.cxx,v $
 * Revision 1.1  2000/12/07 11:14:21  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StEstMaker.h"

void StEstMaker::Eval(int onoffmatrix, int nminhit) {

  long i,j,k,jb,lm,lm2,lm3,found,found2;
  long bad_in_ideal,bad_in_ideal_pri,bad_in_ideal_sec;
  long bad_notin_ideal,bad_notin_ideal_pri,bad_notin_ideal_sec;
  long bad_found_twice,bad_found_twice_pri,bad_found_twice_sec;
  long ideal_not_found,ideal_not_found_pri,ideal_not_found_sec;
  long ideal_not_found_good_pattern,ideal_not_found_null_pattern;
  long ideal_not_found_in_good,ideal_not_found_in_good_pri,ideal_not_found_in_good_sec;
  long ideal_not_found_in_bad,ideal_not_found_in_bad_pri,ideal_not_found_in_bad_sec;
  long ideal_not_found_in_ideal,ideal_not_found_in_ideal_pri,ideal_not_found_in_ideal_sec;
  long alreadygood,alreadybad,alreadyideal,alreadyfound;
  long real_ideal,real_ideal_pri,real_ideal_sec;
  long real_good,real_good_pri,real_good_sec;
  long good_found_twice,good_found_twice_pri,good_found_twice_sec;
  long reallynotinideal;
  long bad[200], good[200],poss[200],slay_good[200],slay_bad[200];
  long gd,bd,ps;
  long lmideal_id[2000],lmgood_id[2000],lmbad_id[2000];
  long lmideal_i[2000],lmgood_i[2000],lmbad_i[2000];
  long lmideal_pat[2000],lmgood_pat[2000],lmbad_pat[2000];
  long lmideal_tot,lmideal_pri_tot,lmideal_sec_tot;
  long lmgood_tot,lmgood_pri_tot,lmgood_sec_tot;
  long lmbad_tot,lmbad_pri_tot,lmbad_sec_tot;

  int matrix,matrix2,ok,mcid;
  long possmatr[64],badmatr[64],goodmatr[64],badimatr[64],goodimatr[64];
  int nPerfectHit;

  double maxdist;
  ofstream sortie("out.log",ios::app);


  if(mParams[0]->debug>2) cout << "Eval :: Start"<<endl;

  cout<<"+------------------------------------------------+"<<endl;
  cout <<"| EVAL for onoffmatrix= "<<onoffmatrix
       <<" and nminhit="<<nminhit<<" \t |"<<endl;
  cout<<"+------------------------------------------------+"<<endl;
  if (sortie) {
    sortie<<"+------------------------------------------------+"<<endl;
    sortie<<"| EVAL for onoffmatrix= "<<onoffmatrix
	   <<" and nminhit="<<nminhit<<" \t |"<<endl;
    sortie<<"+------------------------------------------------+"<<endl;
  }
  lmideal_tot = 0;
  lmideal_pri_tot = 0;
  lmideal_sec_tot = 0;
  lmgood_tot  = 0;
  lmgood_pri_tot  = 0;
  lmgood_sec_tot  = 0;
  lmbad_tot   = 0;
  lmbad_pri_tot   = 0;
  lmbad_sec_tot   = 0;
  for (i=0;i<2000;i++) {
    lmideal_id[i]  = 0;
    lmideal_i[i]   = 0;
    lmgood_id[i]   = 0;
    lmgood_i[i]    = 0;
    lmbad_id[i]    = 0;
    lmbad_i[i]     = 0;
    lmideal_pat[i] = 0;
    lmgood_pat[i]  = 0;
    lmbad_pat[i]   = 0;
  }
  for (i=0;i<200;i++) {
    bad[i]  = 0;
    good[i] = 0;
    poss[i] = 0;
    slay_good[i] = 0;
    slay_bad[i]  = 0;
  }
  for (i=0;i<64;i++) {
    possmatr[i]  = 0;
    goodmatr[i]  = 0;
    badmatr[i]   = 0;
    goodimatr[i] = 0;
    badimatr[i]  = 0;
  }

  // first loop over all the tracks - to evaluate the ideal tracks
  for (i=0;i<mNTrack; i++) {
    matrix = 0;
    if (mTrack[i]->mTPCTrack->mPt > mParams[0]->ptmax || 
	mTrack[i]->mTPCTrack->mPt <= mParams[mNPass-1]->ptmin || 
	mTrack[i]->mTPCTrack->GetFlag()<=0 || 
	mTrack[i]->mTPCTrack->GetFlagSP()<0) 
      continue;
    j=0;

    mcid = Eval_id_mctrk2est_Track[mTrack[i]->mTPCTrack->GetMcId()];

    while (Eval_mchits[mcid][j]!=NULL && j<10) {      
      matrix = matrix | int(pow(2,Eval_mchits[mcid][j]->GetWafer()->GetLayer()));
      j++;    
    }
    nPerfectHit=0;
    for (k=0;k<4;k++) 
      nPerfectHit = nPerfectHit + 
	((int (pow(2,k))) == ((int (pow(2,k))) & matrix));

    if (j>0 && (onoffmatrix == (matrix & onoffmatrix)) 
	&& nPerfectHit>=nminhit) {
	  poss[j]++;
	  possmatr[matrix]++;
	  lmideal_id[lmideal_tot]=mTrack[i]->mTPCTrack->mId;
	  lmideal_i[lmideal_tot]=i;
	  lmideal_pat[lmideal_tot]=matrix;
	  lmideal_tot++;
	  if (mTrack[i]->mTPCTrack->mType==1) lmideal_pri_tot++;	
	  if (mTrack[i]->mTPCTrack->mType==2) lmideal_sec_tot++;	
	  idealpt->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==2 || mTrack[i]->mTPCTrack->GetPid()==3) Elidealpt->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==5 || mTrack[i]->mTPCTrack->GetPid()==6) Muidealpt->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==8 || mTrack[i]->mTPCTrack->GetPid()==9) Piidealpt->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==11 || mTrack[i]->mTPCTrack->GetPid()==12) Kaidealpt->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==14 || mTrack[i]->mTPCTrack->GetPid()==15) Pridealpt->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->mType==1) {
	    idealpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	    if (mTrack[i]->mTPCTrack->GetPid()==2 || mTrack[i]->mTPCTrack->GetPid()==3) Elidealpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	    if (mTrack[i]->mTPCTrack->GetPid()==5 || mTrack[i]->mTPCTrack->GetPid()==6) Muidealpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	    if (mTrack[i]->mTPCTrack->GetPid()==8 || mTrack[i]->mTPCTrack->GetPid()==9) Piidealpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	    if (mTrack[i]->mTPCTrack->GetPid()==11 || mTrack[i]->mTPCTrack->GetPid()==12) Kaidealpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	    if (mTrack[i]->mTPCTrack->GetPid()==14 || mTrack[i]->mTPCTrack->GetPid()==15) Pridealpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	  }
	  if (mTrack[i]->mTPCTrack->mType==2) {
	    idealpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	    if (mTrack[i]->mTPCTrack->GetPid()==2 || mTrack[i]->mTPCTrack->GetPid()==3) Elidealpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	    if (mTrack[i]->mTPCTrack->GetPid()==5 || mTrack[i]->mTPCTrack->GetPid()==6) Muidealpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	    if (mTrack[i]->mTPCTrack->GetPid()==8 || mTrack[i]->mTPCTrack->GetPid()==9) Piidealpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	    if (mTrack[i]->mTPCTrack->GetPid()==11 || mTrack[i]->mTPCTrack->GetPid()==12) Kaidealpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	    if (mTrack[i]->mTPCTrack->GetPid()==14 || mTrack[i]->mTPCTrack->GetPid()==15) Pridealpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	  }
	  idealphi->Fill(atan(mTrack[i]->mTPCTrack->GetHelix()->y(0.)/mTrack[i]->mTPCTrack->GetHelix()->x(0.)));
	  idealntpc->Fill(mTrack[i]->mTPCTrack->GetNHits());
	  idealrtpc->Fill(sqrt(mTrack[i]->mTPCTrack->mR[0]->x()*mTrack[i]->mTPCTrack->mR[0]->x()+mTrack[i]->mTPCTrack->mR[0]->y()*mTrack[i]->mTPCTrack->mR[0]->y()));
    }
  }// for (i=0;i<mNTrack; i++...
  
  // second loop over all the tracks - to find good and bad tracks
  for (i=0;i<mNTrack; i++) {    

    ok = 1;
    matrix = 0;
    matrix2 = 0;

    if (mTrack[i]->mTPCTrack->mPt>mParams[0]->ptmax || mTrack[i]->mTPCTrack->mPt<=mParams[mNPass-1]->ptmin || mTrack[i]->mTPCTrack->GetFlag()<=0)
      continue;
    
    j=0;

    mcid = mTrack[i]->mTPCTrack->GetMcId();

    if (mcid == -1)
      continue;

    mcid = Eval_id_mctrk2est_Track[mcid];

    while (Eval_mchits[mcid][j]!=NULL && j<10) { 
      matrix2 = matrix2 | int(pow(2,Eval_mchits[mcid][j]->GetWafer()->GetLayer()));
      j++;    
    }

    int _NBR=0;
    k=0;
    
    for (j=0;j<mTrack[i]->GetBranch(_NBR)->GetNHits();j++) {
      matrix |= int(pow(2,mTrack[i]->GetBranch(_NBR)->GetHit(j)->GetWafer()->GetLayer()));
      k=0;
      while (mTrack[i]->GetBranch(_NBR)->GetHit(j) != Eval_mchits[mcid][k] 
	     && Eval_mchits[mcid][k]!=NULL)
	k++;
      if (Eval_mchits[mcid][k]==NULL) ok = 0;

      //number of bad and good conection on the layer
      if (Eval_mchits[mcid][k]==mTrack[i]->GetBranch(_NBR)->GetHit(j)) 
	slay_good[Eval_mchits[mcid][k]->GetWafer()->GetLayer()]++;      
      else
	slay_bad[mTrack[i]->GetBranch(_NBR)->GetHit(j)->GetWafer()->GetLayer()]++;
    }
    
    
    // looking for the max dist among hits in branch
    maxdist = 0;
    for (j=0;j<mTrack[i]->GetBranch(_NBR)->GetNHits();j++) {
      if (mTrack[i]->GetBranch(_NBR)->GetDist(j) > maxdist) 
	maxdist = mTrack[i]->GetBranch(_NBR)->GetDist(j);
    }

    // filling histograms for good and bad tracks
    if (mTrack[i]->GetBranch(_NBR)->GetNHits()>0 && (onoffmatrix == (matrix & onoffmatrix))) {
      if (ok==0) {
	bad[mTrack[i]->GetBranch(_NBR)->GetNHits()]++;
	badmatr[matrix]++;
	//	if (matrix2==0) cout<<"ideal pattern=0 : track_id= "<<mTrack[i]->mTPCTrack->GetId()<<endl;
	badimatr[matrix2]++;
	badpt->Fill(mTrack[i]->mTPCTrack->mPt);
	if (mTrack[i]->mTPCTrack->GetPid()==2 || mTrack[i]->mTPCTrack->GetPid()==3) Elbadpt->Fill(mTrack[i]->mTPCTrack->mPt);
	if (mTrack[i]->mTPCTrack->GetPid()==5 || mTrack[i]->mTPCTrack->GetPid()==6) Mubadpt->Fill(mTrack[i]->mTPCTrack->mPt);
	if (mTrack[i]->mTPCTrack->GetPid()==8 || mTrack[i]->mTPCTrack->GetPid()==9) Pibadpt->Fill(mTrack[i]->mTPCTrack->mPt);
	if (mTrack[i]->mTPCTrack->GetPid()==11 || mTrack[i]->mTPCTrack->GetPid()==12) Kabadpt->Fill(mTrack[i]->mTPCTrack->mPt);
	if (mTrack[i]->mTPCTrack->GetPid()==14 || mTrack[i]->mTPCTrack->GetPid()==15) Prbadpt->Fill(mTrack[i]->mTPCTrack->mPt);
	if (mTrack[i]->mTPCTrack->mType==1) {
	  badpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==2 || mTrack[i]->mTPCTrack->GetPid()==3) Elbadpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==5 || mTrack[i]->mTPCTrack->GetPid()==6) Mubadpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==8 || mTrack[i]->mTPCTrack->GetPid()==9) Pibadpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==11 || mTrack[i]->mTPCTrack->GetPid()==12) Kabadpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==14 || mTrack[i]->mTPCTrack->GetPid()==15) Prbadpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	}
	if (mTrack[i]->mTPCTrack->mType==2) {
	  badpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==2 || mTrack[i]->mTPCTrack->GetPid()==3) Elbadpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==5 || mTrack[i]->mTPCTrack->GetPid()==6) Mubadpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==8 || mTrack[i]->mTPCTrack->GetPid()==9) Pibadpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==11 || mTrack[i]->mTPCTrack->GetPid()==12) Kabadpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==14 || mTrack[i]->mTPCTrack->GetPid()==15) Prbadpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	}
	badphi->Fill(atan(mTrack[i]->mTPCTrack->GetHelix()->y(0.)/mTrack[i]->mTPCTrack->GetHelix()->x(0.)));
	badchi->Fill(mTrack[i]->GetBranch(0)->GetChiSq()/mTrack[i]->GetBranch(0)->GetNHits()); 
	badmaxdist->Fill(maxdist);
	badStep->Fill(1.*(mTrack[i]->GetBranch(0)->GetStep())); 
	
	lmbad_id[lmbad_tot]=mTrack[i]->mTPCTrack->mId;
	lmbad_i[lmbad_tot]=i;
	lmbad_pat[lmbad_tot]=matrix;
	lmbad_tot++;	
	if (mTrack[i]->mTPCTrack->mType==1) lmbad_pri_tot++;	
	if (mTrack[i]->mTPCTrack->mType==2) lmbad_sec_tot++;	
      }
      else {
	good[mTrack[i]->GetBranch(_NBR)->GetNHits()]++;
	goodmatr[matrix]++;
	goodimatr[matrix2]++;
	goodpt->Fill(mTrack[i]->mTPCTrack->mPt);
	if (mTrack[i]->mTPCTrack->GetPid()==2 || mTrack[i]->mTPCTrack->GetPid()==3) Elgoodpt->Fill(mTrack[i]->mTPCTrack->mPt);
	if (mTrack[i]->mTPCTrack->GetPid()==5 || mTrack[i]->mTPCTrack->GetPid()==6) Mugoodpt->Fill(mTrack[i]->mTPCTrack->mPt);
	if (mTrack[i]->mTPCTrack->GetPid()==8 || mTrack[i]->mTPCTrack->GetPid()==9) Pigoodpt->Fill(mTrack[i]->mTPCTrack->mPt);
	if (mTrack[i]->mTPCTrack->GetPid()==11 || mTrack[i]->mTPCTrack->GetPid()==12) Kagoodpt->Fill(mTrack[i]->mTPCTrack->mPt);
	if (mTrack[i]->mTPCTrack->GetPid()==14 || mTrack[i]->mTPCTrack->GetPid()==15) Prgoodpt->Fill(mTrack[i]->mTPCTrack->mPt);
	if (mTrack[i]->mTPCTrack->mType==1) {
	  goodpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==2 || mTrack[i]->mTPCTrack->GetPid()==3) Elgoodpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==5 || mTrack[i]->mTPCTrack->GetPid()==6) Mugoodpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==8 || mTrack[i]->mTPCTrack->GetPid()==9) Pigoodpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==11 || mTrack[i]->mTPCTrack->GetPid()==12) Kagoodpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==14 || mTrack[i]->mTPCTrack->GetPid()==15) Prgoodpt_p->Fill(mTrack[i]->mTPCTrack->mPt);
	}
	if (mTrack[i]->mTPCTrack->mType==2) {
	  goodpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==2 || mTrack[i]->mTPCTrack->GetPid()==3) Elgoodpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==5 || mTrack[i]->mTPCTrack->GetPid()==6) Mugoodpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==8 || mTrack[i]->mTPCTrack->GetPid()==9) Pigoodpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==11 || mTrack[i]->mTPCTrack->GetPid()==12) Kagoodpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	  if (mTrack[i]->mTPCTrack->GetPid()==14 || mTrack[i]->mTPCTrack->GetPid()==15) Prgoodpt_s->Fill(mTrack[i]->mTPCTrack->mPt);
	}
	goodchi->Fill(mTrack[i]->GetBranch(0)->GetChiSq()/mTrack[i]->GetBranch(0)->GetNHits());
	goodphi->Fill(atan(mTrack[i]->mTPCTrack->GetHelix()->y(0.)/mTrack[i]->mTPCTrack->GetHelix()->x(0.)));
	goodmaxdist->Fill(maxdist);
	goodStep->Fill(1.*(mTrack[i]->GetBranch(0)->GetStep())); 
	
	lmgood_id[lmgood_tot]=mTrack[i]->mTPCTrack->mId;
	lmgood_i[lmgood_tot]=i;
	lmgood_pat[lmgood_tot]=matrix;
	lmgood_tot++;
	if (mTrack[i]->mTPCTrack->mType==1) lmgood_pri_tot++;	
	if (mTrack[i]->mTPCTrack->mType==2) lmgood_sec_tot++;	
      }
    }// if (mTrack[i]->GetBranch(_NBR)...
  } // for(i...

  gd=0; 
  bd=0; 
  ps=0; 

  cout <<"*****  RESULTS VERSUS THE TRACK LENGTH *****"<<endl;
  cout <<"Tracks\t\tPOSS\tOK\tNOK"<<endl;
  for (i=1;i<10;i++) {
    cout <<"with "<<i<<" hits :\t"<<poss[i]<<"\t"<<good[i]<<"\t"<<bad[i]<<endl;
    gd += good[i];
    bd += bad[i];
    ps += poss[i];
  }
  cout <<"TOTAL:\t\t"<<ps<<"\t"<<gd<<"\t"<<bd<<endl;
  cout <<endl;
  cout <<"*****  RESULTS VERSUS THE HIT LAYERS *****"<<endl;
  cout <<"GOOD: slay[0]="<<slay_good[0]<<" slay[1]="<<slay_good[1]<<" slay[2]="<<slay_good[2]<<" slay[3]="<<slay_good[3]<<endl;
  cout <<"BAD:  slay[0]="<<slay_bad[0]<<" slay[1]="<<slay_bad[1]<<" slay[2]="<<slay_bad[2]<<" slay[3]="<<slay_bad[3]<<endl;
  cout <<endl;
  cout <<"*****  RESULTS VERSUS THE SEGMENT PATTERNS *****"<<endl;
  cout << "\t\t| final\tpattern\t| ideal\tpattern"<<endl;
  cout << "1234\t| POSS\t| GOOD\tBAD\t| GOOD\tBAD"<<endl;
  for (i=0;i<16;i++) {
    for (j=0;j<4;j++) {
      if (i & int(pow(2,j))) 
	cout << "1";
      else
	cout << "0";
    }
    cout << "\t| "<<possmatr[i]<<"\t| "<<goodmatr[i]<<"\t"<<badmatr[i]<<"\t| "<<goodimatr[i]<<"\t"<<badimatr[i]<<endl;
  }

  cout<<endl;
  if (sortie) {
    gd=0; 
    bd=0; 
    ps=0; 
    sortie <<"*****  RESULTS VERSUS THE TRACK LENGTH *****"<<endl;
    sortie <<"Tracks\t\tPOSS\tOK\tNOK"<<endl;
    for (i=1;i<10;i++) {
      sortie <<"with "<<i<<" hits :\t"<<poss[i]<<"\t"<<good[i]<<"\t"<<bad[i]<<endl;
      gd += good[i];
      bd += bad[i];
      ps += poss[i];
    }
  sortie <<"TOTAL:\t\t"<<ps<<"\t"<<gd<<"\t"<<bd<<endl;
  sortie <<endl;
  sortie <<"*****  RESULTS VERSUS THE HIT LAYERS *****"<<endl;
  sortie <<"GOOD: slay[0]="<<slay_good[0]<<" slay[1]="<<slay_good[1]<<" slay[2]="<<slay_good[2]<<" slay[3]="<<slay_good[3]<<endl;
  sortie <<"BAD:  slay[0]="<<slay_bad[0]<<" slay[1]="<<slay_bad[1]<<" slay[2]="<<slay_bad[2]<<" slay[3]="<<slay_bad[3]<<endl;
  sortie <<endl;
  sortie <<"*****  RESULTS VERSUS THE SEGMENT PATTERNS *****"<<endl;
  sortie << "\t\t| final\tpattern\t| ideal\tpattern"<<endl;
  sortie << "1234\t| POSS\t| GOOD\tBAD\t| GOOD\tBAD"<<endl;
  for (i=0;i<16;i++) {
    for (j=0;j<4;j++) {
      if (i & int(pow(2,j))) 
	sortie << "1";
      else
	sortie << "0";
    }
    sortie << "\t| "<<possmatr[i]<<"\t| "<<goodmatr[i]<<"\t"<<badmatr[i]<<"\t| "<<goodimatr[i]<<"\t"<<badimatr[i]<<endl;
  }

  sortie<<endl;
  }

  cout<<"counting the good tracks found twice"<<endl;
  good_found_twice=0;
  good_found_twice_pri=0;
  good_found_twice_sec=0;
  for (lm=0;lm<lmgood_tot;lm++) {
    alreadyfound=0;
    for (lm2=0;lm2<lm;lm2++)
      if (mTrack[lmgood_i[lm2]]->mTPCTrack->GetMcId()==mTrack[lmgood_i[lm]]->mTPCTrack->GetMcId())
	alreadyfound=1;
    if (alreadyfound==1) {
      good_found_twice++;
      if (mTrack[lmgood_i[lm]]->mTPCTrack->mType==1) good_found_twice_pri++;	
      if (mTrack[lmgood_i[lm]]->mTPCTrack->mType==2) good_found_twice_sec++;	
    }
  }

  cout<<"counting the real number of ideal tracks"<<endl;
  real_ideal=0;
  real_ideal_pri=0;
  real_ideal_sec=0;
  for (lm=0;lm<lmideal_tot;lm++) {
    alreadyfound=0;
    for (lm2=0;lm2<lm;lm2++)
      if (mTrack[lmideal_i[lm]]->mTPCTrack->GetMcId()==mTrack[lmideal_i[lm2]]->mTPCTrack->GetMcId())
	alreadyfound=1;
    if (alreadyfound==0) {
      real_ideal++;
      if (mTrack[lmideal_i[lm]]->mTPCTrack->mType==1) real_ideal_pri++;	
      if (mTrack[lmideal_i[lm]]->mTPCTrack->mType==2) real_ideal_sec++;	
    }
  }
  cout<<"counting the ideal tracks not found"<<endl;
  ideal_not_found=0;
  ideal_not_found_pri=0;
  ideal_not_found_sec=0;
  ideal_not_found_good_pattern=0;
  ideal_not_found_null_pattern=0;
  ideal_not_found_in_good=0;
  ideal_not_found_in_good_pri=0;
  ideal_not_found_in_good_sec=0;
  ideal_not_found_in_bad=0;
  ideal_not_found_in_bad_pri=0;
  ideal_not_found_in_bad_sec=0;
  ideal_not_found_in_ideal=0;
  ideal_not_found_in_ideal_pri=0;
  ideal_not_found_in_ideal_sec=0;
  for (lm=0;lm<lmideal_tot;lm++) {
    found=0;
    for (lm2=0;lm2<lmgood_tot;lm2++)
      if (lmgood_i[lm2]==lmideal_i[lm]) found=1;
    for (lm2=0;lm2<lmbad_tot;lm2++)
      if (lmbad_i[lm2]==lmideal_i[lm]) found=1;
    
    if (found==0) {
      // the ideal track is not in good or bad
      // we check that a tpc segment with the same mcid is not in good already
      alreadygood=0;
      for (lm2=0;lm2<lmgood_tot;lm2++) 
	if (mTrack[lmgood_i[lm2]]->mTPCTrack->GetMcId()==mTrack[lmideal_i[lm]]->mTPCTrack->GetMcId()) 
	  alreadygood=1;
      // we check that a tpc segment with the same mcid is not in bad already
      alreadybad=0;
      for (lm2=0;lm2<lmbad_tot;lm2++) 
	if (mTrack[lmbad_i[lm2]]->mTPCTrack->GetMcId()==mTrack[lmideal_i[lm]]->mTPCTrack->GetMcId()) 
	  alreadybad=1;
      // we check that a tpc segment with the same mcid is not in the already 
      // scanned ideal not found. brute force probably a smarter way.
      alreadyideal=0;
      reallynotinideal=1;
      for (lm2=0;lm2<lm;lm2++) {
	if (mTrack[lmideal_i[lm2]]->mTPCTrack->GetMcId()==mTrack[lmideal_i[lm]]->mTPCTrack->GetMcId()) {
	  found2=0;
	  for (lm3=0;lm3<lmbad_tot;lm3++)
	    if (lmbad_i[lm3]==lmideal_i[lm2]) found2=1;
	  for (lm3=0;lm3<lmgood_tot;lm3++)
	    if (lmgood_i[lm3]==lmideal_i[lm2]) found2=1;
	  if (found2==0) alreadyideal=1;
	}
      }
      for (lm2=lm+1;lm2<lmideal_tot;lm2++) 
	if (mTrack[lmideal_i[lm2]]->mTPCTrack->GetMcId()==mTrack[lmideal_i[lm]]->mTPCTrack->GetMcId())
	  reallynotinideal=0;
      if (alreadygood==0 && alreadybad==0 && alreadyideal==0 && reallynotinideal==1){
	ideal_not_found++;
	if (mTrack[lmideal_i[lm]]->mTPCTrack->mType==1) ideal_not_found_pri++;
	if (mTrack[lmideal_i[lm]]->mTPCTrack->mType==2) ideal_not_found_sec++;
	if (mTrack[lmideal_i[lm]]->GetIdealPattern()==mTrack[lmideal_i[lm]]->GetFindablePattern()) ideal_not_found_good_pattern++;
	if (mTrack[lmideal_i[lm]]->GetFindablePattern()==0) ideal_not_found_null_pattern++;
      }
      if (alreadygood==1) {
	ideal_not_found_in_good++;
	if (mTrack[lmideal_i[lm]]->mTPCTrack->mType==1) ideal_not_found_in_good_pri++;
	if (mTrack[lmideal_i[lm]]->mTPCTrack->mType==2) ideal_not_found_in_good_sec++;
      }
      if (alreadybad==1 && alreadygood==0) {
	ideal_not_found_in_bad++;
	if (mTrack[lmideal_i[lm]]->mTPCTrack->mType==1) ideal_not_found_in_bad_pri++;
	if (mTrack[lmideal_i[lm]]->mTPCTrack->mType==2) ideal_not_found_in_bad_sec++;
      }
      if (alreadyideal==1 && alreadybad==0 && alreadygood==0) {
	ideal_not_found_in_ideal++;
	if (mTrack[lmideal_i[lm]]->mTPCTrack->mType==1) ideal_not_found_in_ideal_pri++;
	if (mTrack[lmideal_i[lm]]->mTPCTrack->mType==2) ideal_not_found_in_ideal_sec++;
      }
    }
  }
  cout<<"counting the bad tracks in ideal"<<endl;
  bad_in_ideal=0;
  bad_in_ideal_pri=0;
  bad_in_ideal_sec=0;
  for (lm=0;lm<lmbad_tot;lm++) {
    found=0;
    for (lm2=0;lm2<lmideal_tot;lm2++)
      if (lmideal_i[lm2]==lmbad_i[lm]) found=1;
    if (found==1)  {
      bad_in_ideal++;
      if (mTrack[lmbad_i[lm]]->mTPCTrack->mType==1) bad_in_ideal_pri++;
      if (mTrack[lmbad_i[lm]]->mTPCTrack->mType==2) bad_in_ideal_sec++;
    }
  }

  cout<<"counting the bad tracks not in ideal"<<endl;
  bad_notin_ideal=0;
  bad_notin_ideal_pri=0;
  bad_notin_ideal_sec=0;
  for (lm=0;lm<lmbad_tot;lm++) {
    found=0;
    for (lm2=0;lm2<lmideal_tot;lm2++)
      if (lmideal_i[lm2]==lmbad_i[lm]) found=1;
    if (found==0)   {
      bad_notin_ideal++;
      if (mTrack[lmbad_i[lm]]->mTPCTrack->mType==1) bad_notin_ideal_pri++;
      if (mTrack[lmbad_i[lm]]->mTPCTrack->mType==2) bad_notin_ideal_sec++;
    }
  }

  cout<<"counting the bad tracks found twice"<<endl;
  bad_found_twice=0;
  bad_found_twice_pri=0;
  bad_found_twice_sec=0;
  for (lm=0;lm<lmbad_tot;lm++) {
    alreadyfound=0;
    for (lm2=0;lm2<lm;lm2++)
      if (mTrack[lmbad_i[lm2]]->mTPCTrack->GetMcId()==mTrack[lmbad_i[lm]]->mTPCTrack->GetMcId())
	alreadyfound=1;
    if (alreadyfound==1) {
      bad_found_twice++;
      if (mTrack[lmbad_i[lm]]->mTPCTrack->mType==1) bad_found_twice_pri++;
      if (mTrack[lmbad_i[lm]]->mTPCTrack->mType==2) bad_found_twice_sec++;
    }
  }

  real_good=lmgood_tot-good_found_twice;
  real_good_pri=lmgood_pri_tot-good_found_twice_pri;
  real_good_sec=lmgood_sec_tot-good_found_twice_sec;

  cout<<"Ideal not found but unique : "<<ideal_not_found
      <<" findable pat=ideal pat : "<<ideal_not_found_good_pattern
      <<" find pat=0 : "<<ideal_not_found_null_pattern<<endl;

  cout<<"---------- 1st level analysis -----------------------------------------------"<<endl;
  cout<<"number of ideal tracks : "<<ps<<"\t all ideal tracks with a hit in the SVT/SSD"<<endl;
  cout<<"number of good tracks  : "<<lmgood_tot<<"\t all reconstructed tracks with correct hits"<<endl;
  cout<<"number of bad tracks   : "<<lmbad_tot<<"\t all reconstructed tracks with at least one wrong hit"<<endl;
  cout<<"Efficiency             : "<< lmgood_tot*100/(ps*1.)<<" %"<<endl;
  cout<<"Purity                 : "<<lmgood_tot*100/(lmgood_tot*1.+lmbad_tot*1.)<<" %"<<endl;
  cout<<"---------- detailled analysis ------------------------------------------------------------"<<endl;
  cout<<"Initial number of ideal tracks   : \t"<<lmideal_tot<<"\t"<<lmideal_pri_tot<<"\t"<<lmideal_sec_tot<<"\t total primaries secondaries"<<endl;
  cout<<"Ideal not found but unique       : \t"<<ideal_not_found<<"\t"<<ideal_not_found_pri<<"\t"<<ideal_not_found_sec<<"\t ideal track not reconstructed (no other copies)"<<endl;
  cout<<"Ideal not found but copy in good : \t"<<ideal_not_found_in_good<<"\t"<<ideal_not_found_in_good_pri<<"\t"<<ideal_not_found_in_good_sec<<"\t (with possibly other copies in ideal not found or bad)"<<endl;
  cout<<"Ideal not found but copy in bad  : \t"<<ideal_not_found_in_bad<<"\t"<<ideal_not_found_in_bad_pri<<"\t"<<ideal_not_found_in_bad_sec<<"\t (with possibly other copies in ideal not found)"<<endl;
  cout<<"Ideal not found but copy in ideal: \t"<<ideal_not_found_in_ideal<<"\t"<<ideal_not_found_in_ideal_pri<<"\t"<<ideal_not_found_in_ideal_sec<<"\t (without other copies in good or bad)"<<endl;
  cout<<"Real ideal tracks                : \t"<<real_ideal<<"\t"<<real_ideal_pri<<"\t"<<real_ideal_sec<<"\t (initial minus the copies)"<<endl;
  cout<<endl;
  cout<<"Number of bad tracks             : \t"<<lmbad_tot<<"\t"<<lmbad_pri_tot<<"\t"<<lmbad_sec_tot<<"\t total primaries secondaries"<<endl;
  cout<<"Bad in ideal                     : \t"<<bad_in_ideal<<"\t"<<bad_in_ideal_pri<<"\t"<<bad_in_ideal_sec<<endl;
  cout<<"Bad not in ideal                 : \t"<<bad_notin_ideal<<"\t"<<bad_notin_ideal_pri<<"\t"<<bad_notin_ideal_sec<<endl;
  cout<<"Bad found twice in bad           : \t"<<bad_found_twice<<"\t"<<bad_found_twice_pri<<"\t"<<bad_found_twice_sec<<endl;
  cout<<endl;
  cout<<"Initial number of good tracks    : \t"<<lmgood_tot<<"\t"<<lmgood_pri_tot<<"\t"<<lmgood_sec_tot<<"\t total primaries secondaries"<<endl;
  cout<<"Good found twice in good         : \t"<<good_found_twice<<"\t"<<good_found_twice_pri<<"\t"<<good_found_twice_sec<<"\t (correct reconstructed tracks with copies in good)"<<endl;
  cout<<"Real good tracks                 : \t"<<real_good<<"\t"<<real_good_pri<<"\t"<<real_good_sec<<"\t Initial minus the good found twice"<<endl;
  cout<<endl;
  cout<<"                Real ideal tracks: \t"<<real_ideal<<"\t"<<real_ideal_pri<<"\t"<<real_ideal_sec<<endl;
  cout<<"                Real good tracks : \t"<<real_good<<"\t"<<real_good_pri<<"\t"<<real_good_sec<<endl;
  cout<<"                Real bad tracks  : \t"<<lmbad_tot+good_found_twice<<"\t"<<lmbad_pri_tot+good_found_twice_pri<<"\t"<<lmbad_sec_tot+good_found_twice_sec<<endl;
  cout<<"             Real efficiency (%) : \t"<<(real_good*100)/(real_ideal*1.)<<"\t"<<(real_good_pri*100)/(real_ideal_pri*1.)<<"\t"<<(real_good_sec*100)/(real_ideal_sec*1.)<<endl;
  cout<<"             Real purity (%)     : \t"<<(real_good*100)/(lmgood_tot*1.+lmbad_tot*1.)<<"\t"<<(real_good_pri*100)/(lmgood_pri_tot*1.+lmbad_pri_tot*1.)<<"\t"<<(real_good_sec*100)/(lmgood_sec_tot*1.+lmbad_sec_tot*1.)<<endl;
  cout<<"------------------------------------------------------------------------------------------"<<endl;

  if (sortie) {
  sortie<<"---------- 1st level analysis -----------------------------------------------"<<endl;
  sortie<<"number of ideal tracks : "<<ps<<"\t all ideal tracks with a hit in the SVT/SSD"<<endl;
  sortie<<"number of good tracks  : "<<lmgood_tot<<"\t all reconstructed tracks with correct hits"<<endl;
  sortie<<"number of bad tracks   : "<<lmbad_tot<<"\t all reconstructed tracks with at least one wrong hit"<<endl;
  sortie<<"Efficiency             : "<< lmgood_tot*100/(ps*1.)<<" %"<<endl;
  sortie<<"Purity                 : "<<lmgood_tot*100/(lmgood_tot*1.+lmbad_tot*1.)<<" %"<<endl;
  sortie<<"---------- detailled analysis ------------------------------------------------------------"<<endl;
  sortie<<"Initial number of ideal tracks   : \t"<<lmideal_tot<<"\t"<<lmideal_pri_tot<<"\t"<<lmideal_sec_tot<<"\t total primaries secondaries"<<endl;
  sortie<<"Ideal not found but unique       : \t"<<ideal_not_found<<"\t"<<ideal_not_found_pri<<"\t"<<ideal_not_found_sec<<"\t ideal track not reconstructed (no other copies)"<<endl;
  sortie<<"Ideal not found but copy in good : \t"<<ideal_not_found_in_good<<"\t"<<ideal_not_found_in_good_pri<<"\t"<<ideal_not_found_in_good_sec<<"\t (with possibly other copies in ideal not found or bad)"<<endl;
  sortie<<"Ideal not found but copy in bad  : \t"<<ideal_not_found_in_bad<<"\t"<<ideal_not_found_in_bad_pri<<"\t"<<ideal_not_found_in_bad_sec<<"\t (with possibly other copies in ideal not found)"<<endl;
  sortie<<"Ideal not found but copy in ideal: \t"<<ideal_not_found_in_ideal<<"\t"<<ideal_not_found_in_ideal_pri<<"\t"<<ideal_not_found_in_ideal_sec<<"\t (without other copies in good or bad)"<<endl;
  sortie<<"Real ideal tracks                : \t"<<real_ideal<<"\t"<<real_ideal_pri<<"\t"<<real_ideal_sec<<"\t (initial minus the copies)"<<endl;
  sortie<<endl;
  sortie<<"Number of bad tracks             : \t"<<lmbad_tot<<"\t"<<lmbad_pri_tot<<"\t"<<lmbad_sec_tot<<"\t total primaries secondaries"<<endl;
  sortie<<"Bad in ideal                     : \t"<<bad_in_ideal<<"\t"<<bad_in_ideal_pri<<"\t"<<bad_in_ideal_sec<<endl;
  sortie<<"Bad not in ideal                 : \t"<<bad_notin_ideal<<"\t"<<bad_notin_ideal_pri<<"\t"<<bad_notin_ideal_sec<<endl;
  sortie<<"Bad found twice in bad           : \t"<<bad_found_twice<<"\t"<<bad_found_twice_pri<<"\t"<<bad_found_twice_sec<<endl;
  sortie<<endl;
  sortie<<"Initial number of good tracks    : \t"<<lmgood_tot<<"\t"<<lmgood_pri_tot<<"\t"<<lmgood_sec_tot<<"\t total primaries secondaries"<<endl;
  sortie<<"Good found twice in good         : \t"<<good_found_twice<<"\t"<<good_found_twice_pri<<"\t"<<good_found_twice_sec<<"\t (correct reconstructed tracks with copies in good)"<<endl;
  sortie<<"Real good tracks                 : \t"<<real_good<<"\t"<<real_good_pri<<"\t"<<real_good_sec<<"\t Initial minus the good found twice"<<endl;
  sortie<<endl;
  sortie<<"                Real ideal tracks: \t"<<real_ideal<<"\t"<<real_ideal_pri<<"\t"<<real_ideal_sec<<endl;
  sortie<<"                Real good tracks : \t"<<real_good<<"\t"<<real_good_pri<<"\t"<<real_good_sec<<endl;
  sortie<<"                Real bad tracks  : \t"<<lmbad_tot+good_found_twice<<"\t"<<lmbad_pri_tot+good_found_twice_pri<<"\t"<<lmbad_sec_tot+good_found_twice_sec<<endl;
  sortie<<"             Real efficiency (%) : \t"<<(real_good*100)/(real_ideal*1.)<<"\t"<<(real_good_pri*100)/(real_ideal_pri*1.)<<"\t"<<(real_good_sec*100)/(real_ideal_sec*1.)<<endl;
  sortie<<"             Real purity (%)     : \t"<<(real_good*100)/(lmgood_tot*1.+lmbad_tot*1.)<<"\t"<<(real_good_pri*100)/(lmgood_pri_tot*1.+lmbad_pri_tot*1.)<<"\t"<<(real_good_sec*100)/(lmgood_sec_tot*1.+lmbad_sec_tot*1.)<<endl;
  sortie<<"------------------------------------------------------------------------------------------"<<endl;
  }


  i=0;
  bd=0;
  gd=0;

  long jbgood=0,jbmin,lowchi[200];
  double chimin;

  for (i=0;i<200;i++) {
    bad[i]  = 0;
    good[i] = 0;
    lowchi[i] = 0;
    poss[i]=0;
  }

  for (i=0;i<mNTrack; i++) {
    if (mTrack[i]->mTPCTrack->mPt>mParams[0]->ptmax || mTrack[i]->mTPCTrack->mPt<=mParams[mNPass-1]->ptmin || mTrack[i]->mTPCTrack->GetFlag()<=0)
      continue;
    mcid = Eval_id_mctrk2est_Track[mTrack[i]->mTPCTrack->GetMcId()];
    jbmin = -1;
    chimin = 10000000;
    jbgood=-1;
    if (mTrack[i]->GetBranch(0)->GetNHits()>0)
      poss[mTrack[i]->GetNBranches()]++;
    for (jb=0;jb<mTrack[i]->GetNBranches();jb++) {
      if (mTrack[i]->GetBranch(jb)->GetNHits()==0) continue;
      ok = 1;
      matrix = 0;
      for (j=0;j<mTrack[i]->GetBranch(jb)->GetNHits();j++) {
	k=0;
	while (mTrack[i]->GetBranch(jb)->GetHit(j) != Eval_mchits[mcid][k] && Eval_mchits[mcid][k]!=NULL)
	  k++;
	matrix |= int(pow(2,mTrack[i]->GetBranch(jb)->GetHit(j)->GetWafer()->GetLayer()));
	if (Eval_mchits[mcid][k]==NULL) {
	  ok = 0;
	}
	//	if (ok==1) 
	//	  slay_good[Eval_mchits[i][k]->GetWafer()->GetLayer()]++;      
      }

      if (mTrack[i]->GetBranch(jb)->GetNHits()>0 && (onoffmatrix == (matrix & onoffmatrix))) {
	if (ok==0)	 
	  bad[jb]++;
	else 
	  good[jb]++;
      }

      if (mTrack[i]->GetBranch(jb)->GetChiSq() < chimin) {
	chimin = mTrack[i]->GetBranch(jb)->GetChiSq();
	jbmin = jb;
      }      
    }
    //    if(mTrack[i]->GetBranch(0)->GetNHits()>0 && _tmpgood!=i)
    //      cout << "bad track mTrack["<<i<<"]= "<< mTrack[i]<<endl;
    lowchi[jbmin]++;
  }

  //  cout <<"branch\tnbr\tgood\tbad\tlowchi"<<endl;
  gd=bd=ps=0;
  for (i=0;i<mParams[0]->nbranch[0];i++) {
    //    cout << i<<"\t"<<poss[i]<<"\t"<<good[i]<<"\t"<<bad[i]<<"\t"<<lowchi[i]<<endl;
    gd += good[i];
    bd += bad[i];
    ps += poss[i];
  }
  //  cout <<endl<<"\t"<<ps<<"\t"<<gd<<"\t"<<bd<<endl;

  int brhit, brnohit;
  long nbrfull=0, br5=0;
  long nbr=0;
  long wrong=0, full;  
  for (i=0;i<mNTrack;i++) {
    if (mTrack[i]->mTPCTrack->mPt>mParams[0]->ptmax || mTrack[i]->mTPCTrack->mPt<=mParams[mNPass-1]->ptmin || mTrack[i]->mTPCTrack->GetFlag()<=0)
      continue;
    brhit=brnohit=0;
    full=0;
    if (mTrack[i]->GetNBranches()==5) 
      br5++;
    for (j=0;j<mTrack[i]->GetNBranches();j++) {
      nbr++;
      if (mTrack[i]->mBranch[j]->GetNHits()>0) 
	brhit=1;
      else
	brnohit=1;
      if (mTrack[i]->mBranch[j]->GetNHits()>0) {
	nbrfull++;
	full=1;
      }
    }
    if (brhit==1 && brnohit==1) wrong++;
  }
  //  cout << "number of branches with at least one hit  : "<<nbrfull<<endl;
  //  cout << "total number of branches:                 : "<<nbr<<endl;
  //  cout << "number of wrong tracks                    : "<<wrong<<endl;
  //  cout << "tracks with 5 branches                    : "<<br5<<endl;

}

void StEstMaker::EvalOnLine(int lay) {

  long i,j,k,jb;
  long bad[200], good[200],poss[200],slay[200];
  long gd=0,bd=0,ps=0, ok;
  int matrix, onoffmatrix=0,goodlocal,goodpertrack[64];
  long possmatr[64],badmatr[64],goodmatr[64];

  onoffmatrix = int(pow(2,lay));

  if(mParams[0]->debug>2) cout << "StEstMaker::EvalOnLine ****START****"<<endl;
  for (i=0;i<200;i++) {
    bad[i]  = 0;
    good[i] = 0;
    poss[i] = 0;
    slay[i] = 0;
  }
  for (i=0;i<64;i++) {
    possmatr[i] = 0;
    goodmatr[i] = 0;
    badmatr[i] = 0;
    goodpertrack[i] = 0;
  }

  cout <<endl<<"*****  RESULTS *****"<<endl;  
  gd=0;
  bd=0;
  ps=0;
  for (i=0;i<mNTrack; i++) {
    matrix = 0;
    if (mTrack[i]->mTPCTrack->mPt > mParams[mPass]->ptmax || mTrack[i]->mTPCTrack->mPt <= mParams[mPass]->ptmin || mTrack[i]->mTPCTrack->GetFlag()<=0)
      continue;
    j=0;
    while (Eval_mchits[i][j]!=NULL && j<10) {      
      matrix = matrix | int(pow(2,Eval_mchits[i][j]->GetWafer()->GetLayer()));
      j++;    
    }
    if (j>0 && (matrix & onoffmatrix)) {
      poss[j]++;
      possmatr[matrix]++;
    }
    for(k=0;k<mTrack[i]->GetNBranches(); k++) 
      if(mTrack[i]->GetBranch(k)->GetNHits()==0) bd++;
    gd++;
    ps+=mTrack[i]->GetNBranches();
  }

  cout << "Number of tracks:         " <<gd<<endl;
  cout << "Number of branches:       " <<ps<<endl;
  cout << "Number of empty branches: " <<bd<<endl;  
  gd=0;
  bd=0;
  ps=0;

  for (i=0;i<mNTrack; i++) {    
    //    nohits = 0;
    ok = 1;
    matrix = 0;
    if (mTrack[i]->mTPCTrack->mPt>mParams[mPass]->ptmax || mTrack[i]->mTPCTrack->mPt<=mParams[mPass]->ptmin || mTrack[i]->mTPCTrack->GetFlag()<=0)
      continue;

    int _NBR=0;
    for(_NBR=0; _NBR<mTrack[i]->GetNBranches(); _NBR++) {
      ok = 1;
      matrix = 0;
      for (j=0;j<mTrack[i]->GetBranch(_NBR)->GetNHits();j++) {
	k=0;
	while (mTrack[i]->GetBranch(_NBR)->GetHit(j) != Eval_mchits[i][k] && Eval_mchits[i][k]!=NULL)
	  k++;
	matrix |= int(pow(2,mTrack[i]->GetBranch(_NBR)->GetHit(j)->GetWafer()->GetLayer()));
	if (Eval_mchits[i][k]==NULL) ok = 0;
      } // for(j...
      if (mTrack[i]->GetBranch(_NBR)->GetNHits()>0) {
	if (ok==0 || !(matrix & onoffmatrix)) {
	  bad[mTrack[i]->GetBranch(_NBR)->GetNHits()]++;
	  badmatr[matrix]++;
	}
	else {
	  good[mTrack[i]->GetBranch(_NBR)->GetNHits()]++;
	  goodmatr[matrix]++;
	}
      }
    } // for(_NBR...
  } // for(i...

  gd=0; 
  bd=0;
  ps=0;
  cout <<"Tracks\t\tPOSS\tOK\tNOK"<<endl;
  for (i=1;i<10;i++) {
    cout <<"with "<<i<<" hits :\t"<<poss[i]<<"\t"<<good[i]<<"\t"<<bad[i]<<endl;
    gd += good[i];
    bd += bad[i];
    ps += poss[i];
  }
  cout << endl << "TOTAL:\t\t" << ps << "\t" << gd << "\t" << bd << endl;

  bd=0;
  gd=0;
  ps=0;
  cout <<endl<<"1234\tPOSS\tGOOD\tBAD"<<endl;
  for (i=1;i<16;i++) {
    for (j=0;j<4;j++) {
      if (i & int(pow(2,j))) 
	cout << "1";
      else
	cout << "0";
    }
    cout << "\t"<<possmatr[i]<<"\t"<<goodmatr[i]<<"\t"<<badmatr[i]<<endl;

    gd += goodmatr[i];
    bd += badmatr[i];
    ps += possmatr[i];
  }  
  cout <<endl<<"TOTAL:\t"<<ps<<"\t"<<gd<<"\t"<<bd<<endl<<endl;

  i=0;
  bd=0;
  gd=0;
  ps=0;

  long jbgood=0,jbmin, maxjb, lowchi[200], chiideal[200];
  double chimin;
  long _tmp;

  for (i=0;i<200;i++) {
    bad[i]      = 0;
    good[i]     = 0;
    lowchi[i]   = 0;
    chiideal[i] = 0;
    poss[i]     = 0;
  }
  maxjb = 0;

  for (i=0;i<mNTrack; i++) {
    goodlocal =0;
    if (mTrack[i]->mTPCTrack->mPt>mParams[mPass]->ptmax || mTrack[i]->mTPCTrack->mPt<=mParams[mPass]->ptmin || mTrack[i]->mTPCTrack->GetFlag()<=0)
      continue;
    jbmin = -1;
    chimin = 10000000;
    jbgood=-1;
    if(maxjb<mTrack[i]->GetNBranches()) maxjb=mTrack[i]->GetNBranches();
    if (mTrack[i]->GetBranch(0)->GetNHits()>0)
      poss[mTrack[i]->GetNBranches()]++;
    for (jb=0;jb<mTrack[i]->GetNBranches();jb++) {
      if (mTrack[i]->GetBranch(jb)->GetNHits()==0) continue;
      ok = 1;
      matrix = 0;
      for (j=0;j<mTrack[i]->GetBranch(jb)->GetNHits();j++) {
	k=0;
	while (mTrack[i]->GetBranch(jb)->GetHit(j) != Eval_mchits[i][k] && Eval_mchits[i][k]!=NULL && k<10)
	  k++;
	matrix |= int(pow(2,mTrack[i]->GetBranch(jb)->GetHit(j)->GetWafer()->GetLayer()));
	if (Eval_mchits[i][k]==NULL || k==10)	 
	  ok = 0;
      }
      if (mTrack[i]->GetBranch(jb)->GetNHits()>0) {
	if (ok==0 || !(matrix & onoffmatrix))	 
	  bad[jb]++;
	else {
	  good[jb]++;
	  if(jbgood!=-1) {
	    if(mTrack[i]->GetBranch(jb)->GetChiSq()<mTrack[i]->GetBranch(jbgood)->GetChiSq())
	      jbgood = jb;
	  }
	  else 
	    jbgood = jb;
	  goodlocal++;
	}
      }

      if (mTrack[i]->GetBranch(jb)->GetChiSq() < chimin) {
	chimin = mTrack[i]->GetBranch(jb)->GetChiSq();
	jbmin = jb;
      }      
    } //for (jb=...

    goodpertrack[goodlocal]++;
    _tmp = 0;
    if(jbgood != -1){
      for (jb=0;jb<mTrack[i]->GetNBranches();jb++) 
	if(mTrack[i]->GetBranch(jb)->GetChiSq()<mTrack[i]->GetBranch(jbgood)->GetChiSq())
	  _tmp++;
      chiideal[_tmp]++;
    }

    lowchi[jbmin]++;
  } //for (i=...

  cout <<"branch\tnbr\tgood\tbad\tlowchi\tchiideal"<<endl;
  gd=bd=ps=0;
  for (i=0;i<maxjb+1;i++) {
    cout << i<<"\t"<<poss[i]<<"\t"<<good[i]<<"\t"<<bad[i]<<"\t"<<lowchi[i]<<"\t"<<chiideal[i]<<endl;
    gd += good[i];
    bd += bad[i];
    ps += poss[i]*i;
  }
  cout <<endl<<"TOTAL:\t"<<ps<<"\t"<<gd<<"\t"<<bd<<endl<<endl;
  
  cout << "Good branches in one track:"<<endl;
  cout << "no of branches\ttotal"<<endl;
  for (i=0;i<10;i++) 
    cout <<i<<"\t"<<goodpertrack[i]<<endl;
}
