/***************************************************************************
 *
 * $Id: StEstEval.cxx,v 1.6 2003/09/02 17:58:04 perev Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Methods for the Evaluation
 *
 ***************************************************************************
 *
 * $Log: StEstEval.cxx,v $
 * Revision 1.6  2003/09/02 17:58:04  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  2001/04/23 12:29:21  lmartin
 * Information on the findable pattern added in the results vs the segment patterns
 * to facilitate the parameter tuning.
 *
 * Revision 1.4  2001/03/02 16:05:05  lmartin
 * Save the numbers of ideal, good and bad tracks.
 *
 * Revision 1.3  2001/01/31 16:43:08  lmartin
 * mParams[]->debug replaced by mDebug
 *
 * Revision 1.2  2001/01/25 17:43:07  lmartin
 * Histogram filling removed
 * Evaluation result dumping into an external file removed
 * Defined as a method of the StEstTracker class
 *
 * Revision 1.1  2000/12/07 11:14:21  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StEstTracker.h"
#include "StEstParams.hh"
#include "Infrastructure/StEstWafer.hh"
#include "Infrastructure/StEstBranch.hh"
#include "Infrastructure/StEstHit.hh"
#include "Infrastructure/StEstTrack.hh"
#include "Infrastructure/StEstTPCTrack.hh"

void StEstTracker::Eval(int onoffmatrix, int nminhit) {
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
  long possfmatr[64],badfmatr[64],goodfmatr[64];
  int nPerfectHit;

  double maxdist;


  if(mDebugLevel>2) cout << "Eval :: Start"<<endl;

  cout<<"+------------------------------------------------+"<<endl;
  cout <<"| EVAL for onoffmatrix= "<<onoffmatrix
       <<" and nminhit="<<nminhit<<" \t |"<<endl;
  cout<<"+------------------------------------------------+"<<endl;
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
    possfmatr[i]  = 0;
    goodimatr[i] = 0;
    goodfmatr[i] = 0;
    badimatr[i]  = 0;
    badfmatr[i]  = 0;
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
      matrix = matrix | 1<<Eval_mchits[mcid][j]->GetWafer()->GetLayer();
      j++;    
    }
    nPerfectHit=0;
    for (k=0;k<4;k++) 
      nPerfectHit = nPerfectHit + 
	(((1<<k)) == ((1<<k) & matrix));

    if (j>0 && (onoffmatrix == (matrix & onoffmatrix)) 
	&& nPerfectHit>=nminhit) {
	  poss[j]++;
	  possmatr[matrix]++;
	  possfmatr[mTrack[i]->GetFindablePattern()]++;
	  lmideal_id[lmideal_tot]=mTrack[i]->mTPCTrack->mId;
	  lmideal_i[lmideal_tot]=i;
	  lmideal_pat[lmideal_tot]=matrix;
	  lmideal_tot++;
	  if (mTrack[i]->mTPCTrack->mType==1) lmideal_pri_tot++;	
	  if (mTrack[i]->mTPCTrack->mType==2) lmideal_sec_tot++;	
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
      matrix2 = matrix2 | (1<<Eval_mchits[mcid][j]->GetWafer()->GetLayer());
      j++;    
    }

    int _NBR=0;
    k=0;
    
    for (j=0;j<mTrack[i]->GetBranch(_NBR)->GetNHits();j++) {
      matrix |= 1<<mTrack[i]->GetBranch(_NBR)->GetHit(j)->GetWafer()->GetLayer();
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
	badfmatr[mTrack[i]->GetFindablePattern()]++;
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
	goodfmatr[mTrack[i]->GetFindablePattern()]++;
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
  cout << "layer|\t\t\t| final\tpattern\t| ideal\tpattern\t| finda\tpattern"<<endl;
  cout << "0123 | POSS\t| FIND\t| GOOD\tBAD\t| GOOD\tBAD\t| GOOD\tBAD"<<endl;
  for (i=0;i<16;i++) {
    for (j=0;j<4;j++) {
      if (i & 1<<j) 
	cout << "1";
      else
	cout << "0";
    }
    cout << " | "<<possmatr[i]<<"\t| "<<possfmatr[i]<<"\t| "<<goodmatr[i]<<"\t"<<badmatr[i]
	 <<"\t| "<<goodimatr[i]<<"\t"<<badimatr[i]
	 <<"\t| "<<goodfmatr[i]<<"\t"<<badfmatr[i]<<endl;
  }

  cout<<endl;

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

  if (mSuperPass==mNSuperPass-1) {
    mNIdealPrim=real_ideal_pri;
    mNIdealSeco=real_ideal_sec;
    mNGoodPrim=real_good_pri;
    mNGoodSeco=real_good_sec;
    mNBadPrim=lmbad_pri_tot+good_found_twice_pri;
    mNBadSeco=lmbad_sec_tot+good_found_twice_sec;
  }

  i=0;
  bd=0;
  gd=0;

  long jbgood=0,jbmin;
  double chimin;

  for (i=0;i<200;i++) {
    bad[i]  = 0;
    good[i] = 0;
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
	matrix |= 1<<mTrack[i]->GetBranch(jb)->GetHit(j)->GetWafer()->GetLayer();
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
  }

  gd=bd=ps=0;
  for (i=0;i<mParams[0]->nbranch[0];i++) {
    gd += good[i];
    bd += bad[i];
    ps += poss[i];
  }

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

