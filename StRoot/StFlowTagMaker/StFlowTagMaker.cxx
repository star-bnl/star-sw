/***************************************************************************
 *
 * $Id: StFlowTagMaker.cxx,v 1.1.1.1 1999/07/22 18:34:39 snelling Exp $
 *
 * Author: Raimond Snellings, LBNL, Jun 1999
 * Description:  Maker to fill the Flow EbyE Tag
 *
 ***************************************************************************
 *
 * $Log: StFlowTagMaker.cxx,v $
 * Revision 1.1.1.1  1999/07/22 18:34:39  snelling
 * FlowTagMaker: fills FlowTags defined by EbE workgroup
 *
 *  
 **************************************************************************/
#include "StFlowTagMaker.h"
#include "StEvent.h"
#include "StGlobalTrack.h"
#include "StChain/StChain.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TMath.h"
#include "StThreeVector.hh"
#include "StLorentzVector.hh"
#include <stdlib.h>

ClassImp(StFlowTagMaker)

StFlowTagMaker::StFlowTagMaker(const Char_t *name, const Char_t *title) 
  : StMaker(name, title)
{
  mFlowTag = 0;
  mEvent = 0;
}

StFlowTagMaker::~StFlowTagMaker() 
{
  delete mFlowTag; //clean up
}

Int_t StFlowTagMaker::Make() 
{

  // Create a new tag
  mFlowTag = new FlowTag_st;

  // Get a pointer to the DST
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (!mEvent) return kStOK; // If no event, we're done
  
  // fill the Flow Tag 
  fillFlowTag();
  
  return kStOK;
}

void StFlowTagMaker::PrintInfo() 
{
  cout << "$Id: StFlowTagMaker.cxx,v 1.1.1.1 1999/07/22 18:34:39 snelling Exp $" << endl;
  if (Debug()) StMaker::PrintInfo();
}

void StFlowTagMaker::printTag(ostream& os) 
{
  os << "--- Event-by-Event Flow Tag Table ---" << endl; 
  if (!mFlowTag) 
    os << "(empty FlowTag)" << endl;
  else {
    int i;
    os <<  "qxa";
    for(i=0; i<4; i++) os << "[" << i << "] =" << mFlowTag->qxa[i] << ' ';
    os << endl;
  }
}

FlowTag_st* StFlowTagMaker::tag() 
{
  return mFlowTag;
}

void StFlowTagMaker::fillFlowTag() 
{
  // Initialize Iterator, loop variables
  const int       isNotDefined       = -1;
  const double    trackquality[3][2] = {{10, 0}, 
					{ 0, 0},
					{ 0, 0}}; 
  const double    bField             = 0.5*tesla;
  
  StTrackCollection* tracks          = mEvent->trackCollection();
  StTrackIterator    itr;

  // i,j should be defined inside every loop however compiler does not like that
  long       i,j;

  // fill FlowTag with cuts 
  for (i = 0; i < 3; i++) {
    for (j = 0; j < 2; j++) {
      mFlowTag->qVector[i][j] = trackquality[i][j];
    }
  }

  // track loop
  long initialMultiplicity = tracks->size();
  float *mPhiAngle = new float[initialMultiplicity];
  float *mPseudoRapidity = new float[initialMultiplicity];
  float *mPt = new float[initialMultiplicity];
  
  long TrackCount = 0;
  for (itr = tracks->begin(), i=0; itr != tracks->end(); itr++) {
    StGlobalTrack* gtrk = *itr;
    StTrackFitTraits& fitTraits = gtrk->fitTraits();
    int nFitPoints = fitTraits.numberOfFitPoints();
    if ((double) nFitPoints > trackquality[0][0]) {
      StThreeVectorD p = gtrk->helix().momentum(bField); 
      mPhiAngle[i] = p.phi();
      mPseudoRapidity[i] = p.pseudoRapidity();
      mPt[i] = p.perp(); 
      TrackCount++;
      i++;
    }
  }

  cout << "after loop over tracks TrackCount = " << TrackCount << endl;

  // Make random subevents routine looks translated from Fortran so 1..n
  float *mRandomVector = new float[TrackCount + 1];
  long  *mIndexVector = new long[TrackCount + 1];
  // Fill array with random numbers
  for (i = 0; i <= TrackCount ; i++) 
    {mRandomVector[i] = rand()/(float)RAND_MAX;}
  // get an Index Vector which sorts the array  
  indexx(TrackCount, mRandomVector, mIndexVector);


  // Make subevent arrays
  long TrackCountSub1 = (long) ceil(TrackCount/2.);
  float *mPhiAngleSub1 = new float[TrackCountSub1];
  float *mPseudoRapiditySub1 = new float[TrackCountSub1];
  float *mPtSub1 = new float[TrackCountSub1];
  long TrackCountSub2 = (long) floor(TrackCount/2.);
  float *mPhiAngleSub2 = new float[TrackCountSub2];
  float *mPseudoRapiditySub2 = new float[TrackCountSub2];
  float *mPtSub2 = new float[TrackCountSub2];

  long Sub1Counter =0;
  long Sub2Counter =0;
  double SumPtSub1 =0.;
  double SumPtSub2 =0.;

  for (i = 0; i < TrackCount ; i++) {
    if (mIndexVector[i + 1] <= TrackCountSub1) {
      mPhiAngleSub1[Sub1Counter] = mPhiAngle[i];
      mPseudoRapiditySub1[Sub1Counter] = mPseudoRapidity[i];
      mPtSub1[Sub1Counter] = mPt[i];
      SumPtSub1 += mPt[i];
      Sub1Counter++;
    }
    else {
      mPhiAngleSub2[Sub2Counter] = mPhiAngle[i];
      mPseudoRapiditySub2[Sub2Counter] = mPseudoRapidity[i];
      mPtSub2[Sub2Counter] = mPt[i];
      SumPtSub2 += mPt[i];
      Sub2Counter++;
    }
  }

  cout << "TrackCountSub1, Sub1Counter, TrackCountSub2, Sub2Counter" << endl;
  cout << TrackCountSub1 << " " << Sub1Counter << " " 
       << TrackCountSub2 << " " << Sub2Counter << endl; 


  for (i = 0; i < 5 ; i++) {
    double Qx, Qy, EventPlaneAngle;
    // calculate plane and q vectors first subevent
    eventPlane(TrackCountSub1, mPseudoRapiditySub1, mPhiAngleSub1, mPtSub1, 
	       &Qx, &Qy, &EventPlaneAngle, i+1, 0);
    cout << "EventPlaneAngle = " << EventPlaneAngle << endl;  
    // fill tags
    mFlowTag->qxa[i] = Qx;
    mFlowTag->qya[i] = Qy;
    mFlowTag->na[i] = TrackCountSub1;
    mFlowTag->spta[i] = SumPtSub1;
    // calculate plane and q vectors second subevent
    eventPlane(TrackCountSub2, mPseudoRapiditySub2, mPhiAngleSub2, mPtSub2, 
	       &Qx, &Qy, &EventPlaneAngle, i+1, 0);
    cout << "EventPlaneAngle = " << EventPlaneAngle << endl;  
    // fill tags
    mFlowTag->qxb[i] = Qx;
    mFlowTag->qyb[i] = Qy;
    mFlowTag->nb[i] = TrackCountSub2;
    mFlowTag->sptb[i] = SumPtSub2;
    // calculate plane and q vectors first subevent with additional weights
    eventPlane(TrackCountSub1, mPseudoRapiditySub1, mPhiAngleSub1, mPtSub1, 
	       &Qx, &Qy, &EventPlaneAngle, i+1, 1);
    cout << "EventPlaneAngle = " << EventPlaneAngle << endl;  
    // fill tags
    mFlowTag->qxc[i] = Qx;
    mFlowTag->qyc[i] = Qy;
    mFlowTag->nc[i] = TrackCountSub1;
    mFlowTag->sptc[i] = SumPtSub1;
    // calculate plane and q vectors second subevent with additional weights
    eventPlane(TrackCountSub2, mPseudoRapiditySub2, mPhiAngleSub2, mPtSub2, 
	       &Qx, &Qy, &EventPlaneAngle, i+1, 1);
    cout << "EventPlaneAngle = " << EventPlaneAngle << endl;  
    // fill tags
    mFlowTag->qxd[i] = Qx;
    mFlowTag->qyd[i] = Qy;
    mFlowTag->nd[i] = TrackCountSub2;
    mFlowTag->sptd[i] = SumPtSub2;
  }

  delete [] mPhiAngle;
  delete [] mPseudoRapidity;
  delete [] mPt;
  delete [] mRandomVector;
  delete [] mIndexVector;
  delete [] mPhiAngleSub1;
  delete [] mPseudoRapiditySub1;
  delete [] mPtSub1;
  delete [] mPhiAngleSub2;
  delete [] mPseudoRapiditySub2;
  delete [] mPtSub2;

}


Int_t StFlowTagMaker::eventPlane(long Multiplicity, float *mPseudoRapidity, 
				 float *mPhiAngle, float *mPt, double *mQx, 
				 double *mQy, double *mEventPlaneAngle, 
				 int OrderParameter, int PhiYWeigt)
{
  //initialize return variables
  const int MinimumMultiplicity = 1;
  const int MinimumOrder = 1;
  const int MaximumOrder = 5;
  *mQx = 0;
  *mQy = 0;
  *mEventPlaneAngle = 0;
  
  if (Multiplicity <= MinimumMultiplicity) {
    cout <<  
      "Selected multiplicity not usefull to calculate event plane" 
	 << endl;
    return 0;
  }
  if (OrderParameter < MinimumOrder || OrderParameter > MaximumOrder) {
    cout <<  
      "Selected order not valid to calculate event plane" 
	 << endl;
    return 0;
  }

  for (long i=0; i < Multiplicity; i++) {
    float weight = 1;
    if (PhiYWeigt) {
      phiRapidityWeight(mPhiAngle[i], mPseudoRapidity[i], mPt[i], &weight);
    }
    if (mPseudoRapidity[i] < 0 && OrderParameter % 2 == 1) {
      weight *= -1;
    }
    *mQx += weight * cos(mPhiAngle[i] * OrderParameter);
    *mQy += weight * sin(mPhiAngle[i] * OrderParameter);
  }
  
  *mEventPlaneAngle = atan2(*mQy,*mQx) / OrderParameter;
  if (*mEventPlaneAngle < 0) {
    *mEventPlaneAngle += twopi / OrderParameter;
  }
  return kStOK;
}

Int_t StFlowTagMaker::phiRapidityWeight(float PhiAngle, float PseudoRapidity, 
					float Pt, float *weight)
{
  
  *weight = 1;
  
  return kStOK;
}

void StFlowTagMaker::SWAP(long &a,long &b)
{
  long itemp = a;
  a = b;
  b = itemp;
}

void StFlowTagMaker::indexx(long n,float arr[], long indx[])
{
  //
  //routine from Numerical Recipes in C pag. 338
  // Indexes an array arr[1..n], i.e. outputs the array indx[1..n]
  // such that arr[indx[j]] is in ascending order for j = 1,2..,N
  // The input quantities n and arr are not changed
  //

  const int M=7, NSTACK=50;
  //  int *ivector(long nl, long nh);
  //  void free_ivector(int *v, long nl, long nh);
  long i, indxt, ir=n, j, k, l=1;
  int jstack=0;
  int *istack;
  float a;
  
  //  istack=ivector(1,NSTACK);
  istack = new int[NSTACK+1];
  for (j=1;j<=n;j++) indx[j]=j;
  for (;;) {
    if (ir-l < M) {
      for (j=l+1;j<=ir;j++) {
	indxt=indx[j];
	a=arr[indxt];
	for (i=j-1;i>=1;i--) {
	  if (arr[indx[i]] <= a) break;
	  indx[i+1]=indx[i];
	}
	indx[i+1]=indxt;
      }
      if (jstack == 0) break;
      ir=istack[jstack--];
      l=istack[jstack--];
    } 
    else {
      k=(l+ir) >> 1;
      SWAP(indx[k],indx[l+1]);
      if (arr[indx[l+1]] > arr[indx[ir]]) {
	SWAP(indx[l+1],indx[ir]);
      }
      if (arr[indx[l]] > arr[indx[ir]]) {
	SWAP(indx[l],indx[ir]);
      }
      if (arr[indx[l+1]] > arr[indx[l]]) {
	SWAP(indx[l+1],indx[l]);
      }
      i=l+1;
      j=ir;
      indxt=indx[l];
      a=arr[indxt];
      for (;;) {
	do i++; while (arr[indx[i]] < a);
	do j--; while (arr[indx[j]] > a);
	if (j < i) break;
	SWAP(indx[i],indx[j]);
      }
      indx[l]=indx[j];
      indx[j]=indxt;
      jstack += 2;
      if (jstack > NSTACK) {cout <<"NSTACK too small in indexx." << endl;}
      if (ir-i+1 >= j-l) {
	istack[jstack]=ir;
	istack[jstack-1]=i;
	ir=j-1;
      } 
      else {
	istack[jstack]=j-1;
	istack[jstack-1]=l;
	l=i;
      }
    }
  }
  //  free_ivector(istack,1,NSTACK);
  delete [] istack;
}

