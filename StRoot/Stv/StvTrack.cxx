#include <TVector3.h>
#include <Stiostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "Stv/StvHit.h"
#include "Stv/StvNode.h"
#include "Stv/StvTrack.h"
#include "Stv/StvDraw.h"
#include "Stv/StvToolkit.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"
int StvTrack::mgId=0;

//______________________________________________________________________________
StvTrack::StvTrack()
{
  mId = -1; 		
}

//______________________________________________________________________________
StvTrack::~StvTrack()
{
  unset(); 		
}
//______________________________________________________________________________
void StvTrack::reset()
{
  mId = ++mgId;
  mPrimary=0;
  clear();
}
//______________________________________________________________________________
void StvTrack::unset()
{
  mId = -1;
  static StvToolkit *kit = StvToolkit::Inst();
  for (StvNodeIter it = begin();it != end();++it) 
  {
    StvNode *node = *it;kit->FreeNode(node);
  }
  clear();
}
//______________________________________________________________________________
int StvTrack::GetNPoss(StDetectorId detectorId) const
{
  int n=0;
  for (StvNodeConstIter it = begin(); it !=end();++it) {
    const StvNode *node = *it; 
    const StHitPlane *hp = node->GetHitPlane();
    if (!hp) continue;
    if (detectorId && hp->GetDetId()!=detectorId) continue;
    n++;
  }
  return n;
}   
//______________________________________________________________________________
int StvTrack::GetNHits(StDetectorId detectorId) const
{
  int n=0;
  for (StvNodeConstIter it = begin(); it !=end();++it) {
    const StvNode *node = *it; 
    const StHitPlane *hp = node->GetHitPlane();
    if (!hp) continue;
    if (detectorId && hp->GetDetId()!=detectorId) continue;
    if (!node->GetHit()) 	continue;
    if (node->GetXi2()>1000) 	continue;
    n++;    
  }
  return n;
}   
//______________________________________________________________________________
StvNode *StvTrack::GetNode(EPointType noTy)
{
  StvNode *node=0;int n=0;
  if (noTy!=kLastPoint) {
    for (StvNodeIter it = begin();it != end();++it) {
      node = *it; n++; if (node->GetXi2()>1000) continue;
      switch(noTy) {
        case kDcaPoint: if (n>2) return 0;
	  if (node->GetType()==StvNode::kDcaNode) {return node;} 
          break;

        case kPrimPoint: 
	  if (node->GetType()==StvNode::kPrimNode) {return node;} 
          return 0;

        case kFirstPoint:
	  if (node->GetType()!=StvNode::kRegNode) 	break;
	  if (!node->GetHit()) 		 		break;
	  if ( node->GetXi2()>1000) 		 	break;
          return node;
        default: assert("Wrong Node type" && 0);
       }//end switch
     }//end loop
   } else {
    for (StvBakwNodeIter it = rbegin();it != rend();++it) {
      node = *it; n++; 
      if (node->GetType()!=StvNode::kRegNode) 	continue;
      if (!node->GetHit()) 		 	continue;
      if ( node->GetXi2()>1000) 		continue;
      return node;
    }
  }// end if
  return 0;

}
//______________________________________________________________________________
const StvNode *StvTrack::GetNode(EPointType noTy) const
{ return ((StvTrack*)this)->GetNode(noTy); }
//______________________________________________________________________________
int StvTrack::CountHits(StvHitCount &cnt) const
{
  cnt.Clear();
  for (StvNodeConstIter it = begin();it != end();++it) 
  {
    StvNode *node = *it;
    if (node->GetType()!= StvNode::kRegNode) 	continue;
    if (!node->GetHitPlane()) 			continue;
    if (node->GetXi2()<1000) {cnt.AddHit();} else {cnt.AddNit();}
  }
  return cnt.nGoodHits;
}
//_____________________________________________________________________________
double StvTrack::GetLength(EPointType ept) const
{
  const StvNodePars *pre=0;
  double len = 0;
  for (StvNodeConstIter it = begin();it != end();++it) 
  {
    StvNode *node = *it;
    if (!pre) {
      StvNode::ENodeType ty = node->GetType();
      if (ty == StvNode::kPrimNode) {
        if (ept!=kPrimPoint) 		continue;
        pre = &(node->GetFP(2)); 	continue;
      }
      if (ty == StvNode::kDcaNode) {
        if (ept!=kDcaPoint) 		continue;
        pre = &(node->GetFP(2)); 	continue;
      }
      if (ty != StvNode::kRegNode) 	continue;
      if (node->GetXi2()>1000) 		continue;
        pre = &(node->GetFP(2)); 	continue;
    }
    const double *x1 = &pre->_x;
    const double *x2 = &node->GetFP(2)._x;
    double dlen = sqrt(pow(x1[0]-x2[0],2) + pow(x1[1]-x2[1],2));
    double curv = 0.5*fabs(pre->_curv+node->GetFP(2)._curv);
    double dsin = (0.5*dlen*curv);
    if (dsin>0.9) dsin=0.9;
    dlen = (dsin<0.1)? dlen*(1.+dsin*dsin/6) : 2*asin(dsin)/curv; 
    len +=sqrt(dlen*dlen + pow(x1[2]-x2[2],2));
    pre = &(node->GetFP(2)); 	
  }
  return len;

}
//_____________________________________________________________________________
double StvTrack::GetXi2() const
{
  double Xi2 = 0;
  int nd=0;
  for (StvNodeConstIter it = begin();it != end();++it) 
  {
    const StvNode *node = *it;
    if (!node->GetHit()) continue;
    if (node->GetXi2()>1000) continue;
    nd++; Xi2+=node->GetXi2();
  }
  nd = nd*2-5;
 
  return (nd>0)? Xi2/nd:0;

}
//_____________________________________________________________________________
int StvTrack::ReleaseHits()
{
  int nd=0;
  for (StvNodeIter it = begin();it != end();++it) 
  {
    StvNode *node = *it;
    StvHit *hit = node->GetHit();
    if (!hit) 			continue;
    if (!node->GetDetId())	continue;
    node->SetHit(0);
    nd++;
  }
  return nd;
}
//_____________________________________________________________________________
void StvTrack::CutTail(const StvNode *start)
{
static StvToolkit *kit = StvToolkit::Inst();
  if (empty()) return;
  if (!start) start = front();
  StvNodeIter tail;
  int kase=0;
  for (StvNodeIter it = begin();it != end();++it) 
  {
    StvNode *node = *it;
    switch (kase) {
      case 0: if (node !=start) break;
              kase=1; tail=it;
      case 1: node->SetHit(0);
              kit->FreeNode(node);		
    }
  }
  assert(kase);
  erase(tail,end());
}
//_____________________________________________________________________________
double StvTrack::Approx(int mode)
{
  THelixFitter hlx;
  int iNode=0;
  StvNode *fstNode = 0,*lstNode = 0;
  for (StvNodeIter it=begin();it!=end(); ++it) {
    StvNode *node = *it; iNode++;
    const StvHit *hit= node->GetHit();
    if (!hit) continue;
    if (!fstNode) fstNode = node;
    lstNode = node;
    hlx.Add(hit->x()[0],hit->x()[1],hit->x()[2]);
  }  
  double Xi2 =hlx.Fit();
  double dL = hlx.Path(fstNode->GetFP().P);
  hlx.Move(dL);

  if (!mode) {//Printout only
    double myPsi = atan2(hlx.Dir()[1],hlx.Dir()[0]);
    double myTan = tan(asin(hlx.Dir()[2]));
    double myCur = hlx.GetRho();
    printf("StvTrack::Approx(fstHelx) Xi2=%g \tPsi,Tan,Curv=%g %g %g\n",  Xi2,myPsi,myTan,myCur);
    dL = hlx.Path(lstNode->GetFP().P); hlx.Move(dL);
    myPsi = atan2(hlx.Dir()[1],hlx.Dir()[0]);
    myTan = tan(asin(hlx.Dir()[2]));
    myCur = hlx.GetRho();
    printf("StvTrack::Approx(lstHelx) Xi2=%g \tPsi,Tan,Curv=%g %g %g\n",  Xi2,myPsi,myTan,myCur);
    myPsi = fstNode->GetFP()._psi;
    myTan = fstNode->GetFP()._tanl;
    myCur = fstNode->GetFP()._curv;
    double myXi2 = fstNode->GetXi2();
    printf("StvTrack::Approx(fstNode) Xi2=%g \tPsi,Tan,Curv=%g %g %g\n",myXi2,myPsi,myTan,myCur);
    myPsi = lstNode->GetFP()._psi;
    myTan = lstNode->GetFP()._tanl;
    myCur = lstNode->GetFP()._curv;
    myXi2 = lstNode->GetXi2();
    printf("StvTrack::Approx(lstNode) Xi2=%g \tPsi,Tan,Curv=%g %g %g\n",myXi2,myPsi,myTan,myCur);
    return Xi2;
  }
  return Xi2;
}
//_____________________________________________________________________________
double StvTrack::GetRes() const
{
  int nRes=0; double res = 0;
  for (StvNodeConstIter it=begin();it!=end(); ++it) {
    StvNode *node = *it;
    const StvHit *hit= node->GetHit();
    if (!hit) continue;
    TVector3 dif,dir;
    const StvNodePars &fp = node->GetFP();
    for (int i=0;i<3;i++) { dif[i]=fp.P[i]-hit->x()[i];}
    dir[0]= fp._cosCA; 
    dir[1]= fp._sinCA; 
    dir[2]= fp._tanl; 
    dir = dir.Unit();
    res += (dif.Cross(dir)).Mag(); nRes++;
  }  
  return (nRes)? res/nRes:0.;
}

//_____________________________________________________________________________
void StvTrack::Show() const
{
StvHits   showHits;
StvPoints showTrak;
  for (StvNodeConstIter it = begin();it != end();++it) 
  {
    const StvNode *node = *it;
    const StvHit  *hit  = node->GetHit();
    showTrak += node->GetFP().P;
    if (hit) showHits+=(StvHit*)hit;
  }
  StvDraw::Inst()->Trak(showTrak,kGlobalTrack);
  StvDraw::Inst()->Hits(showHits,kUsedHit    );
}
//_____________________________________________________________________________
int StvTrack::Check(const char *tit, int dirs) const
{
if (!tit) tit = "";
  int n = -1,nerr=0;
  for (StvNodeConstIter it = begin();it != end();++it) 
  {
    n++; 
    const StvNode *node = *it;
    TString ts;
    if (tit[0]) {ts = tit; ts+="#"; ts+=n;
      char mybuf[40]={0}; sprintf(mybuf," node=%p ",(void*)node);
      ts+=mybuf;
    }
    int fail = node->Check(ts,dirs);
    if (fail) nerr++;
  }
  return nerr;
}
//_____________________________________________________________________________
void StvTrack::Print(const char *opt) const
{
  if (!opt) opt = "";
  printf("Track %p\n",(void*)this);

  int n=0;
  for (StvNodeConstIter it=begin();it!=end();++it) {
    const StvNode *node = (*it);
    const StvHit *hit = node->GetHit();
    if (!hit && strchr(opt,'H')) continue;
    n++;printf("%3d - ",n);
    node->Print(opt);
  }
}
//_____________________________________________________________________________
double StvTrack::ToBeam() const
{
  const StvNode *node = GetNode(kDcaPoint);
  if (!node) return 3e33;
  return node->GetFP().getRxy();
}
//_____________________________________________________________________________
int StvTrack::GetCharge() const
{
  const StvNode *node = front();
  return node->GetFP().getCharge();
}
//_____________________________________________________________________________
void StvTrack::MakeFitTally() 
{
  StvNodeConstIter it,itBeg,itEnd;
  itBeg = end(); --itBeg; itEnd = begin();--itEnd;
  int nFit = 0;
  for (it=itBeg;it!=itEnd;--it) {
    StvNode *node = (*it);
    node->mFitTally[0] = nFit;
    if (!node->GetHit()) 		continue;
    if (node->GetXi2(0) > 1000 )	continue;
    nFit++;
  }
  itBeg = begin(); itEnd = end();
  nFit = 0;
  for (it=itBeg;it!=itEnd;++it) {
    StvNode *node = (*it);
    node->mFitTally[1] = nFit;
    if (!node->GetHit()) 		continue;
    if (node->GetXi2(1) > 1000 )	continue;
    nFit++;
  }
}
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
enum {kTotHits=10	//Min number hits for track
     ,kGoodHits=5	//Min number good hits for track
     ,kContHits=2	//Min length of good hit sequence
     ,kContNits=8	//Max length of acceptable non hit sequence
     ,kTotNits=13	//Max number of acceptable non hits
     };
//_____________________________________________________________________________
void StvHitCount::AddHit()
{
  nPossHits++;  nTotHits++;nContHits++;
  if (!nContNits)		return;
  if (nContHits<kContHits) 	return;
  if (nContNits>kContNits) nSeqLong++;
  nContNits=0;nSeqNits++;
    
}

//_____________________________________________________________________________
void StvHitCount::AddNit()
{
  nPossHits++;nContNits++;nTotNits++;
  if (!nContHits) 	return;
  if (nContHits<kContHits) {nSeqShort++;} else { nGoodHits+=nContHits;}
  nContHits=0;nSeqHits++;
}
//_____________________________________________________________________________
int StvHitCount::Reject()
{
    if (nContNits) {
      if (nContNits>kContNits) nSeqLong++;
      nContNits=0;nSeqNits++;
    }
    if (nContHits) {
      if (nContHits<kContHits) {nSeqShort++;} else { nGoodHits+=nContHits;}
      nContHits=0;nSeqHits++;
    }
  return nGoodHits<kGoodHits || nTotHits<kTotHits || nTotNits > kTotNits;
}
//_____________________________________________________________________________
int StvHitCount::Skip() const
{
  if (nContNits>kContNits) return 1;
  if (nTotNits > kTotNits) return 2;
  return 0 ;
}
