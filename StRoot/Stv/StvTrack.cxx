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
#include "StvUtil/StvKNNUtil.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
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
  memset(mBeg,0,mEnd-mBeg+1);
  clear();
}
//______________________________________________________________________________
void StvTrack::unset()
{
static StvToolkit *kit = StvToolkit::Inst();
  for (StvNodeIter it = begin();it != end();++it) 
  {
    StvNode *node = *it;
    kit->FreeNode(node);		
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
    if (!node->GetHit()) 	continue;
    if (node->GetXi2()>1000) 	continue;
    if (detectorId) {
      const StHitPlane *hp = node->GetHitPlane();
      if (!hp) continue;
      if (hp->GetDetId()!=detectorId) continue;
    }
    n++;    
  }
  return n;
}   
//______________________________________________________________________________
int StvTrack::GetNFits(int dir) const
{
  int n=0;
  for (StvNodeConstIter it = begin(); it !=end();++it) {
    const StvNode *node = *it; 
    if (node->IsFitted(dir)) n++;    
  }
  return n;
}   
//______________________________________________________________________________
int StvTrack::SetUsed() 
{  
  int n = 0;
  for (StvNodeConstIter it = begin(); it !=end();++it) {
    StvNode *node = *it; 
    StvHit *hit = node->GetHit();
    if (!hit) continue;
    if (!hit->detector())	continue;
    if (node->GetXi2()<1000 && !hit->isUsed()) 	{ 
      hit->addTimesUsed();n++;  }
    else {			
      node->SetHit(0);	
    }
  }
  return n;
}   
//______________________________________________________________________________
int StvTrack::SetUnused() 
{  
  int n = 0;
  for (StvNodeConstIter it = begin(); it !=end();++it) {
    StvNode *node = *it; 
    StvHit *hit = node->GetHit();
    if (!hit) 			continue;
    if (!hit->detector())	continue;
    hit->setTimesUsed(0);n++;
    if (node->GetXi2()>1000) node->SetHit(0);	
  }
  
  return n;
}   
//______________________________________________________________________________
StvNode *StvTrack::GetNode(EPointType noTy)
{
  StvNode *node=0,*foundNode=0;int n=0;
  double maxXi2 = 0;
  if (noTy!=kLastPoint) {
    for (StvNodeIter it = begin();it != end();++it) {
      node = *it; n++; 
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
        case kMaxXi2:
	  if (node->GetType()!=StvNode::kRegNode) 	break;
	  if (!node->GetHit()) 		 		break;
          if ( node->GetXi2()<maxXi2) 			break;
          foundNode = node; maxXi2=node->GetXi2();	break;

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
  
  return foundNode;

}
//______________________________________________________________________________
const StvNode *StvTrack::GetNode(EPointType noTy) const
{ return ((StvTrack*)this)->GetNode(noTy); }
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
double StvTrack::GetXi2Aux() const
{
  if (!mXi2Aux) mXi2Aux = GetXi2();
  return mXi2Aux;
}
//_____________________________________________________________________________
double StvTrack::GetXi2P() const
{
  if (!IsPrimary()) return 0;
  const StvNode *node = GetNode(kPrimPoint);
  assert(node);
  double Xi2 = node->GetXi2();
  assert(Xi2<1000.);
  return Xi2;
}
  
//_____________________________________________________________________________
void StvTrack::CutTail(const StvNode *start)
{
static StvToolkit *kit = StvToolkit::Inst();
  if (empty()) return;
  if (!start) start = front();
  StvNodeIter tail = begin();
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
void StvTrack::CutEnds()
{
static StvToolkit *kit = StvToolkit::Inst();
  StvNodeIter it =begin();
   int nDel=0;
   for (; it != end();++it) 
     {
       StvNode *node = *it;
	if (node->GetHit()) break;
	nDel ++;kit->FreeNode(node);
    }
    if (nDel)  {erase(begin(),it);}

    it = end(); --it; 
    nDel=0;
    for (; it !=begin();--it) 
    {
       StvNode *node = *it;
	if (node->GetHit()) break;
	nDel ++; kit->FreeNode(node);
    }
    if (nDel)  { ++it; erase(it,end());}
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
//    const StvHit *hit = node->GetHit();
//    if (!hit && strchr(opt,'H')) continue;
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
void StvTrack::Reverse() 
{ reverse(); }
//______________________________________________________________________________
double StvTrack::GetXi2W() const
{
  if (mXi2W>0) return mXi2W;
  mXi2W = 0;
  for (auto it = begin();it != end();++it) 
  {
    auto *node = *it; 
    if (node->GetType()!=StvNode::kRegNode) 	continue;
    if (!(node->GetHit())) 		 	continue;
    double Xi2 = node->GetXi2();
    if ( Xi2>1000) 				continue;
    if ( Xi2 < mXi2W) 				continue;
    mXi2W = Xi2;
  }
  return  mXi2W;
}
//_____________________________________________________________________________
StvTrack &StvTrack::operator=(const StvTrack &from)
{
static StvToolkit *kit = StvToolkit::Inst();
  for (auto it = begin();it != end();++it) 
  {
    StvNode *node = *it; kit->FreeNode(node);		
  }
  clear(); 		
  memcpy(mBeg,from.mBeg,mEnd-mBeg+1);
  for (StvNodeConstIter it=from.begin();it!=from.end();++it) {
    const StvNode *node = (*it);
    StvNode *myNode = kit->GetNode();
    *myNode = *node; push_back(myNode);
  }
  return *this;
}
//______________________________________________________________________________
StvNode *StvTrack::GetMaxKnnNode() 
{
  StvNode *node=0;int n=0;
  float var[2];
  const StvHit *hit=0;
  StvKNNUtil knn(2,5);
  for (StvNodeIter it = begin();it != end();++it) 
  {
    node = *it; n++; 
    if (node->GetType()!=StvNode::kRegNode) 	continue;
    if (!(hit=node->GetHit())) 		 	continue;
    if ( node->GetXi2()>1000) 			continue;
    const StvNodePars &par = node->GetFP();
    const double cosL = par.getCosL();
    const float *fx = hit->x();
    var[1] = (fx[2]-par._z)*cosL;
    var[0] = ((fx[0]-par._x)*(-par._sinCA)+(fx[1]-par._y)*(par._cosCA));
    knn.Add((ULong_t)node,var);
  }
  knn.GetWost((ULong_t*)&node);
  return node;
}
#include "StarRoot/TIdTruUtil.h"
#include "StEvent/StRnDHit.h"
//_____________________________________________________________________________
double StvTrack::GetQua() const
{

  TIdTruUtil idt;
  const StvHit *hit=0;
  
  for (StvNodeConstIter it = begin();it != end();++it) 
  {
    StvNode *node = *it; 
    if (node->GetType()!=StvNode::kRegNode) 	continue;
    if (!(hit=node->GetHit())) 		 	continue;
    if ( node->GetXi2()>1000) 			continue;
    int idTru = hit->idTru();   
#ifdef kFtsIdentifier
    if (!idTru && hit->detectorId()==kFtsId) {
      auto *rndHit = (StRnDHit*)hit->stHit();
      int id0 = rndHit->extraByte0();
      int id1 = rndHit->extraByte1();
      assert (id0 && id1);
      idt.Add(id0,50);
      idt.Add(id1,50);
      continue;
    }       
#endif
    idt.Add(idTru);
  }
  if (!idt.GetIdTru()) return 0;
  return idt.GetQua();

}



