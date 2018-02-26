/*******************************************************************
 *
 * $Id: StTofrGeometry.cxx,v 1.12 2018/02/26 23:26:51 smirnovd Exp $
 * 
 * Authors: Shuwei Ye, Xin Dong
 *******************************************************************
 *
 * Description: Collection of geometry classes for the TOF-MRPC
 *              initializes from GEANT geometry
 *
 *******************************************************************/
#include "Stiostream.h"
#include <math.h>
#include <vector>
#include <stdlib.h>
#include <stdio.h>

#include "StTofrGeometry.h"
#include "TFile.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "StMaker.h"
//#include "StMemStat.h"
#include "StMessMgr.h"

//#include "Debugger.h"

//////////////////////////////////////////////////////////////////////////
//
// group of classes for Tofr Geometry:
//
//    StTofrGeometry(v1), StTofrNode(v2), StTofrGeomNode(v2)(off)
//    StTofrGeomTray(v1), StTofrGeomSensor(v2), StTofrGeomCell(off)
//
// Usage:
//
//   StTofrGeometry* geo = new StTofrGeometry("tofr","tofr geometry");
//    // geo->Init(TVolume *starHall, const Int_t TofrConf);
//    geo->Init(TVolume *starHall);
//
// ---------------------------------------------------------------------------
//
// StTofrNode
// ==============
//
//////////////////////////////////////////////////////////////////////////////


Bool_t StTofrNode::mDebug = kFALSE;
Double_t const StTofrGeomSensor::mSensorDy = 10.35;   // Actual module length;
char* const StTofrGeometry::sectorPref = "BSEC";
char* const StTofrGeometry::trayPref   = "BTRA";
char* const StTofrGeometry::senPref    = "BRMD";

//_____________________________________________________________________________
StTofrNode::StTofrNode(TVolumeView *element, TVolumeView *top)
  : fView(element), pView(element->GetPosition()), mMasterNode(top), mTransFlag(kFALSE)
{
   UpdateMatrix();
   BuildMembers();
}

//_____________________________________________________________________________
StTofrNode::~StTofrNode()
{ 
  fView = 0;
  pView = 0;
  mMasterNode = 0;
  mTransFlag = kFALSE;
}

//_____________________________________________________________________________
void  StTofrNode::UpdateMatrix()
{
   //
   //Update the Translation and RotateMatrix between local and master
   //  and store them as members: mTransMRS, mRotMRS
   //while TNode stores them locally to share among all TNode objects,
   //  thus they may be changed afterward !
   //

   mTransFlag = kFALSE;
   CalcMatrix(this, mTransMRS, mRotMRS);
   mTransFlag = kTRUE;

}

//_____________________________________________________________________________
void StTofrNode::CalcMatrix(StTofrNode* son,
              Double_t* trans, Double_t* rot, StTofrNode* mother)
{
   //
   //Translation vector and Rotation matrix from TNode "mother" to "son"
   //
   // 1. Transformation: mother(x,y,z) ==> son(x',y',z')
   //
   //    [x']    [ m[0]  m[1]  m[2] ]   [ [x]   [dx] ]
   //    [y']  = [ m[3]  m[4]  m[5] ] . [ [y] - [dy] ]
   //    [z']    [ m[6]  m[7]  m[8] ]   [ [z]   [dz] ]
   //
   //       Transpose([x',y',z'])       = M . Transpose([x-dx,y-dy,z-dz])
   //  and
   //       Transpose([x-dx,y-dy,z-dz]) = inverse(M) . Transpose([x',y',z'])
   //
   //     where transpose(M) = inverse(M)
   //
   // 2. In the representation of vector:
   //
   // Vectors: V' = x' I' + y' J' + z' K'
   //          V  = x  I  + y  J  + z  K   =  V' + dV
   //         dV  = dx I  + dy J  + dz K
   //
   //    V' . I' = x' = (V-dV) . I'
   //            = (x-dx) (I.I') + (y-dy) (J.I') + (z-dz) (K.I')
   //            = (x-dx) m[0]   + (y-dy) m[1]   + (z-dz) m[2]
   //
   //   so, the projections of unit vector I' on I, J, K
   //       yield m[0], m[1], m[2]
   //
   //********************************************************************


   Double_t xl[3], xm[3];

   //  son->TNode::UpdateMatrix();
   //   son->GetpView()->UpdateMatrix();
   //   LOG_INFO << "StTofrNode::CalcMatrix" << endm;

   xl[0] = 0;  xl[1] = 0;  xl[2] = 0;
   ConvertPos(son,xl, mother,xm);
   trans[0] = xm[0];
   trans[1] = xm[1];
   trans[2] = xm[2];

   xl[0] = 1;  xl[1] = 0;  xl[2] = 0;
   ConvertPos(son,xl, mother,xm);
   rot[0] = xm[0]-trans[0];
   rot[1] = xm[1]-trans[1];
   rot[2] = xm[2]-trans[2];

   xl[0] = 0;  xl[1] = 1;  xl[2] = 0;
   ConvertPos(son,xl, mother,xm);
   rot[3] = xm[0]-trans[0];
   rot[4] = xm[1]-trans[1];
   rot[5] = xm[2]-trans[2];

   xl[0] = 0;  xl[1] = 0;  xl[2] = 1;
   ConvertPos(son,xl, mother,xm);
   rot[6] = xm[0]-trans[0];
   rot[7] = xm[1]-trans[1];
   rot[8] = xm[2]-trans[2];

   return;
}

//_____________________________________________________________________________
void StTofrNode::ConvertPos(
                     StTofrNode* from, const Double_t* pos_from,
            StTofrNode* to,         Double_t* pos_to)
{
   if (to==0) from->Local2Master(pos_from,pos_to);
   else {
      Double_t xg[3];
      from->Local2Master(pos_from,xg);
      to->Master2Local(xg,pos_to);
   }

   return;
}

//_____________________________________________________________________________
void  StTofrNode::BuildMembers()
{
   //
   //Build date members: mCenter{Rxy,Eta,Phi}, m{Eta,Phi}{Min,Max}
   //

   Double_t xl[3];
   Double_t xg[3];
   TBRIK *brik = dynamic_cast<TBRIK*>(GetShape());
   //   LOG_INFO << " Get shape ready" << endm;
   if(!brik) { LOG_INFO << " no brik " << endm; return; }
   //   Double_t dx = brik->GetDx();
   Double_t dy = brik->GetDy();
   Double_t dz = brik->GetDz();

   //   LOG_INFO << " size = " <<dx << " " << dy << " " << dz << endm;
   /*
   //center point
   Double_t xc[3];
   xc[0] = 0;  xc[1] = 0;  xc[2] = 0;
   Local2Master(xc, xg);
   StThreeVectorD center(xg[0], xg[1], xg[2]);
   mCenterRxy = center.perp();
   mCenterEta = center.pseudoRapidity();
   mCenterPhi = center.phi();
   */

   // -dz
   xl[0] = 0;  xl[1] = 0;  xl[2] = -dz;
   Local2Master(xl, xg);
   mEtaMin = StThreeVectorD(xg[0],xg[1],xg[2]).pseudoRapidity();

   // +dz
   xl[0] = 0;  xl[1] = 0;  xl[2] = dz;
   Local2Master(xl, xg);
   mEtaMax = StThreeVectorD(xg[0],xg[1],xg[2]).pseudoRapidity();

   // -dy
   xl[0] = 0;  xl[1] = -dy;  xl[2] = 0;
   Local2Master(xl, xg);
   mPhiMin = StThreeVectorD(xg[0],xg[1],xg[2]).phi();

   // +dy
   xl[0] = 0;  xl[1] =  dy;  xl[2] = 0;
   Local2Master(xl, xg);
   mPhiMax = StThreeVectorD(xg[0],xg[1],xg[2]).phi();

   if (mEtaMin>mEtaMax) {
      Double_t tmp = mEtaMin;
      mEtaMin = mEtaMax;
      mEtaMax = tmp;
   }

   if (mPhiMin>mPhiMax) {
      Double_t tmp = mPhiMin;
      mPhiMin = mPhiMax;
      mPhiMax = tmp;
   }

   return;
}

//_____________________________________________________________________________
void StTofrNode::Local2Master(const Double_t* local, Double_t* master)
{
   //
   //Transform local coordinate into global coordinate
   //
   //  pView->UpdateMatrix();
   if (!mTransFlag) {
     if (!mMasterNode) { LOG_INFO << " no Master! " << endm; return;}
     TVolumeView *son = GetfView();
     TVolumeView *mrs = GetTopNode();
     
     TVolumePosition *pos = 0;
     pos = son->Local2Master(son, mrs);
     pos->Local2Master(local, master);
     delete pos;
     return;
   }

   //mTransFlag==kTRUE, i.e. StTofrGeomNode::UpdateMatrix() invoked already
   Double_t x, y, z;
   x = local[0];
   y = local[1];
   z = local[2];

   master[0] = mTransMRS[0] + mRotMRS[0]*x + mRotMRS[3]*y + mRotMRS[6]*z;
   master[1] = mTransMRS[1] + mRotMRS[1]*x + mRotMRS[4]*y + mRotMRS[7]*z;
   master[2] = mTransMRS[2] + mRotMRS[2]*x + mRotMRS[5]*y + mRotMRS[8]*z;

}

//_____________________________________________________________________________
void StTofrNode::Master2Local(const Double_t* master, Double_t* local)
{
   //
   //Transform global coordinate into local coordinate
   //
   //  pView->UpdateMatrix();
   //   pView->Master2Local(master, local);
   if (!mTransFlag) {
     LOG_INFO << " and No TVolumePosition::Master2Local is wrong, so do nothing" << endm;
      return;
   }

   //mTransFlag==kTRUE, i.e. StTofrGeomNode::UpdateMatrix() invoked already
   Double_t x, y, z;
   x = master[0] - mTransMRS[0];
   y = master[1] - mTransMRS[1];
   z = master[2] - mTransMRS[2];

   local[0] = mRotMRS[0]*x + mRotMRS[1]*y + mRotMRS[2]*z;
   local[1] = mRotMRS[3]*x + mRotMRS[4]*y + mRotMRS[5]*z;
   local[2] = mRotMRS[6]*x + mRotMRS[7]*y + mRotMRS[8]*z;
}

//_____________________________________________________________________________
StThreeVectorD StTofrNode::YZPlaneNormal()
{
   //
   //Calculate the vector unit of normal to YZ-plane
   // i.e. the global representation of local unit vector (1,0,0)
   //

   Double_t ux[3], nx[3];

   ux[0] = 1;  ux[1] = 0;  ux[2] = 0;
   Local2Master(ux,nx);

   nx[0] -= mTransMRS[0];
   nx[1] -= mTransMRS[1];
   nx[2] -= mTransMRS[2];

   return StThreeVectorD(nx[0],nx[1],nx[2]);
}

//_____________________________________________________________________________
StThreeVectorD  StTofrNode::GetCenterPosition()
const
{
   //
   //Return the global representation of this node(TBRIk-shape) center
   //

   Double_t xg[3];
   xg[0] = mTransMRS[0];
   xg[1] = mTransMRS[1];
   xg[2] = mTransMRS[2];

   return StThreeVectorD(xg[0],xg[1],xg[2]);
}

//_____________________________________________________________________________
Bool_t StTofrNode::IsLocalPointIn(const Double_t x, const Double_t y, 
                                      const Double_t z)
{
   TBRIK *brik = dynamic_cast<TBRIK*> (GetShape());
   Double_t dx = brik->GetDx();
   Double_t dy = brik->GetDy();
   Double_t dz = brik->GetDz();
   Bool_t ret = -dx<x && x<dx && -dy<y && y<dy && -dz<z && z<dz;

   return ret;
}

//_____________________________________________________________________________
Bool_t  StTofrNode::IsGlobalPointIn(const StThreeVectorD &global)
{
   Double_t xl[3], xg[3];
   xg[0] = global.x();
   xg[1] = global.y();
   xg[2] = global.z();
   Master2Local(xg, xl);
   Bool_t ret = IsLocalPointIn(xl[0], xl[1], xl[2]);

   return ret;
}

//_____________________________________________________________________________
Bool_t StTofrNode::HelixCross(const StHelixD &helix, Double_t &pathLen,
                                  StThreeVectorD &cross)
{
   //
   // check if helix go through this node(TBRIK)
   //  and return the path length of helix before crossing this node
   //
   //   static const Float_t MaxPathLength = 1000.; //Maximum path length
   Float_t MaxPathLength = 1000.;

   Bool_t ret = kFALSE;
   pathLen = 0;

   //
   // Get the normal to the YZ-plane
   //
   StThreeVectorD planeNormal = YZPlaneNormal();

   //
   // Get the center position
   //
   StThreeVectorD centerPos = GetCenterPosition();

   //
   // Find the intersection point between the helix and the cell plane
   //
   pathLen = helix.pathLength(centerPos,planeNormal);
   if ( pathLen>0 && pathLen<MaxPathLength ) {
      cross = helix.at(pathLen);
      //
      // Check if the intersected point is really in the cell
      //
      ret = IsGlobalPointIn(cross);
   }
   return ret;
}

//_____________________________________________________________________________
void StTofrNode::Print(Option_t *opt) const
{
   TBRIK *brik = dynamic_cast<TBRIK*> (GetShape());
   LOG_INFO << "Name=" << GetName() << "\t TBRIK-dimension=" << brik->GetDx()
        << " : " << brik->GetDy() << " : " << brik->GetDz()
        // << "\n Center Rxy:Eta:Phi=" << mCenterRxy << " : "
        // << mCenterEta << " : " << mCenterPhi
        << "\n EtaRange=" << mEtaMin << " : " << mEtaMax
        << "\t PhiRange=" << mPhiMin << " : " << mPhiMax
        << endm;
   LOG_INFO <<"trans[0-2]=" << mTransMRS[0] <<" " <<mTransMRS[1]
                                         <<" " <<mTransMRS[2]
        <<"\nmRotMRS[0-2]=" <<mRotMRS[0] <<" " <<mRotMRS[1] <<" " <<mRotMRS[2]
        <<"\nmRotMRS[3-5]=" <<mRotMRS[3] <<" " <<mRotMRS[4] <<" " <<mRotMRS[5]
        <<"\nmRotMRS[6-8]=" <<mRotMRS[6] <<" " <<mRotMRS[7] <<" " <<mRotMRS[8]
        <<endm;
}

#if 0
// - - - - - -
//
//  StTofrGeomNode
//  ==============
/* 
//_____________________________________________________________________________
StTofrGeomNode::StTofrGeomNode(const char* name, const char* title,
      const TBRIK* brik, const Double_t x, const Double_t y, const Double_t z,
      const Double_t theta)
  : TNode(name, title, brik, x, y, z), mTransFlag(kFALSE)
{
   fMatrix = new TRotMatrix("rot","rot",90.+theta,0.,90.,90.,theta,0.);
   UpdateMatrix();
   BuildMembers();
}
*/


Bool_t StTofrGeomNode::mDebug = kFALSE;
//_____________________________________________________________________________
/*
StTofrGeomNode::StTofrGeomNode(const char* name, const char* title,
      const TBRIK* brik, const Double_t x, const Double_t y, const Double_t z,
      const TRotMatrix* matrix)*/

StTofrGeomNode::StTofrGeomNode(const char* name, const char* title,
      TBRIK* brik, const Double_t x, const Double_t y, const Double_t z,
      TRotMatrix* matrix)
  : TNode(name, title, brik, x, y, z, matrix), mTransFlag(kFALSE)
{
   UpdateMatrix();
   BuildMembers();
}

//_____________________________________________________________________________
StTofrGeomNode::~StTofrGeomNode()
{
}

//_____________________________________________________________________________
void  StTofrGeomNode::UpdateMatrix()
{
   //
   //Update the Translation and RotateMatrix between local and master
   //  and store them as members: mTransMRS, mRotMRS
   //while TNode stores them locally to share among all TNode objects,
   //  thus they may be changed afterward !
   //

   mTransFlag = kFALSE;
   CalcMatrix(this, mTransMRS, mRotMRS);
   mTransFlag = kTRUE;
}

//_____________________________________________________________________________
void StTofrGeomNode::CalcMatrix(TNode* son,
              Double_t* trans, Double_t* rot, StTofrGeomNode* mother)
{
   //
   //Translation vector and Rotation matrix from TNode "mother" to "son"
   //
   // 1. Transformation: mother(x,y,z) ==> son(x',y',z')
   //
   //    [x']    [ m[0]  m[1]  m[2] ]   [ [x]   [dx] ]
   //    [y']  = [ m[3]  m[4]  m[5] ] . [ [y] - [dy] ]
   //    [z']    [ m[6]  m[7]  m[8] ]   [ [z]   [dz] ]
   //
   //       Transpose([x',y',z'])       = M . Transpose([x-dx,y-dy,z-dz])
   //  and
   //       Transpose([x-dx,y-dy,z-dz]) = inverse(M) . Transpose([x',y',z'])
   //
   //     where transpose(M) = inverse(M)
   //
   // 2. In the representation of vector:
   //
   // Vectors: V' = x' I' + y' J' + z' K'
   //          V  = x  I  + y  J  + z  K   =  V' + dV
   //         dV  = dx I  + dy J  + dz K
   //
   //    V' . I' = x' = (V-dV) . I'
   //            = (x-dx) (I.I') + (y-dy) (J.I') + (z-dz) (K.I')
   //            = (x-dx) m[0]   + (y-dy) m[1]   + (z-dz) m[2]
   //
   //   so, the projections of unit vector I' on I, J, K
   //       yield m[0], m[1], m[2]
   //
   //********************************************************************

   Double_t xl[3], xm[3];

   son->TNode::UpdateMatrix();

   xl[0] = 0;  xl[1] = 0;  xl[2] = 0;
   ConvertPos(son,xl, mother,xm);
   trans[0] = xm[0];
   trans[1] = xm[1];
   trans[2] = xm[2];

   xl[0] = 1;  xl[1] = 0;  xl[2] = 0;
   ConvertPos(son,xl, mother,xm);
   rot[0] = xm[0]-trans[0];
   rot[1] = xm[1]-trans[1];
   rot[2] = xm[2]-trans[2];

   xl[0] = 0;  xl[1] = 1;  xl[2] = 0;
   ConvertPos(son,xl, mother,xm);
   rot[3] = xm[0]-trans[0];
   rot[4] = xm[1]-trans[1];
   rot[5] = xm[2]-trans[2];

   xl[0] = 0;  xl[1] = 0;  xl[2] = 1;
   ConvertPos(son,xl, mother,xm);
   rot[6] = xm[0]-trans[0];
   rot[7] = xm[1]-trans[1];
   rot[8] = xm[2]-trans[2];

   return;
}

//_____________________________________________________________________________
void StTofrGeomNode::ConvertPos(
                     TNode* from, const Double_t* pos_from,
            StTofrGeomNode* to,         Double_t* pos_to)
{
   //
   //Convert the coordinate "Double_t* pos_from" in TNode* from
   //   into the coordinate "Doulbe_t *pos_to"   in TNode* to
   //

   if (to==0) from->Local2Master(pos_from,pos_to);
   else {
      Double_t xg[3];
      from->Local2Master(pos_from,xg);
      to->Master2Local(xg,pos_to);
   }

   return;
}

//_____________________________________________________________________________
void  StTofrGeomNode::BuildMembers()
{
   //
   //Build date members: mCenter{Rxy,Eta,Phi}, m{Eta,Phi}{Min,Max}
   //

   Double_t xl[3];
   Double_t xg[3];
   TBRIK *brik = dynamic_cast<TBRIK*> (GetShape());
   //Double_t dx = brik->GetDx();
   Double_t dy = brik->GetDy();
   Double_t dz = brik->GetDz();

   /*
   //center point
   Double_t xc[3];
   xc[0] = 0;  xc[1] = 0;  xc[2] = 0;
   Local2Master(xc, xg);
   StThreeVectorD center(xg[0], xg[1], xg[2]);
   mCenterRxy = center.perp();
   mCenterEta = center.pseudoRapidity();
   mCenterPhi = center.phi();
   */

   // -dz
   xl[0] = 0;  xl[1] = 0;  xl[2] = -dz;
   Local2Master(xl, xg);
   mEtaMin = StThreeVectorD(xg[0],xg[1],xg[2]).pseudoRapidity();

   // +dz
   xl[0] = 0;  xl[1] = 0;  xl[2] = dz;
   Local2Master(xl, xg);
   mEtaMax = StThreeVectorD(xg[0],xg[1],xg[2]).pseudoRapidity();

   // -dy
   xl[0] = 0;  xl[1] = -dy;  xl[2] = 0;
   Local2Master(xl, xg);
   mPhiMin = StThreeVectorD(xg[0],xg[1],xg[2]).phi();

   // +dy
   xl[0] = 0;  xl[1] =  dy;  xl[2] = 0;
   Local2Master(xl, xg);
   mPhiMax = StThreeVectorD(xg[0],xg[1],xg[2]).phi();

   if (mEtaMin>mEtaMax) {
      Double_t tmp = mEtaMin;
      mEtaMin = mEtaMax;
      mEtaMax = tmp;
   }

   if (mPhiMin>mPhiMax) {
      Double_t tmp = mPhiMin;
      mPhiMin = mPhiMax;
      mPhiMax = tmp;
   }

}

//_____________________________________________________________________________
void StTofrGeomNode::Local2Master(const Double_t* local, Double_t* master)
{
   //
   //Transform local coordinate into global coordinate
   //

   if (!mTransFlag) {
      // TNode::UpdateMatrix();
      TNode::Local2Master(local,master);
      return;
   }

   //mTransFlag==kTRUE, i.e. StTofrGeomNode::UpdateMatrix() invoked already
   Double_t x, y, z;
   x = local[0];
   y = local[1];
   z = local[2];

   master[0] = mTransMRS[0] + mRotMRS[0]*x + mRotMRS[3]*y + mRotMRS[6]*z;
   master[1] = mTransMRS[1] + mRotMRS[1]*x + mRotMRS[4]*y + mRotMRS[7]*z;
   master[2] = mTransMRS[2] + mRotMRS[2]*x + mRotMRS[5]*y + mRotMRS[8]*z;

}

//_____________________________________________________________________________
void StTofrGeomNode::Master2Local(const Double_t* master, Double_t* local)
{
   //
   //Transform global coordinate into local coordinate
   // Please notice that TNode::Master2Local is INCORRECT !!!
   //

   if (!mTransFlag) {
      LOG_INFO << "Warning in StTofrGeomNode::Master2Local\n"
           << " StTofrGeomNode::UpdateMatrix not yet invoked\n"
           << " and TNode::Master2Local is wrong, so do nothing" << endm;
      return;
   }

   //mTransFlag==kTRUE, i.e. StTofrGeomNode::UpdateMatrix() invoked already
   Double_t x, y, z;
   x = master[0] - mTransMRS[0];
   y = master[1] - mTransMRS[1];
   z = master[2] - mTransMRS[2];

   local[0] = mRotMRS[0]*x + mRotMRS[1]*y + mRotMRS[2]*z;
   local[1] = mRotMRS[3]*x + mRotMRS[4]*y + mRotMRS[5]*z;
   local[2] = mRotMRS[6]*x + mRotMRS[7]*y + mRotMRS[8]*z;

}

//_____________________________________________________________________________
StThreeVectorD StTofrGeomNode::YZPlaneNormal()
{
   //
   //Calculate the vector unit of normal to YZ-plane
   // i.e. the global representation of local unit vector (1,0,0)
   //

   Double_t ux[3], nx[3];

   ux[0] = 1;  ux[1] = 0;  ux[2] = 0;
   Local2Master(ux,nx);

   nx[0] -= mTransMRS[0];
   nx[1] -= mTransMRS[1];
   nx[2] -= mTransMRS[2];

   return StThreeVectorD(nx[0],nx[1],nx[2]);
}

//_____________________________________________________________________________
StThreeVectorD  StTofrGeomNode::GetCenterPosition()
const
{
   //
   //Return the global representation of this node(TBRIk-shape) center
   //

   Double_t xg[3];
   xg[0] = mTransMRS[0];
   xg[1] = mTransMRS[1];
   xg[2] = mTransMRS[2];

   return StThreeVectorD(xg[0],xg[1],xg[2]);
}

//_____________________________________________________________________________
Bool_t StTofrGeomNode::IsLocalPointIn(const Double_t x, const Double_t y, 
                                      const Double_t z)
const
{
   TBRIK *brik = dynamic_cast<TBRIK*> (GetShape());
   Double_t dx = brik->GetDx();
   Double_t dy = brik->GetDy();
   Double_t dz = brik->GetDz();
   Bool_t ret = -dx<x && x<dx && -dy<y && y<dy && -dz<z && z<dz;

   return ret;
}

//_____________________________________________________________________________
Bool_t  StTofrGeomNode::IsGlobalPointIn(const StThreeVectorD &global)
{
   Double_t xl[3], xg[3];
   xg[0] = global.x();
   xg[1] = global.y();
   xg[2] = global.z();
   Master2Local(xg, xl);
   Bool_t ret = IsLocalPointIn(xl[0], xl[1], xl[2]);

   return ret;
}

//_____________________________________________________________________________
Bool_t StTofrGeomNode::HelixCross(const StHelixD &helix, Double_t &pathLen,
                                  StThreeVectorD &cross)
{
   //
   // check if helix go through this node(TBRIK)
   //  and return the path length of helix before crossing this node
   //

   static const Float_t MaxPathLength = 1000.; //Maximum path length

   Bool_t ret = kFALSE;
   pathLen = 0;

   //
   // Get the normal to the YZ-plane
   //
   StThreeVectorD planeNormal = YZPlaneNormal();

   //
   // Get the center position
   //
   StThreeVectorD centerPos = GetCenterPosition();

   //
   // Find the intersection point between the helix and the cell plane
   //
   pathLen = helix.pathLength(centerPos,planeNormal);
   if ( pathLen>0 && pathLen<MaxPathLength ) {
      cross = helix.at(pathLen);
      //
      // Check if the intersected point is really in the cell
      //
      ret = IsGlobalPointIn(cross);
   }

   return ret;
}

//_____________________________________________________________________________
void StTofrGeomNode::Print(Option_t *opt) const
const
{
   TBRIK *brik = dynamic_cast<TBRIK*> (GetShape());
   LOG_INFO << "Name=" << GetName() << "\t TBRIK-dimension=" << brik->GetDx()
        << " : " << brik->GetDy() << " : " << brik->GetDz()
        // << "\n Center Rxy:Eta:Phi=" << mCenterRxy << " : "
        // << mCenterEta << " : " << mCenterPhi
        << "\n EtaRange=" << mEtaMin << " : " << mEtaMax
        << "\t PhiRange=" << mPhiMin << " : " << mPhiMax
        << endm;
   LOG_INFO <<"trans[0-2]=" << mTransMRS[0] <<" " <<mTransMRS[1]
                                         <<" " <<mTransMRS[2]
        <<"\nmRotMRS[0-2]=" <<mRotMRS[0] <<" " <<mRotMRS[1] <<" " <<mRotMRS[2]
        <<"\nmRotMRS[3-5]=" <<mRotMRS[3] <<" " <<mRotMRS[4] <<" " <<mRotMRS[5]
        <<"\nmRotMRS[6-8]=" <<mRotMRS[6] <<" " <<mRotMRS[7] <<" " <<mRotMRS[8]
        <<endm;
}
#endif


//////////////////////////////////////////////////////////////////////////////
//
// StTofrGeomTray
// ==============
//
//////////////////////////////////////////////////////////////////////////////


Bool_t StTofrGeomTray::mDebug = kFALSE;

//_____________________________________________________________________________
StTofrGeomTray::StTofrGeomTray(const Int_t ibtoh, TVolumeView *sector, TVolumeView *top) 
  : StTofrNode((TVolumeView *)sector->First(), top)
{
  mSectorsInBTOH = top->GetListSize()/2;
  mBTOHIndex = ibtoh + 1;
  mTrayIndex = ibtoh * mSectorsInBTOH + sector->GetPosition()->GetId();
}

//_____________________________________________________________________________
StTofrGeomTray::~StTofrGeomTray()
{
  mBTOHIndex = 0;
  mTrayIndex = 0;
}
//_____________________________________________________________________________
/*StTofrGeomTray::StTofrGeomTray(const char* name, const char* title,
      TBRIK *brik, const Double_t x, const Double_t y, const Double_t z,
      TRotMatrix *matrix, const Int_t itray)
  : StTofrGeomNode(name,title,brik,x,y,z,matrix)
{
   mTrayIndex = itray;
}
*/

//_____________________________________________________________________________
/*void StTofrGeomTray::PrepareCopyNode(TNode* node, StTofrGeomNode* top,
                  TShape* &shape, Double_t* pos, TRotMatrix* &newrot)
{
   //
   //Re-create/Clone components of a node to prepare for CopyNode or AddNode
   //

  //   static const char* where = "StTofrGeomTray::PrepareCopyNode";
  //  gDebugger.Print(mDebug,"==>Come to %s(...)\n",where);

   shape = 0;
   newrot   = 0;
   if (node==0) return;

   THashList* shapeList    = gGeometry->GetListOfShapes();
   shape = dynamic_cast<TShape*>
           ( shapeList->FindObject(node->GetShape()->GetName()) );

   if (shape == 0) {
      //Clone the shape of the node
      shape = dynamic_cast<TShape*> (node->GetShape()->Clone());
      shapeList->Add(shape);
      THashList* materialList = gGeometry->GetListOfMaterials();
      materialList->Add(shape->GetMaterial());
   }

   const char* name;
   const char* title;
   TRotMatrix *oldrot= node->GetMatrix();
   name  = oldrot->GetName();
   title = oldrot->GetTitle();

   Double_t trans[3];
   Double_t matrix[9];
   CalcMatrix(node,trans,matrix,top);
   newrot = new TRotMatrix(name,title,matrix);

   pos[0] = trans[0];
   pos[1] = trans[1];
   pos[2] = trans[2];

   //   gDebugger.Print(mDebug," <== Leaving %s(...)\n",where);
   return;
}

//_____________________________________________________________________________
StTofrGeomTray* StTofrGeomTray::CopyNode(TNode* node, const Int_t itray)
{
   //
   //Clone-Convert "TNode* node" into "StTofrGeomTray*" under top TNode(MRS)
   //

  //   static const char* where = "StTofrGeomTray::CopyNode";
  //   gDebugger.Print(mDebug,"==>Come to %s(.,itray=%d)\n",where, itray);

   if (node==0) return NULL;

   TShape *shape;
   Double_t trans[3];
   TRotMatrix *newrot;

   PrepareCopyNode(node,0,shape,trans,newrot);

   const char* name;
   const char* title;
   TBRIK* brik = dynamic_cast<TBRIK*> (shape);
   name  = node->GetName();
   title = node->GetTitle();

   //create StTofrGeomTray directly under gGeometry
   // gGeometry->SetCurrentNode(0);

   StTofrGeomTray* tray = new StTofrGeomTray(name,title,brik,
                   trans[0],trans[1],trans[2],newrot, itray);

   //   gDebugger.Print(mDebug," <== Leaving %s(.,itray=%d)\n",where, itray);
   return tray;
}

//_____________________________________________________________________________
StTofrGeomSensor*  StTofrGeomTray::AddNode(TNode* node, const Int_t imodule)
{
   //
   //Clone-onvert "TNode* node" into "StTofrGeomSensor*" and put under this
   // the translation/rotation will be built between "node" and this
   //

  //   static const char* where = "StTofrGeomTray::AddNode";
  //   gDebugger.Print(mDebug,"==>Come to %s(.,imodule=%d)\n",
  //                   where, imodule);

   if (node==0) return NULL;

   const char* name;
   const char* title;
   TShape *shape;
   Double_t trans[3];
   TRotMatrix *newrot;

   PrepareCopyNode(node,this,shape,trans,newrot);
   TBRIK* brik = dynamic_cast<TBRIK*> (shape);
   name  = node->GetName();
   title = node->GetTitle();

   this->cd();
   StTofrGeomSensor* sensor = new StTofrGeomSensor(name,title,brik,
                     trans[0],trans[1],trans[2],newrot, imodule);

//   gDebugger.Print(mDebug," <== Leaving %s(.,,imodule=%d)\n",where, imodule);
   return sensor;
}12
*/
//_____________________________________________________________________________
StTofrGeomSensor* StTofrGeomTray::GetSensor(const Int_t imodule) const
{

   StTofrGeomSensor* sensor = 0;

   TVolumeView *volume = GetfView();
   //   TList *list = fView->Nodes();
   if ( !(volume->GetListSize()) ) {
     LOG_INFO << " No Modules in this tray " << endm;
     return sensor;
   }
   //   list->Delete();

   TDataSetIter nextSensor(volume);
   TVolumeView *sensorVolume = 0;
   TVolumeView *top = GetTopNode();

   while ( (sensorVolume = (TVolumeView *)nextSensor()) )
     {
       if ( sensorVolume && (int)(sensorVolume->GetPosition()->GetId()) == imodule ) {
	 sensor = new StTofrGeomSensor(sensorVolume, top);
       }
     }

   return sensor;
}

//_____________________________________________________________________________
void StTofrGeomTray::Print(const Option_t *opt) const
{
   LOG_INFO << "StTofrGeomTray, tray#=" << mTrayIndex << endm;
   StTofrNode::Print(opt);
}


//////////////////////////////////////////////////////////////////////////////
//
// StTofrGeomSensor
// ================
//
//////////////////////////////////////////////////////////////////////////////


Bool_t StTofrGeomSensor::mDebug = kFALSE;

//_____________________________________________________________________________
StTofrGeomSensor::StTofrGeomSensor(TVolumeView *element, TVolumeView *top) 
  : StTofrNode(element, top)
{
   mModuleIndex = element->GetPosition()->GetId();
   CreateGeomCells();
}

//_____________________________________________________________________________
StTofrGeomSensor::~StTofrGeomSensor()
{
  mModuleIndex = 0;
}
//_____________________________________________________________________________
/*
StTofrGeomSensor::StTofrGeomSensor(const char* name, const char* title,
      const TBRIK *brik, const Double_t x, const Double_t y, const Double_t z,
      const TRotMatrix* matrix, const Int_t imodule)
StTofrGeomSensor::StTofrGeomSensor(const char* name, const char* title,
      TBRIK *brik, const Double_t x, const Double_t y, const Double_t z,
      TRotMatrix* matrix, const Int_t imodule)
  : StTofrGeomNode(name,title,brik,x,y,z,matrix)
{
   mModuleIndex = imodule;
   CreateGeomCells();
}
*/
//_____________________________________________________________________________
void StTofrGeomSensor::CreateGeomCells()
{
   //
   //Divide this sensor to creat cells
   //
   /*
   TBRIK *thisBrik = dynamic_cast<TBRIK*> (GetShape());
   Double_t sensor_dy  = thisBrik->GetDy();
   */

   //
   // change the size according to the real cells
   //    mSensorDy -- defined by myself
   //
   Double_t sensor_dy = mSensorDy;
   Double_t cell_width = 2*sensor_dy/mCells;

   for (int i=0; i<=mCells; i++) mCellY[i] = cell_width*i - sensor_dy;

}

//_____________________________________________________________________________
Double_t StTofrGeomSensor::GetCellYMin(const Int_t icell)
const
{
   Double_t ret = 0;
   if (icell<=0 || icell>mCells) {
      LOG_INFO << "cell#=" << icell <<" is out range=[0," << mCells << "]"
           << endm;
   } else ret = mCellY[icell-1];

   return ret;
}

//_____________________________________________________________________________
Double_t StTofrGeomSensor::GetCellYMax(const Int_t icell)
const
{
   Double_t ret = 0;
   if (icell<=0 || icell>mCells) {
      LOG_INFO << "cell#=" << icell <<" is out range=[0," << mCells << "]"
           << endm;
   } else ret = mCellY[icell];

   return ret;
}

//_____________________________________________________________________________
StThreeVectorD StTofrGeomSensor::GetCellPosition(const Int_t icell)
{
   //
   // Get the center position of cell in this sensor
   //

   static const char* where = "StTofrGeomSensor::GetCellPosition";
   /*
   TBRIK *thisBrik = dynamic_cast<TBRIK*> (GetShape());
   Double_t sensor_dy  = thisBrik->GetDy();
   */

   // change the size according to the real cells
   Double_t sensor_dy = mSensorDy;
   Double_t cell_dy    = 2*sensor_dy/mCells;

   Double_t xl[3], xg[3];
   if (icell>=1 && icell<=mCells) {
      xl[0] = 0;
      xl[1] = (icell*2-1)*cell_dy - sensor_dy;
      xl[2] = 0;
      Local2Master(xl,xg);
   } else {  //invalid cell, return (0.,0.,0.)
      LOG_INFO << "Warning in " << where <<" Invalid cell# =" << icell << endm;
      xg[0] = 0.;
      xg[1] = 0.;
      xg[2] = 0.;
   }

   return StThreeVectorD(xg[0],xg[1],xg[2]);  
}


//_____________________________________________________________________________
Int_t StTofrGeomSensor::FindCellIndex(const Double_t* local)
{
   //
   //Look up the cell the local point in
   //

   Int_t icell=-1;
   if ( IsLocalPointIn(local[0],local[1],local[2]) ) {
    
      for (Int_t i=0; i<mCells; i++) {
         if (mCellY[i]<= local[1] && local[1]<=mCellY[i+1]) {
            icell = i+1;
            break;
	    
	 }
      }
   }

   return icell;
}

//_____________________________________________________________________________
void StTofrGeomSensor::Print(const Option_t *opt) const
{
   LOG_INFO << "StTofrGeomSensor, module#=" << mModuleIndex << endm;
   StTofrNode::Print(opt);

   LOG_INFO << " Cells=" << mCells << "\t Y range for cells=\n";
   for (Int_t i=0; i<=mCells; i++) LOG_INFO << " : " << mCellY[i];
   LOG_INFO << endm;
}


//////////////////////////////////////////////////////////////////////////////
//
// StTofrGeometry
// ==============
//
//////////////////////////////////////////////////////////////////////////////

StTofrGeometry *gTofrGeometry = 0;
static const Int_t CELLSINMODULE = 6;


Bool_t StTofrGeometry::mDebug = kFALSE;

//_____________________________________________________________________________
StTofrGeometry::StTofrGeometry(const char* name, const char* title)
  : TNamed(name,title)
{
   mCellsInModule  = StTofrGeomSensor::GetCells();
   mModulesInTray  = 0;
   mTrays          = 0;
   mRootFile       = 0;
   mInitFlag       = kFALSE;
   mTopNode        = 0;
   mStarHall       = 0;

   for(int i=0;i<mNTrays;i++) {
     mTofrTray[i] = 0;
     for(int j=0;j<mNModules;j++) {
       mTofrSensor[i][j] = 0;
     }
   }

   //
   //We only need one instance of StTofrGeometry
   //
   if (gTofrGeometry) {
      LOG_INFO << "Warning !! There is already StTofrGeometry at pointer="
           << (void*)gTofrGeometry << ", so it is deleted"
           << endm;
      delete gTofrGeometry;
   }
   gTofrGeometry = this;
}

//_____________________________________________________________________________
StTofrGeometry::~StTofrGeometry()
{
   LOG_INFO << "Warning !! StTofrGeometry at pointer =" << (void*)gTofrGeometry
        << " is deleted" << endm;
   gTofrGeometry = 0;

   for(int i=0;i<mNTrays;i++) {
     if(mTofrTray[i]) delete mTofrTray[i];
     mTofrTray[i] = 0;
     for(int j=0;j<mNModules;j++) {
       if(mTofrSensor[i][j]) delete mTofrSensor[i][j];
       mTofrSensor[i][j] = 0;
     }
   }
   
}

//_____________________________________________________________________________
//void  StTofrGeometry::Init(const char *file, Option_t *option)
//void StTofrGeometry::Init(TVolume *starHall, const Int_t TofrConf)
void StTofrGeometry::Init(TVolume *starHall)
{
   //
   //Define geometry parameters and establish the geometry
   // current available options: "root", and "xdf"
   //
  
   //   InitFromStar(starHall, TofrConf);
   InitFromStar(starHall);
   mStarHall = starHall;

   //save current gGeometry to resume later
   //
   //TGeometry *currentGeo = gGeometry;

   //static TString optRoot("root");
   //static TString optXdf("xdf");

   //TString opt(option);
   //opt.ToLower();

   //if (opt==optRoot) {
   //   //
   //   //Initialization from ROOT file of geometry (converted from RZ file)
   //   //
   //   InitFromRoot(file);
   //   mRootFile = file;
   //} else if (opt==optXdf) {
   //   //
   //   //Initialize from XDF file, not ready
   //   //
   //   //InitFromXdf(file);
   //   //Xdf2Geometry();
   //   LOG_INFO << "StTofrGeom::Init, sorry! option \"xdf\" not yet ready" << endm;
   //} else {
   //   LOG_INFO << "StTofrGeom::Init, Warning!! not yet implemented option="
   //        << option << endm;
   //}

   ////resume gGeometry back to the value at the beginning
   ////
   //gGeometry = currentGeo;

   ////set the flag of Initialization
   ////
   //mInitFlag = kTRUE;

}
//_____________________________________________________________________________
//void StTofrGeometry::InitFromStar(TVolume *starHall, const Int_t TofrConf=0)
void StTofrGeometry::InitFromStar(TVolume *starHall)
{
  // Initialize TOFr geometry from STAR geometry
  //     TofrConf   --     0     tray_Tofr   (default)
  //                       1     full_Tofr

  //  TVolume *starHall = (TVolume *) GetDataSet("HALL");
  //  mStarHall = starHall;

  //  mTofrConf = TofrConf;
//   static const char* sectorPref="BSEC";
//   static const char* trayPref="BTRA";
//   static const char* senPref ="BRMD";
//   static const char* topName ="HALL1";
  
//   static const char* tofElements[] = { sectorPref, trayPref, senPref, topName };
  
  // Loop over the STAR geometry and mark the volume needed
  TDataSetIter volume(starHall,0);

  TVolume *starDetectorElement = 0;
  while ( (starDetectorElement = ( TVolume *)volume()) )
    {
      //      const char *elementName = starDetectorElement->GetName();
      //      Bool_t found = ! ( strcmp(elementName,tofElements[0]) && strcmp(elementName,tofElements[1]) && strcmp(elementName,tofElements[2]) );
      Bool_t found = ( IsBSEC(starDetectorElement) || IsBTRA(starDetectorElement) || IsBRMD(starDetectorElement) );
      if (found) {

	starDetectorElement->SetVisibility(TVolume::kBothVisible);
	starDetectorElement->Mark();
	if (starDetectorElement->GetLineColor()==1 || starDetectorElement->GetLineColor()==7) 
	  starDetectorElement->SetLineColor(14);
	
      } else {
	
	starDetectorElement->UnMark();
	starDetectorElement->SetVisibility(TVolume::kThisUnvisible);

      }
    }

  starHall->SetVisibility(TVolume::kBothVisible);
  mTopNode = new TVolumeView(*starHall,10); 

  //  mTrays = 120;
  //  mModulesInTray = 33;

  mSectorsInBTOH = mTopNode->GetListSize()/2;    // # of sectors in one half

  // check tray-tofr or full-tofr
  TDataSetIter nextSector(mTopNode);
  TVolumeView *secVolume = 0;
  mTrays = 0;     // non-emtry tray number
  while ( (secVolume = (TVolumeView *)nextSector()) ) {
    TVolumeView *trayVolume = (TVolumeView *)secVolume->First();
    if ( trayVolume->GetListSize() ) {
      mTrays++;
      mModulesInTray = trayVolume->GetListSize();
    }
  }
  mTofrConf = 0;
  if (mTrays==120) mTofrConf = 1;
  //

  /////////////////////////////
  // save the sensors and trays
  /////////////////////////////
  gMessMgr->Info("","OS") << " # of trays = " << mTopNode->GetListSize() << endm;
  TList *list = mTopNode->Nodes();
  Int_t ibtoh =0;
  TVolumeView *sectorVolume = 0;
  mNValidTrays = 0;
  mNValidModules = 0;
  for(Int_t i=0;i<list->GetSize();i++) {
    sectorVolume = dynamic_cast<TVolumeView*> (list->At(i));
    TVolumeView *trayVolume = (TVolumeView *)sectorVolume->First();
    if( !trayVolume->GetListSize() ) continue;
    if ( i>=60 ) ibtoh = 1;
    //    gMessMgr->Info("","OS") << " test sector size = " << trayVolume->GetListSize() << endm;
    mTofrTray[mNValidTrays] = new StTofrGeomTray(ibtoh, sectorVolume, mTopNode);
    TList *list1 = trayVolume->Nodes();
    //    gMessMgr->Info("","OS") << "   # of modules in tray " << mTofrTray[mNValidTrays]->Index() << " = " << trayVolume->GetListSize() << endm;
    if (!list1 ) continue;
    TVolumeView *sensorVolume = 0;
    if(list1->GetSize()>mNValidModules) mNValidModules=list1->GetSize(); 
    for(Int_t j=0;j<list1->GetSize();j++) {
      sensorVolume = dynamic_cast<TVolumeView*> (list1->At(j));
      mTofrSensor[mNValidTrays][j] = new StTofrGeomSensor(sensorVolume, mTopNode);
    }
    mNValidTrays++;
  }
  gMessMgr->Info("","OS") << "\n-------------------------------------------\n"
			  << " Summary of initialization: "
			  << "    NValidTrays = " << mNValidTrays << "   NValidModules = " << mNValidModules << endm;


//   if ( !TofrConf ) {    // tray Tofr --- delete garbages
//     // We should not delete the volumes , just keep them
//     /*
//     TList garbage;
//     TDataSetIter nextSector(mTopNode);
//     TVolumeView *sector = 0;
//     Int_t ibtoh = 0, i = 0;
//     while ( (sector = (TVolumeView *)nextSector()) ) {
//       i++;
//       if ( i>mSectorsInBTOH ) ibtoh = 1;
//       Int_t itray = mSectorsInBTOH * ibtoh + sector->GetPosition()->GetId();
//       if ( itray!=83 ) garbage.Add(sector);
//     }
//     garbage.Delete();
//     */
//     mTrays = 1;
//     mModulesInTray = 33;
//   }

  return;

}

//_____________________________________________________________________________
Bool_t StTofrGeometry::ContainOthers(TVolume *element)
{
  TVolumeView *elementView = new TVolumeView(*element,1);
  TList *list = elementView->GetListOfShapes();
  if ( list ) {
    LOG_INFO << " yes list in " << element->GetName() << endm;
    return kTRUE;
  } else {
    LOG_INFO << " no list in " << element->GetName() << endm;
    return kFALSE;
  }
}

/*
//_____________________________________________________________________________
Bool_t  StTofrGeometry::InitFromRoot(const char* geofile)
{
   //
   //Read geometry root file and setup own simple StTofrGeometry frame
   //   // The geometry root file can be created in the following procedure:
   //
   //  1. staf> make cavegeo
   //         > make btofgeo
   //         > rz/file 80 btofr_geo.rz on
   //
   //  2. shell> g2root btofr_geo.rz btofr_geo.root
   //
   //-------------------------------------------------------------------

   static const char* trayPref="BTR";
   static const char* senPref ="BRMD";
   static const char* topName ="HALL1";

   TObject *obj;
   TFile file(geofile);
   if (! file.IsOpen() ) return kFALSE;

   // Get the geometry in the root file
   //
   TList *list = file.GetListOfKeys();
   TObject *key;
   TGeometry *btofr_geo;
   TIter next(list);
   while ( (key=next()) != 0 ) {
      obj = file.Get(key->GetName());
      if ( strcmp(obj->ClassName(),"TGeometry") == 0) {
         btofr_geo = dynamic_cast<TGeometry*> (obj);
         break;
      }
   }
   if (!btofr_geo) return kFALSE;
   file.Close();

   TNode *topNode = btofr_geo->GetNode(topName);
   if (!topNode) return kFALSE;

   //Create the simplified geometry "top(MRS)->Tray->Module(Sensor)->Cell"
   // under this Geometry
   //
   this->cd();
   CopyTopNode(topNode);

   //Create StTofrGeomTrays and its descedenets(StTofrGeomSensors)
   //
   TList trayList;
   TList senList;
   GetPrefixNodes(topNode,trayPref,trayList);
   Int_t itray = 0 ;
   Int_t ntray = 0;
   Int_t imodule;
   TIter trayNext(&trayList);
   TNode* node;
   while ( (obj=trayNext()) != 0 ) {
      itray++;

      // if (itray>=10 && itray<=20) break;  //Skip tray=[10,20] for yesw-Test

      node = dynamic_cast<TNode*> (obj);
      senList.Clear();
      GetPrefixNodes(node,senPref,senList);
      //gDebugger.Print(mDebug,"%s, in itray=%d found module=%d\n",where,
      //     itray, senList.GetSize() );

      if (senList.GetSize() > 0) {
         ntray++;
         //
         //Create StTofrGeomTray under TopNode
         //
         mTopNode->cd();
         StTofrGeomTray *tray = StTofrGeomTray::CopyNode(node,itray);
         //gDebugger.Print(mDebug,"%s, itray=%d copied as %d tray\n",where,
	 //                      itray, ntray);
         if (!tray) {
            LOG_INFO << "StTofrGeometry::InitFromRoot, Warning!!"
                 << "failed in CopyNode=" <<node->GetName() << endm;
            break;
         }
         tray->cd();
         TIter senNext(&senList);
         TObject* obj2;
         TNode* node2;
         imodule = 0;
         while ( (obj2=senNext()) != 0 ) {
            imodule++;
            node2 = dynamic_cast<TNode*> (obj2);
            //gDebugger.Print(mDebug,"%s, adding sensor %d name=%s\n",where,
	    //imodule, node2->GetName() );
            //
            //Create StTofrGeomSensor under current GeomTray
            //
            tray->AddNode(node2,imodule);
         }
         if (mModulesInTray==0 && imodule!=0) mModulesInTray=imodule;
      }

   }
   if (mTrays==0 && ntray!=0) mTrays=ntray;

   delete btofr_geo;

   return kTRUE;
}
*/
/*
//_____________________________________________________________________________
Bool_t  StTofrGeometry::CopyTopNode(TNode* top)
{
   //
   //Create the top TNode under StTofrGeometry
   //

   //check if it is a top TNode
   //
   if (top==0) return kFALSE;
   if (top->GetParent() != 0) return kFALSE;

   TShape *shape;
   Double_t trans[3];
   TRotMatrix *newrot;
   StTofrGeomTray::PrepareCopyNode(top,0,shape,trans,newrot);

   const char* name;
   const char* title;
   name  = top->GetName();
   title = top->GetTitle();

   //
   // right under StTofrGeometry
   //
   gGeometry->SetCurrentNode(0);
   mTopNode = new TNode(name,title,shape,trans[0],trans[1],trans[2]);

   return kTRUE;
}

//_____________________________________________________________________________
void StTofrGeometry::GetPrefixNodes(const TNode* topNode,
                                           const char* key, TList &list)
{
   //
   // Recusively loop over all descendent nodes and find out those
   //  name prefixed with key
   //

   static Int_t counts = 0;

   if (!topNode || !key) return;
   if (strlen(key)==0) return;   

   TList *sons = topNode->GetListOfNodes();
   if (sons==0) return;

   TIter next(sons);   //create class TIter for iteration over sons
   TObject* obj;
   TNode*   node;
   const char *name;

   while ( (obj=next()) !=0 ) {
      node = dynamic_cast<TNode*> (obj);
      name = node->GetName();
      if (strstr(name,key)==name) {
         counts++;
         list.Add(node);
      } else {
         GetPrefixNodes(node,key,list);
      }
   }


}
*/

//_____________________________________________________________________________
Bool_t StTofrGeometry::LackThis(const char* fromWhere)
{
   if (gTofrGeometry == 0) {
      LOG_INFO << " !! Warning from " << fromWhere
           << "\n no StTofrGeometry existing, create one instance first"
           << endm;
      return kTRUE;
   } else return kFALSE;
}

/*
//_____________________________________________________________________________
TRotMatrix* StTofrGeometry::CreateMatrix(const Double_t theta)
{
   return new TRotMatrix("rot","rot",90.+theta,0.,90.,90.,theta,0.);
}
*/

//_____________________________________________________________________________
Int_t StTofrGeometry::CalcCellId(const Int_t volumeId, const Float_t* local)
const
{
   Double_t dlocal[3];
   dlocal[0] = local[0];
   dlocal[1] = local[1];
   dlocal[2] = local[2];
   return CalcCellId(volumeId,dlocal);
}

//_____________________________________________________________________________
Int_t StTofrGeometry::CalcCellId(const Int_t volumeId, const Double_t* local)
const
{
   //
   // Calculate cellID based on volumeId and local position in a sensor
   //

   Int_t icell, imodule, itray;
   DecodeVolumeId(volumeId, imodule, itray);
   StTofrGeomSensor *sensor = GetGeomSensor(imodule, itray);
   if (!sensor) return -1;
   icell = sensor->FindCellIndex(local);
   Int_t ret = CalcCellId(icell, imodule, itray);

   return ret;
}

//_____________________________________________________________________________
Bool_t StTofrGeometry::IsCellValid(const Int_t icell)
const
{
   //
   //Check the validity of cell# = [1,mCellsInModule]
   //
   return (icell>=1 && icell<=mCellsInModule);
}

//_____________________________________________________________________________
Bool_t StTofrGeometry::IsSensorValid(const Int_t imodule)
const
{
   //
   //Check the validity of module# = [1,mModulesInTray]
   //
   return (imodule>=1 && imodule<=mModulesInTray);
}

//_____________________________________________________________________________
Bool_t StTofrGeometry::IsTrayValid(const Int_t itray)
const
{
   //
   //Check the validity of tray#
   // return kTRUE if found in the tray list
   //

   Bool_t ret = kFALSE;
   //   TList *list = mTopNode->GetListOfNodes();
   TDataSetIter nextSector(mTopNode);
   TVolumeView* sectorVolume = 0;
   Int_t ibtoh = 0, i = 0;
   //   StTofrGeomTray *tray = 0;
   while ( (sectorVolume = (TVolumeView *)nextSector()) ) {
     i++;
     if ( i>mSectorsInBTOH ) ibtoh = 1;
     int trayIndex = ibtoh * mSectorsInBTOH + sectorVolume->GetPosition()->GetId();
     //     tray = new StTofrGeomTray(ibtoh, sectorVolume, mTopNode);
     //     if (tray->Index() == itray) {
     if (trayIndex == itray) {
       ret = kTRUE;
       break;
     }
     //     delete tray;
   }

   /*   TList *list = mTopNode->Nodes();
   if (!list || mTrays==0) return ret;

   StTofrGeomTray* tray;
   for (Int_t i=0; i<list->GetSize(); i++) {
      tray = dynamic_cast<StTofrGeomTray*> (list->At(i));
      if (tray->Index() == itray) {
         ret = kTRUE;
         break;
      }
   }
   */

   return ret;
}

//_____________________________________________________________________________
Int_t StTofrGeometry::CalcSensorId(const Int_t imodule, const Int_t itray)
const
{
   //
   //Encode imodule and itray into SensorId
   //

   Int_t sensorId = -1;
   if (!IsSensorValid(imodule)) return sensorId;

   Int_t idx = GetAtOfTray(itray);

   if (idx<0) return sensorId;

   sensorId = imodule-1 + mModulesInTray*idx;

   return sensorId;
}

//_____________________________________________________________________________
Int_t StTofrGeometry::PrevCellId(const Int_t cellId)
const
{
   //
   //Look up the cell prior to cellId in the same module
   // and return the cellId of cell found
   //

   Int_t found = -1;
   Int_t icell, imodule, itray;
   DecodeCellId(cellId,icell,imodule,itray);
   StTofrGeomSensor* sensor = GetGeomSensor(imodule,itray);
   Int_t icell_p = sensor->PrevCellIndex(icell);

   found = CalcCellId(icell_p,imodule,itray);

   return found;
}

//_____________________________________________________________________________
Int_t StTofrGeometry::NextCellId(const Int_t cellId)
const
{
   //
   //Look up the cell prior to cellId in the same module
   // and return the cellId of cell found
   //

   Int_t found = -1;
   Int_t icell, imodule, itray;
   DecodeCellId(cellId,icell,imodule,itray);
   StTofrGeomSensor* sensor = GetGeomSensor(imodule,itray);
   Int_t icell_p = sensor->NextCellIndex(icell);

   found = CalcCellId(icell_p,imodule,itray);

   return found;
}

//_____________________________________________________________________________
Int_t StTofrGeometry::CalcCellId(const Int_t icell, const Int_t imodule,
                                 const Int_t itray)
const
{
   //
   //Encode icell, imodule and itray into CellId
   //

   Int_t cellId = -1;
   if (!IsCellValid(icell)) return cellId;

   Int_t sensorId = CalcSensorId(imodule,itray);
   if (sensorId<0) return cellId;                 //Invalid sensorId
   
   cellId = icell-1 + mCellsInModule*sensorId;

   return cellId;
}

//_____________________________________________________________________________
Bool_t  StTofrGeometry::DecodeSensorId(const Int_t sensorId,
                                             Int_t &imodule, Int_t &itray)
const
{
   //
   //decode sensorId into tray#, module# in itray, imodule
   //

   imodule   = sensorId%mModulesInTray + 1;
   if (!IsSensorValid(imodule)) return kFALSE;

   Int_t idx = sensorId/mModulesInTray;
   StTofrGeomTray *tray = GetGeomTrayAt(idx);
   if (!tray) return kFALSE;
   itray   = tray->Index();
   delete tray;
   return kTRUE;
}

//_____________________________________________________________________________
Bool_t  StTofrGeometry::DecodeCellId(const Int_t cellId, Int_t &icell,
                                           Int_t &imodule, Int_t &itray)
const
{
   //
   //decode cellId into tray#, module#, cell# in itray, imodule, icell
   //

   Int_t sensorId = cellId/mCellsInModule;
   if (!DecodeSensorId(sensorId,imodule,itray)) return kFALSE;

   icell  = cellId%mCellsInModule + 1;

   return IsCellValid(icell);
}

//_____________________________________________________________________________
Int_t StTofrGeometry::GetCellIndex(const Int_t cellId)
const
{
   //
   //decode cellId and return cell#
   //

   Int_t icell  = cellId%mCellsInModule + 1;

   return icell;
}

//_____________________________________________________________________________
void  StTofrGeometry::DecodeVolumeId(const Int_t volumeId,
                                           Int_t &imodule, Int_t &itray)
const
{
   //
   //decode the volumeId into tray# and module# in itray, imodule
   // see the definition of TOFr's volumeId in "pams/sim/g2t/g2t_volume_id.g"
   //

   Int_t ires    = volumeId;

   Int_t rileft  = int(ires/10/100/100);
   ires          = ires-rileft*100*100*10;

   itray         = int(ires/10/100);
   ires          = ires-itray*100*10;

   imodule       = int(ires/10);
   // Int_t icell   = ires%10;  //always 0 since not defined in geant geometry

   itray = itray + (rileft-1)*mSectorsInBTOH;

   return;
}

//_____________________________________________________________________________
StTofrGeomSensor* StTofrGeometry::GetGeomCell(const Int_t cellId)
const
{
   //
   //Return StTofrGeomSensor* where the cell of cellId is in
   //

   Int_t icell, imodule, itray;
   DecodeCellId(cellId, icell, imodule, itray);
   StTofrGeomSensor* sensor = GetGeomSensor(imodule, itray);

   return sensor;
}

//_____________________________________________________________________________
StTofrGeomSensor* StTofrGeometry::GetGeomSensor(const Int_t imodule,
                                                const Int_t itray)
const
{
   //
   //itray is dummy if itray==0 and it is the current single tray
   //

   StTofrGeomTray *tray = GetGeomTray(itray);
   StTofrGeomSensor* sensor = NULL;
   if (tray) sensor = tray->GetSensor(imodule);
   delete tray;
   return sensor;
}

//_____________________________________________________________________________
StTofrGeomTray*   StTofrGeometry::GetGeomTray(const Int_t itray)
const
{
   //
   //itray is dummy if itray==0 and it is the current single tray
   //

   StTofrGeomTray* found = 0;
   //   StTofrGeomTray* tray = 0;
   TDataSetIter nextSector(mTopNode);
   TVolumeView *sectorVolume = 0;
   Int_t ibtoh = 0, i = 0;
   while ( (sectorVolume = (TVolumeView *)nextSector()) ) {
     i++;
     if (i>mSectorsInBTOH ) ibtoh = 1;
     int trayIndex = ibtoh * mSectorsInBTOH + sectorVolume->GetPosition()->GetId();
     //     tray = new StTofrGeomTray(ibtoh, sectorVolume, mTopNode);
     //     if  (tray->Index() == itray) {
     if (trayIndex == itray) {
       //         found = tray;
       found = new StTofrGeomTray(ibtoh, sectorVolume, mTopNode);
       break;
     }
     //     delete tray;
   }

   /*
   //   TList *list = mTopNode->GetListOfNodes();
   TList *list = mTopNode->Nodes();
   if (!list || mTrays==0) return NULL;

   //case of single tray, itray is dummy
   //
   if (mTrays==1 && itray==0) {
      found = dynamic_cast<StTofrGeomTray*> (list->At(0));
      return found;
   }

   //case of multiple trays
   //
   TIter next(list);
   TObject *obj;
   StTofrGeomTray* tray;
   while ( (obj=next()) != 0 ) {
      tray = dynamic_cast<StTofrGeomTray*> (obj);
      if (tray->Index() == itray) {
         found = tray;
         break;
      }
   }
   */

   return found;
}

//_____________________________________________________________________________
StTofrGeomTray* StTofrGeometry::GetGeomTrayAt(const Int_t idx)
const
{
   //
   //Get the StTofrGeomTray at index of the list
   //

   StTofrGeomTray* found = 0;

   TDataSetIter nextSector(mTopNode);
   TVolumeView *sectorVolume = 0;
   Int_t ibtoh = 0, i = 0;
   while ( (sectorVolume = (TVolumeView *)nextSector()) ) {
     i++;
     if ( i>mSectorsInBTOH ) ibtoh = 1;
     if (i==idx) {
       found = new StTofrGeomTray(ibtoh, sectorVolume, mTopNode);
     }
   }

   //   TList *list = mTopNode->GetListOfNodes();
   /*
   TList *list = mTopNode->Nodes();
   if (!list || mTrays==0) return NULL;

   if (idx<0) return found;    //bug in list->At(i) if i<0

   TVolumeView *trayVolume = dynamic_cast<StTofrGeomTray*> (list->At(idx));
   found = new StTofrGeomTray(trayVolume);
   */
   //   found = dynamic_cast<StTofrGeomTray*> (list->At(idx));

   return found;
}

//_____________________________________________________________________________
Int_t  StTofrGeometry::GetAtOfTray(const Int_t itray)
const
{
   //
   //Find out the list-index of StTofrGeomTray with TrayIndex=itray
   // itray is dummy if itray==0 and it is the current single tray
   //

   Int_t at = -1;

   //   TList *list = mTopNode->GetListOfNodes();
   
   TDataSetIter nextSector(mTopNode);
   TVolumeView *sectorVolume = 0;
   //   StTofrGeomTray *tray = 0;
   Int_t ibtoh = 0, i = 0;
   while ( (sectorVolume = (TVolumeView *)nextSector())  ) {
     i++;
     if ( i>mSectorsInBTOH ) ibtoh = 1;
     int trayIndex = ibtoh * mSectorsInBTOH + sectorVolume->GetPosition()->GetId();
     //     tray = new StTofrGeomTray(ibtoh, sectorVolume, mTopNode);
     //     if (tray->Index() == itray) {
     if (trayIndex == itray) {
       at = i;
       break;
     }
     //     delete tray;
   }

   /*
   TList *list = mTopNode->Nodes();
   if (!list || mTrays==0) return at;

   //case of single tray, itray is dummy
   //
   if (mTrays==1 && itray==0) {
      if (list->At(0) != 0) return 0;
      else return at;
   }

   //case of multiple trays
   //
   StTofrGeomTray* tray;
   for (Int_t i=0; i<list->GetSize(); i++) {
      tray = dynamic_cast<StTofrGeomTray*> (list->At(i));
      if (tray->Index() == itray) {
         at = i;
         break;
      }
   }
   */

   return at;
}

//_____________________________________________________________________________
void StTofrGeometry::Print(Option_t *opt)  const
{
   LOG_INFO << "Trays=" << mTrays <<"\t ModulesInTray=" << mModulesInTray
        << "\t CellsInModule=" << mCellsInModule << endm;
}

//_____________________________________________________________________________
Int_t StTofrGeometry::CellIdPointIn(const StThreeVectorD& point)
const
{
   //
   //Return the ID of cell which containing the global point
   //

   Int_t cellId = -1;
   Double_t xl[3], xg[3];
   xg[0] = point.x();
   xg[1] = point.y();
   xg[2] = point.z();

   //Loop over trays
   //

   Int_t itray = -1, imodule = -1, icell = -1;
   for(int i=0;i<mNValidTrays;i++) {
     if(!mTofrTray[i]) continue;
     if ( mTofrTray[i]->IsGlobalPointIn(point) ) {
       itray = mTofrTray[i]->Index();
       if ( !(mTofrTray[i]->GetfView()->GetListSize()) ) {
	 LOG_INFO << " No sensors in tray " << itray << endm;
	 return cellId;
       }
       
       for( int j=0;j<mNValidModules;j++) {
	 if(!mTofrSensor[i][j]) continue;
	 if ( mTofrSensor[i][j]->IsGlobalPointIn(point) ) {
	   imodule = mTofrSensor[i][j]->Index();
	   mTofrSensor[i][j]->Master2Local(xg,xl);
	   icell = mTofrSensor[i][j]->FindCellIndex(xl);
	 }
       } // end for (j)
     } // end if
   } // end for (i)

   /*
   TDataSetIter nextSector(mTopNode);
   TVolumeView *sectorVolume = 0;
   //   StTofrGeomTray *tray = 0;

   Int_t itray = -1, imodule = -1, icell = -1;
   Int_t ibtoh = 0, i = 0;
   while ( (sectorVolume = (TVolumeView *)nextSector())  ) {
     i++;
     if ( i>mSectorsInBTOH ) ibtoh = 1;
     StTofrGeomTray *tray = new StTofrGeomTray(ibtoh, sectorVolume, mTopNode);
     if ( tray->IsGlobalPointIn(point) ) {
       itray = tray->Index();

       if ( !(tray->GetfView()->GetListSize()) ) {
	 LOG_INFO << " No sensors in tray " << tray->Index() << endm;
	 delete tray;
	 return cellId;
       }

       TDataSetIter nextSensor(tray->GetfView());
       TVolumeView *sensorVolume = 0;
       //       StTofrGeomSensor *sensor = 0;
       while ( (sensorVolume = (TVolumeView *)nextSensor()) ) {
	 StTofrGeomSensor *sensor = new StTofrGeomSensor(sensorVolume, mTopNode);
	 if ( sensor->IsGlobalPointIn(point) ) {
	   imodule = sensor->Index();
	   sensor->Master2Local(xg,xl);
	   icell = sensor->FindCellIndex(xl);
	 }
	 delete sensor;
       }
     }
     delete tray;
   }
   */

   if ( itray <= 0 || imodule <= 0 ) return cellId;
   cellId = CalcCellId(icell, imodule, itray);
   return cellId;
   /*
   StTofrGeomTray* tray;
   Int_t itray = -1;
   //   TIter next( mTopNode->GetListOfNodes() );
   TIter next( mTopNode->Nodes() );
   while ( (obj=next()) != 0 ) {
      tray = dynamic_cast<StTofrGeomTray*> (obj);
      if ( tray->IsGlobalPointIn(point) ) {
         itray = tray->Index();
         break;
      }
   }
   if (itray <= 0) return cellId;

   //Loop over sensors in a tray
   //
   //   TIter next2( tray->GetListOfNodes() );
   TIter next2( tray->GetfView()->Nodes() );
   StTofrGeomSensor *sensor;
   Int_t imodule = -1;
   while ( (obj=next2()) != 0 ) {
      sensor = dynamic_cast<StTofrGeomSensor*> (obj);
      if ( sensor->IsGlobalPointIn(point) ) {
         imodule = sensor->Index();
         break;
      }
   }
   if (imodule <= 0) return cellId;
   */
   // Find cell# in a sensor
   /*   sensor->Master2Local(xg,xl);
   Int_t icell = sensor->FindCellIndex(xl);

   cellId = CalcCellId(icell, imodule, itray);

   return cellId;
   */
}

//_____________________________________________________________________________
Bool_t StTofrGeometry::HelixCross(const StHelixD &helix)
const
{
   //
   // return "kTRUE" if any cell is crossed by this helix
   //

   IntVec idVec;
   DoubleVec pathVec;
   PointVec crossVec;

   Bool_t crossed = HelixCrossCellIds(helix,idVec,pathVec,crossVec);

   return crossed;
}

//_____________________________________________________________________________
Bool_t StTofrGeometry::HelixCrossCellIds(const StHelixD &helix,
                       IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec)
const
{
   //
   // return "kTRUE" if any cell is crossed by this helix
   //  and also fill the cellIds which are crossed by the helix
   //  and the path length of the helix before crossing the cell
   //

   Double_t pathLen;
   Int_t cellId;
   StThreeVectorD cross;
   idVec.clear();
   pathVec.clear();
   crossVec.clear();

   for(int i=0;i<mNValidTrays;i++) {
     if(!mTofrTray[i]) continue;
     int trayId = mTofrTray[i]->Index();

     for(int j=0;j<mNValidModules;j++) {
       if(!mTofrSensor[i][j]) continue;
       int moduleId = mTofrSensor[i][j]->Index();
       if ( mTofrSensor[i][j]->HelixCross(helix,pathLen,cross) ) {	   
	 Double_t global[3], local[3];
	 global[0] = cross.x();
	 global[1] = cross.y();
	 global[2] = cross.z();
	 mTofrSensor[i][j]->Master2Local(global,local);
	 Int_t icell = mTofrSensor[i][j]->FindCellIndex(local);
	 cellId = CalcCellId(icell, moduleId, trayId);
	 if (cellId>=0) {    // a bug before // reject hit in the edge of module;
	   pathVec.push_back(pathLen);
	   idVec.push_back(cellId);
	   crossVec.push_back(cross);
	 }
       }
     } // end for (j)
   } // end for (i)
   

   /*
   TDataSetIter nextSector(mTopNode);
   TVolumeView *sectorVolume = 0;
   //   StTofrGeomTray *tray = 0;

   Int_t ibtoh = 0, i = 0;
   while ( (sectorVolume = (TVolumeView *)nextSector()) ) {
     i++;
     if ( i>mSectorsInBTOH ) ibtoh = 1;
     //
     // For tray-tofr, select tray #83 directly to save time
     //
     if ( !mTofrConf ) {
       int trayindex = ibtoh * mSectorsInBTOH + sectorVolume->GetPosition()->GetId();
       if ( trayindex!=mY03TrayIndex ) {
	 //       	 cout << " skip tray " << trayindex << endl;
	 continue;
       }
     }
     //
     StTofrGeomTray *tray = new StTofrGeomTray(ibtoh, sectorVolume, mTopNode);
     //     TVolumeView *trayVolume = tray->GetfView();
     if ( tray->HelixCross(helix,pathLen,cross) ) {

       cout << " Helix cross tray # = " << tray->Index() << endl;
       if (!tray->GetfView()->GetListSize()) {
	 delete tray;
	 return kFALSE;
       }

       TDataSetIter nextSensor(tray->GetfView());

       TVolumeView *sensorVolume = 0;

       while ( (sensorVolume = (TVolumeView *)nextSensor()) ) {
	 StTofrGeomSensor *sensor = new StTofrGeomSensor(sensorVolume, mTopNode);
	 if ( sensor->HelixCross(helix,pathLen,cross) ) {
	   
	   //	   cout << "   hit the sensor volume " << endl;
	   Double_t global[3], local[3];
	   global[0] = cross.x();
	   global[1] = cross.y();
	   global[2] = cross.z();
	   sensor->Master2Local(global,local);
	   Int_t icell = sensor->FindCellIndex(local);
	   cellId = CalcCellId(icell, sensor->Index(), tray->Index());
	   if (cellId>0) {    // reject hit in the edge of module;
	     pathVec.push_back(pathLen);
	     idVec.push_back(cellId);
	     crossVec.push_back(cross);
	   }
	 }
	 delete sensor;
       }

     } // end if
     delete tray;
     
   }
   */
   /*
   TObject *obj;
   StTofrNode *node;

   // find out the tray crossed by the helix, and one tray can be crossed
   // but there is totally only one tray of TOFr at this moment
   StTofrGeomTray *tray=NULL;
   //   TList *list = mTopNode->GetListOfNodes();
   TList *list = mTopNode->Nodes();
   TIter next(list);
   while ( (obj=next()) != 0) {
      node = dynamic_cast<StTofrNode*> (obj);
      if ( node->HelixCross(helix,pathLen,cross) ) {
         tray = dynamic_cast<StTofrGeomTray*> (obj);
         break;
      }
   }

   if (!tray) return kFALSE;
 
   // find out modules crossed by the helix
   // and next cells further
   //
   StTofrGeomSensor *sensor;
   //   list = tray->GetListOfNodes();
   list = tray->GetfView()->Nodes();
   TIter next2(list);
   while ( (obj=next2()) != 0 ) {
      node = dynamic_cast<StTofrNode*> (obj);
      if ( node->HelixCross(helix,pathLen,cross) ) { //module crossed
         sensor = dynamic_cast<StTofrGeomSensor*> (obj);

         // Method-1: just Look up the cell the point of cross is in
         //
         Double_t global[3], local[3];
         global[0] = cross.x();
         global[1] = cross.y();
         global[2] = cross.z();
         sensor->Master2Local(global,local);
         Int_t icell = sensor->FindCellIndex(local);
         cellId = CalcCellId(icell, sensor->Index(), tray->Index());
         pathVec.push_back(pathLen);
         idVec.push_back(cellId);
         crossVec.push_back(cross);
      }
   }
   */

   if (idVec.size()>0) {
     //     mleak1.PrintMem(" normal return true");
     return kTRUE;
   }
   else {
     //     mleak1.PrintMem(" normal return false");
     return kFALSE;
   }
}


//_____________________________________________________________________________
Bool_t StTofrGeometry::HelixCross(const StHelixD &helix, IntVec validModuleVec, IntVec projTrayVec)
const
{
   //
   // return "kTRUE" if any cell is crossed by this helix
   //

   IntVec idVec;
   DoubleVec pathVec;
   PointVec crossVec;

   Bool_t crossed = HelixCrossCellIds(helix,validModuleVec, projTrayVec, idVec,pathVec,crossVec);

   return crossed;
}


//_____________________________________________________________________________
Bool_t StTofrGeometry::HelixCrossCellIds(const StHelixD &helix, IntVec validModuleVec, IntVec projTrayVec, IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec)
const
{
  /////////////////////////////////////////////////////////
  // optimized :
  // input : helix, validModuleVec from DAQ, 
  //         projTrayVec possible hit tray from this helix
  //                                   Xin Dong
  /////////////////////////////////////////////////////////
   //
   // return "kTRUE" if any cell is crossed by this helix
   //  and also fill the cellIds which are crossed by the helix
   //  and the path length of the helix before crossing the cell
   //

   if(validModuleVec.size()==0) return kFALSE;
   if(projTrayVec.size()==0) return kFALSE;

   Double_t pathLen;
   Int_t cellId;
   StThreeVectorD cross;
   idVec.clear();
   pathVec.clear();
   crossVec.clear();

   for(int i=0;i<mNValidTrays;i++) {
     if(!mTofrTray[i]) continue;
     int trayId = mTofrTray[i]->Index();
     bool itrayFind = kFALSE;

     for(size_t it=0;it<projTrayVec.size();it++) {
       int validtrayId = projTrayVec[it];
       if(validtrayId==trayId) {
	 itrayFind = kTRUE;
	 break;
       }
     }
     if(!itrayFind) continue;

     //     LOG_INFO << " Helix cross sensitive tray " << trayId << endm;

     for(int j=0;j<mNValidModules;j++) {
       if(!mTofrSensor[i][j]) continue;
       int moduleId = mTofrSensor[i][j]->Index();
       for(size_t iv=0;iv<validModuleVec.size();iv++) {
	 int validtrayId = validModuleVec[iv]/100;
	 int validmoduleId = validModuleVec[iv]%100;
	 if(validtrayId==trayId&&validmoduleId==moduleId) {
	   if ( mTofrSensor[i][j]->HelixCross(helix,pathLen,cross) ) {	   
	     Double_t global[3], local[3];
	     global[0] = cross.x();
	     global[1] = cross.y();
	     global[2] = cross.z();
	     mTofrSensor[i][j]->Master2Local(global,local);
	     Int_t icell = mTofrSensor[i][j]->FindCellIndex(local);
	     cellId = CalcCellId(icell, moduleId, trayId);
	     if (cellId>=0) {   // a bug before // reject hit in the edge of module;
	       pathVec.push_back(pathLen);
	       idVec.push_back(cellId);
	       crossVec.push_back(cross);
	     }
	   }
	 } // endif (tray && module)
       } // end for (iv)
     } // end for (j)
   } // end for (i)

   /*
   TDataSetIter nextSector(mTopNode);
   TVolumeView *sectorVolume = 0;
   //   StTofrGeomTray *tray = 0;

   Int_t ibtoh = 0, i = 0;
   while ( (sectorVolume = (TVolumeView *)nextSector()) ) {
     i++;
     if ( i>mSectorsInBTOH ) ibtoh = 1;
     //
     // For tray-tofr, select tray #83 directly to save time
     //
     if ( !mTofrConf ) {
       int trayindex = ibtoh * mSectorsInBTOH + sectorVolume->GetPosition()->GetId();
       if ( trayindex!=mY03TrayIndex ) {
	 //       	 LOG_INFO << " skip tray " << trayindex << endm;
	 continue;
       }
     }
     //
     StTofrGeomTray *tray = new StTofrGeomTray(ibtoh, sectorVolume, mTopNode);
     //     TVolumeView *trayVolume = tray->GetfView();
     if ( tray->HelixCross(helix,pathLen,cross) ) {

       LOG_INFO << " Helix cross tray # = " << tray->Index() << endm;
       if (!tray->GetfView()->GetListSize()) {
	 delete tray;
	 return kFALSE;
       }

       TDataSetIter nextSensor(tray->GetfView());

       TVolumeView *sensorVolume = 0;

       while ( (sensorVolume = (TVolumeView *)nextSensor()) ) {
	 StTofrGeomSensor *sensor = new StTofrGeomSensor(sensorVolume, mTopNode);
	 if ( sensor->HelixCross(helix,pathLen,cross) ) {
	   
	   //	   LOG_INFO << "   hit the sensor volume " << endm;
	   Double_t global[3], local[3];
	   global[0] = cross.x();
	   global[1] = cross.y();
	   global[2] = cross.z();
	   sensor->Master2Local(global,local);
	   Int_t icell = sensor->FindCellIndex(local);
	   cellId = CalcCellId(icell, sensor->Index(), tray->Index());
	   if (cellId>0) {    // reject hit in the edge of module;
	     pathVec.push_back(pathLen);
	     idVec.push_back(cellId);
	     crossVec.push_back(cross);
	   }
	 }
	 delete sensor;
       }

     } // end if
     delete tray;
     
   }
   */
   /*
   TObject *obj;
   StTofrNode *node;

   // find out the tray crossed by the helix, and one tray can be crossed
   // but there is totally only one tray of TOFr at this moment
   StTofrGeomTray *tray=NULL;
   //   TList *list = mTopNode->GetListOfNodes();
   TList *list = mTopNode->Nodes();
   TIter next(list);
   while ( (obj=next()) != 0) {
      node = dynamic_cast<StTofrNode*> (obj);
      if ( node->HelixCross(helix,pathLen,cross) ) {
         tray = dynamic_cast<StTofrGeomTray*> (obj);
         break;
      }
   }

   if (!tray) return kFALSE;
 
   // find out modules crossed by the helix
   // and next cells further
   //
   StTofrGeomSensor *sensor;
   //   list = tray->GetListOfNodes();
   list = tray->GetfView()->Nodes();
   TIter next2(list);
   while ( (obj=next2()) != 0 ) {
      node = dynamic_cast<StTofrNode*> (obj);
      if ( node->HelixCross(helix,pathLen,cross) ) { //module crossed
         sensor = dynamic_cast<StTofrGeomSensor*> (obj);

         // Method-1: just Look up the cell the point of cross is in
         //
         Double_t global[3], local[3];
         global[0] = cross.x();
         global[1] = cross.y();
         global[2] = cross.z();
         sensor->Master2Local(global,local);
         Int_t icell = sensor->FindCellIndex(local);
         cellId = CalcCellId(icell, sensor->Index(), tray->Index());
         pathVec.push_back(pathLen);
         idVec.push_back(cellId);
         crossVec.push_back(cross);
      }
   }
   */

   if (idVec.size()>0) {
     //     mleak1.PrintMem(" normal return true");
     return kTRUE;
   }
   else {
     //     mleak1.PrintMem(" normal return false");
     return kFALSE;
   }
}

//---------------------------------------------------------------------------
// estimate the possible projection on the TOF tray
Bool_t StTofrGeometry::projTrayVector(const StHelixD &helix, IntVec &trayVec) const {

  trayVec.clear();
  double R_tof = 215.;
  double res = 5.0;
  double s1 = helix.pathLength(R_tof).first;
  if(s1<0.) s1 = helix.pathLength(R_tof).second;
  StThreeVectorD point = helix.at(s1);
  double phi = point.phi()*180/3.14159;

  // east ring, start from 108 deg (id=61) , clock-wise from east facing west
  int itray_east = (255+(int)phi)%360/6+61;
  trayVec.push_back(itray_east);

  int itray_east1 = (255+(int)(phi+res))%360/6+61;
  int itray_east2 = (255+(int)(phi-res))%360/6+61;
  if(itray_east1!=itray_east) {
    trayVec.push_back(itray_east1);
  }
  if(itray_east2!=itray_east&&itray_east2!=itray_east1) {
    trayVec.push_back(itray_east2);
  }
  
  // west ring, start from 72 deg (id=1) , clock-wise from west facing east
  int itray_west = (435-(int)phi)%360/6+1;
  trayVec.push_back(itray_west);

  int itray_west1 = (435-(int)(phi+res))%360/6+1;
  int itray_west2 = (435-(int)(phi-res))%360/6+1;
  if(itray_west1!=itray_west) {
    trayVec.push_back(itray_west1);
  }
  if(itray_west2!=itray_west&&itray_west2!=itray_west1) {
    trayVec.push_back(itray_west2);
  }

//   LOG_INFO << " proj tray id = ";
//   for(size_t it=0;it<trayVec.size();it++) {
//     LOG_INFO << trayVec[it] << " ";
//   }
//   LOG_INFO << endm;
  
  if(trayVec.size()>0) return kTRUE;
  else return kFALSE;
}

/*
//_____________________________________________________________________________
//
//  non-member function
//  ==========
//_____________________________________________________________________________
Bool_t lackTofrgeometry(const char* fromWhere)
{
   if (gTofrGeometry == 0) {
      LOG_INFO << " !! Warning from " << fromWhere
           << "\n no StTofrGeometry existing, create one instance first"
           << endm;
      return kTRUE;
   } else return kFALSE;
}
*/

/*******************************************************************
 * $Log: StTofrGeometry.cxx,v $
 * Revision 1.12  2018/02/26 23:26:51  smirnovd
 * StTof: Remove outdated ClassImp macro
 *
 * Revision 1.11  2018/02/26 23:13:21  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.10  2009/01/26 15:05:33  fisyak
 * rename TMemStat => StMemStat due to clash with ROOT class
 *
 * Revision 1.9  2008/03/27 00:15:39  dongx
 *  Update for Run8 finished.
 *
 * Revision 1.8  2007/04/17 23:01:52  dongx
 * replaced with standard STAR Loggers
 *
 * Revision 1.7  2007/02/28 23:28:17  dongx
 * R_tof used for pre-match calculation updated to ~215cm
 *
 * Revision 1.6  2004/05/03 23:07:49  dongx
 * -Introduce data members to save the Tray and Sensor geometries in the initialization.
 * -Optimize the HelixCrossCellIds() function to save CPU time
 * -Introduce a new function projTrayVector()
 * -Update the ClassDef number 1->2
 *
 *
 * Revision 1.4  2004/03/09 16:45:16  dongx
 * Remove InitDaqMap() since a StTofrDaqMap is introduced
 *
 * Revision 1.3  2003/09/11 05:49:23  perev
 * ansi corrs
 *
 * Revision 1.2  2003/09/07 03:49:06  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.1  2003/08/06 23:00:53  geurts
 * First Release
 */
