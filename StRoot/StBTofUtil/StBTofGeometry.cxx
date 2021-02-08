/*******************************************************************
 *
 * $Id: StBTofGeometry.cxx,v 1.32 2019/08/07 15:56:52 geurts Exp $
 * 
 * Authors: Shuwei Ye, Xin Dong
 *******************************************************************
 *
 * Description: Collection of geometry classes for the TOF-MRPC
 *              initializes from GEANT geometry
 *
 *******************************************************************/
#include "Stiostream.h"
#include <algorithm>
#include <array>
#include <math.h>
#include <vector>
#include <string>
#include <stdlib.h>
#include <stdio.h>
#include "tables/St_tofGeomAlign_Table.h"

#include "StBTofGeometry.h"
#include "TFile.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TGeoBBox.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TGeoPhysicalNode.h"

#include "StMaker.h"
//#include "TMemStat.h"
#include "StMessMgr.h"

//#include "Debugger.h"

//////////////////////////////////////////////////////////////////////////
//
// group of classes for BTof Geometry:
//
//    StBTofGeometry, StBTofNode
//    StBTofGeomTray, StBTofGeomSensor
//
// Usage:
//
//   StBTofGeometry* geo = new StBTofGeometry("btof","btof geometry");
//    geo->Init(TVolume *starHall);
//
// ---------------------------------------------------------------------------
//
// StBTofNode
// ==============
//
//////////////////////////////////////////////////////////////////////////////


Bool_t StBTofNode::mDebug = kFALSE;
Double_t const StBTofGeomSensor::mSensorDy = 10.35;   // Actual module length;
const char* StBTofGeometry::sectorPref = "BSEC";
const char* StBTofGeometry::trayPref   = "BTRA";
const char* StBTofGeometry::senPref    = "BRMD";

//_____________________________________________________________________________
StBTofNode::StBTofNode(TVolumeView *element, TVolumeView *top, const StThreeVectorD& align, TVolumePosition *pos)
  : fView(element), pView(new TVolumePosition(*pos)), mMasterNode(top), mTransFlag(kFALSE),
    mAlign{align.x(), align.y(), align.z()}
{
   SetBit(kIsOwner, false);

   UpdateMatrix();
   BuildMembers();
}


StBTofNode::StBTofNode(const TGeoPhysicalNode& gpNode, const StThreeVectorD& align) :
  fView(nullptr), pView(nullptr), mMasterNode(nullptr), mTransFlag(false),
  mAlign{align.x(), align.y(), align.z()}
{
  SetBit(kIsOwner, true);

  TGeoBBox* bbox = static_cast<TGeoBBox*>( gpNode.GetShape() );

  mTShape = new TBRIK( gpNode.GetVolume()->GetName(), "BTOF shape", "unknown", bbox->GetDX(), bbox->GetDY(), bbox->GetDZ() );
  mTVolume = new TVolume( gpNode.GetVolume()->GetName(), "BTOF volume", mTShape );

  TGeoHMatrix* ghMatrix = static_cast<TGeoHMatrix*>( gpNode.GetMatrix() );

  double* rotationM = ghMatrix->GetRotationMatrix();

  double  rotationF[9] = {
    rotationM[0], rotationM[3], rotationM[6],
    rotationM[1], rotationM[4], rotationM[7],
    rotationM[2], rotationM[5], rotationM[8]
  };

  // The rotation matrix is owned by pView
  TRotMatrix* rotMatrix = new TRotMatrix("rotMatrix", "BTOF rotation matrix", rotationF);

  double* trans = ghMatrix->GetTranslation();

  pView = new TVolumePosition(mTVolume, trans[0], trans[1], trans[2], rotMatrix);
  pView->SetMatrixOwner(true);

  fView = new TVolumeView( static_cast<TVolume*>(nullptr), pView);

  TVolumePosition* masterPosition = new TVolumePosition(nullptr, 0, 0, 0, TVolume::GetIdentity());

  // By default masterPosition is owned by mMasterNode
  mMasterNode = new TVolumeView( static_cast<TVolume*>(nullptr), masterPosition);

   UpdateMatrix();
   BuildMembers();
}


//_____________________________________________________________________________
void  StBTofNode::UpdateMatrix()
{
   //
   //Update the Translation and RotateMatrix between local and master
   //  and store them as members: mTransMRS, mRotMRS
   //while TNode stores them locally to share among all TNode objects,
   //  thus they may be changed afterward !
   //

   mTransFlag = kFALSE;
   CalcMatrix(this, mAlign, mTransMRS, mRotMRS);
   mTransFlag = kTRUE;

}

//_____________________________________________________________________________
void StBTofNode::CalcMatrix(StBTofNode* son, Double_t* align,
              Double_t* trans, Double_t* rot, StBTofNode* mother)
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
   //   LOG_INFO << "StBTofNode::CalcMatrix" << endm;

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

   // Alignment parameters;
   trans[0] += align[0];
   trans[1] += align[1];
   trans[2] += align[2];   

   return;
}

//_____________________________________________________________________________
void StBTofNode::ConvertPos(
                     StBTofNode* from, const Double_t* pos_from,
            StBTofNode* to,         Double_t* pos_to)
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
void  StBTofNode::BuildMembers()
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
void StBTofNode::Local2Master(const Double_t* local, Double_t* master)
{
   //
   //Transform local coordinate into global coordinate
   //
   //  pView->UpdateMatrix();
   if (!mTransFlag) {
     if (!mMasterNode) { LOG_INFO << " no Master! " << endm; return;}
     if (pView) {
       pView->Local2Master(local, master);
     } else {
       TVolumeView *son = GetfView();
       TVolumeView *mrs = GetTopNode();
     
       TVolumePosition *pos = 0;
       pos = son->Local2Master(son, mrs);
       pos->Local2Master(local, master);
       delete pos;
     }
     return;
   }

   //mTransFlag==kTRUE, i.e. StBTofGeomNode::UpdateMatrix() invoked already
   Double_t x, y, z;
   x = local[0];
   y = local[1];
   z = local[2];

   master[0] = mTransMRS[0] + mRotMRS[0]*x + mRotMRS[3]*y + mRotMRS[6]*z;
   master[1] = mTransMRS[1] + mRotMRS[1]*x + mRotMRS[4]*y + mRotMRS[7]*z;
   master[2] = mTransMRS[2] + mRotMRS[2]*x + mRotMRS[5]*y + mRotMRS[8]*z;

}

//_____________________________________________________________________________
void StBTofNode::Master2Local(const Double_t* master, Double_t* local)
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

   //mTransFlag==kTRUE, i.e. StBTofGeomNode::UpdateMatrix() invoked already
   Double_t x, y, z;
   x = master[0] - mTransMRS[0];
   y = master[1] - mTransMRS[1];
   z = master[2] - mTransMRS[2];

   local[0] = mRotMRS[0]*x + mRotMRS[1]*y + mRotMRS[2]*z;
   local[1] = mRotMRS[3]*x + mRotMRS[4]*y + mRotMRS[5]*z;
   local[2] = mRotMRS[6]*x + mRotMRS[7]*y + mRotMRS[8]*z;
}

//_____________________________________________________________________________
StThreeVectorD StBTofNode::YZPlaneNormal()
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
StThreeVectorD  StBTofNode::GetCenterPosition()
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
Bool_t StBTofNode::IsLocalPointIn(const Double_t x, const Double_t y, 
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
Bool_t  StBTofNode::IsGlobalPointIn(const StThreeVectorD &global)
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
Bool_t StBTofNode::HelixCross(const StHelixD &helix, Double_t &pathLen,
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
Bool_t StBTofNode::HelixCross(const StHelixD &helix, Double_t &pathLen,
                                  StThreeVectorD &cross, Double_t &theta)
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
	   theta = planeNormal.angle(helix.cat(pathLen));
      //
      // Check if the intersected point is really in the cell
      //
      ret = IsGlobalPointIn(cross);
   }
   return ret;
}

//_____________________________________________________________________________
void StBTofNode::Print(Option_t *opt) const
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

//////////////////////////////////////////////////////////////////////////////
//
// StBTofGeomTray
// ==============
//
//////////////////////////////////////////////////////////////////////////////


Bool_t StBTofGeomTray::mDebug = kFALSE;

//_____________________________________________________________________________
StBTofGeomTray::StBTofGeomTray(const Int_t ibtoh, TVolumeView *sector, TVolumeView *top, const StThreeVectorD& align, TVolumePosition *pos)
  : StBTofNode((TVolumeView *)sector->First(), top, align, pos)
{
  mSectorsInBTOH = top->GetListSize()/2;
  mBTOHIndex = ibtoh + 1;
  mTrayIndex = ibtoh * mSectorsInBTOH + sector->GetPosition()->GetId();
}


StBTofGeomTray::StBTofGeomTray(const int trayId, const TGeoPhysicalNode& node, const StThreeVectorD& align)
  : StBTofNode(node, align)
{
  int mBTOHId  = ( trayId <= 60 ? 1 : 2 );
  int sectorId = ( trayId <= 60 ? trayId : trayId - 60 );

  mSectorsInBTOH = StBTofGeometry::mNTrays/2;
  mBTOHIndex = mBTOHId;
  mTrayIndex = (mBTOHId - 1) * mSectorsInBTOH + sectorId;
}


//_____________________________________________________________________________
void StBTofGeomTray::Print(const Option_t *opt) const
{
   LOG_INFO << "StBTofGeomTray, tray#=" << mTrayIndex << endm;
   StBTofNode::Print(opt);
}


//////////////////////////////////////////////////////////////////////////////
//
// StBTofGeomSensor
// ================
//
//////////////////////////////////////////////////////////////////////////////


Bool_t StBTofGeomSensor::mDebug = kFALSE;

//_____________________________________________________________________________
StBTofGeomSensor::StBTofGeomSensor(TVolumeView *element, TVolumeView *top, const StThreeVectorD& align, TVolumePosition *pos) 
  : StBTofNode(element, top, align, pos)
{
   mModuleIndex = element->GetPosition()->GetId();
   CreateGeomCells();
}


StBTofGeomSensor::StBTofGeomSensor(const int moduleId, const TGeoPhysicalNode& node, const StThreeVectorD& align)
  : StBTofNode(node, align)
{
   mModuleIndex = moduleId;
   CreateGeomCells();
}


//_____________________________________________________________________________
void StBTofGeomSensor::CreateGeomCells()
{
   //
   //Divide this sensor to creat cells
   //

   //
   // change the size according to the real cells
   //    mSensorDy -- defined by myself
   //
   Double_t sensor_dy = mSensorDy;
   Double_t cell_width = 2*sensor_dy/mCells;

   for (int i=0; i<=mCells; i++) mCellY[i] = cell_width*i - sensor_dy;

}

//_____________________________________________________________________________
Double_t StBTofGeomSensor::GetCellYMin(const Int_t icell)
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
Double_t StBTofGeomSensor::GetCellYMax(const Int_t icell)
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
StThreeVectorD StBTofGeomSensor::GetCellPosition(const Int_t icell)
{
   //
   // Get the center position of cell in this sensor
   //

   static const char* where = "StBTofGeomSensor::GetCellPosition";
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
Int_t StBTofGeomSensor::FindCellIndex(const Double_t* local)
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
void StBTofGeomSensor::Print(const Option_t *opt) const
{
   LOG_INFO << "StBTofGeomSensor, module#=" << mModuleIndex << endm;
   StBTofNode::Print(opt);

   LOG_INFO << " Cells=" << mCells << "\t Y range for cells=\n";
   for (Int_t i=0; i<=mCells; i++) LOG_INFO << " : " << mCellY[i];
   LOG_INFO << endm;
}


//////////////////////////////////////////////////////////////////////////////
//
// StBTofGeometry
// ==============
//
//////////////////////////////////////////////////////////////////////////////

StBTofGeometry *gBTofGeometry = 0;
static const Int_t CELLSINMODULE = 6;


Bool_t StBTofGeometry::mDebug = kFALSE;

//_____________________________________________________________________________
StBTofGeometry::StBTofGeometry(const char* name, const char* title)
  : TNamed(name,title)
{
   mCellsInModule  = StBTofGeomSensor::GetCells();
   mModulesInTray  = 0;
   mNValidTrays    = 0;
   mRootFile       = 0;
   mInitFlag       = kFALSE;
   mTopNode        = 0;
   mIsMC           = kFALSE;
   SetAlignFile("");

   std::fill( mBTofTray, mBTofTray + mNTrays, nullptr );
   std::fill( &mBTofSensor[0][0], &mBTofSensor[mNTrays][0], nullptr );

   //
   //We only need one instance of StBTofGeometry
   //
   if (gBTofGeometry) {
      LOG_INFO << "Warning !! There is already StBTofGeometry at pointer="
           << (void*)gBTofGeometry << ", so it is deleted"
           << endm;
      delete gBTofGeometry;
   }
   gBTofGeometry = this;
}

//_____________________________________________________________________________
StBTofGeometry::~StBTofGeometry()
{
   LOG_INFO << "Warning !! StBTofGeometry at pointer =" << (void*)gBTofGeometry
        << " is deleted" << endm;
   gBTofGeometry = 0;

   for(int i=0;i<mNTrays;i++) {
     if(mBTofTray[i]) delete mBTofTray[i];
     mBTofTray[i] = 0;
     for(int j=0;j<mNModules;j++) {
       if(mBTofSensor[i][j]) delete mBTofSensor[i][j];
       mBTofSensor[i][j] = 0;
     }
   }
   
}

//_____________________________________________________________________________
void StBTofGeometry::Init(StMaker *maker, TVolume *starHall, TGeoManager* geoManager )
{
   //
   //Define geometry parameters and establish the geometry
   //  
   if(maker->Debug()) DebugOn();

   // Zero out internal alignment arrays
   std::fill_n(mTrayX0, mNTrays, 0);
   std::fill_n(mTrayY0, mNTrays, 0);
   std::fill_n(mTrayZ0, mNTrays, 0);

   // retrieve align parameters  -- need to use db

   std::array<double, mNTrays> phi0{}, x0{}, z0{};

   // If not MC input, load the alignment parameters from the database; otherwise ignore.
   if (mIsMC) {
     LOG_INFO << "[StBTofGeometry] detected MC-mode: ignore alignment corrections" << endm;
   } else if (strcmp(mAlignFile.c_str(),"")!=0) {
     LOG_INFO << "[StBTofGeometry] receiving alignment parameters from input files" << endm;
     ifstream inData;
     inData.open(mAlignFile.c_str());
     if(inData.good()) {
       for(int i=0;i<mNTrays;i++) {
         inData >> phi0[i] >> z0[i] >> x0[i];
       }
     } else {
       LOG_WARN << " Bad input file ! Use ideal geometry! " << endm;
     }
     inData.close();
   } else {
     LOG_INFO << "[StBTofGeometry] retrieving geometry alignment parameters from database" << endm;
     TDataSet *mDbTOFDataSet = maker->GetDataBase("Calibrations/tof/tofGeomAlign");
     if (!mDbTOFDataSet) {
       LOG_WARN << "[StBTofGeometry] unable to find Calibrations/tof/tofGeomAlign! Use ideal geometry!" << endm;
     } else {
       St_tofGeomAlign* tofGeomAlign = static_cast<St_tofGeomAlign*>(mDbTOFDataSet->Find("tofGeomAlign"));
       if(!tofGeomAlign) {
	 LOG_WARN << "Unable to get tof geometry align parameter! Use ideal geometry!" << endm;
       } else {
         tofGeomAlign_st* geomAlign = static_cast<tofGeomAlign_st*>(tofGeomAlign->GetArray());
     
         for (Int_t i=0;i<mNTrays;i++) {
           phi0[i] = geomAlign[i].phi0;
           x0[i]   = geomAlign[i].x0;
           z0[i]   = geomAlign[i].z0;
         }
       }
     }
   }

   for(int i=0;i<mNTrays;i++) {
     double phi;
     if(i<60) {
       phi = 72 - i*6;   // phi angle of tray Id = i+1, west
       double cs = TMath::Cos(phi*TMath::Pi()/180.);
       double ss = TMath::Sin(phi*TMath::Pi()/180.);
       mTrayX0[i] = phi0[i]*ss + x0[i]*cs;
       mTrayY0[i] = -phi0[i]*cs + x0[i]*ss;
       mTrayZ0[i] = z0[i];
     } else {
       phi = 108 + (i-60)*6;   // phi angle of tray Id = i+1, east
       double cs = TMath::Cos(phi*TMath::Pi()/180.);
       double ss = TMath::Sin(phi*TMath::Pi()/180.);
       mTrayX0[i] = -phi0[i]*ss + x0[i]*cs;
       mTrayY0[i] = phi0[i]*cs + x0[i]*ss;
       mTrayZ0[i] = -z0[i];  // thus z0 will be the distance between the tray end to the TPC central membrane
     }

     if(maker->Debug()) {
       LOG_DEBUG << " Tray # = " << i+1 << " Align parameters " << mTrayX0[i] << " " << mTrayY0[i] << " " << mTrayZ0[i] << endm;
     }
   }

   if ( geoManager )
     InitFrom( *geoManager );
   else if ( starHall )
     InitFrom( *starHall );
   else
     LOG_ERROR << "StBTofGeometry::Init - Cannot build BTOF geometry without Geant or TGeo input\n";


/* Starting with geometry tags in Y2013, GMT units were installed into tof trays 8,23,93, & 108.
 * This caused a shift in the module index for geant[1-24] instead of daqs [5-28].
 * This is a correction to shift the geant modules such that they can match with daq info
 *
 * Be certain that you select the correct geometry tag when using this. Y2012 tag and Y2013+ data will generate a bug!
 */ 
   if( maker->GetDateTime().GetYear() >= 2013 ){
     LOG_INFO << "StBTofGeometry::Init -- GEMTOF-tray module indexes will be corrected for year " <<  maker->GetDateTime().GetYear() << endm;
     for(Int_t j=0;j<mModulesInTray;j++){
       Int_t imod(0);
       if(mBTofSensor[7][j]){
	 imod = mBTofSensor[7][j]->Index();
	 mBTofSensor[7][j]->SetIndex(imod+4);  //The shift is 4
       }
       if(mBTofSensor[22][j]){
	 imod = mBTofSensor[22][j]->Index();
	 mBTofSensor[22][j]->SetIndex(imod+4);
       }
       if(mBTofSensor[92][j]){
	 imod = mBTofSensor[92][j]->Index();
	 mBTofSensor[92][j]->SetIndex(imod+4);
       }
       if(mBTofSensor[107][j]){
	 imod = mBTofSensor[107][j]->Index();
	 mBTofSensor[107][j]->SetIndex(imod+4);
       }  
     }//for j
   }//if Year

   mInitFlag = true;
}
//_____________________________________________________________________________
void StBTofGeometry::InitFrom(TVolume &starHall)
{
  // Initialize TOFr geometry from STAR geometry
  //     BTofConf   --     0     tray_BTof   (default)
  //                       1     full_BTof

  // Loop over the STAR geometry and mark the volume needed
  TDataSetIter volume(&starHall,0);

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

  starHall.SetVisibility(TVolume::kBothVisible);
  mTopNode = new TVolumeView(starHall,10);

  mSectorsInBTOH = mTopNode->GetListSize()/2;    // # of sectors in one half

  /////////////////////////////
  // save the sensors and trays
  /////////////////////////////
  if(!mTopNode) {
    LOG_WARN << " No Top Node for Tof Geometry! " << endm;
    return;
  }
  LOG_INFO << "[StBTofGeometry] # of sectors = " << mTopNode->GetListSize() << endm;

  TVolumePosition *transPos = 0;

//  TDataSetIter nextDet(mTopNode, 0);
  TVolumeViewIter nextDet((TVolumeView *)mTopNode, 0);
  TVolumeView *secVolume = 0;
  TVolumeView *detVolume = 0;
  Int_t ibtoh = 0;
  Int_t isec = 0;
  Int_t isensor = 0;
  mNValidTrays = 0;
  mModulesInTray = 0;
  StThreeVectorD align{};
  while ( (detVolume = (TVolumeView *)nextDet()) ) {

    if(strcmp(detVolume->GetName(), sectorPref)==0) {  // sector volume
      isec++;
      secVolume = (TVolumeView *)detVolume;
      continue;   // sector continue;
    }
    if(strcmp(detVolume->GetName(), trayPref)==0) {   // tray volume
      if ( isec>60 ) ibtoh = 1;
      int sectorsInBTOH = mTopNode->GetListSize()/2;
      int trayIndex = ibtoh * sectorsInBTOH + secVolume->GetPosition()->GetId(); // secVolume
      LOG_DEBUG << " Tray # " << trayIndex << " has # of modules = " << detVolume->GetListSize() << endm;
      isensor = 0;   // clear for this tray
      if(detVolume->GetListSize()) {   // valid tray

        int itray = trayIndex - 1;

        align.set(mTrayX0[itray], mTrayY0[itray], mTrayZ0[itray]);
        transPos = nextDet[0];

        mNValidTrays++;

        mBTofTray[mNValidTrays-1] = new StBTofGeomTray(ibtoh, secVolume, mTopNode, align, transPos);
        delete transPos;  transPos = 0;

        if(mDebug) {
          LOG_DEBUG << "   Initialize and save tray # " << mBTofTray[mNValidTrays-1]->Index() << " with " << detVolume->GetListSize() << " modules" << endm;
          LOG_DEBUG << "   alignment parameters \t" << mTrayX0[itray] << " " <<  mTrayY0[itray] << " " <<  mTrayZ0[itray] << endm;
          mBTofTray[mNValidTrays-1]->Print();
        }

      }
    }
    if(strcmp(detVolume->GetName(), senPref)==0) {   // module volume

      transPos = nextDet[0];

      mBTofSensor[mNValidTrays-1][isensor] = new StBTofGeomSensor(detVolume, mTopNode, align, transPos);
      delete transPos;   transPos = 0;

      if(mDebug) mBTofSensor[mNValidTrays-1][isensor]->Print();

      isensor++;
      if(isensor>mModulesInTray) mModulesInTray = isensor;
    }
  }

  mBTofConf = 0;
  if (mNValidTrays==120) mBTofConf = 1;

  LOG_INFO << "[StBTofGeometry] Done. NValidTrays = " << mNValidTrays << "  NModulesInTray = " << mModulesInTray << endm;

  if(mDebug) Print();

  return;

}

/**
 * Creates BTof volumes for mBTofTray and mBTofSensor arrays using the
 * corresponding volume parameters in TGeoManager.
 */
void StBTofGeometry::InitFrom(TGeoManager &geoManager)
{
  mNValidTrays = 0;

  for (int trayId = 1; trayId <= mNTrays; trayId++)
  {
    bool hasGmt = StBTofGeometry::TrayHasGmtModules(trayId);

    std::string geoPath( FormTGeoPath(geoManager, trayId, hasGmt) );

    if ( geoPath.empty() ) {
      LOG_WARN << "StBTofGeometry::InitFrom(...) - Cannot find path to BTOF tray "
                  "(id " << trayId << "). Skipping...\n";
      continue;
    }

    mNValidTrays++;

    const TGeoPhysicalNode* gpNode = geoManager.MakePhysicalNode( geoPath.c_str() );

    StThreeVectorD align(mTrayX0[trayId-1], mTrayY0[trayId-1], mTrayZ0[trayId-1]);

    mBTofTray[trayId-1] = new StBTofGeomTray(trayId, *gpNode, align);

    // Loop over the max number of modules (mNModules) that can be present in a tray
    int maxModuleId = hasGmt ? 24 : mNModules;

    for(int moduleId = 1; moduleId <= maxModuleId; moduleId++)
    {
      std::string geoPath( FormTGeoPath(geoManager, trayId, hasGmt, moduleId) );

      if ( geoPath.empty() ) {
         LOG_WARN << "StBTofGeometry::InitFrom(...) - Cannot find path to BTOF module "
                     "(id " << moduleId << "). Skipping...\n";
         continue;
      }

      const TGeoPhysicalNode* gpNode = geoManager.MakePhysicalNode( geoPath.c_str() );

      mBTofSensor[trayId-1][moduleId-1] = new StBTofGeomSensor(moduleId, *gpNode, align);
    }
  }

  mBTofConf = ( mNValidTrays == 120 ? 1 : 0 );

  mModulesInTray = mNModules;

  LOG_INFO << "[StBTofGeometry] Done. NValidTrays = " << mNValidTrays << "  NModulesInTray = " << mModulesInTray << endm;
}


/**
 * Returns a full path to the BTOF module placed in a predifined location in
 * the detector's ROOT geometry. An empty string is returned if the module not
 * found in the geometry hierarchy (via TGeoManager).
 */
std::string StBTofGeometry::FormTGeoPath(TGeoManager &geoManager,
  int trayId, bool hasGmt, int moduleId)
{
  // BTOH_1/BTO1_2 - east/west
  // TOF trayId map: west=1-60, east=61-120
  int halfId   = ( trayId <= 60 ? 1 : 2 );
  int sectorId = ( trayId <= 60 ? trayId : trayId - 60 );

  std::ostringstream geoPath;

  // Node paths depend on using TpcRefSys
  bool trs = geoManager.FindVolumeFast("TpcRefSys");
  geoPath << "/HALL_1/CAVE_1/" << ((trs) ? "TpcRefSys_1/" : "") << "BTOF_1"
          << (halfId == 1 ? "/BTOH_" : "/BTO1_") << halfId;

  // Node names depend on whether this sector contains GMT modules
  geoPath << ( hasGmt ? "/BSE1_"  : "/BSEC_" ) << sectorId
          << ( hasGmt ? "/BTR1_1" : "/BTRA_1");

  // Go deeper only when module is requested
  if ( moduleId >= 1 )
  {
    geoPath << ( hasGmt ? "/BXT1_1/BRT1_1/BGM1_1/BRM1_" :
                          "/BXTR_1/BRTC_1/BGMT_1/BRMD_" )
            << moduleId;
  }

  bool found = geoManager.CheckPath( geoPath.str().c_str() );

  return found ? geoPath.str() : "";
}


//_____________________________________________________________________________
Bool_t StBTofGeometry::ContainOthers(TVolume *element)
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

//_____________________________________________________________________________
Bool_t StBTofGeometry::LackThis(const char* fromWhere)
{
   if (gBTofGeometry == 0) {
      LOG_INFO << " !! Warning from " << fromWhere
           << "\n no StBTofGeometry existing, create one instance first"
           << endm;
      return kTRUE;
   } else return kFALSE;
}

//_____________________________________________________________________________
Int_t StBTofGeometry::CalcCellId(const Int_t volumeId, const Float_t* local)
const
{
   Double_t dlocal[3];
   dlocal[0] = local[0];
   dlocal[1] = local[1];
   dlocal[2] = local[2];
   return CalcCellId(volumeId,dlocal);
}

//_____________________________________________________________________________
Int_t StBTofGeometry::CalcCellId(const Int_t volumeId, const Double_t* local)
const
{
   //
   // Calculate cellID based on volumeId and local position in a sensor
   //

   Int_t icell, imodule, itray;
   DecodeVolumeId(volumeId, imodule, itray);
   StBTofGeomSensor *sensor = GetGeomSensor(imodule, itray);
   if (!sensor) return -1;
   icell = sensor->FindCellIndex(local);
   Int_t ret = CalcCellId(icell, imodule, itray);

   return ret;
}

//_____________________________________________________________________________
Bool_t StBTofGeometry::IsCellValid(const Int_t icell)
const
{
   //
   //Check the validity of cell# = [1,mCellsInModule]
   //
   return (icell>=1 && icell<=mCellsInModule);
}

//_____________________________________________________________________________
Bool_t StBTofGeometry::IsSensorValid(const Int_t imodule)
const
{
   //
   //Check the validity of module# = [1,mModulesInTray]
   //
   return (imodule>=1 && imodule<=mModulesInTray);
}

//_____________________________________________________________________________
Bool_t StBTofGeometry::IsTrayValid(const Int_t itray)
const
{
   //
   //Check the validity of tray#
   // return kTRUE if found in the tray list
   //

   Bool_t ret = kFALSE;
   for(int i=0;i<mNValidTrays;i++) {
     if(!mBTofTray[i]) continue;
     if(mBTofTray[i]->Index() == itray) {
       ret = kTRUE;
       break;
     }
   }

   return ret;
}

//_____________________________________________________________________________
Int_t StBTofGeometry::CalcSensorId(const Int_t imodule, const Int_t itray)
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
Int_t StBTofGeometry::PrevCellId(const Int_t cellId)
const
{
   //
   //Look up the cell prior to cellId in the same module
   // and return the cellId of cell found
   //

   Int_t found = -1;
   Int_t icell, imodule, itray;
   DecodeCellId(cellId,icell,imodule,itray);
   StBTofGeomSensor* sensor = GetGeomSensor(imodule,itray);
   Int_t icell_p = sensor->PrevCellIndex(icell);

   found = CalcCellId(icell_p,imodule,itray);

   return found;
}

//_____________________________________________________________________________
Int_t StBTofGeometry::NextCellId(const Int_t cellId)
const
{
   //
   //Look up the cell prior to cellId in the same module
   // and return the cellId of cell found
   //

   Int_t found = -1;
   Int_t icell, imodule, itray;
   DecodeCellId(cellId,icell,imodule,itray);
   StBTofGeomSensor* sensor = GetGeomSensor(imodule,itray);
   Int_t icell_p = sensor->NextCellIndex(icell);

   found = CalcCellId(icell_p,imodule,itray);

   return found;
}

//_____________________________________________________________________________
Int_t StBTofGeometry::CalcCellId(const Int_t icell, const Int_t imodule,
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
Bool_t  StBTofGeometry::DecodeSensorId(const Int_t sensorId,
                                             Int_t &imodule, Int_t &itray)
const
{
   //
   //decode sensorId into tray#, module# in itray, imodule
   //

   imodule   = sensorId%mModulesInTray + 1;
   if (!IsSensorValid(imodule)) return kFALSE;

   Int_t idx = sensorId/mModulesInTray;

   itray = GetTrayIndexAt(idx);

   StBTofGeomTray *tray = GetGeomTrayAt(idx);
   if (!tray) return kFALSE;
   itray   = tray->Index();
//   delete tray;
   return kTRUE;
}

//_____________________________________________________________________________
Bool_t  StBTofGeometry::DecodeCellId(const Int_t cellId, Int_t &icell,
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
Int_t StBTofGeometry::GetCellIndex(const Int_t cellId)
const
{
   //
   //decode cellId and return cell#
   //

   Int_t icell  = cellId%mCellsInModule + 1;

   return icell;
}

//_____________________________________________________________________________
void  StBTofGeometry::DecodeVolumeId(const Int_t volumeId,
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
StBTofGeomSensor* StBTofGeometry::GetGeomCell(const Int_t cellId)
const
{
   //
   //Return StBTofGeomSensor* where the cell of cellId is in
   //

   Int_t icell, imodule, itray;
   DecodeCellId(cellId, icell, imodule, itray);
   StBTofGeomSensor* sensor = GetGeomSensor(imodule, itray);

   return sensor;
}

//_____________________________________________________________________________
StBTofGeomSensor* StBTofGeometry::GetGeomSensor(const Int_t imodule,
                                                const Int_t itray)
const
{
   //
   //itray is dummy if itray==0 and it is the current single tray
   //

   for(int i=0;i<mNValidTrays;i++) {
     if(!mBTofTray[i]) continue;
     if(mBTofTray[i]->Index()==itray) {
       for(int j=0;j<mModulesInTray;j++) {
         if(!mBTofSensor[i][j]) continue;
         if(mBTofSensor[i][j]->Index()==imodule) return mBTofSensor[i][j];
       }
     }
   }

   return 0;
}

//_____________________________________________________________________________
StBTofGeomTray*   StBTofGeometry::GetGeomTray(const Int_t itray)
const
{
   //
   //itray is dummy if itray==0 and it is the current single tray
   //

   for(int i=0;i<mNValidTrays;i++) {
     if(!mBTofTray[i]) continue;
     if(mBTofTray[i]->Index()==itray) return mBTofTray[i];
   }
   return 0;
}

//_____________________________________________________________________________
Int_t StBTofGeometry::GetTrayIndexAt(const Int_t idx)
const
{
   if (!mTopNode) {
     return idx + 1;
   }

   //
   //Get the tray index at index of the list
   //

   TDataSetIter nextSector(mTopNode);
   TVolumeView *sectorVolume = 0;
   Int_t ibtoh = 0, i = 0;
   Int_t itray = -1;
   while ( (sectorVolume = (TVolumeView *)nextSector()) ) {
     i++;
     if ( i>mSectorsInBTOH ) ibtoh = 1;
     int trayIndex = ibtoh * mSectorsInBTOH + sectorVolume->GetPosition()->GetId();
     if (i==idx) {
       itray = trayIndex;
       break;
     }
   }

   return itray;
}

//_____________________________________________________________________________
StBTofGeomTray* StBTofGeometry::GetGeomTrayAt(const Int_t idx)
const
{
   //
   //Get the StBTofGeomTray at index of the list
   //

   Int_t itray = GetTrayIndexAt(idx);
   if(itray<=0||itray>mNTrays) return 0;

   for(int i=0;i<mNValidTrays; i++) {
     if(mBTofTray[i] && mBTofTray[i]->Index()==itray) return mBTofTray[i];
   }

   return 0;
}

//_____________________________________________________________________________
Int_t  StBTofGeometry::GetAtOfTray(const Int_t itray)
const
{
   if (!mTopNode) {
     return itray - 1;
   }

   //
   //Find out the list-index of StBTofGeomTray with TrayIndex=itray
   // itray is dummy if itray==0 and it is the current single tray
   //

   Int_t at = -1;

   TDataSetIter nextSector(mTopNode);
   TVolumeView *sectorVolume = 0;
   Int_t ibtoh = 0, i = 0;
   while ( (sectorVolume = (TVolumeView *)nextSector())  ) {
     i++;
     if ( i>mSectorsInBTOH ) ibtoh = 1;
     int trayIndex = ibtoh * mSectorsInBTOH + sectorVolume->GetPosition()->GetId();
     if (trayIndex == itray) {
       at = i;
       break;
     }
   }

   return at;
}

//_____________________________________________________________________________
void StBTofGeometry::Print(Option_t *opt)  const
{
   LOG_INFO << "Trays=" << mNValidTrays <<"\t ModulesInTray=" << mModulesInTray
        << "\t CellsInModule=" << mCellsInModule << endm;
}

//_____________________________________________________________________________
Int_t StBTofGeometry::CellIdPointIn(const StThreeVectorD& point)
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
     if(!mBTofTray[i]) continue;
     if ( mBTofTray[i]->IsGlobalPointIn(point) ) {
       itray = mBTofTray[i]->Index();
       if ( !(mBTofTray[i]->GetfView()->GetListSize()) ) {
	 LOG_INFO << " No sensors in tray " << itray << endm;
	 return cellId;
       }
       
       for( int j=0;j<mModulesInTray;j++) {
	 if(!mBTofSensor[i][j]) continue;
	 if ( mBTofSensor[i][j]->IsGlobalPointIn(point) ) {
	   imodule = mBTofSensor[i][j]->Index();
	   mBTofSensor[i][j]->Master2Local(xg,xl);
	   icell = mBTofSensor[i][j]->FindCellIndex(xl);
	 }
       } // end for (j)
     } // end if
   } // end for (i)

   if ( itray <= 0 || imodule <= 0 ) return cellId;
   cellId = CalcCellId(icell, imodule, itray);
   return cellId;
}

//_____________________________________________________________________________
Bool_t StBTofGeometry::HelixCross(const StHelixD &helix)
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
Bool_t StBTofGeometry::HelixCrossCellIds(const StHelixD &helix,
                       IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec)
const
{
   //
   // return "kTRUE" if any cell is crossed by this helix
   //  and also fill the cellIds which are crossed by the helix
   //  and the path length of the helix before crossing the cell
   //

   IntVec projTrayVec;
   if( !projTrayVector(helix, projTrayVec) ) return kFALSE;

   Double_t pathLen;
   Int_t cellId;
   StThreeVectorD cross;
   idVec.clear();
   pathVec.clear();
   crossVec.clear();

   for(int i=0;i<mNValidTrays;i++) {
     if(!mBTofTray[i]) continue;
     int trayId = mBTofTray[i]->Index();

     /// first justify if the tray is within the projection range

     bool itrayFind = kFALSE;
     for(size_t it=0;it<projTrayVec.size();it++) {
       int validtrayId = projTrayVec[it];
       if(validtrayId==trayId) {
         itrayFind = kTRUE;
         break;
       }
     }
     if(!itrayFind) continue;

     for(int j=0;j<mModulesInTray;j++) {
       if(!mBTofSensor[i][j]) continue;
       int moduleId = mBTofSensor[i][j]->Index();
       if ( mBTofSensor[i][j]->HelixCross(helix,pathLen,cross) ) {	   
	 Double_t global[3], local[3];
	 global[0] = cross.x();
	 global[1] = cross.y();
	 global[2] = cross.z();
	 mBTofSensor[i][j]->Master2Local(global,local);
	 Int_t icell = mBTofSensor[i][j]->FindCellIndex(local);
	 cellId = CalcCellId(icell, moduleId, trayId);
	 if (cellId>=0) {    // a bug before // reject hit in the edge of module;
	   pathVec.push_back(pathLen);
	   idVec.push_back(cellId);
	   crossVec.push_back(cross);
	 }
       }
     } // end for (j)
   } // end for (i)
   
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
Bool_t StBTofGeometry::HelixCrossCellIds(const StHelixD &helix,
                       IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec, DoubleVec &thetaVec)
const
{
   //
   // return "kTRUE" if any cell is crossed by this helix
   //  and also fill the cellIds which are crossed by the helix
   //  and the path length of the helix before crossing the cell
   //

   IntVec projTrayVec;
   if( !projTrayVector(helix, projTrayVec) ) return kFALSE;

   Double_t pathLen,theta;
   Int_t cellId;
   StThreeVectorD cross;
   idVec.clear();
   pathVec.clear();
   crossVec.clear();

   for(int i=0;i<mNValidTrays;i++) {
     if(!mBTofTray[i]) continue;
     int trayId = mBTofTray[i]->Index();

     /// first justify if the tray is within the projection range

     bool itrayFind = kFALSE;
     for(size_t it=0;it<projTrayVec.size();it++) {
       int validtrayId = projTrayVec[it];
       if(validtrayId==trayId) {
         itrayFind = kTRUE;
         break;
       }
     }
     if(!itrayFind) continue;

     for(int j=0;j<mModulesInTray;j++) {
       if(!mBTofSensor[i][j]) continue;
       int moduleId = mBTofSensor[i][j]->Index();
       if ( mBTofSensor[i][j]->HelixCross(helix,pathLen,cross,theta) ) {
	 Double_t global[3], local[3];
	 global[0] = cross.x();
	 global[1] = cross.y();
	 global[2] = cross.z();
	 mBTofSensor[i][j]->Master2Local(global,local);
	 Int_t icell = mBTofSensor[i][j]->FindCellIndex(local);
	 cellId = CalcCellId(icell, moduleId, trayId);
	 if (cellId>=0) {    // a bug before // reject hit in the edge of module;
	   pathVec.push_back(pathLen);
	   idVec.push_back(cellId);
	   crossVec.push_back(cross);
	   thetaVec.push_back(theta);

	 }
       }
     } // end for (j)
   } // end for (i)

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
Bool_t StBTofGeometry::HelixCross(const StHelixD &helix, IntVec validModuleVec, IntVec projTrayVec)
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
Bool_t StBTofGeometry::HelixCrossCellIds(const StHelixD &helix, IntVec validModuleVec, IntVec projTrayVec, IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec)
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
     if(!mBTofTray[i]) continue;
     int trayId = mBTofTray[i]->Index();
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

     for(int j=0;j<mModulesInTray;j++) {
       if(!mBTofSensor[i][j]) continue;
       int moduleId = mBTofSensor[i][j]->Index();
       for(size_t iv=0;iv<validModuleVec.size();iv++) {
	 int validtrayId = validModuleVec[iv]/100;
	 int validmoduleId = validModuleVec[iv]%100;
	 if(validtrayId==trayId&&validmoduleId==moduleId) {
	   if ( mBTofSensor[i][j]->HelixCross(helix,pathLen,cross) ) {	   
	     Double_t global[3], local[3];
	     global[0] = cross.x();
	     global[1] = cross.y();
	     global[2] = cross.z();
	     mBTofSensor[i][j]->Master2Local(global,local);
	     Int_t icell = mBTofSensor[i][j]->FindCellIndex(local);
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
Bool_t StBTofGeometry::projTrayVector(const StHelixD &helix, IntVec &trayVec) const {

  trayVec.clear();
  double R_tof[2]= {210., 216.};  // inner and outer surfaces
//  double res = 5.0;
// second choice - choose 2 neighbouring trays

  for(int i=0;i<2;i++) {

    double s = helix.pathLength(R_tof[i]).first;
    if(s<0.) s = helix.pathLength(R_tof[i]).second;
    StThreeVectorD point = helix.at(s);
    double phi = point.phi()*180/3.14159;
    double z = point.z();

    int itray[3] = {0,0,0};
    if(z<0) {
      // east ring, start from 108 deg (id=61) , clock-wise from east facing west
      itray[0] = (255+(int)phi)%360/6+61;
      itray[1] = itray[0] - 1;
      if(itray[1]<=60) itray[1] += 60;
      itray[2] = itray[0] + 1;
      if(itray[2]>120) itray[2] -= 60;
//      itray[1] = (255+(int)(phi+res))%360/6+61;
//      itray[2] = (255+(int)(phi-res))%360/6+61;
    } else {
      // west ring, start from 72 deg (id=1) , clock-wise from west facing east
      itray[0] = (435-(int)phi)%360/6+1;
      itray[1] = itray[0] - 1;
      if(itray[1]<=0) itray[1] += 60;
      itray[2] = itray[0] + 1;
      if(itray[2]>60) itray[2] -= 60;
//      itray[1] = (435-(int)(phi+res))%360/6+1;
//      itray[2] = (435-(int)(phi-res))%360/6+1;
    }

    for(int k=0;k<3;k++) {
      if(itray[k]<=0 || itray[k]>120) continue;

      bool found = kFALSE;
      for(size_t j=0;j<trayVec.size();j++) {
        if(trayVec[j]==itray[k]) {
          found = kTRUE;
          break;
        }
      }

      if(found) continue;

      trayVec.push_back(itray[k]);
    } // end loop k  
  } // end loop i

/*
   cout << " proj tray id = ";
   for(size_t it=0;it<trayVec.size();it++) {
     cout << trayVec[it] << " ";
   }
   cout << endl;
*/
  
  if(trayVec.size()>0) return kTRUE;
  else return kFALSE;
}

/*******************************************************************
 * $Log: StBTofGeometry.cxx,v $
 * Revision 1.32  2019/08/07 15:56:52  geurts
 * Fix node paths when using TpcRefSys (affects runs before 2013)
 *
 * Revision 1.31  2018/02/26 23:29:12  smirnovd
 * StBTofGeometry: Missing flag set to initialize geometry once
 *
 * I believe this flag should be set but have been overlooked.
 * Otherwise why call IsInitDone() in StBTofMatchMaker?
 *
 * Revision 1.30  2018/02/26 23:29:05  smirnovd
 * StBTofGeometry: Simple relation between tray index and id
 *
 * Revision 1.29  2018/02/26 23:29:00  smirnovd
 * StBTofGeometry: Introduced alternative initialization using TGeo geometry
 *
 * Revision 1.28  2018/02/26 23:28:53  smirnovd
 * StBTofGeometry: Added private InitFrom(TGeoManager)
 *
 * Revision 1.27  2018/02/26 23:28:45  smirnovd
 * StBTofGeometry: InitFrom(TVolume*) to InitFrom(TVolume&)
 *
 * Revision 1.26  2018/02/26 23:28:38  smirnovd
 * StBTofGeometry: s/InitFromStar/InitFrom/ and make it private
 *
 * Revision 1.25  2018/02/26 23:28:22  smirnovd
 * StBTofGeometry: New method to form TGeo paths for trays and modules
 *
 * Revision 1.24  2018/02/26 23:28:14  smirnovd
 * StBTofGeomSensor: New constructor accepting TGeo
 *
 * Revision 1.23  2018/02/26 23:28:07  smirnovd
 * StBTofGeoTray: New constructor accepting TGeo
 *
 * Revision 1.22  2018/02/26 23:28:00  smirnovd
 * StBTofNode: New constructor accepting TGeo volume
 *
 * The new TGeo constructor creates transient TVolume objects to provide
 * functionality compatible with the existing TVolume-base geometry
 * transformations. Unlike previously, the TVolume objects are owned by this class
 * and so have to be deleted.
 *
 * Revision 1.21  2018/02/26 23:27:53  smirnovd
 * Accept reference instead of pointer to xyz alignment
 *
 * Revision 1.20  2018/02/26 23:27:45  smirnovd
 * StBTofGeometry: Senseless assignments in destructors
 *
 * Revision 1.19  2018/02/26 23:27:38  smirnovd
 * StBTofGeometry: Set correct z component for tray alignment
 *
 * It makes more sense to set the right sign for the z component depending on the
 * BTOF half instead of accounting for that sign later.
 *
 * Revision 1.18  2018/02/26 23:27:30  smirnovd
 * StBTofGeometry: C++ style to zero out arrays
 *
 * Revision 1.17  2018/02/26 23:27:23  smirnovd
 * StBTofGeometry: Use std::array in place of plain C arrays
 *
 * Revision 1.16  2018/02/26 23:27:15  smirnovd
 * StBTofGeometry: Removed unused member pointer to non-TGeo ROOT geometry
 *
 * Revision 1.15  2018/02/26 23:13:19  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.14  2017/10/20 17:50:33  smirnovd
 * Squashed commit of the following:
 *
 *     StBTof: Remove outdated ClassImp macro
 *
 *     Prefer explicit namespace for std:: names in header files
 *
 *     Removed unnecessary specification of default std::allocator
 *
 * Frank signed-off
 *
 * Revision 1.13  2014/02/06 21:21:13  geurts
 * Fix Index() of modules in GEMTOF trays, only applies to Run 13+ geometries [Joey Butterworth]
 *
 * Revision 1.12  2011/07/27 16:15:12  geurts
 * Alignment calibration modifications [Patrick Huck]:
 *  - added mAlignFile and SetAlignFile for use in StBTofMatchMaker
 *  - phi0, x0, z0 made mNTrays dependent
 *
 * Revision 1.11  2010/09/17 20:40:09  geurts
 * Protect Init() and InitFromStar() against non-initialized database/geant.
 * No immediate crash, but a LOG_ERROR instead.
 *
 * Revision 1.10  2010/08/09 18:45:36  geurts
 * Include methods in StBTofNode and StBTofGeometry that calculate local theta [Masa]
 *
 * Revision 1.9  2010/07/14 20:35:28  geurts
 * introduce switch to enable ideal MC geometry, without alignment updates. Default: disabled
 *
 * Revision 1.8  2010/05/25 22:09:44  geurts
 * improved database handling and reduced log output
 *
 * Revision 1.7  2010/04/03 02:00:53  dongx
 * X0 (radial offset) included in the tray alignment
 *
 * Revision 1.6  2009/09/15 00:17:27  dongx
 * Corrected the calculation for tray alignment parameters in X-Y
 *
 * Revision 1.5  2009/08/25 15:41:29  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 1.4  2009/03/18 14:18:18  dongx
 * - Optimized the geometry initialization function, reduced the CPU time use
 * - Optimized the HelixCrossCellIds() function, now doing the tray fast projection to reduce the loop
 *
 * Revision 1.3  2009/02/13 00:00:56  dongx
 * Tray geometry alignment implemented.
 *
 * Revision 1.2  2009/02/12 01:45:57  dongx
 * Clean up
 *
 * Revision 1.1  2009/02/02 21:56:54  dongx
 * first release - Barrel geometry
 */
