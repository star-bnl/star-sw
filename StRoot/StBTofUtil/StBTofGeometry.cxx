/*******************************************************************
 *
 * $Id: StBTofGeometry.cxx,v 1.2 2009/02/12 01:45:57 dongx Exp $
 * 
 * Authors: Shuwei Ye, Xin Dong
 *******************************************************************
 *
 * Description: Collection of geometry classes for the TOF-MRPC
 *              initializes from GEANT geometry
 *
 *******************************************************************
 * $Log: StBTofGeometry.cxx,v $
 * Revision 1.2  2009/02/12 01:45:57  dongx
 * Clean up
 *
 * Revision 1.1  2009/02/02 21:56:54  dongx
 * first release - Barrel geometry
 *
 *******************************************************************/
#include "Stiostream.h"
#include <math.h>
#include <vector>
#include <stdlib.h>
#include <stdio.h>

#include "StBTofGeometry.h"
#include "TFile.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
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

#ifdef __ROOT__      
ClassImp(StBTofNode)
#endif

Bool_t StBTofNode::mDebug = kFALSE;
Double_t const StBTofGeomSensor::mSensorDy = 10.35;   // Actual module length;
char* const StBTofGeometry::sectorPref = "BSEC";
char* const StBTofGeometry::trayPref   = "BTRA";
char* const StBTofGeometry::senPref    = "BRMD";

//_____________________________________________________________________________
StBTofNode::StBTofNode(TVolumeView *element, TVolumeView *top)
  : fView(element), pView(element->GetPosition()), mMasterNode(top), mTransFlag(kFALSE)
{
   UpdateMatrix();
   BuildMembers();
}

//_____________________________________________________________________________
StBTofNode::~StBTofNode()
{ 
  fView = 0;
  pView = 0;
  mMasterNode = 0;
  mTransFlag = kFALSE;
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
   CalcMatrix(this, mTransMRS, mRotMRS);
   mTransFlag = kTRUE;

}

//_____________________________________________________________________________
void StBTofNode::CalcMatrix(StBTofNode* son,
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
     TVolumeView *son = GetfView();
     TVolumeView *mrs = GetTopNode();
     
     TVolumePosition *pos = 0;
     pos = son->Local2Master(son, mrs);
     pos->Local2Master(local, master);
     delete pos;
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

#ifdef __ROOT__      
ClassImp(StBTofGeomTray)
#endif

Bool_t StBTofGeomTray::mDebug = kFALSE;

//_____________________________________________________________________________
StBTofGeomTray::StBTofGeomTray(const Int_t ibtoh, TVolumeView *sector, TVolumeView *top) 
  : StBTofNode((TVolumeView *)sector->First(), top)
{
  mSectorsInBTOH = top->GetListSize()/2;
  mBTOHIndex = ibtoh + 1;
  mTrayIndex = ibtoh * mSectorsInBTOH + sector->GetPosition()->GetId();
}

//_____________________________________________________________________________
StBTofGeomTray::~StBTofGeomTray()
{
  mBTOHIndex = 0;
  mTrayIndex = 0;
}
//_____________________________________________________________________________
StBTofGeomSensor* StBTofGeomTray::GetSensor(const Int_t imodule) const
{

   StBTofGeomSensor* sensor = 0;

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
	 sensor = new StBTofGeomSensor(sensorVolume, top);
       }
     }

   return sensor;
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

#ifdef __ROOT__      
ClassImp(StBTofGeomSensor)
#endif

Bool_t StBTofGeomSensor::mDebug = kFALSE;

//_____________________________________________________________________________
StBTofGeomSensor::StBTofGeomSensor(TVolumeView *element, TVolumeView *top) 
  : StBTofNode(element, top)
{
   mModuleIndex = element->GetPosition()->GetId();
   CreateGeomCells();
}

//_____________________________________________________________________________
StBTofGeomSensor::~StBTofGeomSensor()
{
  mModuleIndex = 0;
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

#ifdef __ROOT__      
ClassImp(StBTofGeometry)
#endif

Bool_t StBTofGeometry::mDebug = kFALSE;

//_____________________________________________________________________________
StBTofGeometry::StBTofGeometry(const char* name, const char* title)
  : TNamed(name,title)
{
   mCellsInModule  = StBTofGeomSensor::GetCells();
   mModulesInTray  = 0;
   mTrays          = 0;
   mRootFile       = 0;
   mInitFlag       = kFALSE;
   mTopNode        = 0;
   mStarHall       = 0;

   for(int i=0;i<mNTrays;i++) {
     mBTofTray[i] = 0;
     for(int j=0;j<mNModules;j++) {
       mBTofSensor[i][j] = 0;
     }
   }

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
void StBTofGeometry::Init(TVolume *starHall)
{
   //
   //Define geometry parameters and establish the geometry
   //  
   InitFromStar(starHall);
   mStarHall = starHall;
}
//_____________________________________________________________________________
void StBTofGeometry::InitFromStar(TVolume *starHall)
{
  // Initialize TOFr geometry from STAR geometry
  //     BTofConf   --     0     tray_BTof   (default)
  //                       1     full_BTof

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
  //  mModulesInTray = 32;

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
  mBTofConf = 0;
  if (mTrays==120) mBTofConf = 1;
  //

  /////////////////////////////
  // save the sensors and trays
  /////////////////////////////
  LOG_INFO << " # of trays = " << mTopNode->GetListSize() << endm;
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
    mBTofTray[mNValidTrays] = new StBTofGeomTray(ibtoh, sectorVolume, mTopNode);
    TList *list1 = trayVolume->Nodes();
    //    gMessMgr->Info("","OS") << "   # of modules in tray " << mBTofTray[mNValidTrays]->Index() << " = " << trayVolume->GetListSize() << endm;
    if (!list1 ) continue;
    TVolumeView *sensorVolume = 0;
    if(list1->GetSize()>mNValidModules) mNValidModules=list1->GetSize(); 
    for(Int_t j=0;j<list1->GetSize();j++) {
      sensorVolume = dynamic_cast<TVolumeView*> (list1->At(j));
      mBTofSensor[mNValidTrays][j] = new StBTofGeomSensor(sensorVolume, mTopNode);
    }
    mNValidTrays++;
  }
  LOG_INFO << "\n-------------------------------------------\n"
           << " Summary of initialization: "
           << "    NValidTrays = " << mNValidTrays << "   NValidModules = " << mNValidModules << endm;

  return;

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
/*
   TDataSetIter nextSector(mTopNode);
   TVolumeView* sectorVolume = 0;
   Int_t ibtoh = 0, i = 0;
   while ( (sectorVolume = (TVolumeView *)nextSector()) ) {
     i++;
     if ( i>mSectorsInBTOH ) ibtoh = 1;
     int trayIndex = ibtoh * mSectorsInBTOH + sectorVolume->GetPosition()->GetId();
     if (trayIndex == itray) {
       ret = kTRUE;
       break;
     }
   }
*/
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
   StBTofGeomTray *tray = GetGeomTrayAt(idx);
   if (!tray) return kFALSE;
   itray   = tray->Index();
   delete tray;
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

   StBTofGeomTray *tray = GetGeomTray(itray);
   StBTofGeomSensor* sensor = NULL;
   if (tray) sensor = tray->GetSensor(imodule);
   delete tray;
   return sensor;
}

//_____________________________________________________________________________
StBTofGeomTray*   StBTofGeometry::GetGeomTray(const Int_t itray)
const
{
   //
   //itray is dummy if itray==0 and it is the current single tray
   //

   StBTofGeomTray* found = 0;
   //   StBTofGeomTray* tray = 0;
   TDataSetIter nextSector(mTopNode);
   TVolumeView *sectorVolume = 0;
   Int_t ibtoh = 0, i = 0;
   while ( (sectorVolume = (TVolumeView *)nextSector()) ) {
     i++;
     if (i>mSectorsInBTOH ) ibtoh = 1;
     int trayIndex = ibtoh * mSectorsInBTOH + sectorVolume->GetPosition()->GetId();
     //     tray = new StBTofGeomTray(ibtoh, sectorVolume, mTopNode);
     //     if  (tray->Index() == itray) {
     if (trayIndex == itray) {
       //         found = tray;
       found = new StBTofGeomTray(ibtoh, sectorVolume, mTopNode);
       break;
     }
     //     delete tray;
   }

   return found;
}

//_____________________________________________________________________________
StBTofGeomTray* StBTofGeometry::GetGeomTrayAt(const Int_t idx)
const
{
   //
   //Get the StBTofGeomTray at index of the list
   //

   StBTofGeomTray* found = 0;

   TDataSetIter nextSector(mTopNode);
   TVolumeView *sectorVolume = 0;
   Int_t ibtoh = 0, i = 0;
   while ( (sectorVolume = (TVolumeView *)nextSector()) ) {
     i++;
     if ( i>mSectorsInBTOH ) ibtoh = 1;
     if (i==idx) {
       found = new StBTofGeomTray(ibtoh, sectorVolume, mTopNode);
     }
   }

   return found;
}

//_____________________________________________________________________________
Int_t  StBTofGeometry::GetAtOfTray(const Int_t itray)
const
{
   //
   //Find out the list-index of StBTofGeomTray with TrayIndex=itray
   // itray is dummy if itray==0 and it is the current single tray
   //

   Int_t at = -1;

   //   TList *list = mTopNode->GetListOfNodes();
   
   TDataSetIter nextSector(mTopNode);
   TVolumeView *sectorVolume = 0;
   //   StBTofGeomTray *tray = 0;
   Int_t ibtoh = 0, i = 0;
   while ( (sectorVolume = (TVolumeView *)nextSector())  ) {
     i++;
     if ( i>mSectorsInBTOH ) ibtoh = 1;
     int trayIndex = ibtoh * mSectorsInBTOH + sectorVolume->GetPosition()->GetId();
     //     tray = new StBTofGeomTray(ibtoh, sectorVolume, mTopNode);
     //     if (tray->Index() == itray) {
     if (trayIndex == itray) {
       at = i;
       break;
     }
     //     delete tray;
   }

   return at;
}

//_____________________________________________________________________________
void StBTofGeometry::Print(Option_t *opt)  const
{
   LOG_INFO << "Trays=" << mTrays <<"\t ModulesInTray=" << mModulesInTray
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
       
       for( int j=0;j<mNValidModules;j++) {
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

   Double_t pathLen;
   Int_t cellId;
   StThreeVectorD cross;
   idVec.clear();
   pathVec.clear();
   crossVec.clear();

   for(int i=0;i<mNValidTrays;i++) {
     if(!mBTofTray[i]) continue;
     int trayId = mBTofTray[i]->Index();

     for(int j=0;j<mNValidModules;j++) {
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

     for(int j=0;j<mNValidModules;j++) {
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
