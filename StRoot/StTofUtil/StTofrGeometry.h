/*******************************************************************
 *
 * $Id: StTofrGeometry.h,v 1.10 2018/02/26 23:27:02 smirnovd Exp $
 * 
 * Authors: Shuwei Ye, Xin Dong
 *******************************************************************
 *
 * Description: Collection of geometry classes for the TOF-MRPC
 *              initializes from GEANT geometry
 *
 *******************************************************************/
#ifndef STTOFRGEOMETRY_H
#define STTOFRGEOMETRY_H

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// group of classes for Tofr Geometry:                                  //
//                                                                      //
//    StTofrGeometry(v1), StTofNode(v2), StTofrGeomNode(v2)(off)        //
//    StTofrGeomTray(v1), StTofrGeomSensor(v2), StTofrGeomCell(off)     //
//                                                                      //
// Usage:                                                               //
//   StTofrGeometry* geo = new StTofrGeometry("tofr","tofr geometry");  //
//     geo->Init(TVolume *starHall, const Int_t TofrConf);              //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TObject.h"
#include "TList.h"
#include "TNode.h"
#include "TBRIK.h"
#include "TGeometry.h"
#include "StThreeVectorD.hh"
#include "StHelixD.hh"
#include "TVolume.h"
#include "TVolumePosition.h"
#include "TVolumeView.h"
#include "TVolumeViewIter.h"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
#ifndef __CINT__
#if !defined(ST_NO_TEMPLATE_DEF_ARGS)
typedef vector<Int_t>  IntVec;
typedef vector<Double_t>  DoubleVec;
typedef vector<StThreeVector<double> > PointVec;
#else
typedef vector<Int_t, allocator<Int_t> >  IntVec;
typedef vector<Double_t, allocator<Double_t> >  DoubleVec;
typedef vector<StThreeVector<double>, allocator<StThreeVector<double>>> PointVec;
#endif
#endif
class StTofrNode;
class StTofrGeomNode;
class StTofrGeomTray;
class StTofrGeomSensor;
// class StTofrGeomCell;
class StTofrGeometry;


//////////////////////////////////////////////////////////////////////////////
//
// StTofrGeomNode
// ==============
//
//////////////////////////////////////////////////////////////////////////////

#if 0
class StTofrGeomNode : public TNode {
 protected:
   Double_t  mTransMRS[3];   //Translate vector in MRS
   Double_t  mRotMRS[9];     //RotateMatrix from MRS to this
   Bool_t    mTransFlag;     //Flag, kTRUE=if translation/matrix updated
   // Double_t  mCenterRxy;     //center position R(xy) in MRS
   // Double_t  mCenterEta;     //center position Eta in MRS
   // Double_t  mCenterPhi;     //center position Phi in MRS
   Double_t  mEtaMin;        //minimum covered Eta in MRS
   Double_t  mEtaMax;        //maximum covered Eta in MRS
   Double_t  mPhiMin;        //minimum covered Phi in MRS
   Double_t  mPhiMax;        //maximum covered Phi in MRS
   // Bool_t    mMatrixUpdated; //is TNode::fRotMatrix updated

   static Bool_t   mDebug;   //!Control message printing of this class

 protected:
   StTofrGeomNode(const char* name, const char* title, TBRIK* brik,
                  const Double_t x, const Double_t y, const Double_t z,
                  TRotMatrix* matrix=0);
   void      UpdateMatrix();
   void      BuildMembers();

 public:
   StTofrGeomNode() {}
   ~StTofrGeomNode();

   static void     DebugOn()   { mDebug = kTRUE; }     
   static void     DebugOff()  { mDebug = kFALSE; }
   static Bool_t   IsDebugOn() { return mDebug; }

   static void     CalcMatrix(TNode* son, Double_t* trans, Double_t* rot,
                              StTofrGeomNode* mother=0);
   static void     ConvertPos(         TNode* from, const Double_t* pos_from,
                              StTofrGeomNode* to,         Double_t* pos_to);
   void            Local2Master(const Double_t* local, Double_t* master);
   void            Master2Local(const Double_t* master, Double_t* local);

   StThreeVectorD  YZPlaneNormal();
   StThreeVectorD  GetCenterPosition() const;
   // Double_t        GetCenterRxy() const { return mCenterRxy; }
   // Double_t        GetCenterEta() const { return mCenterEta; }
   // Double_t        GetCenterPhi() const { return mCenterPhi; }
   Double_t        GetEtaMin() const { return mEtaMin; }
   Double_t        GetEtaMax() const { return mEtaMax; }
   Double_t        GetPhiMin() const { return mPhiMin; }
   Double_t        GetPhiMax() const { return mPhiMax; }
   Bool_t          IsLocalPointIn(const Double_t x, const Double_t y,
                                  const Double_t z) const;
   Bool_t          IsGlobalPointIn(const StThreeVectorD &global);
   Bool_t          HelixCross(const StHelixD &helix,
                              Double_t &pathLen, StThreeVectorD &cross);
   virtual void    Print() const;

 C_l_assDef(StTofrGeomNode,2)  //Virutal TNode for TOFr geometry
};
#endif

class TVolumeView;

//////////////////////////////////////////////////////////////////////////////
//
// StTofrNode
// =========
//
//////////////////////////////////////////////////////////////////////////////

class StTofrNode : public TObject {
 protected:
  TVolumeView *fView;
  TVolumePosition  *pView;
  TVolumeView *mMasterNode;
   
   Double_t  mTransMRS[3];   //Translate vector in MRS
   Double_t  mRotMRS[9];     //RotateMatrix from MRS to this
   Bool_t    mTransFlag;     //Flag, kTRUE=if translation/matrix updated
   // Double_t  mCenterRxy;     //center position R(xy) in MRS
   // Double_t  mCenterEta;     //center position Eta in MRS
   // Double_t  mCenterPhi;     //center position Phi in MRS
   Double_t  mEtaMin;        //minimum covered Eta in MRS
   Double_t  mEtaMax;        //maximum covered Eta in MRS
   Double_t  mPhiMin;        //minimum covered Phi in MRS
   Double_t  mPhiMax;        //maximum covered Phi in MRS
   // Bool_t    mMatrixUpdated; //is TNode::fRotMatrix updated

   static Bool_t   mDebug;   //!Control message printing of this class

 protected:
   //   StTofrNode(const StTofrNode& tofnode);
    StTofrNode(TVolumeView *element, TVolumeView *top);

    StTofrNode& operator=(const StTofrNode&);

    void      UpdateMatrix();
    void      BuildMembers();

 public:
    StTofrNode() {}
   ~StTofrNode();

   TVolumeView*    GetfView() const { return fView; }
   TVolumePosition* GetpView() const { return pView; }
   TVolumeView*    GetTopNode() const { return mMasterNode; }
   static void     DebugOn()   { mDebug = kTRUE; }     
   static void     DebugOff()  { mDebug = kFALSE; }
   static Bool_t   IsDebugOn() { return mDebug; }

   static void     CalcMatrix(StTofrNode* son, Double_t* trans, Double_t* rot,
                              StTofrNode* mother=0);
   static void     ConvertPos(StTofrNode* from, const Double_t* pos_from,
                              StTofrNode* to,         Double_t* pos_to);
   void            Local2Master(const Double_t* local, Double_t* master);
   void            Master2Local(const Double_t* master, Double_t* local);
   TShape         *GetShape() const { return fView->GetPosition()->GetNode()->GetShape();}
   
   StThreeVectorD  YZPlaneNormal();
   StThreeVectorD  GetCenterPosition() const;
   // Double_t        GetCenterRxy() const { return mCenterRxy; }
   // Double_t        GetCenterEta() const { return mCenterEta; }
   // Double_t        GetCenterPhi() const { return mCenterPhi; }
   Double_t        GetEtaMin() const { return mEtaMin; }
   Double_t        GetEtaMax() const { return mEtaMax; }
   Double_t        GetPhiMin() const { return mPhiMin; }
   Double_t        GetPhiMax() const { return mPhiMax; }
   Bool_t          IsLocalPointIn(const Double_t x, const Double_t y,
                                  const Double_t z);
   Bool_t          IsGlobalPointIn(const StThreeVectorD &global);
   Bool_t          HelixCross(const StHelixD &helix,
                              Double_t &pathLen, StThreeVectorD &cross);
   virtual void    Print(const Option_t *opt="") const;

  ClassDef(StTofrNode,2)  //Virutal TNode for TOFr geometry
};


//////////////////////////////////////////////////////////////////////////////
//
// StTofrGeomTray
// ==============
//
//////////////////////////////////////////////////////////////////////////////

class StTofrGeomTray : public StTofrNode {
   friend class StTofrGeometry;

 private:
   Int_t           mTrayIndex;  //Tray Index number
   Int_t           mBTOHIndex;  // BTOH Index number
   Int_t       mSectorsInBTOH; //number of sectors in one half TOF

 protected:
   static Bool_t   mDebug;      //!Control message printing of this class

 protected:
   /*
   StTofrGeomTray(const char* name, const char* title, const TBRIK* brik,
         const Double_t x, const Double_t y, const Double_t z,
         const TRotMatrix* matrix, const Int_t itray);*/
   /*   StTofrGeomTray(const char* name, const char* title, TBRIK* brik,
         const  Double_t x, const Double_t y, const Double_t z,
         TRotMatrix* matrix, const Int_t itray);
   static void  PrepareCopyNode(TNode* node, StTofrGeomNode* top,
                    TShape*& shape, Double_t* pos, TRotMatrix*& newrot);
   static StTofrGeomTray* CopyNode(TNode* node, const Int_t itray);
   StTofrGeomSensor*      AddNode(TNode* node, const Int_t imodule);
   */

 public:
   StTofrGeomTray(const Int_t ibtoh, TVolumeView *sector, TVolumeView *top);
   StTofrGeomTray() {}
   ~StTofrGeomTray();

   StTofrGeomTray& operator=(const StTofrGeomTray&);
   static void       DebugOn()   { mDebug = kTRUE; }     
   static void       DebugOff()  { mDebug = kFALSE; }
   static Bool_t     IsDebugOn() { return mDebug; }

   Int_t             BTOHIndex() const { return mBTOHIndex; }
   Int_t             Index() const { return mTrayIndex; }
   StTofrGeomSensor* GetSensor(const Int_t imodule) const;
   virtual void      Print(const Option_t *opt="") const;

  ClassDef(StTofrGeomTray,1)  //Tray node in TOFr geometry
};


//////////////////////////////////////////////////////////////////////////////
//
// StTofrGeomSensor
// ================
//
//////////////////////////////////////////////////////////////////////////////

class StTofrGeomSensor : public StTofrNode {
   friend class StTofrGeomTray;

 private:
   Int_t               mModuleIndex;     //Module Index number
   static Int_t const  mCells  = 6;      //!Cells in one module
   Double_t            mCellY[mCells+1]; //Y Range of cells
   static Double_t const mSensorDy;// = 10.35;   // Actual module length;

 protected:
   static Bool_t       mDebug;           //!Control message printing of this class

 protected:
   /*
   StTofrGeomSensor(const char* name, const char* title, const TBRIK* brik,
         const Double_t x, const Double_t y, const Double_t z,
         const TRotMatrix* matrix, const Int_t imodule);*/
   /*   StTofrGeomSensor(const char* name, const char* title, TBRIK* brik,
         const Double_t x, const Double_t y, const Double_t z,
         TRotMatrix* matrix, const Int_t imodule);*/
   void CreateGeomCells();

 public:
   StTofrGeomSensor(TVolumeView *element, TVolumeView *top);

   StTofrGeomSensor() {}
   ~StTofrGeomSensor();

   StTofrGeomSensor& operator=(const StTofrGeomSensor&);
   static void       DebugOn()   { mDebug = kTRUE; }     
   static void       DebugOff()  { mDebug = kFALSE; }
   static Bool_t     IsDebugOn() { return mDebug; }

   static Int_t      GetCells()    { return mCells; }

   Int_t             Index() const { return mModuleIndex; }
   Double_t          GetCellYMin(const Int_t icell) const;
   Double_t          GetCellYMax(const Int_t icell) const;
   Int_t             FindCellIndex(const Double_t* local);
   Int_t             PrevCellIndex(const Int_t icell) const;
   Int_t             NextCellIndex(const Int_t icell) const;
   StThreeVectorD    GetCellPosition(const Int_t icell);
   virtual void      Print(Option_t *opt="") const ;

   ClassDef(StTofrGeomSensor,2)  //Module node in TOFr geometry
};

//_____________________________________________________________________________
inline Int_t StTofrGeomSensor::PrevCellIndex(const Int_t icell)
const
{
   Int_t ret = -1;
   if (icell>mCells) ret=mCells;
   else if (icell>0) ret=icell-1;
   return ret;
}

//_____________________________________________________________________________
inline Int_t StTofrGeomSensor::NextCellIndex(const Int_t icell)
const
{
   Int_t ret = -1;
   if (icell<0) ret=0;
   else if (icell<mCells) ret=icell+1;
   return ret;
}

//////////////////////////////////////////////////////////////////////////////
//
// StTofrGeometry
// ==============
//
//////////////////////////////////////////////////////////////////////////////

class StTofrGeometry : public TNamed {
 private:
   TNamed*    mGeoNamed;   //!Geometry to copy from
   static Int_t const mNTrays = 120;
   static Int_t const mNModules = 33;

 protected:
   //structure of btof_modr, btof_tray, etc, containing info in btofgeo.g
   //St_XDFFile* mXdf            //!pointer to the xdf file of tables
   TVolumeView*      mTopNode;       //top TNode as MRS
   const char* mRootFile;      //!the root file of geometry
   Int_t       mSectorsInBTOH; //number of sectors in one half TOF
   Int_t       mTrays;         //amount of TOFr trays
   Int_t       mModulesInTray; //number of modules in a tray
   Int_t       mCellsInModule; //number of cell in a module
   Bool_t      mInitFlag;      //flag of initialization, kTRUE if done
   TVolume*    mStarHall;
   Int_t       mTofrConf;      //configuration for tray/full (0/1) tofr

   StTofrGeomTray* mTofrTray[mNTrays];
   StTofrGeomSensor* mTofrSensor[mNTrays][mNModules];
   Int_t       mNValidTrays;
   Int_t       mNValidModules;

   static Int_t const mY03TrayIndex = 83;  //year03 run tray index

   static Bool_t   mDebug;     //!Control message printing of this class

   static char* const sectorPref ;//= "BSEC";
   static char* const trayPref   ;//= "BTRA";
   static char* const senPref    ;//= "BRMD";

 protected:
   //void        InitFromXdf(const char* xdffile);
   //void        Xdf2Geometry();
   //   Bool_t      InitFromRoot(const char* rootfile);
   //   Bool_t      CopyTopNode(TNode* top);

 public:
   StTofrGeometry(const char* name="tofrGeo",
                  const char* title="Simplified Tofr Geometry");
   ~StTofrGeometry();

   //   static TRotMatrix* CreateMatrix(const Double_t theta);
   //   static void        GetPrefixNodes(const TNode* topNode, const char* key, TList &list);

   Bool_t IsBSEC(const TVolume* element) const
     { return !(strcmp(element->GetName(), sectorPref)); }
   Bool_t IsBTRA(const TVolume* element) const
     { return !(strcmp(element->GetName(), trayPref)); }
   Bool_t IsBRMD(const TVolume* element) const
     { return !(strcmp(element->GetName(), senPref)); }

   Bool_t ContainOthers(TVolume *element);

   static Bool_t      LackThis(const char* fromWhere);

   static void   DebugOn()   { mDebug = kTRUE; }     
   static void   DebugOff()  { mDebug = kFALSE; }
   static Bool_t IsDebugOn() { return mDebug; }

   //   void          Init(const char* file, Option_t* option="root");
   //   void          Init(TVolume *starHall, const Int_t TofrConf=0);
   //   void          InitFromStar(TVolume *starHall, const Int_t TofrConf=0);
   void          Init(TVolume *starHall);
   void          InitFromStar(TVolume *starHall);
   //   void          InitDaqMap();

   Bool_t  IsInitDone() const { return mInitFlag; }
   Bool_t  IsCellValid(const Int_t icell)     const;
   Bool_t  IsSensorValid(const Int_t imodule) const;
   Bool_t  IsTrayValid(const Int_t itray)     const;

   Int_t   CalcCellId(const Int_t volumeId, const Double_t* local) const;
   Int_t   CalcCellId(const Int_t volumeId, const Float_t* local)  const;
   Int_t   CalcSensorId(const Int_t imodule, const Int_t itray=0)  const;
   Int_t   PrevCellId(const Int_t cellId) const;
   Int_t   NextCellId(const Int_t cellId)  const;
   // Int_t   CalcCellId(const Int_t icell,
   //                   const StTofrGeomSensor* sensor)              const;
   Int_t   CalcCellId(const Int_t icell, const Int_t imodule,
                                         const Int_t itray=0)      const;
   void    DecodeVolumeId(const Int_t volumeId,
                                Int_t &imodule, Int_t &itray)      const;
   Bool_t  DecodeSensorId(const Int_t sensorId, Int_t &imodule,
                                                Int_t &itray)      const;
   Bool_t  DecodeCellId(const Int_t cellId,   Int_t &icell,
                              Int_t &imodule, Int_t &itray)        const;
   Int_t   GetCellIndex(const Int_t cellId)                        const;

   Int_t   CellsInModule(const Int_t imodule=0, const Int_t itray=0) const
                         { return StTofrGeomSensor::GetCells(); }
   Int_t   ModulesInTray(const Int_t itray=0) const
                         { return mModulesInTray; }
   Int_t   Trays() const { return mTrays; }

   const char* GeoRootFile() { return mRootFile; }
   virtual void      Print(Option_t *opt="") const ;

   TVolumeView*      GetTopNode() const { return mTopNode; }
   StTofrGeomSensor* GetGeomCell(const Int_t cellId)    const;
   StTofrGeomSensor* GetGeomSensor(const Int_t imodule,
                                   const Int_t itray=0) const;
   StTofrGeomTray*   GetGeomTray(const Int_t itray=0)   const;
   StTofrGeomTray*   GetGeomTrayAt(const Int_t idx=0)   const;
   Int_t             GetAtOfTray(const Int_t itray=0)   const;

   Int_t             CellIdPointIn(const StThreeVectorD& point) const;
#ifndef __CINT__
   Bool_t            HelixCrossCellIds(const StHelixD &helix, IntVec &idVec,
				       DoubleVec &pathVec, PointVec &crossVec) const;
   Bool_t            HelixCrossCellIds(const StHelixD &helix, IntVec validModuleVec, IntVec projTrayVec, IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec) const;
   Bool_t            HelixCross(const StHelixD &helix) const;
   Bool_t            HelixCross(const StHelixD &helix, IntVec validModuleVec, IntVec projTrayVec) const;
   Bool_t            projTrayVector(const StHelixD &helix, IntVec &trayVec) const;
#endif
  ClassDef(StTofrGeometry,2)  //Simplified TOFr Geometry
};

R__EXTERN  StTofrGeometry* gTofrGeometry;

#endif  //end of STTOFRGEOMETRY_H

/*******************************************************************
 * $Log: StTofrGeometry.h,v $
 * Revision 1.10  2018/02/26 23:27:02  smirnovd
 * Remove unnecessary guards around ClassDef macro
 *
 * Revision 1.9  2018/02/26 23:13:21  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.8  2008/03/27 00:15:39  dongx
 *  Update for Run8 finished.
 *
 * Revision 1.7  2005/07/07 01:22:28  fisyak
 * Hide typedefs IntVec, DoubleVec, PointVec and methods HelixCrossCellIds,HelixCross, projTrayVector from CINT
 *
 * Revision 1.6  2005/07/06 19:24:59  fisyak
 * Use templated StThreeVector
 *
 * Revision 1.5  2004/05/03 23:07:49  dongx
 * -Introduce data members to save the Tray and Sensor geometries in the initialization.
 * -Optimize the HelixCrossCellIds() function to save CPU time
 * -Introduce a new function projTrayVector()
 * -Update the //classDef number 1->2
 *
 *
 * Revision 1.3  2004/03/09 16:45:16  dongx
 * Remove InitDaqMap() since a StTofrDaqMap is introduced
 *
 * Revision 1.2  2003/09/11 05:49:23  perev
 * ansi corrs
 *
 * Revision 1.1  2003/08/06 23:00:53  geurts
 * First Release
 */
