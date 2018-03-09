/*******************************************************************
 *
 * $Id: StBTofGeometry.h,v 1.25 2018/03/09 21:36:17 smirnovd Exp $
 * 
 * Authors: Shuwei Ye, Xin Dong
 *******************************************************************
 *
 * Description: Collection of geometry classes for the TOF-MRPC
 *              initializes from GEANT geometry
 *
 *
 *******************************************************************/
#ifndef STBTOFGEOMETRY_H
#define STBTOFGEOMETRY_H

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// group of classes for BTof Geometry:                                  //
//                                                                      //
//    StBTofGeometry, StBTofNode,                                       //
//    StBTofGeomTray, StBTofGeomSensor                                  //
//                                                                      //
// Usage:                                                               //
//   StBTofGeometry* geo = new StBTofGeometry("tof","tof geometry");    //
//     geo->Init(TVolume *starHall, const Int_t BTofConf);              //
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
#include "StMaker.h"

#include <vector>
#include <string>
typedef std::vector<Int_t>  IntVec;
typedef std::vector<Double_t>  DoubleVec;
typedef std::vector<StThreeVector<double> > PointVec;
class StBTofNode;
class StBTofGeomNode;
class StBTofGeomTray;
class StBTofGeomSensor;
class StBTofGeometry;

class TVolumeView;
class TGeoPhysicalNode;
class TGeoManager;

/**
   \class StBTofNode
   Basic TOF geometry class
 */ 
class StBTofNode : public TObject {
 protected:
  TVolumeView *fView;
  TVolumePosition  *pView;
  TVolumeView *mMasterNode;
   
   Double_t  mTransMRS[3];   //Translate vector in MRS
   Double_t  mRotMRS[9];     //RotateMatrix from MRS to this
   Bool_t    mTransFlag;     //Flag, kTRUE=if translation/matrix updated
//   Double_t  mCenterRxy;     //center position R(xy) in MRS
//   Double_t  mCenterEta;     //center position Eta in MRS
//   Double_t  mCenterPhi;     //center position Phi in MRS
   Double_t  mEtaMin;        //minimum covered Eta in MRS
   Double_t  mEtaMax;        //maximum covered Eta in MRS
   Double_t  mPhiMin;        //minimum covered Phi in MRS
   Double_t  mPhiMax;        //maximum covered Phi in MRS
   // Bool_t    mMatrixUpdated; //is TNode::fRotMatrix updated
   Double_t  mAlign[3];      //! Alignment parameters

   static Bool_t   mDebug;   //!Control message printing of this class

 protected:
//    StBTofNode(TVolumeView *element, TVolumeView *top);
    StBTofNode(TVolumeView *element, TVolumeView *top, const StThreeVectorD& align, TVolumePosition *pos=0);

    StBTofNode(const TGeoPhysicalNode& gpNode, const StThreeVectorD& align);
    

    void      UpdateMatrix();
    void      BuildMembers();

 public:
    StBTofNode() {}
   ~StBTofNode() {
     if ( TestBit(kIsOwner) ) {
       delete fView;
       delete pView;
       delete mMasterNode;
       delete mTVolume;
       delete mTShape;
     }
   }

   TVolumeView*    GetfView() const { return fView; }
   TVolumePosition* GetpView() const { return pView; }
   TVolumeView*    GetTopNode() const { return mMasterNode; }
   static void     DebugOn()   { mDebug = kTRUE; }     
   static void     DebugOff()  { mDebug = kFALSE; }
   static Bool_t   IsDebugOn() { return mDebug; }

   static void     CalcMatrix(StBTofNode* son, Double_t* align, Double_t* trans, Double_t* rot,
                              StBTofNode* mother=0);
   static void     ConvertPos(StBTofNode* from, const Double_t* pos_from,
                              StBTofNode* to,         Double_t* pos_to);
   void            Local2Master(const Double_t* local, Double_t* master);
   void            Master2Local(const Double_t* master, Double_t* local);
   TShape         *GetShape() const { return fView->GetPosition()->GetNode()->GetShape();}
   
   StThreeVectorD  YZPlaneNormal();
   StThreeVectorD  GetCenterPosition() const;
//   Double_t        GetCenterRxy() const { return mCenterRxy; }
//   Double_t        GetCenterEta() const { return mCenterEta; }
//   Double_t        GetCenterPhi() const { return mCenterPhi; }
   Double_t        GetEtaMin() const { return mEtaMin; }
   Double_t        GetEtaMax() const { return mEtaMax; }
   Double_t        GetPhiMin() const { return mPhiMin; }
   Double_t        GetPhiMax() const { return mPhiMax; }
   Bool_t          IsLocalPointIn(const Double_t x, const Double_t y,
                                  const Double_t z);
   Bool_t          IsGlobalPointIn(const StThreeVectorD &global);
   Bool_t          HelixCross(const StHelixD &helix, Double_t &pathLen, StThreeVectorD &cross);
   Bool_t          HelixCross(const StHelixD &helix, Double_t &pathLen, StThreeVectorD &cross, Double_t &theta);
   StThreeVectorD* Align() const {return new StThreeVectorD(mAlign[0], mAlign[1], mAlign[2]); }
   virtual void    Print(const Option_t *opt="") const;

 private:

   enum EBTofNodeBits {
      /// True when this node creates TVolume objects that needs to be deleted and
      /// pointed to by fView, pView, and mMasterNode
      kIsOwner = BIT(23)
   };

   /// A transient TVolume and TShape objects may need to be created for this
   /// StBTofNode when the input geometry is not given by a TVolume hierarchy
   /// @{
   TVolume *mTVolume;  //!
   TShape  *mTShape;   //!
   /// @}

  ClassDef(StBTofNode,2)  //Virutal TNode for TOF geometry
};


//////////////////////////////////////////////////////////////////////////////
//
// StBTofGeomTray
// ==============
//
//////////////////////////////////////////////////////////////////////////////

class StBTofGeomTray : public StBTofNode {
   friend class StBTofGeometry;

 private:
   Int_t           mTrayIndex;  //Tray Index number
   Int_t           mBTOHIndex;  // BTOH Index number
   Int_t       mSectorsInBTOH; //number of sectors in one half TOF

 protected:
   static Bool_t   mDebug;      //!Control message printing of this class

 public:
//   StBTofGeomTray(const Int_t ibtoh, TVolumeView *sector, TVolumeView *top);

   StBTofGeomTray(const Int_t ibtoh, TVolumeView *sector, TVolumeView *top, const StThreeVectorD& align, TVolumePosition *pos=0);

   StBTofGeomTray(const int trayId, const TGeoPhysicalNode& gpNode, const StThreeVectorD& align);

   StBTofGeomTray() {}
   ~StBTofGeomTray() {};

   static void       DebugOn()   { mDebug = kTRUE; }     
   static void       DebugOff()  { mDebug = kFALSE; }
   static Bool_t     IsDebugOn() { return mDebug; }

   Int_t             BTOHIndex() const { return mBTOHIndex; }
   Int_t             Index() const { return mTrayIndex; }
   virtual void      Print(const Option_t *opt="") const;

  ClassDef(StBTofGeomTray,1)  //Tray node in TOF geometry
};


//////////////////////////////////////////////////////////////////////////////
//
// StBTofGeomSensor
// ================
//
//////////////////////////////////////////////////////////////////////////////

class StBTofGeomSensor : public StBTofNode {
   friend class StBTofGeomTray;

 private:
   Int_t               mModuleIndex;     //Module Index number
   static Int_t const  mCells  = 6;      //!Cells in one module
   Double_t            mCellY[mCells+1]; //Y Range of cells
   static Double_t const mSensorDy;// = 10.35;   // Actual module length;

 protected:
   static Bool_t       mDebug;           //!Control message printing of this class

 protected:
   void CreateGeomCells();

 public:
//   StBTofGeomSensor(TVolumeView *element, TVolumeView *top);
   StBTofGeomSensor(TVolumeView *element, TVolumeView *top, const StThreeVectorD& align, TVolumePosition *pos=0);

   StBTofGeomSensor(const int moduleId, const TGeoPhysicalNode& gpNode, const StThreeVectorD& align);

   StBTofGeomSensor() {}
   ~StBTofGeomSensor() {}

   static void       DebugOn()   { mDebug = kTRUE; }     
   static void       DebugOff()  { mDebug = kFALSE; }
   static Bool_t     IsDebugOn() { return mDebug; }

   static Int_t      GetCells()    { return mCells; }
   void              SetIndex(Int_t imod);
   Int_t             Index() const { return mModuleIndex; }
   Double_t          GetCellYMin(const Int_t icell) const;
   Double_t          GetCellYMax(const Int_t icell) const;
   Int_t             FindCellIndex(const Double_t* local);
   Int_t             PrevCellIndex(const Int_t icell) const;
   Int_t             NextCellIndex(const Int_t icell) const;
   StThreeVectorD    GetCellPosition(const Int_t icell);
   virtual void      Print(Option_t *opt="") const ;

   ClassDef(StBTofGeomSensor,1)  //Module node in TOF geometry
};
//____________________________________________________________________________
inline void StBTofGeomSensor::SetIndex(Int_t imod){ mModuleIndex = imod;}

//_____________________________________________________________________________
inline Int_t StBTofGeomSensor::PrevCellIndex(const Int_t icell)
const
{
   Int_t ret = -1;
   if (icell>mCells) ret=mCells;
   else if (icell>0) ret=icell-1;
   return ret;
}

//_____________________________________________________________________________
inline Int_t StBTofGeomSensor::NextCellIndex(const Int_t icell)
const
{
   Int_t ret = -1;
   if (icell<0) ret=0;
   else if (icell<mCells) ret=icell+1;
   return ret;
}

//////////////////////////////////////////////////////////////////////////////
//
// StBTofGeometry
// ==============
//
//////////////////////////////////////////////////////////////////////////////

class StBTofGeometry : public TNamed {

   friend class StBTofGeomTray;

 private:
   TNamed*    mGeoNamed;   //!Geometry to copy from
   static Int_t const mNTrays = 120;
   static Int_t const mNModules = 32;

   std::string FormTGeoPath(TGeoManager &geoManager, int trayId, bool hasGmt = false, int moduleId = -1);

   static bool TrayHasGmtModules(int trayId)
   {
      return trayId == 8 || trayId == 23 || trayId == 93 || trayId == 108;
   }

   void InitFrom(TVolume &starHall);

   /// Initializes mBTofTray and mBTofSensor arrays
   void InitFrom(TGeoManager &geoManager);

 protected:
   TVolumeView*      mTopNode;       //top TNode as MRS
   const char* mRootFile;      //!the root file of geometry
   Int_t       mSectorsInBTOH; //number of sectors in one half TOF
   Int_t       mNValidTrays;   //amount of TOF trays
   Int_t       mModulesInTray; //number of modules in a tray
   Int_t       mCellsInModule; //number of cell in a module
   Bool_t      mInitFlag;      //flag of initialization, kTRUE if done
   Int_t       mBTofConf;      //configuration for tray/full (0/1) tof

   StBTofGeomTray* mBTofTray[mNTrays];
   StBTofGeomSensor* mBTofSensor[mNTrays][mNModules];

   Bool_t          mIsMC;      //!Control MC input (ignore alignment corrections)
   static Bool_t   mDebug;     //!Control message printing of this class
   
   string  mAlignFile;  //! filename for alignment input

   static const char* sectorPref ;//= "BSEC";
   static const char* trayPref   ;//= "BTRA";
   static const char* senPref    ;//= "BRMD";
   
   /// Alignment parameters
   Double_t    mTrayX0[mNTrays];
   Double_t    mTrayY0[mNTrays];
   Double_t    mTrayZ0[mNTrays];

 public:
   StBTofGeometry(const char* name="btofGeo",
                  const char* title="Simplified BTof Geometry");
   ~StBTofGeometry();

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
   void          SetMCOn()   { mIsMC = kTRUE; }
   void          SetMCOff()  { mIsMC = kFALSE; }

   void          SetAlignFile(const Char_t *infile="") { mAlignFile = infile; }

   void          Init(StMaker *maker, TVolume *starHall, TGeoManager* geoManager = nullptr);

   Bool_t  IsInitDone() const { return mInitFlag; }
   Bool_t  IsCellValid(const Int_t icell)     const;
   Bool_t  IsSensorValid(const Int_t imodule) const;
   Bool_t  IsTrayValid(const Int_t itray)     const;

   Int_t   CalcCellId(const Int_t volumeId, const Double_t* local) const;
   Int_t   CalcCellId(const Int_t volumeId, const Float_t* local)  const;
   Int_t   CalcSensorId(const Int_t imodule, const Int_t itray=0)  const;
   Int_t   PrevCellId(const Int_t cellId) const;
   Int_t   NextCellId(const Int_t cellId)  const;
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
                         { return StBTofGeomSensor::GetCells(); }
   Int_t   ModulesInTray(const Int_t itray=0) const
                         { return mModulesInTray; }
   Int_t   Trays() const { return mNValidTrays; }

   const char* GeoRootFile() { return mRootFile; }
   virtual void      Print(Option_t *opt="") const ;

   TVolumeView*      GetTopNode() const { return mTopNode; }
   StBTofGeomSensor* GetGeomCell(const Int_t cellId)    const;
   StBTofGeomSensor* GetGeomSensor(const Int_t imodule,
                                   const Int_t itray=0) const;
   StBTofGeomTray*   GetGeomTray(const Int_t itray=0)   const;
   StBTofGeomTray*   GetGeomTrayAt(const Int_t idx=0)   const;
   Int_t             GetTrayIndexAt(const Int_t idx=0)  const;
   Int_t             GetAtOfTray(const Int_t itray=0)   const;

   Int_t             CellIdPointIn(const StThreeVectorD& point) const;
#ifndef __CINT__
   Bool_t            HelixCrossCellIds(const StHelixD &helix, IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec) const;
   Bool_t            HelixCrossCellIds(const StHelixD &helix, IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec, DoubleVec &thetaVec) const;
   Bool_t            HelixCrossCellIds(const StHelixD &helix, IntVec validModuleVec, IntVec projTrayVec, IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec) const;
   Bool_t            HelixCross(const StHelixD &helix) const;
   Bool_t            HelixCross(const StHelixD &helix, IntVec validModuleVec, IntVec projTrayVec) const;
   Bool_t            projTrayVector(const StHelixD &helix, IntVec &trayVec) const;
#endif
  ClassDef(StBTofGeometry,2)  //Simplified TOF Geometry
};

R__EXTERN  StBTofGeometry* gBTofGeometry;

#endif  //end of STBTOFGEOMETRY_H

/*******************************************************************
 * $Log: StBTofGeometry.h,v $
 * Revision 1.25  2018/03/09 21:36:17  smirnovd
 * Remove declared but undefined function
 *
 * Revision 1.24  2018/02/26 23:29:00  smirnovd
 * StBTofGeometry: Introduced alternative initialization using TGeo geometry
 *
 * Revision 1.23  2018/02/26 23:28:53  smirnovd
 * StBTofGeometry: Added private InitFrom(TGeoManager)
 *
 * Revision 1.22  2018/02/26 23:28:45  smirnovd
 * StBTofGeometry: InitFrom(TVolume*) to InitFrom(TVolume&)
 *
 * Revision 1.21  2018/02/26 23:28:38  smirnovd
 * StBTofGeometry: s/InitFromStar/InitFrom/ and make it private
 *
 * Revision 1.20  2018/02/26 23:28:30  smirnovd
 * StBTofGeometry: Added static method to identify trays with GMT modules
 *
 * Revision 1.19  2018/02/26 23:28:22  smirnovd
 * StBTofGeometry: New method to form TGeo paths for trays and modules
 *
 * Revision 1.18  2018/02/26 23:28:14  smirnovd
 * StBTofGeomSensor: New constructor accepting TGeo
 *
 * Revision 1.17  2018/02/26 23:28:07  smirnovd
 * StBTofGeoTray: New constructor accepting TGeo
 *
 * Revision 1.16  2018/02/26 23:28:00  smirnovd
 * StBTofNode: New constructor accepting TGeo volume
 *
 * The new TGeo constructor creates transient TVolume objects to provide
 * functionality compatible with the existing TVolume-base geometry
 * transformations. Unlike previously, the TVolume objects are owned by this class
 * and so have to be deleted.
 *
 * Revision 1.15  2018/02/26 23:27:53  smirnovd
 * Accept reference instead of pointer to xyz alignment
 *
 * Revision 1.14  2018/02/26 23:27:45  smirnovd
 * StBTofGeometry: Senseless assignments in destructors
 *
 * Revision 1.13  2018/02/26 23:27:15  smirnovd
 * StBTofGeometry: Removed unused member pointer to non-TGeo ROOT geometry
 *
 * Revision 1.12  2018/02/26 23:27:01  smirnovd
 * Remove unnecessary guards around ClassDef macro
 *
 * Revision 1.11  2018/02/26 23:13:19  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.10  2017/10/20 17:50:33  smirnovd
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
 * Revision 1.9  2014/02/06 21:21:13  geurts
 * Fix Index() of modules in GEMTOF trays, only applies to Run 13+ geometries [Joey Butterworth]
 *
 * Revision 1.8  2011/07/27 16:15:12  geurts
 * Alignment calibration modifications [Patrick Huck]:
 *  - added mAlignFile and SetAlignFile for use in StBTofMatchMaker
 *  - phi0, x0, z0 made mNTrays dependent
 *
 * Revision 1.7  2010/08/09 18:45:36  geurts
 * Include methods in StBTofNode and StBTofGeometry that calculate local theta [Masa]
 *
 * Revision 1.6  2010/07/14 20:35:28  geurts
 * introduce switch to enable ideal MC geometry, without alignment updates. Default: disabled
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
