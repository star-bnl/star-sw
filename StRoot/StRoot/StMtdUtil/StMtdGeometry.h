/********************************************************************
 * $Id: StMtdGeometry.h,v 1.13 2017/03/08 20:40:38 marr Exp $
 ********************************************************************
 *
 * $Log: StMtdGeometry.h,v $
 * Revision 1.13  2017/03/08 20:40:38  marr
 * Add back the old implementation of GetCellLocalYCenter() function to make
 * the class backward compatible.
 *
 * Revision 1.12  2017/02/13 02:56:11  marr
 * From 2017, do not move BL 8&24 along y direction by hand since this is already
 * done in the geometry file. Calibration, production and analysis should use
 * the new version consistently.
 *
 * Revision 1.11  2016/08/05 16:12:34  marr
 * Add MTD hit IdTruth to avoid applying dy shift for BL 8 and 24 for MC hits
 *
 * Revision 1.10  2015/07/29 01:11:25  smirnovd
 * Initialize static constants outside of class definition
 *
 * C++ forbids initialization of non-integral static const members within the class
 * definition. The syntax is allowed only for integral type variables.
 *
 * Revision 1.9  2015/07/24 15:56:05  marr
 * 1. Remove calling a macro in Init() to create geometry. It should be done within
 * the maker that uses this utility class.
 * 2. Add the TGeoManager parameter to the default constructor to force the existance
 * of the gometry when using this utility class.
 * 3. Simplify the code for getting the pointer to the magnetic field
 *
 * Revision 1.8  2015/05/01 01:55:34  marr
 * Fix the geometry of shifted backleg 8 and 24
 *
 * Revision 1.7  2015/04/07 16:23:33  marr
 * 1. Make use the constants defined in StMtdConstants.h
 * 2. Cleaning up
 *
 * Revision 1.6  2014/08/06 11:43:28  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.5  2014/07/16 15:31:01  huangbc
 * Add an option to lock bfield to FF.
 *
 * Revision 1.4  2014/07/10 20:45:13  huangbc
 * New geometry class for MTD, load geometry from geant geometry. Need gGeoManager.
 *
 * Revision 1.3  2013/08/07 18:27:01  geurts
 * - updated strip gap dimension from old to current MRPC design [Bingchu]
 * - include CVS Id and Log tags
 *
 *
 *******************************************************************/
#ifndef StMtdGeometry_HH
#define StMtdGeometry_HH

#include "TMath.h"
#include "TNamed.h"
#include "TObject.h"
#include "TF1.h"
#include "TGeoManager.h"
#include "TGeoNode.h"
#include "TGeoMatrix.h"
#include "TGeoShape.h"
#include "StThreeVectorD.hh"
#include "StMaker.h"
#include "StPhysicalHelixD.hh"
#include "StarMagField.h"
#include "StMtdConstants.h"

#include <vector>
#include <string>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::sort;
using std::string;
#endif
#ifndef __CINT__
#if !defined(ST_NO_TEMPLATE_DEF_ARGS)
typedef vector<Int_t>  IntVec;
typedef vector<Double_t>  DoubleVec;
typedef vector<StThreeVectorD > PointVec;
#else
typedef vector<Int_t, allocator<Int_t> >  IntVec;
typedef vector<Double_t, allocator<Double_t> >  DoubleVec;
typedef vector<StThreeVectorD, allocator<StThreeVectorD>> PointVec;
#endif
#endif

class TGeoVolume;
class StMaker;
class StMtdGeometry;

const double muonMass= 0.105658389;


//----------------------------------------------------//
//													  //
//					StMtdGeoNode					  //
//													  //
//----------------------------------------------------//
class StMtdGeoNode : public TObject {

public:
  StMtdGeoNode(TGeoVolume *vol, TGeoHMatrix *mat, StThreeVectorD point, Int_t nExtraCells);
 StMtdGeoNode():fTransFlag(0),fNExtraCells(0){}
  virtual ~StMtdGeoNode();

  void            LocalToMaster(const Double_t* local, Double_t* master);
  void            MasterToLocal(const Double_t* master, Double_t* local);
  void  	  UpdateMatrix();
  StThreeVectorD  YZPlaneNormal(); 
  void   	  PrintNormal(); 
  void   	  SetNExtraCells(Int_t val)  { fNExtraCells = val>0?val:0; }
  
  Bool_t	  HelixCross(const StPhysicalHelixD helix, const Double_t pathToMagOutR, const Double_t tofToMagOutR, Double_t &pathL, Double_t &tof, StThreeVectorD &cross);
  Bool_t 	  IsGlobalPointIn(StThreeVectorD &global);
  Bool_t 	  IsLocalPointIn(const Double_t x, const Double_t y, const Double_t z);
  StThreeVectorD  GetNodePoint();

 protected:
  TGeoVolume 	  *fVolume;
  TGeoHMatrix	  *fMatrix;
  StThreeVectorD  fPoint;
  StThreeVectorD  fNormal;
  
  const Double_t  *fTransMRS;   //Translate vector in MRS
  const Double_t  *fRotMRS;     //RotateMatrix from MRS to this
  Bool_t    	  fTransFlag;   //Flag, kTRUE=if translation/matrix updated	
  Int_t		  fNExtraCells; //Number of extra cells considered in matching
};

//_____________________________________________________________________________
inline void StMtdGeoNode::PrintNormal()
{
  LOG_INFO<<"normal x,y,z = "
	  <<fNormal[0]<<", "
	  <<fNormal[1]<<", "
	  <<fNormal[2]<<endm;
}


//----------------------------------------------------//
//													  //
//					StMtdGeoBackleg					  //
//													  //
//----------------------------------------------------//

class StMtdGeoBackleg : public StMtdGeoNode {
  //friend class StMtdGeometry;
 public:
  StMtdGeoBackleg (Int_t iMTTG, Int_t iBL, TGeoVolume *vol, TGeoHMatrix *mat, StThreeVectorD point, Int_t nExtraCells);

  StMtdGeoBackleg() {}
  ~StMtdGeoBackleg();

 private:
  Int_t	  mMTTGIndex;
  Int_t	  mBacklegIndex;

};

//----------------------------------------------------//
//													  //
//					StMtdGeoModule					  //
//													  //
//----------------------------------------------------//

class StMtdGeoModule : public StMtdGeoNode {
  //friend class StMtdGeoBackleg;
 public:
  StMtdGeoModule (Int_t iMTRA, Int_t iMod, TGeoVolume *vol, TGeoHMatrix *mat, StThreeVectorD point, Int_t nExtraCells);
  StMtdGeoModule() {}
  ~StMtdGeoModule();
  Int_t	       FindCellId(const Double_t *local);
  Float_t      GetCellPhiCenter(Int_t iCell);
  Float_t      GetCellZCenter(Int_t iCell);
  Float_t      GetCellLocalYCenter(Int_t iCell, Int_t iBL, Int_t idTruth);
  Float_t      GetCellLocalYCenter(Int_t iCell);

 private:
  Int_t	       mMTRAIndex;
  Int_t	       mModuleIndex;
};

//----------------------------------------------------//
//													  //
//					StMtdGeometry					  //
//													  //
//----------------------------------------------------//

class StMtdGeometry : public TNamed{
 private:
  static const Double_t mMtdMinR; /// mtd system minimum radius
  static const Double_t mMtdMaxR; /// mtd system maximum radius
  static const Double_t mMagInR;  /// magnet system inner radius
  static const Double_t mMagOutR; /// magnet system outer radius
  static const Double_t mEmcInR;  /// EMC system inner radius
  static const Double_t mEmcOutR; /// EMC system outer radius

 public:
  StMtdGeometry(const char* name="mtdGeo",
		const char* title="Simplified Mtd Geometry",
		TGeoManager *manager = 0);
  ~StMtdGeometry();
  
  void	 Init(StMaker *maker);
   
  void   DebugOn()   { mDebug = kTRUE; }     
  void   DebugOff()  { mDebug = kFALSE; }
  Bool_t IsDebugOn() { return mDebug; }

  void   SetNExtraCells(Int_t val)  { mNExtraCells = val>0?val:0; }
  Bool_t ProjToMagOutR(const StPhysicalHelixD helix, const StThreeVectorD vertex, StPhysicalHelixD &outHelix, Double_t &pathL, Double_t &tof, StThreeVectorD &pos);
  void   ProjToVertex(const StPhysicalHelixD helix, const StThreeVectorD vertex, Double_t &pathL, Double_t &tof, StThreeVectorD &dcaPos);
#ifndef __CINT__
  Bool_t ProjToBLModVect(const StPhysicalHelixD helix, IntVec &blVect, IntVec &modVect);
  Bool_t HelixCrossCellIds(const StPhysicalHelixD helix, const StThreeVectorD vertex, IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec, DoubleVec &tofVec );
  Bool_t HelixCrossCellIds(const StPhysicalHelixD helix, IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec, DoubleVec &tofVec );
#endif

  Int_t	 CalcCellId(Int_t iBL, Int_t iMod, Int_t iCel); /// return BL*1000+Module*100+(Cell+50). BL: 1-30, Module: 1-5, Cell: 0-11.
  Bool_t IsIdValid(Int_t id);
  Int_t  FindBLId(Double_t phi);
  Int_t  FindModId(Double_t z);
  void   SetBFactor(Float_t val){mBFactor=val;}
  void   SetELossFlag(Bool_t val){mELossFlag=val;}
  void   SetCosmicFlag(Bool_t val){mCosmicFlag=val;}
  void   SetLockBField(Bool_t val);
  void   DecodeCellId(Int_t id, Int_t &iBL, Int_t &iMod, Int_t &iCell);
  StThreeVectorD GetField(StThreeVectorD pos) const; /// tesla
  StThreeVectorD GetField(Double_t x, Double_t y, Double_t z) const;
  Double_t 	 GetFieldZ(StThreeVectorD pos) const;
  Double_t 	 GetFieldZ(Double_t x, Double_t y, Double_t z) const;
  StMtdGeoModule *GetGeoModule(Int_t iBL, Int_t iMod) const;

 protected:
  Bool_t   mDebug;        //!Control message printing of this class
  Bool_t   mCosmicFlag;   //!Cosmic event flag
  Bool_t   mELossFlag;    //!Control energy loss flag
  Bool_t   mLockBField;    //!Control mag field to FF 
  Int_t    mNExtraCells;  //!Control matching range in the module. 
  Int_t    mNValidBLs;
  Int_t    mMTTG2BL[gMtdNBacklegs];
  Int_t    mMTRA2Mod[gMtdNBacklegs][gMtdNModules]; //BL, Module index all start from 1
  Float_t  mBFactor;

  StMtdGeoBackleg *mMtdGeoBackleg[gMtdNBacklegs];
  StMtdGeoModule  *mMtdGeoModule[gMtdNBacklegs][gMtdNModules];
  StarMagField    *mStarBField;
  TF1             *fMagEloss;
   
  static const char* backlegPref[4];//= "MTMT,MTMF,MTTG,MTT1";
  static const char* modulePref  ;//= "MTRA";
  TGeoManager *mGeoManager;

  Bool_t 		IsMTTG(const TGeoVolume * vol) const;
  Bool_t 		IsMTRA(const TGeoVolume * vol) const { return !(strcmp(vol->GetName(), modulePref));}
  
#ifndef __CINT__
  void   		RemoveDuplicate(IntVec &vec);
#endif

  const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StMtdGeometry.h,v 1.13 2017/03/08 20:40:38 marr Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  ClassDef(StMtdGeometry,1);
};

R__EXTERN  StMtdGeometry* gMtdGeometry;

#endif
