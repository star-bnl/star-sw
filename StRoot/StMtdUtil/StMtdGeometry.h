/********************************************************************
 * $Id: StMtdGeometry.h,v 1.6 2014/08/06 11:43:28 jeromel Exp $
 ********************************************************************
 *
 * $Log: StMtdGeometry.h,v $
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
#include "TGeoNode.h"
#include "TGeoMatrix.h"
#include "TGeoShape.h"
#include "StThreeVectorD.hh"
#include "StMaker.h"
#include "StPhysicalHelixD.hh"
#include "StarMagField.h"

//class StMtdGeoBackleg;
//class StMtdGeoModule;

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
//typedef vector<Int_t>  IntVec;
//typedef vector<Double_t>  DoubleVec;
//typedef vector<StThreeVectorD > PointVec;
#endif

class TGeoVolume;

class StMaker;
class StMtdGeometry;
//class StMtdGeoNode;

const double muonMass= 0.105658389;
//------------------------//
// Geometry constant
//------------------------//
//const double backLegPhiWidth = 8.*(TMath::Pi())/180.;   //rad,  8 degree per backLeg
//const double backLegPhiGap   = 4.*(TMath::Pi())/180.;   //rad,  4 degree 
//const double mFirstBackLegPhi = 90.*(TMath::Pi())/180.; // rad, No.1 backLeg located at phi=90 degree, id from 1-30.
const double vDrift = 56.;                              // ps/cm drifting velocity of electronic signal
//const double mCellWidth = 3.8; //cm
//const double mCellGap = 0.6; //cm

//----------------------------------------------------//
//													  //
//					StMtdGeoNode					  //
//													  //
//----------------------------------------------------//
class StMtdGeoNode : public TObject {
 protected:
	 //TGeoNode 		*fNode;
	 TGeoVolume 		*fVolume;
	 TGeoHMatrix		*fMatrix;
	 StThreeVectorD 	fPoint;
	 StThreeVectorD 	fNormal;

   	 const Double_t  	*fTransMRS;   //Translate vector in MRS
   	 const Double_t  	*fRotMRS;     //RotateMatrix from MRS to this
   	 Bool_t    			fTransFlag;     //Flag, kTRUE=if translation/matrix updated
	 static const Double_t	fCellWidth = 3.8; /// cm	
	 static const Double_t	fCellGap = 0.6; /// cm	
	 Int_t				fNExtraCells;


 public:
	StMtdGeoNode(TGeoVolume *vol, TGeoHMatrix *mat, StThreeVectorD point, Int_t nExtraCells);
    StMtdGeoNode():fTransFlag(0),fNExtraCells(0){}
   	virtual ~StMtdGeoNode();
   void            LocalToMaster(const Double_t* local, Double_t* master);
   void            MasterToLocal(const Double_t* master, Double_t* local);
   void  		   UpdateMatrix();
   StThreeVectorD  YZPlaneNormal(); 
   void   		   PrintNormal(); 
   void   		   SetNExtraCells(Int_t val)  { fNExtraCells = val>0?val:0; }

   Bool_t		   HelixCross(const StPhysicalHelixD helix, const Double_t pathToMagOutR, const Double_t tofToMagOutR, Double_t &pathL, Double_t &tof, StThreeVectorD &cross);
   Bool_t 		   IsGlobalPointIn(StThreeVectorD &global);
   Bool_t 		   IsLocalPointIn(const Double_t x, const Double_t y, const Double_t z);
   StThreeVectorD  GetNodePoint();


//#ifdef __ROOT__
  	//ClassDef(StMtdGeoNode,1)  //Virutal TGeoNode for Mtd geometry
//#endif
};

//_____________________________________________________________________________
inline void StMtdGeoNode::PrintNormal(){
	LOG_INFO<<"normal x,y,z="<<fNormal[0]<<","<<fNormal[1]<<","<<fNormal[2]<<endm;}


//----------------------------------------------------//
//													  //
//					StMtdGeoBackleg					  //
//													  //
//----------------------------------------------------//

class StMtdGeoBackleg : public StMtdGeoNode {
   //friend class StMtdGeometry;
 private:
   Int_t		mMTTGIndex;
   Int_t		mBacklegIndex;
 public:
   StMtdGeoBackleg (Int_t iMTTG, Int_t iBL, TGeoVolume *vol, TGeoHMatrix *mat, StThreeVectorD point, Int_t nExtraCells);

   StMtdGeoBackleg() {}
   ~StMtdGeoBackleg();

//#ifdef __ROOT__      
  //ClassDef(StMtdGeoBackleg,1)  //Tray node in TOF geometry
//#endif
};

//----------------------------------------------------//
//													  //
//					StMtdGeoModule					  //
//													  //
//----------------------------------------------------//

class StMtdGeoModule : public StMtdGeoNode {
   //friend class StMtdGeoBackleg;
 private:
   Int_t				mMTRAIndex;
   Int_t				mModuleIndex;
   static Int_t const	mCells=12;
 public:
   StMtdGeoModule (Int_t iMTRA, Int_t iMod, TGeoVolume *vol, TGeoHMatrix *mat, StThreeVectorD point, Int_t nExtraCells);
   StMtdGeoModule() {}
   ~StMtdGeoModule();
   Int_t		   FindCellId(const Double_t *local);
   Float_t		   GetCellPhiCenter(Int_t iCell);
   Float_t		   GetCellZCenter(Int_t iCell);
   Float_t		   GetCellLocalYCenter(Int_t iCell);

// protected:

//#ifdef __ROOT__      
   //ClassDef(StMtdGeoModule,1)  //module node in Mtd geometry
//#endif
};

//----------------------------------------------------//
//													  //
//					StMtdGeometry					  //
//													  //
//----------------------------------------------------//

class StMtdGeometry : public TNamed{
 private:
   static Int_t const mNBacklegs = 30;
   static Int_t const mNModules = 5;
   static Int_t const mNCells = 12;
   static Double_t const mStripLength = 87.; /// cm

   static Double_t const mMtdMinR = 392.802; /// mtd system minimum radius
   static Double_t const mMtdMaxR = 418.865; /// mtd system maximum radius
   static Double_t const mMagInR  = 303.290; /// magnet system inner radius
   static Double_t const mMagOutR = 364.290; /// magnet system outer radius
   static Double_t const mEmcInR  = 223.505; /// EMC system inner radius
   static Double_t const mEmcOutR = 248.742; /// EMC system outer radius
   
   static Double_t const mEmcELoss  = 0.215; /// EMC    mom eloss (GeV/c)
   static Double_t const mCoilELoss = 0.176; /// Coil   mom eloss (GeV/c)
   //static Double_t const mMagELoss  = 0.824; /// avg. Magnet mom eloss (GeV/c)

 public:
  StMtdGeometry(const char* name="mtdGeo",
		const char* title="Simplified Mtd Geometry");
  ~StMtdGeometry();
  
  void		Init(StMaker *maker);
   
  void   DebugOn()   { mDebug = kTRUE; }     
  void   DebugOff()  { mDebug = kFALSE; }
  Bool_t IsDebugOn() { return mDebug; }
  void   SetNExtraCells(Int_t val)  { mNExtraCells = val>0?val:0; }
  Bool_t   ProjToMagOutR(const StPhysicalHelixD helix, const StThreeVectorD vertex, StPhysicalHelixD &outHelix, Double_t &pathL, Double_t &tof, StThreeVectorD &pos);
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
  Double_t 		 GetFieldZ(StThreeVectorD pos) const;
  Double_t 		 GetFieldZ(Double_t x, Double_t y, Double_t z) const;
  StMtdGeoModule *GetGeoModule(Int_t iBL, Int_t iMod) const;
  Int_t  GetNModules(){return mNModules;}
  Int_t  GetNCells(){return mNCells;}
  void   SetGeomTag(const char *tag){mGeomTag=tag;}

 protected:
  Bool_t   mDebug;        //!Control message printing of this class
  Bool_t   mCosmicFlag;   //!Cosmic event flag
  Bool_t   mELossFlag;    //!Control energy loss flag
  Bool_t   mLockBField;    //!Control mag field to FF 
  Int_t    mNExtraCells;  //!Control matching range in the module. 
  Int_t    mNValidBLs;
  Int_t    mMTTG2BL[mNBacklegs];
  Int_t    mMTRA2Mod[mNBacklegs][mNModules];
  //BL, Module, Cell index all start from 1
  Float_t  mBFactor;

  StMtdGeoBackleg* mMtdGeoBackleg[mNBacklegs];
  StMtdGeoModule*  mMtdGeoModule[mNBacklegs][mNModules];

  StarMagField *mStarBField;

  TF1 *fMagEloss;
   
  static const char* backlegPref[4];//= "MTMT,MTMF,MTTG,MTT1";
  static const char* modulePref  ;//= "MTRA";
  //static const char* sensorPref  ;//= "MIGG";
  TString mGeomTag;

  Bool_t 		IsMTTG(const TGeoVolume * vol) const;
  //Bool_t 		IsMTT1(const TGeoVolume * vol) const { return !(strcmp(vol->GetName(), backlegPref2));}
  Bool_t 		IsMTRA(const TGeoVolume * vol) const { return !(strcmp(vol->GetName(), modulePref));}
  //Bool_t 		IsMIGG(const TGeoVolume * vol) const { return !(strcmp(vol->GetName(), sensorPref));}
  
#ifndef __CINT__
  void   		RemoveDuplicate(IntVec &vec);
#endif

  const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StMtdGeometry.h,v 1.6 2014/08/06 11:43:28 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  ClassDef(StMtdGeometry,1)
};

R__EXTERN  StMtdGeometry* gMtdGeometry;

#endif
