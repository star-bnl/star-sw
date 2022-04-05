/********************************************************************
 * $Id: StMtdGeometry.cxx,v 1.20 2018/04/25 15:58:35 marr Exp $
 ********************************************************************
 *
 * $Log: StMtdGeometry.cxx,v $
 * Revision 1.20  2018/04/25 15:58:35  marr
 * Fix the bug of converting projected localy to cellId
 *
 * Revision 1.19  2017/03/08 20:40:38  marr
 * Add back the old implementation of GetCellLocalYCenter() function to make
 * the class backward compatible.
 *
 * Revision 1.18  2017/02/13 02:56:11  marr
 * From 2017, do not move BL 8&24 along y direction by hand since this is already
 * done in the geometry file. Calibration, production and analysis should use
 * the new version consistently.
 *
 * Revision 1.17  2016/08/05 16:12:34  marr
 * Add MTD hit IdTruth to avoid applying dy shift for BL 8 and 24 for MC hits
 *
 * Revision 1.16  2015/07/29 14:52:47  smirnovd
 * Added class scope to static members (overlooked in previous commit)
 *
 * Revision 1.15  2015/07/29 01:11:25  smirnovd
 * Initialize static constants outside of class definition
 *
 * C++ forbids initialization of non-integral static const members within the class
 * definition. The syntax is allowed only for integral type variables.
 *
 * Revision 1.14  2015/07/24 15:56:05  marr
 * 1. Remove calling a macro in Init() to create geometry. It should be done within
 * the maker that uses this utility class.
 * 2. Add the TGeoManager parameter to the default constructor to force the existance
 * of the gometry when using this utility class.
 * 3. Simplify the code for getting the pointer to the magnetic field
 *
 * Revision 1.13  2015/05/01 01:55:34  marr
 * Fix the geometry of shifted backleg 8 and 24
 *
 * Revision 1.12  2015/04/07 16:23:33  marr
 * 1. Make use the constants defined in StMtdConstants.h
 * 2. Cleaning up
 *
 * Revision 1.11  2015/02/09 21:29:23  marr
 * Fix an overlook in extracting geometry for 2015 during bfc chain running
 *
 * Revision 1.10  2014/12/23 16:32:33  marr
 * In 2015 geometry file, MTD modules are placed under directory MagRefSys_1.
 * Modification is maded to reflect this change.
 *
 * Revision 1.9  2014/09/24 18:14:47  marr
 * Flip localY for cells in modules 4 and 5
 *
 * Revision 1.8  2014/09/18 21:59:08  marr
 * 1. Disable retrieving table geant2backlegID for year 2012 ana before
 * 2. The default geometry tag is set to yYYYYa
 *
 * Revision 1.7  2014/07/25 19:44:18  marr
 * Fix a minor inconsistency in using the fNExtraCells
 *
 * Revision 1.6  2014/07/24 17:02:30  huangbc
 * Add protection for reading magnetic field in case of track projection position is (nan,nan,nan).
 *
 * Revision 1.5  2014/07/16 15:31:01  huangbc
 * Add an option to lock bfield to FF.
 *
 * Revision 1.4  2014/07/10 20:45:13  huangbc
 * New geometry class for MTD, load geometry from geant geometry. Need gGeoManager.
 *
 * Revision 1.3  2013/08/07 18:25:30  geurts
 * include CVS Id and Log tags
 *
 *
 *******************************************************************/
#include <vector>
#include <assert.h>
#include <stdlib.h>
#include "StMtdGeometry.h"
#include "StMaker.h"
#include "StMessMgr.h" 
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoBBox.h"
#include "TGeoNode.h"
#include "TROOT.h"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"   // has "tesla" in it
#include "StDetectorDbMaker/St_MagFactorC.h"
#include "tables/St_mtdGeant2BacklegIDMap_Table.h"


const Double_t StMtdGeometry::mMtdMinR = 392.802; /// mtd system minimum radius
const Double_t StMtdGeometry::mMtdMaxR = 418.865; /// mtd system maximum radius
const Double_t StMtdGeometry::mMagInR  = 303.290; /// magnet system inner radius
const Double_t StMtdGeometry::mMagOutR = 364.290; /// magnet system outer radius
const Double_t StMtdGeometry::mEmcInR  = 223.505; /// EMC system inner radius
const Double_t StMtdGeometry::mEmcOutR = 248.742; /// EMC system outer radius

const char* StMtdGeometry::backlegPref[4] = {"MTMT","MTMF","MTTG","MTT1"};
const char* StMtdGeometry::modulePref   = "MTRA";
//const char* StMtdGeometry::sensorPref  = "MIGG";

//----------------------------------------------------//
//													  //
//					StMtdGeoNode					  //
//													  //
//----------------------------------------------------//

//#ifdef __ROOT__      
//ClassImp(StMtdGeoNode)
//#endif

// ___________________________________________________________________________
StMtdGeoNode::StMtdGeoNode(TGeoVolume *vol, TGeoHMatrix *mat, StThreeVectorD point, Int_t nExtraCells)
: fPoint(point)
{

	fMatrix = new TGeoHMatrix();
	fMatrix->CopyFrom(mat);
	fVolume = vol->CloneVolume();
	fTransFlag = kFALSE;	
	fNExtraCells = nExtraCells;
	UpdateMatrix();
	fNormal = YZPlaneNormal();
}

StMtdGeoNode::~StMtdGeoNode()
{

	delete fVolume;	
	delete fMatrix;	
	delete fTransMRS;	
	delete fRotMRS;	
}


//_____________________________________________________________________________
void  StMtdGeoNode::UpdateMatrix()
{
	//
	//Update the Translation and RotateMatrix between local and master
	//  and store them as members: mTransMRS, mRotMRS
	//while TNode stores them locally to share among all TNode objects,
	//  thus they may be changed afterward !
	//

	fTransFlag = kFALSE;
	if(fMatrix){ 
		fTransMRS = fMatrix->GetTranslation(); 
		fRotMRS	 = fMatrix->GetRotationMatrix(); 
		fTransFlag = kTRUE;
	}else{
		LOG_WARN<<"No TGeoHMatrix!"<<endm;
	}

}
//_____________________________________________________________________________
void StMtdGeoNode::LocalToMaster(const Double_t* local, Double_t* master)
{
	//
	//Transform local coordinate into global coordinate
	//
	//  UpdateMatrix();

	if (!fMatrix) { 
		if(fTransFlag){
			//mTransFlag==kTRUE, i.e. StMtdGeomNode::UpdateMatrix() invoked already
			Double_t x, y, z;
			x = local[0];
			y = local[1];
			z = local[2];

			master[0] = fTransMRS[0] + fRotMRS[0]*x + fRotMRS[1]*y + fRotMRS[2]*z;
			master[1] = fTransMRS[1] + fRotMRS[3]*x + fRotMRS[4]*y + fRotMRS[5]*z;
			master[2] = fTransMRS[2] + fRotMRS[6]*x + fRotMRS[7]*y + fRotMRS[8]*z;
		}else{
			LOG_WARN << " No TGeoHMatrix::LocalToMaster is wrong, so do nothing" << endm;
		}
	} else {
		fMatrix->LocalToMaster(local, master);
	}     

	//definition is not same as StBTofGeometry, belows are wrong
	//master[0] = fTransMRS[0] + fRotMRS[0]*x + fRotMRS[3]*y + fRotMRS[6]*z;
	//master[1] = fTransMRS[1] + fRotMRS[1]*x + fRotMRS[4]*y + fRotMRS[7]*z;
	//master[2] = fTransMRS[2] + fRotMRS[2]*x + fRotMRS[5]*y + fRotMRS[8]*z;
	//LOG_INFO<<"master ("<<master[0]<<","<<master[1]<<","<<master[2]<<")"<<endm;
	//LOG_INFO<<"test ("<<test[0]<<","<<test[1]<<","<<test[2]<<")"<<endm;

	return;

}

//_____________________________________________________________________________
void StMtdGeoNode::MasterToLocal(const Double_t* master, Double_t* local)
{
	//
	//Transform global coordinate into local coordinate
	//
	if(!fMatrix){
		if (fTransFlag) {
			//fTransFlag==kTRUE, i.e. StMtdGeoNode::UpdateMatrix() invoked already
			Double_t x, y, z;
			x = master[0] - fTransMRS[0];
			y = master[1] - fTransMRS[1];
			z = master[2] - fTransMRS[2];

			local[0] = fRotMRS[0]*x + fRotMRS[3]*y + fRotMRS[6]*z;
			local[1] = fRotMRS[1]*x + fRotMRS[4]*y + fRotMRS[7]*z;
			local[2] = fRotMRS[2]*x + fRotMRS[5]*y + fRotMRS[8]*z;

		}else{
			LOG_WARN<< " No TGeoHMatrix::MasterToLocal is wrong, so do nothing" << endm;
		}
	}else{
		fMatrix->MasterToLocal(master,local);
	}

	Double_t x, y, z;
	x = master[0] - fTransMRS[0];
	y = master[1] - fTransMRS[1];
	z = master[2] - fTransMRS[2];

	Double_t test[3];
	test[0] = fRotMRS[0]*x + fRotMRS[3]*y + fRotMRS[6]*z;
	test[1] = fRotMRS[1]*x + fRotMRS[4]*y + fRotMRS[7]*z;
	test[2] = fRotMRS[2]*x + fRotMRS[5]*y + fRotMRS[8]*z;

	return;

}

//_____________________________________________________________________________
StThreeVectorD StMtdGeoNode::YZPlaneNormal()
{
	//
	//Calculate the vector unit of normal to YZ-plane
	// i.e. the global representation of local unit vector (1,0,0)
	//

	Double_t ux[3], nx[3];

	ux[0] = 1;  ux[1] = 0;  ux[2] = 0;
	LocalToMaster(ux,nx);

	nx[0] -= fTransMRS[0];
	nx[1] -= fTransMRS[1];
	nx[2] -= fTransMRS[2];

	return StThreeVectorD(nx[0],nx[1],nx[2]);
}
//_____________________________________________________________________________
Bool_t StMtdGeoNode::HelixCross(const StPhysicalHelixD helix, const Double_t pathToMagOutR, const Double_t tofToMagOutR, Double_t &pathL, Double_t &tof, StThreeVectorD &cross){

	Float_t MaxPathLength = 1000.;

	Bool_t ret = kFALSE;
	pathL = -9999.;
	tof   = -9999.;

	StThreeVectorD oh = helix.origin();
	if(TMath::IsNaN(oh.x())||TMath::IsNaN(oh.y())||TMath::IsNaN(oh.z())||TMath::Abs(oh.perp())>450.||TMath::Abs(oh.z())>300.) return ret;
	pathL = helix.pathLength(fPoint, fNormal);
	double betaGam = helix.momentum(gMtdGeometry->GetFieldZ(oh)).mag()/muonMass;
	double velocity = sqrt(betaGam*betaGam/(1+betaGam*betaGam))*c_light*1e-9;
	tof   = pathL/velocity;
	LOG_DEBUG<<"StMtdGeoNode::HelixCross() pathL = "<<pathL<<" point ("<<fPoint.x()<<","<<fPoint.y()<<","<<fPoint.z()<<") normal("<<fNormal.x()<<","<<fNormal.y()<<","<<fNormal.z()<<")  radius = "<<fPoint.perp()<<endm;
	if(pathL>0 && pathL<MaxPathLength){
		cross = helix.at(pathL);
		ret = IsGlobalPointIn(cross);
		LOG_DEBUG<<"track cross point "<<cross.x()<<","<<cross.y()<<","<<cross.z()<<endm;
		pathL += pathToMagOutR;
		tof   += tofToMagOutR;
	}	
	return ret;
}

//_____________________________________________________________________________
Bool_t StMtdGeoNode::IsGlobalPointIn(StThreeVectorD &global){
	Double_t xl[3], xg[3];
	xg[0] = global.x();
	xg[1] = global.y();
	xg[2] = global.z();
	MasterToLocal(xg, xl);
	Bool_t ret = IsLocalPointIn(xl[0], xl[1], xl[2]);
	return ret;
}

//_____________________________________________________________________________
Bool_t StMtdGeoNode::IsLocalPointIn(const Double_t x, const Double_t y, const Double_t z){
	TGeoBBox *brik = (TGeoBBox*)fVolume->GetShape();
	Double_t dx = brik->GetDX();
	Float_t nExtraCells = fNExtraCells>1.66?fNExtraCells-1.66:0;
	Double_t dy = brik->GetDY()+nExtraCells*(gMtdCellWidth+gMtdCellGap);
	Double_t dz = brik->GetDZ();
	Bool_t ret = -dx<x && x<dx && -dy<y && y<dy && -dz<z && z<dz;

	return ret;
}

//_____________________________________________________________________________
StThreeVectorD  StMtdGeoNode::GetNodePoint(){
	return fPoint;
}

//----------------------------------------------------//
//													  //
//					StMtdGeoBackleg					  //
//													  //
//----------------------------------------------------//

//#ifdef __ROOT__      
//ClassImp(StMtdGeoBackleg)
//#endif

// ___________________________________________________________________________
	StMtdGeoBackleg::StMtdGeoBackleg (Int_t iMTTG, Int_t iBL, TGeoVolume *vol, TGeoHMatrix *mat, StThreeVectorD point, Int_t nExtraCells)
: StMtdGeoNode(vol, mat, point, nExtraCells), mMTTGIndex(iMTTG), mBacklegIndex(iBL)
{

}

StMtdGeoBackleg::~StMtdGeoBackleg()
{

}

//----------------------------------------------------//
//													  //
//					StMtdGeoModule					  //
//													  //
//----------------------------------------------------//

//#ifdef __ROOT__      
//ClassImp(StMtdGeoModule)
//#endif

// ___________________________________________________________________________
	StMtdGeoModule::StMtdGeoModule (Int_t iMTRA, Int_t iMod, TGeoVolume *vol, TGeoHMatrix *mat, StThreeVectorD point, Int_t nExtraCells)
: StMtdGeoNode(vol, mat, point, nExtraCells), mMTRAIndex(iMTRA), mModuleIndex(iMod) 
{

}

StMtdGeoModule::~StMtdGeoModule()
{

}

//_____________________________________________________________________________
Int_t StMtdGeoModule::FindCellId(const Double_t *local){
	Int_t cellId = -99;
	if ( IsLocalPointIn(local[0],local[1],local[2]) ) {
   		cellId = TMath::FloorNint((local[1]+(gMtdCellWidth+gMtdCellGap)*gMtdNCells/2.)/(gMtdCellWidth+gMtdCellGap));
        }
	return cellId;
}

//_____________________________________________________________________________
Float_t StMtdGeoModule::GetCellPhiCenter(Int_t iCell){
	Float_t phi = fPoint.phi();	
	Float_t r = fPoint.perp();
	Float_t stripPhiCen = 0.;
	if(mModuleIndex>0&&mModuleIndex<4){
		stripPhiCen = phi-(gMtdNCells/2.-0.5-iCell)*(gMtdCellWidth+gMtdCellGap)/r; // approximation
	}else{
		stripPhiCen = phi+(gMtdNCells/2.-0.5-iCell)*(gMtdCellWidth+gMtdCellGap)/r; 
	}
	if(stripPhiCen>2.*TMath::Pi()) stripPhiCen -= 2.*TMath::Pi();
	if(stripPhiCen<0.)    stripPhiCen += 2.*TMath::Pi();
	return stripPhiCen;
}

//_____________________________________________________________________________
Float_t StMtdGeoModule::GetCellZCenter(Int_t iCell){
	return fPoint.z();
}

//_____________________________________________________________________________
Float_t StMtdGeoModule::GetCellLocalYCenter(Int_t iCell, Int_t iBL, Int_t idTruth){
  // Suitable before Run17  

  Float_t cell_width = gMtdCellWidth+gMtdCellGap;
  Float_t y_center = (mModuleIndex<4? 1 : -1) * (iCell-gMtdNCells/2+0.5) * cell_width;	 
   	 
  if(idTruth==0)	 
    {	 
      // only needed for real hits	 
      // MC hits are produced with ideal geometry	 
      if(iBL==8)  y_center -= 3 * cell_width;	 
      if(iBL==24) y_center += 2 * cell_width;	 
    }	 
 	 
  return y_center;
}

//_____________________________________________________________________________
Float_t StMtdGeoModule::GetCellLocalYCenter(Int_t iCell){
  // Suitable starting from Run17  

  Float_t cell_width = gMtdCellWidth+gMtdCellGap;
  Float_t y_center = (mModuleIndex<4? 1 : -1) * (iCell-gMtdNCells/2+0.5) * cell_width;
  return y_center;
}

//----------------------------------------------------//
//													  //
//					StMtdGeometry					  //
//													  //
//----------------------------------------------------//

StMtdGeometry* gMtdGeometry = 0;

#ifdef __ROOT__      
ClassImp(StMtdGeometry)
#endif

// ___________________________________________________________________________
StMtdGeometry::StMtdGeometry(const char* name, const char* title, TGeoManager *manager)
: TNamed(name,title)
{
	//
	//We only need one instance of StMtdGeometry
	//

	mDebug = kFALSE;
	mCosmicFlag = kFALSE;
	mELossFlag = kTRUE;
	mNExtraCells = 0;
	mNValidBLs = 0;
	mStarBField = 0;
	mBFactor = -1.;
	mLockBField = 0;
	assert(manager);
	mGeoManager = manager;

	fMagEloss = new TF1("f2","[0]*exp(-pow([1]/x,[2]))",0.,100);
	fMagEloss->SetParameters(1.38147e+00,6.08655e-02,5.03337e-01);

	for(int i=0;i<gMtdNBacklegs;i++) {
		mMtdGeoBackleg[i] = 0;
		for(int j=0;j<gMtdNModules;j++) {
			mMtdGeoModule[i][j] = 0;
		}
	}

	if (gMtdGeometry) {
		LOG_INFO << "Warning !! There is already StMtdGeometry at pointer="
			<< (void*)gMtdGeometry << ", so it is deleted"
			<< endm;
		delete gMtdGeometry;
	}
	gMtdGeometry = this;
}

StMtdGeometry::~StMtdGeometry()
{
	if(IsDebugOn()){
		LOG_INFO << "StMtdGeometry at pointer =" << (void*)gMtdGeometry
			<< " will be deleted" << endm;
	}
	gMtdGeometry = 0;
}

void StMtdGeometry::Init(StMaker *maker){

	if(maker->Debug()) DebugOn();

	Int_t mYear = maker->GetDateTime().GetYear();
	if(IsDebugOn()){ 
		LOG_INFO<<"Input data from year "<<mYear<<endm;
	}

	// Get magnetic field map
	if(!StarMagField::Instance() && mLockBField)
	  {
	    LOG_INFO<<" Initializing locked mag.field for simulation! "<<endm;
	    new StarMagField ( StarMagField::kMapped, mBFactor);
	  }
	assert(StarMagField::Instance());
	mStarBField = StarMagField::Instance();
	LOG_INFO<<" Initializing mag.field from StarMagField! fScale = "<<mStarBField->GetFactor()<<endm;
	
	// Load geant2backlegID Map 
	// Extract MTD maps from database
	if(mYear<=2012)
	  {
	    LOG_WARN<<"No geant2backlegID table for 2012 and before"<<endm;
	  }
	else
	  {
	    LOG_INFO << "Retrieving geant2backlegID table from database ..." << endm;

	    TDataSet *dataset = maker->GetDataBase("Geometry/mtd/mtdGeant2BacklegIDMap");
	    St_mtdGeant2BacklegIDMap *mtdGeant2BacklegIDMap = static_cast<St_mtdGeant2BacklegIDMap*>(dataset->Find("mtdGeant2BacklegIDMap"));
	    if ( !mtdGeant2BacklegIDMap )
	      {
		LOG_ERROR << "No mtdGeant2BacklegIDMap found in database" << endm;
		return; 
	      }
	
	    mtdGeant2BacklegIDMap_st *mGeant2BLTable = static_cast<mtdGeant2BacklegIDMap_st*>(mtdGeant2BacklegIDMap->GetTable());
	    if(mGeant2BLTable)
	      { 
		for ( Int_t i = 0; i < 30; i++ )
		  {
		    mMTTG2BL[i] = (Int_t)mGeant2BLTable->geant2backlegID[i];
		    if(mMTTG2BL[i]>30 || mMTTG2BL[i]<0) LOG_ERROR<<" Wrong map! check database! "<<endm;
		  }
	      }
	    else
	      {
		LOG_ERROR << "No geant2backlegIDMap table found in database" << endm;
	      }
	  }

	for(int i=0;i<30;++i)
	  { 
	    for(int j=0;j<5;++j)
	      {
		if(i<11||i>19) mMTRA2Mod[i][j] = j+1;
		else
		  {
		    if(j==3||j==4) mMTRA2Mod[i][j] = 0;
		    else mMTRA2Mod[i][j] = j+2;
		  }
	      }
	  }

	Int_t mGeoYear = 0;
	if(mGeoManager->CheckPath("/HALL_1/CAVE_1/MUTD_1/MTMT_1")){
		LOG_INFO<<"found y2012 geometry"<<endm;
	   	mGeoYear=2012;
	}
	else if(mGeoManager->CheckPath("/HALL_1/CAVE_1/MagRefSys_1/MUTD_1")){
		LOG_INFO<<"found y2015 geometry"<<endm;
	   	mGeoYear=2015;
	}

	// intialize backleg/module geometry
	TGeoVolume *mMtdGeom = mGeoManager->FindVolumeFast("MUTD");
	const char *elementName = mMtdGeom->GetName();
	if(elementName){
		if(IsDebugOn()) LOG_INFO <<" found detector:"<<elementName<<endm;
		TGeoIterator next(mMtdGeom);
		if(mGeoYear==2015) next.SetTopName("/HALL_1/CAVE_1/MagRefSys_1/MUTD_1");
		else next.SetTopName("/HALL_1/CAVE_1/MUTD_1");
		TGeoNode   *node = 0;
		TGeoVolume *blVol = 0;
		TGeoVolume *modVol = 0;
		Int_t ibackleg = 0;
		Int_t imodule = 0;
		Double_t minR = 999.;
		Double_t maxR = 0.;
		while ( (node=(TGeoNode*)next()) )
		{
			TString name = node->GetName();
			TString path;
			next.GetPath(path);
			if(!mGeoManager->CheckPath(path.Data())){ 
				LOG_WARN<<"Path "<<path.Data()<<" is not found"<<endm;
				continue;
			}
			mGeoManager->cd(path.Data());
			TGeoVolume *detVol = mGeoManager->GetCurrentVolume();	
			Bool_t found = ( IsMTTG(detVol) || IsMTRA(detVol) );
			if (found) {
				detVol->SetVisibility(kTRUE);
				//if (detVol->GetLineColor()==1 || detVol->GetLineColor()==7) 
				//	detVol->SetLineColor(14);
			} else {
				detVol->SetVisibility(kFALSE);
				continue;
			}
			if(IsDebugOn()) LOG_INFO<<"currentpath = "<<mGeoManager->GetPath()<<" node name="<<mGeoManager->GetCurrentNode()->GetName()<<endm;

			//fill GeoBLs and GeoModules
			if(IsMTTG(detVol)){
				blVol = (TGeoVolume *)detVol;
				Int_t iMTTG = node->GetNumber(); 
				if(IsDebugOn()) LOG_INFO<<"Node name = "<<node->GetName()<<" iMTTG="<<iMTTG<<endm;
				if(iMTTG>0){ 
					Int_t iBL = 0;
					if(mGeoYear==2012)
					  {
					    if(!strcmp(blVol->GetName(), backlegPref[0]))
					      { 
						iBL = 26;
					      }
					    else if(!strcmp(blVol->GetName(), backlegPref[1]))
					      {
						if(iMTTG==1) iBL = 27;
						else if(iMTTG==2) iBL = 28;
						else
						  { 
						    LOG_ERROR<<"Wrong BL id, this is not Y2012 geometry!"<<endm;
						    iBL = 0;
						    continue;
						  }
					      }
					    else
					      {
						LOG_ERROR<<"Wrong BL id, this is not Y2012 geometry!"<<endm;
						iBL = 0;
						continue;
					      }
					  }
					else
					  {
					    iBL = mMTTG2BL[iMTTG-1];
					  }
					Double_t op[3];
					Double_t local[3] = {0,0,0};
					mGeoManager->LocalToMaster(local,op);
					++mNValidBLs;
					TGeoHMatrix *mat = mGeoManager->GetCurrentMatrix();
					StThreeVectorD point(op[0],op[1],op[2]);
					if(mGeoYear==2012)
					  {
					    Double_t *trans = mat->GetTranslation();
					    Double_t *rot   = mat->GetRotationMatrix();
					    trans[0] = -1*trans[0];
					    rot[0] = -1*rot[0];
					    rot[4] = -1*rot[4];
					    mat->SetTranslation(trans);
					    mat->SetRotation(rot);
					    point.setX(-1*op[0]);
					  }
					mMtdGeoBackleg[iBL-1] = new StMtdGeoBackleg(iMTTG, iBL, blVol, mat, point, mNExtraCells);
					if(IsDebugOn()) LOG_INFO<<"iMTTG="<<iMTTG<<" iBL="<<iBL<<endm;
					imodule = 0;   // clear for this backleg
					ibackleg = iBL;
				}
			}

			if(IsMTRA(detVol)){
				modVol = (TGeoVolume *)detVol;
				Int_t iMTRA = node->GetNumber();
				if(iMTRA>0){
					Int_t iMod = mMTRA2Mod[ibackleg-1][iMTRA-1];
					if(mGeoYear==2012&&ibackleg==26){
						iMod = iMTRA+1;
					}
					Double_t op[3];
					Double_t local[3] = {0,0,0};
					mGeoManager->LocalToMaster(local,op);
					++imodule;
					double R = sqrt(op[0]*op[0]+op[1]*op[1]);
					if(R<minR) minR = R;
					if(R>maxR) maxR = R;

					TGeoHMatrix *mat = mGeoManager->GetCurrentMatrix();
					StThreeVectorD point(op[0],op[1],op[2]);
					if(mGeoYear==2012)
					  {
					    Double_t *trans = mat->GetTranslation();
					    Double_t *rot   = mat->GetRotationMatrix();
					    trans[0] = -1*trans[0];
					    rot[0] = -1*rot[0];
					    rot[4] = -1*rot[4];
					    mat->SetTranslation(trans);
					    mat->SetRotation(rot);
					    point.setX(-1*op[0]);
					  }

					mMtdGeoModule[ibackleg-1][iMod-1] = new StMtdGeoModule(iMTRA, iMod, modVol, mat, point, mNExtraCells);
					if(IsDebugOn()) LOG_INFO<<"iMTRA="<<iMTRA<<" iMod="<<iMod<<endm;
				}
			}
		}
	}

	if(IsDebugOn()){
		for(int i=0;i<gMtdNBacklegs;i++){
			for(int j=0;j<gMtdNModules;j++){
				if(mMtdGeoModule[i][j])
					LOG_INFO<<"valid (backleg,module) = "<<i+1<<","<<j+1<<endm;
			}
		}
	}
}

Bool_t StMtdGeometry::ProjToMagOutR(const StPhysicalHelixD helix, const StThreeVectorD vertex, StPhysicalHelixD &outHelix, Double_t &pathL, Double_t &tof, StThreeVectorD &pos){

	// 1. project to vertex
	pathL = 0.;
	tof = -9999.;
	pos.set(0,0,0);
	StThreeVectorD dcaPos(0,0,0);
	Double_t pathL2Vtx = 0.;
	ProjToVertex(helix,vertex,pathL2Vtx,tof,dcaPos);

	if(IsDebugOn()){
		LOG_INFO<<"------------ start of projection to magOutR ------------"<<endm;
		LOG_INFO<<" to vertex("<< vertex.x()<<","<<vertex.y()<<","<<vertex.z()<<") dca = "<<dcaPos.mag()<<endm;
	}
	//if(!mCosmicFlag && dcaPos.mag()>10) return kFALSE;

	//2. project to Emc
	pairD sInnerEmc = helix.pathLength(mEmcInR);
	if(sInnerEmc.first<=0 && sInnerEmc.second<=0) return kFALSE;
	double rInnerEmc =  (sInnerEmc.first < 0 || sInnerEmc.second < 0) 
		? max(sInnerEmc.first, sInnerEmc.second) : min(sInnerEmc.first, sInnerEmc.second); 
	StThreeVectorD innerEmcPos = helix.at(rInnerEmc);
		
	if(TMath::IsNaN(innerEmcPos.x())||TMath::IsNaN(innerEmcPos.y())||TMath::IsNaN(innerEmcPos.z())||TMath::Abs(innerEmcPos.perp())>450.||TMath::Abs(innerEmcPos.z())>300.) return kFALSE;
	Double_t bField = GetFieldZ(innerEmcPos);
	Int_t charge = helix.charge(bField);
	if(IsDebugOn()) LOG_INFO<<" charge = "<<charge<<" bField = "<<bField<<endm;
	StThreeVectorD innerEmcMom = helix.momentumAt(rInnerEmc,bField*tesla);
	StPhysicalHelixD helixInEmc(innerEmcMom,innerEmcPos,bField*tesla,charge);

	double betaGam = innerEmcMom.mag()/muonMass;
	double vInner  = sqrt(betaGam*betaGam/(1+betaGam*betaGam))*c_light*1e-9;
	double tof2InnerEmc = -9999.;
	tof2InnerEmc = (pathL2Vtx+rInnerEmc)/vInner;

	if(IsDebugOn()){
		LOG_INFO<<" to Emcinner: pos x,y,z:"<<innerEmcPos.x()<<","<<innerEmcPos.y()<<","<<innerEmcPos.z()<<endm;
		LOG_INFO<<" to Emcinner: mom p,pt,eta,phi:"<<innerEmcMom.mag()<<","<<innerEmcMom.perp()<<","<<innerEmcMom.pseudoRapidity()<<","<<innerEmcMom.phi()<<endm;
		LOG_INFO<<" to Emcinner: tof:"<<tof2InnerEmc<<endm;
	}

	//3. outer Emc
	const int nEmcStep = 4;
	double rEmcStep = (mEmcOutR-mEmcInR)/nEmcStep; //cm
	double pathLEmcLayer[nEmcStep];
	double tofEmcLayer[nEmcStep];
	double vEmc = -9999.;
	double betaGamEmc= -9999.;
	StThreeVectorD EmcLayerPos = innerEmcPos;
	StThreeVectorD EmcLayerMom = innerEmcMom;
	Double_t  elossEmc = 0.;
	if(mELossFlag){
		if(mCosmicFlag&&EmcLayerPos.phi()>0&&EmcLayerPos.phi()<TMath::Pi()) elossEmc = -1.*gMtdMuonELossInEmc/nEmcStep;
		else elossEmc = gMtdMuonELossInEmc/nEmcStep;
	}
	for( int i=0; i<nEmcStep; i++){
		double EmcLayerRadius = mEmcInR+rEmcStep*(i+1);

		if(TMath::IsNaN(EmcLayerPos.x())||TMath::IsNaN(EmcLayerPos.y())||TMath::IsNaN(EmcLayerPos.z())||TMath::Abs(EmcLayerPos.perp())>450.||TMath::Abs(EmcLayerPos.z())>300.) return kFALSE;
		bField = GetFieldZ(EmcLayerPos);
		StPhysicalHelixD helixInEmc(EmcLayerMom,EmcLayerPos,bField*tesla,charge);

		pairD sEmcLayer = helixInEmc.pathLength(EmcLayerRadius);
		if(sEmcLayer.first<=0 && sEmcLayer.second<=0) return kFALSE;

		double rEmcLayer = (sEmcLayer.first < 0 || sEmcLayer.second < 0) 
			? max(sEmcLayer.first, sEmcLayer.second) : min(sEmcLayer.first, sEmcLayer.second); 

		betaGamEmc 	= EmcLayerMom.mag()/muonMass;
		vEmc  	    = sqrt(betaGamEmc*betaGamEmc/(1.+betaGamEmc*betaGamEmc))*c_light*1e-9;
		pathLEmcLayer[i]  = rEmcLayer;
		tofEmcLayer[i]     = rEmcLayer/vEmc;

		EmcLayerPos = helixInEmc.at(rEmcLayer);
		EmcLayerMom = helixInEmc.momentumAt(rEmcLayer,bField*tesla);
		//EmcLayerMom = (sqrt(pow((sqrt(pow((abs(EmcLayerMom)),2)+pow(muonMass,2))-rEmcLayer*elossEmc),2)-pow(muonMass,2)))/(abs(EmcLayerMom))*EmcLayerMom;
		EmcLayerMom = (EmcLayerMom.mag()-elossEmc)/EmcLayerMom.mag()*EmcLayerMom;
	}

	if(IsDebugOn()){
		double tof2OuterEmc = 0.;
		for(int i=0;i<nEmcStep;i++) tof2OuterEmc += tofEmcLayer[i];
		LOG_INFO<<" to Emcouter: pos x,y,z:"<<EmcLayerPos.x()<<","<<EmcLayerPos.y()<<","<<EmcLayerPos.z()<<endm;
		LOG_INFO<<" to Emcouter: mom p,pt,eta,phi:"<<EmcLayerMom.mag()<<","<<EmcLayerMom.perp()<<","<<EmcLayerMom.pseudoRapidity()<<","<<EmcLayerMom.phi()<<endm;
		LOG_INFO<<" to Emcouter: tof:"<<tof2OuterEmc<<endm;
	}

	//4: coil 
	if(TMath::IsNaN(EmcLayerPos.x())||TMath::IsNaN(EmcLayerPos.y())||TMath::IsNaN(EmcLayerPos.z())||TMath::Abs(EmcLayerPos.perp())>450.||TMath::Abs(EmcLayerPos.z())>300.) return kFALSE;
	bField = GetFieldZ(EmcLayerPos);
	const int nCoilStep = 5;
	double rCoilStep = (mMagInR-mEmcOutR)/nCoilStep; //cm
	double pathLCoilLayer[nCoilStep];
	double tofCoilLayer[nCoilStep];
	double vCoil=-9999.;
	double betaGamCoil=-9999.;
	StThreeVectorD CoilLayerPos = EmcLayerPos;
	StThreeVectorD CoilLayerMom = EmcLayerMom;
	Double_t  elossCoil = 0.;
	if(mELossFlag){
		if(mCosmicFlag&&CoilLayerPos.phi()>0&&CoilLayerPos.phi()<TMath::Pi()) elossCoil = -1.*gMtdMuonELossInCoil/nCoilStep;
		else elossCoil = gMtdMuonELossInCoil/nCoilStep;
	}
	for( int i=0; i<nCoilStep; i++){
		double CoilLayerRadius = mEmcOutR+rCoilStep*(i+1);

		if(TMath::IsNaN(CoilLayerPos.x())||TMath::IsNaN(CoilLayerPos.y())||TMath::IsNaN(CoilLayerPos.z())||TMath::Abs(CoilLayerPos.perp())>450.||TMath::Abs(CoilLayerPos.z())>300.) return kFALSE;
		bField = GetFieldZ(CoilLayerPos);
		StPhysicalHelixD helixInCoil(CoilLayerMom,CoilLayerPos,bField*tesla,charge);

		pairD sCoilLayer = helixInCoil.pathLength(CoilLayerRadius);
		if(sCoilLayer.first<=0 && sCoilLayer.second<=0) return kFALSE;

		double rCoilLayer = (sCoilLayer.first < 0 || sCoilLayer.second < 0) 
			? max(sCoilLayer.first, sCoilLayer.second) : min(sCoilLayer.first, sCoilLayer.second); 

		betaGamCoil = CoilLayerMom.mag()/muonMass;
		vCoil  	    = sqrt(betaGamCoil*betaGamCoil/(1.+betaGamCoil*betaGamCoil))*c_light*1e-9;
		pathLCoilLayer[i]  = rCoilLayer;
		tofCoilLayer[i]     = rCoilLayer/vCoil;

		CoilLayerPos = helixInCoil.at(rCoilLayer);
		CoilLayerMom = helixInCoil.momentumAt(rCoilLayer,bField*tesla);
		if(IsDebugOn()){
			LOG_INFO<<" to Coil Layer "<<i<<" bField = "<<bField<<endm;
			LOG_INFO<<" to Coil Layer"<< i <<" : pos x,y,z:"<<CoilLayerPos.x()<<","<<CoilLayerPos.y()<<","<<CoilLayerPos.z()<<endm;
			LOG_INFO<<" to Coil Layer"<< i <<" : mom p,pt,eta,phi:"<<CoilLayerMom.mag()<<","<<CoilLayerMom.perp()<<","<<CoilLayerMom.pseudoRapidity()<<","<<CoilLayerMom.phi()<<endm;
		}
		//CoilLayerMom=(sqrt(pow((sqrt(pow((abs(CoilLayerMom)),2)+pow(muonMass,2))-rCoilLayer*elossCoil),2)-pow(muonMass,2)))/(abs(CoilLayerMom))*CoilLayerMom;
		CoilLayerMom = (CoilLayerMom.mag()-elossCoil)/CoilLayerMom.mag()*CoilLayerMom;
	}

	if(IsDebugOn()){
		double tof2OuterCoil = 0.;
		for(int i=0;i<nCoilStep;i++) tof2OuterCoil += tofCoilLayer[i];
		LOG_INFO<<" to Coilouter: pos x,y,z:"<<CoilLayerPos.x()<<","<<CoilLayerPos.y()<<","<<CoilLayerPos.z()<<endm;
		LOG_INFO<<" to Coilouter: mom p,pt,eta,phi:"<<CoilLayerMom.mag()<<","<<CoilLayerMom.perp()<<","<<CoilLayerMom.pseudoRapidity()<<","<<CoilLayerMom.phi()<<endm;
		LOG_INFO<<" to Coilouter: tof:"<<tof2OuterCoil<<endm;
	}

	//5. Mag
	const Int_t nMagStep = 10;
	double pathLMagLayer[nMagStep];
	double tofMagLayer[nMagStep];
	double vMag;
	double betaGamMag;
	double rStep = (mMagOutR-mMagInR)/nMagStep; //cm
	StThreeVector<double> MagLayerPos = CoilLayerPos;
	StThreeVector<double> MagLayerMom = CoilLayerMom;
	Double_t elossMag = 0.;
	double mMagELoss  = 0.;
	if(mELossFlag){
		mMagELoss = fMagEloss->Eval(innerEmcMom.mag())-gMtdMuonELossInEmc-gMtdMuonELossInCoil;
		if(mCosmicFlag&&MagLayerPos.phi()>0&&MagLayerPos.phi()<TMath::Pi()) elossMag = -1.*mMagELoss/nMagStep;
		else elossMag = mMagELoss/nMagStep;
	}
	if(IsDebugOn()){
		LOG_INFO<<" mMagELoss = "<<mMagELoss<<" p = "<<innerEmcMom.mag()<<endm;
	}
	for( int i=0; i<nMagStep; i++){

		double MagLayerRadius = mMagInR+rStep*(i+1);
		if(TMath::IsNaN(MagLayerPos.x())||TMath::IsNaN(MagLayerPos.y())||TMath::IsNaN(MagLayerPos.z())||TMath::Abs(MagLayerPos.perp())>450.||TMath::Abs(MagLayerPos.z())>300.) return kFALSE;
		bField = GetFieldZ(MagLayerPos);
		StPhysicalHelixD helixInMag(MagLayerMom,MagLayerPos,bField*tesla,charge);

		if(IsDebugOn()){
			LOG_INFO<<" to Magnet layer "<<i<<" bField = "<<bField<<endm;
			LOG_INFO<<" to Magnet Layer "<< i <<" : pos x,y,z:"<<MagLayerPos.x()<<","<<MagLayerPos.y()<<","<<MagLayerPos.z()<<endm;
			LOG_INFO<<" to Magnet Layer "<< i <<" : mom p,pt,eta,phi:"<<MagLayerMom.mag()<<","<<MagLayerMom.perp()<<","<<MagLayerMom.pseudoRapidity()<<","<<MagLayerMom.phi()<<endm;
		}
		pairD sMagLayer = helixInMag.pathLength(MagLayerRadius);
		if(sMagLayer.first<=0 && sMagLayer.second<=0) return kFALSE;

		double rMagLayer = (sMagLayer.first < 0 || sMagLayer.second < 0) 
			? max(sMagLayer.first, sMagLayer.second) : min(sMagLayer.first, sMagLayer.second); 

		betaGamMag  = MagLayerMom.mag()/muonMass;
		vMag  	    = sqrt(betaGamMag*betaGamMag/(1+betaGamMag*betaGamMag))*c_light*1e-9;
		pathLMagLayer[i]  = rMagLayer;
		tofMagLayer[i]  = rMagLayer/vMag;

		MagLayerPos = helixInMag.at(rMagLayer);
		MagLayerMom = helixInMag.momentumAt(rMagLayer,bField*tesla);

		double momMag = MagLayerMom.mag();
		if(momMag<elossMag) return kFALSE;
		MagLayerMom = (momMag-elossMag)/momMag*MagLayerMom;
	}

	if(IsDebugOn()){
		double tof2OuterMag = 0.;
		for(int i=0;i<nMagStep;i++) tof2OuterMag += tofMagLayer[i];
		LOG_INFO<<" to OuterMag: pos x,y,z:"<<MagLayerPos.x()<<","<<MagLayerPos.y()<<","<<MagLayerPos.z()<<endm;
		LOG_INFO<<" to OuterMag: mom p,pt,eta,phi:"<<MagLayerMom.mag()<<","<<MagLayerMom.perp()<<","<<MagLayerMom.pseudoRapidity()<<","<<MagLayerMom.phi()<<endm;
		LOG_INFO<<" to OuterMag: tof:"<<tof2OuterMag<<endm;
	}

	double tof2MagOuter = -9999.;
	tof2MagOuter = tof2InnerEmc; 
	for(int i=0;i<nEmcStep;i++)  tof2MagOuter += tofEmcLayer[i];
	for(int i=0;i<nCoilStep;i++) tof2MagOuter += tofCoilLayer[i];
	for(int i=0;i<nMagStep;i++)  tof2MagOuter += tofMagLayer[i];

	double pathL2MagOuter = -9999.;
	if(IsDebugOn()) LOG_INFO<<" path to vertex("<< vertex.x()<<","<<vertex.y()<<","<<vertex.z()<<") = "<<pathL2Vtx<<endm;
	pathL2MagOuter = pathL2Vtx+rInnerEmc;
	if(IsDebugOn()) LOG_INFO<<" path to inner Emc  = "<<pathL2MagOuter<<endm;
	for(int i=0;i<nEmcStep;i++)  pathL2MagOuter += pathLEmcLayer[i];
	if(IsDebugOn()) LOG_INFO<<" path to outer Emc  = "<<pathL2MagOuter<<endm;
	for(int i=0;i<nCoilStep;i++) pathL2MagOuter += pathLCoilLayer[i];
	if(IsDebugOn()) LOG_INFO<<" path to outer Coil = "<<pathL2MagOuter<<endm;
	for(int i=0;i<nMagStep;i++)  pathL2MagOuter += pathLMagLayer[i];
	if(IsDebugOn()) LOG_INFO<<" path to outer mag  = "<<pathL2MagOuter<<endm;

	pathL = pathL2MagOuter;
	tof   = tof2MagOuter;
	pos   = MagLayerPos;
	if(TMath::IsNaN(MagLayerPos.x())||TMath::IsNaN(MagLayerPos.y())||TMath::IsNaN(MagLayerPos.z())||TMath::Abs(MagLayerPos.perp())>450.||TMath::Abs(MagLayerPos.z())>300.) return kFALSE;
	bField = GetFieldZ(MagLayerPos);
	//bField = 0.;
	outHelix = StPhysicalHelixD(MagLayerMom,MagLayerPos,bField*tesla,charge);
	if(IsDebugOn()){
		LOG_INFO<<"bField of magnet outside is "<<bField<<" from magMap = "<<GetFieldZ(MagLayerPos)<<" mom = "<<MagLayerMom.x()<<","<<MagLayerMom.y()<<","<<MagLayerMom.z()<<" mag = "<<MagLayerMom.mag()<<endm;
		LOG_INFO<<"------------ end of projection to magOutR ------------"<<endm;
	}
	return kTRUE;
}

void StMtdGeometry::ProjToVertex(const StPhysicalHelixD helix, const StThreeVectorD vertex, Double_t &pathL, Double_t &tof, StThreeVectorD &dcaPos){
	pathL  = -9999.;
	tof = -9999.;
	pathL  = TMath::Abs(helix.pathLength(vertex));
	dcaPos = helix.at(helix.pathLength(vertex));

	StThreeVectorD oh = helix.origin();
	if(TMath::IsNaN(oh.x())||TMath::IsNaN(oh.y())||TMath::IsNaN(oh.z())||TMath::Abs(oh.perp())>450.||TMath::Abs(oh.z())>300.) return;
	Double_t betaGam = helix.momentum(GetFieldZ(oh)).mag()/muonMass;
	Double_t v  = sqrt(betaGam*betaGam/(1+betaGam*betaGam))*c_light*1e-9;
	if(v!=0){
		tof = pathL/v;
	}else{
		tof = -9999.;
	}
}

Bool_t StMtdGeometry::ProjToBLModVect(const StPhysicalHelixD helix, IntVec &blVect, IntVec &modVect){

	blVect.clear();
	modVect.clear();

	Double_t R_mtd[2] = {mMtdMinR,mMtdMaxR};
	Double_t iBL[2] = {0,0};
	Double_t iMod[2] = {0,0};
	for(int i=0;i<2;i++) {
		Double_t s = helix.pathLength(R_mtd[i]).first;
		if(s<0.) s = helix.pathLength(R_mtd[i]).second;
		StThreeVectorD point = helix.at(s);
		Double_t phi = point.phi();
		Double_t z   = point.z();

		iBL[i]  = FindBLId(phi);
		iMod[i] = FindModId(z);
	}

	for (Int_t i = 0; i < 2; i++) {
		for(Int_t j=0;j<3;j++){
			Int_t idx = iBL[i]-1+j;
			if(idx>gMtdNBacklegs) idx -= gMtdNBacklegs;
			if(idx<1) idx += gMtdNBacklegs;
			if(idx>0&&idx<=gMtdNBacklegs)  blVect.push_back(idx);
		}
	}
	RemoveDuplicate(blVect);
	for (Int_t i = 0; i < 2; i++) {
		for(Int_t j=0;j<3;j++){
			Int_t idx = iMod[i]-1+j;
			if(idx>0&&idx<=gMtdNModules) modVect.push_back(idx);
		}
	}
	RemoveDuplicate(modVect);
	return kTRUE;
}

void StMtdGeometry::RemoveDuplicate(IntVec &vec){

	sort(vec.begin(),vec.end());
	Int_t tmpid = 0;
	for(IntVec::iterator tmpIter = vec.begin();tmpIter<vec.end();++tmpIter){
		if(tmpIter==vec.begin()){
			tmpid = *tmpIter;
			continue;
		}
		if(tmpid==*tmpIter){
			vec.erase(tmpIter);
			--tmpIter;
		}else{
			tmpid=*tmpIter;
		}
	}
}

Int_t StMtdGeometry::FindBLId(Double_t phi){

	Int_t iBL = -1;
	if(phi<0)  phi += 2.*(TMath::Pi()); // -pi,pi --> 0,2*pi
	double backLegPhiWidth = 8.*(TMath::Pi())/180.;   //rad,  8 degree per backLeg
	double backLegPhiGap   = 4.*(TMath::Pi())/180.;   //rad,  4 degree 
	double dphi = backLegPhiWidth+backLegPhiGap;
	iBL = (int)(phi/dphi);
	iBL+= 24;
	if(iBL>30) iBL-= 30;
	if(iBL<1||iBL>30){
		if(IsDebugOn()) LOG_WARN<<"Invalid BL id:"<<iBL<<" input phi = "<<phi<<endm;
		return -1;
	}

	return iBL;
}

Int_t StMtdGeometry::FindModId(Double_t z){

	Int_t iMod = -1;

	iMod = (int)((z+2.5*gMtdCellLength)/gMtdCellLength+1);

	if(iMod<0||iMod>6){
		if(IsDebugOn()) LOG_WARN<<"Invalid Module id:"<<iMod<<" input z = "<<z<<endm;
		return -1;
	}

	return iMod;
}

Bool_t StMtdGeometry::HelixCrossCellIds(const StPhysicalHelixD helix, IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec, DoubleVec &tofVec){
	const StThreeVectorD vertex(0,0,0);
	return HelixCrossCellIds(helix,vertex,idVec,pathVec,crossVec,tofVec);
}

Bool_t StMtdGeometry::HelixCrossCellIds(const StPhysicalHelixD helix, const StThreeVectorD vertex, IntVec &idVec, DoubleVec &pathVec, PointVec &crossVec, DoubleVec &tofVec){

	// input : helix
	// output: hit cell id, pathlength, cross point, tof

	// A. project to magnet outer radius
	StPhysicalHelixD outHelix;
	Double_t pathLToMagOutR;
	Double_t tofToMagOutR;
	StThreeVectorD posAtMagOutR;
	if(!ProjToMagOutR(helix,vertex,outHelix,pathLToMagOutR,tofToMagOutR,posAtMagOutR)) return kFALSE;

	// B. project to mtd inner and outer radius, get backleg and module candidates
	IntVec projBLVec;
	IntVec projModVec;
	if(!ProjToBLModVect(outHelix,projBLVec,projModVec)) return kFALSE;

	Int_t cellId=0;
	idVec.clear();
	pathVec.clear();
	crossVec.clear();
	tofVec.clear();

	// C. loop over all the candidate modules, find the hit cells and positions.
	for (UInt_t i = 0; i < projBLVec.size(); i++) {
		Int_t iBL = projBLVec[i];
		if(!mMtdGeoBackleg[iBL-1]) continue;
		for (UInt_t j = 0; j < projModVec.size(); j++) {
			Int_t iMod = projModVec[j];
			if(!mMtdGeoModule[iBL-1][iMod-1]) continue;
			Double_t pathToMtd = 0.;
			Double_t tofToMtd  = 0.;
			StThreeVectorD crossToMtd(0,0,0);
			if(IsDebugOn()){
				LOG_INFO<<" checking track cross BL = "<<iBL<<" Module = "<<iMod<<" or not ... "<<endm;
			}
			if(mMtdGeoModule[iBL-1][iMod-1]->HelixCross(outHelix,pathLToMagOutR,tofToMagOutR,pathToMtd,tofToMtd,crossToMtd)){
				Double_t global[3] = {crossToMtd.x(),crossToMtd.y(),crossToMtd.z()};	
				Double_t local[3] = {0,0,0};	
				mMtdGeoModule[iBL-1][iMod-1]->MasterToLocal(global,local);
				Int_t iCel = mMtdGeoModule[iBL-1][iMod-1]->FindCellId(local);
				if(iCel<-50) continue;
				if(iMod>3) iCel = 11-iCel;
				cellId = CalcCellId(iBL , iMod, iCel);
				if(IsDebugOn()){
					LOG_INFO<<" track cross BL = "<<iBL<<" Module = "<<iMod<< " iCel = "<<iCel<<endm;
					LOG_INFO<<" cellId = "<<cellId<<" pathToMtd = "<<pathToMtd<<" tofToMtd = "<<tofToMtd<<endm;
				}
				if(IsIdValid(cellId)){
					idVec.push_back(cellId);
					pathVec.push_back(pathToMtd);
					crossVec.push_back(crossToMtd);
					tofVec.push_back(tofToMtd);
				}
			}
		}
	}
	return kTRUE;
}

Int_t  StMtdGeometry::CalcCellId(Int_t iBL, Int_t iMod, Int_t iCel){
	Int_t cellId = -999;	
	if(iBL<1|| iBL>gMtdNBacklegs) return cellId; 
	if(iMod<1|| iMod>gMtdNModules) return cellId; 
	if(iCel<-mNExtraCells || iCel>11+mNExtraCells) return cellId; 
	cellId = iBL*1000+iMod*100+(iCel+50);

	return cellId;
}


Bool_t StMtdGeometry::IsIdValid(Int_t id){

	Int_t iBL   = id/1000;
	Int_t iMod  = (id%1000)/100;
	Int_t iCell = id%100-50;

	if(iBL<1 || iBL>gMtdNBacklegs) return kFALSE; 
	if(iMod<1 || iMod>gMtdNModules) return kFALSE; 
	if(iCell<-mNExtraCells || iCell>11+mNExtraCells) return kFALSE; 
	return kTRUE;
}

void StMtdGeometry::DecodeCellId(Int_t id, Int_t &iBL, Int_t &iMod, Int_t &iCell){

	iBL   = id/1000;
	iMod  = (id%1000)/100;
	iCell = id%100-50;

}

StThreeVectorD StMtdGeometry::GetField(StThreeVectorD pos) const{
	return GetField(pos.x(),pos.y(),pos.z());
}

StThreeVectorD StMtdGeometry::GetField(Double_t x, Double_t y, Double_t z) const{
	Double_t B[3] = {0,0,0};
	Double_t X[3] = {x,y,z};
	mStarBField->BField(X,B);
	for(int i=0;i<3;++i) B[i] /= 10.;
	return StThreeVectorD(B[0],B[1],B[2]);
}

Double_t StMtdGeometry::GetFieldZ(StThreeVectorD pos) const{
	StThreeVectorD bField = GetField(pos.x(),pos.y(),pos.z());
	return bField.z(); 
}

Double_t StMtdGeometry::GetFieldZ(Double_t x, Double_t y, Double_t z) const{
	StThreeVectorD bField = GetField(x,y,z);
	return bField.z(); 
}

StMtdGeoModule *StMtdGeometry::GetGeoModule(Int_t iBL, Int_t iMod) const{
	if(iBL>0&&iBL<=gMtdNBacklegs&&iMod>0&&iMod<=gMtdNModules){
		return mMtdGeoModule[iBL-1][iMod-1];
	}else{
		return 0;
	}
}

Bool_t 	StMtdGeometry::IsMTTG(const TGeoVolume * vol) const { 
	Bool_t found = false;
	for(int i=0;i<4;i++){
		if(!strcmp(vol->GetName(), backlegPref[i])){ 
			found = true;
			break;
		}
	}
	return found;
}

void   StMtdGeometry::SetLockBField(Bool_t val){
	mLockBField=val;
	SetBFactor(1);
}
