//#define DEBUG_DbUtil
#include "StTpcHitMoverMaker.h"

#include "StMessMgr.h"

#include "tables/St_tcl_tphit_Table.h"

#include "StDbUtilities/StMagUtilities.h"
#include "StDbUtilities/StSectorAligner.h"
#include "StDbUtilities/StCoordinates.hh"

#include "StEventTypes.h"
#include "TMath.h"
StTpcHitMover::StTpcHitMover(const Char_t *name) : StMaker(name), 
						   mAlignSector(kFALSE),
						   mSectorAligner(NULL),
						   mExB(NULL) {
  gMessMgr->Info("StTpcHitMover::StTpcHitMover: constructor called");
  setInputDataSetName("tpc_hits");
  setInputHitName("tphit");
  setOutputMode(0);
}

StTpcHitMover::~StTpcHitMover() {
  FlushDB();
}

Int_t StTpcHitMover::Init() {

  TString giHN  = getInputHitName();
  TString giDSn = getInputDataSetName();

  gMessMgr->Info() << "StTpcHitMover::Init() - reading hits from "  <<
    giDSn << ":" << giHN << endm;

  gMessMgr->Info() << "StTpcHitMover::Init() - sector align:     "  <<
    mAlignSector   << endm;

  gMessMgr->Info() << "StTpcHitMover::Init() - ExB corrections:  " <<
    (TMath::Abs(m_Mode) & 0x01)<< endm;

  gMessMgr->Info() << "StTpcHitMover::Init() - mag utils options " <<
    ((TMath::Abs(m_Mode) & 0x3FFE) >> 1) << endm;

  return StMaker::Init();
}

Int_t StTpcHitMover::InitRun(Int_t runnumber) {
  FlushDB();
  return kStOk;
}


void StTpcHitMover::FlushDB() {
  if (mSectorAligner) delete mSectorAligner; mSectorAligner= 0;
  if (mExB)           delete mExB;           mExB          = 0;

}


Int_t StTpcHitMover::Make() {
  if (TMath::Abs(m_Mode) & 0x01) {
    // option handling needs some clean up, but right now we stay compatible
    Int_t option = (TMath::Abs(m_Mode) & 0x7FFFFFFE) >> 1;
    if (! mExB ) {
      TDataSet *RunLog = GetDataBase("RunLog");
      mExB = new StMagUtilities(gStTpcDb, RunLog, option);
    }
  }
  StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if ( pEvent) {
    gMessMgr->Info() << "StTpcHitMover::Make use StEvent " << endm;
    if (! gStTpcDb) {
      gMessMgr->Error() << "StTpcHitMover::Make TpcDb has not been instantiated " << endm;
      return kStErr;
    }
    StTpcCoordinateTransform transform(gStTpcDb);
    StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
    if (TpcHitCollection) {
      UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
      StTpcLocalSectorCoordinate local;
      StTpcLocalSectorCoordinate  coorLS; 
      StTpcLocalCoordinate  coorLT, coorLTD;
      StTpcLocalSectorAlignedCoordinate  coorLSA;
      StGlobalCoordinate    coorG;
      for (UInt_t i = 0; i< numberOfSectors; i++) {
	StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
	if (sectorCollection) {
	  Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	  for (int j = 0; j< numberOfPadrows; j++) {
	    StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	    if (rowCollection) {
	      StSPtrVecTpcHit &hits = rowCollection->hits();
	      UInt_t NoHits = hits.size();
	      if (NoHits) {
		for (UInt_t k = 0; k < NoHits; k++) {
		  StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[k]);
		  if (m_Mode < 0 && tpcHit->idTruth() && tpcHit->qaTruth() > 95) continue; // don't move embedded hits
		  StTpcLocalCoordinate  coorL(tpcHit->position().x(),tpcHit->position().y(),tpcHit->position().z(),i+1,j+1);
		  transform(coorL,coorLS);   // to sector 12
		  transform(coorLS,coorLSA); // alignment 
		  transform(coorLSA,coorLT); //
		  coorLTD = coorLT;          // distortions
		  // ExB corrections
		  Float_t pos[3] = {coorLTD.position().x(),coorLTD.position().y(),coorLTD.position().z()};
		  if ( mExB ) {
		    Float_t posMoved[3];
		    mExB->UndoDistortion(pos,posMoved);   // input pos[], returns posMoved[]
		    StThreeVector<double> newPos(posMoved[0],posMoved[1],posMoved[2]);
		    coorLTD.setPosition(newPos);
		  }
		  transform(coorLTD,coorG);
		  StThreeVectorF xyzF(coorG.position().x(),coorG.position().y(),coorG.position().z());
#ifdef DEBUG_DbUtil
		  Float_t x[3] = {tpcHit->position().x(),tpcHit->position().y(),tpcHit->position().z()};
		  Float_t xprime[3];
		  moveTpcHit(x,xprime,i+1,j+1);
		  Float_t xA[3] = {xyzF.x() - xprime[0],
				   xyzF.y() - xprime[1],
				   xyzF.z() - xprime[2]
				     };
		  Double_t dev = xA[0]*xA[0] + xA[1]*xA[1] +  xA[2]*xA[2];
		  if (dev > 1.e-6) {
		    StTpcPadCoordinate coorP;
		    transform(coorLS,coorP);
		    StTpcLocalSectorCoordinate  coorLSR; // reference for sector/row/pad
		    transform(coorP,coorLSR);
		    cout << "sector/row " << i+1 << "/" << j+1 
			 << "\t xyzFCF\t" << pos[0] << "\t" << pos[1] << "\t" << pos[2] << endl;
		    cout << "coorL " << coorL << endl;
		    cout << "coorLS " <<  coorLS <<endl;
		    cout << "coorP " << coorP << endl;
		    cout << "coorLSR " << coorLSR <<endl;
		    cout << "coorLSA " << coorLSA << endl;
		    cout << "coorLT " << coorLT << endl;
		    cout << "coorLTD " << coorLTD << endl;
		    cout << "coorG " << coorG << endl;
		    cout << "xprime/xA";
		    for (int i = 0; i < 3; i++) 
		      cout << "\t" << xprime[i] << "/" << xA[i];
		    cout << "\tdev\t" << dev << endl;
		  }
#endif		  
		  tpcHit->setPosition(xyzF);
		}
	      }
	    }
	  }
	}
      }
    }
    
    return kStOK;        
  }
  
  St_DataSet *tpc_data = GetInputDS(mInputDataSetName);
  if (!tpc_data){
    // no TPC ???
    gMessMgr->Warning() << "StTpcHitMover::Make: no " << mInputDataSetName << endm;
    return kStOk;   
  }

  //tpc_data->ls(4);
  //(void) printf("DEBUG1 :: [%s]\n",mInputHitName.Data());

  St_DataSetIter gime(tpc_data);
  St_tcl_tphit *tphit = (St_tcl_tphit *) gime(mInputHitName);
  if (!tphit){
    // no cluster data?
    gMessMgr->Warning() << "StTpcHitMover::Make: no " << mInputHitName     << endm;
    return kStWarn; 
  }
  
  tcl_tphit_st* spc = tphit->GetTable();
  if (!spc)      return kStWarn;

  gMessMgr->Info() << "StTpcHitMover::Make: Input hit table size is " << 
    (int) tphit->GetNRows() << " m_Mode=" << m_Mode << endm;

  Float_t x[3];
  Float_t xprime[3];
  for (Int_t i = 0; i < tphit->GetNRows(); i++, spc++) {
    if (m_Mode < 0 && spc->id_simtrk && spc->id_quality > 95) continue; // don't move embedded hits
    short sector = short(spc->row/100.);
    short row = spc->row-sector*100;
    x[0] = spc->x;
    x[1] = spc->y;
    x[2] = spc->z;
    
    moveTpcHit(x,xprime,sector,row);
    
    if (mOutputMode == 0) {
      spc->x = xprime[0];
      spc->y = xprime[1];
      spc->z = xprime[2];
    }
  }
  return kStOk;
}

Int_t StTpcHitMover::Finish() {
  return kStOk;
}

void StTpcHitMover::setInputDataSetName(const Char_t *inputDataSetName) {
  if (inputDataSetName) {
    mInputDataSetName = inputDataSetName;
  }
}

void StTpcHitMover::setInputHitName(const Char_t *inputHitName) {
  if (inputHitName) {
    mInputHitName = inputHitName;
  }
}

void StTpcHitMover::setOutputMode(Int_t mode) {
  /*!
    Controls output options:
    0 == tpt compatible mode (overwrite table)
    1 == jeromes risky StEvent fill mode (fill hits directly into StEvent)
  */
  if ((mode >= 0) && (mode <= 1)) {
    mOutputMode = mode;
  }
}

void StTpcHitMover::moveTpcHit(Float_t pos[3], Float_t posMoved[3],
			       Short_t sector, Short_t row) {
  // initialize variables
  posMoved[0] = pos[0];
  posMoved[1] = pos[1];
  posMoved[2] = pos[2];

  // align sector
  if (mAlignSector) { 
    if (! mSectorAligner) mSectorAligner = new StSectorAligner(gStTpcDb);
    mSectorAligner->moveHit(pos,posMoved,sector,row);
    pos[0] = posMoved[0];
    pos[1] = posMoved[1];
    pos[2] = posMoved[2];
  }

  // ExB corrections
  if ( mExB ) {
    mExB->UndoDistortion(pos,posMoved);   // input pos[], returns posMoved[]
    pos[0] = posMoved[0];
    pos[1] = posMoved[1];
    pos[2] = posMoved[2];
  }

  // transformation to global coordinates
  StTpcCoordinateTransform transform(gStTpcDb);

  StTpcLocalCoordinate local(pos[0],pos[1],pos[2],sector,row);
  StGlobalCoordinate global;
  transform(local,global);

  posMoved[0] = global.position().x();
  posMoved[1] = global.position().y();
  posMoved[2] = global.position().z();
} 

ClassImp(StTpcHitMover)
