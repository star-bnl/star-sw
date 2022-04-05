/***************************************************************************
 *
 * $Id: StSvtDriftVelocityMaker.cxx,v 1.6 2004/01/26 23:10:49 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Drift Velocity calculation Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDriftVelocityMaker.cxx,v $
 * Revision 1.6  2004/01/26 23:10:49  perev
 * Leak off
 *
 * Revision 1.5  2003/09/02 17:59:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.4  2002/03/04 17:06:48  willson
 * Laser spot positions recorded
 *
 * Revision 1.3  2002/02/06 00:10:45  willson
 * File entries, variable names.
 *
 * Revision 1.2  2002/01/18 20:53:47  willson
 * Some bug fixes by Helen
 *
 * Revision 1.1  2002/01/18 19:57:42  willson
 * Drift Velocity Calculation v.1
 *
 *
 **************************************************************************/

#include <assert.h>
#include "StSvtDriftVelocityMaker.h"

#include "StMessMgr.h"

// #include "StSvtDb/StSvtDbMaker.h"


#include "StEventTypes.h"
#include "StEvent/StPrimaryVertex.h"
#include "StEvent/StEvent.h"
#include "StarClassLibrary/StSequence.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "StSvtClassLibrary/StSvtHybridDriftVelocity.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtHybridPixels.hh"
#include "StSvtClusterMaker/StSvtAnalysedHybridClusters.hh"
#include "Stiostream.h"
#include <fstream>

#define N_INJECTOR_LINES 4

ClassImp(StSvtDriftVelocityMaker)

//_____________________________________________________________________________
  StSvtDriftVelocityMaker::StSvtDriftVelocityMaker(const char *name):StMaker(name)
{
  int n = (char*)&mDebug - (char*)&mSvtData + sizeof(mDebug);
  memset(&mSvtData,0,n);

  mNumTimeBins = 128;
  mMaximumTB = 128;
  mMinimumTB = 0;
  mDebug = false;
  mFraction = 0.5;
  mMoveForward = true;
  mT0Guess = 0.5;
}

//_____________________________________________________________________________
StSvtDriftVelocityMaker::~StSvtDriftVelocityMaker()
{
  delete mSvtDriftVeloc; mSvtDriftVeloc=0;
  for (int i=0;i<mNHybridDriftVelocityHisto;i++) {
    delete mHybridDriftVelocityHisto[i]; 
    delete mHybridDriftVelocity2DHisto[i];}
  delete [] mHybridDriftVelocityHisto;
  delete [] mHybridDriftVelocity2DHisto;

  delete mGlobalDriftVelocityHisto; 
  delete mGlobalDriftVelocity2DHisto; 
  delete mCalculatedDriftVelocity; 
  delete mLaserSpotDistL07B3_1; 
  delete mLaserSpotDistL15B3_1; 
  delete mLaserSpotDistL15B3_2; 

}

//_____________________________________________________________________________
Int_t StSvtDriftVelocityMaker::Init()
{
  //cout << "StSvtDriftVelocityMaker::Init" << endl;

  SetSvtRawData();

  SetSvtDriftVelocity();

  int indexHybrid;
  StSvtHybridDriftVelocity* hybridDriftVeloc;
  TH1D* hybridHisto;
  TH2D* hybrid2DHisto;
  char CharString1[100];
  char CharString2[100];
  mNHybridDriftVelocityHisto  = mSvtRawData->getTotalNumberOfHybrids();
  mHybridDriftVelocityHisto = new TH1D*[mNHybridDriftVelocityHisto];
  
  if (mDebug)
    mHybridDriftVelocity2DHisto = new TH2D*[mNHybridDriftVelocityHisto];

  // Loop over barrels, ladders, wafers and hybrids
  for (int barrel = 1;barrel <= mSvtRawData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtRawData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtRawData->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= mSvtRawData->getNumberOfHybrids();hybrid++) {

	  indexHybrid = mSvtRawData->getHybridIndex(barrel, ladder, wafer, hybrid);

	  if (indexHybrid < 0) continue;

	  hybridDriftVeloc = (StSvtHybridDriftVelocity*)mSvtRawData->at(indexHybrid);
	  if (!hybridDriftVeloc)
	    hybridDriftVeloc = new StSvtHybridDriftVelocity(barrel, ladder, wafer, hybrid);
	    
	  mSvtRawData->put_at(hybridDriftVeloc,indexHybrid);

	  sprintf(CharString1, "Histo%d", indexHybrid);
	  sprintf(CharString2, "2DHisto%d", indexHybrid);

	  hybridHisto = new TH1D(CharString1, CharString1, mNumTimeBins, mMinimumTB, mMaximumTB);
	  mHybridDriftVelocityHisto[indexHybrid] = hybridHisto;

	  if (mDebug) {
	    hybrid2DHisto = new TH2D(CharString1, CharString1, 128, 0, 128, 240, 0, 240);
	    mHybridDriftVelocity2DHisto[indexHybrid] = hybrid2DHisto;
	  }

	}  // end of loop over hybrids
      }  // end of loop over wafers
    }  // end of loop over ladders
  }  // end of loop over barrels
  
  if (mDebug) {
    mGlobalDriftVelocityHisto = new TH1D("Global1", "Global1", 640, 0, 128); 
    mGlobalDriftVelocity2DHisto = new TH2D("Global2", "Global2", 640, 0, 128, 1200, 0, 240); 
  }
    
  mCalculatedDriftVelocity = new TH1D("Final", "Final", 432, 0, 431);
  mLaserSpotDistL07B3_1 = new TH1D("L07B3 Laser Distribution 1", "L07B3 Laser Distribution 1", 1280, 0, 128); 
  mLaserSpotDistL15B3_1 = new TH1D("L15B3 Laser Distribution 1", "L15B3 Laser Distribution 1", 1280, 0, 128); 
  mLaserSpotDistL15B3_2 = new TH1D("L15B3 Laser Distribution 2", "L15B3 Laser Distribution 2", 1280, 0, 128); 

  return StMaker::Init();
}
//______________________________________________
Int_t StSvtDriftVelocityMaker::SetSvtRawData()
{
  St_DataSet *dataSet;
  
  dataSet = GetDataSet("StSvtRawData");
  if( !dataSet) {
    gMessMgr->Warning() << " No Svt Raw data set" << endm;
    dataSet = new St_ObjectSet("DriftVelocity");
    mSvtRawData = new StSvtData("FULL");
    return kStWarn;
  }
    

  mSvtRawData = (StSvtData*)(dataSet->GetObject());
  if( !mSvtRawData) {
    gMessMgr->Warning() << " No Svt Raw data " << endm;
    return kStWarn;
  }
  
  return kStOK;
}
//_____________________________________________________________________________
Int_t StSvtDriftVelocityMaker::SetSvtData()
{
  St_DataSet *dataSet;

  dataSet = GetDataSet("StSvtAnalResults");
  if (dataSet) mRaw = true; else
    mRaw = false;

  if (mRaw) {
    mSvtData = (StSvtHybridCollection*)(dataSet->GetObject());
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDriftVelocityMaker::SetSvtDriftVelocity()
{
  assert(!mSvtDriftVeloc);
  if (mSvtRawData)
    mSvtDriftVeloc = new StSvtHybridCollection(mSvtRawData->getConfiguration());
  else
    mSvtDriftVeloc = new StSvtHybridCollection("FULL");

    return kStOK;  
}

//_____________________________________________________________________________
Int_t StSvtDriftVelocityMaker::Make()
{
  cout << "StSvtDriftVelocityMaker::Make" << endl;
  
  SetSvtData();

  if (mRaw)
    FillHistogramsRaw();
  else
    FillHistogramsStEvent();

  cout << "Current Event  Number is " << ((StSvtData*)mSvtRawData)->getEventNumber() << endl;
  cout << "Current Run Number is " << ((StSvtData*)mSvtRawData)->getRunNumber() << endl;


  //WriteToDb("2000-02-03 07:31:00");

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDriftVelocityMaker::FillHistogramsRaw()
{

  //cout << "StSvtDriftVelocityMaker::FillHistogramsRaw" << endl;
  StSvtAnalysedHybridClusters* hybridData;
  //StSvtHybridDriftVelocity* hybridDriftVeloc;

  int indexHybrid, index, nHits;

  mEventCounter++;  // increase counter

  // Loop over barrels, ladders, wafers and hybrids
  for (int barrel = 1;barrel <= mSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtData->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= mSvtData->getNumberOfHybrids();hybrid++) {

	  indexHybrid = mSvtData->getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;

	  // Get hybrid data
	  hybridData = (StSvtAnalysedHybridClusters*)(mSvtData->at(indexHybrid));

	  if (!hybridData) continue;

	  nHits = hybridData->numOfHits();
	  mHitCounter += nHits;

	  for (index=0; index<nHits; index++) {
	    
	    if (accept(hybridData->svtHit())) {
	      mHybridDriftVelocityHisto[indexHybrid]->Fill(hybridData->WaferPosition()[index].x());
	      
	      if (indexHybrid==293) {
		if (hybridData->WaferPosition()[index].y()>196.5 && hybridData->WaferPosition()[index].y()<200.5 && hybridData->WaferPosition()[index].x()>60)
		  mLaserSpotDistL07B3_1->Fill(hybridData->WaferPosition()[index].x());
	      }

	      if (indexHybrid==416) {
		if (hybridData->WaferPosition()[index].y()>198.5 && hybridData->WaferPosition()[index].y()<200.5 && hybridData->WaferPosition()[index].x()>60)
		  mLaserSpotDistL15B3_1->Fill(hybridData->WaferPosition()[index].x());
		if (hybridData->WaferPosition()[index].y()>196.5 && hybridData->WaferPosition()[index].y()<198.5 && hybridData->WaferPosition()[index].x()>60)
		  mLaserSpotDistL15B3_2->Fill(hybridData->WaferPosition()[index].x());
	      }

	      if (mDebug) {
		mHybridDriftVelocity2DHisto[indexHybrid]->Fill(hybridData->WaferPosition()[index].x(), hybridData->WaferPosition()[index].y());
		mGlobalDriftVelocityHisto->Fill(hybridData->WaferPosition()[index].x());
		mGlobalDriftVelocity2DHisto->Fill(hybridData->WaferPosition()[index].x(), hybridData->WaferPosition()[index].y());
	      }

	    }
	  }	  
	}  // end of loop over hybrids
      }  // end of loop over wafers
    }  // end of loop over ladders
  }  // end of loop over barrels
      
  return kStOK;
}


//_____________________________________________________________________________
Int_t StSvtDriftVelocityMaker::FillHistogramsStEvent()
{
  //cout << "StSvtDriftVelocityMaker::FillHistogramsStEvent" << endl;
  //StSvtHybridData* hybridData;
  //StSvtHybridDriftVelocity* hybridDriftVeloc;
  
  //
  //	Get pointer to StEvent
  //
  StEvent* event;
  event = (StEvent *) GetInputDS("StEvent");

  cout << "StEvent = " << event << endl;

  if (!event) return kStOK;        // if no event, we're done
  
  StSvtConfig* fSvtConfig = new StSvtConfig();
  fSvtConfig->setConfiguration("FULL");
  //
  //  See if this event survives the event filter.
  //  If not we stop here right away.
  //
  if (!accept(event)) return kStOK;  
  
  mEventCounter++;  // increase counter
 
  // Get pointers to svt hit collection 
  
  StSvtHitCollection* rSvtHitColl = event->svtHitCollection();
  
  cout << "Number of hits = " << rSvtHitColl->numberOfHits() << "." << endl;
  mHitCounter += rSvtHitColl->numberOfHits();

  StSvtHit* sCurrentHit;

  for (unsigned int br=0; br<rSvtHitColl->numberOfBarrels(); br++) {
    for (unsigned int ld=0; ld<rSvtHitColl->barrel(br)->numberOfLadders(); ld++) {
      for (unsigned int w=0; w<rSvtHitColl->barrel(br)->ladder(ld)->numberOfWafers(); w++) {
	StSPtrVecSvtHit& hits = rSvtHitColl->barrel(br)->ladder(ld)->wafer(w)->hits();
	for (StSvtHitIterator i = hits.begin(); i != hits.end(); i++) {
	  sCurrentHit = (*i);
	  if( accept(sCurrentHit)) {
	    mHybridDriftVelocityHisto[sCurrentHit->index()]->Fill(sCurrentHit->timebucket());
	    if (mDebug)
	      mGlobalDriftVelocityHisto->Fill(sCurrentHit->timebucket());
	  }
	}
      }
    } 
  }
      
  return kStOK;

}


//_____________________________________________________________________________
Int_t StSvtDriftVelocityMaker::CalcDriftVelocity()
{

  double Aver=0, Num=0;
  char filename[100];
  Int_t Offset= (mMaximumTB - mMinimumTB)/10;
  Int_t Start = Offset + mMinimumTB;
  Int_t End   = mMaximumTB - Offset;
  Int_t i, j;
  double DriftVelocity, TotalDriftTime;
  ofstream file;

  sprintf(filename, "Run%d_DV.out", mSvtRawData->getRunNumber());

  file.open(filename);

  for (i=0; i<mSvtDriftVeloc->getTotalNumberOfHybrids(); i++) {
    
    for (j=Start; j<End; j++) {
      Aver += mHybridDriftVelocityHisto[i]->GetBinContent(j);
      Num++;
    }
    
    Aver /= Num;

    Aver *= mFraction;
    
    if (mMoveForward) {
      for (j=mMaximumTB/2; j<mMaximumTB+1; j++) {
	if (mHybridDriftVelocityHisto[i]->GetBinContent(j) < Aver) 
	  break;
      }
    }

    else {
      for (j=mMaximumTB-2; j>mMaximumTB/2; j--) {
	if (mHybridDriftVelocityHisto[i]->GetBinContent(j) > Aver) 
	  break;
      }
    }
    
      

    if (mDebug)
      cout << j << endl;

    TotalDriftTime = (j-mMinimumTB-mT0Guess)*(128.0/mNumTimeBins)*0.00000004;

    DriftVelocity = 3.0/TotalDriftTime;

    if (mDebug)
      mCalculatedDriftVelocity->Fill(i, DriftVelocity);
  
    file << i << "\t" << DriftVelocity << "\t\t" << j*128.0/mNumTimeBins << "\n";
    
    Aver=Num=0;

  }

  file << "Laser Spot Time Bucket Positions:\n";

  file << "L07B3_1" << "\t" << mLaserSpotDistL07B3_1->GetMean() << "\n";
  file << "L15B3_1" << "\t" << mLaserSpotDistL15B3_1->GetMean() << "\n";
  file << "L15B3_2" << "\t" << mLaserSpotDistL15B3_2->GetMean() << "\n";

  file.close();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDriftVelocityMaker::GetInjectorLine(float peak)
{
  return 0;
}

//_____________________________________________________________________________
Int_t  StSvtDriftVelocityMaker::GetInjectorLine(float* peak)
{
  return 0;
}

//_____________________________________________________________________________
Float_t  StSvtDriftVelocityMaker::GetClosestToLine(float peak1, float peak2)
{
  return 0;
}

//_____________________________________________________________________________
Float_t  StSvtDriftVelocityMaker::GetTimeZero(int anode)
{
  return 0;
}

//_____________________________________________________________________________
Float_t  StSvtDriftVelocityMaker::GetDistanceInjectorLine(int line)
{
  return 0;
}

//_____________________________________________________________________________
Float_t  StSvtDriftVelocityMaker::FitVelocity(int nInjectorsFired, float* peak, float t0)
{
  return 0;
}

//_____________________________________________________________________________
Float_t  StSvtDriftVelocityMaker::CalcV1(int anode, float velocity)
{
  return 0;
}

//_____________________________________________________________________________
Float_t  StSvtDriftVelocityMaker::CalcV2(int anode, float velocity)
{
  return 0;
}

//_____________________________________________________________________________
Float_t  StSvtDriftVelocityMaker::CalcV3(int anode, float velocity)
{
  return 0;
}

//_____________________________________________________________________________
Int_t  StSvtDriftVelocityMaker::FillAllAnodes()
{
  return 0;
}

//_____________________________________________________________________________
Int_t  StSvtDriftVelocityMaker::WriteToDb(Text_t *timestamp)
{
  cout << "StSvtDriftVelocityMaker::WriteToDb" << endl;

  if (!GetMaker("SvtDb")) {
    gMessMgr->Warning() << " SVT database was not instantiated. Impossible to write! " << endm;
    return kStWarn;
  }

  //if (timestamp)
  //  ((StSvtDbMaker*)GetMaker("SvtDb"))->setTimeStamp(timestamp);

  //((StSvtDbMaker*)GetMaker("SvtDb"))->writeSvtDriftVelocity(mSvtDriftVeloc);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDriftVelocityMaker::Finish()
{
  //cout << "StSvtDriftVelocityMaker::Finish" << endl;

  CalcDriftVelocity();

  Int_t indexHybrid;

  if (!mDebug) {
    // Loop over barrels, ladders, wafers and hybrids
    for (int barrel = 1;barrel <= mSvtDriftVeloc->getNumberOfBarrels();barrel++) {
      for (int ladder = 1;ladder <= mSvtDriftVeloc->getNumberOfLadders(barrel);ladder++) {
	for (int wafer = 1;wafer <= mSvtDriftVeloc->getNumberOfWafers(barrel);wafer++) {
	  for (int hybrid = 1;hybrid <= mSvtDriftVeloc->getNumberOfHybrids();hybrid++) {
	    
	    indexHybrid = mSvtDriftVeloc->getHybridIndex(barrel, ladder, wafer, hybrid);
	    
	    if (indexHybrid < 0) continue;
	    
	    delete mHybridDriftVelocityHisto[indexHybrid];
	    
	  }  // end of loop over hybrids
	}  // end of loop over wafers
      }  // end of loop over ladders
    }  // end of loop over barrels
    
    delete [] mHybridDriftVelocityHisto;
  }
  
  return kStOK;
}

bool StSvtDriftVelocityMaker::accept(StEvent* event)
{
    //
    //  This is a kind of very simple event filter.
    //  We select only events with a valid event vertex,
    //  i.e. event->primaryVertex() returns a non-zero pointer.
    // and  with the primary vertex under the svt

  if( !(event->primaryVertex())) return 0;
  //cout << event->primaryVertex()->position().z() << endl;GetGlobalHisto()
  //if( fabs(event->primaryVertex()->position().z()) < 20) return 1;
  
  return 1;
}

bool StSvtDriftVelocityMaker::accept(StSvtHit* hit)
{
  //Accept hits with flag < 4 and peak ADC>15


  if( hit->flag() < 100) return 1;
  return 0;
}
