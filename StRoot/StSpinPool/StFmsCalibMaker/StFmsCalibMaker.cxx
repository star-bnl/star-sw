#include "StFmsCalibMaker.h"
#include "StFmsCalibMakerQa.h"
ClassImp(StFmsCalibMaker);

#include "Stypes.h"
#include "StMessMgr.h"
#include "StLorentzVectorF.hh"

#include "StEnumerations.h"
#include "StEventTypes.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFmsCollection.h"
#include "StEvent/StFmsHit.h"
#include "StEvent/StFmsPoint.h"
#include "StEvent/StFmsPointPair.h"
#include "StEvent/StTriggerData.h"
#include "StEvent/StTriggerId.h"
#include "StFmsDbMaker/StFmsDbMaker.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "Stypes.h"

#include <TFile.h>
#include <TTree.h>
#include <TLeaf.h>
#include <TH1.h>
#include <TH2.h>
#include <TString.h>

#include <cmath>
#include <iostream>
#include <fstream>
#include <map>
using namespace std;

//--------------------------------------------------
void StFmsCalibMaker::ReadCellStat(const char* list)
{
	mReadCellStat = true;
	cout <<Form("Reading cell status from %s...", list) <<endl;

	int detId, ch, stat, nBad = 0, nDead = 0;
	ifstream in;
	in.open(list);
	if (!in.is_open())
	{
		LOG_ERROR <<"StFmsCalibMaker::ReadCellStat - cannot find the list!\n" <<endm;
		return;
	}
	while (in.is_open())
	{
		in >> detId >> ch >> stat; //0 for not bad/dead, 1 for bad, 2 for dead, and 9 for converged
		if (!in.good()) break;

		mCellStat[detId-8].insert(std::pair<int, int>(ch, stat));
		if (stat==BAD)  { cout <<Form("%2i %3i, %i", detId, ch, stat) <<endl; nBad++;  }
		if (stat==DEAD) { cout <<Form("%2i %3i, %i", detId, ch, stat) <<endl; nDead++; }
	}
	in.close();
	in.clear();
	cout <<Form("Total # of bad/dead channels: %i/%i \n", nBad, nDead) <<endl;

	return;
}//ReadCellStat

//-----------------------------------------------------------------------------------------
Int_t StFmsCalibMaker::CheckFmsTrigger(const StTriggerId& trigId, const int mFmsTrigIdBase)
{
    int trigFired = 0;

    //Loop over RUN15 FMS triggers
    for (int a=0; a<4;  a++) //4 periods
    for (int b=0; b<3;  b++) //3 possible versions
    for (int c=0; c<13; c++) //13 triggers: SmBS1,SmBS2,SmBS3, LgBS1,LgBS2,LgBS3, DiBS, JP2,JP1,JP0,DiJP, "",LED
    {
        int tempId = mFmsTrigIdBase + 10000*a + 20*b + 1+c;
        if (trigId.isTrigger(tempId)) trigFired |= (1 << c);
    }

    return trigFired;
}//CheckFmsTrigger

//-------------------------------------------------------------
Int_t StFmsCalibMaker::ReadBbcSlewing(const char* filename_bbc) //Written by Oleg Eyser
{
	mApplyBbcZvtx = true; //CKim

    // reading parameters for BBC slewing correction
    char s[100];
    int iew, ipmt;
    float ca, cb, cc;

    FILE *pFile = fopen( filename_bbc, "read" );
    fgets( s, 100, pFile );
    for( int ew=0; ew<2; ew++ )
    for( int p=0; p<16; p++ )
    {
        fscanf( pFile, " %d %d %f %f %f \n", &iew, &ipmt, &ca, &cb, &cc);
        if ( ew==iew && p+1==ipmt )
        {
            mBbcSlew[ew][p][0] = ca;
            mBbcSlew[ew][p][1] = cb;
            mBbcSlew[ew][p][2] = cc;
        }
        else return kError;
    }
    fclose( pFile );

    cout << "BBC slewing: z(A+B/[C+adc])" << endl;
    for( int ew=0; ew<2; ew++ )
    {
        if( ew==0 ) cout << " East" << endl;
        if( ew==1 ) cout << " West" << endl;
        for( int p=0; p<16; p++ )
        {
            cout << Form("PMT%2d - %7.2f %7.2f %7.2f ",
					p+1, mBbcSlew[ew][p][0], mBbcSlew[ew][p][1], mBbcSlew[ew][p][2]) << endl;
        }
    }
	cout <<endl;

    return kStOK;
}//ReadBbcSlewing

//--------------------------------------------------------------------
Float_t StFmsCalibMaker::GetBbcZCorr(const StTriggerData* triggerData) //Written by Oleg Eyser, Modified by CKim
{
	//DO NOT use muDST->event()->bbcTriggerDetector() -- obsolete!!!
	Float_t bbcZ     = -999.;
	Float_t bbcTdiff = -999.;
	UShort_t tdc1east, tdc1west;
	UShort_t pmt1east, pmt1west;
	UShort_t adc1east, adc1west;
	unsigned int tdcMatchEast = 0;
	unsigned int tdcMatchWest = 0;
	bbcTdiff = (float)triggerData->bbcTimeDifference();
	tdc1east = triggerData->bbcEarliestTDC(east);
	tdc1west = triggerData->bbcEarliestTDC(west);

	//Compare TDC values to find earliest PMT (east/west)
	for ( int i=1; i<=16; i++ )
	{
		if ( tdc1east==triggerData->bbcTDC(east, i) )
		{
			adc1east = triggerData->bbcADC(east, i);
			pmt1east = i-1;
			++tdcMatchEast;
		}
		if ( tdc1west==triggerData->bbcTDC(west, i) )
		{
			adc1west = triggerData->bbcADC(west, i);
			pmt1west = i-1;
			++tdcMatchWest;
		}
	}

	//BBC slewing correction (east/west)
	if ( tdcMatchEast==1 && tdcMatchWest==1 )
	{
		Float_t zEast = -0.3 * ( bbcTdiff
								 - mBbcSlew[0][pmt1east][0]
								 - mBbcSlew[0][pmt1east][1]/(mBbcSlew[0][pmt1east][2] + adc1east) );
		Float_t zWest = -0.3 * ( bbcTdiff
								 - mBbcSlew[1][pmt1west][0]
								 - mBbcSlew[1][pmt1west][1]/(mBbcSlew[1][pmt1west][2] + adc1west) );
		bbcZ = (zEast + zWest)/2.0;
	}

	return bbcZ;
}//GetBbcZCorr

//--------------------------------------------------------------------------------------------
Float_t StFmsCalibMaker::GetBbcZCorrMass(StFmsPointPair* pair, Float_t bbcZ, bool returnOpenA)
{
	const Float_t e0 = pair->point(0)->energy();
	const Float_t e1 = pair->point(1)->energy();
	const Float_t x0 = pair->point(0)->XYZ().x();
	const Float_t x1 = pair->point(1)->XYZ().x();
	const Float_t y0 = pair->point(0)->XYZ().y();
	const Float_t y1 = pair->point(1)->XYZ().y();
	const Float_t z0 = pair->point(0)->XYZ().z() - bbcZ;
	const Float_t z1 = pair->point(1)->XYZ().z() - bbcZ;

	const Float_t dist = sqrt(pow(x0 - x1, 2) + pow(y0 - y1, 2) + pow(z0 - z1, 2));
	const Float_t zAvg = (z0 + z1)/2;

	const Float_t corrOpenA = 2 * atan(dist / (2*zAvg));
	const Float_t corrMass  = sqrt(2 * e0 * e1 * (1 - cos(corrOpenA)));

	if (returnOpenA) return corrOpenA;
	return corrMass;
}//GetBbcZCorrMass

//---------------------------
Int_t StFmsCalibMaker::Init()
{
	mFmsDbMk = static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));
	if (!mFmsDbMk)
	{
		LOG_ERROR <<"StFmsCalibMaker::InitRun - !StFmsDbMaker" <<endl;
		return kStFatal;
	}

	return kStOk;
}//Init

//---------------------------------------
Int_t StFmsCalibMaker::InitRun(int runNo)
{
	Info("InitRun", "Start run %i...", runNo);
	mRunNo = runNo;

	mFile = new TFile(mOutName, "RECREATE");
	mFile->SetCompressionLevel(9);
	mFile->cd();

	//Masked channels xcheck, Get histograms
	for (int a=0; a<4; a++)
	{
		const int detId = a+8;
		const int maxCh = mFmsDbMk->maxChannel(detId);

		//Printout gainCorr/cellStat, and check if bad marked cells are masked out properly
		for (int b=0; b<maxCh; b++)
		{
			const int   ch       = b+1;
			const float gain     = mFmsDbMk->getGain(detId, ch);
			const float gainCorr = mFmsDbMk->getGainCorrection(detId, ch);
			if (gain==0) continue; //Cells physically not exist

			const char* stat = "";
			if      (mCellStat[a][ch]==BAD)  stat = "BAD";
			else if (mCellStat[a][ch]==DEAD) stat = "DEAD";
			cout <<Form("%2i %3i %4.3f %s", detId, ch, gainCorr, stat) <<endl;

			//Enforce stop if any bad marked cell alive (nonzero gainCorr)
			if (mReadCellStat==true && mCellStat[a][ch]==BAD &&	gainCorr!=0.000)
			{
				cout <<Form("\nWARNING!!! Bad marked cell's gainCorr is alive: d%i ch%i\n", detId, ch) <<endl;
				return kStStop;
			}
		}//b, ch (all)

		//Histograms (essential, to be used in calibration process)
		mH2_mass[a] = new TH2F(Form("mass_d%i", detId), ";ch;mass", maxCh,0.5,maxCh+0.5, 50,0.,0.5);
		mH2_mass[a]->Sumw2();
		mH2_massFine[a] = new TH2F(Form("mass_d%i_fine", detId), ";ch;mass", maxCh,0.5,maxCh+0.5, 250,0.,0.5);
		mH2_massFine[a]->Sumw2();
	}//a, detId

	//Map will be used in calibFms.C: position/bit shift will be obatined from DB according to date
	if (mGetMap)
	{
		ofstream out1, out2;
		out1.open("FmsMapBase.txt");
		out2.open("FmsMapBitShift.txt");
		for (int a=0; a<4; a++)
		{
			const int detId = a+8;
			const int maxCh = mFmsDbMk->maxChannel(detId);
			for (int b=0; b<maxCh; b++)
			{
				const int   ch   = b+1;
				const float gain = mFmsDbMk->getGain(detId, ch);
				if (gain==0) continue; //Cells physically not exist

				const int bs  = mFmsDbMk->getBitShiftGain(detId, ch);
				const int col = mFmsDbMk->getColumnNumber(detId, ch);
				const int row = mFmsDbMk->getRowNumber(detId, ch);
				const float chX = mFmsDbMk->getStarXYZ(detId, ch).x();
				const float chY = mFmsDbMk->getStarXYZ(detId, ch).y();

				out1 <<Form("%2i %3i %4.3f %2i %2i %6.2f %6.2f", detId, ch, gain, col, row, chX, chY) <<endl;
				out2 <<Form("%2i %3i %2i", detId, ch, bs) <<endl;
			}//b, ch
		}//a, detId
		out1.close();
		out2.close();
	}

	//QA items
	if (mApplyBbcZvtx ||
		mGetQaHist ||
		mGetQaHistAdc ||
		mGetQaTree) mQa = new StFmsCalibMakerQa();

	if (mApplyBbcZvtx) mQa->CreateQaHistZVtx();
	for (int a=0; a<4; a++)
	{
		const int detId = a+8;
		const int maxCh = mFmsDbMk->maxChannel(detId);
		if (mGetQaHist) mQa->CreateQaHist(detId, maxCh);
		if (mGetQaHistAdc)
		{
			mQa->CreateQaHistAdc(detId, maxCh, mTrigMB);
			if (a==0) cout <<Form("\nWARNING! GetQaHistAdc() is used with trigger %i\n", mTrigMB) <<endl;
		}
	}
	if (mGetQaTree) mQa->CreateQaTree();

	return kStOk;
}//InitRun

//-----------------------------
Int_t StFmsCalibMaker::Finish()
{
	mFile->Write();
	mFile->Close();
    return kStOK;
}//Finish

//---------------------------
Int_t StFmsCalibMaker::Make()
{
	mEvent++;
	if (mEvent%1000 == 0) cout <<Form("%5i processed...", mEvent) <<endl;
	if (mGetQaHist) mQa->mH1_nEvents->Fill(mRunNo);

    StMuDst* muDST = (StMuDst*)GetInputDS("MuDst");
    if (!muDST)
	{
		LOG_ERROR <<"StFmsCalibMaker::Make - !MuDst" <<endl;
		return kStErr;
	}
	else
	{
		//Skip events in abort gap
        mXing = muDST->event()->triggerData()->bunchId7Bit();
		if ((mXing>=30 && mXing<40) || (mXing>=110 && mXing<120)) return kStSkip;

		//Check trigger
		if (mGetQaHistAdc) //Check specified minBias trigger: only for ADC QA
		{
			const StTriggerId& trigId = muDST->event()->triggerIdCollection().nominal();
			if (trigId.isTrigger(mTrigMB)) mQa->mH1_trig->Fill(mTrigMB);
			else return kStSkip;
		}
		else //Check FMS trigger, skip invalid cases
		{
			mTrig = 0;
			mTrig = CheckFmsTrigger(muDST->event()->triggerIdCollection().nominal());
			if (mTrig==0)
			{
				cout <<"No FMS trigger fired! Skip event " <<mEvent <<endl;
				return kStSkip;
			}
			for (int i=0; i<13; i++)
			{
				bool mTrigFire = false;
				if (mTrig & (1<<i)) mTrigFire = true;
				if (i==11 && mTrigFire==true) return kStSkip; //Empty bit
				if (i==12 && mTrigFire==true) return kStSkip; //LED
			}
		}//Check trigger

		//Check VPD west timing: for RUN15 pAu/pAl, written by X. Chu
		if (mApplyVpdTime)
		{
			const StTriggerData* triggerData = muDST->event()->triggerData();
			if (!triggerData) { LOG_ERROR <<"StFmsCalibMaker::Make - !triggerData" <<endl; return kStErr; }
			UShort_t vpdWestTdc = triggerData->vpdEarliestTDC(west);
			if (vpdWestTdc > mVpdCut) return kStSkip;
		}
	}//Eventwise cut

	StEvent* event = (StEvent*)GetInputDS("StEvent");
	if (!event)
	{
		LOG_ERROR <<"StFmsCalibMaker::Make - !StEvent" <<endl;
		return kStErr;
	}
	else mFmsColl = (StFmsCollection*) event->fmsCollection();

	//-------------------------------------------

	//zVtx from BBC, with slewing correction by Oleg
	if (mApplyBbcZvtx)
	{
		const StTriggerData* triggerData = muDST->event()->triggerData();

		mBbcZ = -999;
		mBbcZ = GetBbcZCorr(triggerData);
		if (mBbcZ == -999.) return kStSkip;
		else mQa->mH1_bbcZ->Fill(mBbcZ);
	}

	//QA: ADC by specific minBias trigger. WARNING: lines after this function will be skipped
	if (mGetQaHistAdc)
	{
		StSPtrVecFmsHit& HITS = mFmsColl->hits();
		const int nHITS = mFmsColl->numberOfHits();
		for (int a=0; a<nHITS; a++)
		{
			const int detId = HITS[a]->detectorId();
			const int ch    = HITS[a]->channel();
			if (detId<8 || detId>11) continue;

			const unsigned short adcRaw = HITS[a]->adc();
			const unsigned short adcCor = mFmsDbMk->getCorrectedAdc(detId, ch, adcRaw);

			if (adcCor>3 && adcCor<250) mQa->mH2_adc[detId-8]->Fill(ch, adcCor); //Cut out pedestal tails & overflow
			mQa->mH2_adcWide[detId-8]->Fill(ch, adcCor); 
		}//a, loop over hits

		return kStOk;
	}//GetQaHistAdc

	//Fill pi0 mass vs. channels for calibration
	//------------------------------------------

	vector<StFmsPointPair*>& PAIRS = mFmsColl->pointPairs();
	const int nPAIRS = mFmsColl->numberOfPointPairs();
	for (int a=0; a<nPAIRS; a++)
	{
		//Cut on a pair
		StFmsPointPair* pair = PAIRS[a];
		if (pair->energy() < 20.) continue;
		if (pair->energy() > 40.) continue; //CUT1
		if (pair->zgg()    > 0.7) continue; //CUT2

		//Get zVtx corrected mass: if ReadBbcSlewing is NOT invoked default mass wo/ correction will be used
		const float mass = (mApplyBbcZvtx)?GetBbcZCorrMass(pair, mBbcZ):pair->mass();
		if (mass > 0.5) continue; //CUT3

		//Single point clusters only
		if (pair->point(0)->nParentClusterPhotons() != 1 ||
			pair->point(1)->nParentClusterPhotons() != 1) continue; //CUT4, SPC

		//Loop over point/cluster -> hit
		bool massFilled = false;
		for (int b=0; b<2; b++)
		{
			StFmsCluster* cluster = pair->point(b)->cluster();

			bool cutClu = true;
			const float nTowers = cluster->nTowers();
			const float sigMax = cluster->sigmaMax();
			const float sigMin = cluster->sigmaMin();
			if (nTowers < 2 || sigMax < 0.1 || sigMin < 0.01) cutClu = false;

			StPtrVecFmsHit* hits = &(cluster->hits());
			const int nHits = hits->size();
			for (int c=0; c<nHits; c++)
			{
				const int   tempDet = hits->at(c)->detectorId();
				const int   tempCh  = hits->at(c)->channel();
				const float tempE   = hits->at(c)->energy();
				if (tempDet<8 || tempDet>11) continue;
				if (tempDet>9 && cutClu==false) continue; //CUT5, Cluster quality, Only for small cells

				if (tempE/pair->energy() > 0.3) //CUT6
				{
					mH2_mass    [tempDet-8]->Fill(tempCh, mass);
					mH2_massFine[tempDet-8]->Fill(tempCh, mass);
					massFilled = true;
				}
			}//c, Loop over hits
		}//b, Loop over point/cluster

		//QA for zVtx effect evaluation
		if (massFilled && mApplyBbcZvtx)
		{
			StLorentzVectorF v0 = pair->point(0)->fourMomentum();
			StLorentzVectorF v1 = pair->point(1)->fourMomentum();
			const float pointE0 = pair->point(0)->energy();
			const float pointE1 = pair->point(1)->energy();
			const float orig_openA = acos((v0.px()*v1.px() + v0.py()*v1.py() + v0.pz()*v1.pz()) / (pointE0*pointE1));
			const float orig_mass  = pair->mass();
			const float corr_openA = GetBbcZCorrMass(pair, mBbcZ, true);
			const float corr_mass  = GetBbcZCorrMass(pair, mBbcZ);
			mQa->mH1_diffOpenA->Fill(orig_openA - corr_openA);
			mQa->mH1_diffMass ->Fill(orig_mass - corr_mass);
		}
	}//a, Loop over pairs

	//QA
	//-------------------------------------------

	if (mGetQaHist==false && mGetQaTree==false) return kStOk;

	for (int a=0; a<nPAIRS; a++)
	{
		//Cut on a pair
		StFmsPointPair* pair = PAIRS[a];
		if (pair->energy() < 20.) continue; //CUT1: no upper energy limit
		if (pair->zgg()    > 0.7) continue; //CUT2

		//Get zVtx corrected openA and mass
		const float openA =	(mApplyBbcZvtx)?GetBbcZCorrMass(pair, mBbcZ, true):GetBbcZCorrMass(pair, 0, true);
		const float mass  = (mApplyBbcZvtx)?GetBbcZCorrMass(pair, mBbcZ):pair->mass();
		if (mass > 1.0) continue; //CUT3: extended from 0.5 -> 1.0

		//Single point clusters only
		if (pair->point(0)->nParentClusterPhotons() != 1 ||
			pair->point(1)->nParentClusterPhotons() != 1) continue; //CUT4, SPC

		//Loop over point -> cluster -> hit
		const float pairE = pair->energy();
		bool fillQaTree = false;
		for (int b=0; b<2; b++)
		{
			const float pointX    = pair->point(b)->XYZ().x();
			const float pointY    = pair->point(b)->XYZ().y();
			StLorentzVectorF vecF = pair->point(b)->fourMomentum();
			StFmsCluster* cluster = pair->point(b)->cluster();

			bool cutClu = true;
			const float nTowers = cluster->nTowers();
			const float sigMax = cluster->sigmaMax();
			const float sigMin = cluster->sigmaMin();
			if (nTowers < 2 || sigMax < 0.1 || sigMin < 0.01) cutClu = false;

			StPtrVecFmsHit* hits = &(cluster->hits());
			const int nHits = hits->size();
			for (int c=0; c<nHits; c++)
			{
				const int   tempDet = hits->at(c)->detectorId();
				const int   tempCh  = hits->at(c)->channel();
				const float tempE   = hits->at(c)->energy();
				if (tempDet<8 || tempDet>11) continue;

				//-------------------------------

				if (mGetQaTree) //WARNING: cluster quality cut is NOT applied here
				{
					if (tempE/pairE > 0.3) fillQaTree = true;

					const int nHit = mQa->mNhit;
					if (nHit > mQa->mNhitMax) LOG_ERROR <<"WARNING! nhit exceeds allowed limit!" <<endl;
					mQa->mDetId [nHit] = tempDet;
					mQa->mCh    [nHit] = tempCh;
					mQa->mPointB[nHit] = b;
					mQa->mHitE  [nHit] = tempE;
					mQa->mNhit++;
				}//mGetQaTree, hit

				//-------------------------------

				if (mGetQaHist)
				{
					if (tempDet>9 && cutClu==false) continue; //CUT5, Cluster quality, Only to small cells
					
					int iZgg = -1;
					for (int i=0; i<7; i++) { if (pair->zgg() > 0.1*i && pair->zgg() < 0.1*(i+1)) iZgg = i;	}
					
					int iPairE = -1;
					for (int i=0; i<7; i++)
					{
						if (pairE > 20+10*i && pairE < 20+10*(i+1)) iPairE = i;
						if (pairE > 80) iPairE = 6;
					}

					//PointXY, Same condition to default mass vs. ch
					if (pairE<40 && mass<0.5 && tempE/pairE>0.3)
					{
						mQa->mH2_pointsEP[tempDet-8]->Fill(vecF.pseudoRapidity(), vecF.phi());
						mQa->mH2_pointsXY[b][0]->Fill(pointX, pointY);
						if (tempDet<=9)	mQa->mH2_pointsXY[b][1]->Fill(pointX, pointY);
						else            mQa->mH2_pointsXY[b][2]->Fill(pointX, pointY);
					}

					//Mass, default condition except range is wider
					if (pairE<40 && tempE/pairE>0.3)
					{
						mQa->mH2_massWide[tempDet-8]->Fill(tempCh, mass);
						mQa->mH2_massOpenA[tempDet-8][iZgg]->Fill(openA, mass);
					}

					//Scan pairE or Zgg
					if (tempE/pairE>0.3)
					{
						mQa->mH2_massPairE[tempDet-8][iZgg]->Fill(pairE, mass);
						mQa->mH2_massZgg[tempDet-8][iPairE]->Fill(pair->zgg(), mass);
					}
				}//mGetQaHist
			}//c, Loop over hits, 2nd for QA, histograms/tree

			if (mGetQaTree)
			{
				mQa->mCluTowers[b] = cluster->nTowers();
				mQa->mCluMax[b]    = cluster->sigmaMax();
				mQa->mCluMin[b]    = cluster->sigmaMin();
				mQa->mCluX[b]      = cluster->x();
				mQa->mCluY[b]      = cluster->y();

				mQa->mPointE[b] = pair->point(b)->energy();
				mQa->mPointX[b] = pair->point(b)->XYZ().x();
				mQa->mPointY[b] = pair->point(b)->XYZ().y();
			}//mGetQaTree, cluster/point

		}//b, Loop over point/cluster, 2nd for QA
		
		if (mGetQaTree && fillQaTree && mQa->mNhit>0)
		{
			mQa->mMass    = mass;
			mQa->mOpenA   = (mApplyBbcZvtx)?GetBbcZCorrMass(pair, mBbcZ, true):GetBbcZCorrMass(pair, 0, true);
			mQa->mZgg     = pair->zgg();
			mQa->mTrigBit = mTrig;

			mQa->T->Fill();
		}
		mQa->ResetQaTree();

	}//a, Loop over pairs, 2nd for QA

	return kStOk;
}//Make
