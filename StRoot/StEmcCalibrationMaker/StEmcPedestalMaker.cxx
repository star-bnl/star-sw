#include "StEmcPedestalMaker.h"

#include <fstream>
using namespace std;

#include <TFile.h>
#include <TROOT.h>
#include <TF1.h>

#include <StEventTypes.h>
#include <StEvent.h>
#include <StEmcUtil/geometry/StEmcGeom.h>
#include <StDbLib/StDbManager.hh>
#include <StDbLib/StDbTable.h>
#include <StDbLib/StDbConfigNode.hh>
#include <tables/St_emcPed_Table.h>
#include <tables/St_smdPed_Table.h>
#include <StDaqLib/EMC/StEmcDecoder.h>

ClassImp(StEmcPedestalMaker)

//_____________________________________________________________________________
StEmcPedestalMaker::StEmcPedestalMaker(const Char_t *name)
    : StEmcCalibMaker(name)
    {
    setRange(300);
    setMaxTracks(100);
    mLastPedDate = 2000;
    mLastPedTime = 0;

    setNPedEvents(2000);
    setSaveTables(false);
    setPedDiffSaveDB(2.0);
    setPedDiffSaveNum(3);
    setPedDiffSaveMinTime(60 * 60 * 24);
    setCompareLastTableDB(false);
    setPedCrateFilenameFormat("pedestal_crate0x%02x.dat");
    setBemcStatusFilename("bemcStatus.txt");
    setUseBemcStatus(true);
}
//_____________________________________________________________________________
StEmcPedestalMaker::~StEmcPedestalMaker()
{
}
//_____________________________________________________________________________
Int_t StEmcPedestalMaker::Init()
{
  mPedestal = new TH1F("mPed","",getNChannel(),0.5,getNChannel()+0.5);
  mRms = new TH1F("mRms","",getNChannel(),0.5,getNChannel()+0.5);
  mChi = new TH1F("mChi","",getNChannel(),0.5,getNChannel()+0.5);
  mStatus = new TH1F("mStatus","",getNChannel(),0.5,getNChannel()+0.5);

  mSpecName = "mSpecPed";
  mAcceptName = "mAcceptPed";
  
  mStarted = false;
  
  return StEmcCalibMaker::Init();
}
//_____________________________________________________________________________
void StEmcPedestalMaker::Clear(Option_t *option)              
{
}
//_____________________________________________________________________________
Int_t StEmcPedestalMaker::Make()
{  
  if(!accept()) return kStOk;
	
	if(!mStarted)
	{
		if(getTimeInterval(mLastPedDate,mLastPedTime)>mPedInterval)
		{
			mLastPedDate = getDate();
			mLastPedTime = getTime();
			mStarted = true;
		  reset();
		}
		else
		{
			if(isDebug()) cout <<"Time remaining for a new pedestal run is "
			                   <<mPedInterval-getTimeInterval(mLastPedDate,mLastPedTime)
					               <<" hours for detector number "<<getDetector()<<endl;
		}
	}
  
	if(!mStarted) return kStOk;
	//cout <<"Started\n";
  for(int i=0;i<mNChannel;i++)
  {
    int id = i+1;
    float adc = (float) getCalib()->getADC(mDetector,id);
		//cout <<"Detector = "<<mDetector<<"  id = "<<id<<"  adc = "<<adc<<endl;
    if(adc!=0) fill(id,adc);
  }
  
  if(getNEvents()>getNPedEvents())
  {
    calcPedestals();
		savePedestals(mLastPedDate,mLastPedTime,isAutoSaveDB());
		if(isAutoSaveDB() || getSaveTables()) saveToDb(mLastPedDate,mLastPedTime);
		mStarted = false;
  }
  
  return kStOK;
}
//_____________________________________________________________________________
Int_t StEmcPedestalMaker::Finish()
{
  return StMaker::Finish();
}
//_____________________________________________________________________________
void StEmcPedestalMaker::calcPedestals()
{	
  cout <<"***** Calculating pedestals for detector "<<getDetector()<<" ...\n"; 
  mPedestal->Reset();
  mRms->Reset();
  mChi->Reset();
  mStatus->Reset();
  float left = 3;
  float right= 2;

  int ngood=0,nped=0,nrms=0,nchi=0,nbad=0,nodata=0;
  
  for(int id = 1;id<=getNChannel();id++) {		
    TH1D *h = getSpec(id);
    int ibin = h->GetMaximumBin();
    float avg = (float)h->GetBinCenter(ibin);
    float max = (float)h->GetMaximum();
    float rms = 1.5;
    if(getDetector()>2) rms = h->GetRMS();
    float integral = (float)h->Integral();
    if((max!=0) && (integral > (getNPedEvents() / 2.0))) {
	TF1 func("ped","gaus(0)");
	float rmsInit = rms;
    	func.SetParameter(0,max);
    	func.SetParameter(1,avg);
    	func.SetParameter(2,rms);
    	func.SetParLimits(2,0,100000);
    	float seed = avg;    
	float fitleft = avg-left*rms;
	if(fitleft<0) fitleft = 0;
	float fitright = avg+right*rms;
    	func.SetRange(fitleft,fitright);
    
    	int npt = (Int_t)((left+right+1.0)*rms);
    	int ndg = (Int_t)((float)npt-3.0);
    
    	h->Fit(&func,"RQN"); // pre fit
    	max = func.GetParameter(0);
    	avg = func.GetParameter(1);
    	rms = func.GetParameter(2);
    	int status = 1; // data present
    	float chi = func.GetChisquare()/(float)ndg;
    	float res = avg-seed;
			    
    	if(avg<0)                        {status+= 2; nped++; avg = 0;}// negative pedestal
    	if(rms<0 || rms >7*rmsInit)      {status+= 4; nrms++;}// bad rms
    	if(fabs(res)>1.5*rms)            {status+= 8; nchi++;}// large distance to seed
    	if(status==1) ngood++; else nbad++;
    	mPedestal->Fill((float)id,avg);
    	mRms->Fill((float)id,rms);
    	mChi->Fill((float)id,chi);
    	mStatus->Fill((float)id,(float)status);    
    	if(status>1) cout <<"det = "<<getDetector()<<"  id = "<<id <<"  max = "<<seed<<"  initRms = "<<rmsInit
			                                           <<"  peakY = "<<max
                                                 <<"  ped = "<<avg <<"  res = " <<res<<"  rms = "<<rms
                                                 <<"  chi = "<<chi<<"  status = "<<status<<endl;
    } else {
    	mPedestal->Fill((float)id,0);
    	mRms->Fill((float)id,0);
    	mChi->Fill((float)id,0);
    	mStatus->Fill((float)id,0);
	nodata++;
	nbad++;
    	cout << "det = " << getDetector() << "  id = " << id << " peakY = "<< max
                                                 << " ped = " << avg << " rms = " << rms
                                                 << " int = " << integral << endl;
    }
    if(h) {delete h; h = 0;}
  }
  cout <<"nGood = "<<ngood<<"; nBad = "<<nbad<<": neg Ped = "<<nped<<", bad rms = "<<nrms<<", large res = "<<nchi << ", no data = " << nodata <<endl;
}
//_____________________________________________________________________________
void StEmcPedestalMaker::saveToDb(const Char_t *timeStamp, const Char_t *tableFilename) const {
    cout << "=================================================" << endl;
    cout << "Saving pedestal table for detector " << getDetector() << endl;
    cout << "TimeStamp = " << timeStamp << endl;
    
    TString n[] = {"bemcPed", "bprsPed", "bsmdePed", "bsmdpPed"};

    Bool_t saveThisTable = true;
    TString lastTableFilename = getLastTablePath();
    lastTableFilename += "/";
    lastTableFilename += n[getDetector() - 1];
    lastTableFilename += ".last";
    TString lastTableTimestampFilename = lastTableFilename + ".timestamp";
    lastTableFilename += ".root";

    if (getCompareLastTableDB()) {
    cout << "Comparing to the last table file " << lastTableFilename << endl;
    TFile *lastTableFile = new TFile(lastTableFilename);
    if (lastTableFile && lastTableFile->IsOpen()) {
	cout << "Opened last table file " << lastTableFilename << endl;
	saveThisTable = false;
	ifstream ifstr_timestamp(lastTableTimestampFilename);
	if (ifstr_timestamp.good()) {
	    Char_t lastTableTimestampStr[2048];
	    ifstr_timestamp.getline(lastTableTimestampStr, 2048);
	    ifstr_timestamp.close();
	    TDatime tsLast(&lastTableTimestampStr[0]);
	    TDatime tsCurrent(timeStamp);
    	    UInt_t timeLast = tsLast.Convert();
    	    UInt_t timeCurrent = tsCurrent.Convert();
	    Double_t diffTime = difftime(timeCurrent, timeLast);
	    cout << "Timestamps:  Last: " << lastTableTimestampStr << ", " << tsLast.AsSQLString() << endl;
	    cout << "Timestamps:  Current: " << timeStamp << ", " << tsCurrent.AsSQLString() << "; Diff: " << diffTime << endl;
	    if (diffTime > this->getPedDiffSaveMinTime()) {
		saveThisTable = true;
		cout << "Saving this table because min time passed" << endl;
	    }
	} else {
	    saveThisTable = true;
	    cout << "Saving this table because cannot open last timestamp file " << lastTableTimestampFilename << endl;
	}
	const emcPed_st *ped_t_st = 0;
	const smdPed_st *ped_s_st = 0;
	if (getDetector() < 3) {
	    const St_emcPed *pedT = (const St_emcPed *)lastTableFile->Get(n[getDetector() - 1]);
	    ped_t_st = pedT ? pedT->GetTable() : 0;
	} else {
	    const St_smdPed *pedT = (const St_smdPed *)lastTableFile->Get(n[getDetector() - 1]);
	    ped_s_st = pedT ? pedT->GetTable() : 0;
	}
	Float_t maxPedDiff = 0;
	Float_t numPedDiff = 0;
	Int_t *bemcStatus = 0;
	if ((getDetector() == 1) && this->getUseBemcStatus() && this->getBemcStatusFilename()) {
	    TString bemcStatusFilename = this->getBemcStatusFilename();
	    ifstream ifstr(bemcStatusFilename);
	    if (ifstr.good()) {
    		cout << "Reading BEMC trigger status file " << bemcStatusFilename << endl;
	    } else {
    		cout << "Cannot open BEMC trigger status file! " << bemcStatusFilename << endl;
	    }
	    if (ifstr.good()) {
		bemcStatus = new Int_t[4800];
		if (bemcStatus) {
		    for (Int_t i = 4800;i;bemcStatus[--i] = 1);
		    while (ifstr.good()) {
    			string token;
    			do {
			    if (token == "#") {
				char dummy[4096];
				ifstr.getline(dummy, sizeof(dummy));
			    }
			    ifstr >> token;
			} while (ifstr.good() && (token != "SoftId"));
    			if (ifstr.good()) {
			    if (token == "SoftId") {
				int softId, crate, crateSeq, unmaskTower, unmaskHT, unmaskPA;
				float ped;
				ifstr >> softId >> crate >> crateSeq >> unmaskTower >> unmaskHT >> unmaskPA >> ped;
				if ((softId >= 1) && (softId <= 4800)) {
				    bemcStatus[softId - 1] = (unmaskTower && (unmaskHT || unmaskPA)) ? 1 : 0;
				    if (bemcStatus[softId - 1] != 1) {
					//cout << "tower " << softId << " masked out: " << unmaskTower << " " << unmaskHT << " " << unmaskPA << endl;
				    }
	    			}
			    }
			}
		    }
    		}
		ifstr.close();
	    }
	}
	for (Int_t i = 0;i < getNChannel();i++) {
	    Int_t id = i + 1;
	    Float_t pedNew = getPedestal(id);
	    Float_t rmsNew = getRms(id);
	    //Float_t chiNew = getChi(id);
	    Int_t statusNew = (int)getStatus(id);
	    Float_t pedLast = 0;
	    Float_t rmsLast = 0;
	    Float_t chiLast = 0;
	    Int_t statusLast = 0;
	    if (getDetector() < 3) {
		if (ped_t_st) {
		    pedLast = (short)ped_t_st->AdcPedestal[i] / 100.0;
		    rmsLast = (short)ped_t_st->AdcPedestalRMS[i] / 100.0;
		    chiLast = ped_t_st->ChiSquare[i];
		    statusLast = ped_t_st->Status[i];
		}
	    } else {
		if (ped_s_st) {
		    pedLast = (short)ped_s_st->AdcPedestal[i][0] / 100.0;
		    rmsLast = (short)ped_s_st->AdcPedestalRMS[i][0] / 100.0;
		    statusLast = ped_s_st->Status[i];
		}
	    }
	    Float_t pedDiff = pedNew - pedLast;
	    if (maxPedDiff < TMath::Abs(pedDiff)) maxPedDiff = TMath::Abs(pedDiff);
	    if (/*(statusLast == 1) || */(statusNew == 1)) {
		if (TMath::Abs(pedDiff) >= TMath::Abs(getPedDiffSaveDB() * rmsNew)) {
		    TString statusLabel = "";
		    if (!bemcStatus || ((getDetector() == 1) && bemcStatus && (id >= 1) && (id <= 4800) && (bemcStatus[id-1] == 1))) {
			numPedDiff++;
		    } else {
			statusLabel = " (bad in trigger)";
		    }
		    cout << "det " << getDetector() << ", id " << id << statusLabel << ": ped diff " << pedDiff << ", last " << pedLast << " " << rmsLast << " " << (Int_t)statusLast << ", new " << pedNew << " " << rmsNew << " " << (Int_t)statusNew << endl;
		}
	    }
	}
	if (bemcStatus) delete [] bemcStatus; bemcStatus = 0;
	cout << "max ped diff " << maxPedDiff << endl;
	cout << "num ped diff " << numPedDiff << endl;
	saveThisTable |= (numPedDiff >= this->getPedDiffSaveNum());
	lastTableFile->Close();
	if (!saveThisTable) cout << "This table does not need to be saved" << endl;
    }
    if (lastTableFile) delete lastTableFile; lastTableFile = 0;
    }
    
    St_emcPed *pedT_t = new St_emcPed(n[getDetector() - 1].Data(), 1);
    emcPed_st *tnew = pedT_t ? pedT_t->GetTable() : 0;
    
    St_smdPed *pedS_t = new St_smdPed(n[getDetector() - 1].Data(), 1);
    smdPed_st *snew = pedS_t ? pedS_t->GetTable() : 0;
    
    for (int i = 0;i < getNChannel();i++) {
	int id = i + 1;
	float ped = getPedestal(id);
	float rms = getRms(id);
	float chi = getChi(id);
	int status = (int)getStatus(id);
	if (getDetector() < 3) {
	    if (tnew) {
    		tnew->Status[i] = (char)status;
		tnew->AdcPedestal[i] = (short)(ped * 100.0);
		tnew->AdcPedestalRMS[i] = (short)(rms * 100.0);
		tnew->ChiSquare[i] = chi;
		//cout << "tnew i = " << i << ": Status " << (Int_t)tnew->Status[i] << ", ped " << tnew->AdcPedestal[i] << ", rms " << tnew->AdcPedestalRMS[i] << ", chi " << tnew->ChiSquare[i] << endl;
	    }
	} else {
	    if (snew) {
		snew->Status[i] = (char)status;
		snew->AdcPedestal[i][0] = (short)(ped * 100.0);
		snew->AdcPedestalRMS[i][0] = (short)(rms * 100.0);
		snew->AdcPedestal[i][1] = 0;
		snew->AdcPedestalRMS[i][1] = 0;
		snew->AdcPedestal[i][2] = 0;
		snew->AdcPedestalRMS[i][2] = 0;
		//cout << "snew i = " << i << ": Status " << (Int_t)snew->Status[i] << ", ped " << snew->AdcPedestal[i][0] << ", rms " << snew->AdcPedestalRMS[i][0] << endl;
	    }
	}
    }
    if (isAutoSaveDB() && (!getCompareLastTableDB() || saveThisTable)) {
	StDbManager* mgr = StDbManager::Instance();
	cout << "mgr = " << mgr << endl;
	StDbConfigNode* node = mgr ? mgr->initConfig(dbCalibrations, dbEmc) : 0;
	cout << "node = " << node << endl;
	StDbTable* table = node ? node->addDbTable(n[getDetector() - 1].Data()) : 0;
	cout << "table = " << table << endl;
	if (table) {
	    table->setFlavor("ofl");
	    if (getDetector() < 3) {
		table->SetTable((char*)tnew, 1);
	    } else {
		table->SetTable((char*)snew, 1);
	    }
	}
	cout << "table set" << endl;
	if (mgr && table) {
	    cout << "setStoreTime " << timeStamp << endl;
	    mgr->setStoreTime(timeStamp);
	    cout << "Storing " << n[getDetector() - 1] << " " << timeStamp << endl;
	    mgr->storeDbTable(table);
	    cout << "Stored." << endl;
	}
    }
    if (getSaveTables() && (!getCompareLastTableDB() || saveThisTable) && tableFilename) {
	cout << "Saving DB table into " << tableFilename << endl;
	TFile *f = new TFile(tableFilename, "RECREATE");
	if (f) {
	    if (getDetector() < 3) {
		pedT_t->AddAt(tnew, 0);
		pedT_t->Write();
	    } else {
		pedS_t->AddAt(snew, 0);
		pedS_t->Write();
	    }
	    f->Close();
	    delete f; f = 0;
	}
    }
    if (getCompareLastTableDB() && saveThisTable) {
	cout << "Saving last DB table into " << lastTableFilename << endl;
	TFile *f = new TFile(lastTableFilename, "RECREATE");
	if (f) {
	    if (getDetector() < 3) {
		pedT_t->AddAt(tnew, 0);
		pedT_t->Write();
	    } else {
		pedS_t->AddAt(snew, 0);
		pedS_t->Write();
	    }
	    f->Close();
	    delete f; f = 0;
	    ofstream ofstr_timestamp(lastTableTimestampFilename);
	    ofstr_timestamp << timeStamp << endl;
	}
	if (getDetector() == 1) {
	    StEmcDecoder *d = new StEmcDecoder();
	    for (Int_t crate = 1;crate <= 30;crate++) {
		TString pedCrateFilename = getLastTablePath();
	        pedCrateFilename += "/";
	        pedCrateFilename += Form(getPedCrateFilenameFormat(), crate);
	        TString pedCrateTimestampFilename = pedCrateFilename + ".timestamp";
	        cout << "Saving pedestals for crate " << crate << ": " << pedCrateFilename << endl;
	        ofstream ofstr(pedCrateFilename);
    	        for (Int_t ch = 0;ch < 160;ch++) {
		    Int_t softId = -1;
            	    if (d) d->GetTowerIdFromCrate(crate, ch, softId);
            	    if ((softId >= 1) && (softId <= 4800)) {
	    		TString line = Form("%i %.2f %.2f", ch, getPedestal(softId), getRms(softId));
			ofstr << line.Data() << endl;
		    }
		}
	        ofstream ofstr_timestamp(pedCrateTimestampFilename);
	        ofstr_timestamp << timeStamp << endl;
	    }
	    if (d) delete d; d = 0;
	}
    }
}
//_____________________________________________________________________________
void StEmcPedestalMaker::saveToDb(Int_t date, Int_t time) const
{   
  Int_t year  = (Int_t)(date/10000);
  Int_t month = (Int_t)(date-year*10000)/100; 
  Int_t day   = (Int_t)(date-year*10000-month*100);
  Int_t hour  = (Int_t)(time/10000);
  Int_t minute= (Int_t)(time-hour*10000)/100;
  Int_t second= (Int_t)(time-hour*10000-minute*100);
	Char_t ts[1024]; ts[0] = 0;
	Char_t tf[1024]; tf[0] = 0;
	TString d[] = {"bemc","bprs","bsmde","bsmdp"};
	TString n[] = {"bemcPed","bprsPed","bsmdePed","bsmdpPed"};
	sprintf(ts,"%04d-%02d-%02d %02d:%02d:%02d",year,month,day,hour,minute,second);
	//sprintf(ts,"%04d%02d%02d%02d%02d%02d",year,month,day,hour,minute,second);
	sprintf(tf, "%s/y3%s/%s.%08d.%06d.root", getTablesPath(), d[getDetector() - 1].Data(), n[getDetector() - 1].Data(), date, time);
	saveToDb(ts, tf);
}
//_____________________________________________________________________________
void StEmcPedestalMaker::savePedestals(Int_t date, Int_t time, Bool_t DB) const
{   
	Char_t ts[1024]; ts[0] = 0;
	TString n[] = {"bemcPed","bprsPed","bsmdePed","bsmdpPed"};
	if (DB) sprintf(ts,"%s/%s.%08d.%06d.root", getSavePath(), n[getDetector()-1].Data(), date, time);
	else sprintf(ts,"%s/%s.%08d.%06d.NO_DB.root", getSavePath(), n[getDetector()-1].Data(), date, time);
	cout << "Saving pedestal histograms to " << ts << endl;
	TFile *f = new TFile(ts,"RECREATE");
	if(getSpec()) getSpec()->Write();
	if(mPedestal) mPedestal->Write();
	if(mRms) mRms->Write();
	if(mChi) mChi->Write();
	if(mStatus) mStatus->Write();
	f->Close();
	delete f;
}
//_____________________________________________________________________________
void StEmcPedestalMaker::loadPedestals(const Char_t* file)
{   
	TFile *f = new TFile(file);
	if(getSpec()) getSpec()->Reset();
	if(mPedestal) mPedestal->Reset();
	if(mRms) mRms->Reset();
	if(mChi) mChi->Reset();
	if(mStatus) mStatus->Reset();
  TH2F* h = (TH2F*)f->Get("mSpec;1");
  if(h && getSpec()) getSpec()->Add(h,1);
  TH1F *g=(TH1F*)f->Get("mPed;1");
  if(g && mPedestal) mPedestal->Add(g,1);
  g=(TH1F*)f->Get("mRms;1");
  if(g && mRms) mRms->Add(g,1);
  g=(TH1F*)f->Get("mChi;1");
  if(g && mChi) mChi->Add(g,1);
  g=(TH1F*)f->Get("mStatus;1");
  if(g && mStatus) mStatus->Add(g,1);
	f->Close();
	delete f;
	return;
}

