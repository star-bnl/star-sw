/***************************************************************************
 *
 * $Id: StSvtPedMaker.cxx,v 1.10 2007/04/28 17:57:03 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Pedestal calculation Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtPedMaker.cxx,v $
 * Revision 1.10  2007/04/28 17:57:03  perev
 * Redundant StChain.h removed
 *
 * Revision 1.9  2007/04/20 17:26:31  caines
 * Removing print statements
 *
 * Revision 1.8  2004/01/26 23:12:55  perev
 * Leak off
 *
 * Revision 1.7  2003/12/01 00:53:10  caines
 * Dont follow zero pointer if raw data not there
 *
 * Revision 1.6  2003/12/01 00:19:43  caines
 * Version of pedestal maker to go with EMbedding
 *
 * Revision 1.5  2003/07/18 18:31:49  perev
 * test for nonexistance of XXXReader added
 *
 * Revision 1.4  2001/10/24 16:47:52  munhoz
 * adding RMS methods
 *
 * Revision 1.3  2001/07/11 23:30:47  munhoz
 * modification to allow pedestal reading from daq file
 *
 * Revision 1.2  2000/11/30 20:32:02  caines
 * Use MessMgr
 *
 * Revision 1.1  2000/08/23 13:08:12  munhoz
 * SVT pedestal calculation
 *
 *
 **************************************************************************/

#include "TH2.h"
#include "TFile.h"
#include "TKey.h"
#include "StSvtPedMaker.h"
#include "St_DataSetIter.h"
#include "TObjectSet.h"
#include "StMessMgr.h"
#include "StarClassLibrary/StSequence.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "StSvtClassLibrary/StSvtHybridPixels2.hh"
#include "StSvtClassLibrary/StSvtHybridStat.hh"
#include "StSvtClassLibrary/StSvtHybridStat2.hh"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtDaqMaker/StSvtHybridDaqPed.hh"

ClassImp(StSvtPedMaker)

//_____________________________________________________________________________
  StSvtPedMaker::StSvtPedMaker(const char *name, pedestalType type):StMaker(name)
{
  fType = type;

  fStat     = NULL;     
  fPed      = NULL;      
  fPedRms   = NULL;      
  fData     = NULL;     

  fSvtStat  = NULL;  
  fSvtStat2 = NULL;
  fSvtPed   = NULL;   
  fSvtPedRms= NULL;   
  fSvtData  = NULL;    
  mConfig   = NULL;
}

//______________________________________________________________________________
StSvtPedMaker::~StSvtPedMaker()
{
  delete fStat;     
  
  delete fPed;      
  delete fPedRms;      
  delete fData;     

  delete fSvtStat;  
  delete fSvtStat2;  
  //  delete fSvtPed;   
  //delete fSvtData;    
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::Init()
{
  //cout << "StSvtPedMaker::Init" << endl;

  SetSvtData();
  getConfig();
  
  SetSvtPed();

  SetSvtRMSPed();

  return StMaker::Init();
}

//____________________________________________________________________________
Int_t StSvtPedMaker::getConfig()
{
  mConfig=NULL;
  St_DataSet *dataSet = NULL;
  dataSet = GetDataSet("StSvtConfig");

  if (!dataSet)
    {
      gMessMgr->Warning() << " No SvtConfig  data set" << endm;
      dataSet = new St_ObjectSet("StSvtConfig");                                                               
      AddConst(dataSet);
      mConfig=NULL;
    }

  mConfig=((StSvtConfig*)(dataSet->GetObject()));  

  if (!mConfig) {
    gMessMgr->Warning() << "SvtConfig data set is empty- using raw data"<< endm;
    mConfig=new StSvtConfig();

    if (!fSvtData) //this should not be used- for backward compatibility
      {
	gMessMgr->Warning() << "no raw data -seting default full configuration" << endm;
	mConfig->setConfiguration("FULL");
      } else mConfig->setConfiguration(fSvtData->getConfiguration());
   
    dataSet->SetObject(mConfig);
  }
  
  
 
  return kStOK;
}


//_____________________________________________________________________________
Int_t StSvtPedMaker::SetType(pedestalType option)
{
  fType = option;

  ResetStat();
  ResetPed();

  return Init();
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::SetSvtData()
{
  St_DataSet *dataSet;

  dataSet = GetDataSet("StSvtRawData");
  //assert(dataSet);
  if( dataSet) fSvtData = (StSvtData*)(dataSet->GetObject());
  //assert(fSvtData);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::SetSvtPed()
{
  fSvtStat = new StSvtHybridCollection(mConfig->getConfiguration());

  St_DataSet *dataSet;
  dataSet = (TObjectSet*)GetDataSet("StSvtPedestal");

  if (!dataSet) {
    fPedSet = new TObjectSet("StSvtPedestal");
    AddConst(fPedSet);

    fSvtPed = new StSvtHybridCollection(mConfig->getConfiguration());
    fPedSet->SetObject((TObject*)fSvtPed);
    assert(fSvtPed);
  }
  else {
    fSvtPed = (StSvtHybridCollection*)(dataSet->GetObject());
    assert(fSvtPed);
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::SetSvtPed2ndOrd()
{
  fSvtStat2 = new StSvtHybridCollection(mConfig->getConfiguration());

  fPedSet2 = new TObjectSet("StSvtPedestal2ndOrd");
  AddConst(fPedSet2);    

  fSvtPed2 = new StSvtHybridCollection(mConfig->getConfiguration());
  fPedSet2->SetObject((TObject*)fSvtPed2);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::SetSvtRMSPed()
{
  St_DataSet *dataSet;
  dataSet = (TObjectSet*)GetDataSet("StSvtRMSPedestal");

  if (!dataSet) {
    fPedRmsSet = new TObjectSet("StSvtRMSPedestal");
    AddConst(fPedRmsSet);

    fSvtPedRms = new StSvtHybridCollection(mConfig->getConfiguration());
    fPedRmsSet->SetObject((TObject*)fSvtPedRms);
    assert(fSvtPedRms);
  }
  else {
    fSvtPedRms = (StSvtHybridCollection*)(dataSet->GetObject());
    assert(fSvtPedRms);
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::Make()
{
  if (Debug()) gMessMgr->Debug() << "StSvtPedMaker::Make" << endm;

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::AddStat()
{
  if (Debug()) gMessMgr->Debug() << "StSvtPedMaker::AddStat" << endm;

  int anodeID, nAnodes, nSeq, iseq, time, timeSeq, status;
  int capacitor, nSCAZero;
  int* anodeList;
  StSequence* Seq;
  int previousBin, nextBin, timeCap0th;

  for (int barrel = 1;barrel <= fSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= fSvtData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= fSvtData->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= fSvtData->getNumberOfHybrids();hybrid++) {
	  
	  // check if the hybrid is part of the SVT
	  if (fSvtData->getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;
	  
	  fData = (StSvtHybridData*)fSvtData->at(fSvtData->getHybridIndex(barrel, ladder, wafer, hybrid));

	  if (!fData) continue;

	  nSCAZero = fData->getSCAZero();

	  // Calculate time bin of capacitor = 0
	  timeCap0th = 128 - nSCAZero;
	  if (timeCap0th == 128)
	    timeCap0th = 0;

	  // and time bin of capacitor = 127
	  if (timeCap0th == 0)
	    previousBin = 127;
	  else
	    previousBin = timeCap0th - 1;

	  // and time bin of capacitor = 1
	  if (timeCap0th == 127)
	    nextBin = 0;
	  else
	    nextBin = timeCap0th + 1;

	  /*
	  //Skip events where the capacitor number of 0th time bin is wrong
	  status = fData->getSequences(120,nSeq,Seq);

	  if ((timeCap0th != 1) && (timeCap0th != 2))
	    if (((int)Seq[0].firstAdc[previousBin]) > ((int)Seq[0].firstAdc[timeCap0th]))  {
	      return kStOK; 
	    }
	  */

          int idx = fSvtStat->getHybridIndex(barrel, ladder, wafer, hybrid);
	  fStat = (StSvtHybridStat*)fSvtStat->at(idx);
	  if (!fStat) {
	    fStat = new StSvtHybridStat(barrel, ladder, wafer, hybrid);
	    fSvtStat->put_at(fStat,idx);
	  }
	  
	  anodeList = NULL;
	  nAnodes = fData->getAnodeList(anodeList);

	  for (int ianode=0;ianode<nAnodes;ianode++) {
	    
	    anodeID = anodeList[ianode];
	    Seq = NULL;
	    nSeq = 0;
	
	    status = fData->getSequences(anodeID,nSeq,Seq);
	    
	    for (iseq=0;iseq<nSeq;iseq++) {	  	  
	      for (timeSeq=0; timeSeq<Seq[iseq].length; timeSeq++) {

		time = Seq[iseq].startTimeBin + timeSeq;

		if ( fType == kCapacitor ) {
		  // Exclude first two time bins from capacitor-related pedestal calculation
		  if ((time == 0) || (time == 1)) continue;		
		  
		  capacitor = time + nSCAZero;
		  if (capacitor > 127)
		    capacitor -= 128;		
		  
		  fStat->fillMom((int)Seq[iseq].firstAdc[timeSeq],anodeID,capacitor);

		}
		else if ( fType == kTime ) {
		  // Exclude last and first capacitors from time bin-related pedestal calculation
		  if ((time == previousBin) || (time == timeCap0th) || (time == nextBin)) continue;
		  
		  fStat->fillMom((int)Seq[iseq].firstAdc[timeSeq],anodeID,time);
		}
	      }
	    }
	  }
	}
      }
    }
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::AddStat2ndOrd()
{
  int anodeID, nAnodes, nSeq, iseq, time, timeSeq, status;
  int capacitor, nSCAZero;
  int* anodeList;
  StSequence* Seq;
  int previousBin, nextBin, timeCap0th;

  for (int barrel = 1;barrel <= fSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= fSvtData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= fSvtData->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= fSvtData->getNumberOfHybrids();hybrid++) {
	  
	  // check if the hybrid is part of the SVT
	  if (fSvtData->getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;
	  
	  fData = (StSvtHybridData*)fSvtData->at(fSvtData->getHybridIndex(barrel, ladder, wafer, hybrid));

	  if (!fData) continue;

	  nSCAZero = fData->getSCAZero();

	  // Calculate time bin of capacitor = 0
	  timeCap0th = 128 - nSCAZero;
	  if (timeCap0th == 128)
	    timeCap0th = 0;

	  // and time bin of capacitor = 127
	  if (timeCap0th == 0)
	    previousBin = 127;
	  else
	    previousBin = timeCap0th - 1;

	  // and time bin of capacitor = 1
	  if (timeCap0th == 127)
	    nextBin = 0;
	  else
	    nextBin = timeCap0th + 1;

	  /*
	  //Skip events where the capacitor number of 0th time bin is wrong	  	  
	  status = fData->getSequences(120,nSeq,Seq);
	    
	  if ((timeCap0th != 1) && (timeCap0th != 2))
	    if (((int)Seq[0].firstAdc[previousBin]) > ((int)Seq[0].firstAdc[timeCap0th]))  {
	      test[129] += 7;
	      return kStOK; 
	    }
	  // end of skip
	  */

          int idx = fSvtStat2->getHybridIndex(barrel, ladder, wafer, hybrid);
	  fStat2 = (StSvtHybridStat2*)fSvtStat2->at(idx);
	  if (!fStat2) {
	    fStat2 = new StSvtHybridStat2(barrel, ladder, wafer, hybrid);
	    fSvtStat2->put_at(fStat2,idx);
	  }
	  
	  anodeList = NULL;
	  nAnodes = fData->getAnodeList(anodeList);

	  for (int ianode=0;ianode<nAnodes;ianode++) {
	    
	    anodeID = anodeList[ianode];
	    Seq = NULL;
	    nSeq = 0;
	
	    status = fData->getSequences(anodeID,nSeq,Seq);
	    
	    for (iseq=0;iseq<nSeq;iseq++) {	  	  
	      for (timeSeq=0; timeSeq<Seq[iseq].length; timeSeq++) {

		time = Seq[iseq].startTimeBin + timeSeq;

		capacitor = time + nSCAZero;
		if (capacitor > 127)
		  capacitor -= 128;		

		// Exclude first two time bins from capacitor-related pedestal calculation and 
		// last and first capacitors from time bin-related pedestal calculation
		if ((time == 0) || (time == 1) || 
		    (time == previousBin) || (time == timeCap0th) || (time == nextBin)) continue;		
		fStat2->fillMom((int)Seq[iseq].firstAdc[timeSeq],anodeID,time,capacitor);
	      }
	    }
	  }
	}
      }
    }
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::CalcPed()
{
  if (Debug()) gMessMgr->Debug() << "StSvtPedMaker::CalcPed" << endm;

  float rms = 0;
  
  for (int barrel = 1;barrel <= fSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= fSvtData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= fSvtData->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= fSvtData->getNumberOfHybrids();hybrid++) {

	  if (fSvtData->getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;
	  fStat = (StSvtHybridStat*)fSvtStat->at(fSvtStat->getHybridIndex(barrel, ladder, wafer, hybrid));
	  if (!fStat) continue;

          int idxPed = fSvtPed->getHybridIndex(barrel, ladder, wafer, hybrid);
	  fPed = (StSvtHybridPed*)fSvtPed->at(idxPed);

	  if (!fPed) {
	    fPed = new StSvtHybridPed(barrel, ladder, wafer, hybrid);
	    fPed->setType(fType);
	    fSvtPed->put_at(fPed,idxPed);
          }

          int idxRms=0;
	  if (fSvtPedRms) {
            idxRms = fSvtPedRms->getHybridIndex(barrel, ladder, wafer, hybrid);
	    fPedRms = (StSvtHybridPixels*)fSvtPedRms->at(idxRms);
	    if (!fPedRms) {
	      fPedRms = new StSvtHybridPixels(barrel, ladder, wafer, hybrid);
              fSvtPedRms->put_at(fPedRms,idxRms);
            }
	  }


	  for (int anode=1; anode<=fPed->getNumberOfAnodes();anode++) {
	    for (int time=0; time<fPed->getNumberOfTimeBins();time++) {

	      fPed->AddAt(fStat->getMean(anode,time),fPed->getPixelIndex(anode,time));
	      //if ((anode == 120) && (time == 64))
	      //	cout << fPed->getPixelContent(anode,time) << endl;
	      rms += fStat->getRMS(anode,time);

	      if (fPedRms)
		fPedRms->AddAt(fStat->getRMS(anode,time),fPedRms->getPixelIndex(anode,time));
	    }
	  }

	  rms /= fPed->getNumberOfAnodes()*fPed->getNumberOfTimeBins();
	  fPed->setRMS(rms);

	  // cout << "rms = " << fPed->getRMS() << endl;

	}
      }
    }
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::CalcPed2ndOrd()
{
  for (int barrel = 1;barrel <= fSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= fSvtData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= fSvtData->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= fSvtData->getNumberOfHybrids();hybrid++) {

	  if (fSvtData->getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  fPed2 = (StSvtHybridPixels2*)fSvtPed2->at(fSvtPed2->getHybridIndex(barrel, ladder, wafer, hybrid));
	  if (!fPed2)	    
	    fPed2 = new StSvtHybridPixels2(barrel, ladder, wafer, hybrid);

	  fStat2 = (StSvtHybridStat2*)fSvtStat2->at(fSvtStat2->getHybridIndex(barrel, ladder, wafer, hybrid));

	  if (!fStat2) continue;

	  for (int time2=0; time2<fPed2->getNumberOfCapacitors();time2++) {

	    fPed = (StSvtHybridPed*)fPed2->getSvtHybridPixels(time2);
	    if (!fPed)
	      fPed = new StSvtHybridPed(barrel, ladder, wafer, hybrid);
	    fPed->setType(fType);

	    for (int anode=1; anode<=fPed->getNumberOfAnodes();anode++) {
	      for (int time=0; time<fPed->getNumberOfTimeBins();time++) {

		fPed->AddAt( fStat2->getMean(anode,time,time2),fPed->getPixelIndex(anode,time));

	      }
	    }
	    
	    fPed2->setSvtHybridPixels(fPed,time2);
	    
	  }

	  fSvtPed2->at(fSvtPed2->getHybridIndex(barrel, ladder, wafer, hybrid))
	    = fPed2;
	}
      }
    }
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::WriteToFile(const char* fileName, char* option)
{
  if (Debug()) gMessMgr->Debug() << "StSvtPedMaker::WriteToFile" << endm;

  TFile *file = new TFile(fileName,"RECREATE");
  char name[20];

  for (int barrel = 1;barrel <= fSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= fSvtData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= fSvtData->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= fSvtData->getNumberOfHybrids();hybrid++) {

	  if (fSvtData->getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  fPed = (StSvtHybridPed*)fSvtPed->at(fSvtPed->getHybridIndex(barrel, ladder, wafer, hybrid));

	  if (fPed) {
	    sprintf(name,"Ped_%d_%d_%d_%d",barrel, ladder, wafer, hybrid);
	    fPed->Write(name);
	  }
	}
      }
    }
  }

  file->Close();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::WriteRMSToFile(const char* fileName, char* option)
{
  if (Debug()) gMessMgr->Debug() << "StSvtPedMaker::WriteToFile" << endm;

  TFile *file = new TFile(fileName,"RECREATE");
  char name[20];

  for (int barrel = 1;barrel <= fSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= fSvtData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= fSvtData->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= fSvtData->getNumberOfHybrids();hybrid++) {

	  if (fSvtData->getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  fPedRms = (StSvtHybridPixels*)fSvtPedRms->at(fSvtPed->getHybridIndex(barrel, ladder, wafer, hybrid));

	  if (fPedRms) {
	    sprintf(name,"Ped_%d_%d_%d_%d",barrel, ladder, wafer, hybrid);
	    fPedRms->Write(name);
	  }
	}
      }
    }
  }

  file->Close();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::ReadFromFile(const char* fileName)
{
  if (Debug()) gMessMgr->Debug() << "StSvtPedMaker::ReadFromFile" << endm;
 
  TFile *file = new TFile(fileName);
  if (file->IsZombie()){
    gMessMgr->Info() << "Error reading file:" <<fileName<< endm;
    return kStOk;
  }
  char name[20];
 
  for (int barrel = 1;barrel <= mConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mConfig->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= mConfig->getNumberOfHybrids();hybrid++) {
	  //cout<<"chp1"<<endl;
	  if (mConfig->getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  sprintf(name,"Ped_%d_%d_%d_%d",barrel, ladder, wafer, hybrid);
	  fPed = (StSvtHybridPed*)file->Get(name);
	  
	  if (fPed) {
            int idx = fSvtPed->getHybridIndex(barrel, ladder, wafer, hybrid);
	    delete fSvtPed->at(idx); fSvtPed->at(idx)=0;
	    fSvtPed->put_at(fPed,idx);
	  }
	}
      }
    }
  }

  file->Close();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::ReadRMSFromFile(const char* fileName)
{
  if (Debug()) gMessMgr->Debug() << "StSvtPedMaker::ReadRMSFromFile" << endm;

  TFile *file = new TFile(fileName);
  if (file->IsZombie()){
    gMessMgr->Info() << "Error reading file:" <<fileName<< endm;
    return kStOk;
    }
  char name[20];

  for (int barrel = 1;barrel <= mConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mConfig->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= mConfig->getNumberOfHybrids();hybrid++) {

	  if (mConfig->getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  sprintf(name,"Ped_%d_%d_%d_%d",barrel, ladder, wafer, hybrid);
	  fPedRms = (StSvtHybridPixels*)file->Get(name);
 	  
	  if (fPedRms) {
            int idx = fSvtPedRms->getHybridIndex(barrel, ladder, wafer, hybrid);
	    delete fSvtPedRms->at(idx); fSvtPedRms->at(idx)=0;
	    fSvtPedRms->put_at(fPedRms,idx);
	    
	  }
	}
      } 
    }
  }

  file->Close();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::ResetStat()
{
  if (Debug()) gMessMgr->Debug() << "StSvtPedMaker::ResetStat" << endm;

  for (int i=0; i<fSvtStat->getTotalNumberOfHybrids();i++) {
    fStat = (StSvtHybridStat*)fSvtStat->at(i);
    if (fStat)
      fStat->reset();
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::ResetPed()
{
  if (Debug()) gMessMgr->Debug() << "StSvtPedMaker::ResetPed" << endm;

  for (int i=0; i<fSvtPed->getTotalNumberOfHybrids();i++) {
    fPed = (StSvtHybridPed*)fSvtPed->at(i);
    if (fPed)
      fPed->reset();
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::Finish()
{
  if (Debug()) gMessMgr->Debug() << "StSvtPedMaker::Finish" << endm;

  return kStOK;
}

//_____________________________________________________________________________
void StSvtPedMaker::PrintInfo()
{

  if (Debug()) StMaker::PrintInfo();
}

