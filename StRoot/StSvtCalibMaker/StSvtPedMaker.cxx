/***************************************************************************
 *
 * $Id: StSvtPedMaker.cxx,v 1.1 2000/08/23 13:08:12 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Pedestal calculation Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtPedMaker.cxx,v $
 * Revision 1.1  2000/08/23 13:08:12  munhoz
 * SVT pedestal calculation
 *
 *
 **************************************************************************/

#include "TH2.h"
#include "TFile.h"
#include "TKey.h"
#include "StSvtPedMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "TObjectSet.h"
#include "StDAQMaker/StDAQReader.h"
#include "StarClassLibrary/StSequence.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "StSvtClassLibrary/StSvtHybridPixels2.hh"
#include "StSvtClassLibrary/StSvtHybridStat.hh"
#include "StSvtClassLibrary/StSvtHybridStat2.hh"

ClassImp(StSvtPedMaker)

//_____________________________________________________________________________
  StSvtPedMaker::StSvtPedMaker(const char *name, pedestalType type):StMaker(name)
{
  fType = type;

  fStat     = NULL;     
  fPed      = NULL;      
  fData     = NULL;     

  fSvtStat  = NULL;  
  fSvtPed   = NULL;   
  fSvtData  = NULL;    
}

//_____________________________________________________________________________
StSvtPedMaker::~StSvtPedMaker()
{
  delete fStat;     
  delete fPed;      
  delete fData;     

  delete fSvtStat;  
  delete fSvtPed;   
  //delete fSvtData;    
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::Init()
{
  //  cout << "StSvtPedMaker::Init" << endl;

  SetSvtData();
  
  SetSvtPed();

  return StMaker::Init();
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

  dataSet = GetDataSet("StSvtData");
  assert(dataSet);
  fSvtData = (StSvtData*)(dataSet->GetObject());
  assert(fSvtData);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::SetSvtPed()
{
  fSvtStat = new StSvtHybridCollection(fSvtData->getConfiguration());

  fPedSet = new TObjectSet("StSvtPedestal");
  AddConst(fPedSet);    

  fSvtPed = new StSvtHybridCollection(fSvtData->getConfiguration());
  fPedSet->SetObject((TObject*)fSvtPed);
  assert(fSvtPed);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::SetSvtPed2ndOrd()
{
  fSvtStat2 = new StSvtHybridCollection(fSvtData->getConfiguration());

  fPedSet2 = new TObjectSet("StSvtPedestal2ndOrd");
  AddConst(fPedSet2);    

  fSvtPed2 = new StSvtHybridCollection(fSvtData->getConfiguration());
  fPedSet2->SetObject((TObject*)fSvtPed2);
  assert(fSvtPed2);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::Make()
{
  //cout << "StSvtPedMaker::Make" << endl;

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::AddStat()
{
  //cout << "StSvtPedMaker::AddStat" << endl;

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

	  nSCAZero = fSvtData->getSCAZero();

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

	  fStat = (StSvtHybridStat*)fSvtStat->at(fSvtStat->getHybridIndex(barrel, ladder, wafer, hybrid));
	  if (!fStat) {
	    fStat = new StSvtHybridStat(barrel, ladder, wafer, hybrid);
	    fSvtStat->at(fSvtStat->getHybridIndex(barrel, ladder, wafer, hybrid)) = fStat;
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
  //cout << "StSvtPedMaker::AddStat" << endl;

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

	  nSCAZero = fSvtData->getSCAZero();

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

	  fStat2 = (StSvtHybridStat2*)fSvtStat2->at(fSvtStat2->getHybridIndex(barrel, ladder, wafer, hybrid));
	  if (!fStat2) {
	    fStat2 = new StSvtHybridStat2(barrel, ladder, wafer, hybrid);
	    fSvtStat2->at(fSvtStat2->getHybridIndex(barrel, ladder, wafer,hybrid)) = fStat2;
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
  //cout << "StSvtPedMaker::CalcPed" << endl;

  for (int barrel = 1;barrel <= fSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= fSvtData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= fSvtData->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= fSvtData->getNumberOfHybrids();hybrid++) {

	  if (fSvtData->getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  fPed = (StSvtHybridPed*)fSvtPed->at(fSvtPed->getHybridIndex(barrel, ladder, wafer, hybrid));

	  if (!fPed) {
	    fPed = new StSvtHybridPed(barrel, ladder, wafer, hybrid);
	    fPed->setType(fType);
	  }

	  fStat = (StSvtHybridStat*)fSvtStat->at(fSvtStat->getHybridIndex(barrel, ladder, wafer, hybrid));

	  if (!fStat) continue;

	  for (int anode=1; anode<=fPed->getNumberOfAnodes();anode++) {
	    for (int time=0; time<fPed->getNumberOfTimeBins();time++) {

	      fPed->AddAt(fStat->getMean(anode,time),fPed->getPixelIndex(anode,time));
	    }
	  }

	  fSvtPed->at(fSvtPed->getHybridIndex(barrel, ladder, wafer, hybrid))= fPed;	   
	}
      }
    }
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtPedMaker::CalcPed2ndOrd()
{
  //  cout << "StSvtPedMaker::CalcPed2ndOrd" << endl;

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
Int_t StSvtPedMaker::WriteToFile(char* fileName, char* option)
{
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
Int_t StSvtPedMaker::ReadFromFile(char* fileName)
{
  TFile *file = new TFile(fileName);
  char name[20];

  for (int barrel = 1;barrel <= fSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= fSvtData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= fSvtData->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= fSvtData->getNumberOfHybrids();hybrid++) {

	  if (fSvtData->getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  sprintf(name,"Ped_%d_%d_%d_%d",barrel, ladder, wafer, hybrid);
	  fPed = (StSvtHybridPed*)file->Get(name);
	  	
	  if (fPed) {
	    if (fSvtPed->at(fSvtPed->getHybridIndex(barrel, ladder, wafer, hybrid)))
	      delete fSvtPed->at(fSvtPed->getHybridIndex(barrel, ladder, wafer, hybrid));
	    fSvtPed->at(fSvtPed->getHybridIndex(barrel, ladder, wafer, hybrid)) = fPed;
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
  cout << "StSvtPedMaker::ResetStat" << endl;

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
  cout << "StSvtPedMaker::ResetPed" << endl;

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
  //cout << "StSvtPedMaker::Finish" << endl;

  return kStOK;
}

//_____________________________________________________________________________
void StSvtPedMaker::PrintInfo()
{
  printf("**************************************************************\n");
  printf("* $Id: StSvtPedMaker.cxx,v 1.1 2000/08/23 13:08:12 munhoz Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

