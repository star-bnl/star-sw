// STAR
#include "StChain.h" // Needed before StTpcDb.h ; should be added there
//#include "St_DataSetIter.h"
//#include "StTpcDb/StTpcDb.h"
#include "StDAQMaker/StDAQReader.h"
#include "StDaqLib/TPC/trans_table.hh"
// C/C++
#include <fstream.h>
#include <strstream.h>
#include <iomanip.h>
#include <math.h>
// Root
#include "TH1.h"
#include "TH2.h"
// Local
#include "StTpcCalibSector.h"
#include "StTpcCalibSetup.h"

ClassImp(StTpcCalibSector)
//
// _____________________________________________________________________
//
StTpcCalibSector::StTpcCalibSector(const StTpcCalibSetup* aSetup, 
			     const  int aSectorId,
			     int* aNumberOfPadAtRow)
: 
mSectorId(aSectorId),
mSetup(aSetup),
mNumberOfPadAtRow(aNumberOfPadAtRow)
{  

  //int tMaxNumberOfPad=gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(gStTpcDb->PadPlaneGeometry()->firstOuterSectorPadRow()-1);
int tMaxNumberOfPad=182;
//int tNumberOfRow=gStTpcDb->PadPlaneGeometry()->numberOfRows();
int tNumberOfRow=45;
//int tNumberOfInnerRow=gStTpcDb->PadPlaneGeometry()->firstOuterSectorPadRow()-1;
int tNumberOfInnerRow=13;
//int tFirstOuterRow=gStTpcDb->PadPlaneGeometry()->firstOuterSectorPadRow();
int tFirstOuterRow=14;
//int tLastOuterRow=gStTpcDb->PadPlaneGeometry()->lastOuterSectorPadRow();
int tLastOuterRow=45;
int tNumberOfOuterRow=tLastOuterRow-tFirstOuterRow+1;
//int tMaxNumberOfOuterPad=gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(gStTpcDb->PadPlaneGeometry()->lastOuterSectorPadRow());
int tMaxNumberOfOuterPad=144;



  char tBuffHName[20];
  char tBuffHTitle[100];
  ostrstream* tHName;
  ostrstream* tHTitle;

  tHName = new  ostrstream(tBuffHName, 20);
  (*tHName) << "HCor" << mSectorId << ends;
  tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHTitle) <<"Number of corrupted pad, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHCorrupted= new TH1F(tHName->str(),tHTitle->str(),513,0.,512.);

  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HRMS" << mSectorId << ends;
  (*tHTitle) <<"Mean RMS, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHMeanRMS= new TH1F(tHName->str(),tHTitle->str(),65,0.,16.);

  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HCorMap" << mSectorId << ends;
  (*tHTitle) <<"Corrupted pad, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHCorruptedMap= new TH2F(tHName->str(),tHTitle->str(),
			   tMaxNumberOfPad, 0.5, tMaxNumberOfPad+0.5,
			   tNumberOfRow, 0.5, tNumberOfRow+0.5);

  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HBadMap" << mSectorId << ends;
  (*tHTitle) <<"Bad pad map, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHBadMap = new TH2S(tHName->str(),tHTitle->str(),
		      tMaxNumberOfPad, 0.5, tMaxNumberOfPad+0.5,
		      tNumberOfRow, 0.5, tNumberOfRow+0.5);

  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HBadFEE" << mSectorId << ends;
  (*tHTitle) <<"Number of bad pads per FEE, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHBadFEE = new TH1S(tHName->str(),tHTitle->str(),182,0.5,182.5);

  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HBadRDO" << mSectorId << ends;
  (*tHTitle) <<"Number of bad pads per RDO, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHBadRDO = new TH1S(tHName->str(),tHTitle->str(),6,0.5,6.5);

  // Pulser calc
  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HAmp" << mSectorId << ends;
  (*tHTitle) <<"Cluster amplitude, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHAmp = new TH1F(tHName->str(),tHTitle->str(),201,0.,2000.);

  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HNSeq" << mSectorId << ends;
  (*tHTitle) <<"Number sequence per pads, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHNSequence = new TH1F(tHName->str(),tHTitle->str(),101,0.,100.);

  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HT0" << mSectorId << ends;
  (*tHTitle) <<"t0 distribution, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHT0 = new TH1F(tHName->str(),tHTitle->str(),100,
		  mSetup->getExpectedPulsePos()-mSetup->getMinDistToPulse(),
		  mSetup->getExpectedPulsePos()+mSetup->getMinDistToPulse());

  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HAmpMap" << mSectorId << ends;
  (*tHTitle) <<"Cluster amplitude, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHAmpMap = new TH2F(tHName->str(),tHTitle->str(),
		      tMaxNumberOfPad, 0.5, tMaxNumberOfPad+0.5,
		      tNumberOfRow, 0.5, tNumberOfRow+0.5);

  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HT0Map" << mSectorId << ends;
  (*tHTitle) <<"Cluster t0, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHT0Map = new TH2F(tHName->str(),tHTitle->str(),
		     tMaxNumberOfPad, 0.5, tMaxNumberOfPad+0.5,
		     tNumberOfRow, 0.5, tNumberOfRow+0.5);

  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HFoundMap" << mSectorId << ends;
  (*tHTitle) <<"N Cluster found, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHFoundMap = new TH2F(tHName->str(),tHTitle->str(),
                        tMaxNumberOfPad, 0.5, tMaxNumberOfPad+0.5,
                        tNumberOfRow, 0.5, tNumberOfRow+0.5);

  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HDeadMap" << mSectorId << ends;
  (*tHTitle) <<"Dead pads, sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHDeadMap = new TH2F(tHName->str(),tHTitle->str(),
                        tMaxNumberOfPad, 0.5, tMaxNumberOfPad+0.5,
                        tNumberOfRow, 0.5, tNumberOfRow+0.5);

  // Calibration histo
  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HICal" << mSectorId << ends;
  (*tHTitle) <<"Calibration coeficient, inner sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle ->str()<< endl;  
  mHInnerCalibMap = new TH2F(tHName->str(),tHTitle->str(),
			     tMaxNumberOfPad, 0.5, tMaxNumberOfPad+0.5,
			     tNumberOfInnerRow, 0.5, tNumberOfInnerRow+0.5);

  delete tHName;tHName = new  ostrstream(tBuffHName, 20);
  delete tHTitle;tHTitle = new  ostrstream(tBuffHTitle, 100);
  (*tHName) << "HOCal" << mSectorId << ends;
  (*tHTitle) <<"Calibration coeficient, outer sector " << mSectorId << ends;
  //cout << "Construct " << tHName->str() << " " << tHTitle->str() << endl;  
  mHOuterCalibMap = new TH2F(tHName->str(),tHTitle->str(),
			     tMaxNumberOfPad, 0.5, tMaxNumberOfPad+0.5,
			     tNumberOfOuterRow, tFirstOuterRow-0.5, 
			     tLastOuterRow+0.5);

}
// __________________________________
//
StTpcCalibSector::~StTpcCalibSector(){
  delete mHCorrupted;
  delete mHCorruptedMap;
  delete mHMeanRMS;
  delete mHBadMap;  
  delete mHNSequence;
  delete mHAmp;
  delete mHAmpMap;
  delete mHT0;
  delete mHT0Map;  
  delete mHFoundMap;
  delete mHDeadMap;
  delete mHInnerCalibMap;
  delete mHOuterCalibMap;
}
//
// _____________________________________________________________________
//
// __________________________________
//
void StTpcCalibSector::updateBad(StTPCReader* aRMSReader){
  cout << "StTpcCalibSector::updateBad " << mSectorId << endl;
  //int tNumberOfRows=gStTpcDb->PadPlaneGeometry()->numberOfRows();
  int tNumberOfRows=45;
  int tOk,tNTB;
  unsigned char* tRMSArray;
  for(int tiRow=1;
      tiRow<=tNumberOfRows;
      tiRow++){
    for(int tiPad=1;
	tiPad<=mNumberOfPadAtRow[tiRow-1];//gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(tiRow);
	tiPad++){
      tOk = aRMSReader->getRMSPedestals(mSectorId, tiRow, tiPad,
					tNTB, tRMSArray);
      //    cout << tiRow << " " << tiPad << " " << tOk << " " << tNTB << endl;
      float tMeanRMS=0.;
      int tNCorrupted=0;
      int tN=0;
      if(tOk && tNTB==512){
	for(int tiTB=mSetup->getFirstTB();tiTB<=mSetup->getLastTB();tiTB++){
	  if(((int)tRMSArray[tiTB])==255){
	    tNCorrupted++;
	  }
	  tMeanRMS+=((int)tRMSArray[tiTB])/16.;
	  tN++;
	}
      }
      tMeanRMS/=tN;
      mHCorrupted->Fill(tNCorrupted);
      mHMeanRMS->Fill(tMeanRMS);
      mHCorruptedMap->Fill(tiPad,tiRow,tNCorrupted);
      if(tNCorrupted>mSetup->getMaxNumberOfCorruptedTB() || tMeanRMS==0.){
	mHBadMap->Fill(tiPad,tiRow,1.);
      }
    }
  } 
}
// __________________________________
//
void StTpcCalibSector::updateDead(StTPCReader* aZSupReader){
  updateGain(aZSupReader);
}
// __________________________________
//
void StTpcCalibSector::updateGain(StTPCReader* aZSupReader){
  //int tNumberOfRows=gStTpcDb->PadPlaneGeometry()->numberOfRows();
  int tNumberOfRows=45;

  float tDistToPulseTBPos;
  int tFound;
  double tPeakPos,tPeakAmp;
  TPCSequence* tSeq;
  unsigned char* tPadList;
  int tADC;
  // --- Get the pad list a the given row
  for(int tRowId=1;tRowId<=tNumberOfRows;tRowId++){
    int tNPad = aZSupReader->getPadList(mSectorId,tRowId, tPadList);
    // --- Get the data for a given pad
    int tNSeq; 
    int tOk;
    for(int tPadId=0;tPadId<tNPad;tPadId++){
      tDistToPulseTBPos=(float)mSetup->getMinDistToPulse();
      tFound=0;
      int tThisPad=tPadList[tPadId];
      tOk = aZSupReader->getSequences(mSectorId,tRowId, tThisPad
				      , tNSeq, tSeq);
      // --- Fill the cluster finder
      if (tOk==0 && tNSeq>0) {
	for(int tISeq=0;tISeq<tNSeq;tISeq++){
	  int tClAmp=0;
	  double tClPos=0;
	  for(int tSeqL=0;tSeqL<tSeq[tISeq].Length;tSeqL++){
	    tADC=tSeq[tISeq].FirstAdc[tSeqL];
	    tADC=log8to10_table[tADC];
	    tClAmp+=tADC;
	    tClPos+=tADC*(tSeq[tISeq].startTimeBin+tSeqL);
	  }
	  tClPos/=tClAmp;
	  float tCurDistToPulseTBPos=fabs(tClPos-
					  mSetup->getExpectedPulsePos());
	  if(tCurDistToPulseTBPos<tDistToPulseTBPos){
	    tFound=1;
	    tPeakPos=tClPos;
	    tPeakAmp=tClAmp;
	    tDistToPulseTBPos=tCurDistToPulseTBPos;
	  }
	}
      }
      mHNSequence->Fill(tNSeq);
      mHNSequenceMap->Fill(tPadList[tPadId],tRowId,tNSeq);
      if(tFound){
	mHAmp->Fill(tPeakAmp);
	mHT0->Fill(tPeakPos);
	mHAmpMap->Fill(tPadList[tPadId],tRowId,tPeakAmp);
	mHT0Map->Fill(tPadList[tPadId],tRowId,tPeakPos);
	mHFoundMap->Fill(tPadList[tPadId],tRowId,1.);
      }
    }
  }
}
//
// _____________________________________________________________________
//
// __________________________________
//
void StTpcCalibSector::findBad(){

}
// __________________________________
//
void StTpcCalibSector::findBadElectronics(int** aPadToFeeConvertor,
				       int** aPadToRDOConvertor){
for(int tiRow=1;tiRow<=mHBadMap->GetNbinsY();tiRow++){
  for(int tiPad=1; tiPad<=mHBadMap->GetNbinsX();tiPad++){
    if(mHBadMap->GetCellContent(tiPad,tiRow)==1) {
      //      cout << tiPad << " " << tiRow 
      //	   << aPadToFeeConvertor[tiRow-1][tiPad-1] << " " 
      //	   << aPadToRDOConvertor[tiRow-1][tiPad-1] << endl;
      mHBadFEE->Fill(aPadToFeeConvertor[tiRow-1][tiPad-1],1.);
      mHBadRDO->Fill(aPadToRDOConvertor[tiRow-1][tiPad-1],1.);
    }
  }
} 
}
// __________________________________
//
void StTpcCalibSector::findDead(){
  //int tNumberOfRows=gStTpcDb->PadPlaneGeometry()->numberOfRows();
  int tNumberOfRows=45;

  for(int tiRow=1;
      tiRow<tNumberOfRows;
      tiRow++){
    for(int tiPad=1;
	tiPad<mNumberOfPadAtRow[tiRow-1];//gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(tiRow);
	tiPad++){
      if(mHFoundMap->GetCellContent(tiPad,tiRow)==0.){
	mHDeadMap->Fill(tiPad,tiRow,1.);
      }
    }
  }
}

void StTpcCalibSector::calcGainCoeficient(){
  const int tPadExcluded=mSetup->getNSidePadsExcluded();
  //int tFirstOuterPadRow=gStTpcDb->PadPlaneGeometry()->firstOuterSectorPadRow();
  int tFirstOuterPadRow=14;
  //int tNumberOfRows=gStTpcDb->PadPlaneGeometry()->numberOfRows();
  int tNumberOfRows=45;

  double tGood=0.; double tAll=0.;
  int tiRow,tiPad;
  double tMeanAmp;
  int tNCount;

  mHAmpMap->Divide(mHFoundMap);
  mHT0Map->Divide(mHFoundMap);

// Inner sector
// >>> Mean amplitude calculation
  tMeanAmp=0.;
  tNCount=0;
  for(tiRow=1;
      tiRow<tFirstOuterPadRow;
      tiRow++){
    for(tiPad=tPadExcluded;
	tiPad<=(mNumberOfPadAtRow[tiRow-1]//gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(tiRow)
		-tPadExcluded);
	tiPad++){
      if(mHBadMap->GetCellContent(tiPad,tiRow)==0 &&
	 mHFoundMap->GetCellContent(tiPad,tiRow)!=0) {
	tMeanAmp+=mHAmpMap->GetCellContent(tiPad,tiRow);
	tNCount++;
      }
    }
  }
  tMeanAmp/=tNCount;
  // >>> make the map
  for(tiRow=1;
      tiRow<tFirstOuterPadRow;
      tiRow++){
    for(tiPad=1;
	tiPad<=mNumberOfPadAtRow[tiRow-1];//gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(tiRow);
	tiPad++){
      double tCalibCoef=0.;
      if(mHBadMap->GetCellContent(tiPad,tiRow)==0) {
	if(mHFoundMap->GetCellContent(tiPad,tiRow)==0 || 
	   tMeanAmp==0. || tiPad<tPadExcluded || 
	   tiPad>(mNumberOfPadAtRow[tiRow-1]//gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(tiRow)
		  -tPadExcluded)){
          tCalibCoef=1.;
	  if(mHFoundMap->GetCellContent(tiPad,tiRow)==0) {
	    mHDeadMap->Fill(tiPad,tiRow,1.);
	  }
	}
	else{
          tCalibCoef=tMeanAmp/mHAmpMap->GetCellContent(tiPad,tiRow);
	}	
      }
      tAll++;
      tGood+=tCalibCoef;
      mHInnerCalibMap->Fill(tiPad,tiRow,tCalibCoef);
    }
  }  
  // Outer sector
  // >>> Mean amplitude calculation
  tMeanAmp=0.;
  tNCount=0;
  for(tiRow=tFirstOuterPadRow;tiRow<=tNumberOfRows;tiRow++){
    for(tiPad=tPadExcluded;
	tiPad<=(mNumberOfPadAtRow[tiRow-1]//gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(tiRow)
		-tPadExcluded);
	tiPad++){
      if(mHBadMap->GetCellContent(tiPad,tiRow)==0 &&
	 mHFoundMap->GetCellContent(tiPad,tiRow)!=0) {
	tMeanAmp+=mHAmpMap->GetCellContent(tiPad,tiRow);
	tNCount++;
      }
    }
  }
  tMeanAmp/=tNCount;
  // >>> make the map
  for(tiRow=tFirstOuterPadRow;tiRow<=tNumberOfRows;tiRow++){
    for(tiPad=1;
	tiPad<=mNumberOfPadAtRow[tiRow-1];//gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(tiRow);
	tiPad++){
      double tCalibCoef=0.;
      if(mHBadMap->GetCellContent(tiPad,tiRow)==0) {
	if(mHFoundMap->GetCellContent(tiPad,tiRow)==0 || 
	   tMeanAmp==0. ||
	   tiPad<tPadExcluded || 
	   tiPad>(mNumberOfPadAtRow[tiRow-1]//(gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(tiRow)
		  -tPadExcluded)){
	  tCalibCoef=1.;
	  if(mHFoundMap->GetCellContent(tiPad,tiRow)==0){
	    mHDeadMap->Fill(tiPad,tiRow,1.);
	  }
	}
        else{
          tCalibCoef=tMeanAmp/mHAmpMap->GetCellContent(tiPad,tiRow);
        }     
      }
      tAll++;
      tGood+=tCalibCoef;
      mHOuterCalibMap->Fill(tiPad,tiRow,tCalibCoef);
    }
  }
  //return ((1-tGood/tAll)*100.);
}
//
// _____________________________________________________________________
//
// __________________________________
//
void StTpcCalibSector::writeBadTable(ofstream* aOutFile){
  int tNBadRows=0;
  for(int tiRow=1;tiRow<=mHBadMap->GetNbinsY();tiRow++){
    if(mHBadMap->Integral(1,mHBadMap->GetNbinsX(),tiRow,tiRow)!=0) {
      tNBadRows++;
    }
  }
  (*aOutFile) << "Sector " << mSectorId << " " << tNBadRows << endl;
  if(tNBadRows!=0){
    int tNBadPads;
    for(int tiRow=1;tiRow<=mHBadMap->GetNbinsY();tiRow++){
      tNBadPads=(int)mHBadMap->Integral(1,mHBadMap->GetNbinsX(),tiRow,tiRow);
      if(tNBadPads!=0) {
	(*aOutFile) << "Row " << tiRow << " " 
		    <<tNBadPads << endl;
	for(int tiPad=1; tiPad<=mHBadMap->GetNbinsX();tiPad++){
	  if(mHBadMap->GetCellContent(tiPad,tiRow)==1) {
	    (*aOutFile) << tiPad << " ";
	  }
	}
	(*aOutFile) << endl;
      }  
    }
  }
}


// __________________________________
//
void StTpcCalibSector::readBadTable(ifstream* aInFile){
}
// __________________________________
//
void StTpcCalibSector::writeDeadTable(ofstream* aOutFile){
}
// __________________________________
//
void StTpcCalibSector::readDeadTable(ifstream* aInFile){
}
// __________________________________
//
void StTpcCalibSector::writeCalibCoefTable(ofstream* aOutFile){
}
// __________________________________
//
void StTpcCalibSector::writeBadHisto(){
  mHCorrupted->Write();
  mHMeanRMS->Write();
  mHCorruptedMap->Write();
  mHBadMap->Write();
  mHBadFEE->Write();
  mHBadRDO->Write();
}
// __________________________________
//
void StTpcCalibSector::writeDeadHisto(){
  mHFoundMap->Write();
  mHNSequence->Write();
  mHDeadMap->Write();
}
// __________________________________
//
void StTpcCalibSector::writeGainHisto(){
  mHAmp->Write();
  mHT0->Write();
  mHAmpMap->Write();
  mHT0Map->Write();
  mHInnerCalibMap->Write();
  mHOuterCalibMap->Write();
}
// __________________________________
//
void StTpcCalibSector::writeAllHisto(){
  writeBadHisto();
  writeDeadHisto();
  writeGainHisto();
}


