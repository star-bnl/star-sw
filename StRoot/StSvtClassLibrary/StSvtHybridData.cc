/***************************************************************************
 *
 * $Id: StSvtHybridData.cc,v 1.1.1.1 2000/03/10 14:26:21 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridData.cc,v $
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This is the class to access the data from each hybrid.                 //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "StSvtHybridData.hh"
#include "StSequence.hh"

ClassImp(StSvtHybridData)

StSvtHybridData::StSvtHybridData() : StSvtHybridObject()
{
  // Default Constructor
  nAnodes = 0;
  anodeList = NULL;       
  nSeq = NULL;            
  seq = NULL;    
}

StSvtHybridData::StSvtHybridData(int barrel, int ladder, int wafer, int hybrid) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  //This constructor has four input parameters: Barrel, Ladder, Wafer and Hybrid number (as expected).
  nAnodes = 0;
  anodeList = NULL;       
  nSeq = NULL;            
  seq = NULL;    
}

StSvtHybridData::~StSvtHybridData()
{
  delete [] anodeList;       
  delete [] nSeq;            
  for (int ianode=0;ianode<nAnodes;ianode++)
    if (seq[ianode]) delete seq[ianode];
  delete [] seq;
}

StSvtHybridData::StSvtHybridData(const StSvtHybridData& hybrid)
{
  // Copy Constructor
  mBarrel   = hybrid.mBarrel;
  mLadder   = hybrid.mLadder;
  mWafer    = hybrid.mWafer;
  mHybrid   = hybrid.mHybrid;
  nAnodes   = hybrid.nAnodes;
  anodeList = hybrid.anodeList;       
  nSeq      = hybrid.nSeq;            
  seq       = hybrid.seq;    
}

StSvtHybridData& StSvtHybridData::operator = (const StSvtHybridData& hybrid)
{
  mBarrel   = hybrid.mBarrel;
  mLadder   = hybrid.mLadder;
  mWafer    = hybrid.mWafer;
  mHybrid   = hybrid.mHybrid;
  nAnodes   = hybrid.nAnodes;
  anodeList = hybrid.anodeList;       
  nSeq      = hybrid.nSeq;            
  seq       = hybrid.seq;    
  return *this;
}

int StSvtHybridData::getAnodeList(int*& list)
{
  // returns the number of anodes of a given hybrid that has one or more sequences of data. 
  // The anodeList contains the number (ID) of such anodes. 
  // In the case of raw data, it returns always 240 anodes.

  list = anodeList;
  return nAnodes;
}

int StSvtHybridData::getSequences(int anode, int& nSequence, StSequence*& sequence)
{
  // provides the number of sequences each anode has (nSequences) and a list of the sequences (sequence).
  // The structure StSequence (from StarClassLibrary) gives the first time bin of the sequence, 
  // the length of the sequence and a pointer for the first ADC of the sequence.

  for (int i=0;i<nAnodes;i++) {

    if (anodeList[i]==anode) {
      nSequence = nSeq[i];
      sequence = seq[i];
    }    
  }
	  
  return 0;
}

int StSvtHybridData::getListSequences(int listID, int& nSequence, StSequence*&  sequence)
{
  nSequence = 0;
  sequence = NULL;
  if(listID >= 0 && listID <= nAnodes)
    {
      nSequence = nSeq[listID];
      sequence = seq[listID];
    }
  return 0;
}

int StSvtHybridData::SetListSequences(int listID, int& nSequence, StSequence* tempSeq)
{  
  // Resets the sequences for a given anode 
  if(listID >= 0 && listID < nAnodes){
    delete [] seq[listID];
    if( nSequence > 0){
      seq[listID] = new StSequence[nSequence];
      for( int i=0; i<nSequence; i++){
	seq[listID][i].startTimeBin = tempSeq[i].startTimeBin;
	seq[listID][i].firstAdc = tempSeq[i].firstAdc;
	seq[listID][i].length = tempSeq[i].length;
      }	  
    }
    nSeq[listID]=nSequence;
  }
  return 0;
}

int StSvtHybridData::SetAnodeList()
{
  // Loops over anode list and removes from the list those anodes who no longer have any sequences on them
 
  int newTot=0;
  
  for(int i=0; i<nAnodes; i++){

    if( nSeq[i]!=0){
      nSeq[newTot]=nSeq[i];
      seq[newTot]=seq[i];
      anodeList[newTot]=anodeList[i];
      newTot++;
    }
  }
  nAnodes=newTot;
  return 0;
}
