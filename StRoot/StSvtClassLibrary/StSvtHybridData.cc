/***************************************************************************
 *
 * $Id: StSvtHybridData.cc,v 1.4 2001/04/30 22:20:42 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridData.cc,v $
 * Revision 1.4  2001/04/30 22:20:42  caines
 * Add Anode to setList fn so works with ZSP data
 *
 * Revision 1.3  2000/11/30 20:39:12  caines
 * Changed to allow us of database
 *
 * Revision 1.2  2000/07/30 21:13:04  munhoz
 * adding correction for copy constructor and equal operator
 *
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

  anodeList = new int[nAnodes];
  nSeq = new int[nAnodes];
  seq =  new StSequence*[nAnodes];

  for(int an = 0; an < nAnodes; an++)
    {
      anodeList[an] = hybrid.anodeList[an];
      nSeq[an] = hybrid.nSeq[an];
      seq[an] = new StSequence[nSeq[an]];

      for(int mseq = 0; mseq < nSeq[an] ; mseq++)
	{
         seq[an][mseq].startTimeBin =  hybrid.seq[an][mseq].startTimeBin;
	 seq[an][mseq].firstAdc =  hybrid.seq[an][mseq].firstAdc;
	 seq[an][mseq].length =  hybrid.seq[an][mseq].length;
	}
    }
  
}

StSvtHybridData& StSvtHybridData::operator = (const StSvtHybridData& hybrid)
{
  mBarrel   = hybrid.mBarrel;
  mLadder   = hybrid.mLadder;
  mWafer    = hybrid.mWafer;
  mHybrid   = hybrid.mHybrid;
  nAnodes   = hybrid.nAnodes;
  
  anodeList = new int[nAnodes];
  nSeq = new int[nAnodes];
  seq =  new StSequence*[nAnodes];

  for(int an = 0; an < nAnodes; an++)
    {
      anodeList[an] = hybrid.anodeList[an];
      nSeq[an] = hybrid.nSeq[an];
      seq[an] = new StSequence[nSeq[an]];

      for(int mseq = 0; mseq < nSeq[an] ; mseq++)
	{
         seq[an][mseq].startTimeBin =  hybrid.seq[an][mseq].startTimeBin;
	 seq[an][mseq].firstAdc =  hybrid.seq[an][mseq].firstAdc;
	 seq[an][mseq].length =  hybrid.seq[an][mseq].length;
	}
    }
  
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
  if(listID >= 0 && listID < nAnodes)
    {
      nSequence = nSeq[listID];
      sequence = seq[listID];
    }
  return 0;
}

int StSvtHybridData::setListSequences(int listID, int Anode, int& nSequence, StSequence* tempSeq)
{  
  if (nAnodes == 0){
    nAnodes = 240;
    anodeList = new int[nAnodes];
  }

  anodeList[listID] = Anode;
  if (!nSeq) {
    nSeq = new int[nAnodes];
    for(int i=0; i<nAnodes; i++)
      nSeq[i] = 0;
      
  }

  if (!seq)
    seq = new StSequence*[nAnodes];

  // Resets the sequences for a given anode 
  if(listID >= 0 && listID < nAnodes){
    if (seq[listID])
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

int StSvtHybridData::setAnodeList()
{
  // Loops over anode list and removes from the list those anodes who no longer have any sequences on them
 
  int newTot=0;
  
  if (!anodeList) {
    anodeList = new int[nAnodes];
    for(int i=0; i<nAnodes; i++)
      anodeList[i] = i+1;
  }

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
