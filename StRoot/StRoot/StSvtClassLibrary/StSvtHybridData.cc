/***************************************************************************
 *
 * $Id: StSvtHybridData.cc,v 1.11 2008/06/12 14:22:12 fisyak Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridData.cc,v $
 * Revision 1.11  2008/06/12 14:22:12  fisyak
 * Add default no. of anodes and timeBins
 *
 * Revision 1.10  2005/07/23 03:37:33  perev
 * IdTruth + Cleanup
 *
 * Revision 1.9  2004/04/29 03:45:37  perev
 * fill array by -1. walgrind complained non itializeds variable
 *
 * Revision 1.8  2003/09/17 12:57:51  munhoz
 * initializing pointer seq[]
 *
 * Revision 1.7  2002/02/12 23:09:50  munhoz
 * fixing problems for new compiler
 *
 * Revision 1.6  2002/01/05 21:44:06  caines
 * Initialise TimeZero and first sca always
 *
 * Revision 1.5  2001/08/24 20:58:35  caines
 * Zero Seq and nseq for getSequences
 *
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
#include <assert.h>
#include <string.h>
#include "StSvtHybridData.hh"
#include "StSequence.hh"

ClassImp(StSvtHybridData)

//____________________________________________________________________________
StSvtHybridData::StSvtHybridData() : StSvtHybridObject()
{
  // Default Constructor
  mSCAZero =0;
  mTimeZero = 0;
}

//____________________________________________________________________________
StSvtHybridData::StSvtHybridData(int barrel, int ladder, int wafer, int hybrid) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  //This constructor has four input parameters: Barrel, Ladder, Wafer and Hybrid number (as expected).
  mSCAZero = 0;
  mTimeZero = 0;
}

//____________________________________________________________________________
StSvtHybridData::~StSvtHybridData()
{
//   delete [] anodeList;       
//   delete [] nSeq;            
//   for (int ianode=0;ianode<nAnodes;ianode++)
//     if (seq[ianode]) delete seq[ianode];
//   delete [] seq;
}

//____________________________________________________________________________
StSvtHybridData::StSvtHybridData(const StSvtHybridData& hybrid)
{
  // Copy Constructor
  mBarrel   = hybrid.mBarrel;
  mLadder   = hybrid.mLadder;
  mWafer    = hybrid.mWafer;
  mHybrid   = hybrid.mHybrid;
  mTimeZero = hybrid.mTimeZero;
  mSCAZero  = hybrid.mSCAZero;

  seq       = hybrid.seq;  
  anodeList = hybrid.anodeList;
  lookUp    = hybrid.lookUp;
}

//____________________________________________________________________________
StSvtHybridData& StSvtHybridData::operator = (const StSvtHybridData& hybrid)
{
  mBarrel   = hybrid.mBarrel;
  mLadder   = hybrid.mLadder;
  mWafer    = hybrid.mWafer;
  mHybrid   = hybrid.mHybrid;
  mTimeZero = hybrid.mTimeZero;
  mSCAZero  = hybrid.mSCAZero;
  seq       = hybrid.seq;  
  anodeList = hybrid.anodeList;
  lookUp    = hybrid.lookUp;
  return *this;
}

//____________________________________________________________________________
int StSvtHybridData::getAnodeList(int*& list)
{
  // returns the number of anodes of a given hybrid that has one or more sequences of data. 
  // The anodeList contains the number (ID) of such anodes. 
  // In the case of raw data, it returns always 240 anodes.
  if (!anodeList.size()) setAnodeList();
  list = &anodeList[0];
  return anodeList.size();
}

//____________________________________________________________________________
int StSvtHybridData::getSequences(int anode, int& nSequence, StSequence*& sequence)
{
  // provides the number of sequences each anode has (nSequences) and a list of the sequences (sequence).
  // The structure StSequence (from StarClassLibrary) gives the first time bin of the sequence, 
  // the length of the sequence and a pointer for the first ADC of the sequence.

  nSequence = 0;
  sequence  = 0;
  if (!lookUp.size()) setAnodeList();
  if (anode>=(int)lookUp.size())	return 0;
  int i = lookUp[anode];
  assert(i<(int)seq.size());
  if (i<0 ) 				return 0;
  nSequence = seq[i].size();
  sequence  = &(seq[i][0]);
  return 0;
}

//____________________________________________________________________________
int StSvtHybridData::getListSequences(int listID, int& nSequence, StSequence*&  sequence)
{
  nSequence = 0;
  sequence = NULL;
  if(listID >= 0 && listID < (int)seq.size())
    {
      nSequence =  seq[listID].size();
      sequence  = &seq[listID][0];
    }
  return 0;
}
//____________________________________________________________________________
int StSvtHybridData::getListTruth(int listID, int& nSequence, StMCTruth*&  sequence)
{
  nSequence = 0;
  sequence = NULL;
  if(listID >= 0 && listID < (int)seq.size())
    {
      nSequence =  seq[listID].mTruth.size();
      sequence  = &seq[listID].mTruth[0];
    }
  if(!nSequence) sequence=0;
  return 0;
}

//____________________________________________________________________________
int StSvtHybridData::setListSequences(int listID, int Anode, int& nSequence, StSequence* tempSeq)
{  

  int nAnodes = seq.size();
  if (!nAnodes) {
     nAnodes = 240; 
     seq.resize(0);
     seq.resize(nAnodes);
     anodeList.resize(0);
     lookUp.resize(0);
  }
  // Resets the sequences for a given anode 
  if(listID >= 0 && listID < nAnodes){
    seq[listID].resize(nSequence);
    seq[listID].mAnode = Anode;
    seq[listID].mTruth.resize(0);
    memcpy(&(seq[listID][0]),tempSeq,nSequence*sizeof(StSequence));
  }
  return 0;
}
//____________________________________________________________________________
int StSvtHybridData::setListTruth(int listID, int Anode, int& nSequence, StMCTruth* tempTru)
{  

  int nAnodes = seq.size();
  // Resets the sequences for a given anode 
  if(listID >= 0 && listID < nAnodes){
    assert(Anode==seq[listID].mAnode);
    assert((int)seq[listID].size()==nSequence);
    seq[listID].mTruth.resize(nSequence);
    memcpy(&(seq[listID].mTruth[0]),tempTru,nSequence*sizeof(StMCTruth));
  }
  return 0;
}

//____________________________________________________________________________
int StSvtHybridData::setAnodeList()
{
  // Loops over anode list and removes from the list those anodes who no longer have any sequences on them
 
  int newTot=0;
  int nAnodes = seq.size();  
  anodeList.resize(nAnodes);
  int maxAnode = 0;
  for(int i=0; i<nAnodes; i++){
    if(!seq[i].size()) continue;
    if (maxAnode<seq[i].mAnode) maxAnode=seq[i].mAnode;
    if (newTot!=i) {
      seq[newTot].mAnode = seq[i].mAnode;
      seq[newTot].swap(seq[i]);
      seq[newTot].mTruth.swap(seq[i].mTruth);
    }
    anodeList[newTot] = seq[newTot].mAnode;
    newTot++;
  }
  nAnodes = newTot;
  seq.resize(nAnodes);
  anodeList.resize(nAnodes);
  lookUp.resize(0);
  lookUp.resize(maxAnode+1,-1);
  for(int i=0; i<nAnodes; i++){lookUp[seq[i].mAnode] = i;}
  
  return 0;
}
