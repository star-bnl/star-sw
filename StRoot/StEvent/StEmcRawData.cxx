/***************************************************************************
 *
 * $Id: StEmcRawData.cxx,v 2.2 2004/07/15 16:36:24 ullrich Exp $
 *
 * Author: Alex Suaide, Mar 2004
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcRawData.cxx,v $
 * Revision 2.2  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.1  2004/03/26 21:53:45  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StEmcRawData.h"

static const char rcsid[] = "$Id: StEmcRawData.cxx,v 2.2 2004/07/15 16:36:24 ullrich Exp $";

ClassImp(StEmcRawData)

StEmcRawData::StEmcRawData()
{
    for(int i=0;i<MAXEMCDATABANK;i++) {
	mHeader[i].Set(0);
	mData[i].Set(0);
    }
}

StEmcRawData::StEmcRawData(const StEmcRawData& h) : StObject(h)
{
  for(int i=0;i<MAXEMCDATABANK;i++) {
      if(h.header(i) && h.data(i)) {
	  createBank(i,h.sizeHeader(i),h.sizeData(i));
	  setHeader(i,(unsigned short*)h.header(i));
	  setData(i,(unsigned short*)h.data(i));
      }
  }
}

StEmcRawData::~StEmcRawData() 
{
  for(int i=0;i<MAXEMCDATABANK;i++) deleteBank(i);
}

void
StEmcRawData::createBank(int bank,int sizeHeader, int sizeData)
{
    deleteBank(bank);
    mHeader[bank].Set(sizeHeader);
    mData[bank].Set(sizeData);
    for(int i = 0;i<sizeHeader;i++) mHeader[bank][i] = 0;
    for(int i = 0;i<sizeData;i++) mData[bank][i] = 0;
}

void
StEmcRawData::deleteBank(int bank) 
{
    mHeader[bank].Set(0);
    mData[bank].Set(0);
}

void
StEmcRawData::setHeader(int bank,unsigned short* data) 
{
    if(sizeHeader(bank)==0) return;
    mHeader[bank].Set(sizeHeader(bank),(const Short_t*)data);
}

void
StEmcRawData::setData(int bank,unsigned short* data) 
{
    if(sizeData(bank)==0) return;
    mData[bank].Set(sizeData(bank),(const Short_t*)data);
}

void
StEmcRawData::setHeader(int bank,int position, unsigned short data) 
{
    if(sizeHeader(bank)==0) return;
    mHeader[bank][position] = data;
}

void
StEmcRawData::setData(int bank,int position, unsigned short data) 
{
    if(sizeData(bank)==0) return;
    mData[bank][position] = data;
}

const unsigned short*
StEmcRawData::header(int bank) const 
{
    return (unsigned short*)mHeader[bank].GetArray();
}

const unsigned short*
StEmcRawData::data(int bank) const 
{
    return (unsigned short*)mData[bank].GetArray();
}

unsigned short*
StEmcRawData::header(int bank) 
{
    return (unsigned short*)mHeader[bank].GetArray();
}

unsigned short*
StEmcRawData::data(int bank) 
{
    return (unsigned short*)mData[bank].GetArray();
}

unsigned short
StEmcRawData::header(int bank,int position) 
{
    if(sizeHeader(bank)==0) return 0;
    return mHeader[bank][position];
}

unsigned short
StEmcRawData::data(int bank,int position)
{
    if(sizeData(bank)==0) return 0;
    return mData[bank][position];
}

const unsigned short
StEmcRawData::header(int bank,int position) const  
{
    if(sizeHeader(bank)==0) return 0;
    return mHeader[bank][position];
}

const unsigned short
StEmcRawData::data(int bank,int position) const 
{
    if(sizeData(bank)==0) return 0;
    return mData[bank][position];
}

int
StEmcRawData::sizeHeader(int bank) 
{
    return mHeader[bank].GetSize();
}

int
StEmcRawData::sizeData(int bank)
{
    return mData[bank].GetSize();
}

const int
StEmcRawData::sizeHeader(int bank)  const
{
    return mHeader[bank].GetSize();
}

const int
StEmcRawData::sizeData(int bank) const
{
    return mData[bank].GetSize();
}
