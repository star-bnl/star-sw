/***********************************************************************
 * $Id: EztEmcRawData.cxx,v 1.1 2004/10/28 00:10:19 mvl Exp $
 * Author: Alex Suaide, Mar 2004, JB
 ************************************************************************/

#include "EztEmcRawData.h"
 
ClassImp(EztEmcRawData)

//----------------------------------------------------
EztEmcRawData::EztEmcRawData()
{
    for(int i=0;i<MAXEMCDATABANK;i++) {
	mHeader[i].Set(0);
	mData[i].Set(0);
	setCorruption(i,0xff); // all bad is default
    }
}

//----------------------------------------------------
EztEmcRawData::EztEmcRawData(const EztEmcRawData& h) : TObject(h)
{
  for(int i=0;i<MAXEMCDATABANK;i++) {
      if(h.header(i) && h.data(i)) {
	  createBank(i,h.sizeHeader(i),h.sizeData(i));
	  setHeader(i,(unsigned short*)h.header(i));
	  setData(i,(unsigned short*)h.data(i));
      }
  }
}

//----------------------------------------------------
EztEmcRawData::~EztEmcRawData() 
{
  for(int i=0;i<MAXEMCDATABANK;i++) deleteBank(i);
}


//----------------------------------------------------
void
EztEmcRawData::createBank(int bank,int sizeHeader, int sizeData)
{
    deleteBank(bank);
    mHeader[bank].Set(sizeHeader);
    mData[bank].Set(sizeData);
    for(int i = 0;i<sizeHeader;i++) mHeader[bank][i] = 0;
    for(int i = 0;i<sizeData;i++) mData[bank][i] = 0;
    setCorruption(bank,0xff); // all bad is default

} 

//----------------------------------------------------
void
EztEmcRawData::deleteBank(int bank) 
{
    mHeader[bank].Set(0);
    mData[bank].Set(0);
}

//----------------------------------------------------
void
EztEmcRawData::setHeader(int bank,unsigned short* data) 
{
    if(sizeHeader(bank)==0) return;
    mHeader[bank].Set(sizeHeader(bank),(const Short_t*)data);
}

//----------------------------------------------------
void
EztEmcRawData::setData(int bank,unsigned short* data) 
{
    if(sizeData(bank)==0) return;
    mData[bank].Set(sizeData(bank),(const Short_t*)data);
}


//----------------------------------------------------
const UShort_t*
EztEmcRawData::header(int bank) const 
{
    return (UShort_t*)mHeader[bank].GetArray();
}

//----------------------------------------------------
const UShort_t*
EztEmcRawData::data(int bank) const 
{
    return (UShort_t*)mData[bank].GetArray();
}

//----------------------------------------------------
const int
EztEmcRawData::sizeHeader(int bank)  const
{
    return mHeader[bank].GetSize();
}

//----------------------------------------------------
const int
EztEmcRawData::sizeData(int bank) const
{
    return mData[bank].GetSize();
}

/***************************************************************************
 *
 * $Log: EztEmcRawData.cxx,v $
 * Revision 1.1  2004/10/28 00:10:19  mvl
 * Initial revision of ezTree classes (for EEmc raw data)
 *
 *
 **************************************************************************/

