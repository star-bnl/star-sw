/***********************************************************************
 * $Id: EztEmcRawData.cxx,v 1.5 2005/03/10 16:59:54 mvl Exp $
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
	setCorruption(i,0xffff); // all bad is default
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
    setCorruption(bank,0xffff); // all bad is default

} 

//----------------------------------------------------
void
EztEmcRawData::deleteBank(int bank) 
{
    mHeader[bank].Set(0);
    mData[bank].Set(0);
    setCorruption(bank,0xffff); // all bad is default
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

//----------------------------------------------------
bool
EztEmcRawData::purgeCrateOFF(int ib){
  bool isOFF=isCrateOFF(header(ib));
  if(isOFF) { // crate was OFF based on the header
    mData[ib].Set(0); // erase data block
  }
  return isOFF;
} 

//----------------------------------------------------
void 
EztEmcRawData::tagHeadValid( int ib, int token, int crId, int len, int trigComm, int errFlag, int dbg)  {
  UShort_t ret= isHeadValid(header(ib), token, crId,  len, trigComm,  errFlag, dbg);
  setCorruption(ib,ret);
  // return ret;
} 

//----------------------------------------------------
bool
EztEmcRawData::doHeadCorruptionTest(int token, int lenCount, int firstCrId,  int errFlag){
  // general corruption test of all types
  // RETURN : true =any corruption, false=no corruption
  int trigComm=0x4; // physics, 9=laser/LED, 8=??

  int nCrIn=0;
  int nCrOK=0;
  int icr;
  for(icr=0;icr<getNBlocks();icr++) {
    if(isCrateVoid(icr)) continue;
    if(purgeCrateOFF(icr)) continue;
    nCrIn++;
    int crID=icr+firstCrId;
    //........... hader..........
    tagHeadValid(icr,token, crID,lenCount,trigComm,errFlag);
    if(getCorruption(icr)) continue;
    nCrOK++; // count number of valid crates
  }

  return nCrOK!=nCrIn;
}


//----------------------------------------------------
bool
EztEmcRawData::isCrateOFF( const UShort_t* hd) {
  bool a=(hd[0]& 0xFFF)==0xFFF; // Gerard's prescription
  bool b=(hd[1]& 0xFFF)==0xFFF;
  return a && b;
}

//----------------------------------------------------
UShort_t  
EztEmcRawData::isHeadValid(const UShort_t* hd, int token, int crId, int len, int trigComm, int errFlag, int dbg) {// just test one header  
  // encode failure of all test as subsequent bits
  UShort_t ret=0;
  if (getCrateID(hd)!=crId) ret |=bitCrateID;
  if (getToken(hd)!=token)  ret |=bitToken;
  if (getLenCount(hd)!=len) ret |=bitLenCount;
  if (getTrigComm(hd)!=trigComm) ret |=bitTrigComm;
  if (getErrFlag(hd)!=errFlag)   ret |=bitErrFlag;
  if (dbg) {
    print(hd);
    printf("getCrateID()/0x is=%x %x=required\n",getCrateID(hd),crId);
    printf("getToken()/0x = %x %x\n",getToken(hd),token);
    printf("getLenCount()/0x = %x %x\n",getLenCount(hd),len);
    printf("getTrigComm()/0x = %x %x\n",getTrigComm(hd),trigComm);
    printf("getErrFlag()/0x = %x %x\n",getErrFlag(hd),errFlag);
    printf(" corruption=0x%02x\n",ret);
  }
  return ret;
}
  
 
//----------------------------------------------------
void
EztEmcRawData::print(int ib, int flag) 
{
  printf("EztEmcRawData block=%d corruption=0x%04x\n",ib,getCorruption(ib));

  if(flag<=0) {   print(header(ib)); return; }

   print(header(ib),data(ib),sizeData(ib));
  
}

//----------------------------------------------------
void
EztEmcRawData::print(int flag) 
{
  int icr, nb=0;
  for(icr=0;icr<getNBlocks();icr++) {
    if(sizeHeader(icr)<=0) continue; // skip empty bloks
    print(icr,flag);
    nb++;
  }
  printf("EztEmcRawData total %d of non empty blocks\n\n",nb);

}

//----------------------------------------------------
void EztEmcRawData::print(const UShort_t* hd, const UShort_t* d, int nd) {
  printf("EztEmcRawData Head: 0x%04hx 0x%04hx 0x%04hx 0x%04hx ",hd[0],hd[1],hd[2],hd[3]);
  printf("\n --> token=0x%2x  crateID=0x%x  trigComm=0x%x  lenCount=0x%x  errFlag=0x%x\n",
  getToken(hd),getCrateID(hd),getTrigComm(hd),getLenCount(hd),getErrFlag(hd));
  if(d==0) return;
  printf("Data[%3d]:",nd);
  for(int i=0;i<nd;i++) {
    if( i%8 == 0 ) printf("\n");
    printf("0x%04hx ",d[i]);   
  }
  printf("\n");
}

/***************************************************************************
 *
 * $Log: EztEmcRawData.cxx,v $
 * Revision 1.5  2005/03/10 16:59:54  mvl
 * New routine for corruption checking by Jan Balewski
 *
 * Revision 1.4  2005/01/11 21:33:40  mvl
 * Minor mod to hearder checking (do not delete data for certain error states ) By Jan Balewski.
 *
 * Revision 1.2  2004/11/29 18:36:59  mvl
 * New code for header checks and some printing (by Jan Balewski)
 *
 * Revision 1.1  2004/10/28 00:10:19  mvl
 * Initial revision of ezTree classes (for EEmc raw data)
 *
 *
 **************************************************************************/

