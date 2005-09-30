// *-- Author : Jan Balewski
// 
// $Id: StSpinDbMaker.cxx,v 1.1 2005/09/30 23:47:45 balewski Exp $
 

#include <time.h>
#include <string.h>

#include <TDatime.h>
#include "St_db_Maker/St_db_Maker.h"

#include "StSpinDbMaker.h"

#include "tables/St_spinDbV124_Table.h"
#include "tables/St_spinDbStar_Table.h"
#include "tables/St_spinDbBXmask_Table.h"

#include <StMessMgr.h>

ClassImp(StSpinDbMaker)

//________________________________________________________
//________________________________________________________
StSpinDbMaker::StSpinDbMaker(const char *name):StMaker(name){
  gMessMgr->Message("","D") <<GetName()<<endm;
  setDBname("Calibrations/rhic");
}


//________________________________________________________
//________________________________________________________
//_______________________________________________________
StSpinDbMaker::~StSpinDbMaker(){

}


//________________________________________________________
//________________________________________________________
//________________________________________________________
Int_t 
StSpinDbMaker::Init(){
  nFound=0; // just in case
  return StMaker::Init();
}

//__________________________________________________
//__________________________________________________
//__________________________________________________
void  
StSpinDbMaker::clearTables(){
  nFound=0;
  mTabSpinV124=0;
  mTabSpinStar=0;
  mTabSpinBXmask=0;
  int i;
  for(i=0;i<SPINDbMaxBXings;i++) {
    spin8bits[i]=-1;
    spin4bits[i]=-1;
  }
}

//__________________________________________________
//__________________________________________________
//__________________________________________________
Int_t  
StSpinDbMaker::InitRun  (int runNumber){
  gMessMgr->Message("","I") << GetName()<<"::InitRun  " <<endm;
  clearTables();
  requestDataBase();
  //  if(valid()) // tmp ?
  optimizeTables();
  gMessMgr->Message("","I") << GetName()<<"::InitRun()  Found "<< nFound<<" SPIN related tables "<<endm;

  return StMaker::InitRun(runNumber);
}  


//__________________________________________________
//__________________________________________________

void  StSpinDbMaker::requestDataBase(){
 
  printf("%s::reloadDb using TimeStamp from 'StarDb'=%p or 'db'=%p \n",GetName(),(void*)GetMaker("StarDb"),(void*)GetMaker("db"));
  

  St_db_Maker* stdb = (St_db_Maker*)GetMaker("StarDb");
  if(stdb==0) stdb = (St_db_Maker*)GetMaker("db");
  assert(stdb);
    
  StEvtHddr* fEvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
  
  gMessMgr->Message("","I") << GetName()<<"::RequestDataBase(),  event time stamp="<< (int)fEvtHddr->GetUTime()<< " , yyyy/mm/dd="<< fEvtHddr->GetDate()<<" hh/mm/ss="<<fEvtHddr->GetTime()<<endm;  
  
  gMessMgr->Message("","I") << GetName()<<"::RequestDataBase(), access DB='"<<dbName<<"'  use timeStamp="<< stdb->GetDateTime().AsString()<<endm;
  
  TDataSet *spindb=GetDataBase(dbName );
  if(spindb==0) {
    gMessMgr->Message("","E") << GetName()<<"::RequestDataBase()  Could not find dbName ="<<dbName <<endm;
    return ;
    // down-stream makers should check for presence of dataset
  }
  // spindb->ls(2);  
  getTable<St_spinDbV124,spinDbV124_st>(spindb,"spinV124",&mTabSpinV124);    
  getTable<St_spinDbStar,spinDbStar_st>(spindb,"spinStar",&mTabSpinStar);  
  getTable<St_spinDbBXmask,spinDbBXmask_st>(spindb,"spinBXmask",&mTabSpinBXmask);  
   
  gMessMgr->Message("","I") << GetName()<<"::Request valid="<< valid()<<endm;


}
 
//__________________________________________________
//__________________________________________________
//__________________________________________________

void  StSpinDbMaker::optimizeTables  (){
  /* labels of spin state for one beam 
     using respective nibble from 8-spinBit word
     0 = unpol&filled, nibble=1001
     + = up&filled   , nibble=0011
     - = down&filled , nibble=0101
     E = empty&filled, nibble=0000
     X = not allowed, any other nibble
  */
  
  //        Zero    Posit   Negat   Empty
  const int nibZ=9, nibP=3, nibN=5, nibE=0;
  printf("optimizeTables: bucketOffset: B=%d Y=%d\n",mTabSpinV124->bucketOffset[blueRing],mTabSpinV124->bucketOffset[yellRing]);
  
  int bx;
  int nP2=0,nP1=0,nP0=0,nPer=0;
  for(bx=0;bx<SPINDbMaxBXings;bx++){
    // ..... find correct time bucket for pattern rotation in each ring
    // this is tricky: bx is in STAR IP, but bBucket & yBucket have subtracted offsets modulo 120 !
    int bBucket=(bx*3+mTabSpinV124->bucketOffset[blueRing])%SPINDbMaxBuckets;
    if(bBucket<0) bBucket+=SPINDbMaxBuckets; 

    int yBucket=(bx*3+mTabSpinV124->bucketOffset[yellRing])%SPINDbMaxBuckets;

    //...... find 4 spin bits for each ring
    int yNib=mTabSpinV124->v124bits[yBucket]&0x0f;
    int bNib=mTabSpinV124->v124bits[bBucket]>>4;

    int spin8=(bNib<<4) + yNib;
      spin8bits[bx]=spin8;
    //...... map 8spin bits to 4spin bits 
    int spin4 =-1;
    if( (yNib==nibZ && bNib==nibZ) ||  (yNib==nibE && bNib==nibE) )
      spin4=0;
    else if ( yNib==nibE && bNib==nibP )  spin4=1;
    else if ( yNib==nibE && bNib==nibN )  spin4=2;
    else if ( yNib==nibP && bNib==nibE )  spin4=4;
    else if ( yNib==nibP && bNib==nibP )  spin4=5;
    else if ( yNib==nibN && bNib==nibP )  spin4=6;
    else if ( yNib==nibN && bNib==nibE )  spin4=8;
    else if ( yNib==nibP && bNib==nibN )  spin4=9;
    else if ( yNib==nibN && bNib==nibN )  spin4=10;
    // if(spin4>=0)  printf("bx=%3d bucket: B%03d Y%03d spin8=%3d spin4=%d\n",bx,bBucket,yBucket,bNib<<4+yNib,spin4);
    spin4bits[bx]=spin4;
    //.......just count types of bXings
    switch(spin4) {
    case 0: nP0++; break;
    case 1: 
    case 2: 
    case 4: 
    case 8: 
      nP1++; break;
    case 5: 
    case 6: 
    case 9: 
    case 10: 
      nP2++; break;
    default:
      nPer++;
    }
    //  printf("bx=%4d  b=%4d y=%4d  bNib=x%x yNib=x%x  spin8=x%2x spin4=%2d\n",bx,bBucket/3, yBucket/3,bNib,yNib,spin8,spin4); 

  }   
  gMessMgr->Message("","I") <<"  SPINDB mOptimized() "<<
    "bXing w/ pol2="<<nP2<<" pol1="<<nP1<<" unPol="<<nP0<<" errPol="<<nPer<<endm; 
  return;
} 


//_________________________________________________________
//_________________________________________________________
//_________________________________________________________

Int_t StSpinDbMaker::Make(){
  
  //  printf("\n\nMake :::::: %s\n\n\n",GetName());

  return kStOK;

}

//_________________________________________________________
//_________________________________________________________
//_________________________________________________________

template <class St_T, class T_st> void 
StSpinDbMaker::getTable(TDataSet *mydb,  TString tabName,   T_st** outTab ){

  *outTab=0;
  //  printf("\n\n%s::TTT --> %s, size=%d\n",GetName(),tabName.Data(),sizeof(T_st));

  char name[1000];
  sprintf(name,"%s",tabName.Data());
  
  gMessMgr->Message("","D") <<"SPINDB request="<< name <<endm;

  St_T *ds= (St_T *)mydb->Find(name);
  if(ds==0) {
    gMessMgr->Message("","W") <<"SPINDB  table='"<< name <<"' not Found in DB, continue "<<endm;
    return ;
  }

  if(ds->GetNRows()!=1) {
    gMessMgr->Message("","W") <<"SPINDB  table='"<< name <<"' no records, continue "<<endm; 
    return ;
  }
  
  *outTab=(T_st *) ds->GetArray();

  if(*outTab==0) {
    printf(" GetArray() failed\n");
    return  ;
  }

  nFound++;
  gMessMgr->Message("","I") << GetName()<<"::table '"<< name <<"': "<<(*outTab)->comment<<endm; 

  return ; // copy the whole s-struct to allow flavor change;

}


//--------------------------------------------------
//--------------------------------------------------
bool
StSpinDbMaker::isPolDir(spinDbEnum polDir){
  if(!valid()) return false;
  int i;  
  //  for(i=0;i<SPINDbMaxRing;i++) printf("rot(%d)=%d\n",i,mTabSpinV124->rotatorState[i]);
  
  for(i=0;i<SPINDbMaxRing;i++)
    if(mTabSpinV124->rotatorState[i]!=polDir) return false;
  return  true;
}

//--------------------------------------------------
//--------------------------------------------------
int
StSpinDbMaker:: spin4usingBX48(int bx48){
  if(!valid()) return -1;
  if(bx48<0 || bx48>=SPINDbMaxBXings) return -1;
  int bx=(bx48+mTabSpinStar->bXoff48)%SPINDbMaxBXings;
  // printf("inspin4..., bx48=%d bx=%d\n",bx48,bx);
  return spin4bits[bx];
}

//--------------------------------------------------
//--------------------------------------------------
int
StSpinDbMaker::spin8usingBX48(int bx48){
  if(!valid()) return -1;
  if(bx48<0 || bx48>=SPINDbMaxBXings) return -1;
  int bx=(bx48+mTabSpinStar->bXoff48)%SPINDbMaxBXings;
  return spin8bits[bx];
}

//--------------------------------------------------
//--------------------------------------------------
int
StSpinDbMaker::BX48offset(){
  if(!valid()) return -1;
  return mTabSpinStar->bXoff48;
}

//--------------------------------------------------
//--------------------------------------------------
bool
StSpinDbMaker::isBXfilledUsingBX48(int bx48){
  if(!valid()) return false;
  if(bx48<0 || bx48>=SPINDbMaxBXings) return false;
  int bx=(bx48+mTabSpinStar->bXoff48)%SPINDbMaxBXings;
  return  ((spin8bits[bx] & 0x11)==0x11);
}

//--------------------------------------------------
//--------------------------------------------------
int
StSpinDbMaker:: BXstarUsingBX48(int bx48){
  if(!valid()) return -1;
  if(bx48<0 || bx48>=SPINDbMaxBXings) return -1;
  return (bx48+mTabSpinStar->bXoff48)%SPINDbMaxBXings;
}

//--------------------------------------------------
//--------------------------------------------------
int
StSpinDbMaker::offsetBX48minusBX7(int bx48, int bx7){
  if(!valid()) return -1;
  int bxa=BXstarUsingBX48(bx48);
  int bxb=BXstarUsingBX7(bx7);
  if(bxa<0 || bxb<0) return -1;
  int diff=bxa-bxb;
  if(diff<0) diff+=SPINDbMaxBXings;
  return diff;
}


//--------------------------------------------------
//--------------------------------------------------
// EXPERT ONLY METHODS ....
//--------------------------------------------------
//--------------------------------------------------


//--------------------------------------------------
//--------------------------------------------------
const unsigned char *
StSpinDbMaker::getRawV124bits(){
  return mTabSpinV124->v124bits;
}

//--------------------------------------------------
//--------------------------------------------------
const int *
StSpinDbMaker::getBucketOffsets(){
  return mTabSpinV124->bucketOffset;
}

//--------------------------------------------------
//--------------------------------------------------
const int *
StSpinDbMaker::getSpin8bits(){
  return spin8bits;
}


//--------------------------------------------------
//--------------------------------------------------
int
StSpinDbMaker::BX7offset(){
  if(!valid()) return -1;
  return mTabSpinStar->bXoff7;
}


//--------------------------------------------------
//--------------------------------------------------
int
StSpinDbMaker:: BXstarUsingBX7(int bx7){
  if(!valid()) return -1;
  if(bx7<0 || bx7>=SPINDbMaxBXings) return -1;
  return (bx7+mTabSpinStar->bXoff7)%SPINDbMaxBXings;
}



//--------------------------------------------------
//--------------------------------------------------
void StSpinDbMaker::print(int level) {

  printf("SpinDb::print(level=%d) ...\n",level);

  printf("Dump spinDB: valid=%d polTrans=%d polLong=%d BX7off=%d BX48off=%d\n", valid(), isPolDirTrans(), isPolDirLong(), BX7offset(), BX48offset());
  if(!valid()) return;
  printf("     spinDB: timeBucket offset: Blue=%d Yell=%d\n",getBucketOffsets()[blueRing],getBucketOffsets()[yellRing]);
  printf("DB records labels:\n V124: %s \n Star: %s\n",mTabSpinV124->comment,mTabSpinStar->comment); 
  printf("dump of spin bits  @ STAR IP, for 120 bXings, range [0,119]\n");
  int bx;
  for(bx=SPINDbMaxBXings-BX48offset();bx<2*SPINDbMaxBXings-BX48offset();bx++) {
    int bx48=bx%SPINDbMaxBXings;
    int bXstar=BXstarUsingBX48(bx48);
    int blueBx=(bXstar+getBucketOffsets()[blueRing]/3)%SPINDbMaxBXings;
    int yellBx=(bXstar+getBucketOffsets()[yellRing]/3)%SPINDbMaxBXings;
    int spin8=spin8usingBX48(bx48);
    int spin4=spin4usingBX48(bx48);
    char *ftt="empty";
    if (isBXfilledUsingBX48(bx48))ftt="filled";
    printf("bXstar=%3d   spin8=0x%02x  spin4=%2d (dec) %6s  bx48=%3d   blueBx=%3d yellBx=%3d\n",bXstar,spin8,spin4,ftt,bx48,blueBx,yellBx);
  }
  
  if(level<=0) return;
  int j;
  printf("dump raw V124 spin bits:\n time bucket, spin8(hex)\n");
  for(j=0;j<SPINDbMaxBuckets;j++) 
    printf("timeBucket=%3d rawV124=0x%x\n",j,getRawV124bits()[j]);


}



// $Log: StSpinDbMaker.cxx,v $
// Revision 1.1  2005/09/30 23:47:45  balewski
// start
//
