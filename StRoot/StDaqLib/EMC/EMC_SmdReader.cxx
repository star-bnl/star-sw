
/***************************************************************************
 * $id: EMC Barrel SMD daq reader.
 * Author: Subhasis and Herbert Ward
 ***************************************************************************
 *  Navigates through pointers and Fills Barrel Smd Structs
 *
 **************************************
************************************/

#include "EMC_SmdReader.hh"
#include <assert.h>
#define MAX_ADC 0xFFF

int FEE1[4]={1,1,1,2};
int FEE2[4]={2,2,3,3};
int FEE3[4]={3,4,4,4};
 
int connector1[20]={1,2,3,1,4,5,6,4,7,8,9,7,10,11,12,10,13,14,15,13};
int connector2[20]={2,3,1,2,5,6,4,5,8,9,7,8,11,12,10,11,14,15,13,14};
int connector3[20]={3,1,2,3,6,4,5,6,9,7,8,9,12,10,11,12,15,13,14,15};         




EMC_SmdReader::EMC_SmdReader(EventReader* er,Bank_EMCP *pEMCP): pBankEMCP(pEMCP),ercpy(er)
{
  cout<<"ctor EMC_Smdreader**"<<endl;
}


void EMC_SmdReader::Initialize()
{
  //
  //  for(int i=0;i<4800;i++)
  //  {
  //   mTheTowerAdcR.TowerADCArray[i]=0;
  //   mTheTowerAdcD.TowerADCArray[i]=0;
  //   mTheTowerPedR.TowerADCArray[i]=0;
  //   mTheTowerRMSR.TowerADCArray[i]=0; 
  //  }

        mTheSmdAdcR.NSmdHits = 0;

  // Initialize SMDDATA array to 0's
  for(int i = 0 ; i <120 ; i++) {

    //SMD_eta
    for(int j = 0 ; j < 150; j++) {
        mTheSmdAdcR.SmdE_ADCMatrix[i][j] = 0;
    }
  //SMD_Phi
    for(int j = 0 ; j < 10; j++) {
      for(int k = 0;k < 15; k++){
        mTheSmdAdcR.SmdP_ADCMatrix[i][j][k] = 0;
    }
    }
  }

}

/////////////////////////////////////////////////////////////////////
  Bank_EMCSECP* EMC_SmdReader::getBarrelSection(const Bank_EMCP* pBankEMCP,int section)
{

  if((!pBankEMCP->EMCSecPointer[section].offset) ||
     (!pBankEMCP->EMCSecPointer[section].length))
  {
    char str0[40];
    sprintf(str0,"getBarrelsection(hs %d)",section);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0);
    cout<<" SmdReader::getBarrelSection** , no offset or length for section**"<<section<<endl;
    return NULL;
  } 


  Bank_EMCSECP * ptr=(Bank_EMCSECP*)(((INT32 *)pBankEMCP)+pBankEMCP->EMCSecPointer[1].offset); // 2nd pointer is for SMD 
 
//put some checks
  if(strncmp(ptr->header.BankType,"EMCSECP",7)) {
    char str0[40];
    cout<<" error in header name**"<<endl;
    sprintf(str0,"getBarrelSection(hs %d)",section);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return NULL;
  }
   if(!ptr->test_CRC()) {
    char str0[40];
    cout<<"error in CRC**"<<endl;
    sprintf(str0,"getBarrelsection(hs %d)",section);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL;
  }
  printf("Byte order of header for EMCSECP Before swap**: %x\n",ptr->header.ByteOrder);
           if(ptr->swap() < 0) {
    char str0[40];
    cout<<"error in swap**"<<endl;
    sprintf(str0,"getBarrelsection(hs %d)",section);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL;
  }

   ptr->header.CRC = 0;
   return ptr;
}

  Bank_EMCRBP* EMC_SmdReader::getBarrelSmdFiber(Bank_EMCSECP* secp,int section)
{
  if((!secp->FiberPointer[section].offset) ||
     (!secp->FiberPointer[section].length))
  {
    char str0[40];
    sprintf(str0,"getBarrelSmdFiber(hs %d)",section);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0);
    cout<<" getBarrelSmdfiber** , no offset or length for section**"<<section<<endl;
    return NULL;
  } 

 Bank_EMCRBP* ptr =(Bank_EMCRBP*)(((INT32 *)secp)+secp->FiberPointer[section].offset); 
 
  // some checks and swap

  if(strncmp(ptr->header.BankType,"EMCRBP",6)) {
    char str0[40];
    cout<<" error in header name**"<<endl;
    sprintf(str0,"getBarrelSmdfiber(hs %d)",section);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return NULL;
  }
   if(!ptr->test_CRC()) {
    char str0[40];
    cout<<"error in CRC**"<<endl;
    sprintf(str0,"getBarrelSmdfiber(hs %d)",section);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL;
  }
           if(ptr->swap() < 0) {
    char str0[40];
    cout<<"error in swap**"<<endl;
    sprintf(str0,"getBarrelSmdFiber(hs %d)",section);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL;
  }

   ptr->header.CRC = 0;
   return ptr;
}



  Bank_SMDADCR* EMC_SmdReader::getSmdADC(Bank_EMCRBP* rbp)
{
  if((!rbp->EMCADCR.offset) ||
     (!rbp->EMCADCR.length))
  {
    char str0[40];
    sprintf(str0,"getSmdADC(hs )");
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0);
    cout<<" getBarrelADC** , no offset or length for ADCR**"<<endl;
    return NULL;
  } 


 Bank_SMDADCR* pADCR =(Bank_SMDADCR*)(((INT32 *)rbp)+rbp->EMCADCR.offset); 

  // some checks and swap

  if(strncmp(pADCR->header.BankType,"EMCADCR",7)) {
    char str0[40];
    cout<<" error in header name**"<<endl;
    sprintf(str0,"getSmdADC(hs)");
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return NULL;
  }
   if(!pADCR->test_CRC()) {
    char str0[40];
    cout<<"error in CRC**"<<endl;
    sprintf(str0,"getSmdADC(hs)");
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL;
  }
           if(pADCR->swap() < 0) {
    char str0[40];
    cout<<"error in swap**"<<endl;
    sprintf(str0,"getBankSVTSECP(hs)");
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL;
  }

   pADCR->header.CRC = 0; 
return pADCR;
}



   int EMC_SmdReader::FillBarrelSmd(Bank_SMDADCR* pADCR,int smd_fiberindex)
{

 /*
 //Bank Header
 for(int i=0;i<10;i++){
   cout<<"Bankheader "<<i<<" "<<pADCR->bankHeader[i]<<endl;
 }
 */

 //fiber header
 for(int i=0;i<64;i++){
  printf("Fiber header**: %x\n",pADCR->fiberHeader[i]);
  //   cout<<"Fiberheader "<<i<<" "<<pADCR->fiberHeader[i]<<endl;
   //   cout<<"fiberheader "<<i<<" "<<pADCR->fiberHeader[i]<<endl;
 }

 //
 // Fiber data and fill the struct

  mTheSmdAdcR.EventNumber=pADCR->header.Token; //Token number
  mTheSmdAdcR.PedFlag=0; //Pedestal subtracted (1) or not (0)
  mTheSmdAdcR.SMDErrorFlag=0; // Error from SMD (0=good)
  mTheSmdAdcR.TimeBin[smd_fiberindex]=0; // To be taken from header

  //SMD_data
 //fiberdata
 for(int i=0;i<4800;i++){
           int index=i;
           int mod=0;
           int RDO=1,strip=0,det=0;
	   int smdphi_strip=0;
	   int smdphi_bin=0;
	   mTheSmdAdcR.SMDADCArray[smd_fiberindex][i]=pADCR->fiberData[index];
           mTheSmdAdcR.BankType="BSMDADCR\n";
           if(pADCR->fiberData[index]>0)mTheSmdAdcR.NSmdHits++;
int binstat=getSmdBin(RDO,index+1,mod,strip,det);
  if(!binstat)
    {
    cout<<" problem in bin conversion "<<index<<endl;
    }
  else
  {
    //SMDE

  if(det==3){
   mTheSmdAdcR.SmdE_ADCMatrix[mod-1][strip-1]=pADCR->fiberData[index];
  }

  mTheSmdAdcR.DetFlag=det; //Detector flag for BSMDE=3,BSMDP=4

  //SMDP

   if(det==4){
	   // get SMD_phi specific co-ordinates
	   int smdphistat = get_smdphistrip(strip,smdphi_bin,smdphi_strip);
   mTheSmdAdcR.SmdP_ADCMatrix[mod-1][smdphi_bin-1][smdphi_strip-1]=pADCR->fiberData[index];
  }
 }
 }

 return 1;
}

 int EMC_SmdReader::ProcessBarrelSmd(const Bank_EMCP* EmcPTR)
   {
  // First Barrel Tower
    Bank_EMCSECP* barrelsmd=getBarrelSection(EmcPTR,0);

  if(barrelsmd){
    for(int smd_fiberindex=0;smd_fiberindex<1;smd_fiberindex++){
   cout<<" taking SMDfiber ***"<<smd_fiberindex<<endl;
    Bank_EMCRBP* smdfiber=getBarrelSmdFiber(barrelsmd,smd_fiberindex);

  if(smdfiber){
   cout<<"Got SMD fiber "<<endl;
      Bank_SMDADCR* smdadc=getSmdADC(smdfiber);

      if(smdadc){
         int fillstat= FillBarrelSmd(smdadc,smd_fiberindex);
       if(fillstat){cout<<" Barrel Filling O.K  "<<endl;}
        else{cout<<"error in filling  "<<endl;}
      }
      else{
	cout<<" ADCR absent , looking for ADCD"<<endl;
      }
      if(smdadc)smdadc=0;
      if(smdfiber)smdfiber=0;
  } 
  else{
    cout<<" BANK_EMRBP absent**"<<endl;
  }//smdfiber
    }

  }//barrelsmd

  else{
    cout<<" BANK_EMCSECP for SMD absent**"<<endl;
    return 0;
  }//barrelsmd

  return 1;
   }
/*
void EMC_SmdReader::PrintTowerArray()
{

  cout<<"BankType **"<<mTheTowerAdcR.BankType<<endl;
  cout<<"Det flag **"<<mTheTowerAdcR.DetFlag<<endl;
  cout<<"Event no**"<<mTheTowerAdcR.EventNumber<<endl;
  cout<<"Pedflag**"<<mTheTowerAdcR.PedFlag<<endl;
  cout<<"TDCERR **"<<mTheTowerAdcR.TDCErrorFlag<<endl;
  for(int i=0;i<120;i++){
    for(int j=0;j<20;j++){
      for(int k=0;k<2;k++){
	if(mTheTowerAdcR.TowerMatrix[i][j][k]>0)cout<<"ADCDATA** mod"<<i+1<<"eta "<<j+1<<"phi "<<k+1<<"ADC "<<mTheTowerAdcR.TowerMatrix[i][j][k]<<endl;
      }
    }
  }
}
*/

Bank_BSMDADCR& EMC_SmdReader::getBSMDADCR()
 {
   return mTheSmdAdcR;
 }



int EMC_SmdReader::getSmdBin(const int RDO,const int daq_smd,int &mod,int &strip,int&det)

{
int category=daq_smd/1600;
int wire=(daq_smd-category*1600)/20;
int A_step=daq_smd%4;
int S_step=daq_smd%20;
int A_value;
int S_value;
 
if(category==0){
A_value=FEE1[A_step];
S_value=connector1[S_step];
}
if(category==1){
A_value=FEE2[A_step];
S_value=connector2[S_step];
}
if(category==2){
A_value=FEE3[A_step];
S_value=connector3[S_step];
}                                                                               
//detector no
int half=0;
 
if(A_value==1){
det=3;
half=2;
}
if(A_value==2){
det=4;
half=1;
}
if(A_value==3){
det=4;
half=2;
}
if(A_value==4){
det=3;
half=1;
}
 
int mod_stat=GetModuleFromConnector(S_value,mod);
 
if(mod_stat){
//Get strip no
  int temp1=wire+1;
  int dummy=checkdummy(temp1);
  if(dummy==0){
    int stat=getsmdfiber(det,half,temp1,strip);
if(mod==60)cout<<daq_smd<<"  "<<wire+1<<"   "<<mod<<"   "<<A_value<<"   "<<det<<"    "<<half<<"    "<<strip<<endl;
  }
//  else{cout<<"dummy fiber**"<<wire<<endl;}
}

 return 0; // 28-aug-2001 by PAI ??
}

/*
// conversion from FEE Tower number to m,e,s for Tower

int EMC_SmdReader::getSmdBin(const int RDO,const int rid,int &mod,int &strip,int&det)
{
  //Transiion from environment Id to m,e,s to fill the arrays
  // Copy of StEmcGeom version

//  if(checkTowerId(rid) == 0) return 0;
int rch=0,half=0; 
int test=get_RDOch(RDO,rid,rch,det,half);
if(test){
  int testmod=getmodule(RDO,rch,mod);
   if(testmod){
    int fiberno =0;
    int test_fiber=get_fiberno(rid,fiberno);
if(test_fiber)
{
  int dummy=checkdummy(fiberno);
  if(dummy==0){
    int stat=getsmdfiber(det,half,fiberno,strip);
  }
  else{cout<<"dummy fiber**"<<fiberno<<endl;}
}
 else{cout<<"testfiber error**"<<endl;}
   }
  else
   {cout<<"error in testmodule**"<<endl;}
 }
else
  {cout<<"error in test**"<<endl;}
  return 1;                   // zero is bad
}


int EMC_SmdReader::get_RDOch(const int RDO,const int rid,int&rch,int&det,int&half)
{

  int temp_ch=(rid%4);
  if(temp_ch==0)temp_ch=4;
  int temp_rdo=(rid-1)/60;
  int temp_bin=rid-60*temp_rdo; 
  int temp_scabin=(temp_bin-1)/20;
  temp_bin-=(temp_scabin*20);
  temp_bin=(temp_bin-1)/4+1;
  if(temp_ch==1)
    {rch=12*(temp_bin-1)+(temp_scabin*4)+2;
    det=3;
    half=2;
    }

  if(temp_ch==2)
    {rch=12*(temp_bin-1)+(temp_scabin*4)+1;
    det=3;
    half=1;
    }

  if(temp_ch==3)
    {rch=12*(temp_bin-1)+(temp_scabin*4)+4;
    det=4;
    half=2;
    }
  if(temp_ch==4)
    {rch=12*(temp_bin-1)+(temp_scabin*4)+3;
    det=4;
    half=1;
    }

  return 1;
}

int EMC_SmdReader::getmodule(const int RDO,int&rch,int&mod)

{
  if(RDO==1){mod=60-(rch-1)/4;}
  return 1;
}

int EMC_SmdReader::get_fiberno(const int rid,int&fiberno)

{
  fiberno=(rid-1)/60+1;
  return 1;
}

int EMC_SmdReader::checkdummy(int&fiberno)
{
  int dummy=0;
  if(fiberno==1 || fiberno==17 || fiberno==33 || fiberno==65){dummy=1;}
  else {dummy=0;}
  return dummy;
  }

int EMC_SmdReader::getsmdfiber(int& det,int& half,int&fiberno,int&strip)

{
  int gap=(fiberno-1)/16;
  if(half==2)
    {
      strip=(fiberno-1)*2 - gap*2-1;
    }
  if(half==1)
    {strip=(fiberno-(gap+1))*2;}

  return 1;
}

int EMC_SmdReader::get_smdphistrip(int& strip,int& smdphi_bin,int&smdphi_strip)
{
 
  smdphi_bin=strip%10;
  if(smdphi_bin==0)smdphi_bin=10; 
  smdphi_strip=16-((strip-1)/10+1);
  return 1; // 0 is bad
}

*/

