/*!\class StEmcDecoder
/author Alexandre A. P. Suaide

This class makes the decodification from EMC daq
and electronics scheme to software scheme. This class
has methods to decode the numbers in both directions and
it works for SMD and towers.
*/ 
#include "StEmcDecoder.h"
#include <time.h>
#include <stdlib.h>
#include <iostream.h>
#include <stdio.h>

//--------------------------------------------------------
/// StEmcDecoder constructor - date and time should be in GMT
/// to get the correct electronics configuration for towers and SMD
StEmcDecoder::StEmcDecoder(unsigned int date,unsigned int time)
{
  cout <<"TIME USED FOR DECODER = "<<date<<" "<<time<<endl;
  Init(date,time);
  // reverse order for tower
  for (int i=0;i<30;i++) Crate_TDC[TDC_Crate[i]-1]=i;
  for (int RDO=0;RDO<4800;RDO++)
  {
    int id=0;
    if(GetTowerIdFromDaqId(RDO,id)==1) ReverseOrder[id-1]=RDO;
  }
  
  // reverse order for SMD
  int det,m,e,s;
  for(int RDO=0;RDO<8;RDO++)
    for(int index=0;index<4800;index++)
    {
      int status=GetSmdCoord(RDO,index,det,m,e,s);
      if(status==1)
      {
        if(det==3)
        {
          SmdeRDO[m-1][e-1]=RDO;
          SmdeIndex[m-1][e-1]=index;
        }
        if(det==4)
        {
          SmdpRDO[m-1][e-1][s-1]=RDO;
          SmdpIndex[m-1][e-1][s-1]=index;          
        }
      }
    }
  
}
//--------------------------------------------------------
/// StEmcDecoder destructor
StEmcDecoder::~StEmcDecoder()
{
}
//--------------------------------------------------------
/// Init method - should be called from constructor
/// this method initializes the variables used in decoder
void StEmcDecoder::Init(unsigned int date,unsigned int time)
{
  
  ////////////////////////////////////////////////////////////////////////
  // these vectors are for tower decoding ////////////////////////////////

  // initial position in the crate
  int Init_Crate_tmp[]={2260,2420,2580,2740,2900,3060,3220,3380,3540,3700,
                        3860,4020,4180,4340,4500,2180,2020,1860,1700,1540,
                        1380,1220,1060,900,740,580,420,260,100,2340};
  Init_Crate=Init_Crate_tmp;
  

  // which crate is connected to each TDC channel. See log book for details
  if(date <= 20011223)
  {
    int TDC_Crate_tmp[]= {18,17,16,30,29,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                          19,20,21,22,23,24,25,26,27};
    TDC_Crate=TDC_Crate_tmp;
    goto SMD;
  }
  if(date == 20011124 && time <=163000)
  {
    int TDC_Crate_tmp[]= {18,17,16,30,29,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                          19,20,21,22,23,24,25,26,27};
    TDC_Crate=TDC_Crate_tmp;  
    goto SMD;
  }
  if(date == 20011124 && time >163000)
  {
    int TDC_Crate_tmp[]= {18,17,16,29,30,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                          19,20,21,22,23,24,25,26,27};
    TDC_Crate=TDC_Crate_tmp;      
    goto SMD;
  }
  if(date == 20011125 && time <=073000)
  {
    int TDC_Crate_tmp[]= {18,17,16,29,30,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                          19,20,21,22,23,24,25,26,27};
    TDC_Crate=TDC_Crate_tmp;      
    goto SMD;
  }
  if(date == 20011125 && time >073000)
  {
    int TDC_Crate_tmp[]= {18,17,16,30,29,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                          19,20,21,22,23,24,25,26,27};
    TDC_Crate=TDC_Crate_tmp;      
    goto SMD;
  }
  if(date >= 20011126)
  {
    int TDC_Crate_tmp[]= {18,17,16,30,29,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                          19,20,21,22,23,24,25,26,27};
    TDC_Crate=TDC_Crate_tmp;      
    goto SMD;
  }
   

  ///////////////////////////////////////////////////////////////////////
  // these tables are for SMD decoding //////////////////////////////////

  // which module is connected to each RDO and crate board
  SMD:
  if(date <= 20011201)  // SMD modules connection before this date
  {
    int SmdModules_tmp[8][15]={
                              {46,47,48,49,50,51,52,53,54,55,56,57,58,59,60},                //RDO 0
                              {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15},                         //RDO 1
                              {16,17,18,19,20,21,22,23,24,25,26,27,28,29,30},                //RDO 2
                              {31,32,33,34,35,36,37,38,39,40,41,42,43,44,45},                //RDO 3
                              {61,62,63,64,65,66,67,68,69,70,71,72,73,74,75},                //RDO 4
                              {76,77,78,79,80,81,82,83,84,85,86,87,88,89,90},                //RDO 5
                              {91,92,93,94,95,96,97,98,99,100,101,102,103,104,105},          //RDO 6
                              {106,107,108,109,110,111,112,113,114,115,116,117,118,119,120}  //RDO 7
                              }; 
    SmdModules=SmdModules_tmp;
    goto FEE;
  }
  
  if(date >= 20011202) // SMD modules connection after this date
  {
    int SmdModules_tmp[8][15]={
                              {49,54,48,46,50,51,52,53,47,55,56,57,58,59,60},                //RDO 0
                              {11,2,10,12,5,6,7,8,13,3,1,14,15,4,9},                         //RDO 1
                              {16,17,18,19,20,21,22,23,24,25,26,27,28,29,30},                //RDO 2
                              {31,32,33,34,35,36,37,38,39,40,41,42,43,44,45},                //RDO 3
                              {61,62,63,64,65,66,67,68,69,70,71,72,73,74,75},                //RDO 4
                              {76,77,78,79,80,81,82,83,84,85,86,87,88,89,90},                //RDO 5
                              {91,92,93,94,95,96,97,98,99,100,101,102,103,104,105},          //RDO 6
                              {106,107,108,109,110,111,112,113,114,115,116,117,118,119,120}  //RDO 7
                              }; 
    SmdModules=SmdModules_tmp;
    goto FEE;
  }

  FEE:
  int FEE1_tmp[4]={1,4,3,2};
  int FEE2_tmp[4]={2,1,4,3};
  int FEE3_tmp[4]={3,2,1,4};
  FEE1=FEE1_tmp;
  FEE2=FEE2_tmp;
  FEE3=FEE3_tmp;

  int connector1_tmp[20]={1,1,2,3,4,4,5,6,7,7,8,9,10,10,11,12,13,13,14,15};
  int connector2_tmp[20]={1,2,2,3,4,5,5,6,7,8,8,9,10,11,11,12,13,14,14,15};
  int connector3_tmp[20]={1,2,3,3,4,5,6,6,7,8,9,9,10,11,12,12,13,14,15,15};
  connector1=connector1_tmp;
  connector2=connector2_tmp;
  connector3=connector3_tmp;
  
  ///////////////////////////////////////////////////////////////////////
  return;
}
//--------------------------------------------------------
///Transiion from environment rid to m,e,s for towers
///Copy of StEmcGeom version
int StEmcDecoder::GetTowerBin(const int rid,int &m,int &e,int &s)
{
  //Transiion from environment Id to m,e,s to fill the arrays
  // Copy of StEmcGeom version

  if(rid<1 || rid>4800) return 0;
 
  int idw;
  m   = (rid - 1) / 40 + 1; // Module number
  idw = (rid - 1) % 40;     // change from 0 to 39
  s   = idw/20 + 1;
  e   = idw%20+1;
//  e   = 20 - idw%20;
  return 1;                   // zero is bad
}
//--------------------------------------------------------
///Get crate number from TDC channel for towers
int StEmcDecoder::GetTowerCrateFromTDC(int TDC, int& crate)
{
  if(TDC>=0 && TDC<30) 
  {
    crate = TDC_Crate[TDC];
    return 1;
  }
  return 0;
}
//--------------------------------------------------------
/// get TDC channel from crate number for towers
int StEmcDecoder::GetTowerTDCFromCrate(int crate, int& TDC)
{
  if(crate>0 && crate<31) 
  {
    TDC = Crate_TDC[crate-1];
    return 1;
  }
  return 0;
}
//--------------------------------------------------------
/// get TDC channel from Daq Id for towers
int StEmcDecoder::GetTowerTDCFromDaqId(int daq_tower, int& TDC)
{
  if(daq_tower>=0 && daq_tower<=4799)
  {
    TDC=daq_tower%30;
    return 1;
  }
  return 0;
}
//--------------------------------------------------------
/// Get crate number from Daq Id for towers
int StEmcDecoder::GetTowerCrateFromDaqId(int daq_tower,
                                                int& crate,
                                                int& crate_sequency)
{
  int tdc=daq_tower%30;
  crate_sequency=daq_tower/30;
  if(GetTowerCrateFromTDC(tdc, crate)==1) return 1; // 1 is ok
  return 0; 
}
//--------------------------------------------------------
///Get Software Id for West size for towers
int StEmcDecoder::Getjose_towerWest(int start,int crate_seq)
{
  int card=crate_seq/32;
  int card_seq=31-(crate_seq%32);
  int channel_seq=card_seq/4;
  int channel=card_seq-(channel_seq*4)+1;
  int jose_tower=start+channel_seq*20+card*4+(5-channel);
  if(jose_tower>2400)jose_tower-=2400;
  return jose_tower;
}
//--------------------------------------------------------
///Get Software Id for East side for towers
int StEmcDecoder::Getjose_towerEast(int start,int crate_seq)
{
  int card=crate_seq/32;
  int card_seq=31-(crate_seq%32);
  int channel_seq=card_seq/4;
  int channel=card_seq-(channel_seq*4)+1;
  int jose_tower=start+channel_seq*20+card*4+(5-channel);
  if(jose_tower<2400)jose_tower+=2400;
  return jose_tower;
}
//--------------------------------------------------------
///Get Sofwtare Id from Daq Id for towers
int StEmcDecoder::GetTowerIdFromDaqId(int RDO,int& id)
{
  //RDO from 0 to 4799
  if(RDO<0 || RDO>4799) return 0;  //0 is bad
  
  int crate_seq=0;
  int Crate;
  int tdc;
  GetTowerCrateFromDaqId(RDO,Crate,crate_seq);
  GetTowerTDCFromDaqId(RDO,tdc);
  int start=Init_Crate[Crate-1];
  if(Crate>15 && Crate<31)
  {
    id=Getjose_towerWest(start,crate_seq);
    return 1;
  }
  if(Crate>0 && Crate<16)
  {
    id=Getjose_towerEast(start,crate_seq);
    return 1;
  }
  return 0;
}
//--------------------------------------------------------
/// Get Daq Id from Software Id for towers
int StEmcDecoder::GetDaqIdFromTowerId(int id,int& RDO)
{
  if(id<1 || id >4800) return 0; // 0 is bad
  
  RDO = ReverseOrder[id-1];
  
  return 1;
}
//--------------------------------------------------------
///Get Software Id from Crate number and position in crate for towers
int StEmcDecoder::GetTowerIdFromCrate(int Crate,int crate_sequency,
                                             int& id)
{
  if(Crate>15 && Crate<31)
  {
    int start=Init_Crate[Crate-1];
    id=Getjose_towerWest(start,crate_sequency);
    return 1;
  }
  if(Crate>0 && Crate<16)
  {
    int start=Init_Crate[Crate-1];
    id=Getjose_towerEast(start,crate_sequency);
    return 1;
  }
  return 0;
}
//--------------------------------------------------------
///Get SMD fiber and position from detector number (3==SMDE, 4==SMDP), m, e, s
int StEmcDecoder::GetSmdRDO(int det,int module, int eta, int sub, int& RDO, int& index)
{
  if(module<1 || module >120) return 0;
  if(det==3)
  {
    if(eta<1 || eta>150) return 0;
    RDO=SmdeRDO[module-1][eta-1];
    index=SmdeIndex[module-1][eta-1];
    return 1;
  }
  if(det==4)
  {
    if(eta<1 || eta>10) return 0;
    if(sub<1 || sub>15) return 0;
    RDO=SmdpRDO[module-1][eta-1][sub-1];
    index=SmdpIndex[module-1][eta-1][sub-1];
  }
  return 0;
}
//--------------------------------------------------------
///Get SMD detector (3==SMDE, 4==SMDP), m, e, s from RDO and position for SMD
int StEmcDecoder::GetSmdCoord(int RDO,int index,
                                     int& detector,
                                     int& module,int& eta,int& sub)
{
  if(RDO<0 || RDO>7) return 0;
  if(index <0 || index >4799) return 0; 
  
  int daq_smd=index;
  
  int category=daq_smd/1600;
  int wire=(daq_smd-category*1600)/20;
  int A_step=daq_smd%4;
  int S_step=daq_smd%20;
  int A_value=0;
  int S_value=0;
  
  if(category==0)
  {
    A_value=FEE1[A_step];
    S_value=connector1[S_step];
    
  }

  if(category==1)
  {
    A_value=FEE2[A_step];
    S_value=connector2[S_step];
  }

  if(category==2)
  {
    A_value=FEE3[A_step];
    S_value=connector3[S_step];
  }
  
  //detector no
  int det=0;
  int half=0;

  if(A_value==1)
  {
    det=3;
    half=2;
  }
  if(A_value==2)
  {
    det=4;
    half=1;
  }
  if(A_value==3)
  {
    det=4;
    half=2;
  }
  if(A_value==4)
  {
    det=3;
    half=1;
  }
  
  int mod=0;
  int mod_stat=getSmdModule(RDO, S_value, mod);
  int pin=0;
  if(mod_stat)
  {
    //Get strip no
    int dummy=checkDummy(wire+1);
    if(dummy==0)
    {
      int stat=getSmdPin(det,half,wire+1,pin);

      /*cout <<"RDO = "<<RDO
           <<"  idx = "<<index
           <<"  AS = "<<A_step
           <<"  SS = "<<S_step
           <<"  A = "<<A_value
           <<"  S = "<<S_value
           <<"  cat = "<<category
           <<"  wire = "<<wire
           <<"  det = "<<det
           <<"  half = "<<half
           <<"  pin = "<<pin;*/
               
      detector = det;
      module = mod;
      if(detector == 3) // bsmde
      {
        eta = 151-pin;
        sub = 1;
        //cout <<" m = "<<module<<"  e = "<<eta <<"  s = "<<sub;
        return 1;
      }
      if(detector == 4) // bsmdp
      {
        int stat=getSmdpStrip(pin,eta,sub);
        if(stat!=1) return 0;
        //cout <<" m = "<<module<<"  e = "<<eta <<"  s = "<<sub;
        return 1;
      }
    }
  }
  return 0;
}
//--------------------------------------------------
///Get SMD module in a given position in RDO
int StEmcDecoder::getSmdModule(int RDO, int S_value,int& mod)
 
{
  mod=SmdModules[RDO][S_value-1];
  return 1;
} 
//--------------------------------------------------
///Check dummy positions on SMD crate
int StEmcDecoder::checkDummy(int fiberno)
{
  int dummy=0;
  if(fiberno==1 || fiberno==17 || fiberno==33 || fiberno==49 || fiberno==65) {dummy=1;}
  else {dummy=0;}
  return dummy;
}
//--------------------------------------------------
///Get SMD pin number
int StEmcDecoder::getSmdPin(int det,int half,int fiberno,int& pin)
 
{
  int gap=(fiberno-1)/16;
  if(half==2)
  {
    pin=(fiberno-1)*2 - gap*2-1;
  }
  if(half==1)
  {
    pin=(fiberno-(gap+1))*2;
  }
  return 1;
}
//--------------------------------------------------
///Get SMDP strip
int StEmcDecoder::getSmdpStrip(int pin,int& etabin,int& phibin)
{
  etabin=(pin-1)%10+1;
  phibin=15-(pin-1)/10;
  return 1; // 0 is bad
}       
//--------------------------------------------------
///Print Tower MAP
void StEmcDecoder::PrintTowerMap(ofstream *out)
{
  *out <<"TDC channels connections\n";
  *out <<"-----------------------------------------------------------\n";
  for(int i=0;i<30;i++) 
    *out <<"  TDC channel "<<i<<" connected to crate "<<TDC_Crate[i]<<endl;
  *out <<endl;
  *out <<"Tower MAP\n";
  *out <<"-----------------------------------------------------------\n";
  for(int daq =0;daq<4800;daq++)
  {
    int towerId,tdc,crate,position;
    GetTowerIdFromDaqId(daq,towerId);
    GetTowerTDCFromDaqId(daq,tdc);
    GetTowerCrateFromDaqId(daq,crate,position);
    *out <<"  daq id = "<<daq<<"  TDC channel = "<<tdc
         <<"  Crate = "<<crate<<"  position in crate = "<<position
         <<"  software id = "<<towerId<<endl;
  }
  *out <<endl;
}
//--------------------------------------------------
///Print SMD MAP
void StEmcDecoder::PrintSmdMap(ofstream *out)
{
  *out <<"Modules connected to SMD crate\n";
  *out <<"-----------------------------------------------------------\n";
  
  for(int i=0;i<8;i++) 
  {
    *out <<"SMD CRATE number "<<i+1<<endl;
    for(int j=0;j<15;j++)
      *out <<"  channel "<<j<<" is connected to module "<<SmdModules[i][j]<<endl;
  }
  *out <<"\nSMD MAP\n";
  *out <<"-----------------------------------------------------------\n";
  
  for(int i=0;i<8;i++) 
  {
    *out <<"SMD CRATE number "<<i+1<<endl;
    for(int index =0;index<4800;index++)
    {
      int det,m,e,s;
      int status = GetSmdCoord(i,index,det,m,e,s);
      *out <<"  RDO = "<<i<<"  index = "<<index;
      if(status == 1)
        *out <<" detector = "<<det<<"  mod = "<<m<<"  eta = "<<e<<"  sub = "<<s<<endl;
      else
        *out <<"  dummy connection\n";
    }
  }
  *out <<endl;
}






