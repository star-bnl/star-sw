// tofr.h , Jing Liu, 02/05/2005
#ifndef TOFR_H
#define TOFR_H

#include <iostream>
#include <string>
#include <iomanip>
#include <vector>
using namespace std;

//*********************************************************************
const int LEADING=4;
const int TRAILING=5;
const float bin2timeVHR=25./1024.;
const float bin2timeHR=100./1024.;
const int TOTTRAYCHAN=192;

struct TofHitList {
  int fiberid;              // 0-tray 1-west 2-east 
  int halftrayid;           // 0, 1, 
  int tdigid;               // 0, 1, ..., 7
  int tdcid;                // 0, 1, 2, 3
  int tdcchan;              // 0, 1, 2, ..., 8  for leading edge;
                            // 0, 1, 2, ..., 23 for trailing edge;
  int timeinbin;            // 
  float time;               // time
 
  int moduleid;             // 0, 1, 2, ..., 31
  int modulechan;           // 0, 1, 2, ..., 5

  int globaltdcchan;        // 0, 1, 2, ..., 191, + 6 pVPD channels
  int globalmodulechan;     // 0, 1, 2, ..., 191
};

vector<TofHitList> leadinghits;
vector<TofHitList> trailinghits;

// map of TDIG channels to module channels
//*********************************************************************

int tdcchan2modulechan(int tdcid, int tdcchan,int flag)
{

  if(tdcid<1 || tdcid>4) {cout<<"Error! Not a valid tdc id!"<<endl;return -1;}
  if((tdcid>=1&&tdcid<=3)&& (tdcchan<0 || tdcchan>7)) {
    cout<<"Error! Not a valid tdc channel! tdcid="<<tdcid-1<<" tdcchan="<<tdcchan-1<<endl;
    return -1;
  }
  if(tdcid==4 &&(tdcchan<0||tdcchan>23)){
    cout<<"Error! Not a valid tdc channel! tdcid="<<tdcid-1<<" tdcchan="<<tdcchan-1<<endl;
    return -1;
  }

  int moduleid[24]  = {1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4};
  int modulechan[24]= {1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6};
  int hptdc[24]     = {2,1,2,2,2,1,1,1,1,1,1,1,2,2,3,2,3,2,3,3,3,3,3,3};
  int datachan[24]  = {0,7,2,1,3,6,2,1,4,3,5,0,6,5,0,7,1,4,4,3,6,5,7,2};

  int trailchan[24]= {8,7,10,9,11,6,2,1,4,3,5,0,14,13,16,15,17,12,20,19,22,21,23,18};
  //get index according to tdcid and tdcchan
  int index=0;
  if(tdcid<4){
    for(int i=0;i<24;i++){
      if((tdcid == hptdc[i]) && (tdcchan==datachan[i])) {
        index=i;
        break;
      }
    }
  }
  if(tdcid==4){
    for(int i=0;i<24;i++){
      if((tdcid == 4 ) && (tdcchan==trailchan[i])) {
        index=i;
        break;
      }
    }
  }
  int retmodule=moduleid[index];
  int retmodulechan=modulechan[index];
  if(flag==0) return retmodule;
  if(flag==1) return retmodulechan;
  return -1;
}

#endif

















