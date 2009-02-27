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
const int NEASTCHAN=19;
const int NWESTCHAN=19;
const int TOTPVPDCHAN=38;
struct TofHitList {
  //Jing Liu add trayid, 12/10/2007
  int trayid;

  int fiberid;              // 0,1,2,...
  int halftrayid;           // 0, 1, 
  int tdigboardid;               // 0, 1, ..., 7
  int hptdcid;                // 0, 1, 2
  int tdcchan;              // 0, 1, 2, ..., 7 

  int edgeid;
  int timeinbin;            // 
  float time;               // time 
 
  int moduleid;             // 0, 1, 2, ..., 31
  int modulechan;           // 0, 1, 2, ..., 5

  int globaltdcchan;        // 0, 1, 2, ..., 191
  int globalmodulechan;     // 0, 1, 2, ..., 191

  double numberforsort;
};

bool compareIt(TofHitList const &x, TofHitList const &y)
{
 return x.numberforsort < y.numberforsort;
}

vector<TofHitList> leadinghits;
vector<TofHitList> trailinghits;


// map of TDIG channels to module channels
//*********************************************************************
// Jing Liu, 12/10/2007 for Run8
//
int tdcchan2mrpcchan(int globaltdcchan)
{
  if(globaltdcchan<0 || globaltdcchan>191) {cout<<"Wrong global tdc chan: "<<globaltdcchan<<endl; return -1;}

  int tdcidmap[4][6] = { {0,1,0,1,0,1}, {2,0,2,0,1,0}, {1,2,0,2,0,2}, {2,1,2,1,2,1}};
  int tdcchanmap[4][6]={ {7,7,0,2,5,6}, {7,4,4,2,3,6}, {0,2,3,3,1,6}, {0,5,1,4,5,1}};

  int theglobalmodulechan[192];
  int theglobaltdcchan[192];

  for(int isec=0;isec<8;isec++){
    for(int imodule=0;imodule<4;imodule++){
      for(int ipad=0;ipad<6;ipad++){
        int globalmodule  =  isec*24 + imodule*6 + ipad;
        int globaltdc     =  isec*24 + tdcidmap[imodule][ipad]*8+tdcchanmap[imodule][ipad];
        theglobalmodulechan[globalmodule]=globalmodule;
        theglobaltdcchan[globalmodule]=globaltdc;
        //cout<<"global module chan="<<globalmodule<<" global tdc chan="<<globaltdc<<endl;
     }
    }
  }
  int returnthis=0;

  //int thistdcchan=tdig*24+(tdcid%4)*8+tdcchan;
  int thistdcchan=globaltdcchan;
  for(int i=0;i<192;i++){
    if(thistdcchan == theglobaltdcchan[i]) {returnthis = i;break;}
  }
  return returnthis;
}
 
int tdcchan2upvpdPMTchan(int globaltdcchan, int edgeid,int trayid)
{

  if(globaltdcchan<0 || globaltdcchan>191) {cout<<"Wrong global tdc chan: "<<globaltdcchan<<endl; return -1;}

//                      1   2   3  4  5  6  7  8  9  10  11 12  13  14  15 16 17 18 19
  int upvpdLEchan[54]={142,122,118,98,46,26,22,2,112,101,24,136,123,120,99,40,27,16,3,  //west
                       142,122,118,98,46,26,22,2,112,101,24,136,123,120,99,40,27,16,3,  //east
		       48,64,50,70,0,29,5,96,   48,64,50,70,0,29,5,96};                //pp2pp 
  int upvpdTEchan[54]={129,131,105,107,33,35,9,11,109,110,39,133,132,135,108,37,36,13,12,  //west
                       129,131,105,107,33,35,9,11,109,110,39,133,132,135,108,37,36,13,12,  //east
		       63,61,59,57,15,38,14,111,    63,61,59,57,15,38,14,111};             //pp2pp 
  int inputglobalchan=globaltdcchan;
  int pmtchan=-1;
  int pmtLEchan =-1;
  int startpoint=-1;
  if(trayid==121) startpoint=0;   // west
  if(trayid==122) startpoint=19;  // east

  for(int i=startpoint;i<startpoint+19;i++){
    if(upvpdLEchan[i]==inputglobalchan) {pmtLEchan=i;break;}
  }
  for(int i=38;i<46;i++){
    if(upvpdLEchan[i]==inputglobalchan) {pmtLEchan=i;if(trayid==122)pmtLEchan=pmtLEchan+8;break;}
  }
  //
  int pmtTEchan=-1;
  for(int i=startpoint;i<startpoint+19;i++){
    if(upvpdTEchan[i]==inputglobalchan) {pmtTEchan=i;break;}
  }
  for(int i=38;i<46;i++){
    if(upvpdTEchan[i]==inputglobalchan) {pmtTEchan=i;if(trayid==122)pmtTEchan=pmtTEchan+8;break;}
  }

  if(edgeid==4) pmtchan = pmtLEchan;
  if(edgeid==5) pmtchan = pmtTEchan;

  //cout<<" inside map:: trayid="<<trayid<<" globaltdcchan="<<globaltdcchan<<" edgeid="<<edgeid<<" return="<<pmtchan<<endl;

  return pmtchan;
}
#endif


/***************************************************************************
 *
 * $Id: tofr.h,v 1.2 2009/02/27 22:30:17 dkettler Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: tofr.h,v $
 * Revision 1.2  2009/02/27 22:30:17  dkettler
 * TOF Updates
 *
 * Revision 1.1  2009/01/23 16:11:00  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.3  2008/02/15 18:51:56  dkettler
 * Updates to laser and TOF reader
 *
 * Revision 1.1  2007/02/27 15:23:42  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:34  laue
 * Initial Version
 *
 *
 ***************************************************************************/

