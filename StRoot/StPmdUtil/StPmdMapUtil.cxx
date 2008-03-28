/********************************************************
 *
 * $Id: StPmdMapUtil.cxx,v 1.4 2007/05/05 13:41:08 rashmi Exp $
 *
 * Author: Subhasis Chattopadhyay
 *
 *********************************************************
 *
 * Description: This the class of PMD Utillty which deals with maps of channel to sm,row,col.
 * It can be used for reverse mapping as well.
 *
 *********************************************************
 * $Log: StPmdMapUtil.cxx,v $
 * Revision 1.4  2007/05/05 13:41:08  rashmi
 * bug fix for unphysical year in StoreMapInfo()
 *
 * Revision 1.3  2005/12/07 20:35:45  perev
 * iostream.h ==> iostream. WarnOff
 *
 * Revision 1.2  2005/01/27 13:10:08  subhasis
 * New map for 2005 data
 *
 * Revision 1.1  2004/06/24 13:54:39  subhasis
 *  Maputils added
 *
 *
 **********************************************************/
#include <iostream> 
#include <strings.h>
#include <stdlib.h>
#include <TROOT.h>
#include<TMatrix.h>
#include<TArrayF.h>
#include <math.h>
#include "StPmdGeom.h"
#include "StPmdMapUtil.h"
#include "StMessMgr.h"
ClassImp(StPmdMapUtil)

  StPmdMapUtil::StPmdMapUtil()         //! A constructor
{
  mPmdGeom = new StPmdGeom();
  StoreMapInfo();
}

StPmdMapUtil::~StPmdMapUtil(){/*none*/}  //! A destructor

void StPmdMapUtil::StoreMapInfo(Int_t runno1)
{
  char runfile[20];
  sprintf(runfile,"%d",runno1);
// Fetch from the run # the day
  char iRun[8];
  char iyear[8];
  for (Int_t ik=0; ik<3; ik++)
    {
      iRun[ik] = runfile[ik+1];
    }
  iyear[0] = runfile[0];

  Int_t rn =0;
  Int_t year =0;
  rn=atoi(iRun);
  year=atoi(iyear);
  cout<<"runno, rn1 "<<runno1<<" "<<rn<<" "<<year<<endl;
  if(year<4){
    gMessMgr->Warning("Unphysical year");
    return;
  }
//////////////////////////////////////////////
  Int_t sm=0,row=0,col=0;
  //Initialize
  for(Int_t ism=0;ism<PMD_CRAMS_MAX*2;ism++){
    for(Int_t irow=0;irow<PMD_ROW_MAX;irow++){
      for(Int_t icol=0;icol<PMD_COL_MAX;icol++){
	m_ChannelInBoard[ism][irow][icol]=0;
	m_TempChannelInBoard[ism][irow][icol]=0;
	m_Chain[ism][irow][icol]=0;
      }
    }
  }
  
  for(Int_t Chain_No=1;Chain_No<=PMD_CHAIN_MAX;Chain_No++){
    for(Int_t CHANNEL=0;CHANNEL<PMD_CHAIN_CHANNEL_MAX;CHANNEL++){
      Int_t channel=CHANNEL;
      Int_t chtemp;

	if(year==4)mPmdGeom->ChainMapping(Chain_No,channel,sm,col,row,chtemp);
	if(year>4){
	   mPmdGeom->readBoardDetail(runno1);
	   mPmdGeom->ChainMapping(Chain_No,channel,sm,col,row,chtemp,year);
	}
	if(year>=4 && sm>0 && row>0 && col>0){
 //  if(sm>12)cout<<"chain,ch,sm,col,row,chtemp,year "<<Chain_No<<" "<<channel<<" "<<sm-1<<" "<<col-1<<" "<<row-1<<" "<<chtemp<<" "<<year<<endl;
	 m_ChannelInBoard[sm-1][row-1][col-1]=channel;
	 m_TempChannelInBoard[sm-1][row-1][col-1]=chtemp;
	 m_Chain[sm-1][row-1][col-1]=Chain_No;
	}
    }
  }
}

void StPmdMapUtil::StoreMapInfo()
{
  Int_t sm,row,col;
  //Initialize
  for(Int_t ism=0;ism<PMD_CRAMS_MAX*2;ism++){
    for(Int_t irow=0;irow<PMD_ROW_MAX;irow++){
      for(Int_t icol=0;icol<PMD_COL_MAX;icol++){
	m_ChannelInBoard[ism][irow][icol]=0;
	m_TempChannelInBoard[ism][irow][icol]=0;
	m_Chain[ism][irow][icol]=0;
      }
    }
  }
  
  for(Int_t Chain_No=1;Chain_No<=PMD_CHAIN_MAX;Chain_No++){
    for(Int_t CHANNEL=0;CHANNEL<PMD_CHAIN_CHANNEL_MAX;CHANNEL++){
      Int_t channel=CHANNEL;
      Int_t chtemp;

	mPmdGeom->ChainMapping(Chain_No,channel,sm,col,row,chtemp);
	m_ChannelInBoard[sm-1][row-1][col-1]=channel;
	m_TempChannelInBoard[sm-1][row-1][col-1]=chtemp;
	m_Chain[sm-1][row-1][col-1]=Chain_No;

    }
  }
}
