/********************************************************
 *
 * $Id: StPmdMapUtil.cxx,v 1.1 2004/06/24 13:54:39 subhasis Exp $
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
 * Revision 1.1  2004/06/24 13:54:39  subhasis
 * Maputils added
 *
 *
 **********************************************************/
#include <iostream.h> 
#include <strings.h>
#include <stdlib.h>
#include <TROOT.h>
#include<TMatrix.h>
#include<TArrayF.h>
#include <math.h>
#include "StPmdGeom.h"
#include "StPmdMapUtil.h"
ClassImp(StPmdMapUtil)

  StPmdMapUtil::StPmdMapUtil()         //! A constructor
{
  mPmdGeom = new StPmdGeom();
  StoreMapInfo();
}

StPmdMapUtil::~StPmdMapUtil(){/*none*/}  //! A destructor


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
