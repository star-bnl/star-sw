/********************************************************
 *
 * $Id: StPmdDBUtil.cxx,v 1.6 2004/06/28 20:49:26 perev Exp $
 *
 * Author: Subhasis Chattopadhyay
 *
 *********************************************************
 *
 * Description: This the class of PMD Utillty which contains
 * FEE boardwise information.
 *
 *********************************************************
 * $Log: StPmdDBUtil.cxx,v $
 * Revision 1.6  2004/06/28 20:49:26  perev
 * obsolete stream replaced
 *
 * Revision 1.5  2004/03/23 08:50:23  subhasis
 * biardDetail put by had
 *
 *
 **********************************************************/
#include "Stiostream.h"
#include <strings.h>
#include <stdlib.h>
#include <TROOT.h>
#include<TMatrix.h>
#include<TArrayF.h>
#include <math.h>
#include "StPmdGeom.h"
#include "StPmdDBUtil.h"
ClassImp(StPmdDBUtil)

  StPmdDBUtil::StPmdDBUtil()         //! A constructor
{
  mPmdGeom = new StPmdGeom();
  GetBoardInfo();
}

StPmdDBUtil::~StPmdDBUtil(){/*none*/}  //! A destructor


void StPmdDBUtil::GetBoardInfo()
{
  Int_t sm,row,col;
  for(Int_t ism=0;ism<PMD_CRAMS_MAX*2;ism++){
    for(Int_t irow=0;irow<PMD_ROW_MAX;irow++){
      for(Int_t icol=0;icol<PMD_COL_MAX;icol++){
	m_BoardNumber[ism][irow][icol]=0;
	m_ChannelInBoard[ism][irow][icol]=0;
      }
    }
  }
  
  for(Int_t Chain_No=1;Chain_No<=PMD_CHAIN_MAX;Chain_No++){
    for(Int_t CHANNEL=0;CHANNEL<PMD_CHAIN_CHANNEL_MAX;CHANNEL++){
      //Int_t Chain_No=(CRAM+1)+(SEC*12)+(BLOCK*24);
  //??    Int_t channel=CHANNEL+1;
      Int_t channel=CHANNEL;
      Int_t chtemp;

	mPmdGeom->ChainMapping(Chain_No,channel,sm,col,row,chtemp);
	Int_t Board_no=(Int_t)channel/PMD_BOARD_CH_MAX;
//	cout<<"#2: Chain,ch,sm,col,row,brd "<<Chain_No<<" "<<channel<<" "<<sm<<" "<<col<<" "<<row<<" "<<Board_no<<endl;
	m_BoardNumber[sm-1][row-1][col-1]=Board_no;
	m_ChannelInBoard[sm-1][row-1][col-1]=channel;
	m_Chain[sm-1][row-1][col-1]=Chain_No;
	// if(Chain_No>=45) cout<<"m_ChannelInBoard = "<<m_ChannelInBoard[sm-1][row-1][col-1]<<endl;
    }
  }
}

