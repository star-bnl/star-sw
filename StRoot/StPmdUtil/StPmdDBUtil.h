/*!
 *\class StPmdDBUtil
 *\author
 */
/*********************************************************
 *
 * $Id: StPmdDBUtil.h,v 1.1 2003/11/27 12:32:37 subhasis Exp $
 *
 * Author: Dipak Mishra
 *
 ************************************************************
 *
 * Description: This is the class of PMD geometry for offline 
 *
 *************************************************************
 *
 * $Log: StPmdDBUtil.h,v $
 * Revision 1.1  2003/11/27 12:32:37  subhasis
 * First version
 *
 *************************************************************/
#ifndef STAR_StPmdDBUtil
#define STAR_StPmdDBUtil
#include <stdlib.h>
#include <TMatrix.h>
#include <sstream>
#include <math.h>
#include "StPmdGeom.h"

#define  PMD_BOARD_CH_MAX  64
#define  PMD_BOARD_MAX 1296 
#define  PMD_ROW_MAX 120
#define  PMD_COL_MAX 120
#define  PMD_CHAIN_CHANNEL_MAX 1728
#define  PMD_CHAIN_MAX 48
#define PMD_SECTOR 2
#define PMD_CRAMS_MAX 12
#define PMD_CRAMS_BLOCK 2
#define PMD_CRAMS_CH_MAX 2016

class StPmdGeom;

class StPmdDBUtil {
 private:
  StPmdGeom * mPmdGeom;
 protected:
  Int_t m_BoardNumber[PMD_CRAMS_MAX*2][PMD_ROW_MAX][PMD_COL_MAX];
  Int_t m_ChannelInBoard[PMD_CRAMS_MAX*2][PMD_ROW_MAX][PMD_COL_MAX];
 public: 
  StPmdDBUtil();                 //! A constructor
  virtual  ~StPmdDBUtil();       //! A destructor
  
  void GetBoardInfo();
  void BoardNumber( Int_t,Int_t,Int_t,Int_t& ); 
  void ChannelInBoard( Int_t,  Int_t, Int_t, Int_t& ); 
  
  
  ClassDef(StPmdDBUtil, 1)
    };
    
    inline void StPmdDBUtil::BoardNumber(Int_t sm, Int_t row, Int_t col, Int_t& brd)
{brd=m_BoardNumber[sm][row][col];}

inline void StPmdDBUtil::ChannelInBoard(Int_t sm, Int_t row, Int_t col, Int_t& channel)
{channel=m_ChannelInBoard[sm][row][col];}

#endif













