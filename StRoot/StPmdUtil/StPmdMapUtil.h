/*!
 *\class StPmdDBUtil
 *\author
 */
/*********************************************************
 *
 * $Id: StPmdMapUtil.h,v 1.2 2005/01/27 13:10:13 subhasis Exp $
 *
 * Author: Dipak Mishra
 *
 ************************************************************
 *
 * Description: This is the class of PMD geometry for offline 
 *
 *************************************************************
 *
 * $Log: StPmdMapUtil.h,v $
 * Revision 1.2  2005/01/27 13:10:13  subhasis
 * New map for 2005 data
 *
 * Revision 1.1  2004/06/24 13:54:35  subhasis
 *  Maputils added
 *
 *************************************************************/
#ifndef STAR_StPmdMapUtil
#define STAR_StPmdmapUtil
#include <stdlib.h>
#include <TMatrix.h>
#include <sstream>
#include <math.h>
#include "StPmdGeom.h"
#include "StPmdDBUtil.h"

class StPmdGeom;

class StPmdMapUtil {
 private:
  StPmdGeom * mPmdGeom;
 protected:
  Int_t m_TempChannelInBoard[PMD_CRAMS_MAX*2][PMD_ROW_MAX][PMD_COL_MAX];
  Int_t m_ChannelInBoard[PMD_CRAMS_MAX*2][PMD_ROW_MAX][PMD_COL_MAX];
  Int_t m_Chain[PMD_CRAMS_MAX*2][PMD_ROW_MAX][PMD_COL_MAX];
 public: 
  StPmdMapUtil();                 //! A constructor
  virtual  ~StPmdMapUtil();       //! A destructor
  
  void StoreMapInfo();
  void StoreMapInfo(Int_t );
  void ReverseChannelOriginal( Int_t,  Int_t, Int_t, Int_t& ); 
  void ReverseChannelConverted( Int_t,  Int_t, Int_t, Int_t& ); 
  void ChainNumber( Int_t,  Int_t, Int_t, Int_t& ); 
  ClassDef(StPmdMapUtil, 1)
    };
    
inline void StPmdMapUtil::ReverseChannelConverted(Int_t sm, Int_t row, Int_t col, Int_t& channel)
{
channel=m_TempChannelInBoard[sm-1][row-1][col-1];
//cout<<"channelC, sm,row,col "<<channel<<" "<<sm<<" "<<row<<" "<<col<<endl;
}

inline void StPmdMapUtil::ReverseChannelOriginal(Int_t sm, Int_t row, Int_t col, Int_t& channel)
{
channel=m_ChannelInBoard[sm-1][row-1][col-1];
//cout<<"channelR, sm,row,col "<<channel<<" "<<sm<<" "<<row<<" "<<col<<endl;
}

inline void StPmdMapUtil::ChainNumber(Int_t sm, Int_t row, Int_t col, Int_t& chain)
{
chain=m_Chain[sm-1][row-1][col-1];
//cout<<"chain, sm,row,col "<<chain<<" "<<sm<<" "<<row<<" "<<col<<endl;
}

#endif













