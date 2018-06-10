/*!
 *\class StPmdDBUtil
 *\author
 */
/*********************************************************
 *
 * $Id: StPmdMapUtil.h,v 1.6 2018/06/08 03:33:09 smirnovd Exp $
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
 * Revision 1.6  2018/06/08 03:33:09  smirnovd
 * Fix typo in header include guards
 *
 * Revision 1.5  2010/05/02 13:14:00  rashmi
 *  removed some couts
 *
 * Revision 1.4  2007/05/26 00:40:07  perev
 * Initialization added
 *
 * Revision 1.3  2007/05/21 04:38:05  rashmi
 * functions for SMChain Combination information 21/05/07
 *
 * Revision 1.2  2005/01/27 13:10:13  subhasis
 * New map for 2005 data
 *
 * Revision 1.1  2004/06/24 13:54:35  subhasis
 *  Maputils added
 *
 *************************************************************/
#ifndef STAR_StPmdMapUtil
#define STAR_StPmdMapUtil
#include <stdlib.h>
#include <TMatrix.h>
#include <sstream>
#include <Stiostream.h>
#include <math.h>
#include "StPmdGeom.h"
#include "StPmdDBUtil.h"

class StPmdGeom;

class StPmdMapUtil {
 private:
  char beg[1];
  StPmdGeom * mPmdGeom;
 protected:
  Int_t m_TempChannelInBoard[PMD_CRAMS_MAX*2][PMD_ROW_MAX][PMD_COL_MAX];
  Int_t m_ChannelInBoard    [PMD_CRAMS_MAX*2][PMD_ROW_MAX][PMD_COL_MAX];
  Int_t m_Chain             [PMD_CRAMS_MAX*2][PMD_ROW_MAX][PMD_COL_MAX];
  Int_t SMChainExists[24][48];
  Int_t nBoardSMChain[24][48];
  Int_t FirstBoard[60];
  Int_t Chain_Combo[60];
  Int_t SM_Combo[60];

  Int_t nSMChain; 
  char end[1];
 public: 
  StPmdMapUtil();                 //! A constructor
  virtual  ~StPmdMapUtil();       //! A destructor
  
  void StoreMapInfo();
  void StoreMapInfo(Int_t );
  void ReverseChannelOriginal( Int_t,  Int_t, Int_t, Int_t& ); 
  void ReverseChannelConverted( Int_t,  Int_t, Int_t, Int_t& ); 
  void ChainNumber( Int_t,  Int_t, Int_t, Int_t& ); 
  
  void SetSMChainCombos(Int_t);
  void ComboSMChain(Int_t,Int_t&,Int_t&);
  Int_t GetComboFirstBoard(Int_t);
  Int_t GetSMChainCombo(Int_t,Int_t);
  Int_t GetnSMChain();
  Int_t GetnBoardSMChain(Int_t);
  Int_t GetnBoardSMChain(Int_t,Int_t);
  
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

inline void StPmdMapUtil::ComboSMChain(Int_t icombo,Int_t& ism, Int_t& ichain){
  ism = SM_Combo[icombo];
  ichain = Chain_Combo[icombo];
}
inline Int_t StPmdMapUtil::GetnBoardSMChain(Int_t ism,Int_t ichain){
  return nBoardSMChain[ism-1][ichain-1];
}
inline Int_t StPmdMapUtil::GetnBoardSMChain(Int_t icombo){
  Int_t ism=0,ichain=0;
  ComboSMChain(icombo,ism,ichain);
  //  cout<<"ism,ichain="<<ism<<","<<ichain<<endl;
  return nBoardSMChain[ism-1][ichain-1];
}

inline Int_t StPmdMapUtil::GetnSMChain(){
  return nSMChain;
}

inline Int_t StPmdMapUtil::GetComboFirstBoard(Int_t icombo){
  return FirstBoard[icombo];
}

#endif

