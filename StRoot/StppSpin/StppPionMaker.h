//
// JB 3/30/01 - removed creation of miniDST
//  modified by JB 2/2/01: trigOnCtb() isolated and upgraded
// 
//*-- Author : George , Jan Balewski 
// $Id: StppPionMaker.h,v 1.1 2001/05/03 23:38:10 balewski Exp $
// $Log: StppPionMaker.h,v $
// Revision 1.1  2001/05/03 23:38:10  balewski
// *** empty log message ***
//
// Revision 1.4  2001/04/12 15:19:09  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:13  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                     
//  Emulates Pionger response for the M-C data
//  May work active (m_mode==1) or passive (m_mode=0=default) 
//                                                                     
//////////////////////////////////////////////////////////////////////////
#ifndef STAR_StppPionMaker
#define STAR_StppPionMaker
   
//////////////////////////////////////////////////////////////////////////
//                                                              
// StppPionMaker : Piongering high Pt events
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif


class StppPionMaker : public StMaker 
{  
 
 private: //....................................................
 protected:  //....................................................
  
 public: //....................................................
   StppPionMaker(const char *name="ppPion");
   virtual       ~StppPionMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual const char *GetCVS() const
     {static const char cvs[]="Tag $Name:  $ $Id: StppPionMaker.h,v 1.1 2001/05/03 23:38:10 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
   
   ClassDef(StppPionMaker, 0)   //StAF chain virtual base class for Makers
};

#endif
















