// $Id: StSsdDbMaker.cxx,v 1.10 2007/02/18 13:32:35 bouchet Exp $
//
// $Log: StSsdDbMaker.cxx,v $
// Revision 1.10  2007/02/18 13:32:35  bouchet
// Changes for the new Logger
//
// Revision 1.9  2006/10/16 19:53:24  fisyak
// Adjust for new Ssd chain
//
// Revision 1.8  2006/09/18 16:40:14  fisyak
// Add sim flag for ssdWafersPosition
//
// Revision 1.7  2005/06/03 21:30:41  perev
// Move configuration Init()==>InitRun()
//
// Revision 1.6  2005/05/10 12:48:06  reinnart
// The new StSsdDbMaker without DirectDataBase Access
// 

/***************************************************************************
 * Author: christelle roy
 * Description: SSD DB access Maker
 **************************************************************************/

#include "StSsdDbMaker.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_ssdConfiguration_Table.h"
#include "tables/St_ssdDimensions_Table.h"
StSsdDbMaker* gStSsdDbMaker=NULL; 

ClassImp(StSsdDbMaker)
//_____________________________________________________________________________
Int_t StSsdDbMaker::Init()
{
  LOG_DEBUG << "Init - Start - " << endm;
  if( m_Mode == 1) {
    LOG_INFO << "Init setting WafersPostions to sim" << endm;
    SetFlavor("sim","ssdWafersPosition");   
  }
  LOG_DEBUG << "StSsdDbMaker::Init() - Done - "<<endm;
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSsdDbMaker::Make()
{
  LOG_DEBUG << "Make" << endm;

  return kStOK;
}

//_____________________________________________________________________________
void StSsdDbMaker::Clear(const char*)
{
  LOG_DEBUG << "Clear" << endm;
  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StSsdDbMaker::Finish()
{
  LOG_DEBUG << "Finish" << endm;
  return kStOK;
}
