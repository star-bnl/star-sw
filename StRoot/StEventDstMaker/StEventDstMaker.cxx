//*-- Author : Victor Perevoztchikov
// 
// $Id: StEventDstMaker.cxx,v 1.2 2006/08/10 03:31:46 perev Exp $
// $Log: StEventDstMaker.cxx,v $
// Revision 1.2  2006/08/10 03:31:46  perev
// Assert==>assert
//
// Revision 1.1  2001/05/30 17:48:30  perev
// StEvent branching
//
// Revision 1.14  2000/06/23 16:50:07  fisyak
// remove params
//
// Revision 1.13  1999/12/19 16:07:01  perev
// Add README
//
// Revision 1.12  1999/07/15 13:57:44  perev
// cleanup
//
// Revision 1.11  1999/07/10 22:59:16  fine
// Some comments have been introduced to show html docs
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventDstMaker class for Makers                                        //
//                                                                      //
// This commented block at the top of the source file is considered as  //
// this class description to be present on the this class Web page.     //
//  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html                    //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TError.h"
#include "StEventDstMaker.h"
#include "StEvent.h"
#include "StEventClusteringHints.h"

ClassImp(StEventDstMaker)

//_____________________________________________________________________________
StEventDstMaker::StEventDstMaker(const char *name):StMaker(name)
{
 //
 //  const char *name -  the name of maker instance
 //
}
//_____________________________________________________________________________
StEventDstMaker::~StEventDstMaker()
{
}
//_____________________________________________________________________________
Int_t StEventDstMaker::Init()
{
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StEventDstMaker::Make()
{
static const char *EVT = "StEvent";

  StEvent *evt = (StEvent*)GetDataSet(EVT);
  if (!evt) return kStWarn;
   
  StEventClusteringHints *clu= evt->clusteringHints();
  assert(clu);
  if (m_Mode==1) clu->setMiniDstMode();
  if (m_Mode==0) clu->setDstMode();
  return kStOK;
}










