// $Id: StChargedPionVertex.cxx,v 1.3.2.1 2016/05/23 18:33:19 jeromel Exp $

#include "StChargedPionVertex.h"

ClassImp(StChargedPionVertex)

StChargedPionVertex::StChargedPionVertex() : TVector3(), mRanking(-1000.) { /* no-op */ }

StChargedPionVertex::~StChargedPionVertex() { /* no-op */ }

/*****************************************************************************
 * $Log: StChargedPionVertex.cxx,v $
 * Revision 1.3.2.1  2016/05/23 18:33:19  jeromel
 * Updates for SL12d / gcc44 embedding library - StDbLib, QtRoot update, new updated StJetMaker, StJetFinder, StSpinPool ... several cast fix to comply with c++0x and several cons related fixes (wrong parsing logic). Changes are similar to SL13b (not all ode were alike). Branch BSL12d_5_embed.
 *
 * Revision 1.4  2012/11/09 03:31:34  perev
 * Cleanup
 *
 * Revision 1.3  2008/12/29 15:58:31  kocolosk
 * removed commented code and added $Id: StChargedPionVertex.cxx,v 1.3.2.1 2016/05/23 18:33:19 jeromel Exp $/$Log: StChargedPionVertex.cxx,v $
 * removed commented code and added $Id: StChargedPionVertex.cxx,v 1.4 2012/11/09 03:31:34 perev Exp $/Revision 1.3.2.1  2016/05/23 18:33:19  jeromel
 * removed commented code and added $Id: StChargedPionVertex.cxx,v 1.4 2012/11/09 03:31:34 perev Exp $/Updates for SL12d / gcc44 embedding library - StDbLib, QtRoot update, new updated StJetMaker, StJetFinder, StSpinPool ... several cast fix to comply with c++0x and several cons related fixes (wrong parsing logic). Changes are similar to SL13b (not all ode were alike). Branch BSL12d_5_embed.
 * removed commented code and added $Id: StChargedPionVertex.cxx,v 1.4 2012/11/09 03:31:34 perev Exp $/
 * removed commented code and added $Id: StChargedPionVertex.cxx,v 1.3.2.1 2016/05/23 18:33:19 jeromel Exp $/Revision 1.4  2012/11/09 03:31:34  perev
 * removed commented code and added $Id: StChargedPionVertex.cxx,v 1.3.2.1 2016/05/23 18:33:19 jeromel Exp $/Cleanup
 * removed commented code and added $Id: StChargedPionVertex.cxx,v 1.3.2.1 2016/05/23 18:33:19 jeromel Exp $/ as needed
 *
 *****************************************************************************/
