/**********************************************
 *
 * $Id: StTpcMcAnalysisMaker.h,v 1.4 2015/01/28 21:20:02 fisyak Exp $
 * $Log: StTpcMcAnalysisMaker.h,v $
 * Revision 1.4  2015/01/28 21:20:02  fisyak
 * Freeze
 *
 * Revision 1.3  2012/09/25 13:38:13  fisyak
 * Freeze
 *
 * Revision 1.2  2010/11/09 16:32:45  fisyak
 * rename TTree and add RcHit
 *
 * Revision 1.1.1.1  2004/05/13 22:57:50  fisyak
 * TPC pixel/cluster analysis
 *
 **********************************************/

#ifndef StTpcMcAnalysisMaker_HH
#define StTpcMcAnalysisMaker_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TpcCluster.h"
class TTree;

class StTpcMcAnalysisMaker : public StMaker {
 public:
  StTpcMcAnalysisMaker(const char* name = "StTpcMcAnalysisMaker",
		       const char* title = "event/StTpcMcAnalysisMaker") : 
  StMaker(name,title), mTpcT(0), fCluster(0) {}
  virtual ~StTpcMcAnalysisMaker() {}
  virtual Int_t Init();
  virtual Int_t Make();
  Int_t  SingleCluster();
  Int_t  MultiCluster();
  void   SetMultiCluster() {m_Mode = 1;} // switch between single and multiple cluster on TTree
 private:
  TTree      *mTpcT;
  TpcCluster *fCluster;
  virtual const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcMcAnalysisMaker.h,v 1.4 2015/01/28 21:20:02 fisyak Exp $ built " __DATE__ " " __TIME__; return cvs;}
  ClassDef(StTpcMcAnalysisMaker,0)
};
#endif
