/***************************************************************************
 *
 * $Id: QinvPairCut.h,v 1.1 2000/09/14 18:36:59 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on Qinvariant
 *
 ***************************************************************************
 *
 * $Log: QinvPairCut.h,v $
 * Revision 1.1  2000/09/14 18:36:59  lisa
 * Added Qinv and ExitSep pair cuts and BPLCMSFrame3DCorrFctn_SIM CorrFctn
 *
 *
 *
 **************************************************************************/


#ifndef QinvPairCut_hh
#define QinvPairCut_hh


#include "StHbtMaker/Base/StHbtPairCut.h"

class QinvPairCut : public StHbtPairCut{
public:
  QinvPairCut();
  QinvPairCut(const QinvPairCut&);
  //~QinvPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  QinvPairCut* Clone();

  void SetQinvRange(const double& Lo, const double& Hi);

private:
  long mNPairsPassed;
  long mNPairsFailed;
  double mQinvLo;
  double mQinvHi;

#ifdef __ROOT__
  ClassDef(QinvPairCut, 1)
#endif
};

inline QinvPairCut::QinvPairCut(const QinvPairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline QinvPairCut* QinvPairCut::Clone() { QinvPairCut* c = new QinvPairCut(*this); return c;}

#endif
