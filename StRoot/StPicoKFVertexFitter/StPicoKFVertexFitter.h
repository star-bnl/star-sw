#ifndef StPicoKFVertexFitter_h
#define StPicoKFVertexFitter_h

/* **************************************************
 *  Class to fit primary vertex using KF vertex maker
 *
 *  Usage:
 *
 * **************************************************
 *  Authors:
 *            **Liang He(he202@purdue.edu)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *
 * ** Code Maintainer
 *
 * **************************************************
 */

#include <vector>
#include "StThreeVectorF.hh"

class StPicoDst;

class StPicoKFVertexFitter
{
  public:
   StPicoKFVertexFitter() {}
   ~StPicoKFVertexFitter() {}
   
   StThreeVectorF primaryVertexRefit(StPicoDst const*) const;
   StThreeVectorF primaryVertexRefit(StPicoDst const*,
       std::vector<int>& tracksToRemove) const;
};

inline StThreeVectorF StPicoKFVertexFitter::primaryVertexRefit(StPicoDst const* picoDst) const
{
  std::vector<int> v;
  return primaryVertexRefit(picoDst,v);
}
#endif
