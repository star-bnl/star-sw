//! Structure for TOFp Match TTree 
/*! \struct StTofpMatchData
 *  \brief structure for the TOFp Match TTree
 *  \author Frank Geurts
 *
 * $Id: StTofpMatchData.h,v 1.1 2003/08/07 23:55:47 geurts Exp $
 */
/* -------------------------------------------------------------------------
 * $Log: StTofpMatchData.h,v $
 * Revision 1.1  2003/08/07 23:55:47  geurts
 * first release
 *
 *
 * -------------------------------------------------------------------------
 */
#ifndef STTOFPMATCHDATA_H
#define STTOFPMATCHDATA_H
struct StTofpMatchData {
  Int_t daqid;
  Int_t nneighbors;
  Float_t zlocal, philocal;
  Int_t hitprof;
  Int_t nfitpoints,ntrackpoints,maxpoints;
  Float_t r_last,p,pt;
};
#endif
