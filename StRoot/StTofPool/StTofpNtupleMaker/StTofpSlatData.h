//! Structure for TOFp Slat TTree 
/*! \struct StTofpSlatData
 *  \brief  structure for filling TOFp Slat TTree
 *  \author Frank Geurts
 *
 * $Id: StTofpSlatData.h,v 1.2 2003/12/04 06:54:08 geurts Exp $
 */
/* -------------------------------------------------------------------------
 * $Log: StTofpSlatData.h,v $
 * Revision 1.2  2003/12/04 06:54:08  geurts
 * introduced variables relevant to TOFp flow analysis
 *  * trackId, px and py
 *  * xvtx and yvtx
 *
 * Revision 1.1  2003/08/07 23:55:47  geurts
 * first release
 *
 *
 * -------------------------------------------------------------------------
 */
#ifndef STTOFPSLATDATA_H
#define STTOFPSLATDATA_H
  struct StTofpSlatData {
    Int_t run, evt, trgword;
    Float_t magfield, ctbsum, zdcsum, xvtx, yvtx, zvtx, zvtxchi2;
    Int_t refmult;
    Int_t nprimary;
    Float_t meanpt;
    Float_t tdcstart;
    Int_t te1, te2, te3, tw1, tw2, tw3;
    Int_t ae1, ae2, ae3, aw1, aw2, aw3;
    Int_t slat, tdc, adc, hitprof, matchflag;
    Float_t zhit;
    Float_t zhitinner, zhitouter;
    Float_t ss, theta_xy, theta_zr;
    Int_t trackId;
    Int_t ntrackpoints, nfitpoints;
    Float_t r_last, chi2;
    Float_t s, p, pt, px, py, pz, eta;
    Float_t dedx;
    Int_t dedx_np;
    Float_t cherang;
    Int_t cherang_nph;
  };
#endif
