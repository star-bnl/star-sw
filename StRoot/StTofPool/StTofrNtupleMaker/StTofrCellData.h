/*******************************************************************
 *
 * $Id: StTofrCellData.h,v 1.1 2004/03/11 22:39:54 dongx Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: variables list for filling TOFr TTree
 *
 *****************************************************************
 *
 * $Log: StTofrCellData.h,v $
 * Revision 1.1  2004/03/11 22:39:54  dongx
 * first release
 *
 *
 *******************************************************************/
#ifndef STTOFRCELLDATA_H
#define STTOFRCELLDATA_H
  struct StTofrCellData {
    Int_t run, evt, trgword;
    Float_t magfield, ctbsum, zdcsum, xvtx, yvtx, zvtx, zvtxchi2;
    Int_t refmult;
    Int_t nprimary;
    Float_t meanpt;
    Float_t TdcStart, Tdiff;
    Int_t Ieast, Iwest;
    Float_t TdcSumEast, TdcSumWest, TdcSum;
    Int_t te1, te2, te3, tw1, tw2, tw3;
    Int_t ae1, ae2, ae3, aw1, aw2, aw3;
    Int_t tray, module, cell, daq, tdc, adc, hitprof, matchflag;
    Float_t xlocal, ylocal, zlocal, deltay;
    Int_t trackId, charge;
    Int_t ntrackpoints, nfitpoints;
    Float_t dca;
    Float_t s, p, pt, px, py, pz, eta;
    Float_t dedx, dedxerror, cherang;
    Int_t dedx_np, cherang_nph;
    Int_t nSigE, nSigPi, nSigK, nSigP;
  };
#endif
