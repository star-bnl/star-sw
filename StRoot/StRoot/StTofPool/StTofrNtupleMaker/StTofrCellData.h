/*******************************************************************
 *
 * $Id: StTofrCellData.h,v 1.6 2008/06/05 18:33:45 dongx Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: variables list for filling TOFr TTree
 *
 *****************************************************************
 *
 * $Log: StTofrCellData.h,v $
 * Revision 1.6  2008/06/05 18:33:45  dongx
 * -added members in tree: tDiff, tofcorr and beta for check
 * -beamLine read from database
 *
 * Revision 1.5  2008/05/08 21:09:36  dongx
 * Changed precision of time info to double type
 *
 * Revision 1.4  2008/05/06 18:42:09  dongx
 * Updated for Run8 analysis
 *
 * Revision 1.2  2004/04/12 16:17:03  dongx
 * add AdcLoRes in the ntuple
 *
 * Revision 1.1  2004/03/11 22:39:54  dongx
 * first release
 *
 *
 *******************************************************************/
#ifndef STTOFRCELLDATA_H
#define STTOFRCELLDATA_H
  const Int_t kMaxTracks = 1000;

  struct StTofrCellData {
    Int_t run, evt, trgword;
    Float_t  vertexX, vertexY, vertexZ;
    Int_t nTofHits;
    Int_t vpdEast, vpdWest;
    Int_t numberOfVpdEast, numberOfVpdWest;
    Double_t tdcSumEast, tdcSumWest, tdcSum;
    Float_t tDiff;
    Double_t pvpdLeadingEdgeTimeEast[19], pvpdTotEast[19];
    Double_t pvpdLeadingEdgeTimeWest[19], pvpdTotWest[19];
    Int_t tray[kMaxTracks], module[kMaxTracks], cell[kMaxTracks], daq[kMaxTracks];
    Double_t leadingEdgeTime[kMaxTracks], tot[kMaxTracks];
//    Int_t matchFlag[kMaxTracks];
    Float_t xlocal[kMaxTracks], ylocal[kMaxTracks], zlocal[kMaxTracks], deltay[kMaxTracks];
    Int_t trackId[kMaxTracks], charge[kMaxTracks];
    Int_t nHits[kMaxTracks], nHitsFit[kMaxTracks];
    Float_t dcaX[kMaxTracks], dcaY[kMaxTracks], dcaZ[kMaxTracks];//point closet approach to beam line
    Float_t length[kMaxTracks], eta[kMaxTracks];
    Float_t p[kMaxTracks], pt[kMaxTracks], px[kMaxTracks], py[kMaxTracks], pz[kMaxTracks];
    Float_t dedx[kMaxTracks], dedxError[kMaxTracks], cherenkovAngle[kMaxTracks];
    Int_t nHitsDedx[kMaxTracks], cherenkovPhotons[kMaxTracks];
    Float_t nSigE[kMaxTracks], nSigPi[kMaxTracks], nSigK[kMaxTracks], nSigP[kMaxTracks];
    Float_t tofcorr[kMaxTracks], beta[kMaxTracks];
  };  
#endif
