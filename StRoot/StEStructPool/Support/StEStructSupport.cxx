/**********************************************************************
 *
 * $Id: StEStructSupport.cxx,v 1.21 2009/05/08 00:21:42 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description: Simple helper class for calculating
 *              delta-rho, delta-rho/rho, and delta-rho/sqrt(rho)
 *              plus some other goodies
 *
 ***********************************************************************/
#include "StEStructSupport.h"
#include "StEStructPool/Correlations/StEStructMaxB.h"
#include "Stiostream.h"
#include <sstream>
#include "Stsstream.h"
#include "TMath.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2D.h"


const char* _pair_typename[] = {"Sib","Mix"};
const char* _pair_chargename[]   = {"pp","pm","mp","mm"};
const char* _pair_ptweight[]   = {"Pr","Su"};
int _pair_typemax=2;
int _pair_chargemax=4;
int _pair_totalmax=_pair_typemax*_pair_chargemax;

ClassImp(StEStructSupport)

//---------------------------------------------------------
bool StEStructSupport::goodName(const char* name){

  TString testName(_pair_typename[0]);
  testName+=_pair_chargename[0];
  testName+=name;
  TObject* obj = NULL;
  mtf->GetObject(testName.Data(),obj);
  if(!obj) return false;

  return true;
}
//---------------------------------------------------------
bool StEStructSupport::goodName_zBuf(const char* name, int zBin){

  TString testName(_pair_typename[0]);
  testName+=_pair_chargename[0];
  testName+=name;
  testName+="_zBuf_"; testName += zBin;
  TObject* obj=NULL;
  mtf->GetObject(testName.Data(),obj);
  if(!obj) return false;

  return true;
}

//---------------------------------------------------------
char* StEStructSupport::getFrontName(int itype){

  /* for itype=0-7, returns Sibpp, Sibpm, Sibmp, Sibmm, Mixpp, Mixpm, Mixmp, Mixmm   */

  if(mtmpString) delete [] mtmpString;
  mtmpString=new char[256];
  ostringstream ts;
  int j,k;
  if(itype<_pair_chargemax){
    j=0; k=itype;
  } else {
    j=1; k=itype-_pair_chargemax;
  }

  ts<<_pair_typename[j]<<_pair_chargename[k];
  strcpy(mtmpString,(ts.str()).c_str());
  return mtmpString;
}

//---------------------------------------------------------
const char* StEStructSupport::getTypeName(int itype){ 
  return _pair_typename[itype]; 
}

//---------------------------------------------------------
const char* StEStructSupport::getChargeSignName(int ics){ 
  return _pair_chargename[ics]; 
}

//---------------------------------------------------------
//
//  Now the real class work 
//
//---------------------------------------------------------

StEStructSupport::StEStructSupport(TFile* tf, int bgmode, float* npairs) : mtf(tf), mbgMode(bgmode), mtmpString(NULL){

  //
  // npairs is a normalization for when one cuts on say two (many) different 
  // ytyt slices and wants to compare the amplitudes, the generic normalization
  // of sum of rho = 1. isn't sufficient
  //

  msilent=false;
  mapplyDEtaFix=false; // must set explicitly now
  mDoSymmetrize = false;
  mPairNormalization = false;
  mIdenticalPair = true;

  // Scan file for number of z-buffer bins
  getNZBins();
  
  if(npairs){
    mnpairs = new float[8];
    for(int i=0;i<8;i++)mnpairs[i]=npairs[i];
  } else {
    mnpairs = 0;
  }

}

StEStructSupport::~StEStructSupport(){ 
  if(mtmpString) delete [] mtmpString; 
  if(mnpairs) delete [] mnpairs; 
};

//---------------------------------------------------------
int StEStructSupport::getNZBins(){
    TString hname("NEventsSib_zBuf_");
    int iZBin;
    for (iZBin=1;iZBin<99;iZBin++) {
        TString zBinName(hname.Data());
        zBinName += iZBin-1;
        TH1* tmp = NULL;
        mtf->GetObject(zBinName.Data(),tmp);
        if (!tmp) {
            break;
        }
    }
    mNumZBins = iZBin-1;
    return mNumZBins;
};
//---------------------------------------------------------
// We have been calculating \Delta\rho/rhoin different multiplicity/z bins
// then averaging using the number of pairs as a weight.
// This heavily favors slightly larger multiplicities..
// These get*Number routines are so I can try multiplicity weighting.
//
// Note: We allocate space for four floats, fill in the numbers, then return a pointer to them.
//       Valgrind claims they are being used un-initialized. I think that is an issue
//       with Valgrind. Might be nice to change how we return the numbers to get
//       rid of those warnings.
float *StEStructSupport::getCommonNumber(int zBin) {
    float *number = new float[4];
    TString hname("");
    TH1 *hpt[4];

    hname = "meanPtPA_zBuf_"; hname += zBin;   mtf->GetObject(hname.Data(),hpt[0]);
    hname = "meanPtMA_zBuf_"; hname += zBin;   mtf->GetObject(hname.Data(),hpt[1]);
    hname = "meanPtPB_zBuf_"; hname += zBin;   mtf->GetObject(hname.Data(),hpt[2]);
    hname = "meanPtMB_zBuf_"; hname += zBin;   mtf->GetObject(hname.Data(),hpt[3]);

    // Want pp, pm, mp, mm
    number[0] = hpt[0]->Integral() + hpt[2]->Integral();
    number[1] = hpt[0]->Integral() + hpt[3]->Integral();
    number[2] = hpt[1]->Integral() + hpt[2]->Integral();
    number[3] = hpt[1]->Integral() + hpt[3]->Integral();
    return number;
};
//---------------------------------------------------------
float *StEStructSupport::getCommonPairs(int zBin) {
    float *pairs = new float[4];
    for (int i=0;i<4;i++) {
        pairs[i] = 0;
        TString hname("");
        hname += "Sib";
        hname += _pair_chargename[i];
        hname += "NDEtaDPhi_zBuf_";
        hname += zBin;
        TH1 *tmp = NULL;
        mtf->GetObject(hname.Data(),tmp);
        if (tmp) {
            pairs[i] = tmp->Integral();
        } else {
            pairs[i] = 0;
        }
    }
    return pairs;
};
//---------------------------------------------------------
float *StEStructSupport::getChargeNumber(int zBin) {
    float *number = new float[4];
    TString hname("");
    TH1 *hpt[4];

    hname = "meanPtPA_zBuf_"; hname += zBin;   mtf->GetObject(hname.Data(),hpt[0]);
    hname = "meanPtMA_zBuf_"; hname += zBin;   mtf->GetObject(hname.Data(),hpt[1]);
    hname = "meanPtPB_zBuf_"; hname += zBin;   mtf->GetObject(hname.Data(),hpt[2]);
    hname = "meanPtMB_zBuf_"; hname += zBin;   mtf->GetObject(hname.Data(),hpt[3]);

    // Want LS, US, CI, CD. Turns out to just be grand sum.
    number[0] = hpt[0]->Integral() + hpt[2]->Integral();
    number[0]+= hpt[0]->Integral() + hpt[3]->Integral();
    number[0]+= hpt[1]->Integral() + hpt[2]->Integral();
    number[0]+= hpt[1]->Integral() + hpt[3]->Integral();
    number[1] = number[0];
    number[2]+= number[0];
    number[3] = number[0];
    return number;
};
//---------------------------------------------------------
float *StEStructSupport::getChargePairs(int zBin) {
    float *pairs = new float[4];
    for (int i=0;i<4;i++) {
        pairs[i] = 0;
        TString hname("");
        hname += "Sib";
        hname += _pair_chargename[i];
        hname += "NDEtaDPhi_zBuf_";
        hname += zBin;
        TH1 *tmp = NULL;
        mtf->GetObject(hname.Data(),tmp);
        if (tmp) {
            pairs[i] = tmp->Integral();
        } else {
            pairs[i] = 0;
        }
    }
    pairs[0] += pairs[3];
    pairs[1] += pairs[2];
    pairs[2] = pairs[0] + pairs[1];
    pairs[3] = pairs[0] + pairs[1];
    return pairs;
};

//---------------------------------------------------------
double *StEStructSupport::getd2NdEtadPhi(int zBin) {
    double* retVal = new double[5];
    // (nA+)*(nB+), (nA+)*(nB-), (nA-)*(nB+), (nA-)*(nB-), (nA+)+(nA-)+(nB+)+(nB-)
    // where the () means {d^2() \over d\eta d\phi}
    // Note that in cases where we don't distinguish a and b
    // (nA+)*(nB-) and (nA-)*(nB+) are the same and retVal[2] is not necessary.

    TString hSibName("NEventsSib_zBuf_"); hSibName += zBin;  TH1* hNSum;  mtf->GetObject(hSibName.Data(),hNSum);
    double nEvents = hNSum->Integral();

    double nTracksAP, nTracksAM, nTracksBP, nTracksBM;
    double dNdEtaAP, dNdEtaAM, dNdEtaBP, dNdEtaBM;
    TH1 *hEta;
    TString hname;

    hname = "etaPA_zBuf_";  hname += zBin;  mtf->GetObject(hname.Data(),hEta);  nTracksAP = hEta->Integral();
    hname = "etaMA_zBuf_";  hname += zBin;  mtf->GetObject(hname.Data(),hEta);  nTracksAM = hEta->Integral();
    hname = "etaPB_zBuf_";  hname += zBin;  mtf->GetObject(hname.Data(),hEta);  nTracksBP = hEta->Integral();
    hname = "etaMB_zBuf_";  hname += zBin;  mtf->GetObject(hname.Data(),hEta);  nTracksBM = hEta->Integral();

    // The 0.5 is because we integrate tracks over two units of rapidity.
    dNdEtaAP = 0.5 * nTracksAP / nEvents;
    dNdEtaAM = 0.5 * nTracksAM / nEvents;
    dNdEtaBP = 0.5 * nTracksBP / nEvents;
    dNdEtaBM = 0.5 * nTracksBM / nEvents;
    retVal[0] = dNdEtaAP*dNdEtaBP / pow(2*3.1415926,2);
    retVal[1] = dNdEtaAP*dNdEtaBM / pow(2*3.1415926,2);
    retVal[2] = dNdEtaAM*dNdEtaBP / pow(2*3.1415926,2);
    retVal[3] = dNdEtaAM*dNdEtaBM / pow(2*3.1415926,2);

    // There are factors of 2 when expanding the full correlation term that
    // could be included in loops over pairs, but appear not to have been.
    // Fix that up here.
    if (mIdenticalPair) {
        retVal[1] *= 2;
        retVal[2]  = 0;
        retVal[4] = (dNdEtaAP + dNdEtaAM) / (2*3.1415926);
    } else {
        retVal[0] *= 2;
        retVal[2] *= 2;
        retVal[3] *= 2;
        retVal[4] = (dNdEtaAP + dNdEtaAM + dNdEtaBP + dNdEtaBM) / (2*3.1415926);
    }
    return retVal;
}
//---------------------------------------------------------
// These are the values of d2N/dEtadPhi used when combining \Delta\rho/\rho_{ref}
// First four values are product  A+B+, A+B-, A-B+, A-A-.
// Firth value is A+ + A- + B+ + B- (or one have of that, look in getd2NdEtadPhi).
double *StEStructSupport::getScaleFactors() {
    double* retVal = new double[5];
    retVal = getd2NdEtadPhi(0);

    double *d2NdEtadPhi;
    for (int iz=1;iz<mNumZBins;iz++) {
        d2NdEtadPhi = getd2NdEtadPhi(iz);
        for (int iType=0;iType<5;iType++) {
            retVal[iType] += d2NdEtadPhi[iType];
        }
        delete [] d2NdEtadPhi;
    }
    for (int iType=0;iType<5;iType++) {
        retVal[iType] /= mNumZBins;
    }
    return retVal;
}
double *StEStructSupport::getScaleFactors(int zBin) {
    double* retVal = new double[5];
    retVal = getd2NdEtadPhi(zBin);
    return retVal;
}
//---------------------------------------------------------
double *StEStructSupport::getptHat(int zBin) {
    double* retVal = new double[4];
    TH1 *hpt;
    TString hname;

    hname = "meanPtPA_zBuf_"; hname += zBin;  mtf->GetObject(hname.Data(),hpt);  retVal[0] = hpt->GetMean();
    hname = "meanPtMA_zBuf_"; hname += zBin;  mtf->GetObject(hname.Data(),hpt);  retVal[1] = hpt->GetMean();
    hname = "meanPtPB_zBuf_"; hname += zBin;  mtf->GetObject(hname.Data(),hpt);  retVal[2] = hpt->GetMean();
    hname = "meanPtMB_zBuf_"; hname += zBin;  mtf->GetObject(hname.Data(),hpt);  retVal[3] = hpt->GetMean();

    return retVal;
}
//---------------------------------------------------------
int StEStructSupport::histogramExists(const char* name, int zBin){
    TH2D* tmpF;
    TString hname(getFrontName(0));  hname += name;  hname+= "_zBuf_";  hname+= zBin;
    mtf->GetObject(hname.Data(),tmpF);
    if (tmpF) {
        return 1;
    }
    return 0;
}
TH2D** StEStructSupport::getHists(const char* name, int zBin){
    TH2D** retVal=NULL;
    // If zBin = 0 check for name without with zBuf first.
    if ((0 == zBin) && !goodName(name)) {
        if (!goodName_zBuf(name,zBin)) {
            return retVal;
        }
    }

    retVal = new TH2D*[8];
    for (int i=0;i<_pair_totalmax;i++) {
        TString hname(getFrontName(i));  hname+=name;
        mtf->GetObject(hname.Data(),retVal[i]);
        if (!retVal[i]) {
            TString hname(getFrontName(i));  hname += name;  hname+= "_zBuf_";  hname+= zBin;
            mtf->GetObject(hname.Data(),retVal[i]);
        }
  }
  return retVal;
}
//---------------------------------------------------------
TH2D** StEStructSupport::getLocalClones(const char* name, int zBin){
  // A note on Sumw2.  It appears that by default root will use the square root of the
  //                   bin contents as the error. Not correct when we scale bin contents.
  //                   One can use Sumw2 to trigger error propogation. However, this seems
  //                   to be done implicitly in StEStructHAdd so we don't need to do it again.

  // hset contains pointers to in-memory histograms, _not_ copies.
  TH2D** hset=getHists(name,zBin);
  if(!hset) return (TH2D**)NULL;

  // make local clones
  TH2D** hlocal=new TH2D*[_pair_totalmax];
  for(int i=0;i<_pair_totalmax;i++) {
    hlocal[i]=(TH2D*)hset[i]->Clone();
//    hlocal[i]->Sumw2();  // trigger error propogation
  }
  // Delete array containing pointers, but not objects being pointed to.
  delete [] hset;

  return hlocal;
}
//-----------------------------------------------------
void StEStructSupport::rescale(TH2D** hists, int zBin) {
    // Divide hists by bin widths, divide by Nevents.
    // Bin by bin rrors should be scaled properly, but won't include errors on the scale factors.

    if(!mtf) return;

    //>>>>>Need to rethink how to handle old cases where _zBuf_n is not in histogram name!!!!!
    TH1 *hNum;
    TString hSibName("NEventsSib_zBuf_");  hSibName += zBin;  mtf->GetObject(hSibName.Data(),hNum);
    double nSib = hNum->Integral();
    TString hMixName("NEventsMix_zBuf_");  hMixName += zBin;  mtf->GetObject(hMixName.Data(),hNum);
    double nMix = hNum->Integral();

    for (int i=0;i<4;i++) {
        if (hists[i]->Integral() > 0) {
            // dividing by ex*ey converts all input hists from counts to densities, so all operations and final result should also be density.
            double dx = (hists[i]->GetXaxis())->GetBinWidth(1);
            double dy = (hists[i]->GetYaxis())->GetBinWidth(1);
            double binFactor = dx*dy; 
            hists[i]->Scale(1.0/(nSib*binFactor));
            if (i==0 && !msilent) {
                cout << "Scaling with Nevents " << nSib << " and binFactor " << binFactor << endl;
            }

            if (mPairNormalization) {
                // This is original normalization. Average value of \Delta\rho should be one.
                double nSibPairs = hists[i]->Integral();
                double nMixPairs = hists[i+4]->Integral();
                if (nMixPairs > 0) {
                    hists[i+4]->Scale(nSibPairs/nMixPairs);
                } else {
                    hists[i+4]->Scale(0);
                }
            } else {
                // We know how many sibling and mixed events there were.
                // In this normalization we should be able to integrate (weighted with an
                //  appropriate kernel) to get fluctuations at larger scales.
                if (nMix > 0) {
                    hists[i+4]->Scale(0.5/(nMix*binFactor));
                } else {
                    hists[i+4]->Scale(0);
                }
            }
        }
    }
}

//---------------------------------------------------------
TH2D** StEStructSupport::getPtHists(const char* name, int zBin){

  TH2D** retVal=NULL;
  // If zBin = 0 check for name without with zBuf first.
  // These is a possiblitiy that the mean pt histograms are not in the file
  // although the number correlations are.
  // Add "Pr" in front of name before checking for existance of histogram.
  TString chkName("Pr");  chkName += name;
  if((0 == zBin) && !goodName(chkName.Data())) {
      if (!goodName_zBuf(chkName.Data(),zBin)) {
          return retVal;
      }
  }
  
  retVal=new TH2D*[32];

  for(int i=0;i<_pair_totalmax;i++){
    TString hname(getFrontName(i)),hprname(getFrontName(i)),hpaname(getFrontName(i)),hpbname(getFrontName(i));
                   hname+=name;   hname+="_zBuf_";   hname+=zBin;
    hprname+="Pr"; hprname+=name; hprname+="_zBuf_"; hprname+=zBin;
    hpaname+="Pa"; hpaname+=name; hpaname+="_zBuf_"; hpaname+=zBin;
    hpbname+="Pb"; hpbname+=name; hpbname+="_zBuf_"; hpbname+=zBin;
    mtf->GetObject(hname.Data(),retVal[i]);
    mtf->GetObject(hprname.Data(),retVal[i+8]);
    mtf->GetObject(hpaname.Data(),retVal[i+16]);
    mtf->GetObject(hpbname.Data(),retVal[i+24]);
  }

  return retVal;
}
//---------------------------------------------------------------
TH2D** StEStructSupport::getPtClones(const char* name, int zBin){
  // A note on Sumw2.  See note in getClones above. I don't think we have done the
  // >>>>>             errors properly in StEStructHAdd yet. Need to look into this.

  // hset contains pointers to in-memory histograms, _not_ copies.
  TH2D** hset=getPtHists(name,zBin);
  if(!hset) return (TH2D**)NULL;

  // make local clones
  TH2D** hlocal=new TH2D*[_pair_totalmax*4];
  for(int i=0;i<_pair_totalmax*4;i++) {
    hlocal[i]=(TH2D*)hset[i]->Clone();
//    hlocal[i]->Sumw2();  // trigger error propogation
  }
  // Delete array containing pointers, but not objects being pointed to.
  delete [] hset;

  return hlocal;
}
//-----------------------------------------------------
void StEStructSupport::rescalePt(TH2D** hists, int zBin) {
    // Divide hists by bin widths, divide by Nevents.

    if(!mtf) return;

    TH1 *hNum;
    TString hSibName("NEventsSib_zBuf_");  hSibName += zBin;  mtf->GetObject(hSibName.Data(),hNum);
    double nSib = hNum->GetEntries();
    TString hMixName("NEventsMix_zBuf_");  hMixName += zBin;  mtf->GetObject(hMixName.Data(),hNum);
    double nMix = hNum->GetEntries();

    for(int i=0;i<4;i++) {
        for (int j=0;j<4;j++) {
            if (hists[i+4*j]->Integral() > 0) {
                double binFactor = 1.0;
                double dx = (hists[i+8*j]->GetXaxis())->GetBinWidth(1);
                double dy = (hists[i+8*j]->GetYaxis())->GetBinWidth(1);
                binFactor = dx*dy;
                // divinding by ex*ey converts all input hists from counts to densities, so all operations and final result should also be density.
                if(i==0 && !msilent) cout << "Scaling with Nevents " << nSib << " and binFactor " << binFactor << endl;
                hists[i+8*j]->Scale(1.0/(nSib*binFactor));

                double nSibPairs = hists[i+8*j]->Integral();
                double nMixPairs = hists[i+4+8*j]->Integral();
                if (mPairNormalization) {
                    if (nMixPairs > 0) {
                        hists[i+4+8*j]->Scale(nSibPairs/nMixPairs);
                    } else {
                        hists[i+4+8*j]->Scale(0);
                    }
                } else {
                    if (nMix > 0) {
                        hists[i+4+8*j]->Scale(0.5/(nMix*binFactor));
                    } else {
                        hists[i+4+8*j]->Scale(0);
                    }
                }
            }
        }
    }
};

//-----------------------------------------------------
void StEStructSupport::setSymmetrizeUS(bool symm) {
    mDoSymmetrize = symm;
}
//-----------------------------------------------------
void StEStructSupport::symmetrizeUS(const char *name, TH2D** histos) {
  // For pid mode the US X vs X and LS but different species histograms are not symmetrized.
  // To form CD and CI combos we need the US to be symmetrized in the cases the LS are.

  // Histograms to be symmetrized are:
  char *symHistos[] = {"YtYt",   "NYtYt",   "PtPt",
                       "PhiPhi", "NPhiPhi", "PrPhiPhi", "PaPhiPhi", "PbPhiPhi",
                       "EtaEta",            "PrEtaEta", "PaEtaEta", "PbEtaEta"};
  // eight input histograms ++,+-,-+,-- for Sib and Mix
  int symInt[] = {1,2, 5, 6};
  for (int xy=0;xy<12;xy++) {
      if (!strcmp(name,symHistos[xy])) {
          for (int ih=0;ih<4;ih++) {
              for (int ix=1;ix<=histos[symInt[ih]]->GetNbinsX();ix++) {
                  for (int iy=ix;iy<=histos[symInt[ih]]->GetNbinsY();iy++) {
                      double sum = histos[symInt[ih]]->GetBinContent(ix,iy) + histos[symInt[ih]]->GetBinContent(iy,ix);
                      double err = sqrt(pow(histos[symInt[ih]]->GetBinError(ix,iy),2) + pow(histos[symInt[ih]]->GetBinError(iy,ix),2));
                      histos[symInt[ih]]->SetBinContent(ix,iy,sum);
                      histos[symInt[ih]]->SetBinContent(iy,ix,sum);
                      histos[symInt[ih]]->SetBinError(ix,iy,err);
                      histos[symInt[ih]]->SetBinError(iy,ix,err);
                  }
              }
          }
      }
  }
};
//-----------------------------------------------------
void StEStructSupport::symmetrizePtUS(const char *name, TH2D** histos) {
  // For pid mode the US X vs X and LS but different species histograms are not symmetrized.
  // To form CD and CI combos we need the US to be symmetrized in the cases the LS are.

  // Histograms to be symmetrized are:
  char *symHistos[] = {"YtYt",   "NYtYt",   "PtPt",
                       "PhiPhi", "NPhiPhi", "PrPhiPhi", "PaPhiPhi", "PbPhiPhi",
                       "EtaEta",            "PrEtaEta", "PaEtaEta", "PbEtaEta"};
  int symInt[] = {1,2, 5, 6};
  //    4 groups of 8 (Sibpp,Sibpm,Sibmp,Sibmm,Mixpp,Mixpm,Mixmp,Mixmm) 
  //    1st 8 are number, 2nd 8 are pt1*pt2, 3rd 8 are pt1 and 4th 8 are pt2
  for (int xy=0;xy<12;xy++) {
      if (!strcmp(name,symHistos[xy])) {
          for (int ig=0;ig<32;ig+=8) {
              for (int ih=0;ih<4;ih++) {
                  int histNum = ig + symInt[ih];
                  for (int ix=1;ix<=histos[histNum]->GetNbinsX();ix++) {
                      for (int iy=ix;iy<=histos[histNum]->GetNbinsY();iy++) {
                          double sum = histos[histNum]->GetBinContent(ix,iy) + histos[histNum]->GetBinContent(iy,ix);
                          double err = sqrt(pow(histos[histNum]->GetBinContent(ix,iy),2) + pow(histos[histNum]->GetBinContent(iy,ix),2));
                          histos[histNum]->SetBinContent(ix,iy,sum);
                          histos[histNum]->SetBinContent(iy,ix,sum);
                          histos[histNum]->SetBinError(ix,iy,err);
                          histos[histNum]->SetBinError(iy,ix,err);
                      }
                  }
              }
          }
      }
  }
};

//-----------------------------------------------------
float* StEStructSupport::getNorms(TH2D** histArray){

  /* not really used any more but keep */

  float* retVal=NULL;
  if(!histArray) return retVal;

  retVal=new float[8];
  for(int i=0;i<_pair_totalmax; i++) retVal[i]=histArray[i]->Integral();

  return retVal;
}

//---------------------------------------------------------
TH2D** StEStructSupport::buildCommonRatios(const char* name, float* sf){
  return buildCommon(name,0,sf);
}
//---------------------------------------------------------
TH2D** StEStructSupport::buildCommonCFunctions(const char* name, float* sf){
  return buildCommon(name,1,sf);
}
//---------------------------------------------------------
TH2D** StEStructSupport::buildCommonRFunctions(const char* name, float* sf){
  return buildCommon(name,2,sf);
}

//---------------------------------------------------------
TH2D** StEStructSupport::buildCommonRatios(const char* name, float* sf, int zBin){
  return buildCommon(name,0,sf,zBin);
}
//---------------------------------------------------------
TH2D** StEStructSupport::buildCommonCFunctions(const char* name, float* sf, int zBin){
  return buildCommon(name,1,sf,zBin);
}
//---------------------------------------------------------
TH2D** StEStructSupport::buildCommonRFunctions(const char* name, float* sf, int zBin){
  return buildCommon(name,2,sf,zBin);
}
//---------------------------------------------------------
// Note for all buildXXX functions:
//  We have three basic quantities, \rho_{sib}, \rho_{ref} and {d^2N \over d\eta d\phi}
//  We use {d^2N \over d\eta d\phi}^2 as a proxy for \rho_{ref} in cases where we want
//  an acceptance/efficiency independent number. (Ok, there is still an average
//  efficiency that we have to divide by, and we probably want it at \eta=0.)
//  When combining z-bins and/or centralities we want to add \Delta\rho, but
//  each bin has a different efficiency correction. To account for this we define
//   \hat\Delta\rho = {d^2N \over d\eta d\phi}^2 * \Delta\rho / \rho_{ref}.
//   This takes role of \Delta\rho when we combine multiple bins.
//  Option 0) \Delta\rho/\rho_{ref}       -> \Sum_1^n{\hat\Delta\rho} / {(1/n) \Sum_1^n{dN/d\eta}}^2
//         1) \Delta\rho                  -> \hat\Delta\rho
//         2) \Delta\rho/sqrt(\rho_{ref}) -> \Sum_1^n{\hat\Delta\rho} / {(1/n) \Sum_1^n{dN/d\eta}}
// For YtYt we cannot use d2NdEtadPhi as proxy for \rho_{ref}.
// opt= 3, 4 and 5 combine \Delta\rho and \rho_{ref} directly.
//         3           \Delta\rho / rho_ref
//         4           \Delta\rho
//         5           \Delta\rh0 / sqrt(rho_ref)

// Common ->  {"PP","PM","MP","MM"};
TH2D** StEStructSupport::buildCommon(const char* name, int opt, float* sf) {
    TH2D** retVal= buildCommon(name, opt, sf, 0);
    if (!retVal) {
        return retVal;
    }

    // Fix up names and titles of histograms we will return.
    for (int iType=0;iType<4;iType++) {
        if (strstr(retVal[iType]->GetName(),"_zBuf_0")) {
            retVal[iType]->SetName(swapIn(retVal[iType]->GetName(),"_zBuf_0",""));
        }
        if (strstr(retVal[iType]->GetTitle(),"_zBuf_0")) {
            retVal[iType]->SetTitle(swapIn(retVal[iType]->GetTitle(),"_zBuf_0",""));
        }
    }
    const char* oldName[4]  = {"Sibpp","Sibpm","Sibmp","Sibmm"};
    const char* newName[4]  = {"PP ","PM ","MP ","MM "};
    const char* oldTitle[4] = {"Sibling : +.+","Sibling : +.-","Sibling : -.+","Sibling : -.-"};
    const char* newTitle[4] = {"PP : ++ ","PM : +- ","MP: -+ ","MM : -- "};
    for(int i=0;i<4;i++) {
        retVal[i]->SetName(swapIn(retVal[i]->GetName(),oldName[i],newName[i]));
        retVal[i]->SetTitle(swapIn(retVal[i]->GetTitle(),oldTitle[i],newTitle[i]));
    }

    if (opt < 3) {
        // In the final scaling to \Delta\rho/\sqrt(\rho_{ref}) or \Delta\rho/\rho_{ref}
        // one might want to use a {d^2N\over d\eta d\phi} for that specific
        // charge combination. I always use the value appropriate for CI.

        // Note on combining centralites:
        //    Here we are taking ratios of sib/mix to cancel acceptance/efficiency before averaging.
        //    Combining centralities in this way will not give the same result as using a larger
        //    centrality bin to begin with, but may remove some of the systematic problems.

        double *d2NdEtadPhi = getd2NdEtadPhi(0);
        double dNdEtadPhi = d2NdEtadPhi[4];
        for (int iType=0;iType<4;iType++) {
            retVal[iType]->Scale(d2NdEtadPhi[iType]);
        }
        delete [] d2NdEtadPhi;

        for (int iz=1;iz<mNumZBins;iz++) {
            d2NdEtadPhi = getd2NdEtadPhi(iz);
            dNdEtadPhi += d2NdEtadPhi[4];
            TH2D** tmpVal= buildCommon(name, opt, sf, iz);
            for (int iType=0;iType<4;iType++) {
                retVal[iType]->Add(tmpVal[iType],d2NdEtadPhi[iType]);
                delete tmpVal[iType];
            }
            delete [] tmpVal;
            delete [] d2NdEtadPhi;
        }
        double sqrtRho = dNdEtadPhi / mNumZBins;
        for (int iType=0;iType<4;iType++) {
            if (0 < sqrtRho) {
                // Note: I can imagine cases where a bin is empty (or nearly so) so it should
                //       not count. Currently rely on some other QA to catch.
                retVal[iType]->Scale(1.0/mNumZBins);
                if (0 == opt) {
                    retVal[iType]->Scale(1.0/pow(sqrtRho,2));
                } else if (2 == opt) {
                    retVal[iType]->Scale(1.0/sqrtRho);
                }
            } else {
                retVal[iType]->Scale(0.0);
            }
        }
    } else {
        for (int iz=1;iz<mNumZBins;iz++) {
            TH2D** tmpVal= buildCommon(name, opt, sf, iz);
            for (int iType=0;iType<_pair_totalmax;iType++) {
                retVal[iType]->Add(tmpVal[iType]);
                delete tmpVal[iType];
            }
            delete [] tmpVal;
        }
        // Scale according to opt.
        for (int iType=0;iType<4;iType++) {
            retVal[iType]->Scale(1.0/mNumZBins);
            retVal[iType+4]->Scale(1.0/mNumZBins);
            retVal[iType]->Add(retVal[iType+4],-1);
            for (int ix=1;ix<=retVal[iType]->GetNbinsX();ix++) {
                for (int iy=1;iy<=retVal[iType]->GetNbinsY();iy++) {
                    double rhoRef = retVal[iType+4]->GetBinContent(ix,iy);
                    if (1 < rhoRef) {
                        double val = retVal[iType]->GetBinContent(ix,iy);
//                        double err = retVal[iType]->GetBinError(ix,iy);
                        if (3 == opt) {
                            retVal[iType]->SetBinContent(ix,iy,val/rhoRef);
//                            retVal[iType]->SetBinError(ix,iy,err/rhoRef);
                        } else if (5 == opt) {
                            retVal[iType]->SetBinContent(ix,iy,val/sqrt(rhoRef));
//                            retVal[iType]->SetBinError(ix,iy,err/sqrt(rhoRef));
                        }
                    }
                }
            }
        }
    }
    return retVal;
}
//---------------------------------------------------------
// Note that I (djp) modified the analysis code so that we only fill the -+
// histograms for identified, different particles. So we get -+ filled for
// pi-K+ as an example but not for unidentified particles (as in mode 1
// and mode 3) where we don't know what the particle types are.

// Ignore opt here, only use it for over-loading distinction.
// For opt < 3 return \Delta\rho/rho_{ref}
// For larger opt return rho and rho_{ref}.
TH2D** StEStructSupport::buildCommon(const char* name, int opt, float* sf, int zBin) {

    /* builds hist types = ++,+-,-+,-- */


    // eight input histograms ++,+-,-+,-- for Sib and Mix 
    TH2D** retVal = getLocalClones(name, zBin);
    if (!retVal) {
        return retVal;
    }

    if (mDoSymmetrize) {
        symmetrizeUS(name,retVal);
    }

    if (mnpairs) {  // manual scaling
        for(int i=0;i<8;i++) {
            retVal[i]->Scale(1.0/mnpairs[i]);
        }
    } else {
        rescale(retVal, zBin);  // Divide out bin size, number of events to approximate density.
                                // Optionally scale number of mix pairs to be same as sibling.
    }
    if (strstr(name,"DEta") || strstr(name,"SEta")) {
        fixDEta((TH2**)retVal,8); // does nothing unless mapplyDEtaFix is set
    }

    float scf1=0.;
    float scf2=0.;
    if (sf) {
        scf1=sf[0];
        scf2=sf[1];
    }
    // if requested, scale bg to require correlations>=0 where statistics are large
    // This is important for Yt-Yt correlations. When we return to study those we
    // better double check this stuff is reasonable.
    if (1==mbgMode) {
        scaleBackGround(retVal[0],retVal[4],scf1);
        scaleBackGround(retVal[1],retVal[5],scf2);
        scaleBackGround(retVal[2],retVal[6],scf2);
        scaleBackGround(retVal[3],retVal[7],scf1);
    }

    if (opt < 3) {
        // \Delta\rho/\rho_{ref}
        // Actually calculate \rho/\rho_{ref} - 1 so errors are correct.
        // Don't know a convenient way to subtract one from every bin!!!!
        for (int i=0;i<4;i++) {
            retVal[i]->Divide(retVal[i+4]);                        // rho/rho_{ref}
            for (int ix=1;ix<=retVal[i]->GetNbinsX();ix++) {
                for (int iy=1;iy<=retVal[i]->GetNbinsY();iy++) {
                    double val = retVal[i]->GetBinContent(ix,iy);
                    retVal[i]->SetBinContent(ix,iy,val-1);         // delta-rho/rho_{ref}
                }
            }
        }
    }

    return retVal;
}
//---------------------------------------------------------
// When we have imposed some type of pt cuts we may have introduced
// a covariance which will show up here. Subtract mixed pairs
// to reduce this contribution.

// opt: 0 = delta-rho/rho_mix;  1 = delta-rho;  2 = delta-rho/sqrt(rho_mix);
TH2D** StEStructSupport::buildPtCommon(const char* name, int opt, int subtract) {
    double *d2NdEtadPhi = getd2NdEtadPhi(0);
    double dNdEtadPhi = d2NdEtadPhi[4];
    TH2D** retVal= buildPtCommon(name, opt, subtract, 0);
    if (!retVal) {
        return retVal;
    }

    for (int iType=0;iType<4;iType++) {
        retVal[iType]->Scale(d2NdEtadPhi[iType]);
    }
    delete [] d2NdEtadPhi;

    // Fix up name and title of histograms we return
    for (int iType=0;iType<4;iType++) {
        if (strstr(retVal[iType]->GetName(),"_zBuf_0")) {
            retVal[iType]->SetName(swapIn(retVal[iType]->GetName(),"_zBuf_0",""));
        }
        if (strstr(retVal[iType]->GetTitle(),"_zBuf_0")) {
            retVal[iType]->SetTitle(swapIn(retVal[iType]->GetTitle(),"_zBuf_0",""));
        }
    }
    const char* oldName[4]  = {"Sibpp","Sibpm","Sibmp","Sibmm"};
    const char* newName[4]  = {"PP ","PM ","MP ","MM "};
    const char* oldTitle[4] = {"Sibling : +.+","Sibling : +.-","Sibling : -.+","Sibling : -.-"};
    const char* newTitle[4] = {"PP : ++ ","PM : +- ","MP: -+ ","MM : -- "};
    for (int i=0;i<4;i++) {
        retVal[i]->SetName(swapIn(retVal[i]->GetName(),oldName[i],newName[i]));
        retVal[i]->SetTitle(swapIn(retVal[i]->GetTitle(),oldTitle[i],newTitle[i]));
    }

    for (int iz=1;iz<mNumZBins;iz++) {
        d2NdEtadPhi = getd2NdEtadPhi(iz);
        dNdEtadPhi += d2NdEtadPhi[4];
        TH2D** tmpVal= buildPtCommon(name, opt, subtract, iz);
        for (int iType=0;iType<4;iType++) {
            retVal[iType]->Add(tmpVal[iType],d2NdEtadPhi[iType]);
            delete tmpVal[iType];
        }
        delete [] tmpVal;
        delete [] d2NdEtadPhi;
    }

    double sqrtRho = dNdEtadPhi/mNumZBins;
    for (int iType=0;iType<4;iType++) {
        if (0 < sqrtRho) {
            retVal[iType]->Scale(1.0/mNumZBins);
            if (0 == opt) {
                retVal[iType]->Scale(1.0/pow(sqrtRho,2));
            } else if (2 == opt) {
                retVal[iType]->Scale(1.0/sqrtRho);
            }
        } else {
            retVal[iType]->Scale(0.0);
        }
    }
    return retVal;
}
//---------------------------------------------------------
TH2D** StEStructSupport::buildPtCommon(const char* name, int opt, int subtract, int zBin) {

  /* builds hist types = ++,+-,-+,-- */

    if(!mtf) return (TH2D**)NULL;

    // -- here we get 32 hists: 
    //    4 groups of 8 (Sibpp,Sibpm,Sibmp,Sibmm,Mixpp,Mixpm,Mixmp,Mixmm)
    //    1st 8 are number, 2nd 8 are pt1*pt2, 3rd are pt1 and 4th are pt2

    TH2D** hlocal=getPtClones(name,zBin);
    if (!hlocal) {
        return hlocal;
    }

    if (mDoSymmetrize) {
        symmetrizePtUS(name,hlocal);
    }
    rescalePt(hlocal, zBin);  // Divide out bin size, number of events to approximate density.
                              // Optionally scale number of mix pairs to be same as sibling.

    // four returned hists
    TH2D** retVal= new TH2D*[4]; // 0=++ 1=+- 2=-+ 3=--
    TH2D** mixVal= new TH2D*[4]; // 0=++ 1=+- 2=-+ 3=--
    for (int i=0;i<4;i++) {
        retVal[i]=(TH2D*)hlocal[i]->Clone();// just get correct dimensions & names
        retVal[i]->Scale(0.); // zero the hists
        mixVal[i]=(TH2D*)hlocal[0]->Clone();
        mixVal[i]->Scale(0.); // zero the hists 
    }

    if(strstr(name,"DEta") || strstr(name,"SEta")) {
        fixDEta((TH2**)hlocal,32);
    }
    double *ptHat = getptHat(zBin);
    double ptHatA[4], ptHatB[4];
    ptHatA[0] = ptHat[0];
    ptHatA[1] = ptHat[0];
    ptHatA[2] = ptHat[1];
    ptHatA[3] = ptHat[1];
    ptHatB[0] = ptHat[2];
    ptHatB[1] = ptHat[2];
    ptHatB[2] = ptHat[3];
    ptHatB[3] = ptHat[3];
    delete [] ptHat;
    for (int i=0;i<4;i++) {  // Note that i=0->3 is ++, +-, -+, --
        retVal[i]->Add(hlocal[i+ 8]);
        retVal[i]->Add(hlocal[i+16],-ptHatB[i]);
        retVal[i]->Add(hlocal[i+24],-ptHatA[i]);
        retVal[i]->Add(hlocal[i   ],ptHatA[i]*ptHatB[i]);

        mixVal[i]->Add(hlocal[i+12]);
        mixVal[i]->Add(hlocal[i+20],-ptHatB[i]);
        mixVal[i]->Add(hlocal[i+28],-ptHatA[i]);
        mixVal[i]->Add(hlocal[i+ 4],ptHatA[i]*ptHatB[i]);
    }

    for (int i=0;i<4;i++) {
        retVal[i]->Divide(hlocal[i]);
        mixVal[i]->Divide(hlocal[i+4]);
        if (subtract) {
            retVal[i]->Add(mixVal[i],-1);  // Subtract mixed event artifacts
        }
    }



    // Free local histograms.
    for (int i=1;i<4;i++) {
        delete mixVal[i];
    }
    for(int i=0;i<_pair_totalmax*4;i++) {
        delete hlocal[i];
    }
    delete [] mixVal;
    delete [] hlocal;

    return retVal;
}

//---------------------------------------------------------
TH2D** StEStructSupport::buildChargeTypeRatios(const char* name, float* sf){
  return buildChargeTypes(name,0,sf);
}
//---------------------------------------------------------
TH2D** StEStructSupport::buildChargeTypeCFunctions(const char* name, float* sf){
  return buildChargeTypes(name,1,sf);
}
//---------------------------------------------------------
TH2D** StEStructSupport::buildChargeTypeRFunctions(const char* name, float* sf){
  return buildChargeTypes(name,2,sf);
}
//---------------------------------------------------------
// See comments before buildCommon about d2NdEtadPhi being turned into proxy for \rho_{ref}
// ChargeTypes ->  {"LS","US","CD","CI"}
// For opt=0 calculate \Delta\rho / rho_ref
//         1           \Delta\rho
//         2           \Delta\rh0 / sqrt(rho_ref)
// For YtYt we cannot use d2NdEtadPhi as proxy for \rho_{ref}.
// opt= 3, 4 and 5 combine \Delta\rho and \rho_{ref} directly.
//         3           \Delta\rho / rho_ref
//         4           \Delta\rho
//         5           \Delta\rh0 / sqrt(rho_ref)
TH2D** StEStructSupport::buildChargeTypes(const char* name, int opt, float *sf) {
    // Scale ++, +-, -+ and -- by their respective {d^2N \over d\eta d\phi}^2 to create
    // \Delta\rho. Scale by appropriate amount of {d^2N \over d\eta d\phi} for opt.
    // Finally add together to form LS, US, CD and CI.

    // 1/14/08 djp This routine did exactly the same thing as buildCommon up to the point
    //             of forming LS, US, CD and CI. Use that routine instead so we have fewer
    //             places for bugs.
    TH2D** retVal = buildCommon(name, opt, sf);
    if (!retVal) {
        return retVal;
    }

    // Fix up names and titles of histograms we will return.
    for (int iType=0;iType<4;iType++) {
        if (strstr(retVal[iType]->GetName(),"_zBuf_0")) {
            retVal[iType]->SetName(swapIn(retVal[iType]->GetName(),"_zBuf_0",""));
        }
        if (strstr(retVal[iType]->GetTitle(),"_zBuf_0")) {
            retVal[iType]->SetTitle(swapIn(retVal[iType]->GetTitle(),"_zBuf_0",""));
        }
    }
    const char* oldName[4]  = {"PP ","PM ","MP ","MM "};
    const char* newName[4]  = {"LS ","US ","CD ","CI "};
    const char* oldTitle[4] = {"PP : ++ ","PM : +- ","MP: -+ ","MM : -- "};
    const char* newTitle[4] = {"LS : ++ + -- ","US : +- + -+ ","CD: ++ + -- - +- - -+ ","CI : ++ + -- + +- + -+ "};
    for(int i=0;i<4;i++) {
        retVal[i]->SetName(swapIn(retVal[i]->GetName(),oldName[i],newName[i]));
        retVal[i]->SetTitle(swapIn(retVal[i]->GetTitle(),oldTitle[i],newTitle[i]));
    }

    // Now form LS, US, CD and CI
    retVal[0]->Add(retVal[3]);
    retVal[1]->Add(retVal[2]);
    retVal[2]->Add(retVal[0],retVal[1],1,-1);
    retVal[3]->Add(retVal[0],retVal[1]);

    return retVal;
}
//---------------------------------------------------------
TH2D** StEStructSupport::buildChargeTypeRatios(const char* name, float* sf, int zBin){
  return buildChargeTypes(name,0,sf,zBin);
}
//---------------------------------------------------------
TH2D** StEStructSupport::buildChargeTypeCFunctions(const char* name, float* sf, int zBin){
  return buildChargeTypes(name,1,sf,zBin);
}
//---------------------------------------------------------
TH2D** StEStructSupport::buildChargeTypeRFunctions(const char* name, float* sf, int zBin){
  return buildChargeTypes(name,2,sf,zBin);
}
//---------------------------------------------------------
// This is not used except by buildChargeTypeRatios, buildChargeTypeCFunctions and
// buildChargeTypeRFunctions. This might be useful as a check on zBuffer dependence.
TH2D** StEStructSupport::buildChargeTypes(const char* name, int opt, float *sf, int zBin) {
    // Scale ++, +-, -+ and -- by their respective {d^2N \over d\eta d\phi}^2 to create
    // \Delta\rho. Scale by appropriate amount of {d^2N \over d\eta d\phi} for opt.
    // Finally add together to form LS, US, CD and CI.

    TH2D** retVal= buildCommon(name, opt, sf, zBin);
    if (!retVal) {
        return retVal;
    }

    // Fix up names and titles of histograms we will return.
    for (int iType=0;iType<4;iType++) {
        if (strstr(retVal[iType]->GetName(),"_zBuf_0")) {
            retVal[iType]->SetName(swapIn(retVal[iType]->GetName(),"_zBuf_0",""));
        }
        if (strstr(retVal[iType]->GetTitle(),"_zBuf_0")) {
            retVal[iType]->SetTitle(swapIn(retVal[iType]->GetTitle(),"_zBuf_0",""));
        }
    }
    const char* oldName[4]  = {"Sibpp","Sibpm","Sibmp","Sibmm"};
    const char* newName[4]  = {"LS ","US ","CD ","CI "};
    const char* oldTitle[4] = {"Sibling : +.+","Sibling : +.-","Sibling : -.+","Sibling : -.-"};
    const char* newTitle[4] = {"LS : ++ + -- ","US : +- + -+ ","CD: ++ + -- - +- - -+ ","CI : ++ + -- + +- + -+ "};
    for(int i=0;i<4;i++) {
        retVal[i]->SetName(swapIn(retVal[i]->GetName(),oldName[i],newName[i]));
        retVal[i]->SetTitle(swapIn(retVal[i]->GetTitle(),oldTitle[i],newTitle[i]));
    }

    if (opt < 3) {
        // Have \Delta\rho/rho_{ref}.
        // Scale for desired option.
        double *d2NdEtadPhi = getd2NdEtadPhi(zBin);
        double sqrtRho = d2NdEtadPhi[4];
        delete [] d2NdEtadPhi;
        for (int iType=0;iType<4;iType++) {
            if (0 < sqrtRho) {
                if (1 == opt) {
                    retVal[iType]->Scale(pow(sqrtRho,2));
                } else if (2 == opt) {
                    retVal[iType]->Scale(sqrtRho);
                }
            } else {
                retVal[iType]->Scale(0.0);
            }
        }
    } else {
        // Scale according to opt.
        // Here we cannot use d2NdEtadPhi as proxy for rho_{ref}
        for (int iType=0;iType<4;iType++) {
            retVal[iType]->Add(retVal[iType+4],-1);
            for (int ix=1;ix<=retVal[iType]->GetNbinsX();ix++) {
                for (int iy=1;iy<=retVal[iType]->GetNbinsY();iy++) {
                    double rhoRef = retVal[iType+4]->GetBinContent(ix,iy);
                    if (1 < rhoRef) {
                        double val = retVal[iType]->GetBinContent(ix,iy);
//                        double err = retVal[iType]->GetBinError(ix,iy);
                        if (3 == opt) {
                            retVal[iType]->SetBinContent(ix,iy,val/rhoRef);
//                            retVal[iType]->SetBinError(ix,iy,err/rhoRef);
                        } else if (5 == opt) {
                            retVal[iType]->SetBinContent(ix,iy,val/sqrt(rhoRef));
//                            retVal[iType]->SetBinError(ix,iy,err/sqrt(rhoRef));
                        }
                    }
                }
            }
        }
    }

    // Now form LS, US, CD and CI
    retVal[0]->Add(retVal[3]);
    retVal[1]->Add(retVal[2]);
    retVal[2]->Add(retVal[0],retVal[1],1,-1);
    retVal[3]->Add(retVal[0],retVal[1]);

    return retVal;
}

//---------------------------------------------------------
TH2D** StEStructSupport::buildChargeTypesSumOfRatios(const char* name, int opt, float *sf){
    TH2D** retVal= buildChargeTypesSumOfRatios(name, opt, sf, 0);
    if (!retVal) {
        return retVal;
    }

    float *nPairs = getChargeNumber(0);
    for (int iType=0;iType<4;iType++) {
        retVal[iType]->Scale(nPairs[iType]);

    }
    for (int iType=0;iType<4;iType++) {
        if (strstr(retVal[iType]->GetName(),"_zBuf_0")) {
            retVal[iType]->SetName(swapIn(retVal[iType]->GetName(),"_zBuf_0",""));
        }
        if (strstr(retVal[iType]->GetTitle(),"_zBuf_0")) {
            retVal[iType]->SetTitle(swapIn(retVal[iType]->GetTitle(),"_zBuf_0",""));
        }
    }
    for (int iz=1;iz<mNumZBins;iz++) {
        TH2D** tmpVal= buildChargeTypesSumOfRatios(name, opt, sf, iz);
        float *tempPairs = getChargeNumber(iz);
        for (int iType=0;iType<4;iType++) {
            retVal[iType]->Add(tmpVal[iType],tempPairs[iType]);
            nPairs[iType]+=tempPairs[iType];
            delete tmpVal[iType];
        }
        delete [] tmpVal;
        delete [] tempPairs;
    }
    for (int iType=0;iType<4;iType++) {
        if (nPairs[iType]) {
            retVal[iType]->Scale(1.0/nPairs[iType]);
        } else {
            retVal[iType]->Scale(0.0);
        }
    }
    delete [] nPairs;
    return retVal;
}
//---------------------------------------------------------
TH2D** StEStructSupport::buildChargeTypesSumOfRatios(const char* name, int opt, float* sf, int zBin){
  // finds LS and US same as buildChargeTypes, but here CI and CD are sums of ratios

  // build hist types = LS, US, CD, CI
  // eight input histograms ++,+-,-+,-- for Sib and Mix
  TH2D** hlocal=getLocalClones(name,zBin);
  if (!hlocal) {
      return hlocal;
  }

  if(mnpairs){  // manual scaling
    for(int i=0;i<8;i++) hlocal[i]->Scale(1.0/mnpairs[i]);
  } else rescale(hlocal,zBin);  // automatic scaling, norm sib to pairs per event, norm mix to sib

  if(strstr(name,"DEta") || strstr(name,"SEta"))fixDEta((TH2**)hlocal,8); // does nothing unless mapplyDEtaFix is set

  // four returned hists
  TH2D** retVal= new TH2D*[4]; // 0=LS 1=US 2=CD=LS-US 3=CI=LS+US
  const char* oldName[4]={"Sibpp","Sibpm","Sibmp","Sibmm"};
  const char* newName[4]={"LS ","US ","CD ","CI "};
  const char* oldTitle[4]={"Sibling : +.+","Sibling : +.-","Sibling : -.+","Sibling : -.-"};
  const char* newTitle[4]={"LS : ++ + -- ","US : +- + -+ ","CD: ++ + -- - +- - -+ ","CI : ++ + -- + +- + -+ "};
  for(int i=0;i<4;i++){
    retVal[i]=(TH2D*)hlocal[i]->Clone();
    retVal[i]->SetName(swapIn(hlocal[i]->GetName(),oldName[i],newName[i]));
    retVal[i]->SetTitle(swapIn(hlocal[i]->GetTitle(),oldTitle[i],newTitle[i]));
    retVal[i]->Scale(0.); // zero the hists
  }

  hlocal[0]->Add(hlocal[3]);                // ++ + -- Sib
  if(hlocal[2]) hlocal[1]->Add(hlocal[2]);  // +- + -+ Sib, backward compatible
  hlocal[4]->Add(hlocal[7]);                // ++ + -- Mix
  if(hlocal[6]) hlocal[5]->Add(hlocal[6]);  // +- + -+ Mix

  float scf1=0.;
  float scf2=0.;
  if(sf){
    scf1=sf[0];
    scf2=sf[1];
  }
  // if requested, scale bg to require correlations>=0 where statistics are large
  if(1==mbgMode){
    scaleBackGround(hlocal[0],hlocal[4],scf1);
    scaleBackGround(hlocal[1],hlocal[5],scf2);
    if(opt==3)scaleBackGround(hlocal[3],hlocal[7]);
  }

  retVal[0]->Add(hlocal[0]);  // LS sib
  retVal[1]->Add(hlocal[1]);  // US sib

  // opt: 0 = delta-rho/rho_mix;  1 = delta-rho;  2 = delta-rho/sqrt(rho_mix)

  for(int i=0;i<2;i++){  // loop over LS and US
    retVal[i]->Add(hlocal[i+4],-1.0);  // delta-rho
  }
  retVal[2]->Add(retVal[0],retVal[1],1.,-1.);  // CD sib
  retVal[3]->Add(retVal[0],retVal[1],1.,1.);   // CI sib

  if(opt!=1) {  // retVal has LS, US, CD, CI sib; use hlocal to take ratios
    hlocal[6]->Add(hlocal[4], hlocal[5]);
    hlocal[7]->Add(hlocal[4], hlocal[5]);  // hlocal[4] starts mix for denominators

    for(int i=0;i<4;i++) {  //2
      retVal[i]->Divide(hlocal[i+4]);  // delta-rho/rho_mix
      // NOTE: now CI = (LS_sib + US_sib) / (LS_mix + US_mix), not sum of ratios

      if(opt>=2){    // dN/d\eta * (delta-rho/rho_mix)
        TString hSibName("NEventsSib_zBuf_"); hSibName += zBin;
        TH1 *hNum;  mtf->GetObject(hSibName.Data(),hNum);
        double dNdeta = hNum->GetMean()/2;
        retVal[i]->Scale(dNdeta/(2*3.1415926));
      }
    }
  }

  // Free local histograms.
  for(int i=0;i<_pair_totalmax;i++) {
    delete hlocal[i];
  }
  delete hlocal;

  return retVal;
}
//---------------------------------------------------------
// When we have imposed some type of pt cuts we may have introduced
// a covariance which will show up here. Subtract mixed pairs
// to reduce this contribution.
TH2D** StEStructSupport::buildPtChargeTypes(const char* name, int opt, int subtract){
    // See comments in buildChargeTypes.
    TH2D** retVal= buildPtCommon(name, opt, subtract);
    if (!retVal) {
        return retVal;
    }

    // Fix up name and title of histograms we return
    for (int iType=0;iType<4;iType++) {
        if (strstr(retVal[iType]->GetName(),"_zBuf_0")) {
            retVal[iType]->SetName(swapIn(retVal[iType]->GetName(),"_zBuf_0",""));
        }
        if (strstr(retVal[iType]->GetTitle(),"_zBuf_0")) {
            retVal[iType]->SetTitle(swapIn(retVal[iType]->GetTitle(),"_zBuf_0",""));
        }
    }
    const char* oldName[4]  = {"PP ","PM ","MP ","MM "};
    const char* newName[4]  = {"LS ","US ","CD ","CI "};
    const char* oldTitle[4] = {"PP : ++ ","PM : +- ","MP: -+ ","MM : -- "};
    const char* newTitle[4] = {"LS : ++ + --  ","US : +- + -+  ","CD: ++ + -- - +- - -+  ","CI : ++ + -- + +- + -+  "};
    for (int i=0;i<4;i++) {
        retVal[i]->SetName(swapIn(retVal[i]->GetName(),oldName[i],newName[i]));
        retVal[i]->SetTitle(swapIn(retVal[i]->GetTitle(),oldTitle[i],newTitle[i]));
    }

    // Form charge combinations.
    retVal[0]->Add(retVal[3]);
    retVal[1]->Add(retVal[2]);
    retVal[2]->Add(retVal[0],retVal[1],1,-1);
    retVal[3]->Add(retVal[0],retVal[1]);
    return retVal;
}
//---------------------------------------------------------
// This routine is not used except for checking zBuffer dependence.
TH2D** StEStructSupport::buildPtChargeTypes(const char* name, int opt, int subtract, int zBin) {

    TH2D** retVal= buildPtCommon(name, opt, subtract, zBin);
    if (!retVal) {
        return retVal;
    }

    // Fix up name and title of histograms we return
    const char* oldName[4]  = {"Sibpp","Sibpm","Sibmp","Sibmm"};
    const char* newName[4]  = {"LS ","US ","CD ","CI "};
    const char* oldTitle[4] = {"Sibling : +.+","Sibling : +.-","Sibling : -.+","Sibling : -.-"};
    const char* newTitle[4] = {"LS : ++ + --  ","US : +- + -+  ","CD: ++ + -- - +- - -+  ","CI : ++ + -- + +- + -+  "};
    for(int i=0;i<4;i++){
        retVal[i]->SetName(swapIn(retVal[i]->GetName(),oldName[i],newName[i]));
        retVal[i]->SetTitle(swapIn(retVal[i]->GetTitle(),oldTitle[i],newTitle[i]));
    }

    // Form charge combinations.
    retVal[0]->Add(retVal[3]);
    retVal[1]->Add(retVal[2]);
    retVal[2]->Add(retVal[0],retVal[1],1,-1);
    retVal[3]->Add(retVal[0],retVal[1]);
    return retVal;
}

//---------------------------------------------------------
// djp I don't understand the logic here. If any mixed bin exceeds the sibling
//     bin by three sigma and is at least 45% of the maximum sibling bin then
//     we scale entire mixed histogram by sibling/mixed for this one bin.
//     And if it happens for more than one bin we use the last one!
//
//     Finally noticed, when we explicitly give a scale factor we use that.
void StEStructSupport::scaleBackGround(TH2D* sib, TH2D* mix, float sf) {
    float alpha=1.0;
    if (sf!=0.) {
        alpha=sf;
    } else {
        float hmax = mix->GetMaximum();
        for (int ix=1;ix<=mix->GetNbinsX();ix++) {
            for(int iy=1;iy<=mix->GetNbinsY();iy++) {
                float mval = mix->GetBinContent(ix,iy);
                if (mval>0.) {
                    float sval = sib->GetBinContent(ix,iy);
                    if (sval==0.) {
                        continue;
                    }
                    float eval = sib->GetBinError(ix,iy)/sval;
                    float dval = fabs(sval-mval)/sval;
                    if (sval<mval && dval>3.0*eval && mval/hmax>0.45) {
//                        if (sval/hmax>0.5) {
                        float rval = sval/mval;
                        if (rval<alpha) {
                            alpha = rval;
                        }
                    }
                }
            }
        }
    }
    mix->Scale(alpha);
    if (!msilent) {
        cout<<"Scaling "<<mix->GetName()<<" by "<<alpha<<endl;
    }
}

//----------------------------------------------------------------
void StEStructSupport::fixDEta(TH2** h, int numHists) {

  // Expect histograms in groups of eight. The first eight are number,
  // with the first four being sibling and the second four being
  // mixed. If there are more than eight they will be pt weighted.
  // I find acceptance in the pair hole region is different for ++ and
  // +-, so we create two acceptance histograms, ++ + -- and +- + -+
  // using the mixed number. Not sure how to normalize the acceptance
  // histograms. For now try setting acceptance for \eta_\Delta = 0
  // to 1, excluding the pair cut hole.

    if(!mapplyDEtaFix) return;

    TH2D *acc[2];
    acc[0] = (TH2D *) h[4]->Clone();
    acc[0]->Add(h[7]);
    acc[1] = (TH2D *) h[5]->Clone();
    acc[1]->Add(h[6]);
    int n = acc[0]->GetNbinsX();
    double scale[] = {0, 0};
    if (n%2) {
        int mid = 1 + n/2;
        for (int iy=12;iy<=25;iy++) {
            scale[0] += acc[0]->GetBinContent(mid,iy);
            scale[1] += acc[1]->GetBinContent(mid,iy);
        }
        scale[0] /= 14;
        scale[1] /= 14;
    } else {
        int mid = n/2;
        for (int iy=12;iy<=25;iy++) {
            scale[0] += acc[0]->GetBinContent(mid,iy);
            scale[0] += acc[0]->GetBinContent(mid+1,iy);
            scale[1] += acc[1]->GetBinContent(mid,iy);
            scale[1] += acc[1]->GetBinContent(mid+1,iy);
        }
        scale[0] /= 28;
        scale[1] /= 28;
    }
    if (scale[0] > 0) {
        acc[0]->Scale(1.0/scale[0]);
    }
    if (scale[1] > 0) {
        acc[1]->Scale(1.0/scale[1]);
    }

    for (int ig=0;ig<numHists;ig+=8) {
        if (acc[0]->Integral() > 0) {
            h[ig+0]->Divide(acc[0]);
            h[ig+1]->Divide(acc[1]);
            h[ig+2]->Divide(acc[1]);
            h[ig+3]->Divide(acc[0]);
            h[ig+4]->Divide(acc[0]);
            h[ig+5]->Divide(acc[1]);
            h[ig+6]->Divide(acc[1]);
            h[ig+7]->Divide(acc[0]);
        }
    }
  delete acc[0];
  delete acc[1];
}

//---------------------------------------------------------
void StEStructSupport::writeAscii(TH2D** h, int iHist, const char* fname, int optErrors){

  ofstream of(fname);
   int xbins=h[iHist]->GetNbinsX();
   TAxis * xa= h[iHist]->GetXaxis();
   of<<"# Histogram Name = "<<h[iHist]->GetName()<<endl;
   of<<"# X-axis: min="<<xa->GetBinLowEdge(1)<<" max="<<xa->GetBinUpEdge(xbins)<<" nbins="<<xbins<<endl;
 
   int ybins=h[iHist]->GetNbinsY();
   TAxis * ya= h[iHist]->GetYaxis();
   if(ybins>1){
     of<<"# Y-axis: min="<<ya->GetBinLowEdge(1)<<" max="<<ya->GetBinUpEdge(ybins)<<" nbins="<<ybins<<endl;
     of<<"# ix  iy  Value "<<endl; 
     for(int i=1;i<=xbins;i++){
       for(int j=1;j<=ybins;j++){
         of<<i<<"   "<<j<<"   "<<h[iHist]->GetBinContent(i,j);
         if(optErrors>0)of<<"  "<<h[iHist]->GetBinError(i,j);
         of<<endl;
       }
     }
     of<<endl;
   } else {
     of<<"# ix Value "<<endl;
     for(int i=1;i<=xbins;i++){
         of<<i<<"   "<<h[iHist]->GetBinContent(i);
         if(optErrors>0)of<<"  "<<h[iHist]->GetBinError(i);
         of<<endl;
     }
     of<<endl;
   }

  of.close();
}

//---------------------------------------------------------
char* StEStructSupport::swapIn(const char* name, const char* s1, const char* s2){

  // looks for s1 in name and replaces with s2
  //
  //  in perl    $name=~s/$s1/$s2/;

  if(mtmpString) delete [] mtmpString;
  mtmpString=NULL;

  if(!name) return mtmpString;

  int len=strlen(name);
  char* tmp=new char[len+1];
  strcpy(tmp,name);

  char* ptr1;
  if(!(ptr1=strstr(tmp,s1))) {
    delete [] tmp;
    return mtmpString;
  }

  int len1=strlen(s1);
  mtmpString=new char[256];

  ostringstream ts;
  char* ptr2=ptr1;
  *ptr1='\0';
  ts<<tmp; 
  if(s2)ts<<s2;
  ptr1+=len1;
  ts<<ptr1;
  *ptr2=' ';

  delete [] tmp;
  strcpy(mtmpString,(ts.str()).c_str());
  return mtmpString;
}

/***********************************************************************
 *
 * $Log: StEStructSupport.cxx,v $
 * Revision 1.21  2009/05/08 00:21:42  prindle
 * In StEStructHadd remove support for old style of histogram names, do a better job calculating
 * errors (at least for number (\eta_\Delta,\phi_\Delta) histograms), double bins which
 * have an edge in the center (purely cosmetic when looking at intermediate histograms).
 * In StEStructSupport check for existance of histograms and return gracefully.
 * Code in buildChargeTypes and buildPtChargeTypes was essentially duplicate of code
 * in buildCommon and buildPtCommon so I refactored to reduce redundancy.
 *
 * Revision 1.20  2009/02/03 14:30:23  fisyak
 * Add missing includes for ROOT 5.22
 *
 * Revision 1.19  2008/12/02 23:52:53  prindle
 * Get information about histogram XX being symmetrized from CutBin.
 * Changed TH1* to TH2D* in many places hoping to be able to plot DEtaDPhi
 * as colz (doesn't work yet).
 * Added direct calculation of \Delta\rho/\rho_{ref} (and  similar) which is
 * needed for YtYt correlations.
 *
 * Revision 1.18  2008/03/19 22:08:38  prindle
 * Use GetObject instead of Get for type safety. Stop deleting objects we didn't create.
 * Treat \Delta\rho = d^2n/dEtadphi (rho - rho_ref)/rho_ref as basic unit when combining
 * centralities and z bins.
 *
 * This code should be check in more detail before being completely relied upon.
 *
 * Revision 1.17  2007/11/26 20:07:19  prindle
 * Modified to average \Delta\rho/sqrt(\rho) over z-bins (if more than one z-bin
 * present for given centrality. Note: I weight by number of tracks, not number of
 * pairs. This is important when we are also combining different centralities (which
 * I do by combining centrality tag with z-bin tag in macro/addCentralities.)
 *
 * Scale mixed histograms by number of events. Integral of \Delta\rho need not be 0.
 *
 * delete items that are created and valgrind complained were lost. (Not a big deal
 * since macro is run once)
 *
 * [Still need to commit StEStructHAdd.cxx which cvs complained that check-update failed.]
 *
 * Revision 1.16  2007/05/27 22:46:01  msd
 * Added buildChargeTypes mode 3 which takes rho_ref from track counts.
 * Added buildChargeTypeSumOfRatios.
 *
 * Revision 1.15  2007/01/26 17:20:58  msd
 * Updated HAdd for new binning scheme.
 * Improved Support::buildChargeTypes.
 *
 * Revision 1.14  2006/12/14 20:07:11  prindle
 *   I was calculating \Delta\rho/sqrt(rho) for ++, +-, -+ and --
 * and then combining those into LS, US, CD and CI. The was wrong
 * and now I am doing it correctly. For CI this makes only a slight
 * change, it seems the amplitude is decreased a little. For CD
 * this is a bigger change. I left the old versions (with _Old appended)
 * for now.
 *
<<<<<<< StEStructSupport.cxx
=======
 * Revision 1.13  2006/10/27 00:05:32  prindle
 *   Modified buildChargeTypes to handle case where the -+ histogram is
 * empty. Also tried making buildCommonTypes work, but there one of the
 * output histograms was intended to be -+ and most of the time that
 * will be empty.
 *
>>>>>>> 1.13
 * Revision 1.11  2006/04/26 18:52:12  dkettler
 *
 * Added reaction plane determination for the analysis
 *
 * Added reaction plane angle calculation
 *
 * Case 3 in buildPtChargeTypes needs to be corrected
 *
 * Revision 1.10  2006/04/25 21:04:39  msd
 * Added Jeff's patch for getHists to create doubles instead of floats
 *
 * Revision 1.9  2006/04/06 01:09:49  prindle
 *   Calculating pt for each cut bin caused changes in HAdd.
 * The splitting of +- into +- and -+ caused changes in Support.
 *
 * Revision 1.8  2006/04/04 22:14:10  porter
 * fixdeta is now NOT default but included in StEStruct2ptCorrelations
 *
 * Revision 1.7  2005/09/14 17:25:37  msd
 * minor tweak
 *
 * Revision 1.6  2005/09/07 20:26:16  prindle
 *
 *
 *     Support: Fixed some meory leaks.
 *
 * Revision 1.4  2005/03/08 21:56:42  porter
 * fixed bug in StEStructHAdd.cxx and added diagnostic option in ptcorrelations to
 * view individual terms separately
 *
 * Revision 1.3  2005/03/08 20:16:34  msd
 * included <sstream> library
 *
 * Revision 1.2  2005/03/03 01:33:05  porter
 * Added pt-correlations method to support and included
 * these histograms to the HAdd routine
 *
 * Revision 1.1  2004/07/01 00:37:17  porter
 * new code previously my StEStructHelper. Takes hists from correltation
 * pass and builds final ressults.  Also the StEStructHAdd.h is a simple
 * replacemnt for my sumyt.C macro which could be expanded later as needed.
 *
 *
 *
 *********************************************************************/


