/**********************************************************************
 *
 * $Id: StEStructSupport.cxx,v 1.30 2012/11/16 21:27:23 prindle Exp $
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
  std::ostringstream ts;
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
  mPairWeighting = true;
  mIdenticalPair = true;
  mYtYtNormalization = false;
  mYtYtVolumeNormalization = false;

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
// We have been calculating \Delta\rho/rho in different multiplicity/z bins
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
  /* 09/27/12 djp Now taking eta range from Cuts file. Had been interrogating
   *              binning class for detaMax. Extract from special purpose histogram instead.
   */
double StEStructSupport::getCIdNdEtadPhi() {
    // A == sum over zBins  {nA+} + {nA-}
    // B == sum over zBins  {nB+} + {nB-}
    // return 0.5 sqrt(A*B) / nEvents

    double retVal = 0;
    TH1 *hNSum;
    TString hSibName;

    double nEvents = 0;
    double nTracksA = 0;
    double nTracksB = 0;
    TH1 *hEta;
    TString hname;
    for (int iz=0;iz<mNumZBins;iz++) {
        hSibName = "NEventsSib_zBuf_"; hSibName += iz;  mtf->GetObject(hSibName.Data(),hNSum);  nEvents += hNSum->Integral();

        hname = "etaPA_zBuf_";  hname += iz;  mtf->GetObject(hname.Data(),hEta);  nTracksA += hEta->Integral();
        hname = "etaMA_zBuf_";  hname += iz;  mtf->GetObject(hname.Data(),hEta);  nTracksA += hEta->Integral();
        hname = "etaPB_zBuf_";  hname += iz;  mtf->GetObject(hname.Data(),hEta);  nTracksB += hEta->Integral();
        hname = "etaMB_zBuf_";  hname += iz;  mtf->GetObject(hname.Data(),hEta);  nTracksB += hEta->Integral();
    }

    // The 0.5 is because we integrate tracks over two units of rapidity.
    TH2D *hEtaPhi;
    mtf->GetObject("EtaPhiRange",hEtaPhi);
    double etaMin = -1;
    double etaMax = +1;
    if (hEtaPhi) {
        TAxis *x = hEtaPhi->GetXaxis();
        etaMin = x->GetXmin();
        etaMax = x->GetXmax();
    }
    retVal = sqrt( nTracksA * nTracksB ) / (2*3.1415926 * nEvents * (etaMax-etaMin) );
    return retVal;
}
//---------------------------------------------------------
double *StEStructSupport::getd2NdEtadPhi(int zBin, bool include2s) {
    double* retVal = new double[6];
    // (nA+)*(nB+), (nA+)*(nB-), (nA-)*(nB+), (nA-)*(nB-), (nA+)+(nA-)+(nB+)+(nB-)
    // where the () means {d^2() \over d\eta d\phi}
    // Note that in cases where we don't distinguish a and b
    // (nA+)*(nB-) and (nA-)*(nB+) are the same and retVal[2] is not necessary.

    
    TString hSibName("NEventsSib_zBuf_"); hSibName += zBin;  TH1* hNSum;  mtf->GetObject(hSibName.Data(),hNSum);
    double nEvents = hNSum->Integral();
    retVal[5] = nEvents;

    double nTracksAP, nTracksAM, nTracksBP, nTracksBM;
    double dNdEtaAP, dNdEtaAM, dNdEtaBP, dNdEtaBM;
    TH1 *hEta;
    TString hname;

    hname = "etaPA_zBuf_";  hname += zBin;  mtf->GetObject(hname.Data(),hEta);  nTracksAP = hEta->Integral();
    hname = "etaMA_zBuf_";  hname += zBin;  mtf->GetObject(hname.Data(),hEta);  nTracksAM = hEta->Integral();
    hname = "etaPB_zBuf_";  hname += zBin;  mtf->GetObject(hname.Data(),hEta);  nTracksBP = hEta->Integral();
    hname = "etaMB_zBuf_";  hname += zBin;  mtf->GetObject(hname.Data(),hEta);  nTracksBM = hEta->Integral();

    // The 0.5 is because we integrate tracks over two units of rapidity.
    TH2D *hEtaPhi;
    mtf->GetObject("EtaPhiRange",hEtaPhi);
    double etaMin = -1;
    double etaMax = +1;
    if (hEtaPhi) {
        TAxis *x = hEtaPhi->GetXaxis();
        etaMin = x->GetXmin();
        etaMax = x->GetXmax();
    }

    dNdEtaAP = nTracksAP / (nEvents * (etaMax-etaMin) );
    dNdEtaAM = nTracksAM / (nEvents * (etaMax-etaMin) );
    dNdEtaBP = nTracksBP / (nEvents * (etaMax-etaMin) );
    dNdEtaBM = nTracksBM / (nEvents * (etaMax-etaMin) );
    retVal[0] = dNdEtaAP*dNdEtaBP / pow(2*3.1415926,2);
    retVal[1] = dNdEtaAP*dNdEtaBM / pow(2*3.1415926,2);
    retVal[2] = dNdEtaAM*dNdEtaBP / pow(2*3.1415926,2);
    retVal[3] = dNdEtaAM*dNdEtaBM / pow(2*3.1415926,2);

    // There are factors of 2 when expanding the full correlation term that
    // could be included in loops over pairs, but appear not to have been.
    // Fix that up here.
    if(include2s) {
        if (mIdenticalPair) {
            retVal[4] = (dNdEtaAP + dNdEtaAM) / (2*3.1415926);
        } else {
            retVal[0] *= 2;
            retVal[2] *= 2;
            retVal[3] *= 2;
            retVal[4] = (dNdEtaAP + dNdEtaAM + dNdEtaBP + dNdEtaBM) / (2*3.1415926);
        }
    }

    return retVal;
}
//---------------------------------------------------------
// These are the values of d2N/dEtadPhi used when combining \Delta\rho/\rho_{ref}
// First four values are product  A+B+, A+B-, A-B+, A-A-.
// Fifth value is A+ + A- + B+ + B- (or one have of that, look in getd2NdEtadPhi).
double *StEStructSupport::getScaleFactors() {
    double* retVal = new double[6];
    for (int iType=0;iType<6;iType++) {
        retVal[iType] = 0;
    }

    double *d2NdEtadPhi;
    for (int iz=0;iz<mNumZBins;iz++) {
        d2NdEtadPhi = getd2NdEtadPhi(iz);
        for (int iType=0;iType<5;iType++) {
            retVal[iType] += d2NdEtadPhi[iType] * d2NdEtadPhi[5];
        }
        retVal[5] += d2NdEtadPhi[5];
        delete [] d2NdEtadPhi;
    }
    for (int iType=0;iType<5;iType++) {
        retVal[iType] /= retVal[5];
    }
    return retVal;
}
double *StEStructSupport::getScaleFactors(int zBin) {
    return getd2NdEtadPhi(zBin);
}
//---------------------------------------------------------
double *StEStructSupport::getptHat() {
    double* retVal = new double[4];
    double d2N_Tot = 0;
    double *d2NdEtadPhi;
    for (int iType=0;iType<4;iType++) {
        retVal[iType] = 0;
    }
    double *ptHat;
    for (int iz=0;iz<mNumZBins;iz++) {
        ptHat = getptHat(iz);
        d2NdEtadPhi = getd2NdEtadPhi(iz);
        for (int iType=0;iType<4;iType++) {
            retVal[iType] += ptHat[iType] * d2NdEtadPhi[5];
        }
        d2N_Tot += d2NdEtadPhi[5];
        delete [] d2NdEtadPhi;
        delete [] ptHat;
    }
    for (int iType=0;iType<4;iType++) {
        retVal[iType] /= d2N_Tot;
    }
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
void StEStructSupport::rescale(TH2D** hists) {
    // Divide hists by bin widths, divide by Nevents.
    // Bin by bin errors should be scaled properly, but won't include errors on the scale factors.

    if(!mtf) return;

    //>>>>>Need to rethink how to handle old cases where _zBuf_n is not in histogram name!!!!!
    TH1 *hNum;
    double nSib = 0;
    double nMix = 0;
    for (int zBin=0;zBin<mNumZBins;zBin++) {
        TString hSibName("NEventsSib_zBuf_");  hSibName += zBin;  mtf->GetObject(hSibName.Data(),hNum);
        nSib += hNum->Integral();
        TString hMixName("NEventsMix_zBuf_");  hMixName += zBin;  mtf->GetObject(hMixName.Data(),hNum);
        nMix += hNum->Integral();
    }

    for (int i=0;i<4;i++) {
//        cout << "\ni=" << i << " Integral: " << hists[i]->Integral();
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
                // This is original normalization. Average value of \rho_{sib}/\rho_{ref} should be one.
                TH2D *tmp = (TH2D *) hists[i]->Clone();
                tmp->Divide(hists[i+4]);
                double scale = tmp->Integral() / (tmp->GetNbinsX() * tmp->GetNbinsY());
                hists[i+4]->Scale(scale);
                delete tmp;
            } else if (mYtYtNormalization) {
                // Scale so hists[i+4]->Integral() = hists[i]->Integral()
                // Seems that bins with small number of counts (the case for large Yt in YtYt plots)
                // can skew the scaling when we scale as above.
                hists[i+4]->Scale(hists[i]->Integral()/hists[i+4]->Integral());
            } else if (mYtYtVolumeNormalization) {
                // Scale so integral of \Delta\rho/sqrt(\rho) = 0;
                TH2D *tmpA = (TH2D *) hists[i]->Clone();
                tmpA->Reset();
                TH2D *tmpB = (TH2D *) hists[i]->Clone();
                tmpB->Reset();
                for (int ix=1;ix<=tmpA->GetNbinsX();ix++) {
                    for (int iy=1;iy<=tmpA->GetNbinsY();iy++) {
                        double valS = hists[i]->GetBinContent(ix,iy);
                        double valR = hists[i+4]->GetBinContent(ix,iy);
                        if (valR > 0) {
                            tmpA->SetBinContent(ix,iy,valS/sqrt(valR));
                        } else {
                            tmpA->SetBinContent(ix,iy,0);
                        }
                        tmpB->SetBinContent(ix,iy,sqrt(valR));
                    }
                }
                double num = tmpA->Integral();
                double den = tmpB->Integral();
                if (den > 0) {
                    hists[i+4]->Scale(num/den);
                } else {
                    hists[i+4]->Scale(0);
                }
                delete tmpA;
                delete tmpB;
            } else {
                // We know how many sibling and mixed events there were.
                // In this normalization we should be able to integrate (weighted with an
                //  appropriate kernel) to get fluctuations at larger scales.
                if (nMix > 0) {
                    // For sibling the number of pairs are;
                    //     n(n-1)/2  for ++ and --
                    //     n^+ * n^- for +- (and -+)
                    // For mixed the number of pairs are
                    //     n^(1) * n^(2)                       for ++ and --
                    //     n^(+1) * n^(-1) + n^(-1) * n^(+2)   for +-
                    // Thus we have about twice as many pairs per mixed event as sibling event.
                    hists[i+4]->Scale(0.5/(nMix*binFactor));
                } else {
                    hists[i+4]->Scale(0);
                }
            }
        }
    }
}

//-----------------------------------------------------
void StEStructSupport::rescale(TH2D** hists, int zBin) {
    // Divide hists by bin widths, divide by Nevents.
    // Bin by bin errors should be scaled properly, but won't include errors on the scale factors.

    if(!mtf) return;

    //>>>>>Need to rethink how to handle old cases where _zBuf_n is not in histogram name!!!!!
    TH1 *hNum;
    TString hSibName("NEventsSib_zBuf_");  hSibName += zBin;  mtf->GetObject(hSibName.Data(),hNum);
    double nSib = hNum->Integral();
    TString hMixName("NEventsMix_zBuf_");  hMixName += zBin;  mtf->GetObject(hMixName.Data(),hNum);
    double nMix = hNum->Integral();

    for (int i=0;i<4;i++) {
//        cout << "\ni=" << i << " Integral: " << hists[i]->Integral();
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
                // This is original normalization. Average value of \rho_{sib}/\rho_{ref} should be one.
                TH2D *tmp = (TH2D *) hists[i]->Clone();
                tmp->Divide(hists[i+4]);
                double scale = tmp->Integral() / (tmp->GetNbinsX() * tmp->GetNbinsY());
                hists[i+4]->Scale(scale);
                delete tmp;
            } else if (mYtYtNormalization) {
                // Scale so hists[i+4]->Integral() = hists[i]->Integral()
                // Seems that bins with small number of counts (the case for large Yt in YtYt plots)
                // can skew the scaling when we scale as above.
                hists[i+4]->Scale(hists[i]->Integral()/hists[i+4]->Integral());
            } else if (mYtYtVolumeNormalization) {
                // Scale so integral of \Delta\rho/sqrt(\rho) = 0;
                TH2D *tmpA = (TH2D *) hists[i]->Clone();
                tmpA->Reset();
                TH2D *tmpB = (TH2D *) hists[i]->Clone();
                tmpB->Reset();
                for (int ix=1;ix<=tmpA->GetNbinsX();ix++) {
                    for (int iy=1;iy<=tmpA->GetNbinsY();iy++) {
                        double valS = hists[i]->GetBinContent(ix,iy);
                        double valR = hists[i+4]->GetBinContent(ix,iy);
                        if (valR > 0) {
                            tmpA->SetBinContent(ix,iy,valS/sqrt(valR));
                        } else {
                            tmpA->SetBinContent(ix,iy,0);
                        }
                        tmpB->SetBinContent(ix,iy,sqrt(valR));
                    }
                }
                double num = tmpA->Integral();
                double den = tmpB->Integral();
                if (den > 0) {
                    hists[i+4]->Scale(num/den);
                } else {
                    hists[i+4]->Scale(0);
                }
                delete tmpA;
                delete tmpB;
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
    // Adjust errors on Pr, Pa and Pb. Current error is sqrt(bin content).
    // Error on p_t is roughly \sigma_{p_t}  = 0.015 * p_t
    // We kind of interchange sums of squares and squares of sums to get following.
    double n, pa, pb, er, ea, eb;
    for (int ix=1;ix<=retVal[i+8]->GetNbinsX();ix++) {
        for (int iy=1;iy<=retVal[i+8]->GetNbinsY();iy++) {
            // Adjust sibling histograms for p_t uncertainties
            // First four groups of retVal are ++, +-, -+ and -- for sibling.
            // Second four are for mixed.
            n = retVal[i]->GetBinContent(ix,iy);
            pa = retVal[i+16]->GetBinContent(ix,iy);
            pb = retVal[i+24]->GetBinContent(ix,iy);
            er = (0.015*pa*pb/n)*sqrt(2/n);
            ea = (0.015*pa)/sqrt(n);
            eb = (0.015*pb)/sqrt(n);
            retVal[i+8]->SetBinError(ix,iy,er);
            retVal[i+16]->SetBinError(ix,iy,ea);
            retVal[i+24]->SetBinError(ix,iy,eb);
        }
    }
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
        double dx = (hists[i]->GetXaxis())->GetBinWidth(1);
        double dy = (hists[i]->GetYaxis())->GetBinWidth(1);
        double binFactor = dx*dy;
        double nSibPairs = hists[i]->Integral();
        double nMixPairs = hists[i+4]->Integral();
        double norm;
        if (nMixPairs > 0) {
            norm = nSibPairs/(nMixPairs*nSib*binFactor);
        } else {
            norm = 0;
        }
        for (int j=0;j<4;j++) {
            if (hists[i+4*j]->Integral() > 0) {
                // divinding by ex*ey converts all input hists from counts to densities, so all operations and final result should also be density.
                if(i==0 && !msilent) cout << "Scaling with Nevents " << nSib << " and binFactor " << binFactor << endl;
                hists[i+8*j]->Scale(1.0/(nSib*binFactor));

                if (mPairNormalization) {
                    if (nMixPairs > 0) {
                        hists[i+4+8*j]->Scale(norm);
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
  const char *symHistos[] = {"YtYt",   "NYtYt",   "PtPt",
                             "PhiPhi", "NPhiPhi", "PrPhiPhi", "PaPhiPhi", "PbPhiPhi",
                             "EtaEta",            "PrEtaEta", "PaEtaEta", "PbEtaEta",
                             "EtaEtaSS",          "PrEtaEtaSS", "PaEtaEtaSS", "PbEtaEtaSS",
                             "EtaEtaAS",          "PrEtaEtaAS", "PaEtaEtaAS", "PbEtaEtaAS"};
  // eight input histograms ++,+-,-+,-- for Sib and Mix
  int symInt[] = {1,2, 5, 6};
  for (int xy=0;xy<20;xy++) {
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
  const char *symHistos[] = {"YtYt",   "NYtYt",   "PtPt",
                             "PhiPhi", "NPhiPhi", "PrPhiPhi", "PaPhiPhi", "PbPhiPhi",
                             "EtaEta",            "PrEtaEta", "PaEtaEta", "PbEtaEta",
                             "EtaEtaSS",          "PrEtaEtaSS", "PaEtaEtaSS", "PbEtaEtaSS",
                             "EtaEtaAS",          "PrEtaEtaAS", "PaEtaEtaAS", "PbEtaEtaAS"};
  int symInt[] = {1,2, 5, 6};
  //    4 groups of 8 (Sibpp,Sibpm,Sibmp,Sibmm,Mixpp,Mixpm,Mixmp,Mixmm) 
  //    1st 8 are number, 2nd 8 are pt1*pt2, 3rd 8 are pt1 and 4th 8 are pt2
  for (int xy=0;xy<20;xy++) {
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
    for(int i=0;i<_pair_totalmax;i++) {
        retVal[i]->Reset();
    }

    if (opt < 3) {
        // In the final scaling to \Delta\rho/\sqrt(\rho_{ref}) or \Delta\rho/\rho_{ref}
        // one might want to use a {d^2N\over d\eta d\phi} for that specific
        // charge combination. I always use the value appropriate for CI.

        // Note on combining centralites:
        //    Here we are taking ratios of sib/mix to cancel acceptance/efficiency before averaging.
        //    Combining centralities in this way will not give the same result as using a larger
        //    centrality bin to begin with, but may remove some of the systematic problems.

        // We weight each zBin by the total number of sibling pairs (integrated over (\eta_\Delta,\phi_\Delta))
        // Divided by the number of pairs summed over zBins.

        // Twist is that if a Sibling OR Mixed  histogram has a zero the ratio of root histograms
        // gives 0. Want to ignore these in the averaging.
        // Use histograms for weights, setting individual bins to 0 or weight as described above.
        TH2D*** wScale = new TH2D**[mNumZBins];
        TH2D**  wTotal = new TH2D*[4];
        for (int iType=0;iType<4;iType++) {
            TString wName("sumWeighting");  wName += iType;
            wTotal[iType] = (TH2D *) retVal[iType]->Clone(wName.Data());
        }
        for (int iz=0;iz<mNumZBins;iz++) {
            wScale[iz] = getLocalClones(name, iz);
            double weight;
            for (int iType=0;iType<4;iType++) {
                weight = wScale[iz][iType]->Integral();
                for (int ix=1;ix<=wScale[iz][iType]->GetNbinsX();ix++) {
                    for (int iy=1;iy<=wScale[iz][iType]->GetNbinsY();iy++) {
                        double wTot = wTotal[iType]->GetBinContent(ix,iy);
                        if ((wScale[iz][iType]->GetBinContent(ix,iy) == 0) ||
                            (wScale[iz][iType+4]->GetBinContent(ix,iy) == 0)) {
                            wScale[iz][iType]->SetBinContent(ix,iy,0);
                        } else {
                            wScale[iz][iType]->SetBinContent(ix,iy,weight);
                            wTotal[iType]->SetBinContent(ix,iy,wTot+weight);
                        }
                    }
                }
            }
        }
        for (int iz=0;iz<mNumZBins;iz++) {
            for (int iType=0;iType<4;iType++) {
                wScale[iz][iType]->Divide(wTotal[iType]);
            }
        }

        // Now sum \Delta\rho / \rho_{ref} weighting by number of pairs in each zBin.
        for (int iz=0;iz<mNumZBins;iz++) {
            TH2D** tmpVal= buildCommon(name, opt, sf, iz);
            for (int iType=0;iType<4;iType++) {
                tmpVal[iType]->Multiply(wScale[iz][iType]);
                retVal[iType]->Add(tmpVal[iType]);
                delete tmpVal[iType];
            }
            delete [] tmpVal;
        }

        // Have \Delta\rho / \rho_{ref}.
        // Scale by some power of d^2N/d\eta d\phi (always using CI for this to be able to compare different charge combinations correctly).
        // I was normalizing these \Delta\rho / \rho_{ref} by 1/4 thinking that would correct for
        // adding four normalized histograms together. Actually that was approximately the algebra for
        // combining to get CI, really wanted things like (n+/Nch)^2 which is close to 1/4.
        // Move that scaling to code where we calculate  CI, LS, US and CD (unless someone is relying
        // on the old normalization for PP, PM, MP and MM in which case we move it back here.)
        double *scale = getScaleFactors();
        if (!msilent) {
            cout << "@@@@@ In buildCommon: using d^2N/detadphi = " << scale[4] << endl;
        }
        for (int iType=0;iType<4;iType++) {
            if (0 == opt) {
                // Do nothing
            } else if (1 == opt) {
                retVal[iType]->Scale(scale[iType]);
            } else if (2 == opt) {
                retVal[iType]->Scale(sqrt(scale[iType]));
            }
        }

        // Delete space needed for scaling factors.
        // Seem to be only deleting first four histograms most times when
        // we getLocalClones. That really returns 8 histograms.
        for (int iType=0;iType<8;iType++) {
            for (int iz=0;iz<mNumZBins;iz++) {
                delete wScale[iz][iType];
            }
        }
        for (int iType=0;iType<4;iType++) {
            delete wTotal[iType];
        }
        delete [] wScale;
        delete [] wTotal;
    } else {
        double wIdentical[4] = {1, 1, 1, 1};
        if (mIdenticalPair) {
            wIdentical[1] = 2;
            wIdentical[2] = 0;
        }
        // YtYt correlations
        // I don't see any evidence for YtYt depending on the z-vertex position of the event.
        // (I think there is clear evidence that (\eta_\Delta,\phi_\Delta) depends on z-vertex)
        // It seems that in the  ratio \rho{sib} / \rho_{ref} we have problems with bins where
        // \rho_{ref} is small.
        // For now just add zBins together, then do calculation.

        // buildCommon would scale each zBin. May want to refactor its functionality.
        // For now copy the appropriate part here.
        for (int iz=0;iz<mNumZBins;iz++) {
            TH2D** tmpVal = getLocalClones(name, iz);
            if (!tmpVal) {
                continue;
            }
            for (int iType=0;iType<8;iType++) {
                retVal[iType]->Add(tmpVal[iType]);
                delete tmpVal[iType];
            }
            delete [] tmpVal;
        }

        if (mDoSymmetrize) {
            symmetrizeUS(name,retVal);
        }

        if (mnpairs) {  // manual scaling
            for(int i=0;i<8;i++) {
                retVal[i]->Scale(1.0/mnpairs[i]);
            }
        } else {
            rescale(retVal);
        }

        if (strstr(name,"DEta") || strstr(name,"SEta")) {
            fixDEtaGeometric((TH2**)retVal,8); // does nothing unless mapplyDEtaFix is set
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


        // Calculate quantity we were asked for.
        for (int iType=0;iType<4;iType++) {
            // If rho and rho_ref are empty we skip the calculation
            if ((0 == retVal[iType]->Integral()) && (0 == retVal[iType]->GetMaximum()) &&
                (0 == retVal[iType+4]->Integral()) && (0 == retVal[iType+4]->GetMaximum())) {
                continue;
            }
            retVal[iType]->Scale(wIdentical[iType]);
            retVal[iType+4]->Scale(wIdentical[iType]);
            if (3 == opt) {        // \Delta\rho/\rho_ref,  Calculate \rho_sib/\rho_ref. Subtract 1 later.
                bool message = false;
                retVal[iType]->Divide(retVal[iType+4]);
                for (int ix=1;ix<=retVal[iType]->GetNbinsX();ix++) {
                    for (int iy=1;iy<=retVal[iType]->GetNbinsY();iy++) {
                        double rhoSib = retVal[iType]->GetBinContent(ix,iy);
                        if (0 == rhoSib) {
                            message = true;
                        } else {
                            retVal[iType]->SetBinContent(ix,iy,rhoSib-1);
                        }
                    }
                }
                if (message) {
                    cout << "!!!!! Had at least one empty bin in Pt histogram " << name;
                    cout << " type " << iType << " option 3 !!!!!" << endl;
                }
            } else if (4 == opt) { // \rho_sib - \rho_ref
                bool message = false;
                for (int ix=1;ix<=retVal[iType]->GetNbinsX();ix++) {
                    for (int iy=1;iy<=retVal[iType]->GetNbinsY();iy++) {
                        if (0 == retVal[iType]->GetBinContent(ix,iy)) {
                            message = true;
                        }
                    }
                }
                if (message) {
                    cout << "!!!!! Had at least one empty bin in Pt histogram " << name;
                    cout << " type " << iType << " option 4!!!!!" << endl;
                }
                retVal[iType]->Add(retVal[iType+4],-1);
            } else if (5 == opt) { // \Delta\rho/sqrt(\rho_ref). Need to calculate errors by hand.
                TH1 *hNum;
                double nSib = 0;
                for (int zBin=0;zBin<mNumZBins;zBin++) {
                    TString hSibName("NEventsSib_zBuf_");  hSibName += zBin;  mtf->GetObject(hSibName.Data(),hNum);
                    nSib += hNum->Integral();
                }
                for (int ix=1;ix<=retVal[iType]->GetNbinsX();ix++) {
                    for (int iy=1;iy<=retVal[iType]->GetNbinsY();iy++) {
                        double rhoSib = retVal[iType]->GetBinContent(ix,iy);
                        double rhoRef = retVal[iType+4]->GetBinContent(ix,iy);
                        double eRhoSib = retVal[iType]->GetBinError(ix,iy);
                        double eRhoRef = retVal[iType+4]->GetBinError(ix,iy);
                        if (rhoRef > 0) {
                            double eVal2 = eRhoSib*eRhoSib/rhoRef +
                                           0.25*eRhoRef*eRhoRef*pow(rhoSib/rhoRef+1,2)/rhoRef;
                            double val = (rhoSib-rhoRef)/sqrt(rhoRef);
                            retVal[iType]->SetBinContent(ix,iy,val);
                            retVal[iType]->SetBinError(ix,iy,sqrt(eVal2));
                        } else {
                            double eVal2 = eRhoSib*eRhoSib + 1;
                            retVal[iType]->SetBinContent(ix,iy,0);
                            // Not sure about this error.
                            retVal[iType]->SetBinError(ix,iy,sqrt(eVal2/nSib));
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
        fixDEtaGeometric((TH2**)retVal,8); // does nothing unless mapplyDEtaFix is set
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
        // Note that when a bin in retVal[i] or retVal[i+4] is 0 the ratio
        // will be set to 0 and then we subtract 1. Check for this and don't subtract the 1.
        // In parent method we hopefully weight this bin with 0 so don't include in an average.
        for (int i=0;i<4;i++) {
            // If rho and rho_ref are empty we skip the calculation
            if ((0 == retVal[i]->Integral()) && (0 == retVal[i]->GetMaximum()) &&
                (0 == retVal[i+4]->Integral()) && (0 == retVal[i+4]->GetMaximum())) {
                continue;
            }
            retVal[i]->Divide(retVal[i+4]);                        // rho/rho_{ref}
            for (int ix=1;ix<=retVal[i]->GetNbinsX();ix++) {
                for (int iy=1;iy<=retVal[i]->GetNbinsY();iy++) {
                    double val = retVal[i]->GetBinContent(ix,iy);
                    if (val == 0) {
                        retVal[i]->SetBinContent(ix,iy,0);
                    } else {
                        retVal[i]->SetBinContent(ix,iy,val-1);         // delta-rho/rho_{ref}
                    }
                }
            }
        }
    }

    return retVal;
}
//---------------------------------------------------------
// We should not need to subtract mixed pairs in calculation of pt correlations.
// It turns out to make a surprisingly big difference.

// Combine zBuffers as we do for number correlations.

// opt: 0 = delta-rho/rho_mix;  1 = delta-rho;  2 = delta-rho/sqrt(rho_mix);
TH2D** StEStructSupport::buildPtCommon(const char* name, int opt, int subtract) {
    TH2D** retVal= buildPtCommon(name, opt, subtract, 0);
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
    const char* oldName[4]  = {"Sibpp","Sibpm","Sibmp","Sibmm"};
    const char* newName[4]  = {"PP ","PM ","MP ","MM "};
    const char* oldTitle[4] = {"Sibling : +.+","Sibling : +.-","Sibling : -.+","Sibling : -.-"};
    const char* newTitle[4] = {"PP : ++ ","PM : +- ","MP: -+ ","MM : -- "};
    for (int i=0;i<4;i++) {
        retVal[i]->SetName(swapIn(retVal[i]->GetName(),oldName[i],newName[i]));
        retVal[i]->SetTitle(swapIn(retVal[i]->GetTitle(),oldTitle[i],newTitle[i]));
        retVal[i]->Reset();
    }

    // In the final scaling to \Delta\rho/\sqrt(\rho_{ref}) or \Delta\rho/\rho_{ref}
    // one might want to use a {d^2N\over d\eta d\phi} for that specific
    // charge combination. I always use the value appropriate for CI.

    // Note on combining centralites:
    //    Here we are taking ratios of sib/mix to cancel acceptance/efficiency before averaging.
    //    Combining centralities in this way will not give the same result as using a larger
    //    centrality bin to begin with, but may remove some of the systematic problems.

    // We weight each zBin by the total number of sibling pairs (integrated over (\eta_\Delta,\phi_\Delta)).
    // First sum over zBins to get total number of pairs.
    TH2D** temp;
    double *wScale[4];
    wScale[0] = new double[mNumZBins];
    wScale[1] = new double[mNumZBins];
    wScale[2] = new double[mNumZBins];
    wScale[3] = new double[mNumZBins];
    double wTotal[4]  = {0, 0, 0, 0};
    for (int iz=0;iz<mNumZBins;iz++) {
        temp = getLocalClones(name, iz);
        for (int iType=0;iType<4;iType++) {
            wScale[iType][iz] = temp[iType]->Integral();
            wTotal[iType]    += wScale[iType][iz];
            delete temp[iType];
        }
        for (int iType=4;iType<8;iType++) {
            delete temp[iType];
        }
        delete [] temp;
    }

    // Now sum \Delta\rho / \rho_{ref} weighting by number of pairs in each zBin.
    for (int iz=0;iz<mNumZBins;iz++) {
        TH2D** tmpVal= buildPtCommon(name, opt, subtract, iz);
        for (int iType=0;iType<4;iType++) {
            if (0 != wTotal[iType]) {
                retVal[iType]->Add(tmpVal[iType], wScale[iType][iz]/wTotal[iType]);
            }
            delete tmpVal[iType];
        }
        delete [] tmpVal;
    }

    // Have weighted \Delta\rho / \rho_{ref}.
    // Scale by some power of d^2N/d\eta d\phi (always using CI for this to be able to compare different charge combinations correctly).
    double *scale = getScaleFactors();
    if (!msilent) {
        cout << "@@@@@ In buildPtCommon: using d^2N/detadphi = " << scale[4] << endl;
    }
    for (int iType=0;iType<4;iType++) {
        if (0 == opt) {
            // Do nothing
        } else if (1 == opt) {
            retVal[iType]->Scale(scale[iType]);
        } else if (2 == opt) {
            retVal[iType]->Scale(sqrt(scale[iType]));
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
        fixDEtaGeometric((TH2**)hlocal,32);
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
        retVal[i]->Add(hlocal[i+16],-ptHatA[i]);
        retVal[i]->Add(hlocal[i+24],-ptHatB[i]);
        retVal[i]->Add(hlocal[i   ],ptHatA[i]*ptHatB[i]);

        mixVal[i]->Add(hlocal[i+12]);
        mixVal[i]->Add(hlocal[i+20],-ptHatA[i]);
        mixVal[i]->Add(hlocal[i+28],-ptHatB[i]);
        mixVal[i]->Add(hlocal[i+ 4],ptHatA[i]*ptHatB[i]);
    }

    for (int i=0;i<4;i++) {
        if (subtract) {
            // Zero errors of mixed events. Rationale is that those errors are
            // already included in the sibling.
            for (int ix=1;ix<=mixVal[i]->GetNbinsX();ix++) {
                for (int iy=1;iy<=mixVal[i]->GetNbinsY();iy++) {
                    mixVal[i]->SetBinError(ix,iy,0);
                }
            }
            retVal[i]->Add(mixVal[i],-1);  // Subtract mixed event artifacts
        }
        retVal[i]->Divide(hlocal[i+4]);
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
    double *scale = getScaleFactors();
    for (int iType=0;iType<4;iType++) {
        // Need to undo scaling with charge specific normalization and redo with CI
        if (0 == opt) {
            retVal[iType]->Scale(scale[iType]/pow(scale[4],2));
        } else if (1 == opt) {
            retVal[iType]->Scale(1.0);
        } else if (2 == opt) {
            retVal[iType]->Scale(sqrt(scale[iType])/scale[4]);
        }
    }
    delete [] scale;
    retVal[0]->Add(retVal[3]);
    if (mIdenticalPair) {
        retVal[1]->Scale(2);
    } else {
        retVal[1]->Add(retVal[2]);
    }
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

  if(strstr(name,"DEta") || strstr(name,"SEta"))fixDEtaGeometric((TH2**)hlocal,8); // does nothing unless mapplyDEtaFix is set

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

    // Form charge combinations LS, US, CD and CI.
    // Need to scale PP by (n+/Nch)^2, PM by n+n-/Nch^2 and MM by (n-/Nch)^2 to combine terms properly
    double *scale = getScaleFactors();
    for (int iType=0;iType<4;iType++) {
        // Need to undo scaling with charge specific normalization and redo with CI
        if (0 == opt) {
            retVal[iType]->Scale(scale[iType]/pow(scale[4],2));
        } else if (1 == opt) {
            retVal[iType]->Scale(1.0);
        } else if (2 == opt) {
            retVal[iType]->Scale(sqrt(scale[iType])/scale[4]);
        }
    }
    delete [] scale;
    retVal[0]->Add(retVal[3]);
    if (mIdenticalPair) {
        retVal[1]->Scale(2);
    } else {
        retVal[1]->Add(retVal[2]);
    }
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
void StEStructSupport::fixDEtaGeometric(TH2** h, int numHists) {

    // That other version of fixDEta is actually trying to do a complicated
    // acceptance correction. The geometric part is well defined, and we
    // do only that here.
    // No need to assume max and min eta are same distance from 0.
    // Makes it look slightly more complicated.
    // Take geometry from first histogram.

    if(!mapplyDEtaFix) return;

    TAxis *x = h[0]->GetXaxis();
    TAxis *y = h[0]->GetYaxis();
    double etaMax = x->GetXmax();
    double etaMin = x->GetXmin();
    double H = 0.5*( etaMax - etaMin );
    double x0 = 0.5*( etaMax + etaMin );
    for (int ix=1;ix<=x->GetNbins();ix++) {
        double eta1 = x->GetBinLowEdge(ix);
        double eta2 = x->GetBinUpEdge(ix);
        double corr;
        if (eta2 <= x0) {
            corr = H / ( 0.5*(eta2+eta1) - etaMin );
        } else if (eta1 >= x0) {
            corr = H / ( etaMax - 0.5*(eta2+eta1) );
        } else {
            double area = H*(eta2-eta1);
            double aplus  = (eta2-x0)*0.5*(H + etaMax-eta2);
            double aminus = (x0-eta1)*0.5*(H + eta1-etaMin);
            corr = area / (aplus + aminus);
        }
        for (int iy=1;iy<=y->GetNbins();iy++) {
            for (int ih=0;ih<numHists;ih++) {
                double val = h[ih]->GetBinContent(ix,iy);
                double err = h[ih]->GetBinError(ix,iy);
                h[ih]->SetBinContent(ix,iy,val*corr);
                h[ih]->SetBinError(ix,iy,err*corr);
            }
        }
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

  std::ostringstream ts;
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
 * Revision 1.30  2012/11/16 21:27:23  prindle
 * AS, SS histograms. Get eta limits from histogram in file. Check for empty bins in ratio (showed up as offset in correlations). Scale histograms according to actual number of pairs, don't assume 1/2 for example.
 *
 * Revision 1.29  2012/06/11 14:35:33  fisyak
 * std namespace
 *
 * Revision 1.28  2011/08/02 20:42:24  prindle
 *   Added YtYtVolumeNormalization.
 *   Fixed calculation of error for YtYt \Delta\rho/sqrt(\rho_{ref})
 *   Added error calculation for p_t histograms
 *   Added warning when either \rho_{sib} or \rho_{ref} has an empty bin. Set ratio
 *    bin to 0 instead of -1.
 *
 * Revision 1.27  2010/09/02 21:31:14  prindle
 *   Support: Can't find evidence that YtYt correlation depends on z vertex.
 *            Add numerators and denominators then take ratio. Need a new
 *            rescale method independent of z bin. Looks like we can normalize
 *            mixed so \int{sib/mix} = number of bins (what we have recently been
 *            doing) or \int{sib} = \int{mix} and the former is more snesitive
 *            to bins with very few counts. That isn't important for angular
 *            histograms but is for (yt,yt). I am calling the \int{sib} = \int(mix}
 *            Yt normalization (even though it is what we did long ago).
 *
 * Revision 1.26  2010/06/23 22:33:50  prindle
 *   In HAdd we distinguish between the parent distributions of the
 *    two particles.
 *   In Support I fixed a number of problems in the Pt correlation section.
 *
 * Revision 1.25  2010/03/02 21:48:30  prindle
 *   Fix addDensities (for checking pair cuts)
 *   Lots of small changes
 *
 * Revision 1.24  2009/11/09 21:32:59  prindle
 * Fix warnings about casting char * to a const char * by redeclaring as const char *.
 *
 * Revision 1.23  2009/08/17 23:05:20  dkettler
 * Normalization fix
 *
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
 * Revision 1.13  2006/10/27 00:05:32  prindle
 *   Modified buildChargeTypes to handle case where the -+ histogram is
 * empty. Also tried making buildCommonTypes work, but there one of the
 * output histograms was intended to be -+ and most of the time that
 * will be empty.
 *
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


