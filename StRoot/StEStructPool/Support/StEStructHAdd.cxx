/**********************************************************************
 *
 * $Id: StEStructHAdd.cxx,v 1.17 2012/11/16 21:27:23 prindle Exp $
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
#include "StEStructHAdd.h"

#include "StEStructPool/Correlations/StEStructBinning.h"  
#include "StEStructPool/Correlations/StEStructCutBin.h"  

#include "Stiostream.h"
#include "TH1.h"
#include "TH2.h"
#include "TH2D.h"
#include "TFile.h"

ClassImp(StEStructHAdd)

  /*
   * 12/03/08 djp Instrumented StEStruct2ptCorrelations so we can select specific
   *              groups of histograms. We still try to read all the possible histograms
   *              but don't get upset if we don't find them.
   *              Expunge code to look for old style of name (without zBin) to make code simpler.
   */
  /*
   * 06/05/08 djp Looking at SEtaDPhi histograms I was confused that they were not symmetric
   *              around SEta=0. I guess this was intentional with the idea that forward and
   *              backward could be different. Should re-think this as it may make more
   *              sense to symmetrize.
   */
  /* 09/27/12 djp Now taking eta range from Cuts file. Had been interrogating
   *              binning class for detaMax. Extract from special purpose histogram instead.
   */
  //------------------------------------------------------------------------
void StEStructHAdd::addCuts(const char* outfile, TFile* inFile,
                            int* nlist, int ntot, int parentDist[][2], int* nParentDist, int symmXX) {

    const char* base[]={"Sib","Mix"};
    const char* tpe[]={"pp","pm","mp","mm"};
    const char* knd[]={"YtYt",         "NYtYt",        "PtPt",
                       "PhiPhi",       "NPhiPhi",      "PrPhiPhi",      "PaPhiPhi",      "PbPhiPhi",
                       "EtaEta",                       "PrEtaEta",      "PaEtaEta",      "PbEtaEta",
                       "EtaEtaSS",                     "PrEtaEtaSS",    "PaEtaEtaSS",    "PbEtaEtaSS",
                       "EtaEtaAS",                     "PrEtaEtaAS",    "PaEtaEtaAS",    "PbEtaEtaAS",
                       "DEtaDPhiArr",  "NDEtaDPhiArr", "PrDEtaDPhiArr", "PaDEtaDPhiArr", "PbDEtaDPhiArr",
                       "SYtDYt",       "NSYtDYt",
                       "SEtaDPhiArr",  "NSEtaDPhiArr", "PrSEtaDPhiArr", "PaSEtaDPhiArr", "PbSEtaDPhiArr",
                       "Qinv",         "NQinv"};
    int isXXHist[]={1,   1,   1,
                    1,   1,   1,   1,   1,
                    1,        1,   1,   1,
                    1,        1,   1,   1,
                    1,        1,   1,   1,
                    0,   0,   0,   0,   0,
                    0,   0,
                    0,   0,   0,   0,   0,
                    0,   0};
    const char* symEtaPhi[]={"DEtaDPhi", "NDEtaDPhi", "PrDEtaDPhi", "PaDEtaDPhi", "PbDEtaDPhi"};
    const char* symPhi[]={"SEtaDPhi", "NSEtaDPhi", "PrSEtaDPhi", "PaSEtaDPhi", "PbSEtaDPhi"};
    const char* Title[]={"Sibling","Mixed"};
    const char* Species[]={"A","B"};
    const char* Type[]={" : +.+"," : +.-"," : -.+"," : -.-"};
    StEStructBinning* b=StEStructBinning::Instance();
    TH2D *hEtaPhi;
    inFile->GetObject("EtaPhiRange",hEtaPhi);
    double etaMin = -1;
    double etaMax = +1;
    if (hEtaPhi) {
        TAxis *x = hEtaPhi->GetXaxis();
        etaMin = x->GetXmin();
        etaMax = x->GetXmax();
    }
    b->setEtaRange(etaMin,etaMax);

    StEStructCutBin* cb = StEStructCutBin::Instance();

    TFile* outFile = new TFile(outfile,"RECREATE");
    outFile->cd();
    if (hEtaPhi) {
        hEtaPhi->Write();
    }

    for (int i=0;i<2;i++) {
        for (int j=0;j<4;j++) {
            for (int k=0;k<34;k++) {

                TH2D *outhist=0;
                TH2D *tmp=0, *cpy=0;
                inFile->cd();
                int zBin = 0;
                while (zBin<99) {
                    // Histogram name is of form
                    //    SibppDEtaDPhi_cutBin_n_zBuf_m
                    // where n is the usual cut bin number and m is the zbuffer index.
                    TString htype(base[i]); htype+=tpe[j]; htype+=knd[k];
                    for (int n=0;n<ntot;n++) {
                        TString fullName(htype.Data()); fullName+="_cutBin_"; fullName+=nlist[n]; fullName+="_zBuf_"; fullName+=zBin;
                        TString hName(htype.Data()); hName+="_zBuf_"; hName+=zBin;
                        inFile->GetObject(fullName.Data(),tmp);
                        if (!tmp) {
                            // Have either processed all zbins or this histogram (at least for zBuf=0) isn't in the file.
                            goto lastZ;
                        }
                        if (0==n) {
                            outhist=(TH2D *)tmp->Clone();
                            if (symmXX && isXXHist[k]) {
                                // This part is (was) only intended to be used with mode 5 (pid)
                                if (cb->notSymmetrizedXX(nlist[n],j)) {
                                    symmetrizeXX(outhist);
                                }
                            }
                            outhist->SetName(hName.Data());
                            outhist->SetTitle(tmp->GetTitle());
                        } else {
                            cpy = (TH2D *)tmp->Clone();
                            if (symmXX && isXXHist[k]) {
                                // This part is (was) only intended to be used with mode 5 (pid)
                                if (cb->notSymmetrizedXX(nlist[n],j)) {
                                    symmetrizeXX(cpy);
                                }
                            }
                            outhist->Add(cpy);
                            delete cpy;
                        }
                    }
                    for (int isym=0;isym<5;isym++) {
                        if (!strncmp(symEtaPhi[isym],knd[k],8)) {
                            // Here outhist is not symmetrized. It will be replaced by tmp which is.
                            b->setNDEtaBins(outhist->GetNbinsX());
                            b->setNDPhiBins(outhist->GetNbinsY());
                            TString hname(base[i]);    hname+=tpe[j];     hname+=symEtaPhi[isym];
                            TString zName(hname);      zName+="_zBuf_";   zName+=zBin;
                            TString htitle(Title[i]);  htitle+=Type[j];   htitle+=symEtaPhi[isym];
                            TString zTitle(htitle);    zTitle+="_zBuf_";  zTitle+=zBin;
                            tmp = new TH2D(zName.Data(),zTitle.Data(),b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
                            // Turn error propagation on by default since it seems setting bin content bypasses that.
                            // For a few histogram types we handle errors explicitly to avoid double counting.
                            tmp->Sumw2();
                            for(int ieta=0;ieta<b->detaBins();ieta++){
                                for(int iphi=0;iphi<b->dphiBins();iphi++){
                                    float eta1 = b->detaVal(ieta);
                                    float eta2 = -eta1;
                                    int ieta1 = b->hdetaBin(eta1);
                                    int ieta2 = b->hdetaBin(eta2);
                                    float phi1 = b->dphiVal(iphi,1);
                                    float phi2 = b->dphiVal(iphi,2);
                                    int iphi1 = b->hdphiBin(phi1);
                                    int iphi2 = b->hdphiBin(phi2);
                                    double val = outhist->GetBinContent(ieta+1,iphi+1);
                                    double err = outhist->GetBinError(ieta+1,iphi+1);
                                    tmp->SetBinContent(ieta1,iphi1,val);
                                    tmp->SetBinContent(ieta2,iphi1,val);
                                    tmp->SetBinContent(ieta1,iphi2,val);
                                    tmp->SetBinContent(ieta2,iphi2,val);
                                    tmp->SetBinError(ieta1,iphi1,err);
                                    tmp->SetBinError(ieta2,iphi1,err);
                                    tmp->SetBinError(ieta1,iphi2,err);
                                    tmp->SetBinError(ieta2,iphi2,err);
                                }
                            }
                            // Adjust for bins at \eta_\Delta=0 and \phi_\Delta=0 and pi
                            // Only do the adjustment if bin is centered.
                            // (When calculating \Delta\rho/\rho_{ref} this adjustment divides out,
                            //  but when looking at intermediate histograms it should be less surprising.)
                            float phi1 = b->dphiVal(0,1);
                            float phi2 = b->dphiVal(0,2);
                            int iphi1 = b->hdphiBin(phi1);
                            int iphi2 = b->hdphiBin(phi2);
                            if (iphi1 == iphi2) {
                                for (int ieta=1;ieta<=b->hdetaBins();ieta++) {
                                    double val = 2*tmp->GetBinContent(ieta,iphi1);
                                    double err = 2*tmp->GetBinError(ieta,iphi1);
                                    tmp->SetBinContent(ieta,iphi1,val);
                                    tmp->SetBinError(ieta,iphi1,err);
                                }
                            }
                            // indices in StEStructBinning start at 0, histogram bins start at 1
                            phi1 = b->dphiVal(outhist->GetNbinsY()-1,1);
                            phi2 = b->dphiVal(outhist->GetNbinsY()-1,2);
                            iphi1 = b->hdphiBin(phi1);
                            iphi2 = b->hdphiBin(phi2);
                            if (iphi1 == iphi2) {
                                for (int ieta=1;ieta<=b->hdetaBins();ieta++) {
                                    double val = 2*tmp->GetBinContent(ieta,iphi1);
                                    double err = 2*tmp->GetBinError(ieta,iphi1);
                                    tmp->SetBinContent(ieta,iphi1,val);
                                    tmp->SetBinError(ieta,iphi1,err);
                                }
                            }
                            float eta1 = b->detaVal(0);
                            float eta2 = -eta1;
                            int ieta1 = b->hdetaBin(eta1);
                            int ieta2 = b->hdetaBin(eta2);
                            if (ieta1 == ieta2) {
                                for (int iphi=1;iphi<=b->hdphiBins();iphi++) {
                                    double val = 2*tmp->GetBinContent(ieta1,iphi);
                                    double err = 2*tmp->GetBinError(ieta1,iphi);
                                    tmp->SetBinContent(ieta1,iphi,val);
                                    tmp->SetBinError(ieta1,iphi,err);
                                }
                            }
                            // fill the repeated \phi_\Delta bin
                            if (0 == tmp->GetBinContent(1,1)) {
                                int iphi = b->hdphiBins();
                                for (int ieta=1;ieta<=b->hdetaBins();ieta++) {
                                    double val = tmp->GetBinContent(ieta,iphi);
                                    double err = tmp->GetBinError(ieta,iphi);
                                    tmp->SetBinContent(ieta,1,val);
                                    tmp->SetBinError(ieta,1,err);
                                }
                            }
                            // Create rotated view from -pi to pi for CD 
                            hname+="Rot";  hname +="_zBuf_", hname+=zBin;
                            htitle+="Rot"; htitle+="_zBuf_", htitle+=zBin;
                            TH2D* rot = new TH2D(hname.Data(),htitle.Data(),b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),-M_PI-M_PI/24,M_PI+M_PI/24);
                            for(int ieta=0;ieta<b->detaBins();ieta++){
                                for(int iphi=0;iphi<b->dphiBins();iphi++) {
                                    float eta1 = b->detaVal(ieta);
                                    float eta2 = -eta1;
                                    float phi1 = b->dphiVal(iphi,1);
                                    if (phi1>M_PI) phi1=2*M_PI - phi1;  // get phi1 to be [0..pi]
                                    float phi2 = -phi1;                 // phi2 [-pi..0]
                                    double val = outhist->GetBinContent(ieta+1,iphi+1);
                                    rot->Fill(eta1,phi1,val);
                                    rot->Fill(eta2,phi1,val);
                                    rot->Fill(eta1,phi2,val);
                                    rot->Fill(eta2,phi2,val);
                                }
                            }
                            int bin1 = rot->GetYaxis()->FindBin(M_PI); // need to double bin values at pi and -pi 
                            int bin2 = rot->GetYaxis()->FindBin(-M_PI);
                            for(int ieta=0;ieta<rot->GetNbinsX();ieta++) { 
                                rot->SetBinContent(ieta+1,bin1, 2*rot->GetBinContent(ieta+1,bin1));
                                rot->SetBinContent(ieta+1,bin2, 2*rot->GetBinContent(ieta+1,bin2));
                            }
                            // Note that we need to adjust errors here also.
                            outFile->cd();
                            rot->Write();
                            delete rot;
                            // end of rotated view
                            delete outhist;
                            outhist = tmp;
                        }
                    }
                    for (int isym=0;isym<5;isym++) {
                        if (!strncmp(symPhi[isym],knd[k],8)) {
                            b->setNSEtaBins(outhist->GetNbinsX());
                            b->setNDPhiBins(outhist->GetNbinsY());
                            TString hname(base[i]);
                            hname+=tpe[j];
                            hname+=symPhi[isym];
                            TString zName(hname);
                            zName+="_zBuf_";
                            zName+=zBin;
                            TString htitle(Title[i]);
                            htitle+=Type[j];
                            htitle+=symPhi[isym];
                            TString zTitle(htitle);
                            zTitle+="_zBuf_";
                            zTitle+=zBin;
                            tmp = new TH2D(zName.Data(),zTitle.Data(),b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
                            for(int ieta=0;ieta<b->setaBins();ieta++){
                                for(int iphi=0;iphi<b->dphiBins();iphi++){
                                    float eta1 = b->setaVal(ieta);
                                    int ieta1 = b->hdetaBin(eta1);
                                    float phi1 = b->dphiVal(iphi,1);
                                    float phi2 = b->dphiVal(iphi,2);
                                    int iphi1 = b->hdphiBin(phi1);
                                    int iphi2 = b->hdphiBin(phi2);
                                    double val = outhist->GetBinContent(ieta+1,iphi+1);
                                    double err = outhist->GetBinError(ieta+1,iphi+1);
                                    tmp->SetBinContent(ieta1,iphi1,val);
                                    tmp->SetBinContent(ieta1,iphi2,val);
                                    tmp->SetBinError(ieta1,iphi1,err);
                                    tmp->SetBinError(ieta1,iphi2,err);
                                }
                            }
                            if (tmp->GetBinContent(1,1)==0) {  // fill the repeated bin
                                for (int ieta=0;ieta<tmp->GetNbinsX();ieta++) {
                                    double val = tmp->GetBinContent(ieta+1,tmp->GetNbinsY());
                                    double err = tmp->GetBinError(ieta+1,tmp->GetNbinsY());
                                    tmp->SetBinContent(ieta+1,1, val);
                                    tmp->SetBinError(ieta+1,1, err);
                                }
                            }
                            delete outhist;
                            outhist = tmp;
                        }
                    }
                    outFile->cd();
                    if (outhist) {
                        outhist->Write();
                        delete outhist;
                    }
                    zBin++;
                }
                lastZ:
                continue;
            }
        }
    }


    inFile->cd();
    TH1D* sib; inFile->GetObject("NEventsSame",sib);
    TH1D* mix; inFile->GetObject("NEventsMixed",mix);
    if(sib && mix) {
        // Histogram with older naming style was found.
        // No z-binning here.
        outFile->cd();
        sib->Write();
        mix->Write();
    } else {
        int zBin = 0;
        TString hSibName("NEventsSib_zBuf_");
        TString hMixName("NEventsMix_zBuf_");
        TString hSibPosName("NEventsPosSib_zBuf_");
        TString hMixPosName("NEventsPosMix_zBuf_");
        TString hSibNegName("NEventsNegSib_zBuf_");
        TString hMixNegName("NEventsNegMix_zBuf_");
        while (zBin<99) {
            TString fullSibName(hSibName.Data()); fullSibName+=zBin;
            TString fullMixName(hMixName.Data()); fullMixName+=zBin;
            TString fullSibPosName(hSibPosName.Data()); fullSibPosName+=zBin;
            TString fullMixPosName(hMixPosName.Data()); fullMixPosName+=zBin;
            TString fullSibNegName(hSibNegName.Data()); fullSibNegName+=zBin;
            TString fullMixNegName(hMixNegName.Data()); fullMixNegName+=zBin;
            inFile->cd();
            inFile->GetObject(fullSibName.Data(),sib);
            inFile->GetObject(fullMixName.Data(),mix);
            TH1D *sibPos; inFile->GetObject(fullSibPosName.Data(),sibPos);
            TH1D *mixPos; inFile->GetObject(fullMixPosName.Data(),mixPos);
            TH1D *sibNeg; inFile->GetObject(fullSibNegName.Data(),sibNeg);
            TH1D *mixNeg; inFile->GetObject(fullMixNegName.Data(),mixNeg);
            if (sib && mix) {
                outFile->cd();
                sib->Write();
                mix->Write();
                sibPos->Write();
                mixPos->Write();
                sibNeg->Write();
                mixNeg->Write();
            } else {
                break;
            }
            TString ptPName;
            TString ptMName;
            TString etaPName;
            TString etaMName;
            TH1 *tmp, *ptP, *ptM, *etaP, *etaM;
            for (int iSpecies=0;iSpecies<2;iSpecies++) {
                inFile->cd();
                for (int iPar=0;iPar<nParentDist[iSpecies];iPar++) {
                    int jPar = parentDist[iPar][iSpecies];
                    ptPName  = "meanPtP_parentBin"; ptPName  += jPar; ptPName  += "_zBuf_"; ptPName  += zBin;
                    ptMName  = "meanPtM_parentBin"; ptMName  += jPar; ptMName  += "_zBuf_"; ptMName  += zBin;
                    etaPName = "etaP_parentBin";    etaPName += jPar; etaPName += "_zBuf_"; etaPName += zBin;
                    etaMName = "etaM_parentBin";    etaMName += jPar; etaMName += "_zBuf_"; etaMName += zBin;
                    if (0 == iPar) {
                        inFile->GetObject(ptPName.Data(),ptP);
                        inFile->GetObject(ptMName.Data(),ptM);
                        inFile->GetObject(etaPName.Data(),etaP);
                        inFile->GetObject(etaMName.Data(),etaM);
                    } else {
                        inFile->GetObject(ptPName.Data(),tmp);  ptP->Add(tmp);
                        inFile->GetObject(ptMName.Data(),tmp);  ptM->Add(tmp);
                        inFile->GetObject(etaPName.Data(),tmp); etaP->Add(tmp);
                        inFile->GetObject(etaMName.Data(),tmp); etaM->Add(tmp);
                    }
                }
                outFile->cd();
                if (ptP) {
                    ptPName = "meanPtP";  ptPName += Species[iSpecies]; ptPName += "_zBuf_"; ptPName += zBin;
                    ptP->SetName(ptPName.Data());
                    ptP->Write();
                }
                if (ptM) {
                    ptMName = "meanPtM"; ptMName += Species[iSpecies]; ptMName += "_zBuf_"; ptMName += zBin;
                    ptM->SetName(ptMName.Data());
                    ptM->Write();
                }
                if (etaP) {
                    etaPName = "etaP"; etaPName += Species[iSpecies]; etaPName += "_zBuf_"; etaPName += zBin;
                    etaP->SetName(etaPName.Data());
                    etaP->Write();
                }
                if (etaM) {
                    etaMName = "etaM"; etaMName += Species[iSpecies]; etaMName += "_zBuf_"; etaMName += zBin;
                    etaM->SetName(etaMName.Data());
                    etaM->Write();
                }
            }
            inFile->cd();
            zBin++;
        }
    }
    outFile->Close();
};
//--------------------------------------------------------------------------
void StEStructHAdd::addCuts(const char* outfile, const char* infile,
                            int* nlist, int ntot, int parentDist[][2], int* nParentDist, int symmXX) {

    TFile* tf=new TFile(infile);
    if (tf) {
        addCuts(outfile,tf,nlist,ntot,parentDist,nParentDist,symmXX);
    }
    return;
};
//------------------------------------------------------------------------
void StEStructHAdd::symmetrizeXX(TH2 *hist) {
    for (int ix=1;ix<=hist->GetNbinsX();ix++) {
        for (int iy=ix;iy<=hist->GetNbinsY();iy++) {
            double sum = hist->GetBinContent(ix,iy) + hist->GetBinContent(iy,ix);
            double err = sqrt(pow(hist->GetBinError(ix,iy),2) + pow(hist->GetBinError(iy,ix),2));
            hist->SetBinContent(ix,iy,sum);
            hist->SetBinContent(iy,ix,sum);
            hist->SetBinError(ix,iy,err);
            hist->SetBinError(iy,ix,err);
        }
    }

};
//------------------------------------------------------------------------
void StEStructHAdd::old_addDensities(const char* outfile, TFile* inFile) {

    TFile* outFile = new TFile(outfile,"RECREATE");

    // If pair density histograms are in input file add z and cut bins,
    // form LS and US, take ratio of sibling to mixed and write to output file.
    inFile->cd();
    TH2 *sibDens; inFile->GetObject("SibppTPCMidTZ_cutBin_0_zBuf_0",sibDens);
    TH2 *mixDens; inFile->GetObject("MixppTPCMidTZ_cutBin_0_zBuf_0",mixDens);
    if(sibDens && mixDens) {
        int nCut = 0;
        int nZBuf = 0;
        TString hSibName("SibppTPCMidTZ_cutBin_");
        while (nCut<99) {
            TString full(hSibName.Data()); full += nCut; full += "_zBuf_0";
            inFile->GetObject(full.Data(),sibDens);
            if (sibDens) {
                nCut++;
            } else {
                break;
            }
        }
        while (nZBuf<99) {
            TString full(hSibName.Data()); full += "0_zBuf_"; full += nZBuf;
            inFile->GetObject(full.Data(),sibDens);
            if (sibDens) {
                nZBuf++;
            } else {
                break;
            }
        }

        TH2D *ls[6], *us[6], *tmp;
        TH2D *cLS[6], *cUS[6];
        inFile->GetObject("SibppTPCMidTZ_cutBin_0_zBuf_0",tmp);
        for (int i=0;i<6;i++) {
            ls[i] = (TH2D *) tmp->Clone();  ls[i]->Clear();
            us[i] = (TH2D *) tmp->Clone();  us[i]->Clear();
            cLS[i] = (TH2D *) tmp->Clone();
            cUS[i] = (TH2D *) tmp->Clone();
        }
        TString hDensName;
        const char *typePP[] = {"SibppTPCMidTZ_cutBin_", "SibppTPCMidTZC_cutBin_", "SibppTPCMidTZNC_cutBin_", "MixppTPCMidTZ_cutBin_", "MixppTPCMidTZC_cutBin_", "MixppTPCMidTZNC_cutBin_"};
        const char *typeMM[] = {"SibmmTPCMidTZ_cutBin_", "SibmmTPCMidTZC_cutBin_", "SibmmTPCMidTZNC_cutBin_", "MixmmTPCMidTZ_cutBin_", "MixmmTPCMidTZC_cutBin_", "MixmmTPCMidTZNC_cutBin_"};
        const char *typePM[] = {"SibpmTPCMidTZ_cutBin_", "SibpmTPCMidTZC_cutBin_", "SibpmTPCMidTZNC_cutBin_", "MixpmTPCMidTZ_cutBin_", "MixpmTPCMidTZC_cutBin_", "MixpmTPCMidTZNC_cutBin_"};
        const char *typeMP[] = {"SibmpTPCMidTZ_cutBin_", "SibmpTPCMidTZC_cutBin_", "SibmpTPCMidTZNC_cutBin_", "MixmpTPCMidTZ_cutBin_", "MixmpTPCMidTZC_cutBin_", "MixmpTPCMidTZNC_cutBin_"};
        const char *pairPos[] = {"TPCMidTZ_LS_", "TPCMidTZC_LS_", "TPCMidTZNC_LS_", "TPCMidTZ_US_", "TPCMidTZC_US_", "TPCMidTZNC_US_"};
        for (int iCut=0;iCut<nCut;iCut++) {
            inFile->cd();
            for (int i=0;i<6;i++) {
                cLS[i]->Clear();
                cUS[i]->Clear();
            }
            hDensName = pairPos[0]; hDensName += iCut; cLS[0]->SetName(hDensName.Data());
            hDensName = pairPos[1]; hDensName += iCut; cLS[1]->SetName(hDensName.Data());
            hDensName = pairPos[2]; hDensName += iCut; cLS[2]->SetName(hDensName.Data());
            hDensName = pairPos[3]; hDensName += iCut; cUS[0]->SetName(hDensName.Data());
            hDensName = pairPos[4]; hDensName += iCut; cUS[1]->SetName(hDensName.Data());
            hDensName = pairPos[5]; hDensName += iCut; cUS[2]->SetName(hDensName.Data());
            for (int zBuf=0;zBuf<nZBuf;zBuf++) {
                for (int it=0;it<6;it++) {
                    hDensName = typePP[it]; hDensName += iCut; hDensName += "_zBuf_"; hDensName += zBuf;
                    inFile->GetObject(hDensName.Data(),tmp);
                    cLS[it]->Add(tmp);
                    ls[it]->Add(tmp);
                    hDensName = typeMM[it]; hDensName += iCut; hDensName += "_zBuf_"; hDensName += zBuf;
                    inFile->GetObject(hDensName.Data(),tmp);
                    cLS[it]->Add(tmp);
                    ls[it]->Add(tmp);

                    hDensName = typePM[it]; hDensName += iCut; hDensName += "_zBuf_"; hDensName += zBuf;
                    inFile->GetObject(hDensName.Data(),tmp);
                    cUS[it]->Add(tmp);
                    us[it]->Add(tmp);
                    hDensName = typeMP[it]; hDensName += iCut; hDensName += "_zBuf_"; hDensName += zBuf;
                    inFile->GetObject(hDensName.Data(),tmp);
                    cUS[it]->Add(tmp);
                    us[it]->Add(tmp);
                }
            }
            cLS[0]->Divide(cLS[3]);
            cLS[1]->Divide(cLS[4]);
            cLS[2]->Divide(cLS[5]);
            cUS[0]->Divide(cUS[3]);
            cUS[1]->Divide(cUS[4]);
            cUS[2]->Divide(cUS[5]);
            outFile->cd();
            cLS[0]->Write();
            cLS[1]->Write();
            cLS[2]->Write();
            cUS[0]->Write();
            cUS[1]->Write();
            cUS[2]->Write();
        }
        ls[0]->Divide(ls[3]);  ls[0]->SetName("TPCMidTZ_LS");
        ls[1]->Divide(ls[4]);  ls[1]->SetName("TPCMidTZC_LS");
        ls[2]->Divide(ls[5]);  ls[2]->SetName("TPCMidTZNC_LS");
        us[0]->Divide(us[3]);  us[0]->SetName("TPCMidTZ_US");
        us[1]->Divide(us[4]);  us[1]->SetName("TPCMidTZC_US");
        us[2]->Divide(us[5]);  us[2]->SetName("TPCMidTZNC_US");
        outFile->cd();
        ls[0]->Write();
        ls[1]->Write();
        ls[2]->Write();
        us[0]->Write();
        us[1]->Write();
        us[2]->Write();
        for (int i=0;i<6;i++) {
            delete ls[i];
            delete us[i];
            delete cLS[i];
            delete cUS[i];
        }
    }
    outFile->Close();
    delete outFile;
}
void StEStructHAdd::addDensities(const char* outfile, TFile* inFile) {


    // If pair density histograms are in input file add z and cut bins,
    // form LS and US, take ratio of sibling to mixed and write to output file.
    TH2 *sibDens; inFile->GetObject("SibppTPCMidTZ_cutBin_0_zBuf_0",sibDens);
    TH2 *mixDens; inFile->GetObject("MixppTPCMidTZ_cutBin_0_zBuf_0",mixDens);
    if (!sibDens || !mixDens) {
        return;
    }

    // Find number of cut bins and number of zBuffers.
    int nCut = 0;
    int nZBuf = 0;
    TString hSibName("SibppTPCMidTZ_cutBin_");
    while (nCut<99) {
        TString full(hSibName.Data()); full += nCut; full += "_zBuf_0";
        inFile->GetObject(full.Data(),sibDens);
        if (sibDens) {
            nCut++;
        } else {
            break;
        }
    }
    while (nZBuf<99) {
        TString full(hSibName.Data()); full += "0_zBuf_"; full += nZBuf;
        inFile->GetObject(full.Data(),sibDens);
        if (sibDens) {
            nZBuf++;
        } else {
            break;
        }
    }


    TH1D *tmp1D;
    TH2D *tmp2D;

    TH1D *sep1D[2][2][4][2];
    TH1D *Sum_sep1D[2][2][4][2];
    inFile->GetObject("SibppTPCAvgTSep_cutBin_0_zBuf_0",tmp1D);
    for (int irel=0;irel<2;irel++) {
        for (int ich=0;ich<2;ich++) {
            for (int ipos=0;ipos<4;ipos++) {
                for (int idir=0;idir<2;idir++) {
                    Sum_sep1D[irel][ich][ipos][idir] = (TH1D *) tmp1D->Clone();
                }
            }
        }
    }

    TH1D *dpt1D[2][2][2][2];
    TH1D *Sum_dpt1D[2][2][2][2];
    inFile->GetObject("SibppTPCMidTdptP_cutBin_0_zBuf_0",tmp1D);
    for (int irel=0;irel<2;irel++) {
        for (int ich=0;ich<2;ich++) {
            for (int ity=0;ity<2;ity++) {
                for (int idir=0;idir<2;idir++) {
                    Sum_dpt1D[irel][ich][ity][idir] = (TH1D *) tmp1D->Clone();
                }
            }
        }
    }

    TH2D *sep2D[2][2][4];
    TH2D *Sum_sep2D[2][2][4];
    inFile->GetObject("SibppTPCAvgTZ_cutBin_0_zBuf_0",tmp2D);
    for (int irel=0;irel<2;irel++) {
        for (int ich=0;ich<2;ich++) {
            for (int ipos=0;ipos<4;ipos++) {
                Sum_sep2D[irel][ich][ipos] = (TH2D *) tmp2D->Clone();
            }
        }
    }

    TH2D *cross[2][2][2];
    TH2D *Sum_cross[2][2][2];
    inFile->GetObject("SibppTPCMidTZC_cutBin_0_zBuf_0",tmp2D);
    for (int irel=0;irel<2;irel++) {
        for (int ich=0;ich<2;ich++) {
            for (int icr=0;icr<2;icr++) {
                Sum_cross[irel][ich][icr] = (TH2D *) tmp2D->Clone();
            }
        }
    }

    TH2D *dpt2D[2][2][3];
    TH2D *Sum_dpt2D[2][2][3];
    inFile->GetObject("SibppTPCEntTdpt_cutBin_0_zBuf_0",tmp2D);
    for (int irel=0;irel<2;irel++) {
        for (int ich=0;ich<2;ich++) {
            for (int ipos=0;ipos<3;ipos++) {
                Sum_dpt2D[irel][ich][ipos] = (TH2D *) tmp2D->Clone();
            }
        }
    }

    TString hName;
    const char *pos[] = {"Ent", "Mid", "Exit", "Avg"};
    const char *dir[] = {"T", "Z"};
    const char *ty[] = {"P", "N"};
    const char *crs[] = {"C", "NC"};

    // Loop over cut bins.
    // For each cut bin we sum over zBuffers, calculate and write Sib/Mix for LS and US
    // We also sum over cut bins, calculating Sib/Mix at end.
    const char *rel[] = {"Sib", "Mix"};
    const char *ch[] = {"pp", "mm", "pm", "mp"};
    const char *chType[] = {"LS", "US"};

    TFile* outFile = new TFile(outfile,"RECREATE");

    for (int iCut=0;iCut<nCut;iCut++) {
        // Reset histograms for this cut. Will sum over zBuffers.
        inFile->cd();
        for (int irel=0;irel<2;irel++) {
            for (int ich=0;ich<2;ich++) {
                for (int ipos=0;ipos<4;ipos++) {
                    inFile->GetObject("SibppTPCAvgTSep_cutBin_0_zBuf_0",tmp1D);
                    for (int idir=0;idir<2;idir++) {
                        sep1D[irel][ich][ipos][idir] = (TH1D *) tmp1D->Clone();
                        sep1D[irel][ich][ipos][idir]->Clear();
                    }
                }
                inFile->GetObject("SibppTPCMidTdptP_cutBin_0_zBuf_0",tmp1D);
                for (int ity=0;ity<2;ity++) {
                    for (int idir=0;idir<2;idir++) {
                        dpt1D[irel][ich][ity][idir] = (TH1D *) tmp1D->Clone();
                        dpt1D[irel][ich][ity][idir]->Clear();
                    }
                }
                inFile->GetObject("SibppTPCAvgTZ_cutBin_0_zBuf_0",tmp2D);
                for (int ipos=0;ipos<4;ipos++) {
                    sep2D[irel][ich][ipos] = (TH2D *) tmp2D->Clone();
                    sep2D[irel][ich][ipos]->Clear();
                }
                inFile->GetObject("SibppTPCMidTZC_cutBin_0_zBuf_0",tmp2D);
                for (int icr=0;icr<2;icr++) {
                    cross[irel][ich][icr] = (TH2D *) tmp2D->Clone();
                    cross[irel][ich][icr]->Clear();
                }
                inFile->GetObject("SibppTPCEntTdpt_cutBin_0_zBuf_0",tmp2D);
                for (int ipos=0;ipos<3;ipos++) {
                    dpt2D[irel][ich][ipos] = (TH2D *) tmp2D->Clone();
                    dpt2D[irel][ich][ipos]->Clear();
                }
            }
        }

        for (int zBuf=0;zBuf<nZBuf;zBuf++) {
            // First do 1D T, Z separations.
            for (int ipos=0;ipos<4;ipos++) {
                for (int idir=0;idir<2;idir++) {
                    for (int irel=0;irel<2;irel++) {
                        for (int ich=0;ich<4;ich++) {
                            hName = rel[irel]; hName += ch[ich]; hName += "TPC"; hName += pos[ipos]; hName += dir[idir];
                            hName += "Sep_cutBin_"; hName += iCut; hName += "_zBuf_"; hName += zBuf;
                            inFile->GetObject(hName.Data(),tmp1D);
                            sep1D[irel][ich/2][ipos][idir]->Add(tmp1D);
                            Sum_sep1D[irel][ich/2][ipos][idir]->Add(tmp1D);
                        }
                    }
                }
            }
            // Now 1D T*dpt, Z*dpt for P and N curvature
            for (int ity=0;ity<2;ity++) {
                for (int idir=0;idir<2;idir++) {
                    for (int irel=0;irel<2;irel++) {
                        for (int ich=0;ich<4;ich++) {
                            hName = rel[irel]; hName += ch[ich]; hName += "TPCMid"; hName += dir[idir]; hName += "dpt"; hName += ty[ity];
                            hName += "_cutBin_"; hName += iCut; hName += "_zBuf_"; hName += zBuf;
                            inFile->GetObject(hName.Data(),tmp1D);
                            dpt1D[irel][ich/2][ity][idir]->Add(tmp1D);
                            Sum_dpt1D[irel][ich/2][ity][idir]->Add(tmp1D);
                        }
                    }
                }
            }
            // Now for 2D T-Z separations.
            for (int ipos=0;ipos<4;ipos++) {
                for (int irel=0;irel<2;irel++) {
                    for (int ich=0;ich<4;ich++) {
                        hName = rel[irel]; hName += ch[ich]; hName += "TPC"; hName += pos[ipos];
                        hName += "TZ_cutBin_"; hName += iCut; hName += "_zBuf_"; hName += zBuf;
                        inFile->GetObject(hName.Data(),tmp2D);
                        sep2D[irel][ich/2][ipos]->Add(tmp2D);
                        Sum_sep2D[irel][ich/2][ipos]->Add(tmp2D);
                    }
                }
            }
            // Now 2D T, Z - dpt separations
            for (int ipos=0;ipos<3;ipos++) {
                for (int irel=0;irel<2;irel++) {
                    for (int ich=0;ich<4;ich++) {
                        hName = rel[irel]; hName += ch[ich]; hName += "TPC"; hName += pos[ipos];
                        hName += "Tdpt_cutBin_"; hName += iCut; hName += "_zBuf_"; hName += zBuf;
                        inFile->GetObject(hName.Data(),tmp2D);
                        dpt2D[irel][ich/2][ipos]->Add(tmp2D);
                        Sum_dpt2D[irel][ich/2][ipos]->Add(tmp2D);
                    }
                }
            }
            // Finally, 2D T-Z separations dependent on crossing or not.
            for (int icr=0;icr<2;icr++) {
                for (int irel=0;irel<2;irel++) {
                    for (int ich=0;ich<4;ich++) {
                        hName = rel[irel]; hName += ch[ich]; hName += "TPCMidTZ"; hName += crs[icr];
                        hName += "_cutBin_"; hName += iCut; hName += "_zBuf_"; hName += zBuf;
                        inFile->GetObject(hName.Data(),tmp2D);
                        cross[irel][ich/2][icr]->Add(tmp2D);
                        Sum_cross[irel][ich/2][icr]->Add(tmp2D);
                    }
                }
            }
        }

        // Calculate sibling/mixed (normalized) and write.
        // Also want to set the name appropriately for when we read these from a file.
        outFile->cd();

        for (int ipos=0;ipos<4;ipos++) {
            for (int idir=0;idir<2;idir++) {
                for (int ich=0;ich<2;ich++) {
                    sep1D[0][ich][ipos][idir]->Scale(sep1D[1][ich][ipos][idir]->Integral()/sep1D[0][ich][ipos][idir]->Integral());
                    sep1D[0][ich][ipos][idir]->Divide(sep1D[1][ich][ipos][idir]);
                    hName = chType[ich]; hName += pos[ipos]; hName += dir[idir]; hName += "Sep_cutBin_"; hName += iCut; 
                    sep1D[0][ich][ipos][idir]->SetName(hName.Data());
                    sep1D[0][ich][ipos][idir]->SetTitle(hName.Data());
                    sep1D[0][ich][ipos][idir]->Write();
                }
            }
        }
        for (int ity=0;ity<2;ity++) {
            for (int idir=0;idir<2;idir++) {
                for (int ich=0;ich<2;ich++) {
                    dpt1D[0][ich][ity][idir]->Scale(dpt1D[1][ich][ity][idir]->Integral()/dpt1D[0][ich][ity][idir]->Integral());
                    dpt1D[0][ich][ity][idir]->Divide(dpt1D[1][ich][ity][idir]);
                    hName = chType[ich]; hName += dir[idir]; hName += ty[ity]; hName += "_cutBin_"; hName += iCut; 
                    dpt1D[0][ich][ity][idir]->SetName(hName.Data());
                    dpt1D[0][ich][ity][idir]->SetTitle(hName.Data());
                    dpt1D[0][ich][ity][idir]->Write();
                }
            }
        }
        for (int ipos=0;ipos<4;ipos++) {
            for (int ich=0;ich<2;ich++) {
                sep2D[0][ich][ipos]->Scale(sep2D[1][ich][ipos]->Integral()/sep2D[0][ich][ipos]->Integral());
                sep2D[0][ich][ipos]->Divide(sep2D[1][ich][ipos]);
                hName = chType[ich]; hName += pos[ipos]; hName += "TZ_cutBin_"; hName += iCut; 
                sep2D[0][ich][ipos]->SetName(hName.Data());
                sep2D[0][ich][ipos]->SetTitle(hName.Data());
                sep2D[0][ich][ipos]->Write();
            }
        }
        for (int ipos=0;ipos<3;ipos++) {
            for (int ich=0;ich<2;ich++) {
                dpt2D[0][ich][ipos]->Scale(dpt2D[1][ich][ipos]->Integral()/dpt2D[0][ich][ipos]->Integral());
                dpt2D[0][ich][ipos]->Divide(dpt2D[1][ich][ipos]);
                hName = chType[ich]; hName += pos[ipos]; hName += "Tdpt_cutBin_"; hName += iCut; 
                dpt2D[0][ich][ipos]->SetName(hName.Data());
                dpt2D[0][ich][ipos]->SetTitle(hName.Data());
                dpt2D[0][ich][ipos]->Write();
            }
        }
        for (int icr=0;icr<2;icr++) {
            for (int ich=0;ich<2;ich++) {
                cross[0][ich][icr]->Scale(cross[1][ich][icr]->Integral()/cross[0][ich][icr]->Integral());
                cross[0][ich][icr]->Divide(cross[1][ich][icr]);
                hName = chType[ich]; hName += "MidTZ"; hName += crs[icr]; hName += "_cutBin_"; hName += iCut; 
                cross[0][ich][icr]->SetName(hName.Data());
                cross[0][ich][icr]->SetTitle(hName.Data());
                cross[0][ich][icr]->Write();
            }
        }

        for (int irel=0;irel<2;irel++) {
            for (int ipos=0;ipos<4;ipos++) {
                for (int idir=0;idir<2;idir++) {
                    for (int ich=0;ich<2;ich++) {
                        delete sep1D[irel][ich][ipos][idir];
                    }
                }
            }
            for (int ity=0;ity<2;ity++) {
                for (int idir=0;idir<2;idir++) {
                    for (int ich=0;ich<2;ich++) {
                        delete dpt1D[irel][ich][ity][idir];
                    }
                }
            }
            for (int ipos=0;ipos<4;ipos++) {
                for (int ich=0;ich<2;ich++) {
                    delete sep2D[irel][ich][ipos];
                }
            }
            for (int ipos=0;ipos<3;ipos++) {
                for (int ich=0;ich<2;ich++) {
                    delete dpt2D[irel][ich][ipos];
                }
            }
            for (int icr=0;icr<2;icr++) {
                for (int ich=0;ich<2;ich++) {
                    delete cross[irel][ich][icr];
                }
            }
        }
    }

    // Finished loop over cut bins.
    // Now calculate and write ratios for summed cut bins.
    outFile->cd();

    for (int ipos=0;ipos<4;ipos++) {
        for (int idir=0;idir<2;idir++) {
            for (int ich=0;ich<2;ich++) {
                Sum_sep1D[0][ich][ipos][idir]->Scale(Sum_sep1D[1][ich][ipos][idir]->Integral()/Sum_sep1D[0][ich][ipos][idir]->Integral());
                Sum_sep1D[0][ich][ipos][idir]->Divide(Sum_sep1D[1][ich][ipos][idir]);
                hName = chType[ich]; hName += pos[ipos]; hName += dir[idir]; hName += "Sep";
                Sum_sep1D[0][ich][ipos][idir]->SetName(hName.Data());
                Sum_sep1D[0][ich][ipos][idir]->SetTitle(hName.Data());
                Sum_sep1D[0][ich][ipos][idir]->Write();
            }
        }
    }
    for (int ity=0;ity<2;ity++) {
        for (int idir=0;idir<2;idir++) {
            for (int ich=0;ich<2;ich++) {
                Sum_dpt1D[0][ich][ity][idir]->Scale(Sum_dpt1D[1][ich][ity][idir]->Integral()/Sum_dpt1D[0][ich][ity][idir]->Integral());
                Sum_dpt1D[0][ich][ity][idir]->Divide(Sum_dpt1D[1][ich][ity][idir]);
                hName = chType[ich]; hName += dir[idir]; hName += ty[ity];
                Sum_dpt1D[0][ich][ity][idir]->SetName(hName.Data());
                Sum_dpt1D[0][ich][ity][idir]->SetTitle(hName.Data());
                Sum_dpt1D[0][ich][ity][idir]->Write();
            }
        }
    }
    for (int ipos=0;ipos<4;ipos++) {
        for (int ich=0;ich<2;ich++) {
            Sum_sep2D[0][ich][ipos]->Scale(Sum_sep2D[1][ich][ipos]->Integral()/Sum_sep2D[0][ich][ipos]->Integral());
            Sum_sep2D[0][ich][ipos]->Divide(Sum_sep2D[1][ich][ipos]);
            hName = chType[ich]; hName += pos[ipos]; hName += "TZ";
            Sum_sep2D[0][ich][ipos]->SetName(hName.Data());
            Sum_sep2D[0][ich][ipos]->SetTitle(hName.Data());
            Sum_sep2D[0][ich][ipos]->Write();
        }
    }
    for (int ipos=0;ipos<3;ipos++) {
        for (int ich=0;ich<2;ich++) {
            Sum_dpt2D[0][ich][ipos]->Scale(Sum_dpt2D[1][ich][ipos]->Integral()/Sum_dpt2D[0][ich][ipos]->Integral());
            Sum_dpt2D[0][ich][ipos]->Divide(Sum_dpt2D[1][ich][ipos]);
            hName = chType[ich]; hName += pos[ipos]; hName += "Tdpt";
            Sum_dpt2D[0][ich][ipos]->SetName(hName.Data());
            Sum_dpt2D[0][ich][ipos]->SetTitle(hName.Data());
            Sum_dpt2D[0][ich][ipos]->Write();
        }
    }
    for (int icr=0;icr<2;icr++) {
        for (int ich=0;ich<2;ich++) {
            Sum_cross[0][ich][icr]->Scale(Sum_cross[1][ich][icr]->Integral()/Sum_cross[0][ich][icr]->Integral());
            Sum_cross[0][ich][icr]->Divide(Sum_cross[1][ich][icr]);
            hName = chType[ich]; hName += "MidTZ"; hName += crs[icr];
            Sum_cross[0][ich][icr]->SetName(hName.Data());
            Sum_cross[0][ich][icr]->SetTitle(hName.Data());
            Sum_cross[0][ich][icr]->Write();
        }
    }
    // Maybe we need to delete histograms or garbage collection gets confused.
    // (Maybe we just have to be careful to not confuse root into attempting to transfer
    //  histograms from input to output files.)


    for (int irel=0;irel<2;irel++) {
        for (int ipos=0;ipos<4;ipos++) {
            for (int idir=0;idir<2;idir++) {
                for (int ich=0;ich<2;ich++) {
                    delete Sum_sep1D[irel][ich][ipos][idir];
                }
            }
        }
        for (int ity=0;ity<2;ity++) {
            for (int idir=0;idir<2;idir++) {
                for (int ich=0;ich<2;ich++) {
                    delete Sum_dpt1D[irel][ich][ity][idir];
                }
            }
        }
        for (int ipos=0;ipos<4;ipos++) {
            for (int ich=0;ich<2;ich++) {
                delete Sum_sep2D[irel][ich][ipos];
            }
        }
        for (int ipos=0;ipos<3;ipos++) {
            for (int ich=0;ich<2;ich++) {
                delete Sum_dpt2D[irel][ich][ipos];
            }
        }
        for (int icr=0;icr<2;icr++) {
            for (int ich=0;ich<2;ich++) {
                delete Sum_cross[irel][ich][icr];
            }
        }
    }

    outFile->Close();
    delete outFile;
}

//------------------------------------------------------------------------
void StEStructHAdd::combineUS(TFile * modFile) {

    // Add the -+ to the +- and clear the -+.
    // This is for pid case when we are creating all. For non-identical particles we
    // distinguish between +- and -+ but for identical particles we do not. In the
    // case of 'all', which we make from summing all other cases, we don't
    // want to distinguish +- and -+.

    const char* base[]={"Sib","Mix"};
    const char* tpe[]={"pp","pm","mp","mm"};
    const char* knd[]={"YtYt",     "NYtYt",     "PtPt",
                       "PhiPhi",   "NPhiPhi",   "PrPhiPhi",   "PaPhiPhi",   "PbPhiPhi",
                       "EtaEta",                "PrEtaEta",   "PaEtaEta",   "PbEtaEta",
                       "DEtaDPhi", "NDEtaDPhi", "PrDEtaDPhi", "PaDEtaDPhi", "PbDEtaDPhi",
                       "SYtDYt",   "NSYtDYt",
                       "SEtaDPhi", "NSEtaDPhi", "PrSEtaDPhi", "PaSEtaDPhi", "PbSEtaDPhi",
                       "Qinv",     "NQinv"
                       "DEtaDPhiRot", "NDEtaDPhiRot", "PrDEtaDPhiRot", "PaDEtaDPhiRot", "PbDEtaDPhiRot"};

//    TFile* modFile = new TFile(fileName,"UPDATE");

    for (int i=0;i<2;i++) {
        for (int k=0;k<28;k++) {

            TH1 *hpm=0, *hmp=0;
            int zBin = 0;
            while (zBin<99) {
                if (k<31) {
                    // Histogram name is of form
                    //    SibppDEtaDPhi_cutBin_n_zBuf_m
                    // where n is the usual cut bin number and m is the zbuffer index.
                    TString pmHist; pmHist+=base[i]; pmHist+=tpe[1]; pmHist+=knd[k];
                    pmHist+="_zBuf_"; pmHist+=zBin;
                    hpm=(TH1*)modFile->Get(pmHist.Data());
                    if (!hpm) {
                        // Have either processed all zbins or this histogram (at least for zBuf=0) isn't in the file.
                        goto lastZ;
                    }
                    TString mpHist; mpHist+=base[i]; mpHist+=tpe[2]; mpHist+=knd[k];
                    mpHist+="_zBuf_"; mpHist+=zBin;
                    hmp=(TH1*)modFile->Get(mpHist.Data());
                    hpm->Add(hmp);
                    hmp->Scale(0);
                    modFile->cd();
                    hpm->Write();
                    hmp->Write();
                }
                zBin++;
            }
            lastZ:
            continue;
        }
    }
//    modFile->Close();
};


/***********************************************************************
 *
 * $Log: StEStructHAdd.cxx,v $
 * Revision 1.17  2012/11/16 21:27:23  prindle
 * AS, SS histograms. Get eta limits from histogram in file. Check for empty bins in ratio (showed up as offset in correlations). Scale histograms according to actual number of pairs, don't assume 1/2 for example.
 *
 * Revision 1.16  2010/06/23 22:33:41  prindle
 *   In HAdd we distinguish between the parent distributions of the
 *    two particles.
 *   In Support I fixed a number of problems in the Pt correlation section.
 *
 * Revision 1.15  2010/03/02 21:48:30  prindle
 *   Fix addDensities (for checking pair cuts)
 *   Lots of small changes
 *
 * Revision 1.14  2009/11/09 21:32:59  prindle
 * Fix warnings about casting char * to a const char * by redeclaring as const char *.
 *
 * Revision 1.13  2009/05/08 00:21:42  prindle
 * In StEStructHadd remove support for old style of histogram names, do a better job calculating
 * errors (at least for number (\eta_\Delta,\phi_\Delta) histograms), double bins which
 * have an edge in the center (purely cosmetic when looking at intermediate histograms).
 * In StEStructSupport check for existance of histograms and return gracefully.
 * Code in buildChargeTypes and buildPtChargeTypes was essentially duplicate of code
 * in buildCommon and buildPtCommon so I refactored to reduce redundancy.
 *
 * Revision 1.12  2008/12/02 23:52:51  prindle
 * Get information about histogram XX being symmetrized from CutBin.
 * Changed TH1* to TH2D* in many places hoping to be able to plot DEtaDPhi
 * as colz (doesn't work yet).
 * Added direct calculation of \Delta\rho/\rho_{ref} (and  similar) which is
 * needed for YtYt correlations.
 *
 * Revision 1.11  2008/05/01 23:46:39  prindle
 *   Changed to use TH1D and TH2D (instead of TH1 and TH2) in some places so
 * we can use GetObject method to enforce type checking. Found I had missed
 * duplicating a \phi_\Delta row in one case. Also added a method to include
 * sum of pairdensity histograms in output file.
 *
 * Revision 1.10  2007/11/26 20:23:20  prindle
 * (Hadd to trick cvs to allow me to commit this modified file?)
 * Modifications to support saving of individual z-bins and the concept
 * of parent distributions important for pid analysis.
 *
 * Revision 1.9  2007/06/26 20:22:17  msd
 * Very minor bug fix
 *
 * Revision 1.8  2007/05/27 22:46:00  msd
 * Added buildChargeTypes mode 3 which takes rho_ref from track counts.
 * Added buildChargeTypeSumOfRatios.
 *
 * Revision 1.7  2007/01/26 17:20:57  msd
 * Updated HAdd for new binning scheme.
 * Improved Support::buildChargeTypes.
 *
 * Revision 1.6  2006/10/02 22:26:48  prindle
 * Hadd now symmetrizes histograms while adding them, so output is usable
 * in Support as before. Need to load library for Correlation so we know
 * how many bins there are.
 * Added  alternative versions of methods to calculate Delta\sigma^2.
 * Important for pt correlations where we need proper normalization before
 * subtracting mixed reference.
 *
 * Revision 1.5  2006/04/06 01:09:46  prindle
 *   Calculating pt for each cut bin caused changes in HAdd.
 * The splitting of +- into +- and -+ caused changes in Support.
 *
 * Revision 1.4  2005/09/29 17:42:14  msd
 * Added Xt to hadd
 *
 * Revision 1.3  2005/03/08 21:56:42  porter
 * fixed bug in StEStructHAdd.cxx and added diagnostic option in ptcorrelations to
 * view individual terms separately
 *
 * Revision 1.2  2005/03/03 01:33:04  porter
 * Added pt-correlations method to support and included
 * these histograms to the HAdd routine
 *
 * Revision 1.1  2004/07/01 00:37:13  porter
 * new code previously my StEStructHelper. Takes hists from correltation
 * pass and builds final ressults.  Also the StEStructHAdd.h is a simple
 * replacemnt for my sumyt.C macro which could be expanded later as needed.
 *
 *
 *
 *********************************************************************/



