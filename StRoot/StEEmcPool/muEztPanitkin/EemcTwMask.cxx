#include <cstdlib>

#include <StMessMgr.h>

#include "EemcTwMask.h"

//--------------------------------------
EemcTwMask::EemcTwMask()
    : txtH(0)
    , nMask(0)
{
}

//--------------------------------------
EemcTwMask::~EemcTwMask() {
    if (txtH) delete txtH; txtH = 0;
}

//--------------------------------------
void EemcTwMask::clear() {
    for (int i = 0;i < nCr;i++) {
        crG[i].Clear();
        crG2[i].Clear();
    }
    phiG.Clear();
    if (txtH) txtH->Clear();
    nMask = 0;
    memset(crCh, 0, sizeof(crCh));
}

//--------------------------------------
bool useTwMask(const Char_t *fname, EemcTwMask *m) {
  if (!(fname && fname[0] && m)) return false;
  const int mx=1000;
  Char_t buf[mx];

  LOG_INFO << "EEqaPresenter::useTwMask(\"" << fname << "\") ..." << endm;

  m->clear();
  FILE *fd = fopen(fname, "r");
  int nok = 0;
  int nM = 1;
  m->txtH = new TPaveText(0, 0., 1, 1);
  TString myTxt = "ETOW masked (cr-ch-name): ";
  if (!fd) goto abandon;
  while (1) {
    Char_t *ret = fgets(buf, mx, fd);
    if (ret == 0) break ; //EOF
    if (buf[0] == '#') continue; // skip comment
    if (buf[0] == '\n') continue; // skip empty lines
    Char_t name[100];
    int cr, ch;
    int sec, isub, jeta, jphi;
    int n = sscanf(buf, "%d %d %s", &cr, &ch, name);
    if (n != 3) goto abandon;
    if ((cr < 1) || (cr > m->nCr) || (ch < 0) || (ch >= m->nCh)) goto abandon;
    if (name[2] != 'T') goto abandon;
    sec = atoi(name);
    isub = name[3] - 'A';
    jeta = atoi(name + 4);
    jphi = (sec - 1) * 5 + isub;
    // LOG_DEBUG << Form("%s sec=%d isub=%d jeta=%d, jphi=%d\n",name, sec,isub,jeta,jphi) << endm;

    m->crCh[cr-1][ch] = 1;
    int jj = m->crG[cr - 1].GetN();
    m->crG[cr - 1].SetPoint(jj, ch, 10);
    m->crG2[cr - 1].SetPoint(jj, 250, ch);
    m->crG2[cr - 1].SetPointError(jj, 250, 0.);

    jj = m->phiG.GetN();
    m->phiG.SetPoint(jj, jphi, jeta);
    TString tt = Form("%d-%d-%s,  ", cr, ch, name);
    myTxt += tt;
    nok++;
    nM++;
    LOG_DEBUG << Form("mask ETOW cr=%d ch=%d =%s=",cr,ch,name) << endm;
    if((nM % 4) == 0) {
       m->txtH->AddText(myTxt);
       myTxt=" ";
    }
  }
  m->nMask = nok;
  m->txtH->AddText(myTxt);
  m->txtH->AddText("--");
  LOG_INFO << " got " << nok << " masked towers" << endm;

  for (int i = 0;i < 6;i++){
    m->crG[i].SetMarkerStyle(23);
    m->crG[i].SetMarkerColor(kRed);
    m->crG2[i].SetMarkerStyle(1);
    m->crG2[i].SetLineColor(kRed);
  }

  m->phiG.SetMarkerStyle(24);

  return true;
abandon: // any error has happened (this is a new approach for me, JB)
  m->clear();
  m->txtH->AddText("List of ETOW hot towers not found");
  m->txtH->AddText("--");
  LOG_ERROR << " EEqaPresenter::useTwMask() FAILED" << endm;
  return false;
}
