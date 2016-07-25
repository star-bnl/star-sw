TProfile *all = 0;
TProfile *allW= 0;
void Prof() {
  TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
  if (! FitP) return;
  Char_t* Names[7] = {"e","p","K","pi","mu","d","t"};
#ifdef bg
  all  = new TProfile("all","all",60,-2,4); all->SetMarkerStyle(20);
#else
  all  = new TProfile("all","all",60,0,60); all->SetMarkerStyle(20);
#endif
#ifdef W
  allW = new TProfile("allW","allW",60,-2,4);allW->SetMarkerStyle(20); allW->SetMarkerColor(2);
#endif
  for (Int_t i = 0; i < 7; i++) {
#ifdef bg
    TString draw(Form("Delta%s:bg10%s>>",Names[i],Names[i]));
#else
    TString draw(Form("exp(Delta%s):exp(RefZ%s+Delta%s)>>",Names[i],Names[i],Names[i]));
#endif
    if (i == 0) draw += "all";
    else        draw += "+all";
    TString cut(Form("Phi%s>0.",Names[i]));
    FitP->SetMarkerStyle(20);
    FitP->Draw(draw,cut);
#ifdef W
    draw += "W";
    TString cutW(Form("Phi%s>0.&&1./(errDelta%s*errDelta%s)",Names[i],Names[i],Names[i]));
    FitP->SetMarkerColor(2);
    FitP->Draw(draw,cutW);
#endif
  }
}
