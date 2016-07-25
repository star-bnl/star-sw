void set(Int_t color=1) {
  Char_t *histos[] = {"MuDst","FitP","mu","sigma","dEdxP","dEdxS","SumT",0};
  for (int i = 0; histos[i]; i++) {
    TObject *obj = gDirectory->Get(histos[i]);
    if (! obj) continue;
    if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
      TH1 *h1 = (TH1*)obj;
      h1->SetMarkerStyle(20);
      h1->SetMarkerColor(color);
      h1->SetLineColor(color);
      h1->SetLineWidth(2);
      h1->SetMarkerSize(0.3);
    }
    else { 
      if ( obj->IsA()->InheritsFrom( "TTree" ) ) {
	TTree *t = (TTree*)obj;
	t->SetMarkerStyle(20);
	t->SetMarkerColor(color);
	t->SetLineColor(color);
	t->SetLineWidth(2);
	//	t->SetMarkerSize(0.3);
      }
    }
  }
}
