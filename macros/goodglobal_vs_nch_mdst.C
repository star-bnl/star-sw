const Float_t kELMASS = 0.00051099907;
const Float_t kPMASS  = 0.93827231;
const Float_t kKMASS  = 0.49367;
const Float_t kPIMASS = 0.13957;

const Char_t *label_str[3]={"40 #font[12]{A}#upointGeV",
			    "80 #font[12]{A}#upointGeV",
			    "158 #font[12]{A}#upointGeV"};

void grey_palette()
{
  col=(TColor*) gROOT->GetListOfColors()->At(2);
  col->SetRGB(0.8,0.8,0.8);
  col=(TColor*) gROOT->GetListOfColors()->At(3);
  col->SetRGB(0.6,0.6,0.6);
  Int_t palette[20];
  for (Int_t i=0;i<20; i++) {
    col=(TColor*) gROOT->GetListOfColors()->At(50+i);
    col->SetRGB(1-0.05*i,1-0.05*i,1-0.05*i);
    palette[i]=50+i;
  }
  gStyle->SetPalette(15,palette);
}

void draw_numbers(TCanvas *c1,Int_t n_pad) {
  TLatex ltx;
  ltx.SetNDC();
  ltx.SetTextSize(0.07);
  for (Int_t i=0; i< n_pad; i++) {
    c1->cd(0);
    TVirtualPad::Pad()->cd(i+1);
    Char_t num[10];
    sprintf(num,"%c)",97+i);
    cout << TVirtualPad::Pad() << " " << num << endl;
    ltx.DrawLatex(0.22,0.8,num);
  }
}

Float_t get_txt_size(TVirtualPad *pad, const Float_t pix_size) {
  if (pad->UtoPixel(1) > pad->VtoPixel(0)) {
    // 'Horizontal pad'
    return pix_size/pad->VtoPixel(0);
  }
  else {
    // 'Vertical pad'
    return pix_size/pad->UtoPixel(1);
  }
}

void set_text_sizes(TVirtualPad *pad, TH1F* hist) {
  const Float_t label_size=9;
  const Float_t title_size=11;
  hist->SetLabelSize(get_txt_size(pad,label_size));
  hist->SetLabelSize(get_txt_size(pad,label_size),"Y");
  hist->SetTitleSize(get_txt_size(pad,title_size));
  hist->SetTitleSize(get_txt_size(pad,title_size),"Y");
}


TCanvas *make_canvas(const Char_t *name, const Char_t *title, Int_t n_x=1, Int_t n_y=1, Int_t share_axes=0, Int_t ww=0, Int_t wh=0){
  const Int_t width=350;
  const Int_t height=350;
  TCanvas *canvas;
  if (share_axes==0) {
    if (ww==0)
      ww=width*n_x;
    if (wh==0)
      wh=height*n_y;
    canvas=new TCanvas(name,title,ww,wh);
    canvas->Divide(n_x,n_y,0,0);
  }
  else {
    Float_t pix_width=(1-gStyle->GetPadLeftMargin()-gStyle->GetPadRightMargin())*width;
    Float_t pix_height=(1-gStyle->GetPadTopMargin()-gStyle->GetPadBottomMargin())*height;
    if (ww==0)
      ww=width+(n_x-1)*pix_width;
    if (wh==0)
      wh=height+(n_y-1)*pix_height;

    canvas=new TCanvas(name,title,ww,wh);
    Float_t tot_width;
    if (n_x>1) 
      tot_width=(n_x-2)+1./(1-gStyle->GetPadLeftMargin())
	+1./(1-gStyle->GetPadRightMargin());
    else
      tot_width=1./(1-gStyle->GetPadLeftMargin()-gStyle->GetPadRightMargin());
    Float_t tot_height;
    if (n_y>1) 
      tot_height=(n_y-2)+1./(1-gStyle->GetPadTopMargin())
	+1./(1-gStyle->GetPadBottomMargin());
    else
      tot_height=1./(1-gStyle->GetPadTopMargin()-gStyle->GetPadBottomMargin());

    //Int_t idx=n_x*n_y;
    
    for (Int_t j=0; j<n_y; j++) {
      for (Int_t i=0; i<n_x; i++) {
	//for (Int_t j=n_y-1; j>=0; j--) {
	//for (Int_t i=n_x-1; i>=0; i--) {
	Char_t tmp_str[256];
	Char_t p_title[256];
	Int_t idx=n_x*j+i+1;
	sprintf(tmp_str,"%s_%d",canvas->GetName(),idx);
	sprintf(p_title,"Pad %d",idx);
	Float_t x1=0,y1=0;
	Float_t x2=1,y2=1;
	if (n_x>1) {
	  if (i==0) 
	    x2=1./(1-gStyle->GetPadLeftMargin())/tot_width;
	  else {
	    x1=(1./(1-gStyle->GetPadLeftMargin())+i-1)/tot_width;
	    if (i<n_x-1)
	      x2=(1./(1-gStyle->GetPadLeftMargin())+i)/tot_width;
	  }
	}
	if (n_y>1) {
	  if (j==0) 
	    y1=1-1./(1-gStyle->GetPadTopMargin())/tot_height;
	  else {
	    y2=1-(1./(1-gStyle->GetPadTopMargin())+j-1)/tot_height;
	    if (j<n_y-1)
	      y1=1-(1./(1-gStyle->GetPadTopMargin())+j)/tot_height;
	  }
	}
	//cout << "x1 " << x1 << ", x2 " << x2 << endl;
	TPad *pad=new TPad(tmp_str,title,x1,y1,x2,y2);
	//pad->SetFillColor(idx+1);
	if (i>0)
	  pad->SetLeftMargin(0.001);
	if (i<n_x-1)
	  pad->SetRightMargin(0.001);
	if (j>0)
	  pad->SetTopMargin(0.001);
	if (j<n_y-1)
	  pad->SetBottomMargin(0.001);

	pad->SetNumber(idx);
	//pad->SetNumber(n_x*j+i+1);
	pad->Draw();
	//idx--;
	//idx++;
	//pad->SetP
      }
    }
  }

  return canvas;
}

TPolyLine3D *draw_func_3d(TF1 *f1) {
  Double_t xmin,xmax;
  Bool_t logx=TVirtualPad::Pad()->GetLogx();
  Bool_t logy=TVirtualPad::Pad()->GetLogy();
  Float_t vmin[3],vmax[3];
  TView *view=TVirtualPad::Pad()->GetView();
  if (view==0){
    cout << "ERROR in draw_func_3d : no view found" << endl; 
    return 0;
  }
  view->GetRange(vmin,vmax);
  Int_t npmax=f1->GetNpx();
  Float_t *xx=new Float_t[npmax];
  Float_t *yy=new Float_t[npmax];
  Float_t *zz=new Float_t[npmax];
  f1->GetRange(xmin,xmax);
  if (logx) {
    if (xmin<=0)
      xmin=vmin[0];
    else
      xmin=log10(xmin);
    xmax=log10(xmax);
  }  
  Float_t step=(xmax-xmin)/(npmax-1);
  Int_t np=0;
  for (Int_t i=0; i < npmax; i++) {
    Float_t x=xmin+step*i;
    Float_t y;
    if (logx)
      y=f1->Eval(pow(10,x));
    else
      y=f1->Eval(x);
    if (logy) {
      if (y<=0)
	y=vmin[1];
      else
	y=log10(y);
    }
    if (y>=vmin[1] && y<=vmax[1] &&
	x>=vmin[0] && x<=vmax[0]) {
      xx[np]=x;
      yy[np]=y;
      zz[np]=vmin[2];
      np++;
    }
  }
  TPolyLine3D *poly=new TPolyLine3D(np,xx,yy,zz);
  poly->SetLineStyle(f1->GetLineStyle());
  poly->SetLineColor(f1->GetLineColor());
  poly->Draw();
  delete [] xx;
  delete [] yy;
  delete [] zz;
  return poly;
}

TH2F *rebin_2d(TH2F *hist,Int_t nx=2,Int_t ny=2,const Char_t *name) { 
  
  Float_t xmin=hist->GetXaxis()->GetXmin();
  Float_t xmax=hist->GetXaxis()->GetXmax();
  Float_t ymin=hist->GetYaxis()->GetXmin();
  Float_t ymax=hist->GetYaxis()->GetXmax();
  
  Int_t nb_x=hist->GetXaxis()->GetNbins();
  nb_x=nb_x%nx?nb_x/nx+1:nb_x/nx;
  xmax=xmin+nb_x*nx*hist->GetXaxis()->GetBinWidth(1);
  Int_t nb_y=hist->GetYaxis()->GetNbins();
  nb_y=nb_x%ny?nb_y/ny+1:nb_y/ny;
  ymax=ymin+nb_y*ny*hist->GetYaxis()->GetBinWidth(1);
  
  TH2F *n_hist=new TH2F(name,hist->GetTitle(),nb_x,xmin,xmax,nb_y,ymin,ymax);

  for (Int_t bx=1; bx<=nb_x; bx++)
    for (Int_t by=1; by<=nb_y; by++) 
      n_hist->Fill(hist->GetXaxis()->GetBinCenter(bx),hist->GetYaxis()->GetBinCenter(by),hist->GetCellContent(bx,by));
   
  return n_hist;
}

TPad *overlay_2d(const Char_t *name="ovl_p",Char_t *title="Overlay pad"){
  TPad *ovl_p=new TPad(name,title,0,0,1,1);
  ovl_p->SetFillStyle(4000);
  ovl_p->RangeAxis(0,0,2,2);
  Float_t dxr=2./(1-TVirtualPad::Pad()->GetLeftMargin()-TVirtualPad::Pad()->GetRightMargin());
  Float_t dyr=2./(1-TVirtualPad::Pad()->GetTopMargin()-TVirtualPad::Pad()->GetBottomMargin());
  ovl_p->Range(-TVirtualPad::Pad()->GetLeftMargin()*dxr,-TVirtualPad::Pad()->GetBottomMargin()*dyr,
	       2+TVirtualPad::Pad()->GetRightMargin()*dxr,2+TVirtualPad::Pad()->GetTopMargin()*dyr);
  ovl_p->RangeAxis(0,0,2,2);
  ovl_p->Draw();
  ovl_p->cd();
  return ovl_p;
}

TGraph *draw_graph_error(TGraphErrors *gr) {
  Int_t np=gr->GetN();
  TGraph *tmp_gr=new TGraph(2*np);
  for (Int_t i=0; i<np; i++) {
    Double_t x,y;
    gr->GetPoint(i,x,y);
    tmp_gr->SetPoint(i,x,y+gr->GetErrorY(i));
    tmp_gr->SetPoint(2*np-i-1,x,y-gr->GetErrorY(i));
  }
  tmp_gr->Draw("f");
  tmp_gr->Draw("l");
  gr->Draw("pX");
  return tmp_gr;
}

void energy_label(Int_t i, Float_t x=0,Float_t y=0, TLatex *ltx=0) {
  if (ltx==0) {
    ltx=new TLatex();
    ltx->SetTextSize(18);
    ltx->SetTextAlign(21);
    ltx->SetNDC();
  }
  if (x==0) 
    x=gPad->GetLeftMargin()+0.5*(1-gPad->GetRightMargin()-gPad->GetLeftMargin());
  if (y==0) 
    y=1-gPad->GetTopMargin()-0.10;

  ltx->DrawLatex(x,y,label_str[i]);
}

Float_t ndc_x(Float_t x) {
  return (1-TVirtualPad::Pad()->GetLeftMargin()-TVirtualPad::Pad()->GetRightMargin())*x+TVirtualPad::Pad()->GetLeftMargin();
}

Float_t ndc_y(Float_t y) {
  return (1-TVirtualPad::Pad()->GetTopMargin()-TVirtualPad::Pad()->GetBottomMargin())*y+TVirtualPad::Pad()->GetBottomMargin();
}

void draw_fill_graph(TGraph *gr, Char_t *opt="p") {
  TGraph *gr_fill=(TGraph*) gr->Clone();
  gr_fill->SetMarkerStyle(gr->GetMarkerStyle()-4);
  gr_fill->SetMarkerColor(10);
  Char_t optx[25];
  sprintf(optx,"%sx",opt);
  gr_fill->Draw(optx);
  gr->Draw(opt);
}

void draw_legend_m(Float_t x, Float_t y, TAttMarker *att_m, const Char_t *label,Int_t i_ent, Int_t colored=1) {
  y-=0.07*i_ent;
  Float_t x_real=TVirtualPad::Pad()->GetUxmin()+(x+0.035)*(TVirtualPad::Pad()->GetUxmax()-TVirtualPad::Pad()->GetUxmin());
  Float_t y_real=TVirtualPad::Pad()->GetUymin()+y*(TVirtualPad::Pad()->GetUymax()-TVirtualPad::Pad()->GetUymin());
  Int_t m_style=att_m->GetMarkerStyle();
  TMarker *mrk=new TMarker(x_real,y_real,m_style);
  Float_t m_siz=att_m->GetMarkerSize();
  mrk->SetMarkerSize(m_siz);
  if (colored)
    mrk->SetMarkerColor(att_m->GetMarkerColor());
  mrk->Draw();
  TLatex *ltx=new TLatex();
  ltx->SetTextFont(133);
  ltx->SetTextSize(16);
  ltx->SetTextAlign(12);
  if (colored)
    ltx->SetTextColor(att_m->GetMarkerColor());
  ltx->SetNDC();
  ltx->DrawLatex(ndc_x(x+0.08),ndc_y(y),label);
}

void draw_legend_l(Float_t x, Float_t y, TAttLine *att_l, const Char_t *label,Int_t i_ent, Size_t txt_size=16) {
  //y-=0.07*i_ent;
  Float_t pix_y_siz=TVirtualPad::Pad()->AbsPixeltoY(1)-TVirtualPad::Pad()->AbsPixeltoY(2);
  y-=pix_y_siz*txt_size*1.05*i_ent/(TVirtualPad::Pad()->GetUymax()-TVirtualPad::Pad()->GetUymin());

  Float_t x_min_real=TVirtualPad::Pad()->GetUxmin()+(x+0.01)*(TVirtualPad::Pad()->GetUxmax()-TVirtualPad::Pad()->GetUxmin());
  Float_t x_max_real=TVirtualPad::Pad()->GetUxmin()+(x+0.08)*(TVirtualPad::Pad()->GetUxmax()-TVirtualPad::Pad()->GetUxmin());
  Float_t y_real=TVirtualPad::Pad()->GetUymin()+y*(TVirtualPad::Pad()->GetUymax()-TVirtualPad::Pad()->GetUymin());
  Int_t l_style=att_l->GetLineStyle();
  if (TVirtualPad::Pad()->GetLogy())
    y_real=exp(log(10)*y_real);
  TLine *line=new TLine(x_min_real,y_real,x_max_real,y_real);
  Float_t l_siz=att_l->GetLineWidth();
  line->SetLineWidth(l_siz);
  line->SetLineStyle(l_style);
  line->SetLineColor(att_l->GetLineColor());
  line->Draw();
  TLatex *ltx=new TLatex();
  ltx->SetTextFont(133);
  ltx->SetTextSize(txt_size);
  ltx->SetTextAlign(12);
  ltx->SetTextColor(att_l->GetLineColor());
  ltx->SetNDC();
  ltx->DrawLatex(ndc_x(x+0.1),ndc_y(y),label);
}

void scale_graph(TGraphErrors *gr,Float_t scale) {
  for (Int_t i=0; i<gr->GetN(); i++) {
    Double_t x,y;
    gr->GetPoint(i,x,y);
    gr->SetPoint(i,x,scale*y);
    gr->SetPointError(i,gr->GetErrorX(i),scale*gr->GetErrorY(i));
  }
}

void scale_graph(TGraph *gr,Float_t scale) {
  for (Int_t i=0; i<gr->GetN(); i++) {
    Double_t x,y;
    gr->GetPoint(i,x,y);
    gr->SetPoint(i,x,scale*y);
  }
}

void set_ratio(TGraphErrors *gr, Int_t pnt, Float_t x_val, Float_t num, Float_t den, Float_t num_err=0, Float_t den_err=0) {
  Float_t rat=num/den;
  gr->SetPoint(i,x_val,rat);
  if (rat!=0) {
    Float_t rat_err=rat*sqrt(num_err*num_err/num/num+den_err*den_err/den/den);
    gr->SetPointError(i,0,rat_err);
  }
}

void set_chi_levels(TH2 *h1, const Int_t n_cont, Double_t *c_levels, Double_t *x_min=0, Double_t *y_min=0) {

  Float_t min_chi=1e10;
  Int_t min_xbin=0, min_ybin=0;
  
  for (Int_t i=1; i <= h1->GetNbinsX(); i++) {
    for (Int_t j=1; j <= h1->GetNbinsY(); j++) {
      if (h1->GetCellContent(i,j) < min_chi) {
	min_xbin=i;
	min_ybin=j;
	min_chi=h1->GetCellContent(i,j);
      }
    }
  }
  
  cout << "min_xbin " << min_xbin << ", " << min_ybin << ", min_chi " << min_chi << endl;

  Double_t *c_levels_tmp=new Double_t[n_cont];
  for (Int_t i=0; i<n_cont;i++) {
    c_levels_tmp[i]=c_levels[i]+min_chi;
    cout << "c_levels[" <<i << "] " << c_levels[i] << ", tmp " << c_levels_tmp[i] << endl;
  }
  h1->SetContour(n_cont,c_levels_tmp);
  //delete c_levels_tmp;
  
  if (x_min) 
    *x_min=h1->GetXaxis()->GetBinCenter(min_xbin);
  if (y_min) 
    *y_min=h1->GetYaxis()->GetBinCenter(min_ybin);

}


void make_gr_file(const Char_t *name,TGraphErrors *gr,const Char_t mode='w') {
  //ios::open_mode m_flag=ios::out|ios::trunc;
  int m_flag=ios_base::out|ios_base::trunc;
  if (mode=='a')
    m_flag=ios_base::out|ios_base::app;
  //  m_flag=ios::out|ios::app;
  ofstream fout(name,m_flag);
  for (Int_t i=0; i< gr->GetN(); i++) {
    Double_t x,y;
    gr->GetPoint(i,x,y);
    fout << x << " " << y;
    fout << " " << gr->GetErrorY(i)<< endl;
  }
  fout.close();
}

// /afs/rhic.bnl.gov/star/users/mvl/macros/goodglobal_vs_nch_mdst.C
void goodglobal_vs_nch_mdst()
{{
  gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  //  gROOT->LoadMacro("/afs/rhic.bnl.gov/star/users/mvl/macros/utils.C");//~/macros/utils.C");
  gStyle->SetPadGridX(0);
  gStyle->SetPadGridY(0);

  TCanvas *c1=new TCanvas("c1","Canvas #1",600,300);
  c1->Divide(2,1,0,0,0);
  TCanvas *c2=new TCanvas("c2","c2: Mult vs ctb",600,300);
  c2->Divide(2,1,0,0,0);
  TCanvas *c3=new TCanvas("c3","c3: Mult vs zdc",600,300);
  c3->Divide(2,1,0,0,0);
  TCanvas *c4=new TCanvas("c4","c4: Vertex distribs",600,300);
  c4->Divide(2,1,0,0,0);
  TCanvas *c5=new TCanvas("c5","c5: # svt hit correlations",600,300);
  c5->Divide(2,1,0,0,0);
  TCanvas *c6=new TCanvas("c6","c6: # hits vs phi",600,300);
  c6->Divide(2,1,0,0,0);
  TCanvas *c7=new TCanvas("c7","c7: dca vs mult",600,300);
  c7->Divide(2,1,0,0,0);

  //muMk=new StMuDstMaker(0,0,"./","5044012.list");
  //muMk=new StMuDstMaker(0,0,"./","5079_svt.list","MuDst.root",50);
  //muMk=new StMuDstMaker(0,0,"./","mb_ff_nosvt.list","MuDst.root",50);
  //muMk=new StMuDstMaker(0,0,"./","5028.list","MuDst.root",50);
  //muMk=new StMuDstMaker(0,0,"./","5079_nosvt.list","MuDst.root",50);
  //muMk=new StMuDstMaker(0,0,"./","5028_tpt.list","MuDst.root",50);
  //muMk=new StMuDstMaker(0,0,"./","5028_04k_23nov.list","MuDst.root",50);
  //  muMk=new StMuDstMaker(0,0,"data/","st_physics_5028057_raw_1010010.MuDst.root","MuDst.root",50);
  muMk=new StMuDstMaker(0,0,"./","MuDst.list","MuDst.root",50);
  //muMk=new StMuDstMaker(0,0,"./","year2001.list");
  
  //  TFile *fout=0;
  TFile *fout=new TFile("mult_hists_04k_01dev.root","RECREATE");

  muMk->Init();

  TH2F *nch_goodglobals=new TH2F("nch_goodglobals","#Good globals vs # charged",500,0,600,500,0,1200);
  nch_goodglobals->SetXTitle("N_{ch}");
  nch_goodglobals->SetYTitle("Good Globals");
  //TH2F *nch_goodglobals2=new TH2F("nch_goodglobals2","#Good globals (w dca cut)vs # charged",500,0,1000,500,0,2000);
  TH2F *nch_goodglobals2=new TH2F("nch_goodglobals2","#Good globals (w dca cut)vs # charged",250,0,600,250,0,600);
  nch_goodglobals2->SetXTitle("N_{ch}");
  nch_goodglobals2->SetYTitle("Good Globals");

  TH2F *nch_nsvt=new TH2F("nch_nsvt","# charged vs #tracks w SVT",250,0,600,250,0,600);
  nch_nsvt->SetXTitle("N_{ch}");
  nch_nsvt->SetYTitle("N_{svt}");

  TH2F *delta_nsvt_glob=new TH2F("delta_nsvt_glob","good globals-nch vs global svt points (large mult)",300,0,1000,200,0,200);
  TH2F *delta_nsvt_prim=new TH2F("delta_nsvt_prim","good globals-nch vs primary svt points (large mult)",300,0,1000,200,0,200);

  TH2F *gg_ctb=new TH2F("gg_ctb","good globals vs ctb",300,0,30000,250,0,1000);
  TH2F *gg_zdc=new TH2F("gg_zdc","good globals vs zdc",125,0,250,250,0,1000);
  TH2F *nch_ctb=new TH2F("nch_ctb","N_ch vs ctb",300,0,30000,250,0,1000);
  TH2F *nch_zdc=new TH2F("nch_zdc","N_ch vs zdc",125,0,250,250,0,1000);

  TH2F *vtx_xy=new TH2F("vtx_xy","Vertex x,y",100,-1,1,100,-1,1);
  TH1F *vtx_z=new TH1F("vtx_z","Vertex z",500,-100,100);
  TH2F *vtx_xy_bad=new TH2F("vtx_xy_bad","Vertex x,y (bad events)",100,-1,1,100,-1,1);
  TH1F *vtx_z_bad=new TH1F("vtx_z_bad","Vertex z (bad events)",500,-100,100);
  
  TH2F *phi_nhit_svt=new TH2F("phi_nhit_svt","# hits vs phi(first point) SVT (pt>1)",24,-3.14,3.14,7,-0.5,6.5);
  TH2F *phi_nhit_tpc=new TH2F("phi_nhit_tpc","# hits vs phi(first point) TPC (pt>1)",24,-3.14,3.14,50,-0.5,49.5);

  Int_t n_dca=30;
  Float_t max_dca=3;
  TH2F *nch_dcaglob=new TH2F("nch_dcaglob","dca vs ncharge",60,0,600,n_dca,0,max_dca);
  TH2F *nch_dcaglob_nosvt=new TH2F("nch_dcaglob_nosvt","dca vs ncharge, tracks w/o SVT",60,0,600,n_dca,0,max_dca);

  TH2F *ctb_nfit_tpc=new TH2F("ctb_nfit_tpc","nfit tpc vs ctb",60,0,30000,50,-0.5,49.5);
  TH2F *ctb_nfit_svt=new TH2F("ctb_nfit_svt","nfit svt vs ctb",60,0,30000,7,-0.5,6.5);
  TH2F *ctb_nfitfrac_tpc=new TH2F("ctb_nfitfrac_tpc","nfit/nposs tpc vs ctb",60,0,30000,50,0,1);
  TH2F *ctb_nfitfrac_svt=new TH2F("ctb_nfitfrac_svt","nfit/nposs svt vs ctb",60,0,30000,6,0,1);
  TH2F *ctb_nfit_tpc_nosvt=new TH2F("ctb_nfit_tpc_nosvt","nfit tpc vs ctb (w/o SVT)",60,0,30000,50,-0.5,49.5);
  TH2F *ctb_nfitfrac_tpc_nosvt=new TH2F("ctb_nfitfrac_tpc_nosvt","nfit/nposs tpc vs ctb (w/o SVT)",60,0,30000,50,0,1);

  TH3F *ctb_nhit_nfit_tpc=new TH3F("ctb_nhit_nfit_tpc","nhit,nfit tpc vs ctb",60,0,30000,50,-0.5,49.5,50,-0.5,49.5);
  TH3F *ctb_nhit_nfit_tpc_nosvt=new TH3F("ctb_nhit_nfit_tpc_nosvt","nhit,nfit tpc vs ctb (trk w/o SVT)",60,0,30000,50,-0.5,49.5,50,-0.5,49.5);

  TH2F *nch_2d_dcaglob=new TH2F("nch_2d_dcaglob","2d dca vs ncharge",60,0,600,n_dca,0,max_dca);
  TH2F *nch_2d_dcaglob_nosvt=new TH2F("nch_2d_dcaglob_nosvt","2d dca vs ncharge, tracks w/o SVT",60,0,600,n_dca,0,max_dca);

  TH2F *phi_2d_dcaglob=new TH2F("phi_2d_dcaglob","2d dca vs phi (pt>2)",63,-3.14,3.14,n_dca,0,max_dca);  
  TH2F *phi_2d_dcaglob_nosvt=new TH2F("phi_2d_dcaglob_nosvt","2d dca vs phi (pt>2) tracks w/o svt",63,-3.14,3.14,n_dca,0,max_dca);

  TH2F *phi_dcaglob=new TH2F("phi_dcaglob","dca vs phi (pt>2)",63,-3.14,3.14,n_dca,0,max_dca);  
  TH2F *phi_dcaglob_nosvt=new TH2F("phi_dcaglob_nosvt","dca vs phi (pt>2) tracks w/o svt",63,-3.14,3.14,n_dca,0,max_dca);
  
  const Int_t n_evt=5000; 
  //const Int_t n_evt=2500; 
  for (Int_t i_evt=0; i_evt<n_evt; i_evt++) {
    Int_t ret=muMk->Make();
    if (ret)
      break;
    StMuEvent *event=muMk->muDst()->event();
    if (!event->triggerIdCollection()->nominal()->isTrigger(15003) && 
	!event->triggerIdCollection()->nominal()->isTrigger(15007))
      continue;
    if (fabs(event->primaryVertexPosition().x()) < 0.0001 &&
	fabs(event->primaryVertexPosition().y()) < 0.0001 &&
	fabs(event->primaryVertexPosition().z()) < 0.0001)
      continue;
    if (fabs(event->primaryVertexPosition().z()) > 30)
      continue;
    cout << "Primary vertex: " << event->primaryVertexPosition() << endl;
    /*
    for (Int_t i_det=0; i_det<10; i_det++) {
      if (muMk->muDst()->detectorStates(i_det)) {
	StDetectorState *det_state=muMk->muDst()->detectorStates(i_det);

	cout << "Detector idx" << i_det << endl;
	cout << "det id " << det_state->detector();
	if (det_state->bad()) 
	  cout << ", bad" << endl; 
	if (det_state->good()) 
	  cout << ", good" << endl; 
      }
      else {
	cout << "No state for det " << i_det << endl;
      }
    }
    */
    Float_t ctb_sum=event->ctbTriggerDetector()->mips();

    Int_t n_prim=muMk->muDst()->numberOfPrimaryTracks();
    Int_t  n_charge=0;
    Int_t n_svt_hit_prim=0;
    Int_t n_svt_prim=0;
    for (Int_t i_prim=0; i_prim<n_prim; i_prim++) {
      StMuTrack *track=muMk->muDst()->primaryTracks(i_prim);
      // from StuRefMult
      //if (track->nHitsFit() >= 10 && fabs(track->eta())<0.5 &&
      if (track->nHitsFit() >= 15 && fabs(track->eta())<0.5 &&
      //if (track->nHits() >= 15 && fabs(track->eta())<0.5 &&
	  track->dca().mag() < 3) { 
	n_charge++;
	n_svt_hit_prim+=track->nHitsFit(kSvtId);
	if (track->nHitsFit(kSvtId))
	  n_svt_prim++;
	if (track->pt()>1) {
	  StMuTrack *g_track=track->globalTrack();
	  if (g_track) {
	    Float_t phi=g_track->helix().origin().phi();
	    phi_nhit_svt->Fill(phi,track->nHitsFit(kSvtId));
	    phi_nhit_tpc->Fill(phi,track->nHitsFit(kTpcId));
	  }
	}
      }
    }
    Int_t n_glob=muMk->muDst()->numberOfGlobalTracks();
    Int_t n_good=0, n_good2=0;
    Int_t n_svt_hit_glob=0;
    for (Int_t i_glob=0; i_glob<n_glob; i_glob++) {
      StMuTrack *track=muMk->muDst()->globalTracks(i_glob);
      // From StHiMicroMaker
      if (track->nHitsFit() >= 15 && fabs(track->eta())<0.5) { 
      //if (track->nHits() >= 15 && fabs(track->eta())<0.5) { 
	 n_good++;
	 if (track->dcaGlobal().mag() < 3) {
	   n_good2++;
	   n_svt_hit_glob+=track->nHitsFit(kSvtId);

	   ctb_nfit_tpc->Fill(ctb_sum,track->nHitsFit(kTpcId));
	   ctb_nhit_nfit_tpc->Fill(ctb_sum,track->topologyMap().numberOfHits(kTpcId),track->nHitsFit(kTpcId));
	   ctb_nfit_svt->Fill(ctb_sum,track->nHitsFit(kSvtId));
	   if (track->nHitsPoss(kTpcId))
	     ctb_nfitfrac_tpc->Fill(ctb_sum,(Float_t)track->nHitsFit(kTpcId)/track->nHitsPoss(kTpcId));
	   if (track->nHitsPoss(kSvtId))
	     ctb_nfitfrac_svt->Fill(ctb_sum,(Float_t)track->nHitsFit(kSvtId)/track->nHitsPoss(kSvtId));

	   if (track->nHitsFit(kSvtId)) {
	     nch_dcaglob->Fill(n_charge,track->dca().mag());
	     nch_2d_dcaglob->Fill(n_charge,track->dca().perp());
	     if (track->pt()>2) {
	       phi_dcaglob->Fill(track->phi(),track->dca().mag());
	       phi_2d_dcaglob->Fill(track->phi(),track->dca().perp());
	     }
	   }
	   else {
	     nch_dcaglob_nosvt->Fill(n_charge,track->dca().mag());
	     nch_2d_dcaglob_nosvt->Fill(n_charge,track->dca().perp());
	     if (track->pt()>2) {
	       phi_dcaglob_nosvt->Fill(track->phi(),track->dca().mag());
	       phi_2d_dcaglob_nosvt->Fill(track->phi(),track->dca().perp());
	     }
	     ctb_nfit_tpc_nosvt->Fill(ctb_sum,track->nHitsFit(kTpcId));
	     ctb_nhit_nfit_tpc_nosvt->Fill(ctb_sum,track->topologyMap().numberOfHits(kTpcId),track->nHitsFit(kTpcId));
	     if (track->nHitsPoss(kTpcId))
	       ctb_nfitfrac_tpc_nosvt->Fill(ctb_sum,(Float_t)track->nHitsFit(kTpcId)/track->nHitsPoss(kTpcId));
	   }
	 }
      }
    }
    Float_t mult_rat=0;
    if (n_charge)
      mult_rat=Float_t(n_good2)/n_charge;
    cout << "Event " << i_evt << ", " << event->eventId() << endl;
    cout << "n_charge " << n_charge << ", refmult " 
	 << muMk->muDst()->event()->refMult() << ", n_good " << n_good
	 << ", n_good2 " << n_good2 << ", n_good2/n_charge " 
         << mult_rat << endl;
    nch_goodglobals->Fill(n_charge,n_good);
    nch_goodglobals2->Fill(n_charge,n_good2);
    nch_nsvt->Fill(n_charge,n_svt_prim);
    gg_zdc->Fill(event->zdcTriggerDetector()->adcSum(),n_good2);
    if (mult_rat>1.8) {
      vtx_xy_bad->Fill(event->primaryVertexPosition().x(),event->primaryVertexPosition().y());
      vtx_z_bad->Fill(event->primaryVertexPosition().z());
    } 
    /*
    Float_t ctb_sum=0;
    for (Int_t tray=0; tray++; tray<120) 
      for (Int_t slat=0; slat++; slat<2) 
	ctb_sum+=event->ctbTriggerDetector()->mips(tray,slat);
    */
    gg_ctb->Fill(ctb_sum,n_good2);
    nch_zdc->Fill(event->zdcTriggerDetector()->adcSum(),n_charge);
    nch_ctb->Fill(ctb_sum,n_charge);
    
    vtx_xy->Fill(event->primaryVertexPosition().x(),event->primaryVertexPosition().y());
    vtx_z->Fill(event->primaryVertexPosition().z());
 
    if (n_charge>300) {
      delta_nsvt_prim->Fill(n_svt_hit_prim,n_good2-n_charge);
      delta_nsvt_glob->Fill(n_svt_hit_glob,n_good2-n_charge);
    }
    if (i_evt%100==0)
      cout << "Event " << i_evt << endl;
  }
  c1->cd(1);
  nch_goodglobals2->Draw();
  c1->cd(2);
  nch_nsvt->Draw();

  c2->cd(1);
  nch_ctb->Draw();
  c2->cd(2);
  gg_ctb->Draw();

  c3->cd(1);
  nch_zdc->Draw();
  c3->cd(2);
  gg_zdc->Draw();

  c4->cd(1);
  vtx_xy->Draw();
  vtx_xy_bad->SetMarkerColor(2);
  vtx_xy_bad->Draw("same");
  c4->cd(2);
  vtx_z->Draw();
  vtx_z_bad->SetLineStyle(2);
  vtx_z_bad->Draw("same");

  c5->cd(1);
  delta_nsvt_prim->Draw();
  c5->cd(2);
  delta_nsvt_glob->Draw();

  c6->cd(1);
  phi_nhit_svt->Draw();
  c6->cd(2);
  phi_nhit_tpc->Draw();

  c7->cd(1);
  gPad->SetLogy();
  dca_1=nch_dcaglob->ProjectionY("dca_1",0,15);
  dca_2=nch_dcaglob->ProjectionY("dca_2",15,30);
  dca_3=nch_dcaglob->ProjectionY("dca_3",30,60);
  dca_2->Draw();
  dca_1->Draw("same");  
  dca_3->Draw("same");
  gPad->Update();
  dca_2->SetLineColor(2);
  dca_3->SetLineColor(4);
  draw_legend_l(0.5,0.9,dca_1,"Ref Mult <150",0);
  draw_legend_l(0.5,0.9,dca_2,"150 < ref m < 300",1);
  draw_legend_l(0.5,0.9,dca_3,"300 < ref m < 600",2);

  c7->cd(2);
  //nch_dcaglob_nosvt->Draw();
  gPad->SetLogy();
  dca_1_ns=nch_dcaglob_nosvt->ProjectionY("dca_1_ns",0,15);
  dca_2_ns=nch_dcaglob_nosvt->ProjectionY("dca_2_ns",15,30);
  dca_3_ns=nch_dcaglob_nosvt->ProjectionY("dca_3_ns",30,60);
  dca_2_ns->Draw();
  dca_1_ns->Draw("same");  
  dca_3_ns->Draw("same");
  gPad->Update();
  dca_2_ns->SetLineColor(2);
  dca_3_ns->SetLineColor(4);
  draw_legend_l(0.5,0.9,dca_1_ns,"Ref Mult <150",0);
  draw_legend_l(0.5,0.9,dca_2_ns,"150 < ref m < 300",1);
  draw_legend_l(0.5,0.9,dca_3_ns,"300 < ref m < 600",2);

  if (fout)
    fout->Write();
}}
