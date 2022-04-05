TString MuDSTFile(TString production, TString trgsetupname)
{
   if( production.Contains("P08ic") && trgsetupname.Contains("upsilon"))
	return "/star/data06/embed/andrewar/MuDstReduced/P08ic_MuDst_st_upsilon.root";
  
}
int Quit(char* prompt="Hit q or n to quit...")
{
  char character;
  fflush(stdin);
  cout << prompt << flush;
  character = getc(stdin);
  cout << endl;
  return ((character=='q')||
	  (character=='Q')||
	  (character=='n')||
	  (character=='N'));
}


/////////////////////////////////////////////////////////

void StringCuts(char *o, const char *s, const char *s1)
{
  //o = malloc(1000);
  strcpy(o,s);
  strcat(o, " {"); strcat(o,s1); strcat(o,"}");
}

void StringCuts(char *o, const char *s, const char *s1, const char *s2)
{
  strcpy(o,s);
  strcat(o, " {");
  strcat(o, " ("); strcat(o,s1); strcat(o,")");
  strcat(o,"&&("); strcat(o,s2); strcat(o,")");
  strcat(o,  "}");
}

void StringCuts(char *o, const char *s, const char *s1, 
		const char *s2, const char *s3)
{
  strcpy(o,s);
  strcat(o, " {");
  strcat(o,  "("); strcat(o,s1); strcat(o,")");
  strcat(o,"&&("); strcat(o,s2); strcat(o,")");
  strcat(o,"&&("); strcat(o,s3); strcat(o,")");
  strcat(o,  "}");
}

void StringCuts(char *o, const char *s, const char *s1, const char *s2, 
		const char *s3, const char *s4)
{
  strcpy(o,s);
  strcat(o, " {");
  strcat(o,  "("); strcat(o,s1); strcat(o,")");
  strcat(o,"&&("); strcat(o,s2); strcat(o,")");
  strcat(o,"&&("); strcat(o,s3); strcat(o,")");
  strcat(o,"&&("); strcat(o,s4); strcat(o,")");
  strcat(o,  "}");
}


/////////////////////////////////////////////////////////

void Style(int i) 
{
  switch (i) {
  case 1:
    //cout << endl << "Style 1 ..." << endl << endl;
    gStyle->SetCanvasColor(10);              // white
    gStyle->SetPadColor(10);                 // white
    gStyle->SetPadTickX(1);
    gStyle->SetPadTickY(1);
    gStyle->SetFillColor(0);                 // clear (no-fill)
    gStyle->SetTitleColor(0);                // clear (no-fill)
    gStyle->SetStatColor(0);                 // clear (no-fill)
    gStyle->SetHistFillColor(0);             // clear (no-fill)
    gStyle->SetFrameFillColor(10);           // white
    gStyle->SetCanvasBorderMode(0);
    gStyle->SetFrameBorderMode(0);
    gStyle->SetPadBorderMode(0);
    //gStyle->SetOptStat(0);
    gStyle->SetOptFit();
    break;
  default:
    break;
  }
}

/////////////////////////////////////////////////////////

void keyLine(Float_t x, Float_t y, const Char_t* tt, 
             Int_t color=1, Int_t style=1, Float_t tSize=0.04)
{
  gPad->Update();
  Float_t x1 = gPad->GetFrame()->GetX1();
  Float_t x2 = gPad->GetFrame()->GetX2();
  Float_t dx = x2-x1;
  x = x1 + dx*x;
  Float_t y1 = gPad->GetFrame()->GetY1();
  Float_t y2 = gPad->GetFrame()->GetY2();
  Float_t dy = y2-y1;
  y = y1 + dy*y;
  TLine *l = new TLine(x,y,x+0.05*dx,y);
  l->SetLineColor(color);
  l->SetLineStyle(style);
  l->Draw();
  TText *t = new TText(x+0.06*dx,y,tt);
  t->SetTextAlign(12);
  t->SetTextSize(tSize);
  t->SetTextColor(color);
  t->Draw();
}

void keySymbol(Float_t x, Float_t y, const Char_t* tt, 
               Int_t color=1, Int_t marker=1, Float_t tSize=0.04)
{
  gPad->Update();
  Float_t x1 = gPad->GetFrame()->GetX1();
  Float_t x2 = gPad->GetFrame()->GetX2();
  Float_t dx = x2-x1;
  x = x1 + dx*x;
  Float_t y1 = gPad->GetFrame()->GetY1();
  Float_t y2 = gPad->GetFrame()->GetY2();
  Float_t dy = y2-y1;
  y = y1 + dy*y;
  TMarker *l = new TMarker(x+0.025*dx,y,marker);
  l->SetMarkerColor(color);
  l->Draw();
  TText *t = new TText(x+0.06*dx,y,tt);
  t->SetTextAlign(12);
  t->SetTextSize(tSize);
  t->SetTextColor(color);
  t->Draw();
}
