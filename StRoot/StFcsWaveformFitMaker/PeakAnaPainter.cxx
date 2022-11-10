#include "PeakAnaPainter.h"
#include "PeakAna.h"

ClassImp(PeakAnaPainter)

PeakAnaPainter::PeakAnaPainter()
{
  Init();
}

void PeakAnaPainter::Init()
{
  mPA = 0;

  mTheBaseLine = 0;
  mTheHitLine  = 0;

  mPaveT_PA = 0;
  mGraphOption = "";
  mPeakOption = "";
  mStatsOption = "";
}

PeakAnaPainter::~PeakAnaPainter()
{
  delete mTheBaseLine;
  delete mTheHitLine;  mTheHitLine=0;
  delete mPaveT_PA; mPaveT_PA=0;
}

void PeakAnaPainter::SetPeakAna(PeakAna* ana)
{
  if( ana==0 ){return;}
  mPA = ana;
}

void PeakAnaPainter::CleanPainter()
{
  if( mPaveT_PA!=0 ){ mPaveT_PA->Clear(); }
}

Color_t PeakAnaPainter::GetBaseLineColor()
{if(mTheBaseLine==0){return 0;}else{return mTheBaseLine->GetLineColor();} }
Style_t PeakAnaPainter::GetBaseLineStyle()
{if(mTheBaseLine==0){return 0;}else{return mTheBaseLine->GetLineStyle();} }
Width_t PeakAnaPainter::GetBaseLineWidth()
{if(mTheBaseLine==0){return 0;}else{return mTheBaseLine->GetLineWidth();} }

Color_t PeakAnaPainter::GetHitLineColor()
{if(mTheHitLine==0){return 0;}else{return mTheHitLine->GetLineColor();} }
Style_t PeakAnaPainter::GetHitLineStyle()
{if(mTheHitLine==0){return 0;}else{return mTheHitLine->GetLineStyle();} }
Width_t PeakAnaPainter::GetHitLineWidth()
{if(mTheHitLine==0){return 0;}else{return mTheHitLine->GetLineWidth();} }

void PeakAnaPainter::SetBaseLineColor(Color_t color)
{if(mTheBaseLine==0){mTheBaseLine=new TLine();} mTheBaseLine->SetLineColor(color); }
void PeakAnaPainter::SetBaseLineColorAlpha(Color_t color,Float_t alpha)
{if(mTheBaseLine==0){mTheBaseLine=new TLine();} mTheBaseLine->SetLineColorAlpha(color,alpha); }
void PeakAnaPainter::SetBaseLineStyle(Style_t style)
{if(mTheBaseLine==0){mTheBaseLine=new TLine();} mTheBaseLine->SetLineStyle(style); }
void PeakAnaPainter::SetBaseLineWidth(Width_t width)
{if(mTheBaseLine==0){mTheBaseLine=new TLine();} mTheBaseLine->SetLineWidth(width); }

void PeakAnaPainter::SetHitLineColor(Color_t color)
{if(mTheHitLine==0){mTheHitLine=new TLine();} mTheHitLine->SetLineColor(color); }
void PeakAnaPainter::SetHitLineColorAlpha(Color_t color,Float_t alpha)
{if(mTheHitLine==0){mTheHitLine=new TLine();} mTheHitLine->SetLineColorAlpha(color,alpha); }
void PeakAnaPainter::SetHitLineStyle(Style_t style)
{if(mTheHitLine==0){mTheHitLine=new TLine();} mTheHitLine->SetLineStyle(style); }
void PeakAnaPainter::SetHitLineWidth(Width_t width)
{if(mTheHitLine==0){mTheHitLine=new TLine();} mTheHitLine->SetLineWidth(width); }

void PeakAnaPainter::Paint(Option_t* opt)
{
  if( mPA==0 ){ return; }
  std::string option(opt);

  CleanPainter();
  bool drawgraph = true;
  bool region = false;
  bool drawbaselines = false;
  bool drawfoundpeakqa = false;
  bool drawfullpeakqa = false;

  std::size_t firstcolon = option.find(";");
  if( firstcolon!=std::string::npos ){
    mGraphOption = option.substr(0,firstcolon);
    std::size_t secondcolon = option.find(";",firstcolon+1);
    if( secondcolon!=std::string::npos ){
      mPeakOption = option.substr(firstcolon+1,secondcolon-firstcolon-1);
      mStatsOption = option.substr(secondcolon+1);
      std::size_t thirdcolon = option.find(";",secondcolon+1);
      if( thirdcolon!=std::string::npos ){ LOG_WARN << "PeakAnaPainter::Paint(): Too many semicolons in draw option" << endm; }
    }
    else{ mPeakOption = option.substr(firstcolon+1); }
  }
  else{ mGraphOption = option; }

  mPeakOption.ToLower();
  if( mGraphOption.Length()==0 ){ drawgraph=false; }
  //if( peakopts.Contains("e") ){ drawgraph=false; }
  if( mPeakOption.Contains("r") ){ region=true; }
  if( mPeakOption.Contains("b") ){ drawbaselines=true; }
  if( mPeakOption.Contains("f") ){ drawfoundpeakqa=true; }
  if( mPeakOption.Contains("p") ){ drawfullpeakqa=true;  }
  if( mPeakOption.Contains("a") ){ drawbaselines=true; drawfullpeakqa=true; }

  if( drawfoundpeakqa && drawfullpeakqa ){ drawfoundpeakqa=false; }//Redundant to do both found and all peaks
  if( drawgraph )                { this->PaintRawData(); }
  if( region && drawgraph )      { this->PaintFoundPeak(); } //Doesn't make sense to restrict graph range if graph is not being drawn
  if( drawbaselines )            { this->PaintBaselines(); }
  if( drawfoundpeakqa )          { this->PaintFoundPeakQa(); }
  if( drawfullpeakqa )           { this->PaintPeakRanges(); }
  if( mStatsOption.Length()!=0 ) { this->PaintStats(); }
}


TPaveText* PeakAnaPainter::MakePaveText(Double_t xmin, Double_t ymin, Double_t xmax, Double_t ymax)
{
  if( mPaveT_PA==0 ){ mPaveT_PA = new TPaveText(xmin,ymin,xmax,ymax,"NB NDC"); }
  else{
    mPaveT_PA->Clear();
  }
  mPaveT_PA->SetFillColorAlpha(kWhite,1);//Fully opaque by default
  //mPaveT_PA->SetFillStyle(4000);
  return mPaveT_PA;
}

bool PeakAnaPainter::ValidGraph()
{
  if( mPA==0 || mPA->GetData()==0 || mPA->GetData()->GetN()==0 ){return false;}
  else{ return true; }
}

void PeakAnaPainter::PaintRawData( )
{
  if( !ValidGraph() ){ return;}
  mPA->GetData()->Paint(mGraphOption.Data());
  return;
}

void PeakAnaPainter::PaintFoundPeak( )
{
  if( !ValidGraph()){return;}

  Double_t Xmin = mPA->PeakStart();
  Double_t Xmax = mPA->PeakEnd();
  if( !(Xmin>mPA->MaxX() || Xmax>mPA->MaxX()) ){mPA->GetData()->GetXaxis()->SetRangeUser(Xmin-5,Xmax+5);}
}

void PeakAnaPainter::PaintFoundPeakQa()
{
  if( !ValidGraph() ){return;}

  Int_t computedindex = mPA->FoundPeakIndex();
  if( computedindex<0 ){ computedindex = mPA->AnalyzeForPeak(); }
  this->PaintBaselines();
  this->PaintFoundRange();

  return;
}

void PeakAnaPainter::PaintPeakQa( )
{
  if( !ValidGraph() ){return;}

  Int_t computedindex = mPA->FoundPeakIndex();
  if( computedindex<0 ){computedindex = mPA->AnalyzeForPeak();}

  this->PaintBaselines();
  this->PaintPeakRanges();

  return;
}

void PeakAnaPainter::PaintBaselines()
{
  Double_t base = mPA->Baseline();
  Double_t hitline = base+mPA->BaselineSigmaScale()*mPA->BaselineSigma();
  if( mTheBaseLine==0 ){mTheBaseLine = new TLine(mPA->MinX(),base,mPA->MaxX(),base);}
  else{ mTheBaseLine->SetY1(base); mTheBaseLine->SetY2(base);}
  if( mTheHitLine==0 ){mTheHitLine = new TLine(mPA->MinX(),hitline,mPA->MaxX(),hitline);}
  else{ mTheHitLine->SetY1(hitline); mTheHitLine->SetY2(hitline);}
  mTheBaseLine->SetLineColor(kBlack);
  mTheHitLine->SetLineColor(kViolet);

  mTheBaseLine->Paint();
  mTheHitLine->Paint();
}

void PeakAnaPainter::PaintFoundRange()
{
  Int_t computedindex =  mPA->FoundPeakIndex();
  if( computedindex<0 ){computedindex = mPA->AnalyzeForPeak();}
  if( computedindex == mPA->NPeaks() ){return;}//If no peak found then computed index is equal to number of peaks in peak vector

  TLine* sl = mPA->GetPeak(computedindex).GetStartLine(mPA->MinY(),mPA->MaxY());
  sl->SetLineColor(kRed);
  sl->Paint();
  TMarker* mp = mPA->GetPeak(computedindex).GetPeakMarker();
  mp->SetMarkerColor(kViolet);
  mp->SetMarkerSize(mPA->GetMarkerSize()*2.0);
  mp->Paint();
  TLine* el = mPA->GetPeak(computedindex).GetEndLine(mPA->MinY(),mPA->MaxY());
  el->SetLineColor(kOrange);
  el->Paint();
}

void PeakAnaPainter::PaintPeakRanges( )
{
  Int_t computedindex =  mPA->FoundPeakIndex();
  if(computedindex<0 ){computedindex = mPA->AnalyzeForPeak();}

  if( mPA->GetDebug() > 1){
    LOG_DEBUG<< "|SizePeaks:"<<mPA->NPeaks() << "|FoundPeak:"<<computedindex << "|Base:"<<mPA->Baseline() << "|Hit:"<<mPA->Baseline()+mPA->BaselineSigmaScale()*mPA->BaselineSigma();
    if( mPA->NPeaks()!=0 && computedindex<mPA->NPeaks() ){(mPA->GetPeak(computedindex)).Print("debug");}
    LOG_DEBUG << endm;
  }
  
  for( UShort_t ipeak = 0; ipeak<mPA->NPeaks(); ++ipeak ){
    if( ipeak==computedindex ){continue;}
    mPA->GetPeak(ipeak).GetStartLine( mPA->MinY(), mPA->MaxY() )->Paint();
    TMarker* mp = mPA->GetPeak(ipeak).GetPeakMarker();
    mp->SetMarkerSize(mPA->GetMarkerSize()*2.0);
    mp->Paint();
    mPA->GetPeak(ipeak).GetEndLine( mPA->MinY(), mPA->MaxY() )->Paint();
  }
  this->PaintFoundRange();

  return;
}

void PeakAnaPainter::PaintStats()
{
  //@[April 8, 2022]>Need a way to determine size of pavetext from number of peaks as well as information to be added for each peak (this could be as simple as counting the number of lines and scaling the size accordingly)
  this->MakePaveText()->SetTextSize(0.025);
  mPA->AddPeakStats( mPaveT_PA,mStatsOption.Data() );
  //mPaveT_PA->Draw("same");
  mPaveT_PA->Paint( mPaveT_PA->GetOption() );

}

