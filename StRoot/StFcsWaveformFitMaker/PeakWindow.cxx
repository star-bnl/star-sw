#include "PeakWindow.h"

ClassImp(PeakWindow)

PeakWindow::PeakWindow():mStartX(0),mEndX(1),mStartY(0),mEndY(0),mP_Peak(-1),mPeakX(-1),mPeakY(-1),mStartLine(0),mPeakMarker(0),mEndLine(0)
{
}

PeakWindow::PeakWindow(Double_t start, Double_t end):PeakWindow()
{
  if( start<end ){
    mStartX=start;
    mEndX=end;
    mPeakX=start-1;//In order for PeakAna algorithm for finding peaks to work peak x-value must be less than start
  }
}

PeakWindow::PeakWindow(const PeakWindow &oldpeak)
{
  oldpeak.Copy(*this);
}

PeakWindow& PeakWindow::operator=(const PeakWindow& rhs)
{
  if( this == &rhs ){ return *this; }
  rhs.Copy(*this);

  return *this;
}


//Note that this does not copy the TLine objects. Use TObject::Clone()
void PeakWindow::Copy(TObject& obj) const
{
  TObject::Copy(obj);//Copy TObject bits

  ((PeakWindow&)obj).mStartX = mStartX;
  ((PeakWindow&)obj).mEndX   = mEndX;
  ((PeakWindow&)obj).mStartY = mStartY;
  ((PeakWindow&)obj).mEndY   = mEndY;
  ((PeakWindow&)obj).mP_Peak = mP_Peak;
  ((PeakWindow&)obj).mPeakX  = mPeakX;
  ((PeakWindow&)obj).mPeakY  = mPeakY;

  ((PeakWindow&)obj).mStartLine = 0;
  ((PeakWindow&)obj).mPeakMarker = 0;
  ((PeakWindow&)obj).mEndLine = 0;
}

//Note that this does not copy the TLine objects. Use TObject::Clone()
TObject* PeakWindow::Clone(const char* newname) const
{
  PeakWindow* window = new PeakWindow();
  Copy(*window);
  //Also copy color, style, and size?
  if( mStartLine!=0 ){ window->GetStartLine( mStartLine->GetY1(), mStartLine->GetY2()); }
  if( mPeakMarker!=0 ){ window->GetPeakMarker(); }
  if( mEndLine!=0 )  { window->GetEndLine(  mEndLine->GetY1(), mStartLine->GetY2()); }

  return window;
}

PeakWindow::~PeakWindow()
{
  delete mStartLine;
  delete mPeakMarker;
  delete mEndLine;
}

void PeakWindow::SetWindow(Double_t s, Double_t e)
{
  mStartX=s;
  mEndX=e;
}

void PeakWindow::GetWindow(Double_t &s, Double_t &e) const
{s=mStartX;e=mEndX;}

void PeakWindow::Reset(Double_t start, Double_t end)
{
  if( start<end ){
    mStartX=start;
    mEndX=end;
  }
  else{
    mStartX = 0;
    mEndX = 1;
  }
  mStartY = 0;
  mEndY = 0;
  mP_Peak=-1;
  mPeakX=start-1;
  mPeakY = -1;
}

void PeakWindow::Print(Option_t* opt)const
{
  TString option(opt);
  option.ToLower();
  if( option.EqualTo("debug") ){
    LOG_DEBUG <<"|Start:("<<mStartX<<","<<mStartY<<")"
	      <<"|End:("<<mEndX<<","<<mEndY<<")"
	      <<"|Ppeak:"<<mP_Peak<<"|Peak:("<<mPeakX<<","<<mPeakY<<")";
  }
  else{
      std::cout <<"|Start:("<<mStartX<<","<<mStartY<<")"
		<<"|End:("<<mEndX<<","<<mEndY<<")"
		<<"|Ppeak:"<<mP_Peak<<"|Peak:("<<mPeakX<<","<<mPeakY<<")";
    }
}

void PeakWindow::SetPeak(TGraph* gdata)
{
  if(mP_Peak<0){return;}
  if(mP_Peak>=gdata->GetN()){return;}
  Double_t x2=0; Double_t y2=0;
  gdata->GetPoint(mP_Peak,x2,y2);
  if( mP_Peak==0 || mP_Peak==gdata->GetN()-1 ){
    mPeakX = x2;
    mPeakY = y2;
    return;
  }
  Double_t x1=0; Double_t y1=0;
  Double_t x3=0; Double_t y3=0;
  gdata->GetPoint(mP_Peak-1,x1,y1);
  gdata->GetPoint(mP_Peak+1,x3,y3);
  Double_t x00 = (x1+x2)/2.0;
  Double_t x11 = (x2+x3)/2.0;
  Double_t PosSlope = (y2-y1)/(x2-x1);
  Double_t NegSlope = (y3-y2)/(x3-x2);
  mPeakX = (x00*NegSlope-x11*PosSlope)/(NegSlope-PosSlope);//Formula for x-intercept of a line with points (x00,PosSlope) and (x11,NegSlope)
  mPeakY = y2;
    
  return;
}

void PeakWindow::Combine( const PeakWindow &other, bool keepthis )
{
  //Assuming mStartX<mEndX for either peak then leftmost start value should be the lower of the two start values
  if( mStartX>other.mStartX ){
    mStartX = other.mStartX;
    mStartY = other.mStartY;
  }
  //Same assumption as above then rightmost end value should be the greater of the two end values
  if( mEndX<other.mEndX ){
    mEndX = other.mEndX;
    mEndY = other.mEndY;
  }
  if( !keepthis ){
    mP_Peak = other.mP_Peak;
    mPeakX = other.mPeakX;
    mPeakY = other.mPeakY;
  }
}

PeakWindow PeakWindow::Combine(const PeakWindow &leftpeak, const PeakWindow &rightpeak, bool keepleft)
{
  PeakWindow comb(leftpeak.mStartX,rightpeak.mEndX);//short for combined
  comb.mStartY = leftpeak.mStartY;
  comb.mEndY = rightpeak.mEndY;
  if( keepleft ){
    comb.mP_Peak = leftpeak.mP_Peak;
    comb.mPeakX = leftpeak.mPeakX;
    comb.mPeakY = leftpeak.mPeakY;
  }
  else{
    comb.mP_Peak = rightpeak.mP_Peak;
    comb.mPeakX = rightpeak.mPeakX;
    comb.mPeakY = rightpeak.mPeakY;
  }
  return comb;
}

Double_t PeakWindow::StartEndLineSlope() const
{
  Double_t xdiff = mEndX-mStartX;
  Double_t ydiff = mEndY-mStartY;
  return ydiff/xdiff; //slope of the line formed by the point (mStartX,mStartY) and (mEndX,mEndY)
}

Double_t PeakWindow::StartEndSlopeUncertainty(Double_t sigma) const
{
  sigma = fabs(sigma);//take positive
  Double_t xdiff = mEndX-mStartX;//Assume error is only in y position so need to scale uncertainty by xdiff
  Double_t ydiff_sigma = TMath::Sqrt2()*sigma;//assuming symmetric error for start and end y points
  return ydiff_sigma/xdiff; //slope of the line formed by the point (mStartX,mStartY) and (mEndX,mEndY)
}

Double_t PeakWindow::StartEndLineYint() const
{
  return mStartY - (StartEndLineSlope()*mStartX);
}

Double_t PeakWindow::MidPoint(TGraph* graph) const
{
  Double_t xdiff = mEndX-mStartX;
  Double_t ydiff = mEndY-mStartY;
  Double_t mslope = ydiff/xdiff; //slope of the line formed by the point (mStartX,mStartY) and (mEndX,mEndY)
  Double_t yint = mStartY-mslope*mStartX;//y-intercept of the line above "^"
  //y-value at peak x value for the line defined above "^"
  if( graph!=0 ){
    Double_t peakx=0; Double_t peaky=0;
    graph->GetPoint(mP_Peak,peakx,peaky);
    return mslope*peakx+yint;
  }
  else{return mslope*mPeakX+yint;}
}

Double_t PeakWindow::SlopeChirality(Double_t scale) const
{  
  //[Feb 28, 2022]>Use small fluctation around zero to return 0??
  Double_t slope = StartEndLineSlope();
  if( slope<0 ){ return -1.0*cosh(scale*slope); }
  else{ return cosh(scale*slope); }
}

Double_t PeakWindow::PeakChirality(Double_t slopescale, Double_t scale) const
{
	if( scale==0 ){scale=1;}
	if( scale<0 ){ scale = fabs(scale); }
	Double_t startendslope = StartEndLineSlope();
  Double_t peakstartslope = (mPeakY-mStartY)/(mPeakX-mStartX);
  Double_t peakendslope = (mPeakY-mEndY)/(mPeakX-mEndX);

  Double_t deltaps = peakstartslope-startendslope;
  Double_t deltape = peakendslope-startendslope;
  //scale==1 peak center is exactly halfway between StartX and EndX
  //if scale>1 peak center i.e. Chirality==0 is towards StartX
  //if scale<1 peak center i.e. Chirality==0 is towards EndX
  return this->SlopeChirality(slopescale*startendslope)*((deltape*deltape)-(deltaps*deltaps)/(scale*scale));
  //return this->SlopeChirality(slopescale*startendslope);
}

Double_t PeakWindow::PeakChiralityProb(Double_t probscale,Double_t chirality) const
{
  if( probscale<0 ){ probscale = fabs(probscale); }
  //Probability is 1/(chir^2+1). This was chosen because 1/x^2+1 "falls softer" than e^-|x|. This returns a probabilty to NOT merge a peak meaning if the chirality is 0 then should return 1 and when chirality is +-inf should return 0.
  return static_cast<Double_t>(1)/(probscale*chirality*chirality+static_cast<Double_t>(1));
}

Double_t PeakWindow::PeakChiralityProb(Double_t probscale, Double_t peakscale, Double_t chirscale ) const
{
  Double_t chir = PeakChirality(peakscale, chirscale);
  return this->PeakChiralityProb(peakscale,chir);
}

Double_t PeakWindow::PeakTunnelProb(TGraph* graph, Double_t scale, Double_t sigma) const
{
  if( sigma<0 ){sigma = fabs(sigma);}
  else if(sigma==0){LOG_ERROR << "PeakWindow::PeakTunnelProb - ERROR:sigma cannot be 0 - Returning probability of 0" << endm; return 0;}
  Double_t xdiff = mEndX-mStartX;
  Double_t ydiff = mEndY-mStartY;
  Double_t mslope = ydiff/xdiff; //slope of the line formed by the point (mStartX,mStartY) and (mEndX,mEndY)
  Double_t yint = mStartY-mslope*mStartX;//y-intercept of the line above "^"
  Double_t peakx=0; Double_t peaky=0;
  graph->GetPoint(mP_Peak,peakx,peaky);
  Double_t yline = mslope*peakx+yint;//y-value at peak x value, use mPeak instead(may not be set so check and pick)??
  Double_t heightdiff = peaky-yline;
  //return 1.0/scale*exp(-1.0*scale*heightdiff*xdiff);//Alternative probablity that is not so easy to normalize
  if( heightdiff<=0 ){ return 1; }//if peak value is less than the line value automatically tunnel
  else{
    //return TMath::Exp(-1.0*fabs(scale)*fabs(xdiff)) * (TMath::Erfc((heightdiff)/(TMath::Sqrt2()*sigma)));
    return (1.0/(scale*xdiff*xdiff+1.0))*(TMath::Erfc((heightdiff)/(TMath::Sqrt2()*sigma)));
  }
  //The idea for this probablity distribution (not probability density function) comes from two assumptions about the underlying data
  //1. As the distance between the start and end x-values increases the probability of tunneling through will decrease exponentially with some scale that must be determined based on the input data. One-tenth of the difference in x-values may work.
  //2. There is some noise in the y-value of the data that follows a Normal distribution. The mean ('yline') will be the y-value of the line connecting the start and end points evaluated at the x-value of the peak. The sigma should be the noise level of the data. The cumulative distribution function (which is the actual probability) for a Normal Distribution is the complimentary error function (Erfc). Note that the normalization constant of 1/2 has been removed since this is a probability distribution not a probability density function and hence does not need to be normalized as long as the value returned is between 0 and 1. This is true for Erfc as long as 'peaky'>='yline' which is the case here.
  //The total proability of tunneling is then (@1)*(@2) with two free parameters the scale and the sigma. Note the scale and sigma must be positive
}

UShort_t PeakWindow::CompareTo(const PeakWindow& other) const
{
  UShort_t check = 0;
  if( mStartX == other.mStartX ){
    if( mEndX == other.mEndX ){ check = 1; }
  }
  if( check>0 && mP_Peak == other.mP_Peak ){check=2;}
  if( check>1 && mStartY == other.mStartY ){
    if( mEndY == other.mEndY ){
      check = 3;
    }
  }
  if( check>2 && mPeakX == other.mPeakX ){ check = 4;}
  if( check>3 && mPeakY == other.mPeakY ){ check = 5;}
  return check;
}

void PeakWindow::Draw(Option_t* opt)
{
  AppendPad(opt);
}

void PeakWindow::Paint(Option_t* opt)
{
  TString option(opt);
  option.ToLower();
  bool drawstart = false;
  bool drawpeak = false;
  bool drawend = false;

  if( option.Length()==0 ){
    drawstart=true;
    drawpeak=true;
    drawend=true;
  }
  else{
    if( option.Contains("s") ){ drawstart = true; }
    if( option.Contains("p") ){ drawpeak = true; }
    if( option.Contains("e") ){ drawend = true; }
  }

  if( drawstart ){GetStartLine()->Paint();}
  if( drawpeak  ){GetPeakMarker()->Paint();}
  if( drawend   ){GetEndLine()->Paint();}
}

TLine* PeakWindow::GetStartLine(Double_t ymin, Double_t ymax)
{
  if( mStartLine==0 ){
    if( ymin==ymax ){ 
      if( mStartY<mEndY ){ymin=mStartY; ymax=mEndY; }
      else if(mStartY==mEndY){ ymin=mStartY; ymax=mPeakY; }
      else{ ymin=mEndY; ymax=mStartY; }
    }
    mStartLine = new TLine(mStartX,ymin,mStartX,ymax);
    mStartLine->SetLineColor(kGreen);
  }
  else{
    mStartLine->SetX1(mStartX);
    mStartLine->SetX2(mStartX);
    if( ymin==ymax ){ return mStartLine; }
    mStartLine->SetY1(ymin);
    mStartLine->SetY2(ymax);
  }
  return mStartLine;
}

Color_t PeakWindow::GetStartLineColor() const
{ if( mStartLine!=0 ){return mStartLine->GetLineColor(); } return 0; }
Style_t PeakWindow::GetStartLineStyle() const
{ if( mStartLine!=0 ){return mStartLine->GetLineStyle(); } return 0; }
Width_t PeakWindow::GetStartLineWidth() const
{ if( mStartLine!=0 ){return mStartLine->GetLineWidth(); } return 0; }
void PeakWindow::SetStartLineColor(Color_t color){ GetStartLine()->SetLineColor(color); }
void PeakWindow::SetStartLineColorAlpha(Color_t color,Float_t alpha){ GetStartLine()->SetLineColorAlpha(color,alpha); }
void PeakWindow::SetStartLineStyle(Style_t style){ GetStartLine()->SetLineStyle(style); }
void PeakWindow::SetStartLineWidth(Width_t width){ GetStartLine()->SetLineWidth(width); }

TMarker* PeakWindow::GetPeakMarker()
{
  if( mPeakMarker==0 ){
    mPeakMarker = new TMarker(mPeakX,mPeakY,4);//Default style is open circle
    mPeakMarker->SetMarkerColor(kGreen+1);//Default color is the green color that is between the green colors of start and end
  }
  else{
    mPeakMarker->SetX(mPeakX);
    mPeakMarker->SetY(mPeakY);
  }
  return mPeakMarker;
}

Color_t PeakWindow::GetPeakMarkerColor() const
{ if( mPeakMarker!=0 ){return mPeakMarker->GetMarkerColor(); } return 0; }
Style_t PeakWindow::GetPeakMarkerStyle() const
{ if( mPeakMarker!=0 ){return mPeakMarker->GetMarkerStyle(); } return 0; }
Size_t PeakWindow::GetPeakMarkerSize() const
{ if( mPeakMarker!=0 ){return mPeakMarker->GetMarkerSize(); } return 0; }
void PeakWindow::SetPeakMarkerColor(Color_t color){ GetPeakMarker()->SetMarkerColor(color); }
void PeakWindow::SetPeakMarkerColorAlpha(Color_t color, Float_t alpha){ GetPeakMarker()->SetMarkerColorAlpha(color,alpha); }
void PeakWindow::SetPeakMarkerStyle(Style_t style){ GetPeakMarker()->SetMarkerStyle(style); }
void PeakWindow::SetPeakMarkerSize(Size_t size){ GetPeakMarker()->SetMarkerSize(size); }

TLine* PeakWindow::GetEndLine(Double_t ymin, Double_t ymax)
{
  if( mEndLine==0 ){
    if( ymin==ymax ){ 
      if( mStartY<mEndY ){ymin=mStartY; ymax=mEndY; }
      else if(mStartY==mEndY){ ymin=mStartY; ymax=mPeakY; }
      else{ ymin=mEndY; ymax=mStartY; }
    }
    mEndLine = new TLine(mEndX,ymin,mEndX,ymax);
    mEndLine->SetLineColor(kGreen+2);
  }
  else{
    mEndLine->SetX1(mEndX);
    mEndLine->SetX2(mEndX);
    if( ymin==ymax ){ return mEndLine; }
    mEndLine->SetY1(ymin);
    mEndLine->SetY2(ymax);
  }
  return mEndLine;
}

Color_t PeakWindow::GetEndLineColor() const
{ if( mEndLine!=0 ){ return mEndLine->GetLineColor(); } return 0; }
Style_t PeakWindow::GetEndLineStyle() const
{ if( mEndLine!=0 ){ return mEndLine->GetLineStyle(); } return 0; }
Width_t PeakWindow::GetEndLineWidth() const
{ if( mEndLine!=0 ){ return mEndLine->GetLineWidth();} return 0; }
void PeakWindow::SetEndLineColor(Color_t color){ GetEndLine()->SetLineColor(color); }
void PeakWindow::SetEndLineColorAlpha(Color_t color,Float_t alpha){ GetEndLine()->SetLineColorAlpha(color,alpha); }
void PeakWindow::SetEndLineStyle(Style_t style){ GetEndLine()->SetLineStyle(style); }
void PeakWindow::SetEndLineWidth(Width_t width){ GetEndLine()->SetLineWidth(width); }

