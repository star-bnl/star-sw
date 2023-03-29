#include "PeakAna.h"
#include "PeakAnaPainter.h"

ClassImp(PeakAna)

PeakAna::PeakAna()
{
  mG_Data = 0;
  Init();
}

PeakAna::PeakAna(TGraph* data )
{
  mG_Data = data;//Set graph object now so doesn't get created in 'Init'
  Init();//This sets internal signal to true so set it false after call
  mInternalSignal=false;
}

PeakAna::PeakAna( int size, double* xvals, double* yvals )
{
  mG_Data = new TGraph(size,xvals,yvals);
  Init();
}

PeakAna::PeakAna( TH1* hist )
{
  mG_Data = ConvertHistToGraph(hist);
  Init();
}

void PeakAna::Init()
{
  mDEBUG = 0;
  mSearch.mStartX = 0; mSearch.mEndX=1;
  
  mComputedIndex = -1;//This will keep from having to determine peaks every time
  mInternalSignal = true;
  mBaseline = -5.0;
  mBaselineSigma = 0.75;
  mBaselineSigmaScale = 4.0;

  mMaxX = -5.0;
  mMaxY = -5.0;

  if( mG_Data==0 ){ mG_Data = new TGraph(); }

  mXRangeMin = 0;
  mXRangeMax = 1;
  mYRangeMin = 0;
  mYRangeMax = 1;
  mFoundPeak.SetWindow(mXRangeMin,mXRangeMax);

  mTunnelScale = 1;//1 is default
  mTunnelSigma = 1;//1 is default
  mTunnelThreshold = -1;//Turn off by default

  mChiralityPeakScale = 1;//default is scale by 1
  mChiralityScale = 1;//default is peaks must be centered in window
  mChiralityProbScale = 1;
  mChiralityThreshold = -1;//Turn off by default

  mDeltaX = -1.0;//Turn off by default
  mFilter = 0;
  mFilterScale = 1;
  mFilterWeights = 0;

  mPainter = 0;
}

PeakAna::PeakAna(const PeakAna &OldAna,TGraph* graph)
{
  //Note that the graph is not cloned because PeakAna should not be changing the contents of the data
  if( graph!=0 ){  mG_Data = graph; }//Set graph object now so doesn't get created in 'Init'
  else{ mG_Data = OldAna.GetData(); }//If no graph object given then clone the graph
  if( mG_Data!=0 ){ mG_Data = (TGraph*)mG_Data->Clone(); }//In case 'OldAna' graph is zero then don't call clone, since if it is zero Init() will create a new graph object
  Init();
  if( graph!=0 ){mInternalSignal=false;}//Init() sets internal signal to true so set it false after call if external graph object given

  ((PeakAna&)OldAna).Copy(*this);

}

PeakAna& PeakAna::operator=(const PeakAna& rhs)
{
  if( this == &rhs ){ return *this; }
  //Note that the graph is not cloned because PeakAna should not be changing the contents of the data
  mG_Data = rhs.GetData();//Set graph object now so doesn't get created in 'Init'
  Init();//This sets internal signal to true so set it false after call
  mInternalSignal=false;//Since graph is not copied don't treat as an internal signal
  
  ((PeakAna&)rhs).Copy(*this);

  return *this;
}

//Note that this function doesn't copy the graph object or a copies the vector of peaks. This is done on purpose becuase the idea is that the copy should run its own analysis and the algorithm is robust enough to generate the same results. To copy all objects use TObject::Clone
void PeakAna::Copy(TObject& obj) const
{
  TObject::Copy(obj);//Copy TObject bits

  ((PeakAna&)obj).mMaxX = mMaxX;
  ((PeakAna&)obj).mMaxY = mMaxY;

  ((PeakAna&)obj).mXRangeMin = mXRangeMin;
  ((PeakAna&)obj).mYRangeMin = mYRangeMin;
  ((PeakAna&)obj).mXRangeMax = mXRangeMax;
  ((PeakAna&)obj).mYRangeMax = mYRangeMax;
  ((PeakAna&)obj).mBaseline = mBaseline;
  ((PeakAna&)obj).mBaselineSigma = mBaselineSigma;
  ((PeakAna&)obj).mBaselineSigmaScale = mBaselineSigmaScale;
  ((PeakAna&)obj).mSearch.SetWindow(SearchPeak(),SearchWidth());
  ((PeakAna&)obj).mDeltaX = mDeltaX;
  ((PeakAna&)obj).mFilter = mFilter;
  ((PeakAna&)obj).mFilterScale = mFilterScale;
  //((PeakAna&)obj).mFilterSigma = mFilterSigma;
  if( ((PeakAna&)obj).mFilterWeights!=0 ){ delete [] ((PeakAna&)obj).mFilterWeights; ((PeakAna&)obj).mFilterWeights = 0; }
  if( mFilterWeights!=0 && mFilterScale>0 ){
    ((PeakAna&)obj).mFilterWeights = new double[2*mFilterScale+1];
    std::copy( mFilterWeights,mFilterWeights+(2*mFilterScale+1), ((PeakAna&)obj).mFilterWeights );
  }
  else{ ((PeakAna&)obj).mFilterWeights = 0; }
  ((PeakAna&)obj).mTunnelScale = mTunnelScale;
  ((PeakAna&)obj).mTunnelSigma = mTunnelSigma;
  ((PeakAna&)obj).mTunnelThreshold = mTunnelThreshold;
  
  ((PeakAna&)obj).mPainter = 0;//Dont' copy painter
}

TObject* PeakAna::Clone(const char* newname) const
{
  TGraph* cloneg = (TGraph*)GetData()->Clone(newname);
  PeakAna* ana = new PeakAna(cloneg);
  Copy(*ana);
  ana->ForceInternal();//Cloned graphs should always be internal
  if( FoundPeakIndex()>=0 ){
    //If mComputedIndex>=0 this means AnalyzeForPeak() was called and need to copy the peaks. This is preffered over re-running the analysis as copy should be faster than re-running the data
    ana->mComputedIndex = mComputedIndex;
    for( UInt_t i=0; i<mPeaks.size(); ++i ){
      ana->mPeaks.push_back( mPeaks.at(i) );
    }
    ana->mFoundPeak = mFoundPeak;
  }
  return ana;
}

PeakAna::~PeakAna()
{
  if(mInternalSignal ){delete mG_Data;}
  delete [] mFilterWeights;
  delete mPainter;
}

Int_t PeakAna::AnalyzeForPeak()
{
  ResetPeak();
  if( mFilter == 1 ){ MeanFilter(mFilterScale,false); }
  if( mFilter == 2 ){ GausFilter(mFilterScale,false); }
  this->GetPossiblePeaks();
  //Doesn't make sense to merge for number of peaks <= 1
  if( mChiralityThreshold>=0 && mPeaks.size()>1 ){
    std::vector<PeakWindow> mergedpeaks;
    this->MergeByChirality(mergedpeaks);
    mPeaks.swap( mergedpeaks );
  };
  mComputedIndex = this->SearchForPeak( mPeaks );//Returns a valid index for the possible peak vector or the size of the vector for an invalid peak
  if( mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ mFoundPeak = mPeaks.at(mComputedIndex); }
  else{
    mFoundPeak.SetWindow(mXRangeMax+1,mXRangeMax+1);//If no valid index then set found peak to greater than max X values so it is obvious something's wrong
  }
  //@[May 10, 2022]>mFoundPeak is a copy of the one in the vector. When drawing the windows the one in the vector is accessed not 'mFoundPeak' which is why the color for the one in the vector is changed and not mFoundPeak.
  return mComputedIndex;
}

Int_t PeakAna::AnalyzeForPeak(Double_t peak, Double_t width)
{
  SetSearchWindow(peak,width);
  return this->AnalyzeForPeak();
}

Int_t PeakAna::AnalyzeForNoisyPeak()
{
  ResetPeak();
  this->GetPossiblePeaks();
  PeakAna noisyana = PeakAna::ConvertPeaksToAna(*this);
  noisyana.SetTunnelThreshold(-1);//Don't use peak tunneling for this kind of peak finding
  noisyana.AnalyzeForPeak();
  //Replace old peak values with new one (maybe use swap in future??)
  mPeaks.clear();
  for( Int_t inoisy=0; inoisy<noisyana.NPeaks(); ++inoisy )
  {
    mPeaks.push_back( noisyana.GetPeak(inoisy) );
  }
  mComputedIndex = this->SearchForPeak( mPeaks );//Returns a valid index for the possible peak vector or the size of the vector for an invalid peak
  if( mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){
    mFoundPeak = mPeaks.at(mComputedIndex); //No offset for now rethink in the future??
    //@[May 10, 2022]>mFoundPeak is a copy of the one in the vector. When drawing the windows the one in the vector is accessed not 'mFoundPeak' which is why the color for the one in the vector is changed and not mFoundPeak.
    //mPeaks.at(mComputedIndex).SetStartLineColor(kRed);
    //mPeaks.at(mComputedIndex).SetEndLineColor(kOrange);
  }
  else{mFoundPeak.SetWindow(mXRangeMax+1,mXRangeMax+1);}//If no valid index then set found peak to greater than max X values so it is obvious something's wrong
  return mComputedIndex;
}

bool PeakAna::ValidPeakIdx() const
{
  if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){return true;}
  else{return false;}
}

bool PeakAna::GoodWindow()
{
  if( mComputedIndex<0 ){this->AnalyzeForPeak();}
  //First check if peak values are within the specified range
  if( mFoundPeak.mStartX<mXRangeMin || mFoundPeak.mStartX>mXRangeMax ){return false;}
  else if( mFoundPeak.mEndX<mXRangeMin || mFoundPeak.mEndX > mXRangeMax ){return false;}
  //Next check if values make physical sense
  else if( mFoundPeak.mStartX==mFoundPeak.mEndX ){return false;}
  else if( mFoundPeak.mStartX > mFoundPeak.mEndX ){return false;}
  else{return true;}
}

void PeakAna::GetXYMax(Double_t xmin, Double_t xmax)
{
  if( mG_Data==0 ){ return; }
  for( int i=0; i<mG_Data->GetN(); ++i ){
    Double_t X; Double_t Y;
    mG_Data->GetPoint(i,X,Y);
    if( X<xmin || X>xmax ){continue;}
    if( Y>mMaxY ){ mMaxY=Y; mMaxX=X; }
  }
  if( mMaxY<0 && mMaxX<0 ){
    LOG_WARN << "Unable to find a maximum adc\nSetting to impossible values" << endm;
    mMaxY = mYRangeMax;
    mMaxX = mXRangeMax;
  }
}

Double_t PeakAna::PeakX()
{
  if( mComputedIndex<0 ){ this->AnalyzeForPeak(); }
  if( mFoundPeak.mP_Peak<0){ return 0; }
  else{ Double_t x,y; mG_Data->GetPoint(mFoundPeak.mP_Peak,x,y); return x;}
}

Double_t PeakAna::PeakY()
{
  if( mComputedIndex<0 ){this->AnalyzeForPeak();}
  if( mFoundPeak.mP_Peak<0){ return 0; }
  else{ Double_t x,y; mG_Data->GetPoint(mFoundPeak.mP_Peak,x,y); return y;}
}

void PeakAna::PeakXY(Double_t &xval, Double_t &yval)
{
  if( mComputedIndex<0 ){this->AnalyzeForPeak();}
  if( mFoundPeak.mP_Peak<0 ){ xval=0; yval=0; }
  else{ mG_Data->GetPoint(mFoundPeak.mP_Peak,xval,yval); }
}

void PeakAna::Print(Option_t* opt) const
{
  std::cout << "|P::Graph:"<<mG_Data;
  std::cout << "|RangeX:["<<mXRangeMin<<","<<mXRangeMax<<"]";
  std::cout << "|RangeY:["<<mYRangeMin<<","<<mYRangeMax<<"]";
  std::cout << "|Flag::Internal:"<<mInternalSignal;
  std::cout << "|Base:"<<mBaseline;
  std::cout << "|BaselineSigma:"<<mBaselineSigma;
  std::cout << "|TunnelA:"<<mTunnelScale << "|S:" << mTunnelSigma << "|T:" << mTunnelThreshold;
  std::cout << std::endl;
  std::cout << "|SearchWindow::"; mSearch.Print();
  std::cout << std::endl;
  std::cout << "|FoundWindow::"; mFoundPeak.Print();
  std::cout << std::endl;
  std::cout << "|MaxX:"<<mMaxX;
  std::cout << "|MaxY:"<<mMaxY;
  std::cout << std::endl;
  std::cout << "Weights:" << mFilterWeights << "|";
  if( mFilterWeights!=0 ){ for( int i=0; i<mFilterScale*2+1; ++i ){ std::cout << mFilterWeights[i] << ","; } }
  std::cout << std::endl;
  std::cout << " - Peaks:" << std::endl;
  for( UInt_t i=0; i<mPeaks.size(); ++i ){
    std::cout << " + |i:"<<i;
    mPeaks.at(i).Print();
    std::cout << std::endl
              << "   + |Prob:"<< mPeaks.at(i).PeakTunnelProb(mG_Data,mTunnelScale,mTunnelSigma)
      //<< "|Chir:"<< mPeaks.at(i).PeakChirality(mChiralityPeakScale, mChiralityScale)
      //<< "|ChirProb:"<< mPeaks.at(i).PeakChiralityProb(mChiralityProbScale,mChiralityPeakScale,mChiralityScale)
      //<< "|ChirVal:"<< this->MergeLeftOrRight(i)
              << std::endl;
  }
}

void PeakAna::ResetPeak()
{
  //If computed index is negative then analyzeforpeak was never called and it is not needed to reset anything
  if( mComputedIndex>=0 ){ 
    mComputedIndex = -1;
    mPeaks.clear();
    mFoundPeak.Reset(mXRangeMin,mXRangeMax);
  }
}

void PeakAna::SetData(TGraph* graph)
{
  if( graph==0 ){LOG_ERROR << "PeaAna::SetData - Graph cannot be 0!" << endm; return;}
  ResetPeak();
  if( mInternalSignal && mG_Data!=0 ){delete mG_Data;}
  mG_Data = graph;
  mInternalSignal = false;
}

void PeakAna::SetData(TH1* hist, UInt_t numavgs)
{
  if( hist==0 ){LOG_ERROR << "PeakAna::SetData - Histogram cannot be 0" << endm; return;}
  ResetPeak();
  if( mInternalSignal ){delete mG_Data;}
  mG_Data = PeakAna::ConvertHistToGraph(hist,numavgs);
  mInternalSignal = true;
}

void PeakAna::SetFilter( UInt_t filter, Int_t scale, Double_t sigma )
{
  mFilter = filter;
  mFilterScale=scale;
  if( scale<=0 ){ return; }
  if( sigma==0 ){ sigma=scale/2.0; }//if sigma is zero use scale/2
  if( mFilterWeights!=0 ){ delete [] mFilterWeights; mFilterWeights=0; }
  if( filter==2 ){ mFilterWeights = GaussianMatrix2D(scale,sigma); }
}

void PeakAna::SetBaseline(Double_t value, Double_t sigma)
{
  if( sigma<0){ LOG_WARN << "PeakAna::SetBaseline - Baseline sigma should not be less than zero\n" << endm; }
  mBaseline = value;
  mBaselineSigma = sigma;
}

void PeakAna::SetBaselineSigmaScale(Double_t scale)
{
  mBaselineSigmaScale = fabs(scale);
}

void PeakAna::SetRange( Double_t xmin, Double_t ymin, Double_t xmax, Double_t ymax)
{
  mXRangeMin = xmin;
  mYRangeMin = ymin;
  mXRangeMax = xmax;
  mYRangeMax = ymax;
}
void PeakAna::SetSearchWindow(Double_t peak, Double_t width)
{
  mSearch.SetWindow(peak,width);
  ResetPeak();//reset since old found peak should be invalid
}

void PeakAna::SetPeak(const Int_t peakpoint, const Double_t peakx )
{ mFoundPeak.mP_Peak=peakpoint; mFoundPeak.mPeakX=peakx; }
void PeakAna::SetWindow(const Int_t start, const Int_t end )
{ mFoundPeak.SetWindow(start,end); }

void PeakAna::SetWindow(PeakWindow window)
{ mFoundPeak=window; }

void PeakAna::SetTunnelScale(Double_t value)
{
  if( value>=0 ){mTunnelScale = value;}
}
void PeakAna::SetTunnelSigma(Double_t value)
{
  if( value>0 ){mTunnelSigma = value;}
}
void PeakAna::SetTunnelPars(Double_t scale, Double_t sigma)
{
  SetTunnelScale(scale);
  SetTunnelSigma(sigma);
}
void PeakAna::SetTunnelThreshold(Double_t value)
{
  if( value<=1 ){ mTunnelThreshold=value; }
}


TGraph* PeakAna::ConvertHistToGraph(TH1* hist, UInt_t numavgs)
{
  Int_t nbins = hist->GetNbinsX();
  //Only consider averaging when number of bins to average are greater than or equal to 1 or less than the number of bins
  if( numavgs<1 || static_cast<Int_t>(numavgs)>nbins ){return 0;}//strictly greater than since GetNbinsX()+1 is overflow
  TGraph* AvgGr = new TGraph();
  for( Int_t ibin=1; ibin<=nbins; ibin+=numavgs ){
    Double_t sum=0;
    UInt_t counter=0;//In case number of averages is not same as how many sums were performed
    for(Int_t i=ibin; i<ibin+static_cast<Int_t>(numavgs) && i<=nbins; ++i ){ sum += hist->GetBinContent(i); ++counter; }
    Double_t avg = sum/static_cast<Double_t>(counter);
    //Set graph's point to center of x range
    Double_t xlow = hist->GetBinLowEdge(ibin);
    Double_t xhigh = hist->GetBinLowEdge(ibin+counter);
    AvgGr->SetPoint(AvgGr->GetN(),(xlow+xhigh)/2.0,avg);//This is for knowing the x-value
  }
  return AvgGr;
}

TGraph* PeakAna::ConvertPeaksToGraph(const std::vector<PeakWindow> &Peaks)
{
  if( Peaks.size()==0 ){return 0;}
  TGraph* graph = new TGraph();
  for( UInt_t ipeak=0; ipeak<Peaks.size(); ++ipeak )
  {
    graph->SetPoint( ipeak,Peaks.at(ipeak).mPeakX,Peaks.at(ipeak).MidPoint() );
  }
  return graph;
}

void PeakAna::ConvertPeaksToGraph()
{
  TGraph* graph = new TGraph();
  for( UInt_t ipeak=0; ipeak<mPeaks.size(); ++ipeak )
  {
    graph->SetPoint( ipeak,mPeaks.at(ipeak).mPeakX,mPeaks.at(ipeak).MidPoint() );
  }
  SetData(graph);
  ForceInternal();//new graph replaces old one so needs to be deleted
  return;
}

PeakAna PeakAna::ConvertPeaksToAna(const PeakAna &Ana)
{
  TGraph* graph = new TGraph();
  for( Int_t ipeak=0; ipeak<Ana.NPeaks(); ++ipeak ){
    graph->SetPoint(ipeak, Ana.GetPeak(ipeak).mPeakX, Ana.GetPeak(ipeak).MidPoint() );
  }
  PeakAna NewAna(Ana,graph);
  NewAna.ForceInternal();//new graph so needs to be deleted
  return NewAna;
}

PeakAna* PeakAna::MeanFilter( Int_t sizeavgs, bool copy )
{
  if( GetData()==0 ){return this;}
  const int npoints = GetData()->GetN();
  if( sizeavgs==0 || npoints<=1 ){ return this; }
  if( sizeavgs<0 ){ sizeavgs = -sizeavgs; }
  double ynew[npoints];
  double* xdata = GetData()->GetX();
  double* ydata = GetData()->GetY();
  for( int ipoint=0; ipoint<npoints; ++ipoint ){
    double sumweights = 1;
    if( mFilterWeights!=0 ){ sumweights = mFilterWeights[sizeavgs]; }
    double sumvalues = sumweights*ydata[ipoint];
    int nextpoint_m = ipoint;
    int nextpoint_p = ipoint;
    double lastx_m = xdata[ipoint];
    double lastx_p = xdata[ipoint];
    for( int sizecounter=0; sizecounter<sizeavgs; ++sizecounter ){
      //@[July 6, 2022]>Also need to check minimum and maximum x-values??
      Double_t delxerr = 0.001*mDeltaX;//Since comparing doubles add 0.1% tolerance to mDeltaX
      if( (nextpoint_m-1)>=0 ){
	if( mDeltaX<=0 || ((mDeltaX-delxerr)<=fabs(xdata[nextpoint_m-1]-lastx_m) && fabs(xdata[nextpoint_m-1]-lastx_m)<=(mDeltaX+delxerr)) ){
	  //Next point is mDeltaX away so valid point
	  nextpoint_m -= 1;
	  lastx_m = xdata[nextpoint_m];
	  if( mFilterWeights!=0 ){ sumvalues += mFilterWeights[sizeavgs-sizecounter-1]*ydata[nextpoint_m]; }
	  else{ sumvalues += ydata[nextpoint_m]; }
	}
	else{
	  //Next point is not mDeltaX away so invalid point and add baseline and decrement lastx value
	  lastx_m -= mDeltaX;
	  if( mFilterWeights!=0 ){ sumvalues += mFilterWeights[sizeavgs-sizecounter-1]*Baseline(); }
	  else{ sumvalues += Baseline(); }
	}
	//Some y-value was added so increment number of points/weights
	if( mFilterWeights!=0 ){ sumweights += mFilterWeights[sizeavgs-sizecounter-1]; }
	else{ sumweights += 1; }
      }
      if( (nextpoint_p+1)<npoints ){
	if( mDeltaX<=0 || ((mDeltaX-delxerr)<=fabs(xdata[nextpoint_p+1]-lastx_p) && fabs(xdata[nextpoint_p+1]-lastx_p)<=(mDeltaX+delxerr)) ){
	  nextpoint_p += 1;
	  lastx_p = xdata[nextpoint_p];
	  if( mFilterWeights!=0 ){ sumvalues += mFilterWeights[sizeavgs+sizecounter+1]*ydata[nextpoint_p]; }
	  else{ sumvalues += ydata[nextpoint_p]; }
	}
	else{
	  lastx_p += mDeltaX;
	  if( mFilterWeights!=0 ){ sumvalues += mFilterWeights[sizeavgs+sizecounter+1]*Baseline(); }
	  else{ sumvalues += Baseline(); }
	}
	if( mFilterWeights!=0 ){ sumweights += mFilterWeights[sizeavgs+sizecounter+1]; }
	else{ sumweights += 1; }
      }
    }
    ynew[ipoint] = sumvalues/sumweights;
  }

  if( copy ){
    TGraph* filtered = new TGraph(npoints,xdata,ynew);
    PeakAna* ana = new PeakAna(*this,filtered);
    ana->ForceInternal();
    return ana;
  }
  if( mInternalSignal ){
    //@[July 6, 2022] > (Copy arrays in c++ )[https://stackoverflow.com/questions/16137953/is-there-a-function-to-copy-an-array-in-c-c]
    std::copy( ynew, ynew+npoints, ydata);
    //[July 3, 2022]>Taken from TGraph CtorAllocate(). Setting minimum and maximum to -1111 effectivley resets the  minimum and maximum.
    GetData()->SetMinimum(-1111);
    GetData()->SetMaximum(-1111);
  }
  else{ 
    TGraph* graph = new TGraph(npoints,xdata,ynew );
    SetData(graph);
    ForceInternal();
  }
  ResetPeak();
  return this;
}

PeakAna* PeakAna::GausFilter( Int_t sizeavgs, bool copy )
{
  if( GetData()==0 ){return this;}
  const int npoints = GetData()->GetN();
  if( sizeavgs==0 || npoints<=1 ){ return this; }
  if( sizeavgs<0 ){ sizeavgs = -sizeavgs; }
  double ynew[npoints];
  double* xdata = GetData()->GetX();
  double* ydata = GetData()->GetY();
  for( int ipoint=0; ipoint<npoints; ++ipoint ){
    double sumweights = 1;
    if( mFilterWeights!=0 ){ sumweights = mFilterWeights[sizeavgs]; }
    double sumvalues = sumweights*ydata[ipoint];
    int nextpoint_m = ipoint;
    int nextpoint_p = ipoint;
    double lastx_m = xdata[ipoint];
    double lastx_p = xdata[ipoint];
    for( int sizecounter=0; sizecounter<sizeavgs; ++sizecounter ){
      //Also need to check minimum and maximum x-values??
      Double_t delxerr = 0.001*mDeltaX;//Since comparing doubles add 0.1% tolerance to mDeltaX
      if( (nextpoint_m-1)>=0 ){
	if( mDeltaX<=0 || ((mDeltaX-delxerr)<=fabs(xdata[nextpoint_m-1]-lastx_m) && fabs(xdata[nextpoint_m-1]-lastx_m)<=(mDeltaX+delxerr)) ){
	  //Next point is mDeltaX away so valid point
	  nextpoint_m -= 1;
	  lastx_m = xdata[nextpoint_m];
	  if( mFilterWeights!=0 ){ sumvalues += mFilterWeights[sizeavgs-sizecounter-1]*ydata[nextpoint_m]; }
	  else{ sumvalues+=ydata[nextpoint_m]; }
	}
	else{
	  //Next point is not mDeltaX away so invalid point and add baseline and decrement lastx value
	  //I should add baselines even if nextpoint+-1 is outside array (is this padding)?
	  lastx_m -= mDeltaX;
	  if( mFilterWeights!=0 ){ sumvalues += mFilterWeights[sizeavgs-sizecounter-1]*Baseline(); }
	  else{ sumvalues += Baseline(); }
	}
      }
      else{
	//nextpoint_m is now negative so add padding by copying first point
	if( mFilterWeights!=0 ){ sumvalues += mFilterWeights[sizeavgs-sizecounter-1]*ydata[0]; }
	else{ sumvalues += ydata[0]; }
      }
      if( (nextpoint_p+1)<npoints ){
	if( mDeltaX<=0 || ((mDeltaX-delxerr)<=fabs(xdata[nextpoint_p+1]-lastx_p) && fabs(xdata[nextpoint_p+1]-lastx_p)<=(mDeltaX+delxerr)) ){
	  nextpoint_p += 1;
	  lastx_p = xdata[nextpoint_p];
	  if( mFilterWeights!=0 ){ sumvalues += mFilterWeights[sizeavgs+sizecounter+1]*ydata[nextpoint_p]; }
	  else{ sumvalues += ydata[nextpoint_p]; }
	}
	else{
	  lastx_p += mDeltaX;
	  if( mFilterWeights!=0 ){ sumvalues += mFilterWeights[sizeavgs+sizecounter+1]*Baseline(); }
	  else{ sumvalues += Baseline(); }
	}
      }
      else{
	//nextpoint_p is now >=npoints so add padding by copying last point
	if( mFilterWeights!=0 ){ sumvalues += mFilterWeights[sizeavgs+sizecounter+1]*ydata[npoints-1]; }
	else{ sumvalues += ydata[npoints-1]; }
      }
      //Some y-value was added for either case so increment number of points/weights accordingly
      if( mFilterWeights!=0 ){
	sumweights += mFilterWeights[sizeavgs-sizecounter-1];
	sumweights += mFilterWeights[sizeavgs+sizecounter+1];
      }
      else{ sumweights += 2; }
    }
    ynew[ipoint] = sumvalues/sumweights;
  }


  if( copy ){
    TGraph* filtered = new TGraph(npoints,xdata,ynew);
    PeakAna* ana = new PeakAna(*this,filtered);
    ana->ForceInternal();
    return ana;
  }
  if( mInternalSignal ){
    //@[July 6, 2022] > (Copy arrays in c++ )[https://stackoverflow.com/questions/16137953/is-there-a-function-to-copy-an-array-in-c-c]
    std::copy( ynew, ynew+npoints, ydata);
    //[July 3, 2022]>Taken from TGraph CtorAllocate(). Setting minimum and maximum to -1111 effectivley resets the  minimum and maximum.
    GetData()->SetMinimum(-1111);
    GetData()->SetMaximum(-1111);
  }
  else{
    TGraph* graph = new TGraph(npoints,xdata,ynew );
    SetData(graph);
    ForceInternal();
  }
  ResetPeak();
  return this;
}

PeakAna PeakAna::ConvertPeaksToAna()
{
  return PeakAna::ConvertPeaksToAna(*this);
}

bool PeakAna::PeakTunnel(const PeakWindow &window) const
{
  if( mTunnelThreshold>=0 ){
    Double_t prob = window.PeakTunnelProb(mG_Data,mTunnelScale,mTunnelSigma);
    if( GetDebug()>1 ){
      LOG_DEBUG << "PeakAna::PeakTunnel - |prob:"<<prob << "|thr:"<<mTunnelThreshold;
      if( GetDebug()>2 ){ LOG_DEBUG << "|scale:"<<mTunnelScale << "|sigma:"<< mTunnelSigma; }
      LOG_DEBUG << endm;
    }
    if( prob>mTunnelThreshold ){return true; }
    else{ return false; }
    
  }
  return false;
}

Double_t PeakAna::PeakProb(const PeakWindow &window, Double_t scale, Double_t sigma ) const
{
  return window.PeakTunnelProb(mG_Data,scale,sigma);
}

void PeakAna::GetPossiblePeaks()
{
  std::vector<PeakWindow> PossiblePeak;//Gather all the possible occurances when signal is larger than Sigma
  if( GetDebug()>1 ){LOG_DEBUG << "PeakAna - In GetPossiblePeaks" << endm; }
  
  Double_t baseline = Baseline();//Already checked baseline above
  Double_t slopecutoff = BaselineSigma()*BaselineSigmaScale();
  if( GetDebug() > 2){
    LOG_DEBUG << "|baseline:"<<baseline << "|slopecutoff:"<<slopecutoff << endm;
  }
  PeakWindow peak(mXRangeMin-1,mXRangeMax+1);//peak values must be less than and greater than range for algorithm to work
  Double_t LocalMax=mXRangeMin-1;//Variable to help keep track of when slope changes (has to be less than minimum)
  if( GetDebug()>1 ){LOG_DEBUG << "Finding Peak for cutoff " << slopecutoff << endm;}
  //The idea is that you start with all things negative and loop through all the ADC values.  When the slope is greater than the cutoff you save it and then the signal will rise and then fall so slope will eventually go negative and this is when you keep track of the end values.  Once signal goes positive again or you no longer pass the slope cutoff stop and save the start and stop values.
  if( GetDebug()>2 ){LOG_DEBUG << "PeakAna::GetPossiblePeaks:Start graph reading loop" << endm;}
  Int_t npoints = mG_Data->GetN();
  for( Int_t ismp=0; ismp<npoints-1; ++ismp ){
    Double_t LX; Double_t LY;//L for left (actually current point)
    Double_t RX; Double_t RY;//R for right (actually next point)
    mG_Data->GetPoint(ismp,LX,LY);
    mG_Data->GetPoint(ismp+1,RX,RY);
    if( GetDebug()>1 ){LOG_DEBUG << "  - |P:"<<ismp <<"|LX:"<<LX << "|LY:"<<LY<< "|RX:"<<RX << "|RY:"<<RY << endm;}

    //Reject values outside the range
    if( LX < mXRangeMin ){if( GetDebug()>2 ){LOG_DEBUG << "'LX' too small"<<endm;} continue; }
    if( LX > mXRangeMax ){if( GetDebug()>2 ){LOG_DEBUG << "'LX' too large"<<endm;} continue; }
    if( LY < mYRangeMin || LY > mYRangeMax){if( GetDebug()>2 ){LOG_DEBUG << "'LY' outside y range"<<endm;} continue; }
    if( RY < mYRangeMin || RY > mYRangeMax){if( GetDebug()>2 ){LOG_DEBUG << "'RY' outside y range"<<endm;} continue; }
    Double_t Slope = (RY-LY)/(RX-LX);
    if( mDeltaX>0 ){
      Double_t delxerr = 0.001*mDeltaX;//Since comparing doubles add 0.1% tolerance to mDeltaX
      if( !((mDeltaX-delxerr)<=(RX-LX) && (RX-LX)<=(mDeltaX+delxerr)) ){
	if( GetDebug()>2 ){ LOG_DEBUG << "  - |DeltaX:"<<mDeltaX << "|RX-LX:"<<RX-LX << endm;}
	RX=LX+mDeltaX;
	RY=mYRangeMin;//Setting RY=mYRangeMin here is a flag that indicates a discontinuity
	Slope = -1.0;//Force slope to be negative to stop peak search or to prevent start of search
      }
    }
    if( GetDebug()>2 ){LOG_DEBUG << "  - |Slope:"<<Slope << endm;}
    //The purpose of this if statement is to ensure that any cases when there is no start time and no end time and the 'Ladc'==0 then we skip those points.  The additional nested if statement is for the case when there is a start and end time (like when the alogithm is working on negative slopes) and the 'Ladc' will still be >0; however since I dynamically change the "end" time until the slope changes to positive this statement ensures that 'continue' only gets called on positive slope results and not negative slopes when it is trying to find the correct end time.
    if( GetDebug()>2 ){LOG_DEBUG << "  - peak|ismp:"<<ismp << "|start:"<<peak.mStartX << "|end:"<<peak.mEndX << endm;}
    //Check above will skip bad values
    if( peak.mStartX<mXRangeMin  ){//No start time yet
      if( GetDebug()>1 ){LOG_DEBUG << "    + No StartTime" << endm;}
      if( LY>baseline+slopecutoff && Slope>0 ){
	//Needs to be checked sequentially since we need to reject any points below the baseline+cutoff that may have a large slope
	if( GetDebug()>1 ){LOG_DEBUG << "    + Passed Slope and baselineCutoff setting as start time" << endm;}
	peak.mStartX=LX;//Set start x-value
	peak.mStartY=LY;//Set start y-value
	LocalMax=LX;//Start checking local maximums
      }
      //If didn't pass thresholds for start time then continue to next point
    }
    else{//Found a suitable start time which indicates positve slope above some thershold
      //The statements below will now check subsequent values and if the slope is >= 0 then we have wrong local max so set local max to next value.  Note this means that localmax is not greater than End which is mXRangeMax+1.  Once slope goes below 0 End will take that value and local max will be less than End.  Once the slope changes again LocalMax will now be greater than End
      if( GetDebug()>1 ){LOG_DEBUG << "    + Found StartTime:"<< peak.mStartX << "|LocalMax:"<<LocalMax<<endm;}
      if( Slope >= 0 ){
	LocalMax=LX; //keep moving local max as long slope>=0
	if( Slope==0 && peak.mP_Peak>=0 ){ peak.mEndX=LX; } //if slope==0 and a peak was found i.e. mP_Peak>=0 then don't end finding but keep moving the end point i.e. peak.mEndX=LX. This condition is needed because sometimes a slope is zero and then will start decreasing again later. The importance of finding a peak is because it establishes the fact that the curve is convave and zero slope is ambiguous in which direction it will go. If slopes keep decreasing then expands peak window, if slope changes sign at next point algorithm will naturally stop. This is the desired behavior. Using the condition `Slope>0` does not produce the same the effect because sometimes this kind of zero slope behavior happens on the increasing slope side of the data and this should not be stopping the search because the next one could be positive.
	if( GetDebug()>2){LOG_DEBUG<<"      + Slope>=0|"<<Slope<<endm;}
      }
      else{ //Slope < 0
	peak.mEndX=LX;
	if(peak.mP_Peak<0){
	  peak.mP_Peak=ismp;
	  mG_Data->GetPoint(ismp,peak.mPeakX,peak.mPeakY);
	}
	if(GetDebug()>2){LOG_DEBUG<<"      + Slope<0"<<endm;}
      }
      if( LocalMax>peak.mEndX || LY<baseline+slopecutoff || ismp==npoints-2 || RY<=mYRangeMin  ){//Slope is now positive again or there was a postive slope to negative but did not fall below threshold
	if( ismp==npoints-2 ){ peak.mEndX=RX; peak.mEndY=RY; if(peak.mP_Peak<0){peak.mP_Peak=ismp+1;} }//last point is actually RX,RY not LX,LY and if no peak found then set last point as found peak
	else{peak.mEndX=LX; peak.mEndY = LY;}//all others should be current point
	if( GetDebug()>2 ){LOG_DEBUG << "    + Slope positive again|StartX:"<<peak.mStartX << "|StartY:"<<peak.mStartY << "|EndX:"<<peak.mEndX << "|EndY:"<<peak.mEndY << "|P:"<<peak.mP_Peak <<"|PeakX:"<<peak.mPeakX << "|PeakY:"<<peak.mPeakY << "|PeakProb:"<< peak.PeakTunnelProb(mG_Data,mTunnelScale,mTunnelSigma) << endm;}
	if( PeakTunnel(peak) ){
	  Int_t n = PossiblePeak.size();
	  if( n==0 ){ PossiblePeak.push_back(peak); }//In case vector is empty and tunnel probablity is true
	  else if( n>0 ){
	    Double_t leftprob = PossiblePeak.back().PeakTunnelProb(mG_Data,mTunnelScale,mTunnelSigma);
	    Double_t rightprob = peak.PeakTunnelProb(mG_Data,mTunnelScale,mTunnelSigma);
	    PeakWindow newpeak = PeakWindow::Combine(PossiblePeak.back(),peak, leftprob<=rightprob?true:false);//Take peak with highest probability
	    PossiblePeak[n-1] = newpeak;
	  }
	}
	else{ PossiblePeak.push_back(peak); }//Store windows
	PossiblePeak.back().SetPeak(mG_Data);
	if( GetDebug()>1){LOG_DEBUG << "  * |Found:"; PossiblePeak.back().Print("debug"); LOG_DEBUG<<endm;}
	peak.Reset(mXRangeMin-1,mXRangeMax+1);//Reset variables
	if( GetDebug()>2){LOG_DEBUG << "  * |peakreset:"; peak.Print("debug"); LOG_DEBUG<<endm;}
	LocalMax=mXRangeMin-1;
	--ismp;//Go back one point in case new signal starts at where slope was positive again
      }
    }
  }
  if( GetDebug()>0 ){ LOG_DEBUG << "PeakAna::GetPossiblePeaks:End graph reading loop|SizePeaks:" <<PossiblePeak.size() << endm; }

  mPeaks.swap(PossiblePeak);
}

//Returns index that corresponds to the serach criteria
Int_t PeakAna::SearchForPeak(const std::vector<PeakWindow> &PossiblePeaks)
{
  if( PossiblePeaks.size()==0 ){
    if( GetDebug()>0 ){LOG_DEBUG << "PeakAna::SearchForPeak - Error:Unable to find a valid peak\nReturning impossible index" << endm; }
    return PossiblePeaks.size();
  }
  else{
    if( GetDebug()>0 ){
      LOG_DEBUG << "|Size of Possible peaks:"<<PossiblePeaks.size();
      LOG_DEBUG << "|Search Peak:"<<mSearch.mStartX;
      LOG_DEBUG << "|Search Width:"<<mSearch.mEndX;
      LOG_DEBUG << endm;
    }
    Short_t peakindex=-1;
    for( UShort_t ipeak=0; ipeak<PossiblePeaks.size(); ++ipeak ){
      if( GetDebug()>1 ){
	LOG_DEBUG << " - ";
	LOG_DEBUG << "|Index:"<<ipeak;
	PossiblePeaks.at(ipeak).Print("debug");
	LOG_DEBUG << endm;
      }
      Double_t PeakLoc = PossiblePeaks.at(ipeak).mPeakX;
      if( mSearch.mStartX-mSearch.mEndX<=PeakLoc && PeakLoc <= mSearch.mStartX+mSearch.mEndX ){
	peakindex=ipeak;
	if( GetDebug()>1 ){
	  LOG_DEBUG << "   + ";
	  LOG_DEBUG << "|TrueIndex:"<<peakindex;
	  LOG_DEBUG << endm;
	}
      }
    }
    if( peakindex>=0 && mSearch.mStartX>0 && PossiblePeaks.at(peakindex).mP_Peak>0 ){
      if(GetDebug()>1){PossiblePeaks.at(peakindex).Print("debug");LOG_DEBUG<<endm;}
      return peakindex;
    }
    else{ return PossiblePeaks.size();}
  }
}

Int_t PeakAna::SearchForPeak(const std::vector<PeakWindow> &PossiblePeaks, const PeakWindow& search)
{
  mSearch = search;
  return this->SearchForPeak(PossiblePeaks);
}

Int_t PeakAna::SearchForPeak(const std::vector<PeakWindow> &PossiblePeaks, Double_t peak, Double_t width)
{
  SetSearchWindow(peak, width);
  return this->SearchForPeak(PossiblePeaks);
}

void PeakAna::MergeByProbability(std::vector<PeakWindow>& newpeaks) const
{
  if( !newpeaks.empty() ){ newpeaks.clear(); }
  for( UInt_t i=0; i<mPeaks.size(); ++i){
    if( PeakTunnel(mPeaks.at(i)) ){
      if( newpeaks.empty() ){ newpeaks.push_back(mPeaks.at(i)); continue; }//vector is empty so add first found peak
      Double_t thisprob = newpeaks.back().PeakTunnelProb(mG_Data,mTunnelScale,mTunnelSigma);
      Double_t nextprob = mPeaks.at(i).PeakTunnelProb(mG_Data,mTunnelScale,mTunnelSigma);
      newpeaks.back().Combine(mPeaks.at(i),thisprob<=nextprob?true:false);//Take peak with highest probability
    }
    else{ newpeaks.push_back(mPeaks.at(i)); }
  }
}

void PeakAna::MergeByChirality(std::vector<PeakWindow>& newpeaks) const
{
  //In order to check peaks to merge globally make a vector that keeps the order of the peaks vector and whose value will be +1 to merge with right peak or -1 to merge with left peak, 0 is neutral.
  //The idea is consecutive +1  would merge with consecutive -1. The zero is special since a +1 0 -1 pattern should be merged into a single peak but a 1 0 0 -1 pattern should not be
  //std::cout << "PeakAna::MergeByChirality - Start" << std::endl;
  std::vector<short> mergelist;
  for( UInt_t i=0; i<mPeaks.size(); ++i ){
    //std::cout << "PeakAna::MergeByChirality - index:" << i << std::endl;
    //Double_t probchir = mPeaks.at(i).PeakChiralityProb(mChiralityProbScale,mChiralityScale);
    //if(  probchir > mChiralityThreshold ){ mergelist.push_back(0); }
    //else{ mergelist.push_back( this->MergeLeftOrRight(i) ); }
    mergelist.push_back( this->MergeLeftOrRight(i) );
  }

  //std::cout << "PeakAna::MergeByChirality - peaksize:" << mPeaks.size()  << std::endl;
  //std::cout << "PeakAna::MergeByChirality - mergesize:" << mergelist.size()  << std::endl;
  std::vector< std::pair<int,int> > coupledindexs = this->MergeIndices(mergelist);

  newpeaks.clear();//Clear any data in the peak
  for( UInt_t i=0; i<mPeaks.size(); ++i ){
    bool combined = false;
    for( UInt_t j=0; j<coupledindexs.size(); ++j ){
      if(static_cast<int>(i)==coupledindexs.at(j).first){
        PeakWindow combpeak=mPeaks.at( i );
        for( int k=coupledindexs.at(j).first+1; k<=coupledindexs.at(j).second; ++k ){
          Double_t thisprob = combpeak.PeakTunnelProb(mG_Data,mTunnelScale,mTunnelSigma);
          Double_t otherprob = mPeaks.at(k).PeakTunnelProb(mG_Data,mTunnelScale,mTunnelSigma);
          combpeak.Combine(mPeaks.at(k),thisprob<=otherprob?true:false);
        }//k loop
        newpeaks.push_back(combpeak);
        i=coupledindexs.at(j).second;
        combined = true;
      }
    }
    if( !combined ){ newpeaks.push_back( mPeaks.at(i) ); }
  }
  //std::cout << "PeakAna::MergeByChirality - newpeaksize:" << newpeaks.size()  << std::endl;
}

short PeakAna::MergeLeftOrRight(UInt_t index) const
{
  if( index>=mPeaks.size() ){return 0;}//invalid index
  if( mPeaks.size()==1 ){return 0;}//Only 1 peak don't need to merge
  //PeakWindow combined = mPeaks.at(index);
  //if( mPeaks.at(index).PeakChiralityProb(mChiralityProbScale,mChiralityScale) > mChiralityThreshold ){return 0;}
  Double_t leftchir = 0;
  Double_t thischir = mPeaks.at(index).PeakChirality(mChiralityPeakScale,mChiralityScale);
  Double_t rightchir = 0;
  if( thischir==0 || thischir==1 ){return 0;}//Sanity check [Feb, 28 2022]>(If slope exactly zero then chirality is 1 for now, maybe use small fluctation around 1 since 0 should mean nonexistent exclusively at least when chirality only depnds on mse??)

  if( index==0 ){
      rightchir = mPeaks.at(index+1).PeakChirality(mChiralityPeakScale,mChiralityScale);
      if( mPeaks.at(index+1).PeakChiralityProb(mChiralityProbScale,rightchir) > mChiralityThreshold ){
      rightchir = 0; }
  }
  else if( index+1==mPeaks.size() ){
    leftchir = mPeaks.at(index-1).PeakChirality(mChiralityPeakScale,mChiralityScale);
    if( mPeaks.at(index-1).PeakChiralityProb(mChiralityProbScale,leftchir) > mChiralityThreshold ){
    leftchir = 0; }
  }
  else if( 0<index && index<mPeaks.size() ){
    leftchir = mPeaks.at(index-1).PeakChirality(mChiralityPeakScale,mChiralityScale);
    rightchir = mPeaks.at(index+1).PeakChirality(mChiralityPeakScale,mChiralityScale);
    if( mPeaks.at(index-1).PeakChiralityProb(mChiralityProbScale,leftchir) > mChiralityThreshold ){
      leftchir = 0;
    }
    if( mPeaks.at(index+1).PeakChiralityProb(mChiralityProbScale,rightchir) > mChiralityThreshold ){
      rightchir = 0;
    }
  }
  else{ std::cout << "PeakAna::MergeLeftOrRight - WARNING:Invalid index" << std::endl; }

  //Check the 4 different cases
  if( leftchir==0 && rightchir==0 ){ return 0; }//don't merge since left and right did not pass thresholds or do not exist
  else if( leftchir!=0 && rightchir==0 ){//ignore right chirality
    if( thischir*leftchir<0 ){return -1;}//opposite signs so merge with left
    else{ return 0; }
  }
  else if(
    leftchir==0 && rightchir!=0 ){//ignore left chirality
    if( thischir*rightchir<0 ){return 1;}//opposite signs so merge with right
    else{ return 0; }
  }
  else{//leftchir!=0 && rightchir!=0
    bool leftoppsign=false;//is left and this chirality opposite signs
    if( (leftchir*rightchir)<0 ){leftoppsign=true;}
    bool rightoppsign=false;//is right and this chirality opposite signs
    if( (rightchir*thischir)<0 ){rightoppsign=true;}

    Double_t leftchirprob = mPeaks.at(index-1).PeakChiralityProb(mChiralityProbScale,leftchir);
    Double_t rightchirprob = mPeaks.at(index+1).PeakChiralityProb(mChiralityProbScale,rightchir);

    if( leftoppsign ){
      if( rightoppsign ){//both left and right have opposite signs
      //Merge with the lower chirality probability favoring left for equality so they will get merged??
      //or maybe need to keep going recursively by calling this function in the direction of the chirality, this way it will stop when chiralities are on longer going back and forth??
      if( leftchirprob<rightchirprob ){ return -1; } else{ return 1; }
      }
      else{ return -1; }//only left peak has opposite sign
    }
    else{
      if( rightoppsign ){ return 1; }//only right peak has opposite sign
      else{//both left and right have same sign
      //Merge with the lower chirality favoring the left for equality so they will get merged??
      //For same sign merge in the direction of chirality??
        if( thischir>0 ){return 1;}
        else{ return -1; }
        //if( leftchirprob<rightchirprob ){ return -1; }else{return 1;}
      }
    }
  }
  //if( MergeLeft(leftchir,thischir,rightchir) ){ return -1; }
  //else{ return 1; }
}

bool PeakAna::MergeLeft(Double_t leftchir, Double_t thischir, Double_t rightchir) const
{
  //Here it is the magnitude that should matter since a lower negative will still be less than any positive one, but zero should be treated as special case since don't compute for zero??
  bool leftoppsign=false;//is left and this chirality opposite signs?
  if( (leftchir*rightchir)<0 ){leftoppsign=true;}
  bool rightoppsign=false;//is right and this chirality opposite signs?
  if( (rightchir*thischir)<0 ){rightoppsign=true;}

  if( leftoppsign ){
    if( rightoppsign ){//both left and right have opposite signs
      //Merge with the lower chirality favoring right for equality
      if( leftchir<rightchir ){ return true; } else{ return false; }
    }
    else{ return true; }//only left peak has opposite sign
  }
  else{
    if( rightoppsign ){ return false; }//only right peak has opposite sign
    else{//both left and right have same sign
      //Merge with the lower chirality favoring the left for equality
      if( leftchir<=rightchir ){ return true; }else{return true;}
    }
  }
}

std::vector< std::pair<int,int> > PeakAna::MergeIndices(std::vector<short>& vec) const
{
  int firstpos_index = -1;
  int lastneg_index = -1;
  int zero_count = 0;//counting number of consecutive zeros when firstpos_index is not
  std::vector< std::pair<int,int> > mergeindexs;
  for( int i=0; i<static_cast<int>(vec.size()); ++i ){
    //std::cout << "entrance|("<<i<<")"<<vec.at(i)<<"|firstpos:"<<firstpos_index << "|lastneg:"<<lastneg_index << std::endl;
    if( vec.at(i)>=0 ){
      if( firstpos_index<0 ){ if( vec.at(i)>0 ){firstpos_index = i;} }
      else{
        if( lastneg_index>firstpos_index ){
          //store
          std::pair<int,int> mergepair(firstpos_index,lastneg_index);
          mergeindexs.push_back(mergepair);
          //std::cout << "store|firstpos:"<<firstpos_index << "|lastneg:"<<lastneg_index << std::endl;
          if( vec.at(i)==0 ){firstpos_index = -1;}
          else{ firstpos_index=i; }
          lastneg_index = -1;
          zero_count = 0;
          continue;
        }
        if( vec.at(i)==0 ){
          zero_count++;
          if( zero_count < 1 ){ firstpos_index=-1; lastneg_index=-1; zero_count=0; }
          continue;
        }
        //std::cout << "reset|firstpos:"<<firstpos_index << "|lastneg:"<<lastneg_index << std::endl;
      }
    }
    if( firstpos_index>=0 ){
      if( vec.at(i)<0 ){
        lastneg_index = i;
      }
    }
  }
  return mergeindexs;
}

void PeakAna::Draw(Option_t* opt)
{
  AppendPad(opt);
  //gPad->IncrementPaletteColor(1, mOption); (Doesn't work with ROOT v5) 
}

void PeakAna::Paint(Option_t* opt)
{
  GetPainter(opt);
 
  if (mPainter) {
    if (strlen(opt) > 0){ mPainter->Paint(opt); }
    else{ mPainter->Paint(mOption.Data()); }
  }
}

PeakAnaPainter* PeakAna::GetPainter(Option_t *opt)
{
  if( mPainter==0 ){
    mPainter = new PeakAnaPainter();
    mPainter->SetPeakAna(this);
  }
  mOption = opt;
  return mPainter;
}

void PeakAna::AddPeakStats(TPaveText* pave, const char* opt)
{
  if( pave==0 ){ return; }
  TString option(opt);
  bool statsalloption = false;
  bool statsdetailoption = false;

  option.ToLower();
  if( option.Contains("a") ){ statsalloption = true; }
  if( option.Contains("d") ){ statsdetailoption = true; }

  pave->AddText("--Found Peaks--");

  if( mComputedIndex<0 ){ this->AnalyzeForPeak(); }
  if( mComputedIndex == static_cast<Int_t>(mPeaks.size()) ){if( !statsalloption ){return;}}//if only writing stats for found peak don't process no found peak case

  //In future base the precision of the values based on the x/y range, and tunnel thershold value ??
  if( !statsalloption ){
    TText* t = pave->AddText( Form("I:%u|S:[%2.2f,%2.2f]|E[%2.2f,%2.2f]|P(%d,%2.2f,%2.2f)",
      mComputedIndex,
      (GetPeak(mComputedIndex)).mStartX,
      (GetPeak(mComputedIndex)).mStartY,
      (GetPeak(mComputedIndex)).mEndX,
      (GetPeak(mComputedIndex)).mEndY,
      (GetPeak(mComputedIndex)).mP_Peak,
      (GetPeak(mComputedIndex)).mPeakX,
      (GetPeak(mComputedIndex)).mPeakY
      ) );
    t->SetTextAlign(11);
    if( statsdetailoption ){
      TText* t2 = pave->AddText( Form(" + |Prob:%1.4f|Chir:%3.1f|ChirProb:%1.4f",
      (GetPeak(mComputedIndex)).PeakTunnelProb(GetData(),TunnelScale(),TunnelSigma()),
      (GetPeak(mComputedIndex)).PeakChirality(ChiralityPeakScale(),ChiralityScale()),
      (GetPeak(mComputedIndex)).PeakChiralityProb(ChiralityProbScale(),ChiralityPeakScale(),ChiralityScale())
      ) );
      t2->SetTextAlign(11);
    }
  }
  else{
    //pave->AddText( Form("|X:[%2.2f,%2.2f]|Y:[%2.2f,%2.2f]|BBS:(%d,%2.2f)|ProbSST:()") );
    for( Int_t ipeak=0; ipeak<NPeaks(); ++ipeak ){
      TText* t = pave->AddText( Form("|I:%u|S[%2.2f,%2.2f]|E[%2.2f,%2.2f]",
        ipeak,
        (GetPeak(ipeak)).mStartX,
        (GetPeak(ipeak)).mStartY,
        (GetPeak(ipeak)).mEndX,
        (GetPeak(ipeak)).mEndY
        ) );
      t->SetTextAlign(11);
      if( mComputedIndex==ipeak ){ t->SetTextColor(kRed+1); }
      if( statsdetailoption ){
        //|Chir:%3.1f|ChirProb:%1.4f
        //(GetPeak(ipeak)).PeakChirality(ChiralityPeakScale(),ChiralityScale()),
        //(GetPeak(ipeak)).PeakChiralityProb(ChiralityProbScale(),ChiralityPeakScale(),ChiralityScale())
        TText* t2 = pave->AddText( Form(" + |P(%d,%2.2f,%2.2f)|Prob:%1.4f",
          (GetPeak(ipeak)).mP_Peak,
          (GetPeak(ipeak)).mPeakX,
          (GetPeak(ipeak)).mPeakY,
          (GetPeak(ipeak)).PeakTunnelProb(GetData(),TunnelScale(),TunnelSigma())
          ) );
        t2->SetTextAlign(11);
        if( mComputedIndex==ipeak ){ t2->SetTextColor(kRed+1); }
      }
    }
  }

}

//Styling for graph
Color_t PeakAna::GetLineColor() const
{ if(mG_Data!=0 ){return mG_Data->GetLineColor();} return 0; }
Style_t PeakAna::GetLineStyle() const
{ if(mG_Data!=0 ){return mG_Data->GetLineStyle();} return 0; }
Width_t PeakAna::GetLineWidth() const
{ if(mG_Data!=0 ){return mG_Data->GetLineWidth();} return 0; }

Color_t PeakAna::GetFillColor() const
{ if(mG_Data!=0 ){return mG_Data->GetFillColor();} return 0; }
Style_t PeakAna::GetFillStyle() const
{ if(mG_Data!=0 ){return mG_Data->GetFillStyle();} return 0; }

Color_t PeakAna::GetMarkerColor() const
{ if(mG_Data!=0 ){return mG_Data->GetMarkerColor();} return 0; }
Size_t PeakAna::GetMarkerSize() const
{ if(mG_Data!=0 ){return mG_Data->GetMarkerSize();} return 0; }
Style_t PeakAna::GetMarkerStyle() const
{ if(mG_Data!=0 ){return mG_Data->GetMarkerStyle();} return 0; }

void PeakAna::SetLineColor(Color_t color)
{ if(mG_Data!=0 ){mG_Data->SetLineColor(color);} }
void PeakAna::SetLineColorAlpha(Color_t color, Float_t alpha)
{ if(mG_Data!=0 ){mG_Data->SetLineColorAlpha(color,alpha);} }
void PeakAna::SetLineStyle(Style_t style)
{ if(mG_Data!=0 ){mG_Data->SetLineStyle(style);} }
void PeakAna::SetLineWidth(Width_t width)
{ if(mG_Data!=0 ){mG_Data->SetLineWidth(width);} }

void PeakAna::SetFillColor(Color_t color)
{ if(mG_Data!=0 ){mG_Data->SetFillColor(color);} }
void PeakAna::SetFillColorAlpha(Color_t color, Float_t alpha)
{ if(mG_Data!=0 ){mG_Data->SetFillColorAlpha(color,alpha);} }
void PeakAna::SetFillStyle(Style_t style)
{ if(mG_Data!=0 ){mG_Data->SetFillStyle(style);} }

void PeakAna::SetMarkerColor(Color_t color)
{ if(mG_Data!=0 ){mG_Data->SetMarkerColor(color);} }
void PeakAna::SetMarkerColorAlpha(Color_t color, Float_t alpha)
{ if(mG_Data!=0 ){mG_Data->SetMarkerColorAlpha(color,alpha);} }
void PeakAna::SetMarkerSize(Size_t size)
{ if(mG_Data!=0 ){mG_Data->SetMarkerSize(size);} }
void PeakAna::SetMarkerStyle(Style_t style)
{ if(mG_Data!=0 ){mG_Data->SetMarkerStyle(style);} }

//Styling for peak painter
Color_t PeakAna::GetBaseLineColor() const
{ if( mPainter!=0 ){ return mPainter->GetBaseLineColor(); } return 0;}
Style_t PeakAna::GetBaseLineStyle() const
{ if( mPainter!=0 ){ return mPainter->GetBaseLineStyle(); } return 0;}
Width_t PeakAna::GetBaseLineWidth() const
{ if( mPainter!=0 ){ return mPainter->GetBaseLineWidth(); } return 0;}

Color_t PeakAna::GetHitLineColor() const
{ if( mPainter!=0 ){ return mPainter->GetHitLineColor(); } return 0;}
Style_t PeakAna::GetHitLineStyle() const
{ if( mPainter!=0 ){ return mPainter->GetHitLineStyle(); } return 0;}
Width_t PeakAna::GetHitLineWidth() const
{ if( mPainter!=0 ){ return mPainter->GetHitLineWidth(); } return 0;}

void PeakAna::SetBaseLineColor(Color_t color)
{GetPainter()->SetBaseLineColor(color);}
void PeakAna::SetBaseLineColorAlpha(Color_t color,Float_t alpha)
{GetPainter()->SetBaseLineColorAlpha(color,alpha);}
void PeakAna::SetBaseLineStyle(Style_t style)
{GetPainter()->SetBaseLineStyle(style);}
void PeakAna::SetBaseLineWidth(Width_t width)
{GetPainter()->SetBaseLineWidth(width);}

void PeakAna::SetHitLineColor(Color_t color)
{GetPainter()->SetHitLineColor(color);}
void PeakAna::SetHitLineColorAlpha(Color_t color,Float_t alpha)
{GetPainter()->SetHitLineColorAlpha(color,alpha);}
void PeakAna::SetHitLineStyle(Style_t style)
{GetPainter()->SetHitLineStyle(style);}
void PeakAna::SetHitLineWidth(Width_t width)
{GetPainter()->SetHitLineWidth(width);}

Color_t PeakAna::GetPeakLineColorS(UInt_t peakidx) const
{return GetPeak(peakidx).GetStartLineColor();}
Color_t PeakAna::GetPeakLineColorE(UInt_t peakidx) const
{return GetPeak(peakidx).GetEndLineColor();}
Style_t PeakAna::GetPeakStyleS(UInt_t peakidx) const
{return GetPeak(peakidx).GetStartLineStyle();}
Style_t PeakAna::GetPeakStyleE(UInt_t peakidx) const
{return GetPeak(peakidx).GetEndLineStyle();}
Width_t PeakAna::GetPeakWidthS(UInt_t peakidx) const
{return GetPeak(peakidx).GetStartLineWidth();}
Width_t PeakAna::GetPeakWidthE(UInt_t peakidx) const
{return GetPeak(peakidx).GetEndLineWidth();}

Color_t PeakAna::GetFoundPeakLineColorS() const
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ return GetPeak(mComputedIndex).GetStartLineColor(); } return 0; }
Color_t PeakAna::GetFoundPeakLineColorE() const
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ return GetPeak(mComputedIndex).GetEndLineColor(); } return 0; }
Style_t PeakAna::GetFoundPeakStyleS() const
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ return GetPeak(mComputedIndex).GetStartLineStyle(); } return 0; }
Style_t PeakAna::GetFoundPeakStyleE() const
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ return GetPeak(mComputedIndex).GetEndLineStyle(); } return 0; }
Width_t PeakAna::GetFoundPeakWidthS() const
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ return GetPeak(mComputedIndex).GetStartLineWidth(); } return 0; }
Width_t PeakAna::GetFoundPeakWidthE() const
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ return GetPeak(mComputedIndex).GetEndLineWidth(); } return 0; }

void PeakAna::SetFoundPeakLineColorS(Color_t s_color)
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ GetPeak(mComputedIndex).SetStartLineColor(s_color); } }
void PeakAna::SetFoundPeakLineColorE(Color_t e_color)
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ GetPeak(mComputedIndex).SetEndLineColor(e_color); } }
void PeakAna::SetFoundPeakLineColor(Color_t s_color, Color_t e_color)
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ GetPeak(mComputedIndex).SetStartLineColor(s_color); GetPeak(mComputedIndex).SetEndLineColor(e_color); } }
void PeakAna::SetFoundPeakStyleS(Style_t s_style)
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ GetPeak(mComputedIndex).SetStartLineStyle(s_style); } }
void PeakAna::SetFoundPeakStyleE(Style_t e_style)
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ GetPeak(mComputedIndex).SetEndLineStyle(e_style); } }
void PeakAna::SetFoundPeakStyle(Style_t s_style,Style_t e_style)
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ GetPeak(mComputedIndex).SetStartLineStyle(s_style); GetPeak(mComputedIndex).SetEndLineStyle(e_style); } }
void PeakAna::SetFoundPeakWidthS(Width_t s_width)
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ GetPeak(mComputedIndex).SetStartLineWidth(s_width); } }
void PeakAna::SetFoundPeakWidthE(Width_t e_width)
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ GetPeak(mComputedIndex).SetEndLineWidth(e_width); } }
void PeakAna::SetFoundPeakWidth(Width_t s_width, Width_t e_width)
{ if( 0<=mComputedIndex && mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ GetPeak(mComputedIndex).SetStartLineWidth(s_width); GetPeak(mComputedIndex).SetEndLineWidth(e_width); } }

void PeakAna::SetPeakLineColorS(UInt_t peakidx, Color_t s_color)
{ GetPeak(peakidx).SetStartLineColor(s_color); }
void PeakAna::SetPeakLineColorE(UInt_t peakidx, Color_t e_color)
{ GetPeak(peakidx).SetStartLineColor(e_color); }
void PeakAna::SetPeakLineColor(UInt_t peakidx, Color_t s_color, Color_t e_color)
{ GetPeak(peakidx).SetStartLineColor(s_color); GetPeak(peakidx).SetStartLineColor(e_color); }
void PeakAna::SetPeakLineColorAlphaS(UInt_t peakidx, Color_t s_color,Float_t s_alpha)
{ GetPeak(peakidx).SetStartLineColorAlpha( s_color, s_alpha); }
void PeakAna::SetPeakLineColorAlphaE(UInt_t peakidx, Color_t e_color,Float_t e_alpha)
{ GetPeak(peakidx).SetEndLineColorAlpha( e_color, e_alpha); }
void PeakAna::SetPeakLineColorAlpha(UInt_t peakidx, Color_t s_color,Float_t s_alpha, Color_t e_color, Float_t e_alpha)
{ GetPeak(peakidx).SetStartLineColorAlpha( s_color, s_alpha); GetPeak(peakidx).SetEndLineColorAlpha( e_color, e_alpha); }
void PeakAna::SetPeakLineStyleS(UInt_t peakidx, Style_t s_style)
{ GetPeak(peakidx).SetStartLineStyle(s_style); }
void PeakAna::SetPeakLineStyleE(UInt_t peakidx, Style_t e_style)
{ GetPeak(peakidx).SetEndLineStyle(e_style); }
void PeakAna::SetPeakLineStyle(UInt_t peakidx, Style_t s_style, Style_t e_style)
{ GetPeak(peakidx).SetStartLineStyle(s_style); GetPeak(peakidx).SetEndLineStyle(e_style); }
void PeakAna::SetPeakLineWidthS(UInt_t peakidx, Width_t s_width)
{ GetPeak(peakidx).SetStartLineWidth(s_width); }
void PeakAna::SetPeakLineWidthE(UInt_t peakidx, Width_t e_width)
{ GetPeak(peakidx).SetEndLineWidth(e_width); }
void PeakAna::SetPeakLineWidth(UInt_t peakidx, Width_t s_width, Width_t e_width)
{ GetPeak(peakidx).SetStartLineWidth(s_width); GetPeak(peakidx).SetEndLineWidth(e_width); }

void PeakAna::SetAllPeakLineColor(Color_t s_color, Color_t e_color)
{
  for( UInt_t ipeak=0; ipeak<mPeaks.size(); ++ipeak ){
    mPeaks.at(ipeak).SetStartLineColor(s_color);
    mPeaks.at(ipeak).SetEndLineColor(e_color);
  }
}
void PeakAna::SetAllPeakLineStyle(Style_t s_style, Style_t e_style)
{
  for( UInt_t ipeak=0; ipeak<mPeaks.size(); ++ipeak ){
    mPeaks.at(ipeak).SetStartLineStyle(s_style);
    mPeaks.at(ipeak).SetEndLineStyle(e_style);
  }
}
void PeakAna::SetAllPeakLineWidth(Width_t s_width, Width_t e_width)
{
  for( UInt_t ipeak=0; ipeak<mPeaks.size(); ++ipeak ){
    mPeaks.at(ipeak).SetStartLineWidth(s_width);
    mPeaks.at(ipeak).SetEndLineWidth(e_width);
  }
}

double* PeakAna::GaussianMatrix2D(int rx,  double sx, int ry, double sy, bool kNorm)
{
  if( rx<0 ){rx = -rx;}
  if( ry<0 ){ry = -ry;}
  if( sx==0 ){ sx = static_cast<double>(rx)*0.5; }
  if( sy==0 ){ sy = static_cast<double>(ry)*0.5; }
  const int nsizex = (2*rx+1);
  const int nsizey = (2*ry+1);
  
  double GM_sum = 0;
  //@[June 7, 2022]>[How to create 2D dynamic arrays](https://stackoverflow.com/questions/936687/how-do-i-declare-a-2d-array-in-c-using-new)
  double* GM_2D = new double[nsizex*nsizey];
  for( int ix=0; ix<nsizex; ++ix ){
    for( int jy=0; jy<nsizey; ++jy ){
      int xi = ix-rx;
      int yi = jy-ry;
      double product = 1.0;
      if( sx!=0 ){ product *= exp( (-1.0*xi*xi)/(2.0*sx*sx) )/(sqrt(2.0*3.14159265358979323846)*sx); }
      if( sy!=0 ){ product *= exp( (-1.0*yi*yi)/(2.0*sy*sy) )/(sqrt(2.0*3.14159265358979323846)*sy); }
      GM_2D[ix*nsizey+jy] = product;
      GM_sum += product;
    }
  }
  
  if( kNorm ){
    double invsum = 1.0/GM_sum;
    for( int i=0; i<nsizex; ++i ){
      for( int j=0; j<nsizey; ++j ){
	GM_2D[i*nsizey+j] *= invsum;
      }
    }
  }
  return GM_2D;
}

