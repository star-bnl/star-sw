#include "StFcsPulseAna.h"

ClassImp(StFcsPulseAna)

double StFcsPulseAna::MaxwellBoltzmannDist(double* x, double* p)
{
  if( x[0]<p[1] ){return p[3];}//Important condition for Maxwell Boltzmann distribution
  double xoff2 = (x[0]-p[1])*(x[0]-p[1]);
  return p[0]*TMath::Sqrt( 2.0/TMath::Pi() )
    *( xoff2 * exp((-1.0*xoff2)/(2.0*p[2]*p[2])) )
    / (p[2]*p[2]*p[2])
    + p[3];
}

StFcsPulseAna::StFcsPulseAna():PeakAna()
{
  mName = "StFcsPulseAna";
  Init();
}

StFcsPulseAna::StFcsPulseAna(std::string name):PeakAna()
{
  mName = name;
  //mG_Data = 0;//This is so 'Initialize' will create the TGraphAsymmErrors object
  Init();
}

StFcsPulseAna::StFcsPulseAna(TGraph* Sig, std::string name):PeakAna(Sig)
{
  mName = name;
  //mG_Data = Sig;//Set graph object now so doesn't get created in 'Initialize'
  //Initialize();//This sets internal signal to true so set it false after call
  //mInternalSignal=false;//should be done correctly in PeakAna
  Init();
}

StFcsPulseAna::StFcsPulseAna(const StFcsPulseAna& old, const char* post_name, TGraph* graph):PeakAna(old,graph)
{
  if( post_name ){ mName = old.mName+post_name; }
  else{ mName = old.mName+"_copy"; }
  //((StFcsPulseAna&)old).Copy(*this);//Since this gets called in base class copy constructor with argument 'this'; c++ correctly calls the 'Copy' of this class
   
  if( old.BaselineHist()!=0 ){mH1_Baseline = (TH1F*)old.BaselineHist()->Clone( (mName+"_H1_Basline").c_str() );}
  if( old.BaselineFit()!=0 ){mF1_BaselineFit = (TF1*)old.BaselineFit()->Clone( (mName+"_F1_BaselineFit").c_str() );}
  if( old.SignalFit()!=0 ){mF1_SignalFit = (TF1*)old.SignalFit()->Clone( (mName+"_F1_SignalFit").c_str() );}
}

StFcsPulseAna& StFcsPulseAna::operator=(const StFcsPulseAna& rhs)
{
  if( this == &rhs ){ return *this; }
  //Note that the graph is not cloned because PeakAna should not be changing the contents of the data
  PeakAna::operator = (rhs);
  //StFcsPulseAna::Init();
  
  //((StFcsPulseAna&)rhs).Copy(*this);
  mName = rhs.Name()+"_copy";

  if( rhs.BaselineHist()!=0 ){mH1_Baseline = (TH1F*)rhs.BaselineHist()->Clone( (mName+"_H1_Basline").c_str() );}
  if( rhs.BaselineFit()!=0 ){mF1_BaselineFit = (TF1*)rhs.BaselineFit()->Clone( (mName+"_F1_BaselineFit").c_str() );}
  if( rhs.SignalFit()!=0 ){ mF1_SignalFit = (TF1*)rhs.SignalFit()->Clone( (mName+"_F1_SignalFit").c_str() ); }

  return *this;
}

void StFcsPulseAna::Init()
{  
  mDbPulse = 0;

  mWindowSum = false; mSumAdc=0;
  mFitSum = false; mSumAdcFit=0.0;

  mH1_Baseline = 0;
  mF1_BaselineFit = 0;

  mF1_SignalFit = 0;
}

void StFcsPulseAna::Copy(TObject& obj) const
{
  PeakAna::Copy(obj);

  ((StFcsPulseAna&)obj).mDbPulse = mDbPulse;
  ((StFcsPulseAna&)obj).mWindowSum = mWindowSum;
  ((StFcsPulseAna&)obj).mSumAdc = mSumAdc;
  ((StFcsPulseAna&)obj).mFitSum = mFitSum;
  ((StFcsPulseAna&)obj).mSumAdcFit = mSumAdcFit;

  ((StFcsPulseAna&)obj).mH1_Baseline = 0;
  ((StFcsPulseAna&)obj).mF1_BaselineFit = 0;
  ((StFcsPulseAna&)obj).mF1_SignalFit = 0;
}

TObject* StFcsPulseAna::Clone(const char* newname) const
{
  TGraph* cloneg = (TGraph*)this->GetData()->Clone();
  StFcsPulseAna* ana = 0;

  if( !strlen(newname) ){ ana = new StFcsPulseAna(cloneg,mName+"_copy"); }
  else{ ana = new StFcsPulseAna(cloneg,newname); }
  //PeakAna::Copy(*ana);
  Copy(*ana);
  ana->ForceInternal();//Cloned graph should be deleted by cloned object
  
  if( this->FoundPeakIndex()>=0 ){
    //If mComputedIndex>=0 this means AnalyzeForPeak() was called and need to copy the peaks. This is preffered over re-running the analysis as copy should be faster than re-running the data
    ana->mComputedIndex = this->mComputedIndex;
    for( UInt_t i=0; i<mPeaks.size(); ++i ){ ana->mPeaks.push_back( mPeaks.at(i) ); }
    ana->mFoundPeak = this->mFoundPeak;
  }

  if( this->mH1_Baseline!=0 ){ ana->mH1_Baseline = (TH1F*)this->mH1_Baseline->Clone((ana->Name()+"_H1_Baseline").c_str()); }
  if( this->mF1_BaselineFit!=0 ){ ana->mF1_BaselineFit = (TF1*)this->mF1_BaselineFit->Clone((ana->Name()+"_F1_Baseline").c_str()); }
  if( this->mF1_SignalFit!=0 ){ ana->mF1_SignalFit = (TF1*)this->mF1_SignalFit->Clone((ana->Name()+"_F1_SignalFit").c_str()); }

  return ana;
}

StFcsPulseAna::~StFcsPulseAna()
{
  if(mH1_Baseline!=0)     { delete mH1_Baseline; }
  if(mF1_SignalFit!=0)    { delete mF1_SignalFit; }
  if(mF1_BaselineFit!=0)  { delete mF1_BaselineFit; }

}

void StFcsPulseAna::ResetFinder()
{
  PeakAna::SetData((TGraph*)0);//Effectively deletes and sets graph to zero and destroys found peaks
  ResetBaseline();
  ResetSum();
}

void StFcsPulseAna::ResetBaseline()
{
  mBaseline = -5.0;
  mBaselineSigma = 0.75;
  if( mH1_Baseline!=0 ){
    mH1_Baseline->Reset();
    mH1_Baseline->GetXaxis()->SetRangeUser(-0.5,4095.5);//Needs to go back to default since calling reset doesn't revert axes to original values
  }
  if( mF1_BaselineFit!=0 ){mF1_BaselineFit->ResetAttLine();}//Values from old fit will be replaced when 'AnalyzeForBaseline' gets called
}

void StFcsPulseAna::ResetSum()
{
  if( mF1_SignalFit!=0 ){ delete mF1_SignalFit; mF1_SignalFit=0; }//This was for plotting but still good to do just in case
  
  mWindowSum=false; mSumAdc = 0;
  mFitSum=false; mSumAdcFit = 0.0;
}

/*void StFcsPulseAna::FillInMissingData()
{
  for(UInt_t i=1; i<mG_Data->GetN(); ++i ){
    Double_t x0; Double_t y0;
    mG_Data->GetPoint(i-1,x0,y0);

    Double_t x1; Double_t y1;
    mG_Data->GetPoint(i,x1,y1);
  }
  }*/

Int_t StFcsPulseAna::AnalyzeForPeak()
{
  mComputedIndex = PeakAna::AnalyzeForPeak();
  if( mTunnelThreshold<0 ){
    mTunnelThreshold *= -1.0;
    if( mTunnelThreshold<=1.0 ){
      std::vector<PeakWindow> merged;
      this->MergeByProbability(merged);
      mPeaks.swap( merged );
      mTunnelThreshold *= -1.0;//Restore to old value
      mComputedIndex = this->SearchForPeak( mPeaks );
    }
  }
  if( mComputedIndex<static_cast<Int_t>(mPeaks.size()) ){ mFoundPeak = mPeaks.at(mComputedIndex); }
  else{mFoundPeak.SetWindow(mXRangeMax+1,mXRangeMax+1);}//If no valid index then set found peak to greater than max X values so it is obvious something's wrong

  //SetFoundPeakLineColor(kRed, kOrange);//Auto checks for valid index
  return mComputedIndex;
}

void StFcsPulseAna::MergeByProbability(std::vector<PeakWindow>& merged)
{
  if( !merged.empty() ){ merged.clear(); }
  for( UInt_t i=0; i<mPeaks.size(); ++i){
    PeakWindow win = mPeaks.at(i);
    if( merged.empty() ){ merged.push_back(win); continue; }
    if( win.mStartX-merged.back().mEndX > 2.1 ){ merged.push_back(win); continue;}//Skip merging peaks that are more than 2 timebins away

    if( PeakTunnel(win) ){
      if( merged.empty() ){ merged.push_back(win); continue; }//vector is empty so add first found peak
      Double_t thisprob = merged.back().PeakTunnelProb(mG_Data,mTunnelScale,mTunnelSigma);
      Double_t nextprob = win.PeakTunnelProb(mG_Data,mTunnelScale,mTunnelSigma);
      merged.back().Combine(win,thisprob<=nextprob?true:false);//Take peak with highest probability
    }
    else{ merged.push_back(win); }
  }
}

void StFcsPulseAna::AnalyzeForPedestal()
{
  ResetBaseline();//Resets histogram and function if they exist
  if( mH1_Baseline==0){mH1_Baseline = new TH1F( (mName+"_"+"H1_Baseline").c_str(),"",4096,-0.5,4095.5); mH1_Baseline->Sumw2(); mH1_Baseline->SetTitle("Baseline Histogram");}
  //Fill histogram of ADC values
  for( int ipoint=0; ipoint<this->GetData()->GetN(); ++ipoint)
    {
      Double_t tb; Double_t adc;
      Int_t check = this->GetData()->GetPoint(ipoint,tb,adc);
      if( check<0 ){ std::cout << "WARNING:Unable to find point " << ipoint << " in Signal graph" << std::endl; continue;}
      if( adc<0.1 ){continue;} //skip adc=0 from histogram since this is most likely missing data in non-pedestal subtracted data
      mH1_Baseline->Fill(adc);
      //if( mDEBUG ){std::cout << "|i:"<<ipoint <<"|TB:"<<tb <<"|ADC:"<<adc << std::endl;}
    }
  /*May 19, 2021: Fitting with too few data points gives bad baselines so just use max of histogram for now*/
  mBaseline = mH1_Baseline->GetMaximumBin()-1;//Offset by one to account for bin counting
  mBaselineSigma = 0.75;

  //Create a function to fit to histogram of ADC values
  if( mF1_BaselineFit==0 ){ mF1_BaselineFit = new TF1((mName+"_"+"F1_BaselineFit").c_str(),"gaus(0)",
  			    mH1_Baseline->GetXaxis()->GetXmin(),
  			    mH1_Baseline->GetXaxis()->GetXmax()
  			    );
  }
  if( FindBaseline() ){return;}
  else
    {
      std::cout << "WARNING:Unable to find a proper baseline" << std::endl;
      if( GetDebug()>2 ){std::cout << "->";Print();}
      else{std::cout << "->Found baseline:"<< mBaseline << " Found BaselineSigma:"<<mBaselineSigma << std::endl;}
      std::cout << "->Setting baseline to 0 and sigma to 0.75 to prevent T0 algorithm from failing" << std::endl;
      mBaseline=0;
      mBaselineSigma=0.75;
      return;
    }
}

//In the future make range around XStartVal as arguments to the function
bool StFcsPulseAna::FindBaseline()
{
  //std::cout << "In FindBaseline:" << std::endl;
  if( mBaseline > -0.001 )
    {
      if( GetDebug()>2 )
	{
	  std::cout << "|BaselineValue:"<<mBaseline;
	  std::cout << "|BaselineSigma:"<<mBaselineSigma;
	  //std::cout << "|(Default)SigmaFactor:"<<4.0;
	  //std::cout << "|ADC Threshold is SigmaFactor*BaselineSigma"
	  std::cout << std::endl;
	}
      return true;
    }
  if( mF1_BaselineFit==0 ){ return false; }
  if( mH1_Baseline==0 || mH1_Baseline->GetEntries()==0 ){mBaseline = -5.0; return false;}
  //For nonpedestal data use full range for pedestal data find largest bin and use a +-5 bin buffer for the fit
  Int_t XStartVal = mH1_Baseline->GetMaximumBin()-1;//Offset by one to make the value correct
  if( GetDebug()>2 ){ std::cout << "|Height:"<<mH1_Baseline->GetBinContent(XStartVal+1) << "|XStartVal:"<< XStartVal << "|Range:"<<3 <<"|StartSigma:"<<0.9 <<std::endl; }
  mF1_BaselineFit->SetRange(XStartVal-3,XStartVal+3);//Only do within this range
  mF1_BaselineFit->SetParameter(0,mH1_Baseline->GetBinContent(XStartVal+1));
  mF1_BaselineFit->SetParameter(1,XStartVal);//Use maximum instead of mean for more accurate center
  mF1_BaselineFit->SetParameter(2,0.9);//Right now fixed but may need changing in the future?
  /*Change this to use set function
  if( mPed==1 ){mF1_BaselineFit->SetParameter( 2,mH1_Baseline->GetRMS() );}//For pedestal RMS is good enough start point
  else{mF1_BaselineFit->SetParameter(2,0.6);}//0.6 is a good start value RMS is too large for non-pedestal data
  if( mPed==0 )//For physics fix the baseline to 0.0 with a sigma of 0.75 with tb range for 2019 in future change hard coded values
    {
      mF1_BaselineFit->SetRange(0,350);
      mF1_BaselineFit->SetParameter(0,1);
      mF1_BaselineFit->SetParameter(1,0.0);
      mF1_BaselineFit->SetParameter(2,0.75);//0.75 range so T0 finder will look for ADC > 3
    }
  else{mH1_Baseline->Fit(mF1_BaselineFit,"QNR");}//For anything other than physics data fit to find baseline
  */
  //Base line is mean of the gaussian fit
  char opt[5] = "NRQ";
  if( GetDebug()>2 ){ opt[2] = '\0';}//Hack to to not quiet fit output
  Int_t FitStatus = mH1_Baseline->Fit(mF1_BaselineFit,opt);
  if( GetDebug()>1 ){ std::cout << "|FitStatus:"<< FitStatus << std::endl; }
  if( FitStatus == 0 )
    {
      Double_t ratio = mF1_BaselineFit->GetChisquare()/static_cast<Double_t>(mF1_BaselineFit->GetNDF());
      if( ratio > 10.0 ){ mBaseline = XStartVal; return true; }//Take care of bad baseline fits by setting baseline to maximum
      else
	{ 
	  mBaseline = static_cast<Double_t>(mF1_BaselineFit->GetParameter(1));
	  mBaselineSigma = static_cast<Double_t>( fabs(mF1_BaselineFit->GetParameter(2)) );//fabs in case sigma is negative
	  return true;
	}
    }
  else if( FitStatus > 0 )
    { 
      //mBaseline = mH1_Baseline->GetMean();
      //Max works better May 19, 202
      mBaseline = mH1_Baseline->GetMaximumBin()-1;//Offset by one to account for bin counting
      mBaselineSigma = 0.75;
      if(GetDebug()>1){std::cout << "WARNING:Fit failed to converge setting baseline to mean of histogram and of sigma to 0.75" << std::endl;}
      return true;
    }
  else{return false;}
}

void StFcsPulseAna::FillAdc(TGraphAsymmErrors* g, unsigned short& counter, int Start, unsigned short* adcdata)
{
  //std::cout << " - StFcsPulseAna::FillAdc|Start:"<<Start << std::endl;
  //adcdata must be size 8 array
  if( counter != 0 ){return;}//counter must start with zero size
  unsigned int startpoint = 0;
  double tb = 0;
  double adc = 0;
  for( unsigned int ipoint=0; static_cast<int>(ipoint)<g->GetN() && counter<8; ++ipoint ){
    g->GetPoint(ipoint,tb,adc);
    //std::cout << " - StFcsPulseAna::FillAdc|tb:"<<tb<<"|adc:"<<adc << std::endl;
    if( int(tb)>=Start ){ startpoint=ipoint; /*std::cout << " - StFcsPulseAna::FillAdc|Start:"<<Start<<"|sp:"<<startpoint << std::endl;*/ break; }
  }
  
  if( startpoint==0 || static_cast<Int_t>(startpoint+8)>=g->GetN() ){return;}
  //unsigned short counter = 0;
  while( int(tb-(Start+counter)) < int(8-counter) ){
    counter = static_cast<unsigned short>(tb-Start);
    adcdata[counter]=adc;
    g->GetPoint(++startpoint,tb,adc);
  }
}

//Copied on Nov. 01, 2021
int StFcsPulseAna::SumDep0(TGraphAsymmErrors* gdata, int Start, int ped)
{
  unsigned short Size = 0;//Must start with zero size
  unsigned short AdcData[8] = {0};//Must be 8 timebin array  
  //for( UShort_t iprint=0; iprint<8; ++iprint ){ std::cout << " - B|StFcsPulseAna::Dep0::Data["<<iprint<<"]"<<":"<<AdcData[iprint] << std::endl; }
  StFcsPulseAna::FillAdc(gdata,Size,Start,AdcData);
  //for( UShort_t iprint=0; iprint<8; ++iprint ){ std::cout << " - A|StFcsPulseAna::Dep0::Data["<<iprint<<"]"<<":"<<AdcData[iprint] << std::endl; }
  if( Size==0 ){return 0;}
  
  //double ped = Baseline();
  int sum = 0 ;
  int peak = 0 ;
  int last = 0 ;
  
  for(int tb=0;tb<8;tb++) {

    unsigned short radc = AdcData[tb] ;
    
    switch(tb) {
    case 0 :
      last = radc ;
      sum = radc ;
      peak = 0 ;
      break ;
    case 1 :
      if(radc>sum) peak = 1 ;
      sum += radc ;
      last = radc ;
      break ;
    case 2 :
      sum += radc ;
      last = radc ;
      break ;
    case 3 :
      sum += radc ;
      last = radc ;
      break ;
    case 4 :
      sum += radc ;
      last = radc ;
      break ;
    case 5 :
      sum += radc ;
      last = radc ;
      break ;
    case 6 :
      sum += radc ;
      last = radc ;
      break ;
    case 7 :
    default :
      //printf("radc %d, last %d, peak %d\n",radc,last,peak) ;
      
      if(radc>=last && peak==0) {
	sum = 0 ;
      }
      else {
	sum += radc ;
	
	sum -= ped*8 ;	// ped is now only 3*ch_ped!
	if(sum < 0) sum = 0 ;
	/*
	if(sum>0 && fcs_trgDebug>=2){ 
	  if(geo.det<2){
	    printf("ns=%1d det=%1d dep=%2d ch=%2d sum=%6d gain=%6d s*g=%6d pT=%8.5f\n",
		   geo.ns,geo.det,geo.dep,geo.ch,
		   sum,pg->gain,(sum*pg->gain)>>8,
		   0.00024711*((sum*pg->gain)>>8) );
	  }else{
	    printf("ns=%1d det=%1d dep=%2d ch=%2d sum=%6d gain=%6d s*g=%6d MIP=%5.3f\n",
		   geo.ns,geo.det,geo.dep,geo.ch,
		   sum,pg->gain,(sum*pg->gain)>>8,
		   float((sum*pg->gain)>>8)/100.0);
	  }					
	}
	
	sum *= pg?pg->gain:0x100 ; // note that in FY21+ gain==1.0 is 0x100
	sum >>= 8 ;		   // see note above
	*/
      }
      
      break ;
    }
  }
  return sum;
  
}

int StFcsPulseAna::SumDep0Mod(TGraphAsymmErrors* gdata, int Start, int ped){
  unsigned short Size = 0;//Must start with zero size
  unsigned short AdcData[8] = {0};//Must be 8 timebin array  
  StFcsPulseAna::FillAdc(gdata,Size,Start,AdcData);
  if( Size==0 ){return 0;}

  //double ped = Baseline();
  int sum = 0 ;
  int peak = 0 ;
  int last = 0 ;
  //int rightshift = (8*sizeof(int)-1);
  
  for(int tb=0;tb<8;tb++) {

    unsigned short radc = AdcData[tb] ;
    
    switch(tb) {
    case 0 :
      last = radc ;
      sum = radc ;
      peak = 0 ;
      break ;
    case 1 :
      //std::cout << "    - SumDep0Mod::Case1|peak:"<<peak << "|radc:"<<radc<<"|sum:"<<sum << std::endl;
      if(radc>sum) peak = 1 ;
      sum += radc ;
      last = radc ;
      break ;
    case 2 :
      //if( peak>0 && radc<=last) peak ^= 1<<rightshift ;
      //std::cout << "    - SumDep0Mod::Case2|peak:"<<peak << "|radc:"<<radc<<"|last:"<<last << std::endl;
      //if( peak!=0 || radc<last) peak = -1 ;
      sum += radc ;
      last = radc ;
      break ;
    case 3 :
      //if( peak>0 && radc<=last) peak ^= 1<<rightshift ;
      //std::cout << "    - SumDep0Mod::Case3|peak:"<<peak << "|radc:"<<radc<<"|last:"<<last << std::endl;
      //if( peak!=0 || radc<last) peak = -1 ;
      sum += radc ;
      last = radc ;
      break ;
    case 4 :
      //std::cout << "    - SumDep0Mod::Case4|peak:"<<peak << "|radc:"<<radc<<"|last:"<<last << std::endl;
      //if( peak>0 && radc<=last) peak ^= 1<<rightshift ;
      //if( peak!=0 || radc<last) peak = -1 ;
      sum += radc ;
      last = radc ;
      break ;
    case 5 :
      //std::cout << "    - SumDep0Mod::Case5|peak:"<<peak << "|radc:"<<radc<<"|last:"<<last << std::endl;
      //if( peak>0 && radc<=last) peak ^= 1<<rightshift ;
      //if( peak!=0 || radc<last) peak = -1 ;
      sum += radc ;
      last = radc ;
      break ;
    case 6 :
      //std::cout << "    - SumDep0Mod::Case6|peak:"<<peak << "|radc:"<<radc<<"|last:"<<last << std::endl;
      //if( peak>0 && radc<=last) peak ^= 1<<rightshift ;
      if( peak!=0 || radc<last) peak = -1 ;
      sum += radc ;
      last = radc ;
      break ;
    case 7 :
    default :
      //std::cout << "    - SumDep0Mod::Case7|peak:"<<peak << "|radc:"<<radc<<"|last:"<<last << std::endl;
      //printf("radc %d, last %d, peak %d\n",radc,last,peak) ;
      //if( peak>0 && radc<=last) peak ^= 1<<rightshift ;//Needed to check last point
      if( peak!=0 || radc<last ) peak = -1 ;//Needed to check last point
      //std::cout << "    - SumDep0Mod::Check|peak:"<<peak << "|radc:"<<radc<<"|last:"<<last << std::endl;
      
      //if( peak>=0 ){ sum=0; }
      if( peak>0 ){ sum=0; }
      else {
	sum += radc ;
	
	sum -= ped*8 ;	// ped is now only 3*ch_ped!
	if(sum < 0) sum = 0 ;
	/*
	if(sum>0 && fcs_trgDebug>=2){ 
	  if(geo.det<2){
	    printf("ns=%1d det=%1d dep=%2d ch=%2d sum=%6d gain=%6d s*g=%6d pT=%8.5f\n",
		   geo.ns,geo.det,geo.dep,geo.ch,
		   sum,pg->gain,(sum*pg->gain)>>8,
		   0.00024711*((sum*pg->gain)>>8) );
	  }else{
	    printf("ns=%1d det=%1d dep=%2d ch=%2d sum=%6d gain=%6d s*g=%6d MIP=%5.3f\n",
		   geo.ns,geo.det,geo.dep,geo.ch,
		   sum,pg->gain,(sum*pg->gain)>>8,
		   float((sum*pg->gain)>>8)/100.0);
	  }					
	}
	
	sum *= pg?pg->gain:0x100 ; // note that in FY21+ gain==1.0 is 0x100
	sum >>= 8 ;		   // see note above
	*/
      }
      
      break ;
    }
  }
  return sum;  

}

Int_t StFcsPulseAna::Sum(Int_t Start, Int_t End)
{
  Int_t SumAdc = 0;
  Double_t base = Baseline();//A bit redundant but neccassray in case T0 had not been computed
  //std::cout << "|SumAdc:" <<SumAdc << "|Baseline:"<<base << std::endl;
  if( base < -4.0 ){return SumAdc;}
  if( mG_Data==0 || mG_Data->GetN()==0 ){return SumAdc;}
  //Ctools::PrintDebug<Int_t>(std::cout,"SumAdc",SumAdc);
  for( Int_t ismp=0; ismp<this->GetData()->GetN(); ++ismp )
    {
      Double_t tb; Double_t adc;
      this->GetData()->GetPoint(ismp,tb,adc);
      if( tb < Start ){continue;}
      if( tb > End ){ break; }
      //Ctools::PrintDebug<Int_t>(std::cout,"SumAdc",SumAdc);
      //Ctools::PrintDebug<Int_t>(std::cout,"tb",tb);
      //Ctools::PrintDebug<Int_t>(std::cout,"adc",adc);
      //std::cout << std::endl;
      SumAdc += adc;
    }
  if( GetDebug()>1 ){std::cout << "|Start:"<<Start << "|End:"<<End << "|SumAdcB:"<< SumAdc; }
  SumAdc -= base*abs(End-Start+1);//Add one to account for the fact that the adc value at start and end is being summed
  if( GetDebug()>1 ){std::cout << "|SumAdcA:"<< SumAdc << "|BaseArea:"<<base*abs(End-Start+1) << std::endl; }
  //std::cout << "FinalSum:"<<SumAdc << std::endl;
  return SumAdc;
}

Int_t StFcsPulseAna::SumWindow()
{
  if( mWindowSum ){ return mSumAdc; }
  mSumAdc = 0;
  //Upon failure to find a window Start and End value will be equal.  In this case just set some default window.  For run 2019 it will be 35-60 and triggered crossing was at 50
  //For 2019 Ecal Cosmic running signal window was between 110-140
  //if( SignalStart()==SignalEnd() ){ SetWindow(110,140); }
  if( !GoodWindow() ){ return mSumAdc; }
  mSumAdc = Sum( PeakStart(), PeakEnd() );
  mWindowSum = true;
  return mSumAdc;
}

void StFcsPulseAna::GetMBPars(const double& xpeak, const double& xrise, const double& yh, const double& ped, double& height, double& scale )
{
  /* For Clarification purposes
     Double_t Xh = mFoundPeak.mPeakX;              //X Position of peak
     rise = mFoundPeak.mStartX;                    //X position where graph start to rise
     scale = (Xh-Xrise)/TMath::Sqrt(2.0); //Scale factor
     Double_t Yh = PeakY();       //Y height of peak
     ped = Baseline();                    //Y-Offset
     height = ((Yh-ped)*TMath::Sqrt(TMath::Pi()/2.0)*scale*TMath::E())/2.0;//Height of Maxwell Boltzmann
  */
  scale = (xpeak-xrise)/TMath::Sqrt2();//Scale of Maxwell Boltzmann
  height = ((yh-ped)*TMath::Sqrt(TMath::Pi()/2.0)*scale*TMath::E())/2.0;//Height of Maxwell Boltzmann
  return;
}

void StFcsPulseAna::SignalMBPars(double& height, double& scale)
{
  if( !GoodWindow() ){return;}
  GetMBPars(mFoundPeak.mPeakX,mFoundPeak.mStartX,PeakY(),Baseline(),height,scale);
}

double StFcsPulseAna::SumMB()
{
  if( !GoodWindow() ){ return 0;}
  double scale=0; double height=0;
  SignalMBPars(height,scale);
  return height;//This is the integral of the Maxwell-Boltzmann distribution above
  //I=\int_0^\infty x^2*e^{-x^2/a^2} = \sqrt(\pi)*a^3*0.25 (a is scale factor;source(Mar1,2021):https://en.wikipedia.org/wiki/Gaussian_integral)
  //For Maxwell boltzmann a=\sqrt(2)*scale and 0 goes to x0 (rise time)
  //Using this and subsistitiution the integral becomes I=\sqrt(2*\pi)*scale^3*0.5*PreFactor
  //Where PreFactor = height*\sqrt(2/\pi)/scale^3
  //Putting it all together and simplifying I=height
}

TF1* StFcsPulseAna::SignalFit(const char option)
{
  switch(option)
    {
    case 'g': if( !mFitSum ){ GausFit(); } break;
    case 'm': if( !mFitSum ){ MBFit(); } break;
    case 'p': if( !mFitSum ){ PulseFit(); } break;
    default: break;
    }
  return mF1_SignalFit;
}

Double_t StFcsPulseAna::GausFit(Int_t Start,Int_t End)
{
  if( mFitSum ){ return mSumAdcFit; }
  mSumAdcFit = 0.0;
  Double_t base = Baseline();//A bit redundant but necessary in case T0 had not been computed
  if( base < -4.0 ){ return mSumAdcFit; }
  if( mG_Data==0 || mG_Data->GetN()==0 ){return mSumAdcFit;}
  if( Start==0 && End==0 ){Start=PeakStart(); End=PeakEnd();}//Default case
  //Upon failure to find a window 'Start' and 'End' value will be equal.  In this case just set some default window.  For run 2019 it will be 35-60 and triggered crossing was at 50
  //For 2019 Ecal Cosmic running signal window was between 110-140
  //if( Start==End ){ SetWindow(110,140); Start=115; End=135; }//40 and 55 here since I add to range later
  //Sanity Checks
  if( !GoodWindow() ){ return mSumAdcFit; }
  if( mF1_SignalFit!=0 ){ delete mF1_SignalFit; mF1_SignalFit=0; }
  mF1_SignalFit = new TF1( (mName+"_Gaus_"+"mF1_SignalFit").c_str(),"gaus(0)+[3]",Start,End);
  
  //mG_Data->GetXaxis()->SetRangeUser(Start-5,End+5);
  mF1_SignalFit->SetParameter( 0, MaxY() );
  mF1_SignalFit->SetParameter( 1, MaxX() );
  mF1_SignalFit->SetParameter( 2, static_cast<Double_t>(End-Start)*0.2 );//Seemed to work most of the time with 0.3, 0.5 seemed too much
  mF1_SignalFit->SetParameter( 3, base );
  char opt[5] = "NRQ";
  if( GetDebug()>2 ){ opt[2] = '\0';}//Hack to to not quiet fit output
  Int_t fitstatus = this->GetData()->Fit( mF1_SignalFit, opt );
  if( fitstatus >= 0 )
    {
      //Fix to 4 sigma for now in the future make it a variable?
      Double_t FitStart = mF1_SignalFit->GetParameter(1) - 4.0*fabs(mF1_SignalFit->GetParameter(2));
      Double_t FitEnd = mF1_SignalFit->GetParameter(1) + 4.0*fabs(mF1_SignalFit->GetParameter(2));
      mSumAdcFit = mF1_SignalFit->Integral(FitStart,FitEnd);//Sum only in the range and subtract the area under the baseline
      if( GetDebug()>2 ){std::cout << "|FitStart:"<<FitStart << "|FitEnd:"<<FitEnd << "|FitSumB:"<<mSumAdcFit; }
      mSumAdcFit -= ((FitEnd-FitStart)*base);
      if( GetDebug()>2 ){std::cout << "|FitSumA:"<<mSumAdcFit << "|BaseArea:"<<((FitEnd-FitStart)*base) << std::endl; }
      mFitSum = true;
      return mSumAdcFit;
    }
  else{ mFitSum = true; return mSumAdcFit; }//If fit failed return 0 don't do a histogram sum
}

Double_t StFcsPulseAna::MBFit(Int_t Start,Int_t End)
{
  if( mFitSum ){ return mSumAdcFit; }
  mSumAdcFit = 0.0;
  Double_t base = Baseline();//A bit redundant but necessary in case T0 had not been computed
  if( base < -4.0 ){ return mSumAdcFit; }
  if( mG_Data==0 || mG_Data->GetN()==0 ){return mSumAdcFit;}
  if( Start==0 && End==0 ){Start=PeakStart(); End=PeakEnd();}//Default case
  //Upon failure to find a window 'Start' and 'End' value will be equal.  In this case just set some default window.  For run 2019 it will be 35-60 and triggered crossing was at 50
  //For 2019 Ecal Cosmic running signal window was between 110-140
  //if( Start==End ){ SetWindow(110,140); Start=115; End=135; }//40 and 55 here since I add to range later
  //Sanity Checks
  if( !GoodWindow() ){ return mSumAdcFit; }
  if( mF1_SignalFit!=0 ){ delete mF1_SignalFit; mF1_SignalFit=0; }
  mF1_SignalFit = new TF1( (mName+"_MB_"+"mF1_SignalFit").c_str(),MaxwellBoltzmannDist,Start,End,4);
  //mG_Data->GetXaxis()->SetRangeUser(Start-5,End+5);
  Double_t height=1; Double_t scale=1;
  SignalMBPars(height,scale);
  mF1_SignalFit->SetParameter( 0, height );
  mF1_SignalFit->SetParameter( 1, mFoundPeak.mStartX );
  mF1_SignalFit->SetParameter( 2, scale );
  mF1_SignalFit->SetParameter( 3, base );
  mF1_SignalFit->SetParName(0,"Amplitude");//Amplitude
  mF1_SignalFit->SetParName(1,"X-Offset"); //x-offset (peak rise location)
  mF1_SignalFit->SetParName(2,"Scale");    //scale parameter
  mF1_SignalFit->SetParName(3,"Y-Offset"); //y-offset
  mF1_SignalFit->FixParameter(3,base);//Works best when y-offset not allowed to change
  
  char opt[10] = "BNRQ";
  if( GetDebug()>2 ){ opt[3] = '\0';}//Hack to to not quiet fit output
  Int_t fitstatus = this->GetData()->Fit( mF1_SignalFit, opt );
  if( fitstatus >= 0 )
    {
      mSumAdcFit = mF1_SignalFit->Integral(Start,End);//Sum only in the range and subtract the area under the baseline
      if( GetDebug()>2 ){std::cout << "|FitStart:"<<Start << "|FitEnd:"<<End << "|FitSumB:"<<mSumAdcFit; }
      mSumAdcFit -= ((End-Start)*base);
      if( GetDebug()>2 ){std::cout << "|FitSumA:"<<mSumAdcFit << "|BaseArea:"<<((End-Start)*base) << std::endl; }
      mFitSum = true;
      return mSumAdcFit;
    }
  else{ mFitSum = true; return mSumAdcFit; }//If fit failed return 0 don't do a histogram sum
}

Double_t StFcsPulseAna::PulseFit(Int_t Start, Int_t End)
{
  if( mFitSum ){ return mSumAdcFit; }
  mSumAdcFit = 0.0;
  if( mDbPulse==0 ){std::cout <<"ERROR:No pulse object"<<std::endl; return mSumAdcFit;}
  Double_t base = Baseline();//A bit redundant but necessary in case T0 had not been computed
  if( base < -4.0 ){ return mSumAdcFit; }
  if( mG_Data==0 || mG_Data->GetN()==0 ){return mSumAdcFit;}
  if( Start==0 && End==0 ){Start=PeakStart(); End=PeakEnd();}//Default case
  //Upon failure to find a window 'Start' and 'End' value will be equal.  In this case just set some default window.
  //Sanity Checks
  if( !GoodWindow() ){ return mSumAdcFit; }
  if( mF1_SignalFit!=0 ){ delete mF1_SignalFit; mF1_SignalFit=0; }
  //Only care about one found peak for now in future can change to include more peaks ??
  int npeak = 1;
  double para[5] = {static_cast<double>(npeak),base,PeakY(),PeakX(),mDbPulse->GSigma()};
  mF1_SignalFit = new TF1( (mName+"_Pulse_"+"mF1_SignalFit").c_str(), mDbPulse,&StFcsDbPulse::multiPulseShape,Start,End,2+npeak*3);
  mF1_SignalFit->SetParameters(para);
  mF1_SignalFit->FixParameter(0,npeak);
  mF1_SignalFit->FixParameter(1,base);
  for(int i=0; i<npeak; i++){
    mF1_SignalFit->SetParLimits(1+i*3+1,0.0,40000.0);       //limit peak not to go negative
    int j=1+i*3+2;
    mF1_SignalFit->SetParLimits(j,para[j]-2.0,para[j]+2.0); //limit peak position to +- 2TB
    mF1_SignalFit->SetParLimits(1+i*3+3,0.01,10.0);          //limit sigma to go too narrow or wide (Note(May 5, 2021):orig 0.5 to 10.0)
  }

  char opt[10] = "BNRQ";
  if( GetDebug()>2 ){ opt[3] = '\0';}//Hack to to not quiet fit output
  Int_t fitstatus = this->GetData()->Fit( mF1_SignalFit, opt );
  //Int_t fitstatus = 0;//For testing
  if( fitstatus >= 0 )
    {
      mSumAdcFit = mF1_SignalFit->Integral(Start,End);//Sum only in the range and subtract the area under the baseline
      if( GetDebug()>2 ){std::cout << "|FitStart:"<<Start << "|FitEnd:"<<End << "|FitSumB:"<<mSumAdcFit; }
      mSumAdcFit -= ((End-Start)*base);
      if( GetDebug()>2 ){std::cout << "|FitSumA:"<<mSumAdcFit << "|BaseArea:"<<((End-Start)*base) << std::endl; }
      mFitSum = true;
      return mSumAdcFit;
    }
  else{ mFitSum = true; return mSumAdcFit; }//If fit failed return 0 don't do a histogram sum
}

void StFcsPulseAna::SetFitPars(TF1* func)
{
  if( mDbPulse==0 ){
    if( func!=0 ){
      func->FixParameter(0,0);
      func->FixParameter(1,0);
    }
    return;
  }
  if( FoundPeakIndex()<0 ){ this->AnalyzeForPeak(); }
  int npeaks = NPeaks();
  if( func==0 ){
    func = mDbPulse->createPulse(mXRangeMin,mXRangeMax,2+npeaks*3);
  }
  if( npeaks>33 ){return;}//Since parameter array has 101 this is (101-2)/3 max peaks
  double para[101] = {0};
  para[0] = npeaks;
  para[1] = Baseline();
  func->SetParName(0,"NPeaks");
  func->SetParName(1,"Ped");
  for( Int_t i=0; i<npeaks; ++i ){
    int j = 1+i*3+1;
    char name[10];
    sprintf(name,"P%d_A",i);
    func->SetParName(j,name);
    para[j++] = GetPeak(i).mPeakY;
    sprintf(name,"P%d_M",i);
    func->SetParName(j,name);
    para[j++] = GetPeak(i).mPeakX;
    sprintf(name,"P%d_S",i);
    func->SetParName(j,name);
    para[j] = mDbPulse->GSigma();
  }
  func->SetParameters(para);
  func->FixParameter(0,npeaks);
  func->FixParameter(1,Baseline());
  for(int i=0; i<npeaks; i++){
    int j=1+i*3+1;
    func->SetParLimits(j++,0.0,50000.0);       //limit peak not to go negative
    func->SetParLimits(j,para[j]-2.0,para[j]+2.0); //limit peak position to +- 2TB
    func->SetParLimits(++j,0.5,10.0);              //limit sigma to go too narrow or wide
  }
}

StFcsPulseAna* StFcsPulseAna::DrawCopy(Option_t* opt, const char* name_postfix, TGraph* graph) const
{
  //TString newname;
  //if( name_postfix ){ newname.Form("%s%s",GetName(),name_postfix); }
  StFcsPulseAna* ana = new StFcsPulseAna(*this,name_postfix,graph);//Don't want to call "Clone" since that will clone the graph which you may not want to do in some cases. If graph==0 then it will be cloned
  ana->SetBit(kCanDelete);
  ana->AnalyzeForPeak();//Since vector of peaks is not copied re-run analysis; this is in case a filter was used which may replace the graph or a different graph is used
  ana->AppendPad(opt);//Only append to pad don't draw yet

  return ana;
}

void StFcsPulseAna::Print(Option_t* opt) const
{
  TString option(opt);
  option.ToLower();
  std::cout << "|Name:"<<mName;
  if( option.Contains("ana") ){ PeakAna::Print(); }
  if( option.Contains("debug") ){
    std::cout << "|P::Graph:"<<this->GetData();
    std::cout << "|P::SignalFit:"<<mF1_SignalFit;
    std::cout << "|P::BaselineHist:"<<mH1_Baseline;
    std::cout << "|P::BaselineFit:"<<mF1_BaselineFit;
    std::cout << "|P::DbPulse:"<<mDbPulse;
  }
  //std::cout << "|Flag::Internal:"<<mInternalSignal;
  std::cout << "|Flag::WindowSum:"<<mWindowSum;
  std::cout << "|Flag::FitSum:"<<mFitSum;
  std::cout << "|Base:"<<mBaseline;
  std::cout << "|BaselineSigma:"<<mBaselineSigma;
  std::cout << "|SearchWindow::"; mSearch.Print();
  std::cout << "|FoundWindow::"; mFoundPeak.Print();
  //std::cout << "|MaxY:"<<mMaxAdc;
  //std::cout << "|MaxX:"<<mMaxTb;
  std::cout << "|AdcSum:"<<mSumAdc;
  std::cout << "|FitSum:"<<mSumAdcFit;
  std::cout << std::endl;
}

