//Author:David Kapukchyan
//Oct 23, 2019
//Class implmentation for signal finding in FCS

#include "StFcsPulseFit.h"

ClassImp(StFcsPulseFit)

StFcsPulseFit::StFcsPulseFit()
{
  mName = "StFcsPulseFit";
  Initialize();
}

StFcsPulseFit::StFcsPulseFit(std::string name)
{
  mName = name;
  mGAE_Signal = 0;//This is so 'Initialize' will create the TGraphAsymmErrors object
  Initialize();
}

StFcsPulseFit::StFcsPulseFit(TGraphAsymmErrors* Sig, std::string name)
{
  mName = name;
  mGAE_Signal = Sig;//Set graph object now so doesn't get created in 'Initialize'
  Initialize();//This sets internal signal to true so set it false after call
  mInternalSignal=false;
}

/*
StFcsPulseFit::StFcsPulseFit(int ntb, double* tb, double* adc, std::string name)
{
  mName = name;
  mGAE_Signal = new TGraphAsymmErrors(ntb, tb, adc);
  Initialize();
}
*/

void StFcsPulseFit::Initialize()
{  
  mDEBUG = false;
  mWindowSum = false; mSumAdc=0;
  mFitSum = false; mSumAdcFit=0.0;
  mSearch.Start = 50; mSearch.End=5;//Chosen to match "WaveFormFitMaker" where 50 is the peak
  
  mT0Computed = false;//This will keep from having to determine T0 every time
  mInternalSignal = true;
  mSignalT0.Reset();
  mBaseline = -5.0;
  mBaselineSigma = 0.75;

  mMaxAdc = -5.0;
  mMaxTb = -5.0;

  if( mGAE_Signal==0 )
    {
      mGAE_Signal = new TGraphAsymmErrors();
      mGAE_Signal->SetNameTitle((mName+"_"+"G_Signal").c_str(),"Signal Pulse");
    }
  mH1_Baseline = 0;
  mF1_BaselineFit = 0;

  mF1_SignalFit = 0;

}

void StFcsPulseFit::SetSignal(TGraphAsymmErrors* SigGraph)
{
  if( SigGraph==0 ){std::cout << "Signal graph cannot be 0" << std::endl; exit(0);}
  ResetFinder();
  if( mInternalSignal ){delete mGAE_Signal;}
  mGAE_Signal = SigGraph;
  mInternalSignal = false;
}

StFcsPulseFit::~StFcsPulseFit()
{
  if(mGAE_Signal!=0 && mInternalSignal ){delete mGAE_Signal;}
  if(mH1_Baseline!=0)     { delete mH1_Baseline; }
  if(mF1_SignalFit!=0)    { delete mF1_SignalFit; }
  if(mF1_BaselineFit!=0)  { delete mF1_BaselineFit; }

}

void StFcsPulseFit::ResetFinder()
{
  if( mInternalSignal ){mGAE_Signal->Set(0);}//Effectively deletes all points in the graph only for internal graph objects
  //delete mGAE_Signal; mGAE_Signal = new TGraphAsymmErrors();
  ResetBaseline();
  ResetT0();
  ResetSum();
}

void StFcsPulseFit::ResetBaseline()
{
  mBaseline = -5.0;
  mBaselineSigma = 0.75;
  if( mH1_Baseline!=0 ){mH1_Baseline->Reset();}
  if( mF1_BaselineFit!=0 ){mF1_BaselineFit->ResetAttLine();}//Values from old fit will be replaced when 'AnalyzeForBaseline' gets called
}

void StFcsPulseFit::ResetT0()
{
  mT0Computed = false;
  mSignalT0.Reset();
}

void StFcsPulseFit::ResetSum()
{
  if( mF1_SignalFit!=0 ){ mF1_SignalFit->ResetAttLine(); }//This was for plotting but still good to do just in case
  
  mWindowSum=false; mSumAdc = 0;
  mFitSum=false; mSumAdcFit = 0.0;
}

void StFcsPulseFit::SetBaseline(Double_t value, Double_t sigma)
{
  if( value<=-0.0001 || sigma<=-0.0001){ std::cout << "WARNING:Baseline cannot be less than zero\nKeeping default behavior to search for baseline" << std::endl; return; }
  mBaseline = value;
  mBaselineSigma = sigma;

}

void StFcsPulseFit::AnalyzeForPedestal()
{
  ResetBaseline();//Resets histogram and function if they exist
  if( mH1_Baseline==0){mH1_Baseline = new TH1F( (mName+"_"+"H1_Baseline").c_str(),"",4096,-0.5,4095.5); mH1_Baseline->Sumw2(); mH1_Baseline->SetTitle("Baseline Histogram");}
  //Fill histogram of ADC values
  for( int ipoint=0; ipoint<mGAE_Signal->GetN(); ++ipoint)
    {
      Double_t tb; Double_t adc;
      Int_t check = mGAE_Signal->GetPoint(ipoint,tb,adc);
      if( check<0 ){ std::cout << "WARNING:Unable to find point " << ipoint << " in Signal graph" << std::endl; continue;}
      if( adc<0.1 ){continue;} //skip adc=0 from histogram since this is most likely missing data in non-pedestal subtracted data
      mH1_Baseline->Fill(adc);
    }
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
      if(mDEBUG){std::cout << "->";PrintInfo();}
      else{std::cout << "->Found baseline:"<< mBaseline << " Found BaselineSigma:"<<mBaselineSigma << std::endl;}
      std::cout << "->Setting baseline to 0 and sigma to 0.75 to prevent T0 algorithm from failing" << std::endl;
      mBaseline=0;
      mBaselineSigma=0.75;
      return;
    }
}

//In the future make range around XStartVal as arguments to the function
bool StFcsPulseFit::FindBaseline()
{
  //std::cout << "In FindBaseline:" << std::endl;
  if( mBaseline > -0.001 )
    {
      if( mDEBUG)
	{
	  std::cout << "|BaselineValue:"<<mBaseline;
	  std::cout << "|BaselineSigma:"<<mBaselineSigma;
	  std::cout << "|(Default)SigmaFactor:"<<4.0;
	  std::cout << "|ADC Threshold is SigmaFactor*BaselineSigma" << std::endl;
	}
      return true;
    }
  if( mF1_BaselineFit==0 ){ return false; }
  if( mH1_Baseline==0 || mH1_Baseline->GetEntries()==0 ){mBaseline = -5.0; return false;}
  //For nonpedestal data use full range for pedestal data find largest bin and use a +-5 bin buffer for the fit
  Int_t XStartVal = mH1_Baseline->GetMaximumBin()-1;//Offset by one to make the value correct
  if( mDEBUG ){ std::cout << "|Height:"<<mH1_Baseline->GetBinContent(XStartVal) << "|XStartVal:"<< XStartVal << "|Range:"<<5 <<"|StartSigma:"<<0.9 <<std::endl; }
  mF1_BaselineFit->SetRange(XStartVal-5,XStartVal+5);//Only do within this range
  mF1_BaselineFit->SetParameter(0,mH1_Baseline->GetBinContent(XStartVal));
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
  Int_t FitStatus = mH1_Baseline->Fit(mF1_BaselineFit,"QNR");
  if( FitStatus >= 0 )
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
  else{return false;}
}

//Vector will hold bin where possible T0 occurred
std::vector<StFcsPulseFit::SigWindow> StFcsPulseFit::GetPossibleT0(Float_t SigmaScale)
{
  std::vector<SigWindow> PossibleT0;//Gather all the possible occurances when signal is larger than Sigma
  //std::cout << "In GetPossible T0" << std::endl;
  if( !FindBaseline() ){ std::cout << "ERROR:No valid baseline\nPlease either run \"AnalyzeForPedestal\" or call \"SetBaseline\" with values greater than or equal to zero" << std::endl; return PossibleT0; }
  Double_t baseline = mBaseline;//Already checked baseline above
  Double_t SlopeCutoff = SigmaScale*fabs(mBaselineSigma);
  //Ctools::PrintDebug<Double_t>(std::cout, "baseline", baseline );
  //Ctools::PrintDebug<Double_t>(std::cout, "SlopeCutoff", SlopeCutoff );
  //std::cout << std::endl;
  SigWindow sig;
  Int_t LocalMax=-5;//Variable to help keep track of when slope changes
  //std::cout << "Finding T0 for cutoff " << SlopeCutoff << std::endl;
  //The idea is that you start with all things negative and loop through all the ADC values.  When the slope is greater than the cutoff you save it and then the signal will rise and then fall so slope will eventually go negative and this is when you keep track of the end values.  Once signal goes positive again or you no longer pass the slope cutoff stop and save the start and stop values
  if( mDEBUG ){std::cout << "StFcsPulseFit::GetPossibleT0:Start graph reading loop" << std::endl;}
  for( Int_t ismp=0; ismp<mGAE_Signal->GetN()-1; ++ismp )
    {
      Double_t Ltb; Double_t Ladc;//L for left (actually current point)
      Double_t Rtb; Double_t Radc;//R for right (actually next point)
      mGAE_Signal->GetPoint(ismp,Ltb,Ladc);
      mGAE_Signal->GetPoint(ismp+1,Rtb,Radc);
      //std::cout << "  - |P:"<<ismp <<"|LTb:"<<Ltb << "|LAdc:"<<Ladc<< "|RTb:"<<Rtb << "|RAdc:"<<Radc;

      //Reject impossible values
      if( Ladc < 0 || Ladc > 4095){std::cout << "Error:Invalid Data 'Ladc'"<<std::endl;}
      if( Radc < 0 || Radc > 4095){std::cout << "Error:Invalid Data 'Radc'"<<std::endl;}
      Double_t Slope = Radc-Ladc;
      //std::cout << "|Slope:"<<Slope << std::endl;
      //The purpose of this if statement is to ensure that any cases when there is no start time and no end time and the 'Ladc'==0 then we skip those points.  The additional nested if statement is for the case when there is a start and end time (like when the alogithm is working on negative slopes) and the 'Ladc' will still be >0; however since I dynamically change the "end" time until the slope changes to positive this statement ensures that 'continue' only gets called on positive slope results and not negative slopes when it is trying to find the correct end time.
      //if( (sig.Start>=0 && sig.End!=2000 && Ladc>0) ){continue;}

      //if( fabs(Slope) > SlopeCutoff*10.0 ){ continue; }//Avoid large sudden changes in slope (Hard code to 10 times the slope cutoff for now in future think of better method
      //std::cout << "Val|ismp:"<<ismp << "|Lbin:"<<LBinValue << "|Rbin:"<<RBinValue << " Sig|Start:"<<sig.Start << "|End:"<<sig.End << std::endl;
      //Check above will skip bad values
      if( sig.Start<0  )//No start time yet
	{
	  //std::cout << "G0|iSmp:"<<ismp << "|LBinValue:"<<Ltb << "|RBinValue:"<<Rtb << std::endl;
	  //std::cout << "    + No StartTime" << std::endl;
	  if( Slope > SlopeCutoff || Ladc>baseline+SlopeCutoff )
	    {
	      //std::cout << "    + Passed Slope and baselineCutoff setting as start time" << std::endl;
	      sig.Start=Ltb;//Set start time
	      LocalMax=Ltb;//Start checking local maximums
	    }
	  //If didn't pass thresholds for start time then continue to next point
	}
      else//Found a suitable start time which indicates positve slope above some thershold
	{
	  //The statements below will now check subsequent values and if the slope is >= 0 then we have wrong local max so set local max to next value.  Note this means that localmax is not greater than End which is 2000.  Once slope goes below 0 End will take that value and local max will be less than End.  Once the slope changes again LocalMax will now be greater than End
	  //std::cout << "    + Found StartTime:"<< sig.Start << std::endl;
	  if( Slope >= 0 ){ LocalMax=Ltb;/* std::cout<<"      + Slope>=0"<<std::endl;*/}
	  else{ sig.End=Ltb; if(sig.P_Peak<0){sig.P_Peak=ismp-1;}/*std::cout<<"      + Slope<0"<<std::endl;*/}//Slope < 0
	  //In the future I can also add a condition that end-start is larger than some value but it works good enough for now
	  if( LocalMax>sig.End || Ladc<baseline+SlopeCutoff )//Slope is now positive again
	    {
	      //std::cout << "    + Slope positive again;End Time" << std::endl;
	      PossibleT0.push_back(sig);//Store windows
	      //std::cout << "  * |Found:"; PossibleT0.back().Print(); std::cout<<std::endl;
	      sig.Reset();//Reset variables
	      LocalMax=-5;
	      --ismp;//Go back one point in case new signal starts at where slope was positive again
	    }//Slope changed so get 
	  //From above we know slope was postive when a valid start time was found
	  //if( Slope > 0 ){ sig.End=ismp; }
	  //if( LBinValue>baseline+SlopeCutoff ){sig.End=ismp;}
	  //else{PossibleT0.push_back(sig); sig.Reset();}
	}

    }
  //PossibleT0.push_back(sig);
  if( mDEBUG ){ std::cout << "StFcsPulseFit::GetPossibleT0:End graph reading loop|SizeT0:" <<PossibleT0.size() << std::endl; }
  return PossibleT0;
}

//Returns index that corresponds to the max slope
UShort_t StFcsPulseFit::SearchForT0(const std::vector<SigWindow> &PossibleT0s)
{
  if( PossibleT0s.size()==0 )
    {
      if( mDEBUG ){std::cout << "Error:Unable to find a valid T0\nReturning 2000" << std::endl; }
      return 2000;
    }
  /*
  else if( PossibleT0s.size()==1 )
    {
      if( mDEBUG ){std::cout << "Only found one T0" << std::endl;}
      return 0;
    }
  */
  else
    {
      //Add this check even if only one entry to get rid of a false positive somewhere outside the range?
      //If multiple T0s are found only pick the one with a specific start time
      //Int_t StartTime=120;//Also good for pedestal which shouldn't find a T0
      //The values chosen are to match the trigger for 2019 for which the physics trigger fired around timebin ~40 and the pulser fires at timebin ~210
      /*Change this to use set function
      if( mPed==0 ){ StartTime = 40; }//physics run
      if( mPed==1 ){return 2000;}     //For obvious reasons don't do pedestal runs
      if( mPed==2 ){ StartTime = 210;}//pulser run
      */
      if( mDEBUG )
	{
	  std::cout << "|Checking Name:"<<mName;
	  std::cout << "|Size of Possible T0s:"<<PossibleT0s.size();
	  std::cout << "|Search Peak:"<<mSearch.Start;
	  std::cout << "|Search Width:"<<mSearch.End;
	  std::cout << std::endl;
	}
      Short_t T0index=-1;
      for( UShort_t it0=0; it0<PossibleT0s.size(); ++it0 )
	{
	  if( mDEBUG )
	    {
	      std::cout << " - ";
	      std::cout << "|Index:"<<it0;
	      PossibleT0s.at(it0).Print();
	      std::cout << std::endl;
	    }
	  Double_t PeakLoc,temp; mGAE_Signal->GetPoint(PossibleT0s.at(it0).P_Peak,PeakLoc,temp);
	  if( mSearch.Start-mSearch.End<=PeakLoc && PeakLoc <= mSearch.Start+mSearch.End )
	    {
	      T0index=it0;
	      if( mDEBUG )
		{
		  std::cout << "   + ";
		  std::cout << "|TrueIndex:"<<T0index;
		  std::cout << std::endl;
		}
	    }
	}
      //if( mSearch.Start>0 && PossibleT0s.at(T0index).Start>0 )
      if( T0index>=0 && mSearch.Start>0 && PossibleT0s.at(T0index).P_Peak>0 )
	{
	  if(mDEBUG){PossibleT0s.at(T0index).Print();std::cout<<std::endl;}
	  return T0index;
	}
      else{ return 2000;}
    }
}

void StFcsPulseFit::AnalyzeForT0( Float_t Sigma )
{
  ResetT0();
  std::vector<SigWindow> MyT0s = GetPossibleT0(Sigma);//This is the sigma to compute the threshold
  UShort_t FoundT0 = SearchForT0(MyT0s);//Returns either a valid index for the possible T0 vector or 2000
  //1024 samples max so that should be the upper bound on the returned index
  if( FoundT0 < 1024 ){mSignalT0 = MyT0s.at(FoundT0);}//No offset for now rethink in the future
  else{mSignalT0.SetWindow(FoundT0,FoundT0);}//If no valid index then set SignalT0 to 2000 so it is obvious something's wrong
  mT0Computed=true;
  return;
}

bool StFcsPulseFit::GoodWindow()
{
  if( !mT0Computed ){AnalyzeForT0();}
  //First check if start and end times are within our max timebin window of 0-1023
  if( mSignalT0.Start<0 || mSignalT0.Start>1023 ){/*std::cout<<"StartOut"<<std::endl;*/return false;}
  else if( mSignalT0.End<0 || mSignalT0.End > 1023 ){/*std::cout<<"EndOut"<<std::endl;*/return false;}
  //Next check if values make physical sense
  else if( mSignalT0.Start==mSignalT0.End ){/*std::cout<<"Equal"<<std::endl;*/return false;}
  else if( mSignalT0.Start > mSignalT0.End ){/*std::cout<<"Start>End"<<std::endl;*/return false;}
  else{return true;}
}

Int_t StFcsPulseFit::Sum(Int_t Start, Int_t End)
{
  Int_t SumAdc = 0;
  Double_t base = Baseline();//A bit redundant but neccassray in case T0 had not been computed
  //std::cout << "|SumAdc:" <<SumAdc << "|Baseline:"<<base << std::endl;
  if( base < -4.0 ){return SumAdc;}
  if( mGAE_Signal==0 || mGAE_Signal->GetN()==0 ){return SumAdc;}
  //Ctools::PrintDebug<Int_t>(std::cout,"SumAdc",SumAdc);
  for( Int_t ismp=0; ismp<mGAE_Signal->GetN(); ++ismp )
    {
      Double_t tb; Double_t adc;
      mGAE_Signal->GetPoint(ismp,tb,adc);
      if( tb < Start ){continue;}
      if( tb > End ){ break; }
      //Ctools::PrintDebug<Int_t>(std::cout,"SumAdc",SumAdc);
      //Ctools::PrintDebug<Int_t>(std::cout,"tb",tb);
      //Ctools::PrintDebug<Int_t>(std::cout,"adc",adc);
      //std::cout << std::endl;
      SumAdc += adc;
    }
  if( mDEBUG ){std::cout << "|Start:"<<Start << "|End:"<<End << "|SumAdcB:"<< SumAdc; }
  SumAdc -= base*abs(End-Start+1);//Add one to account for the fact that the adc value at start and end is being summed
  if( mDEBUG ){std::cout << "|SumAdcA:"<< SumAdc << "|BaseArea:"<<base*abs(End-Start+1) << std::endl; }
  //std::cout << "FinalSum:"<<SumAdc << std::endl;
  return SumAdc;
}

Int_t StFcsPulseFit::SumWindow()
{
  if( mWindowSum ){ return mSumAdc; }
  mSumAdc = 0;
  //Upon failure to find a window Start and End value will be equal.  In this case just set some default window.  For run 2019 it will be 35-60 and triggered crossing was at 50
  //For 2019 Ecal Cosmic running signal window was between 110-140
  //if( SignalStart()==SignalEnd() ){ SetWindow(110,140); }
  if( !GoodWindow() ){ return mSumAdc; }
  mSumAdc = Sum( SignalStart(), SignalEnd() );
  mWindowSum = true;
  return mSumAdc;
}

TF1* StFcsPulseFit::SignalFit(const char option)
{
  switch(option)
    {
    case 's': if( !mFitSum ){ SumAdcFit(); } break;
    default: break;
    }
  return mF1_SignalFit;
}

Double_t StFcsPulseFit::SumAdcFit(Int_t Start,Int_t End)
{
  if( mFitSum ){ return mSumAdcFit; }
  mSumAdcFit = 0.0;
  Double_t base = Baseline();//A bit redundant but necessary in case T0 had not been computed
  if( base < -4.0 ){ return mSumAdcFit; }
  //std::cout << "SumAdcFit|Det:" << Det() << "|Id:" << Id() << "|Baseline:"<<base << std::endl;
  if( mGAE_Signal==0 || mGAE_Signal->GetN()==0 ){return mSumAdcFit;}
  if( Start==0 && End==0 ){Start=SignalStart(); End=SignalEnd();}//Default case
  //Upon failure to find a window 'Start' and 'End' value will be equal.  In this case just set some default window.  For run 2019 it will be 35-60 and triggered crossing was at 50
  //For 2019 Ecal Cosmic running signal window was between 110-140
  //if( Start==End ){ SetWindow(110,140); Start=115; End=135; }//40 and 55 here since I add to range later
  //Sanity Checks
  if( !GoodWindow() ){ return mSumAdcFit; }
  //Always make range a little bigger
  if( mF1_SignalFit==0 ){ mF1_SignalFit = new TF1( (mName+"_"+"mF1_SignalFit").c_str(),"gaus(0)+[3]",Start,End); }
  else{ mF1_SignalFit->SetRange(Start,End); }
  //mGAE_Signal->GetXaxis()->SetRangeUser(Start-5,End+5);
  mF1_SignalFit->SetParameter( 0, MaxAdc() );
  mF1_SignalFit->SetParameter( 1, MaxTb() );
  mF1_SignalFit->SetParameter( 2, static_cast<Double_t>(End-Start)*0.2 );//Seemed to work most of the time with 0.3, 0.5 seemed too much
  mF1_SignalFit->SetParameter( 3, base );
  Int_t fitstatus = mGAE_Signal->Fit( mF1_SignalFit, "QNR" );
  if( fitstatus >= 0 )
    {
      //Fix to 4 sigma for now in the future make it a variable?
      Double_t FitStart = mF1_SignalFit->GetParameter(1) - 4.0*fabs(mF1_SignalFit->GetParameter(2));
      Double_t FitEnd = mF1_SignalFit->GetParameter(1) + 4.0*fabs(mF1_SignalFit->GetParameter(2));
      mSumAdcFit = mF1_SignalFit->Integral(FitStart,FitEnd);//Sum only in the range and subtract the area under the baseline
      if( mDEBUG ){std::cout << "|FitStart:"<<FitStart << "|FitEnd:"<<FitEnd << "|FitSumB:"<<mSumAdcFit; }
      mSumAdcFit -= ((FitEnd-FitStart)*base);
      if( mDEBUG ){std::cout << "|FitSumA:"<<mSumAdcFit << "|BaseArea:"<<((FitEnd-FitStart)*base) << std::endl; }
      mFitSum = true;
      return mSumAdcFit;
    }
  else{ mFitSum = true; return mSumAdcFit; }//If fit failed return 0 don't do a histogram sum
}

void StFcsPulseFit::GetXYMax(Double_t xmin, Double_t xmax)
{
  if( mGAE_Signal==0 ){ return; }
  for( int i=0; i<mGAE_Signal->GetN(); ++i )
    {
      Double_t X; Double_t Y;
      mGAE_Signal->GetPoint(i,X,Y);
      if( X<xmin || X>xmax ){continue;}
      if( Y>mMaxAdc ){ mMaxAdc=Y; mMaxTb=X; }
    }
  if( mMaxAdc<0 && mMaxTb<0 )
    {
      std::cout << "Unable to find a maximum adc\nSetting to impossible values" << std::endl;
      mMaxAdc = 5000;
      mMaxTb = 5000;
    }
}

Double_t StFcsPulseFit::SignalPeakTb()
{
  if( !mT0Computed ){ AnalyzeForT0(); }
  if( mSignalT0.P_Peak<0){ return 0; }
  else{ Double_t x,y; mGAE_Signal->GetPoint(mSignalT0.P_Peak,x,y); return x;}
}

Double_t StFcsPulseFit::SignalPeakAdc()
{
  if( !mT0Computed ){AnalyzeForT0();}
  if( mSignalT0.P_Peak<0){ return 0; }
  else{ Double_t x,y; mGAE_Signal->GetPoint(mSignalT0.P_Peak,x,y); return y;}
}

void StFcsPulseFit::SignalPeak(Double_t &tb, Double_t &adc)
{
  if( !mT0Computed ){AnalyzeForT0();}
  if( mSignalT0.P_Peak<0 ){ tb=0; adc=0; }
  else{ mGAE_Signal->GetPoint(mSignalT0.P_Peak,tb,adc); }
}

void StFcsPulseFit::PrintInfo() const
{
  std::cout << "|Name:"<<mName;
  if( mDEBUG )
    {
      std::cout << "|P::Graph:"<<mGAE_Signal;
      std::cout << "|P::SignalFit:"<<mF1_SignalFit;
      std::cout << "|P::BaselineHist:"<<mH1_Baseline;
      std::cout << "|P::BaselineFit:"<<mF1_BaselineFit;
    }
  std::cout << "|Flag::Internal:"<<mInternalSignal;
  std::cout << "|Flag::WindowSum:"<<mWindowSum;
  std::cout << "|Flag::FitSum:"<<mFitSum;
  std::cout << "|Base:"<<mBaseline;
  std::cout << "|BaselineSigma:"<<mBaselineSigma;
  std::cout << "|SearchWindow::"; mSearch.Print();
  std::cout << "|FoundWindow::"; mSignalT0.Print();
  std::cout << "|MaxY:"<<mMaxAdc;
  std::cout << "|MaxX:"<<mMaxTb;
  std::cout << "|AdcSum:"<<mSumAdc;
  std::cout << "|FitSum:"<<mSumAdcFit;
  std::cout << std::endl;
}

