#include "StHbtMaker/CorrFctn/QinvEbyECorrFctn.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(QinvEbyECorrFctn)
#endif

    //____________________________
    QinvEbyECorrFctn::QinvEbyECorrFctn(char* title, const int& nbins, const float& QinvLo, const float& QinvHi){
    // set up numerator
    //  title = "Num Qinv (MeV/c)";
    char TitNum[100] = "Num";
    strcat(TitNum,title);
    mNumerator = new StHbt1DHisto(TitNum,title,nbins,QinvLo,QinvHi);
    // set up denominator
    //title = "Den Qinv (MeV/c)";
    char TitDen[100] = "Den";
    strcat(TitDen,title);
    mDenominator = new StHbt1DHisto(TitDen,title,nbins,QinvLo,QinvHi);
    // set up ratio
    //title = "Ratio Qinv (MeV/c)";
    char TitRat[100] = "Rat";
    strcat(TitRat,title);
    mRatio = new StHbt1DHisto(TitRat,title,nbins,QinvLo,QinvHi);
    // this next bit is unfortunately needed so that we can have many histos of same "title"
    // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)
    mNumerator->SetDirectory(0);
    mDenominator->SetDirectory(0);
    mRatio->SetDirectory(0);

    // radii histo
    mThreeFitLambda = new  StHbt1DHisto("ThreeFitLambda","ThreeFitLambda",50,0,2);
    mThreeFitRadius = new  StHbt1DHisto("ThreeFitRadius","ThreeFitRadius",100,0,50);
    mTwoFitLambda   = new  StHbt1DHisto("TwoFitLambda","TwoFitLambda",50,0,2);
    mTwoFitRadius   = new  StHbt1DHisto("TwoFitRadius","TwoFitRadius",100,0,50);
    mRmsByHandMeV   = new  StHbt1DHisto("RmsByHandMeV","RmsByHandMeV",100,0.0,0.2);
    mRmsByHandFm    = new  StHbt1DHisto("RmsByHandFm","RmsByHandFm",100,0,20);
    mThreeFitLambda->SetDirectory(0);
    mThreeFitRadius->SetDirectory(0);
    mTwoFitLambda->SetDirectory(0);
    mTwoFitRadius->SetDirectory(0);
    mRmsByHandMeV->SetDirectory(0);
    mRmsByHandFm->SetDirectory(0);

    // Define fit functions
    // 3 param fit to correlation function
    m_corrfunc_3param  = new StHbtTF1("corrfunc_3param","[0]+[1]*exp(- (x**2) * ([2]**2) / (0.197327**2))",0.0,0.2) ; 
    m_corrfunc_3param->SetParNames("Normfactor","Lambda","Rinv") ;
    // 2 param fit to normalized correlation function
    m_corrfunc_2param = new StHbtTF1("corrfunc_2param"," 1 + [0]*exp(- (x**2) * ([1]**2) / (0.197327**2))",0.0,0.2) ;
    m_corrfunc_2param->SetParNames("Lambda","Rinv") ;
    // straight line fit to the flat part of the correlation function
    m_line = new StHbtTF1("line","[0]+[1] * x",0.1,0.2) ;

    // Set (1) or Unset (0) debug option 
    m_Debug_ebye = 1 ;

    // to enable error bar calculation...
    mNumerator->Sumw2();
    mDenominator->Sumw2();
    mRatio->Sumw2();
    
    mTagWriter = StHbtTagWriter::Instance(); 
}

//____________________________
QinvEbyECorrFctn::~QinvEbyECorrFctn(){
    delete mThreeFitLambda;
    delete mThreeFitRadius;
    delete mTwoFitLambda;
    delete mTwoFitRadius;
    delete mRmsByHandMeV;
    delete mRmsByHandFm;
    delete mNumerator;
    delete mDenominator;
    delete mRatio;
    delete m_corrfunc_2param ;
    delete m_corrfunc_3param ;
    delete m_line ;
}
//_________________________
void QinvEbyECorrFctn::Finish(){
}

//____________________________
StHbtString QinvEbyECorrFctn::Report(){
    string stemp = "Qinv Correlation Function Report:\n";
    char ctemp[100];
    sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumerator->GetEntries());
    stemp += ctemp;
    sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenominator->GetEntries());
    stemp += ctemp;
    sprintf(ctemp,"Number of entries in ratio:\t%E\n",mRatio->GetEntries());
    stemp += ctemp;
    //  stemp += mCoulombWeight->Report();
    StHbtString returnThis = stemp;
    return returnThis;
}
//____________________________
void QinvEbyECorrFctn::AddRealPair(const StHbtPair* pair){
    mNumerator->Fill( fabs(pair->qInv()) );
}
//____________________________
void QinvEbyECorrFctn::AddMixedPair(const StHbtPair* pair){
    mDenominator->Fill( fabs(pair->qInv()) );
}
//_____________________________
void QinvEbyECorrFctn::EventBegin(const StHbtEvent* myev)
{              
    //reset histos
    mNumerator->Reset();
    mDenominator->Reset();
    mRatio->Reset();	
}
//_____________________________
void QinvEbyECorrFctn::EventEnd(const StHbtEvent* myev)
{
  // without ebye background we can't do anything
  // except we have ebye background ...
  if (mDenominator->GetEntries() == 0) return ;
  if (m_Debug_ebye)
    {
      cout << "Start ebye." << endl ;
    }

  mRatio->Divide(mNumerator,mDenominator,1.0,1.0) ;   // get back the not normalized correlation function
  
  if (mRatio->GetMean()!=0.) mRatio->Scale(1./mRatio->GetMean()) ;  // normalize histo;

  // calculate rms by hand : only after normalization
  double Width_MeV = 0 ;
  double Width_fm = 0 ;
  Rms_by_hand(Width_MeV, Width_fm) ;
  mRmsByHandMeV->Fill(Width_MeV) ;
  mRmsByHandFm->Fill(Width_fm) ;

  // calculate radius with 2 parameter fit : only after normalization
  twoFit fit2 = { 1.,0., 10.,0., 0., 0. };
  Two_param_fit( fit2 );
  mTwoFitLambda->Fill( fit2.lambda);
  mTwoFitRadius->Fill( fit2.radius);

  // calculate radius with 3 parameter fit    
  mRatio->Reset();
  mRatio->Divide(mNumerator,mDenominator,1.0,1.0) ;   // get back the not normalized correlation function
  threeFit fit3 = { mRatio->GetMean(),0., 1.,0., 10.,0., 0., 0. };
  Three_param_fit( fit3 );
  mThreeFitLambda->Fill( fit3.lambda );
  mThreeFitRadius->Fill( fit3.radius );


  // Normalize ratio
  if (m_Debug_ebye) {
      cout << " twoFit: ";
      cout << " lambda=" << fit2.lambda << " radius=" << fit2.radius << endl; 
      cout << " threeFit: ";
      cout << " constant="<< fit3.lambda << " lambda " << fit3.lambda << " radius=" << fit3.radius << endl; 
      cout << " RmsByHand ";
      cout << " Width MeV " << Width_MeV << " Width FM " << Width_fm << endl; 
      double norm3=0;
      if (fit3.constant!=0) norm3 = 1/(fit3.constant) ;
      //norm_comp->Fill(normdiff);
      cout << " Norm by integ    : " <<    Norm_by_integral() << endl;
      cout << " Norm by line fit : " << Norm_by_fit() << endl;
      cout << " Norm by 3d fit   : " << norm3 << endl;
      cout << " Norm by Mean     : " << mRatio->GetMean() << endl;
      cout << " Scale with       : " << mRatio->GetMean() << endl; 
    }


  // fill tagdb
  cout << " QinvEbyECorrFctn::EventEnd() - fill tags now " << endl;
  // domimik's 3 parameter fit
  mTagWriter->SetTag(mTagMeans,0, fit3.constant);
  mTagWriter->SetTag(mTagMeans,1, fit3.lambda); 
  mTagWriter->SetTag(mTagMeans,2, fit3.radius);  
  mTagWriter->SetTag(mTagMeans,3, fit3.chi2/fit3.ndf ); 
  mTagWriter->SetTag(mTagSigmas,0, fit3.constantErr ); 
  mTagWriter->SetTag(mTagSigmas,1, fit3.lambdaErr ); 
  mTagWriter->SetTag(mTagSigmas,2, fit3.radiusErr );  
  mTagWriter->SetTag(mTagSigmas,3, fit3.chi2 ); 
  mTagWriter->SetTag(mTagSigmas,4, fit3.ndf );  


} ;
//_______________________________________
void QinvEbyECorrFctn::Three_param_fit( threeFit& fit ) {  
  // Fit the unnormalized correlation function with A + B exp(-qinv^2 * C^2) 
  m_corrfunc_3param->SetParameters(fit.constant,fit.lambda,fit.radius) ; // Set start values for fitting
  mRatio->Fit("corrfunc_3param","Q0") ;          // Fit
  fit.constant    =  mRatio->GetFunction("corrfunc_3param")->GetParameter(0);  
  fit.lambda      =  mRatio->GetFunction("corrfunc_3param")->GetParameter(1);
  fit.radius      =  mRatio->GetFunction("corrfunc_3param")->GetParameter(2);
  fit.constantErr =  mRatio->GetFunction("corrfunc_3param")->GetParError(0);
  fit.lambdaErr   =  mRatio->GetFunction("corrfunc_3param")->GetParError(1);
  fit.radiusErr   =  mRatio->GetFunction("corrfunc_3param")->GetParError(2);
  fit.chi2        =  mRatio->GetFunction("corrfunc_3param")->GetChisquare();
  fit.ndf         =  mRatio->GetFunction("corrfunc_3param")->GetNDF() ;
};

//_______________________________________
void QinvEbyECorrFctn::Two_param_fit(twoFit& fit) {
  // Fit the normalized correlation function with  1 + A exp(-qinv^2 * B^2)
  m_corrfunc_2param->SetParameters(fit.lambda, fit.radius) ; // Set start values for fitting
  mRatio->Fit("corrfunc_2param","0") ;    // Fit
  fit.lambda      =  mRatio->GetFunction("corrfunc_2param")->GetParameter(0);
  fit.radius      =  mRatio->GetFunction("corrfunc_2param")->GetParameter(1);
  fit.lambdaErr   =  mRatio->GetFunction("corrfunc_2param")->GetParError(0);
  fit.radiusErr   =  mRatio->GetFunction("corrfunc_2param")->GetParError(1);
  fit.chi2        =  mRatio->GetFunction("corrfunc_2param")->GetChisquare();
  fit.ndf         =  mRatio->GetFunction("corrfunc_2param")->GetNDF() ;
} ;
//_______________________________________
void QinvEbyECorrFctn::Rms_by_hand(double& Width_MeV, double& Width_fm) {
  // "calculate width of the bump" : no fitting of Minuit, ROOT or whatever needed
  double sum =0 ;
  double S_y =0 ;

  // loop over bins in histo with correlation function
  for(int hist_index=1; hist_index <= (int) mRatio->GetNbinsX() ; hist_index++)
    {
      double x = (double) (mRatio->GetBinCenter(hist_index)) ;
      double y = (double) (mRatio->GetBinContent(hist_index)) ;   
      sum = sum + x * x * ( y -1 ) ;
      S_y = S_y + ( y -1 ) ; 
    }
  //cout << "sum "<< sum << endl;
  //cout << "S_y " << S_y << endl;
    
  Width_MeV = sqrt(fabs(sum/(S_y))) ;
  Width_fm  = 0.197327/Width_MeV ;

  cout << "Rough rms calculation : " << (double) Width_MeV << "\t";
  cout << "Rough rms calculation : " << (double) Width_fm  << endl ;
}
//_______________________________________
void QinvEbyECorrFctn::Fill_ratio_artificial()
{
    // Fill ratio histo with an function
    // r = 1 + exp( x^2 * R^2 )
    mRatio->Reset() ;
    double radius = 10 ;
    for(int i = 1 ; i < mRatio->GetNbinsX() ; i++)
    	{
	       double x = (double) (mRatio->GetBinCenter(i));
	       double y = 1 + exp(-(x*x)*(radius*radius)/(0.2*0.2));
	       mRatio->Fill(x,y) ;
	       //cout << x << "\t";
	       //cout << y << endl ;
    	};
};
//_______________________________________
void QinvEbyECorrFctn::Fill_ratio_artificial_random()
{
  // Fill ratio histo with an distribution 
  // r = 1 + lam * exp( x^2 * R^2 ) 
  mRatio->Reset();
  double lam = 1 + rand()/RAND_MAX;     // lam = [1..2]
  double R = 1 + 10 * rand()/RAND_MAX;  // R = [1..11]
  for(int i = 1 ; i < mRatio->GetNbinsX() ; i++)
    {
      double x = (double) (mRatio->GetBinCenter(i));
      double y = 1 + lam * exp(-(x*x)*(R*R)/(0.2*0.2));
      mRatio->Fill(x,y) ;
      //cout << x << "\t";
      //cout << y << endl ;
    } ;
} ;
//_______________________________________
double QinvEbyECorrFctn::Norm_by_integral()
{
  // Divide integral (0.0 - 0.2) over background by integral over signal
  double norm = 0;
  if ( mNumerator->Integral() != 0 )
    {
      norm = mDenominator->Integral() / mNumerator->Integral() ;
      // cout << "Normalized via integral by " << norm << endl ;
    }
  else
    {
      cout << "Cannot normlize, because denominator equals 0." << endl ;
    }
  return norm ;
} ;
//_______________________________________
double QinvEbyECorrFctn::Norm_by_fit()
{
  // Fit the flat part of the correlation function by a straight line 
  //StHbtTF1* line = new StHbtTF1("line","[0]+[1] * x",0.1,0.2) ;
  m_line->SetParameters(0.3,0.0) ;
  mRatio->Fit("line","Q0","",0.1,0.2) ;
  if ( m_Debug_ebye )
    {
      cout << "Normalized via fit. " << endl ;
      cout << "Fit result norm " << mRatio->GetFunction("line")->GetParameter(0) << "  " << mRatio->GetFunction("line")->GetParameter(1) << endl ;
    }
  double norm = 0 ;
  if ( mRatio->GetFunction("line")->GetParameter(0) != 0 )
    {
      norm = 1/(mRatio->GetFunction("line")->GetParameter(0)) ;
    }
  //delete line ;
  return norm ;
} ;
