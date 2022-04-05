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
    // by default point to 0; only if its filled we will use it
    mCorrection = 0; 

    // Integrated over all events
    // set up numerator
    char TitIntNum[100] = "IntNum";
    strcat(TitIntNum,title);
    mIntNumerator = new StHbt1DHisto(TitIntNum,title,nbins,QinvLo,QinvHi);
    // set up denominator
    char TitIntDen[100] = "IntDen";
    strcat(TitIntDen,title);
    mIntDenominator = new StHbt1DHisto(TitIntDen,title,nbins,QinvLo,QinvHi);
    // set up ratio
    char TitIntRat[100] = "IntRat";
    strcat(TitIntRat,title);
    mIntRatio = new StHbt1DHisto(TitIntRat,title,nbins,QinvLo,QinvHi);


    // radii histo
    mThreeFitLambda = new  StHbt1DHisto("ThreeFitLambda","ThreeFitLambda",100,0,2);
    mThreeFitRadius = new  StHbt1DHisto("ThreeFitRadius","ThreeFitRadius",100,0,50);
    mTwoFitLambda   = new  StHbt1DHisto("TwoFitLambda","TwoFitLambda",100,0,2);
    mTwoFitRadius   = new  StHbt1DHisto("TwoFitRadius","TwoFitRadius",100,0,50);
    mRmsByHandMeV   = new  StHbt1DHisto("RmsByHandMeV","RmsByHandMeV",100,0.0,0.2);
    mRmsByHandFm    = new  StHbt1DHisto("RmsByHandFm","RmsByHandFm",100,0,20);
    mThreeFitLambda->SetDirectory(0);
    mThreeFitRadius->SetDirectory(0);
    mTwoFitLambda->SetDirectory(0);
    mTwoFitRadius->SetDirectory(0);
    mRmsByHandMeV->SetDirectory(0);
    mRmsByHandFm->SetDirectory(0);

    // Set (1) or Unset (0) debug option 
    m_Debug_ebye = 1 ;

    // to enable error bar calculation...
    mNumerator->Sumw2();
    mDenominator->Sumw2();
    mRatio->Sumw2();
    
    //mTagWriter = StHbtTagWriter::Instance(); 
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
    delete mIntNumerator;
    delete mIntDenominator;
    delete mIntRatio;
    if (mCorrection) delete mCorrection;
}
//_________________________
void QinvEbyECorrFctn::Finish(){
  // now produce the inclusive correlation function
  mIntRatio->Divide(mIntNumerator,mIntDenominator,1.0,1.0) ;

  // fit it
  threeFit fit3 = { 0,0,0,0,0,0,0,0 };
  Three_param_fit( fit3 , mRatio);
  cout << " Inclusive fit : " << fit3.radius << endl ;
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
void QinvEbyECorrFctn::SetCorrection(const StHbt1DHisto* correc){
    mCorrection = (StHbt1DHisto*)correc;
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
    
    // Create Correlation function !
    mRatio->Divide(mNumerator,mDenominator,1.0,1.0) ;   
    
    //Fill_ratio_artificial() ;

    // Normalize by integral
    cout << "Scale ratio by (integral method) : " << Norm_by_integral(0.1,0.2) << endl ;
    mRatio->Scale(Norm_by_integral(0.1,0.2)) ;
     
    // Perform Coulomb Correction if one is defined
    if (mCorrection){
	cout << "Going to Coulomb correct now..." << endl;
	mRatio->Divide(mCorrection);
    }
    else      {
	cout << "NOT Coulomb correcting..." << endl;
    }

    // Fill inclusive histos
    mIntNumerator->Add(mNumerator);
    mIntDenominator->Add(mDenominator);
    

    // calculate rms by hand : only after normalization
    double Width_MeV = 0 ;
    double Width_fm = 0 ;
    Rms_by_hand(Width_MeV, Width_fm) ;
    mRmsByHandMeV->Fill(Width_MeV) ;
    mRmsByHandFm->Fill(Width_fm) ;

    // calculate radius with 2 parameter fit : only after normalization
    twoFit fit2 = { 0.,0.,0.,0., 0., 0. };
    Two_param_fit( fit2 );
    mTwoFitLambda->Fill( fit2.lambda);
    mTwoFitRadius->Fill( fit2.radius);

    // calculate radius with 3 parameter fit    
    mRatio->Reset();
    mRatio->Divide(mNumerator,mDenominator,1.0,1.0) ;   // get back the not normalized correlation function
    // Perform Coulomb Correction if one is defined
    if (mCorrection){
      cout << "Going to Coulomb correct now for the 3 Fit ..." << endl;
      mRatio->Divide(mCorrection);
    }
    else      {
      cout << "NOT Coulomb correcting for the 3 Fit ..." << endl;
    }
    threeFit fit3 = { 0.2,0.0,0.5,0.0,10.,0., 0., 0. };
    Three_param_fit( fit3 , mRatio);
    mThreeFitLambda->Fill( (fit3.lambda*1/(fit3.constant+0.000001)) );
    mThreeFitRadius->Fill( fit3.radius );

    // just for debugging
    if (m_Debug_ebye) 
	{
	    // do coulomb again to see it after everything was run
	    if (mCorrection){
		cout << "Going to Coulomb correct a second time just to see it ..." << endl;
		mRatio->Divide(mCorrection);
	    }
	    else      {
		cout << "NOT Coulomb correcting..." << endl;
	    }
	    // some output	  
	    cout << endl << endl ;
	    cout << " twoFit:   ";
	    cout << " lambda=" << fit2.lambda << "  radius=" << fit2.radius << endl; 
	    cout << " threeFit: ";
	    cout << " lambda=" << (fit3.lambda/(fit3.constant+0.000001)) << "  radius=" << fit3.radius  ;
	    cout <<"  constant :" << fit3.constant << endl; 
	    cout << " RmsByHand ";
	    cout << " Width MeV " << Width_MeV << " Width FM " << Width_fm << endl; 
	    double norm3=0;
	    if (fit3.constant!=0) norm3 = 1/(fit3.constant) ;
	    
	    cout << " Norm by integ    : " << Norm_by_integral(0.1,0.2) << endl;
	    cout << " Norm by line fit : " << Norm_by_fit(0.1,0.2) << endl;
	    cout << " Norm by 3d fit   : " << norm3 << endl;
	    
	    cout << " Scaled by        : " << Norm_by_integral(0.1,0.2) << endl; 
	    cout << endl << endl ;
	}


    // fill tagdb
    //cout << " QinvEbyECorrFctn::EventEnd() - fill tags now " << endl;
    // domimik's 3 parameter fit
    // mTagWriter->SetTag(mTagMeans,0, fit3.constant);
    //     mTagWriter->SetTag(mTagMeans,1, fit3.lambda); 
    //     mTagWriter->SetTag(mTagMeans,2, fit3.radius);  
    //     mTagWriter->SetTag(mTagMeans,3, fit3.chi2/fit3.ndf ); 
    //     mTagWriter->SetTag(mTagSigmas,0, fit3.constantErr ); 
    //     mTagWriter->SetTag(mTagSigmas,1, fit3.lambdaErr ); 
    //     mTagWriter->SetTag(mTagSigmas,2, fit3.radiusErr );  
    //     mTagWriter->SetTag(mTagSigmas,3, fit3.chi2 ); 
    //     mTagWriter->SetTag(mTagSigmas,4, fit3.ndf );  

} ;
//_______________________________________
void QinvEbyECorrFctn::Three_param_fit( threeFit& fit , StHbt1DHisto* histo  ) { 
    // Fit the unnormalized correlation function with A + B exp(-qinv^2 * C^2) 
    // Unfortunately we have to define the fit function here, if we do it
    // somewhere else we can't set the sart values for the fit !
    StHbtTF1* f3param  = new StHbtTF1("f3param","[0]+[1]*exp(- (x**2) * ([2]**2) / (0.197327**2))",0.0,0.2) ;
    f3param->SetParNames("Normfactor","Lambda","Rinv") ;
    f3param->SetParameters(0.2,0.5,10) ; // Set start values for fitting
    histo->Fit("f3param","R0") ;       // Fit (option Q == quiet!)
    
    // fill results
    fit.constant    =  histo->GetFunction("f3param")->GetParameter(0);  
    fit.lambda      =  histo->GetFunction("f3param")->GetParameter(1);
    fit.radius      =  histo->GetFunction("f3param")->GetParameter(2);
    fit.constantErr =  histo->GetFunction("f3param")->GetParError(0);
    fit.lambdaErr   =  histo->GetFunction("f3param")->GetParError(1);
    fit.radiusErr   =  histo->GetFunction("f3param")->GetParError(2);
    fit.chi2        =  histo->GetFunction("f3param")->GetChisquare();
    fit.ndf         =  histo->GetFunction("f3param")->GetNDF() ;
   
    // clean up
    delete f3param ;
} ;
//_______________________________________
void QinvEbyECorrFctn::Two_param_fit(twoFit& fit) {
    // Fit the normalized correlation function with  1 + A exp(-qinv^2 * B^2)
    // Unfortunately we have to define the fit function here, if we do it
    // somewhere else we can't set the sart values for the fit !
    StHbtTF1* f2param = new StHbtTF1("f2param"," 1 + [0]*exp(- (x**2) * ([1]**2) / (0.197327**2))",0.0,0.2) ;
    f2param->SetParNames("Lambda","Rinv") ;
    f2param->SetParameters(0.5,10) ;// Set start values for fitting
    mRatio->Fit("f2param","R0+") ;// Fit (option Q == quiet!)

    // fill results
    fit.lambda      =  mRatio->GetFunction("f2param")->GetParameter(0);
    fit.radius      =  mRatio->GetFunction("f2param")->GetParameter(1);
    fit.lambdaErr   =  mRatio->GetFunction("f2param")->GetParError(0);
    fit.radiusErr   =  mRatio->GetFunction("f2param")->GetParError(1);
    fit.chi2        =  mRatio->GetFunction("f2param")->GetChisquare();
    fit.ndf         =  mRatio->GetFunction("f2param")->GetNDF() ;
    
    // clean up
    delete f2param ;
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
    
    Width_MeV = ::sqrt(fabs(sum/(S_y))) ;
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
    double lam = rand()/RAND_MAX;     // lam = [0..1]
    double R = 1 + 10 * rand()/RAND_MAX;  // R = [1..11]
    R =10 ; lam = 0.5 ;
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
double QinvEbyECorrFctn::Norm_by_integral(double min_GeV = 0.1,double max_GeV = 0.2 )
{
    // Divide integral (0.1 - 0.2) over background by integral over signal
    double norm = 0;
    int minbin = mDenominator->GetXaxis()->FindFixBin(min_GeV);
    int maxbin = mDenominator->GetXaxis()->FindFixBin(max_GeV);
    double numinteg = mNumerator->Integral(minbin,maxbin) ;
    double denominteg = mDenominator->Integral(minbin,maxbin) ;

    if ( numinteg != 0 )
	{
	    norm =  denominteg / numinteg ;
	    // cout << "Normalized via integral by " << norm << endl ;
	}
    else
	{
	    cout << "Cannot normlize, because denominator equals 0." << endl ;
	}
    return norm ;
} ;
//_______________________________________
double QinvEbyECorrFctn::Norm_by_fit(double min_GeV = 0.1,double max_GeV = 0.2)
{
    // Fit the flat part of the correlation function by a straight line 
    StHbtTF1* line = new StHbtTF1("line","[0]+[1] * x",min_GeV,max_GeV) ;
    line->SetParameters(0.3,0.0) ; // set starting values
    mRatio->Fit("line","QR0") ; // fit
    if ( m_Debug_ebye )
	{
	    //cout << "Normalized via fit. " << endl ;
	    cout << "Fit result line fit, norm : " << mRatio->GetFunction("line")->GetParameter(0) << "  " << mRatio->GetFunction("line")->GetParameter(1) << endl ;
	}
    
    double norm = 0 ;
    double a = mRatio->GetFunction("line")->GetParameter(0) ;
    double b = mRatio->GetFunction("line")->GetParameter(1) ;
    double c = (max_GeV+min_GeV)/2 * b + a ; // get value at (max_GeV+min_GeV)/2 
    if ( c!= 0 )
	{
	    norm = 1/c ;
	}

    // clean up and go home
    delete line ;
    return norm ;
} ;
