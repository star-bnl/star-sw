/***************************************************************************
 *
 * Author: Dominik Flierl, flierl@bnl.gov
 ***************************************************************************
 *
 * Description: 
 *  YKP parametrization in longitudinal direction in kt-Y-Bins
 *
 *
 **************************************************************************/


#include "StHbtMaker/CorrFctn/YKPLongitudinal.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(YKPLongitudinal)
#endif

//____________________________
YKPLongitudinal::YKPLongitudinal( const char* frame ,
				  const int& nbins, const float& QLo, const float& QHi, 
				  const int& nKtbins, const double& ktMin, const double& ktMax,
				  const int& nYbins,  const double& YMin,  const double& YMax )
{
    // set frame
    if (frame == "LCMS" || frame == "CMS" || frame == "PRF") 
	{
	    mFrame = frame[0]  ;
	}
    else 
	{
	    cout << "Error: frame choice didn't fit LCMS.CMS or PRF -> LCMS used !\n" ;
	    mFrame = 'L' ;
	}

    // get intializers in
    mNumberKtBins = nKtbins ;
    mNumberYBins = nYbins ;
    
    // note : bin = binKt + (binY-1) * mNumberKtBins
    mNumberBins = mNumberKtBins * mNumberYBins ;
    
    // pointer to Coulomb Correction object must be set from macro
    mCorrection = 0 ;  
  
    // set up 3d histos
    mNumerator    = new StHbt3DHisto[mNumberBins] ;
    mDenominator  = new StHbt3DHisto[mNumberBins] ;
    mRatio        = new StHbt3DHisto[mNumberBins] ;
    mQinvNumerator   = new StHbt1DHisto[mNumberBins] ;
    mQinvDenominator = new StHbt1DHisto[mNumberBins] ;
    mQinvRatio       = new StHbt1DHisto[mNumberBins] ;
    char TitNum[100] ; 
    char TitDen[100] ; 
    char TitRat[100] ;
    char TitQinvNum[100] ;
    char TitQinvDen[100] ;
    char TitQinvRatio[100] ;
    {for(int ktindex = 0 ; ktindex < mNumberKtBins ; ktindex++)
	{ 
	    for(int yindex = 0 ; yindex < mNumberYBins ; yindex++)	{ 
		// numerator
		sprintf(TitNum,"NumKt%dY%d",ktindex+1,yindex+1) ;
		mNumerator[ktindex+yindex*mNumberKtBins].SetName(TitNum);
		mNumerator[ktindex+yindex*mNumberKtBins].SetTitle(TitNum);
		mNumerator[ktindex+yindex*mNumberKtBins].SetBins(nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi) ;
		mNumerator[ktindex+yindex*mNumberKtBins].SetXTitle("qLong"); 
		mNumerator[ktindex+yindex*mNumberKtBins].SetYTitle("qPerp");  
		mNumerator[ktindex+yindex*mNumberKtBins].SetZTitle("qNull");
		// denominator
		sprintf(TitDen,"DenKt%dY%d",ktindex+1,yindex+1) ;
		mDenominator[ktindex+yindex*mNumberKtBins].SetName(TitDen);
		mDenominator[ktindex+yindex*mNumberKtBins].SetTitle(TitDen);
		mDenominator[ktindex+yindex*mNumberKtBins].SetBins(nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi) ;
		mDenominator[ktindex+yindex*mNumberKtBins].SetXTitle("qLong"); 
		mDenominator[ktindex+yindex*mNumberKtBins].SetYTitle("qPerp");  
		mDenominator[ktindex+yindex*mNumberKtBins].SetZTitle("qNull");
		// ratio
		sprintf(TitRat,"RatKt%dY%d",ktindex+1,yindex+1) ;
		mRatio[ktindex+yindex*mNumberKtBins].SetName(TitRat);
		mRatio[ktindex+yindex*mNumberKtBins].SetTitle(TitRat);
		mRatio[ktindex+yindex*mNumberKtBins].SetBins(nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi) ;
		mRatio[ktindex+yindex*mNumberKtBins].SetXTitle("qLong"); 
		mRatio[ktindex+yindex*mNumberKtBins].SetYTitle("qPerp");  
		mRatio[ktindex+yindex*mNumberKtBins].SetZTitle("qNull");
		// qinv numerator
		sprintf(TitQinvNum,"QinvNumeratorKt%dY%d",ktindex+1,yindex+1) ;
		mQinvNumerator[ktindex+yindex*mNumberKtBins].SetName(TitQinvNum);
		mQinvNumerator[ktindex+yindex*mNumberKtBins].SetTitle(TitQinvNum);
		mQinvNumerator[ktindex+yindex*mNumberKtBins].SetBins(nbins,QLo,QHi) ;
		mQinvNumerator[ktindex+yindex*mNumberKtBins].SetXTitle("qInv"); 
		// qinv denominator
		sprintf(TitQinvDen,"QinvDenominatorKt%dY%d",ktindex+1,yindex+1) ;
		mQinvDenominator[ktindex+yindex*mNumberKtBins].SetName(TitQinvDen);
		mQinvDenominator[ktindex+yindex*mNumberKtBins].SetTitle(TitQinvDen);
		mQinvDenominator[ktindex+yindex*mNumberKtBins].SetBins(nbins,QLo,QHi) ;
		mQinvDenominator[ktindex+yindex*mNumberKtBins].SetXTitle("qInv"); 
		// qinv ratio
		sprintf(TitQinvRatio,"QinvRatioKt%dY%d",ktindex+1,yindex+1) ;
		mQinvRatio[ktindex+yindex*mNumberKtBins].SetName(TitQinvRatio);
		mQinvRatio[ktindex+yindex*mNumberKtBins].SetTitle(TitQinvRatio);
		mQinvRatio[ktindex+yindex*mNumberKtBins].SetBins(nbins,QLo,QHi) ;
		mQinvRatio[ktindex+yindex*mNumberKtBins].SetXTitle("qInv") ; 
		// to enable error bar calculation...
		mNumerator[ktindex+yindex*mNumberKtBins].Sumw2() ;
		mDenominator[ktindex+yindex*mNumberKtBins].Sumw2() ;
		mRatio[ktindex+yindex*mNumberKtBins].Sumw2() ;
		mQinvNumerator[ktindex+yindex*mNumberKtBins].Sumw2() ;
		mQinvDenominator[ktindex+yindex*mNumberKtBins].Sumw2() ; 
		mQinvRatio[ktindex+yindex*mNumberKtBins].Sumw2() ;
	    }
	}}

    ////
    // set up y pt bins boundaries
    ///
    // kt
    mktBinsMin = new double[mNumberKtBins] ;
    mktBinsMax = new double[mNumberKtBins] ;
    double ktstep = (double)(ktMax-ktMin)/(double) mNumberKtBins ;
    {for(int ktindex = 0 ; ktindex < mNumberKtBins  ;  ktindex++ )
	{
	    mktBinsMin[ktindex] = ((double) ktindex) * ktstep + ktMin ;
	    mktBinsMax[ktindex] = ((double) (ktindex+1)) * ktstep + ktMin ;
	}}
    // rap
    mYBinsMin  = new double[mNumberKtBins] ;
    mYBinsMax  = new double[mNumberYBins] ;
    double ystep  = (double)(YMax-YMin)/(double) mNumberYBins ;
    for(int yindex = 0 ; yindex < mNumberYBins  ;  yindex++ )
	{
	    mYBinsMin[yindex] = ((double) yindex) * ystep + YMin ;
	    mYBinsMax[yindex] = ((double) (yindex+1)) * ystep + YMin ;
	}

    /////
    // set up normalization
    ////
    // set range normalization
    mQinvNormLo = 0.15 ;
    mQinvNormHi = 0.18 ;
    // set up  array with normalization values    
    mNumRealsNorm = new unsigned long int [mNumberBins] ;
    mNumMixedNorm = new unsigned long int [mNumberBins] ;
    for( int index = 0 ; index < mNumberBins ; index++ ) 
	{
	    mNumRealsNorm[index] = mNumMixedNorm[index] = 0 ;
	}
}
//____________________________
YKPLongitudinal::~YKPLongitudinal()
{
    delete [] mNumRealsNorm ;
    delete [] mNumMixedNorm ;
    delete [] mNumerator ;
    delete [] mDenominator ;
    delete [] mRatio ; 
    delete [] mQinvNumerator ; 
    delete [] mQinvDenominator ; 
    delete [] mQinvRatio ;
}
//_________________________
void YKPLongitudinal::Finish()
{
    // here is where we can normalize, fit, etc...
    double NumFact,DenFact;
    for(int ktindex = 0 ; ktindex < mNumberKtBins ; ktindex++)
	{ 
	    for(int yindex = 0 ; yindex < mNumberYBins ; yindex++)
		{ 
		    // get the normalisation 
		    if ((mNumRealsNorm[ktindex+yindex*mNumberKtBins] !=0) && (mNumMixedNorm[ktindex+yindex*mNumberKtBins] !=0))
			{
			    NumFact = double(mNumRealsNorm[ktindex+yindex*mNumberKtBins]) ;
			    DenFact = double(mNumMixedNorm[ktindex+yindex*mNumberKtBins]) ;
			    cout << " Normalizing with Num/Denom = norm : " << NumFact << "\t" << DenFact << "\t" <<  NumFact/DenFact 
				 << " for kt bin : " << ktindex << "   y bin : " << yindex << endl ; 
			}
		    // error no normalization filled
		    else
			{
			    cout << "Warning! - no normalization possible ..." << endl;
			    NumFact = 1.0 ;
			    DenFact = 10.0 ;
			}
		    // normalize ratio
		    mRatio[ktindex+yindex*mNumberKtBins].Divide(&mNumerator[ktindex+yindex*mNumberKtBins],
								&mDenominator[ktindex+yindex*mNumberKtBins],DenFact,NumFact) ;

		    // produce qinv ratio
		    mQinvRatio[ktindex+yindex*mNumberKtBins].Divide(&mQinvNumerator[ktindex+yindex*mNumberKtBins],
								    &mQinvDenominator[ktindex+yindex*mNumberKtBins]) ;
		}
	}
}
//____________________________
StHbtString YKPLongitudinal::Report()
{
    // report called by manager per standard
    // get the overall values
    int mNumeratorEntriesAll = 0 ;
    int mDenominatorEntriesAll = 0 ;
    int mRatioEntriesAll = 0 ;
    int mNumRealsNormAll = 0 ;
    int mNumMixedNormAll = 0 ;

    for(int ktindex = 0 ; ktindex < mNumberKtBins ; ktindex++)
	{ 
	    for(int yindex = 0 ; yindex < mNumberYBins ; yindex++)
		{ 
		    mNumeratorEntriesAll   += (int)mNumerator[ktindex+yindex*mNumberKtBins].GetEntries() ;
		    mDenominatorEntriesAll += (int)mDenominator[ktindex+yindex*mNumberKtBins].GetEntries() ;
		    mRatioEntriesAll       += (int)mRatio[ktindex+yindex*mNumberKtBins].GetEntries() ;	
		    mNumRealsNormAll  += mNumRealsNorm[ktindex+yindex*mNumberKtBins] ;
		    mNumMixedNormAll  += mNumMixedNorm[ktindex+yindex*mNumberKtBins] ;
		}
	}
    // put the report together
    string stemp = "YKP Function Report:\n";
    char ctemp[100];
    sprintf(ctemp,"Number of entries in numerator in all %d histos:\t%d\n",mNumberBins,mNumeratorEntriesAll);
    stemp += ctemp;
    sprintf(ctemp,"Number of entries in denominator in all %d histos:\t%d\n",mNumberBins,mDenominatorEntriesAll);
    stemp += ctemp;
    sprintf(ctemp,"Number of entries in ratio in all %d histos:\t%d\n",mNumberBins,mRatioEntriesAll);
    stemp += ctemp;
    sprintf(ctemp,"Normalization region in Qinv was:\t%E\t%E\n",mQinvNormLo,mQinvNormHi);
    stemp += ctemp;
    sprintf(ctemp,"Number of pairs in Normalization region was:\n");
    stemp += ctemp;
    sprintf(ctemp,"In numerator:\t%u\t In denominator:\t%u\n",mNumRealsNormAll,mNumMixedNormAll);
    stemp += ctemp ;
    if (mCorrection)
      {
	  float radius = mCorrection->GetRadius();
	  sprintf(ctemp,"Coulomb correction used radius of\t%E\n",radius) ;
      }
    else
	{
	    sprintf(ctemp,"No Coulomb Correction applied to this CorrFctn\n") ;
	}
    stemp += ctemp ;
  
    //  return it
    StHbtString returnThis = stemp ;
    return returnThis ;
}
//____________________________
void YKPLongitudinal::AddRealPair(const StHbtPair* pair)
{
    // get the pair momenta 
    double mKt = pair->kT() ; 
    double mRap = pair->rap() ;
    double mQinv = fabs(pair->qInv()) ; 
    // sort it into the approriate histo
    for(int ktindex = 0 ; ktindex < mNumberKtBins ; ktindex++)
	{ 
	    if( mKt >= mktBinsMin[ktindex] && mKt < mktBinsMax[ktindex] )
		{
		    for(int yindex = 0 ; yindex < mNumberYBins ; yindex++)
			{ 
			    if( mRap >= mYBinsMin[yindex] && mRap < mYBinsMax[yindex] ) 
				{
				    // sum up qinv pairs to do later the normalisation 
				    if ((mQinv < mQinvNormHi) && (mQinv > mQinvNormLo)) mNumRealsNorm[ktindex+yindex*mNumberKtBins]++;
				    // fill 1d qinv histo
				    mQinvNumerator[ktindex+yindex*mNumberKtBins].Fill(mQinv) ;
				    // choose right frame 
				    double qlong, qperp, q0 ;
				    switch (mFrame)
					{
					case 'L' :
					    pair->qYKPLCMS(qlong,qperp,q0) ;
					    break ;
					case 'C' :
					    pair->qYKPCMS(qlong,qperp,q0) ;
					    break ;    
					case 'P' :    
					    pair->qYKPPF(qlong,qperp,q0) ;
					default :
					    cout << "This should never happen !\n" ;
					}
				    // fill 3d histo
				    mNumerator[ktindex+yindex*mNumberKtBins].Fill(qlong,qperp,q0) ;
				    // end yindex loop
				    break ; 
				}
			}// for yindex
		    // end kt loop
		    break ;
		} // if kt  
	} // for kt
}
//____________________________
void YKPLongitudinal::AddMixedPair(const StHbtPair* pair)
{
    // get the pair momenta 
    double mKt = pair->kT() ; 
    double mRap = pair->rap() ;
    double mQinv = fabs(pair->qInv()); 
    // caculate coulomb weight
    double weight=1.0;
    if (mCorrection)
	{
	    weight = mCorrection->CoulombCorrect(pair);
	}
    
    // sort it into the approriate histo
    for(int ktindex = 0 ; ktindex < mNumberKtBins ; ktindex++)
	{ 
	    if( mKt >= mktBinsMin[ktindex] && mKt < mktBinsMax[ktindex] )
		{
		    for(int yindex = 0 ; yindex < mNumberYBins ; yindex++)
			{ 
			    if( mRap >= mYBinsMin[yindex] && mRap < mYBinsMax[yindex] ) 
				{
				    // sum up qinv pairs to do later the normalisation 
				    if ((mQinv < mQinvNormHi) && (mQinv > mQinvNormLo)) mNumMixedNorm[ktindex+yindex*mNumberKtBins]++;
				    // fill 1d qinv histo
				    mQinvDenominator[ktindex+yindex*mNumberKtBins].Fill(mQinv) ;
				    // choose right frame 
				    double qlong, qperp, q0 ;
				    switch (mFrame)
					{
					case 'L' :
					    pair->qYKPLCMS(qlong,qperp,q0) ;
					    break ;
					case 'C' :
					    pair->qYKPCMS(qlong,qperp,q0) ;
					    break ;    
					case 'P' :    
					    pair->qYKPPF(qlong,qperp,q0) ;
					default :
					    cout << "This should never happen !\n" ;
					}
				    // fill 3d histo
				    mDenominator[ktindex+yindex*mNumberKtBins].Fill(qlong,qperp,q0) ;
				    // end yindex loop
				    break ; 
				}
			}// for yindex
		    // end kt loop
		    break ;
		} // if kt  
	} // for kt
}
