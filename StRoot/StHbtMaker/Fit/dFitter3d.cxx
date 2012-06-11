/***************************************************************************
 *
 *
 * Author: Dominik Flierl, flierl@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a object which uses TMinuit to fit correlationfunctions
 *
 *
 **************************************************************************/

#include "dFitter3d.h"
#include "Stiostream.h"
#include "TMath.h"
#include "TH3.h"
#include "TH1.h"
#include "TMinuit.h"
#include <TVector3.h>
#include "TString.h"
#include "TObject.h"
#include "TMath.h"
//#ifdef __ROOT__
ClassImp(dFitter3d)
    //#endif
  
// this is the stupid way ROOT wants it: the fcn which is going to be minimzed must
// either be static or a global function. here it's global so make sur that we got
// the connection to this class via TMinuit->SetObjectFit(this) !  
    void gfcn(Int_t &nParamters, Double_t *gin, Double_t &finalChi2, Double_t *parameterSet, Int_t iflag)
{
    dFitter3d* dummy = dynamic_cast<dFitter3d*>(gMinuit->GetObjectFit()) ;
    dummy->mfcn(nParamters, gin, finalChi2 , parameterSet, iflag) ;
}


//____________________________
dFitter3d::dFitter3d(TMinuit* gMinuit,TH3D* numerator, TH3D* denominator, TString opt , TString opt2)
{
    // get TMinuit in
    mMinuit = gMinuit ;
    
    // get numerator and denominator in
    mNumerator = numerator ; 
    mDenominator = denominator ;
    
    // set type of correlationfunction
    if ( opt == "ykp" )
	{
	    mCorrFctnType = "ykp" ;
	    cout << "Fitting function set to Yano Koonin Podgoretskii. \n" ;
	}
    else if ( opt == "bp" )
	{
	    mCorrFctnType = "bp" ;
	    cout << "Fitting function set to Bertsch Pratt. \n" ;
	}
    else 
	{
	    mCorrFctnType = "ykp" ;
	    cout << "Bad option : fitting function set to Yano Koonin Podgoretskii. \n" ;
	}

    // set typ of fitting MML or Chi2
    if ( opt2 == "chi2" )
	{
	    mFitMethod  = "chi2" ;
	    cout << "Using chi2.\n" ;
	}
    else if ( opt2 == "mml" )
	{
	    mFitMethod  = "mml" ;
	    cout << "Using mml.\n" ;
	}
    else 
	{
	    mFitMethod  = "chi2" ;
	    cout << "Bad option : Using chi2 method.\n" ;
	}   ;
	

    // set constants
    mhc  = 0.197326960277 ; // GeV * fm
    mhc2 = 0.038937929230 ; // GeV^2 * fm^2 ;
 
  // set defaults for thresholds = 1 means accept any entry
    mThresholdNumerator = 1.0 ;
    mThresholdDenominator = 1.0 ;
}
//____________________________
dFitter3d::~dFitter3d()
{
    // nothing to do here
}
//____________________________
void dFitter3d::FillInternalArrays()
{
    //  some output
    cout << " Filling internal arrays with norm factor : " << mNorm 
	 << "  numerator threshold : " << mThresholdNumerator 
	 << "  denominator threshold : " << mThresholdDenominator << endl ;
    
    
    // get the maximum number of bins
    int maximumInternalArraySize = (mNumerator->GetNbinsX()+2) * (mNumerator->GetNbinsY()+2) * (mNumerator->GetNbinsZ()+2) ;
    if ( maximumInternalArraySize != (mDenominator->GetNbinsX()+2) * (mDenominator->GetNbinsY()+2) * (mDenominator->GetNbinsZ()+2) )
	std::cerr << "Warning : different histogram sizes in numerator and denominator\n " ;
    
    // now create arrays
    mNumeratorInternalArray   = new double[maximumInternalArraySize] ;
    mDenominatorInternalArray = new double[maximumInternalArraySize] ;
    mErrorInternalArray       = new double[maximumInternalArraySize] ;
    mVariablePositionArray    = new TVector3[maximumInternalArraySize] ;
    
    // count number of actually used bins
    mInternalArraySize = 0 ;
    
    // loop over histogram and fill values into vectors 
    for (int index = 0 ; index < maximumInternalArraySize ; index++)
	{
	    if ( mNumerator->GetBinContent(index) > mThresholdNumerator && mDenominator->GetBinContent(index) > mThresholdDenominator )
		{
		    // take center of the BIN ( as according position in qlong,qside,qnull )
		    int binX = 0 ; 
		    int binY = 0 ; 
		    int binZ = 0 ;
		    Bin1ToBin3(mNumerator, index, binX, binY, binZ) ;
		    double qx = mNumerator->GetXaxis()->GetBinCenter(binX) ;
		    double qy = mNumerator->GetYaxis()->GetBinCenter(binY) ;
		    double qz = mNumerator->GetZaxis()->GetBinCenter(binZ) ;
		    
		    if ( qx*qx+qy*qy+qz*qz < mSphereLimit )
			{
			    
			    ////////
			    // fill the numerator and denominator histogram into vectors
			    ///////
			    double num   = mNumerator->GetBinContent(index) ;
			    double denom = mDenominator->GetBinContent(index) ;
			    // fill numerator
			    mNumeratorInternalArray[mInternalArraySize]    = num ;
			    // normalize and fill background
			    mDenominatorInternalArray[mInternalArraySize]  = denom * mNorm ;
			    
			    // the error is cacluated using Gauss' error propagation : e = num/(denom*norm) * ::sqrt(1/num + 1/(denom))
			    double error = num/(denom*mNorm) * ::sqrt( (1/num) + (1/denom) ) ;
			    mErrorInternalArray[mInternalArraySize]  = error ;
			    // the simple case : take center of the BIN ( as according position in qlong,qside,qnull )
			    mVariablePositionArray[mInternalArraySize].SetX(qx) ;
			    mVariablePositionArray[mInternalArraySize].SetY(qy) ;
			    mVariablePositionArray[mInternalArraySize].SetZ(qz) ;
			    
			    // some silly output to see where we are
			    if (0) 
				{
				    cout << (double) num << "\t" << (double) (denom*mNorm) ; //<<index << binX << binY << binZ << endl;
				    cout << "\t" << (double) (num/(denom*mNorm)) ;
				    double xx = mNumerator->GetXaxis()->GetBinCenter(binX) ;
				    double yy = mNumerator->GetYaxis()->GetBinCenter(binY) ;
				    double zz = mNumerator->GetZaxis()->GetBinCenter(binZ) ;
				    cout << "xa " << xx << "\t" << binX << "\t" ;
				    cout << "ya " << yy << "\t" << binY << "\t" ;
				    cout << "za " << zz << "\t" << binZ << endl ;
			}
			    
			    // count entries
			    mInternalArraySize++ ;
			}
		}
	}
    // done
    cout << "Internal array size :" << mInternalArraySize << endl ;
}
//____________________________
void  dFitter3d::mfcn(Int_t &nParameters, Double_t *gin, Double_t &finalChi2, Double_t *parameterSet, Int_t iflag) 
{
    if ( mFitMethod == "chi2")
	{
	    fcnChi2(nParameters,gin,finalChi2,parameterSet,iflag) ;
	}
    else if ( mFitMethod == "mml")
	{
	    fcnMml(nParameters,gin,finalChi2,parameterSet,iflag) ;
	} ;
} 
//____________________________
void  dFitter3d::fcnChi2(Int_t &nParameters, Double_t *gin, Double_t &finalChi2, Double_t *parameterSet, Int_t iflag) 
{
    /////
    // calculate chi2 of the given paramset 'parameterSet'
    /////
    double chi2 = 0.0 ;
        
    // loop over Internal Array and caluclate chi2 
    for (Int_t index = 0 ;  index < mInternalArraySize ; index++)
	{
	    // calculate the expected value at this point (variablePosition) for this set of paramters
	    double theoreticalValue  = mCorrelationFunction( mVariablePositionArray[index] , parameterSet) ; 
	    // get the difference between measured and predicted value
	    double delta = (mNumeratorInternalArray[index]/mDenominatorInternalArray[index]-theoreticalValue)/mErrorInternalArray[index];
	    // sum the chi2
	    chi2 += delta*delta;
	    
      
      // some silly output
	    if(countMinuitCalls == 100 && index%10000==0)
		{
		    cout << "ql : " << mVariablePositionArray[index].x() << "\t" 
			 << "qp : " << mVariablePositionArray[index].y() << "\t"
			 << "q0 : " << mVariablePositionArray[index].z()<< "\t" 
			 << "thval: " <<  theoreticalValue << "\t"
			 << "mes " <<  mNumeratorInternalArray[index]/mDenominatorInternalArray[index] << "\t"
			 << "num: " <<  mNumeratorInternalArray[index]  << "\t"
			 << "th-mea" << fabs( mNumeratorInternalArray[index]/mDenominatorInternalArray[index] - theoreticalValue)   << "\t"
			 << "err " << mErrorInternalArray[index] << endl ;
		}
      
	}
    // chi2 is calculated
    finalChi2 = chi2 ;

    // count minuit calls
    countMinuitCalls++ ;
    //cout << "Minuit call : " << countMinuitCalls <<  "   chi2 : " << finalChi2 << endl ;
};
//____________________________
void  dFitter3d::fcnMml(Int_t &nParameters, Double_t *gin, Double_t &finalMLH, Double_t *parameterSet, Int_t iflag) 
{
    // calculate maximum likelihood asuming an underlying poisson distribution, for details see for example
    // http://nedwww.ipac.caltech.edu/level5/Leo/Stats_contents.html
    /*
      poison distribution : p(r) = y^r * exp(-y) / r!      y = mean
      likelihood distribution to be maximized L, n number of measurements (x1....xn) :
      L = -n*y + Sum(xi * ln(y)) - Sum(ln(xi!)) = Sum( -y + xi * ln(y) - ln(xi!) )
    */
      
    // some definitions
    double MLH  = 0.0 ;
             
    // loop over Internal Array and caluclate chi2 
    for (Int_t index = 0 ;  index < mInternalArraySize ; index++)
	{
	    // calculate the expected value at this point (variablePosition) for this set of paramters
	    double expectedValue  = mCorrelationFunction( mVariablePositionArray[index] , parameterSet) *  mDenominatorInternalArray[index] ; 
	    // measured value
	    double measueredValue = mNumeratorInternalArray[index] ;
	    // sum up
	    MLH += -expectedValue + measueredValue * ::log(expectedValue) - lnfactorial(measueredValue) ;
	}
    // likelihood for this set of paramters
    finalMLH = - MLH ;

    // count number of calls from Minuit
    countMinuitCalls++ ;
}
//________________________________
double dFitter3d::lnfactorial(double arg)
{
    // return the factorial of arg
    double fac = 1.0 ;
    int iarg = (int) arg ;
    if(iarg<50)
	{
	    for (int index = 1; index < iarg+1 ; index++)
		{ fac *=(double)index; } ;
	    fac = ::log(fac) ;
	}
    else 
	{
	  //if the argument is too large : use Stirlings formula
	    fac = 0.91 + (iarg+0.5) * ::log(double(iarg)) - iarg ;
	}

    return fac;
}
//____________________________
double  dFitter3d::mCorrelationFunction(TVector3& position, double* parameterSet ) 
{
    if (  mCorrFctnType == "ykp")
	{
	    return ykpCorrelationFunction( position, parameterSet ) ;
	}
    else if (  mCorrFctnType == "bp")
	{
	    return bpCorrelationFunction( position, parameterSet ) ;
	} 
    else
	{
	    // this should never happen
	    cout << "this should never happen.\n " ;
	    return 0.0 ;
	}
}
//____________________________
double dFitter3d::ykpCorrelationFunction(TVector3& position, double* parameterSet ) 
{
    // return C2 at 'position' with Parameterset 'parameterSet' for YKP parametrization
    
    // position.x = qlong 
    // position.y = qperp 
    // position.z = qnull    
    // parameterSet[0] = lambda 
    // parameterSet[1] = Rlong  
    // parameterSet[2] = Rperp  
    // parameterSet[3] = Rnull
    // parameterSet[4] = beta
    double gamma2 = 1.0/fabs(1.0-::pow(parameterSet[4],2)) ;
    double mhcc =  0.038937929230 ;
    double c2 = 1.0 +  parameterSet[0] * TMath::Exp(
						    (
						     (-1.0) * ::pow(position.y(),2) * ::pow(parameterSet[2],2) 
						     - gamma2 * ::pow((position.x() - parameterSet[4]*position.z()),2) * ::pow(parameterSet[1],2)
						     - gamma2 * ::pow((position.z() - parameterSet[4]*position.x()),2) * ::pow(parameterSet[3],2)
						     ) 
						    / mhcc );
    return c2;
};
//____________________________
double dFitter3d::bpCorrelationFunction(TVector3& position, double* parameterSet ) 
{
    // return C2 at 'position' with 'parameterSet' for Bertsch-Pratt parametrization
  
    // position.x = qlong 
    // position.y = qside 
    // position.z = qout    
    // parameterSet[0] = lambda 
    // parameterSet[1] = Rside  
    // parameterSet[2] = Rout  
    // parameterSet[3] = Rlong
    // parameterSet[4] = Routlong
    double  c2 = 1.0 + parameterSet[0] * TMath::Exp(
						    (
						     (-1.0) * ::pow(position.y(),2) * ::pow(parameterSet[1],2) 
						     - ::pow(position.z(),2) * ::pow(parameterSet[2],2) 
						     - ::pow(position.x(),2) * ::pow(parameterSet[3],2) 
						     - 2* position.x() * position.z() * ::pow(parameterSet[4],2) 
						     ) 
						    / mhc2 );
    return c2 ;
};
//____________________________
void dFitter3d::doFit() 
{
    //  // Errorflag
    int ierflg = 0 ;
    double arglist[4];

    if (  mCorrFctnType == "ykp")
	{
	    // set start values
	    double startValues[5] = { 0.2 , 5.0 , 7.0 , 6.0 , 0.5} ;
	    // set step size
	    double stepSize[5]    = {0.1 ,0.1, 0.1 ,0.1 ,0.1 };
	    // set fit parameter names startvalue and stepsize
	    mMinuit->mnparm(0, "lambda",  startValues[0], stepSize[0], 0,0,ierflg);
	    mMinuit->mnparm(1, "Rlong",   startValues[1], stepSize[1], 0,0,ierflg);
	    mMinuit->mnparm(2, "Rperp",   startValues[2], stepSize[2], 0,0,ierflg);
	    mMinuit->mnparm(3, "Rnull",   startValues[3], stepSize[3], 0,0,ierflg);
	    mMinuit->mnparm(4, "beta",    startValues[4], stepSize[4], 0,0,ierflg);
	}
    else if (  mCorrFctnType == "bp")
	{
	    // set start values
	    double startValues[5] = { 0.5 , 6.0 , 6.0 , 6.0 , 6.0} ;
	    // set step size
	    double stepSize[5]    = {0.1 ,0.1, 0.1 ,0.1 ,0.1 };
	    // set fit parameter names startvalue and stepsize
	    mMinuit->mnparm(0, "lambda",   startValues[0], stepSize[0], 0,0,ierflg);
	    mMinuit->mnparm(1, "Rside",    startValues[1], stepSize[1], 0,0,ierflg);
	    mMinuit->mnparm(2, "Rout",     startValues[2], stepSize[2], 0,0,ierflg);
	    mMinuit->mnparm(3, "Rlong",    startValues[3], stepSize[3], 0,0,ierflg);
	    mMinuit->mnparm(4, "Routlong", startValues[4], stepSize[4], 0,0,ierflg);
	} 
    else
	{
	    // this should never happen
	    cout << "this should never happen.\n " ;
	    return ;
	}
      
  
    // set function to call
    mMinuit->SetObjectFit(this) ;
    mMinuit->SetFCN(gfcn) ;
    
  // Call minuit with some instructions
  //arglist[0] = 1;   mMinuit->mnexcm("SET ERR", arglist ,1,ierflg);     // set error ?
  //arglist[0] = 1.0; mMinuit->mnexcm("SET GRAD", arglist ,1,ierflg);    // set grad  ?
  //arglist[0] = 2;   mMinuit->mnexcm("SET STRA", arglist ,1,ierflg);    // set strategie ?
  //arglist[0] = 12;  mMinuit->mnexcm("CALL FCN", arglist ,1,ierflg);   // call fcn
  
  // Fix some fit parameter to get the fit to converge
  /*arglist[0] = 6;    mMinuit->mnexcm("FIX", arglist ,1,ierflg);  
    arglist[0] = 7;    mMinuit->mnexcm("FIX", arglist ,1,ierflg);  
    arglist[0] = 8;    mMinuit->mnexcm("FIX", arglist ,1,ierflg);  
    arglist[0] = 9;    mMinuit->mnexcm("FIX", arglist ,1,ierflg);
    arglist[0] = 10;   mMinuit->mnexcm("FIX", arglist ,1,ierflg);
    arglist[0] = 11;   mMinuit->mnexcm("FIX", arglist ,1,ierflg);*/

    // Call minuit again ... ???
    //arglist[0] = 4;   mMinuit->mnexcm("CALL FCN", arglist ,1,ierflg) ;
    //arglist[0] = 1000 ;
    //arglist[1] = 4; mMinuit->mnexcm("SEEK", arglist ,2,ierflg);
    arglist[0] = 0;    mMinuit->mnexcm("MIGRATE", arglist ,0,ierflg) ;
    //arglist[0] = 0;    mMinuit->mnexcm("MINOS", arglist   ,0,ierflg) ;
  
  /////
  // get chi2pdf
  /////
  // get parameter values
  //double doubledummy1 = 0 ;
  //double doubledummy2 = 0 ;
  //double pars[5] ;

  //for(int i = 0 ; i < 5 ; i++)
  //  {
  //    mMinuit->GetParameter(i, doubledummy1, doubledummy2) ;
  //    pars[i] = doubledummy1;
  //  }
  // call fcn
  //int intdummy = 0 ;
  //double* doubledummy3 ;
  //double finalchi2 = 0.0;
  //mfcn(intdummy, doubledummy3, finalchi2, pars, intdummy) ;

  //cout << " finalchi2 " << finalchi2 << endl ;
  //cout << " chi2pdf = finalchi2 / InternalArraySize-5parameters = " <<  finalchi2/(double)(mInternalArraySize-5) << endl ;

    cout << "Number of to Minuit calls : " << countMinuitCalls << endl ;
}

//________________________________
void dFitter3d::SetHistos(TH3D* numerator, TH3D* denominator)
{
    // set the numerator and denominator
    mNumerator = numerator ;
    mDenominator = denominator ;

    // calculate the ratio 
    mRatio = (TH3D*) numerator->Clone() ;
    mRatio->Reset() ;
    mRatio->Divide(numerator,denominator,1.0,mNorm) ;
}
//____________________________
void dFitter3d::Bin1ToBin3(TH3D* histo, int bin, int& binx, int& biny, int&  binz)
{
    //  // actually this should be provided by ROOT ... grrrr
    // return bin number of every axis (binx,biny,binz) if the general bin number (bin) is known
    // general bin = binX + binY * (nbinX+2) + binZ * ((nbinx+2) * (nbiny+2) )   (+2 acounts for over and underflow!)
    int nbbinX = (histo->GetNbinsX()) + 2 ;
    int nbbinY = (histo->GetNbinsY()) + 2 ;
    
    binx = bin%nbbinX ;
    biny = (bin/nbbinX) % nbbinY ;
    binz = (bin/nbbinX) / nbbinY ;
    
}
//____________________________
void dFitter3d::setCorrFctn(TString opt)
{
 
};
//____________________________
void dFitter3d::setFitMethod(TString opt2)
{
    // set typ of fitting MML or Chi2
    if ( opt2 == "chi2" )
	{
	    mFitMethod  = "chi2" ;
	    cout << "Using chi2.\n" ;
	}
    else if ( opt2 == "mml" )
	{
	    mFitMethod  = "mml" ;
	    cout << "Using mml.\n" ;
	}
    else 
	{
	    mFitMethod  = "chi2" ;
	    cout << "Bad option : Using chi2 method.\n" ;
	}   ;
 
};
