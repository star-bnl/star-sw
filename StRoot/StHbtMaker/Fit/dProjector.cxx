/***************************************************************************
 *
 * Author: Dominik Flierl, flierl@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   this is an object which produces projections of 3d histograms
 *  
 **************************************************************************/

#include "dProjector.h"
#include "TVector3.h"

#ifdef __ROOT__
ClassImp(dProjector)
#endif

#define __ABS__(x) (x>0) ? x : -x

  
//___________________
dProjector::dProjector(dFitter3d* dFitter) 
{ 
  // get the 3d histo
  mRatio = dFitter->Ratio() ;
  
  // if the ratio histogram does not exist -> generate it
  if (!mRatio)
    {
      mRatio = (TH3D*) (dFitter->Numerator())->Clone() ;
      mRatio->SetName("ratio") ;
      mRatio->Scale(1.0/dFitter->Norm()) ;
      mRatio->Divide(dFitter->Denominator()) ;
    }

  // get minuit to extract the fit result
  TMinuit* tMinuit = dFitter->getMinuit() ;
  // fill fit result into array
  mFitParameters = new double[5] ;
  cout << " used fit parameters : " << "\t" ;
  for( int index = 0 ; index < 5 ; index++ )
    {
      double value = 0 ;
      double error = 0 ;
      tMinuit->GetParameter(index, value, error) ;
      mFitParameters[index] = value ;
      cout << value << "\t" ;
    }
  cout << endl ;
} ;
//___________________
dProjector::~dProjector() 
{ 
  delete [] mFitParameters ;
} ;
//___________________
TH1D* dProjector::get1dProjection(TString axis, double xmin, double xmax, double ymin, double ymax, double zmin, double zmax)
{
  // get ranges of the ratio 3d histogram 
  int xminBin = mRatio->GetXaxis()->FindFixBin(xmin) ;
  int xmaxBin = mRatio->GetXaxis()->FindFixBin(xmax) ;
  int yminBin = mRatio->GetYaxis()->FindFixBin(ymin) ;
  int ymaxBin = mRatio->GetYaxis()->FindFixBin(ymax) ;
  int zminBin = mRatio->GetZaxis()->FindFixBin(zmin) ;
  int zmaxBin = mRatio->GetZaxis()->FindFixBin(zmax) ;

  // get dimensions of the projection histos
  int proNbins = 0 ;
  double proMin = 0.0 ;
  double proMax = 0.0 ;
  char* axistitle = new char[20] ;
  if (axis == "x")        {  proNbins = __ABS__(xmaxBin-xminBin)+1 ;  proMin = xmin ; proMax = xmax ; strcpy(axistitle,mRatio->GetXaxis()->GetTitle()) ; }
  else if (axis == "y")   {  proNbins = __ABS__(ymaxBin-yminBin)+1 ;  proMin = ymin ; proMax = ymax ; strcpy(axistitle,mRatio->GetYaxis()->GetTitle()) ; }
  else if (axis == "z")   {  proNbins = __ABS__(zmaxBin-zminBin)+1 ;  proMin = zmin ; proMax = zmax ; strcpy(axistitle,mRatio->GetZaxis()->GetTitle()) ; }
  else { cout << "Wrong axis in projector, this should not happen ! "<< endl ; return 0; } ;

  // create 1d histo
  int ran = (int) (xmin*ymin*zmin*10000) ; // to distinguish between objects give them a special random id ...
  m1dpro = new TH1D("my1dpro","my1dpro",proNbins,proMin,proMax) ; 
  m1dpro->SetXTitle(axistitle) ;
  m1dpro->SetTitle(axistitle) ;
  char restitle[20] ; sprintf(restitle,"fit%s%d\n",axis.Data(),ran) ;
  m1dpro->SetName(restitle) ;
  m1dfit = new TH1D("my1dfit","my1dfit",proNbins,proMin,proMax) ;
  char fittitle[20] ; sprintf(fittitle,"fit%s%d\n",axis.Data(),ran) ;
  m1dfit->SetName(fittitle);
  m1dfit->SetLineColor(2) ;
  m1dfit->SetLineStyle(2) ;
  m1dfit->SetLineWidth(5) ;
  m1dnor = new TH1D("my1dnor","my1dnor",proNbins,proMin,proMax) ;
  char normtitle[20] ; sprintf(normtitle,"norm%s%d\n",axis.Data(),ran) ;
  m1dnor->SetName(normtitle);

  // fill histo
  for(int indexX = xminBin ; indexX <= xmaxBin ; indexX++)
    {
      for(int indexY = yminBin ; indexY <= ymaxBin ; indexY++)
	{
	  for(int indexZ = zminBin ; indexZ <= zmaxBin ; indexZ++)
	    {
	      // get the content of the 3d cell
	      double cellcontent = mRatio->GetBinContent(indexX,indexY,indexZ) ;
	      // get the content of the 3d cell using the fit function
	      TVector3 position(mRatio->GetXaxis()->GetBinCenter(indexX),
				mRatio->GetYaxis()->GetBinCenter(indexY),
				mRatio->GetZaxis()->GetBinCenter(indexZ));
	      double cellContentFromFit = mFitter->ykpCorrelationFunction(position,mFitParameters) ;
	      
	      
	      // fill the 1d histos
	      int proBin = - 100 ;
	      if (axis == "x")        { proBin = m1dpro->GetXaxis()->FindFixBin(mRatio->GetXaxis()->GetBinCenter(indexX)); }  
	      else if (axis == "y")   { proBin = m1dpro->GetXaxis()->FindFixBin(mRatio->GetYaxis()->GetBinCenter(indexY)); }
	      else if (axis == "z")   { proBin = m1dpro->GetXaxis()->FindFixBin(mRatio->GetZaxis()->GetBinCenter(indexZ)); }
	      
	      if(cellcontent>0.5 && cellcontent <1.5)
		{
		  m1dpro->AddBinContent(proBin,cellcontent) ;
		  m1dfit->AddBinContent(proBin,cellContentFromFit) ;
		  
		  if (axis == "z" && proBin<10) cout << proBin << " " ;
		  // count the number of fillings to normalize the projection to 1
		  m1dnor->AddBinContent(proBin,1.0) ;
		}
	    }
	}
    }
  // normalize fit and projection histos
  m1dpro->Divide(m1dnor) ;
  m1dfit->Divide(m1dnor) ;

  // return projection
  return m1dpro ;
}
//___________________
TH2D* dProjector::get2dProjection(TString axis, double xmin, double xmax, double ymin, double ymax, double zmin, double zmax)
{
    // get ranges of the ratio 3d histogram 
    int xminBin = mRatio->GetXaxis()->FindFixBin(xmin) ;
    int xmaxBin = mRatio->GetXaxis()->FindFixBin(xmax) ;
    int yminBin = mRatio->GetYaxis()->FindFixBin(ymin) ;
    int ymaxBin = mRatio->GetYaxis()->FindFixBin(ymax) ;
    int zminBin = mRatio->GetZaxis()->FindFixBin(zmin) ;
    int zmaxBin = mRatio->GetZaxis()->FindFixBin(zmax) ;
    
    // get dimensions of the projection histos
    int proNbinsX = 0 ;
    double proMinX = 0.0 ;
    double proMaxX = 0.0 ;
    char* axistitleX = new char[20] ;
    int proNbinsY = 0 ;
    double proMinY = 0.0 ;
    double proMaxY = 0.0 ;
    char* axistitleY = new char[20] ;

    if (axis == "x")        
	{  
	    proNbinsX = __ABS__(ymaxBin-yminBin)+1 ;  proMinX = ymin ; proMaxX = ymax ; strcpy(axistitleX,mRatio->GetYaxis()->GetTitle()) ; 
	    proNbinsY = __ABS__(zmaxBin-zminBin)+1 ;  proMinY = zmin ; proMaxY = zmax ; strcpy(axistitleY,mRatio->GetZaxis()->GetTitle()) ; 
	}
    else if (axis == "y")   
	{  
	    proNbinsX = __ABS__(zmaxBin-zminBin)+1 ;  proMinX = zmin ; proMaxX = zmax ; strcpy(axistitleX,mRatio->GetZaxis()->GetTitle()) ; 
	    proNbinsY = __ABS__(xmaxBin-xminBin)+1 ;  proMinY = xmin ; proMaxY = xmax ; strcpy(axistitleY,mRatio->GetXaxis()->GetTitle()) ;
	}
    else if (axis == "z")   
	{  
	    proNbinsX = __ABS__(xmaxBin-xminBin)+1 ;  proMinX = xmin ; proMaxX = xmax ; strcpy(axistitleX,mRatio->GetXaxis()->GetTitle()) ; 
	    proNbinsY = __ABS__(ymaxBin-yminBin)+1 ;  proMinY = ymin ; proMaxY = ymax ; strcpy(axistitleY,mRatio->GetYaxis()->GetTitle()) ;
	}

    else { cout << "Wrong axis in projector, this should not happen ! "<< endl ; return 0; } ;
    
    // create 2d histo
    int ran = (int) (xmin*ymin*zmin*10000) ; // to distinguish between objects give them a special random id ...
    // projection
    m2dpro = new TH2D("my2dpro","my2dpro",proNbinsX,proMinX,proMaxX,proNbinsY,proMinY,proMaxY) ; 
    char resName[20] ; sprintf(resName,"res%s%d\n",axis.Data(),ran) ;  m2dpro->SetName(resName) ;
    m2dpro->SetXTitle(axistitleX) ;
    m2dpro->SetYTitle(axistitleY) ;
    char restitle[20] ; sprintf(restitle,"%s\n",axis.Data()) ;m2dpro->SetTitle(restitle) ;
    // fit
    m2dfit = new TH2D("my2dfit","my2dfit",proNbinsX,proMinX,proMaxX,proNbinsY,proMinY,proMaxY) ;
    char fittitle[20] ; sprintf(fittitle,"fit%s%d\n",axis.Data(),ran) ; m2dfit->SetName(fittitle) ;
    // norm
    m2dnor = new TH2D("my2dnor","my2dnor",proNbinsX,proMinX,proMaxX,proNbinsY,proMinY,proMaxY) ;
    char normtitle[20] ; sprintf(normtitle,"norm%s%d\n",axis.Data(),ran) ; m2dnor->SetName(normtitle) ;
    
    // fill histo
    for(int indexX = xminBin ; indexX <= xmaxBin ; indexX++)
	{
	    for(int indexY = yminBin ; indexY <= ymaxBin ; indexY++)
		{
		    for(int indexZ = zminBin ; indexZ <= zmaxBin ; indexZ++)
			{
			    // get the content of the 3d cell
			    double cellcontent = mRatio->GetBinContent(indexX,indexY,indexZ) ;
			    // get the content of the 3d cell using the fit function
			    TVector3 position(mRatio->GetXaxis()->GetBinCenter(indexX),
					      mRatio->GetYaxis()->GetBinCenter(indexY),
					      mRatio->GetZaxis()->GetBinCenter(indexZ));
			    double cellContentFromFit = mFitter->ykpCorrelationFunction(position,mFitParameters) ;
			    
			    
			    // fill the 1d histos
			    double proX = 100000.0 ;
			    double proY = 100000.0 ;
			    if (axis == "x")        
				{ 
				    proX = mRatio->GetYaxis()->GetBinCenter(indexY) ; 
				    proY = mRatio->GetZaxis()->GetBinCenter(indexZ) ;
				}  
			    else if (axis == "y")   
				{ 
				    proX = mRatio->GetZaxis()->GetBinCenter(indexZ) ; 
				    proY = mRatio->GetXaxis()->GetBinCenter(indexX) ;
				}
			    else if (axis == "z")   
				{ 
				    proX = mRatio->GetXaxis()->GetBinCenter(indexX) ; 
				    proY = mRatio->GetYaxis()->GetBinCenter(indexY) ;
				}
			    
			    if(cellcontent>0.5 && cellcontent <1.5)
				{
				    // now fill 2d histos 
				    m2dpro->Fill(proX,proY,cellcontent) ;
				    m2dfit->Fill(proX,proY,cellContentFromFit) ;
				    // count the number of fillings to normalize the projection to 1
				    m2dnor->Fill(proX,proY,1.0) ;
				}
			}
		}
	}
    // normalize fit and projection histos
    m2dpro->Divide(m2dnor) ;
    m2dfit->Divide(m2dnor) ;
    
    // return projection
    return m2dpro ;
    
}
