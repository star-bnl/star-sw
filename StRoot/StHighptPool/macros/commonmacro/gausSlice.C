Double_t gausBack (Double_t *x, Double_t *par) 
{
    Double_t sigma = par[3];
    Double_t mean = par[2];
    Double_t sigfrac = par[1];
    Double_t backfrac = 1.-sigfrac;
    Double_t amp = par[0];
    
// Both have integrals of 1 between +- 2 Sigma
    Double_t gaus = 1./(sqrt(2.*3.14159265)*sigma*TMath::Erf(sqrt(2)))*
	exp(-0.5*(x[0]-mean)*(x[0]-mean)/(sigma*sigma));
    Double_t back = 1./(4.*sigma);
    
    Double_t fitval = amp*(backfrac*back + sigfrac*gaus);
    return fitval;
}
Double_t erfBack (Double_t *x, Double_t *par) 
{
    Double_t sigma = par[3];
    Double_t mean = par[2];
    Double_t sigfrac = par[1];
    Double_t backfrac = 1.-sigfrac;
    Double_t amp = par[0];
    Double_t blah1 = TMath::Erf(-(x[0]-0.4-mean)/sigma);
    Double_t blah2 = TMath::Erf((x[0]+0.4-mean)/sigma);
    Double_t gaus = 0.5*(blah1+blah2);
    
    Double_t fitval =amp*(backfrac + sigfrac*gaus);
    return fitval;
}

TGraphErrors** 
fitSlice (void* func,TH2D* hist, Int_t nhists) 
{
    TGraphErrors** retGraphs = new TGraphErrors*[5];
    TH1D** hists = new TH1D*[nhists];
    Int_t nbins = hist->GetNbinsX();
    Double_t* x = new Double_t[nhists];
    Double_t* ex = new Double_t[nhists];
    
    for (Int_t i=0; i< nhists; ++i) {
	Int_t low = i*(nbins/nhists)+1;//The 0th bin is underflow
	Int_t high = (i+1)*(nbins/nhists);
	Double_t xlow = hist->GetXaxis()->GetBinLowEdge(low);
	Double_t xhigh = hist->GetXaxis()->GetBinUpEdge(high);
	x[i] = (xlow + xhigh)/2.;
	ex[i] = (xhigh-xlow)/2.;
	
	Char_t* blah = new Char_t[100];
	Char_t* blah2 = hist->GetName();
	
	sprintf(blah,"%s_proj%d",blah2,i);

	hists[i] = hist->ProjectionY(blah,low,high,"E");
    }

    TF1* gausBack = new TF1("gausBack",func,-3,3,4);
    gausBack->SetParNames("2 Sigma Sum","2 Sigma Signal Fraction","Mean","Sigma");
    Double_t* amp = new Double_t[nhists];
    Double_t* sigfrac = new Double_t[nhists];
    Double_t* mean = new Double_t[nhists];
    Double_t* sigma = new Double_t[nhists];
    Double_t* chisq = new Double_t[nhists];

    Double_t* eamp = new Double_t[nhists];
    Double_t* esigfrac = new Double_t[nhists];
    Double_t* emean = new Double_t[nhists];
    Double_t* esigma = new Double_t[nhists];
    Double_t* echisq = new Double_t[nhists];
    
    for (Int_t i=0; i< nhists; ++i) {
	TH1D* histslice = hists[i];
	gausBack->SetParameters(histslice->GetMaximum()*histslice->GetRMS(),1,
				histslice->GetMean(),histslice->GetRMS());
	printf("Original parameters: %f %f %f %f",histslice->GetMaximum()*histslice->GetRMS(),
	       1,histslice->GetMean(),histslice->GetRMS());
	
	if (histslice->GetMaximum()>4.) {
	    
	    histslice->Fit("gausBack","EL");
	    amp[i] = gausBack->GetParameter(0);
	    sigfrac[i] = gausBack->GetParameter(1);
	    mean[i] = gausBack->GetParameter(2);
	    sigma[i] = gausBack->GetParameter(3);
	    chisq[i] = gausBack->GetChisquare();
	    
	    eamp[i] = gausBack->GetParError(0);
	    esigfrac[i] = gausBack->GetParError(1);
	    emean[i] = gausBack->GetParError(2);
	    esigma[i] = gausBack->GetParError(3);
	    echisq[i] = 0;
	}
	else {
	    amp[i] = 0;
	    sigfrac[i] = 0;
	    mean[i] = 0;
	    sigma[i] = 0;
	    chisq[i] = 0;
	    
	    eamp[i] = 0;
	    esigfrac[i] = 0;
	    emean[i] = 0;
	    esigma[i] = 0;
	    echisq[i] = 0;
	}
	
	    
    }
    retGraphs[0] = new TGraphErrors(nhists,x,amp,ex,eamp);
    retGraphs[1] = new TGraphErrors(nhists,x,sigfrac,ex,esigfrac);
    retGraphs[2] = new TGraphErrors(nhists,x,mean,ex,emean);
    retGraphs[3] = new TGraphErrors(nhists,x,sigma,ex,esigma);
    retGraphs[4] = new TGraphErrors(nhists,x,chisq,ex,echisq);
    retGraphs[0]->SetTitle("2 Sigma Sum");
    retGraphs[1]->SetTitle("2 Sigma Signal Fraction");
    retGraphs[2]->SetTitle("Mean");
    retGraphs[3]->SetTitle("Sigma");
    retGraphs[4]->SetTitle("Chi-squared");

    return retGraphs;
    
    
}
TGraphErrors** 
gausSlice (TH2D* hist, Int_t nhists) 
{
    
    TGraphErrors** retval = fitSlice(gausBack,hist,nhists);
    return retval;
}
TGraphErrors** 
erfSlice (TH2D* hist, Int_t nhists) 
{
    
    TGraphErrors** retval = fitSlice(erfBack,hist,nhists);
    return retval;
}

TGraphErrors** 
funcFit (void* func,TH1D* histslice) 
{
    TGraphErrors** retGraphs = new TGraphErrors*[5];
    Int_t nhists=1;
    Int_t i=1;
    
    TF1* gausBack = new TF1("gausBack",func,-3,3,4);
    gausBack->SetParNames("2 Sigma Sum","2 Sigma Signal Fraction","Mean","Sigma");
    Double_t* amp = new Double_t[nhists];
    Double_t* sigfrac = new Double_t[nhists];
    Double_t* mean = new Double_t[nhists];
    Double_t* sigma = new Double_t[nhists];
    Double_t* chisq = new Double_t[nhists];

    Double_t* eamp = new Double_t[nhists];
    Double_t* esigfrac = new Double_t[nhists];
    Double_t* emean = new Double_t[nhists];
    Double_t* esigma = new Double_t[nhists];
    Double_t* echisq = new Double_t[nhists];
    gausBack->SetParameters(histslice->GetMaximum()*histslice->GetRMS(),1,
			    histslice->GetMean(),histslice->GetRMS());
    
    printf("Original parameters: %f %f %f %f",histslice->GetMaximum(),
	   1,histslice->GetMean(),histslice->GetRMS());
    
    if (histslice->GetMaximum()) {
	
	histslice->Fit("gausBack");
	amp[i] = gausBack->GetParameter(0);
	sigfrac[i] = gausBack->GetParameter(1);
	mean[i] = gausBack->GetParameter(2);
	sigma[i] = gausBack->GetParameter(3);
	chisq[i] = gausBack->GetChisquare();
	
	eamp[i] = gausBack->GetParError(0);
	esigfrac[i] = gausBack->GetParError(1);
	emean[i] = gausBack->GetParError(2);
	esigma[i] = gausBack->GetParError(3);
	echisq[i] = 0;
    }
    else {
	amp[i] = 0;
	sigfrac[i] = 0;
	mean[i] = 0;
	sigma[i] = 0;
	chisq[i] = 0;
	
	eamp[i] = 0;
	esigfrac[i] = 0;
	emean[i] = 0;
	esigma[i] = 0;
	echisq[i] = 0;
    }
	
    return 0;
    
    

}

TGraphErrors** 
erfFit(TH1D* hist) 
{
    TGraphErrors** retval = funcFit(erfBack,hist);
    return retval;
    
}
TGraphErrors** 
gausFit(TH1D* hist) 
{
    TGraphErrors** retval = funcFit(gausBack,hist);
    return retval;
    
}





