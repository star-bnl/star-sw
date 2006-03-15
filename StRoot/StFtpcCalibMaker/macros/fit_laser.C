// $Id: fit_laser.C,v 1.2 2006/03/15 15:14:06 jcs Exp $
//
// $Log: fit_laser.C,v $
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

// fit laser mit Minuit (test)

Float_t x[5],y[5],z[5]; 

//______________________________________________________________________________

void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
{
   const Int_t nbins = 5;

   Int_t i; //calculate chisquare

   Double_t chisq = 0;
   Double_t delta;
   for (i=0;i<nbins; i++) 
     {
       //delta  = (y[i]-func(x[i],par));
       delta=(z[i]-func(x[i],y[i],par));
       chisq += delta*delta;
     }

   f = chisq;
}

//______________________________________________________________________________

Double_t func(float x,float y,Double_t *par)
//Double_t func(float x,Double_t *par)
{
  //Double_t value=( (par[0]*par[0])/(x*x)-1)/ ( par[1]+par[2]*y-par[3]*y*y);
  //Double_t value=par[0]*x+par[1];
  //value=(par[0]*x+par[1]*y+par[2])*par[3];
  Double_t value=(par[0]*x+par[1]*y+par[2]);
 //cout<<x<<" "<<y<<" "<<value<<endl;
 return value;
} 

Double_t ffunc(Double_t *x,Double_t *par)
{
  Double_t value=(par[0]*x[0]+par[1]*x[1]+par[2]);
 //cout<<x<<" "<<y<<" "<<value<<endl;
  return value;
}
//______________________________________________________________________________

void fit_laser()
{

// the x values
        x[0]=1;
        x[1]=2;
        x[2]=3;
        x[3]=4;
        x[4]=5;
// the y values
        y[0]=1;
        y[1]=1;
        y[2]=1;
        y[3]=1;
        y[4]=1;    

	z[0]=4;//1;
        z[1]=5;//2;
        z[2]=6;//3;
        z[3]=7;//4;
        z[4]=8;//5;

	TH3F *h3d=new TH3F("h3d","h3d",6,0,5,6,0,5,11,0,10);
	for (int i=0;i<5;i++)
	  h3d->Fill(x[i],y[i],z[i]);

	h3d->SetMarkerStyle(22);
	//h3d->Draw();

	TMinuit *gMinuit = new TMinuit(5);  

	//initialize TMinuit with a maximum of 5 params
	gMinuit->SetFCN(fcn);
    
	Double_t arglist[10];
	Int_t ierflg = 0;    
	arglist[0] = 1;

	gMinuit->mnexcm("SET ERR", arglist ,1,ierflg); 

	// Set starting values and step sizes for parameters
	//static Double_t vstart[4] = {3, 1 , 0.1 , 0.01}; //org
	static Double_t vstart[3] = {1,1,1};
	static Double_t step[3] = {0.01,0.01,0.01};
	gMinuit->mnparm(0, "a1", vstart[0], step[0], 0,0,ierflg);
	gMinuit->mnparm(1, "a2", vstart[1], step[1], 0,0,ierflg);
	gMinuit->mnparm(2, "a3", vstart[2], step[2], 0,0,ierflg);
	//gMinuit->mnparm(3, "a4", vstart[3], step[3], 0,0,ierflg);
	// Now ready for minimization step
	arglist[0] = 1000;
	arglist[1] = 1.;

	gMinuit->mnexcm("MIGRAD", arglist ,2,ierflg); 

	// Print results
	Double_t amin,edm,errdef;
	Int_t nvpar,nparx,icstat;
	gMinuit->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
	gMinuit->mnprin(3,amin); 

	//TGraph *test=new TGraph(5,x,y);
	//test->Draw("PA");

	TF2 *ffunc=new TF2("ffunc",ffunc,0,5,0,5,3);
	//cout<<func(1,1,arglist)<<endl;

	//for (int i=0;i<3;i++)
	//cout<<gMinuit->GetNumFreePars()<<endl;
	//cout<<gMinuit->GetNumFixedPars()<<endl;

	Double_t v,ev;
	for (int i=0;i<3;i++)
	  {
	    gMinuit->GetParameter(i,v,ev);
	    ffunc->SetParameter(i,v);
	    cout<<v<<" "<<ev<<endl;
	    cout<<ffunc->GetParameter(i)<<endl;
	  }
	

	ffunc->DrawCopy("lego");
	h3d->DrawCopy("same");
	//cout<<ffunc->GetZmax()<<endl;
	//cout<<amin[1]<<endl;
	//TGraph *mcont=(TGraph*) gMinuit->Contour();
	//mcont->Draw("APcolz");
}

