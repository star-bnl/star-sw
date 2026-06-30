#include "StFwdAnaData.h"

//@[January 4, 2025] > Information about declaring static members in a class in no particular order
//    - https://stackoverflow.com/questions/27408819/why-cant-non-static-data-members-be-constexpr
//    - https://stackoverflow.com/questions/9141950/initializing-const-member-within-class-declaration-in-c
//    - https://cplusplus.com/forum/beginner/229416/
//    - https://www.reddit.com/r/cpp_questions/comments/1ap7sx1/const_and_constexpr_detailed/
//    - https://stackoverflow.com/questions/35927878/assign-static-constexpr-class-member-to-runtime-variable
//  > Information about anonymous namespaces vs. static
//    - https://stackoverflow.com/questions/357404/why-are-unnamed-namespaces-used-and-what-are-their-benefits
//      -> https://web.archive.org/web/20181115023158/http://www.comeaucomputing.com/techtalk/#nostatic
//    - https://stackoverflow.com/questions/154469/why-should-you-prefer-unnamed-namespaces-over-static-functions
//    - https://www.reddit.com/r/cpp/comments/dwcdm6/unnamed_anonymous_namespaces_vs_static_usage_in/
//      -> https://medium.com/pranayaggarwal25/unnamed-namespaces-static-f1498741c527
//    - https://cplusplus.com/forum/general/133038/
//      -> https://stackoverflow.com/questions/154469/why-should-you-prefer-unnamed-namespaces-over-static-functions

//const double StFwdAnaData::xfbins[] = {0, 0.04, 0.08, 0.12, 0.16, 0.2, 0.24, 0.28, 0.32, 0.36, 0.4, 0.44, 0.48, 0.52};
const Double_t StFwdAnaData::xfbins[] = {0, 0.04, 0.08, 0.12, 0.16, 0.2, 0.24, 0.3, 0.5}; //@[August 4, 2025] > New xF binning

void PolData::Print() const
{
  std::cout << " * |fill:"<<mFillNum
	    << "|En:"<<mBeamEn
	    << "|StartTime:"<<mStartTime
	    << "|BP0:"<<mBlueP0
	    << "|BErrP0:"<<mBlueErrP0
	    << "|BdPdT:"<<mBluedPdT
	    << "|BErrdPdT:"<<mBlueErrdPdT
    	    << "|YP0:"<<mYellowP0
	    << "|YErrP0:"<<mYellowErrP0
	    << "|YdPdT:"<<mYellowdPdT
	    << "|YErrdPdT:"<<mYellowErrdPdT
	    << std::endl;
}

StFwdAnaData::TrigRange::TrigRange()
{
  mName = "";
  mOfflineTrigId = 0;
  mStartRun = 0;
  mEndRun = 0;
  mPtThreshold = 0;
  mPtThresholdAsym = -1;
}

StFwdAnaData::TrigRange::TrigRange(const char* name, Int_t trigid, Int_t startrun, Int_t endrun)
{
  mName = name;
  mOfflineTrigId = trigid;
  mStartRun = startrun;
  mEndRun = endrun;
  mPtThreshold = 0;
  mPtThresholdAsym = -1;
}

bool StFwdAnaData::TrigRange::ValidRun(Int_t runnum) const
{
  if( mStartRun<=runnum && runnum<=mEndRun ){ return true; }
  else{ return false; }
}


ClassImp(StFwdAnaData)

StFwdAnaData::StFwdAnaData()
{
  mSpinRndm.SetSeed(0);
}

StFwdAnaData::~StFwdAnaData()
{
  delete mEvtData;
  delete mPhArr;
  delete mPhPairArr;
  delete mDataTree;
  for( auto itr=mPolarizationData.begin(); itr!=mPolarizationData.end(); ++itr){ delete itr->second; }
  mPolarizationData.clear();
}

std::vector<Double_t> StFwdAnaData::ProjectToEpd(Double_t xfcs, Double_t yfcs, Double_t zfcs, Double_t zvertex)
{
  //Assume x,y=0 at vertex so only need zvertex as origin and is the initial point for the direction
  Double_t linedirection[3] = {xfcs,yfcs,zfcs-zvertex}; //This is the direction vector from the origin to the fcs point (FcsXYZ-Vertex)
  Double_t EpdZ = 375.0;   //Need a point on epd plane so picking x,y=0 is valid and formula below reflects this. Also only care about West EPD which is along positive z-axis
  //Tiles are parallel to z-axis so normal vector is {0,0,1}. The formula below reflects this
  //Solution of intersection of line and plane where line has direction {xdir,ydir,zdir}*t and starts at {0,0,zvertex} and a plane with a normal that points along the z-axis and has a point on the plane at {0,0,EpdZ}; "t" is the free parameter in the parametric equation of the line.
  double tintersection = (EpdZ-zvertex) / (linedirection[2]);
  std::vector<Double_t> intersection;
  intersection.emplace_back(linedirection[0]*tintersection);
  intersection.emplace_back(linedirection[1]*tintersection);
  intersection.emplace_back(linedirection[2]*tintersection+zvertex);
  return intersection;
}

void StFwdAnaData::AddHistStatsOneline( TLegend* HistLeg, const TH1* h1, const std::string &title )
{
  //This function is good for when many histograms are plotted
  if( HistLeg==0 || h1==0 ){return;}
  if( h1->GetDimension()==1 ){
    std::stringstream ss_entry;
    if( title.size()==0 ){ ss_entry << h1->GetName(); }
    else{ ss_entry << title; }
    ss_entry << "|E:"<< h1->GetEntries();
    ss_entry << "|M:"<< h1->GetMean();
    ss_entry << "|R:"<< h1->GetRMS();
    //ss_entry << "|U:"<< h1->GetBinContent(0);
    //ss_entry << "|O:"<< h1->GetBinContent(h1->GetNbinsX()+1);
    HistLeg->AddEntry(h1, ss_entry.str().c_str(),"fle" );
  }
}

Int_t StFwdAnaData::MakeGraph(TFile* file, TObjArray* grapharr, TGraph*& graph, const char* name, const char* title )
{
  if( graph!=0 ){ return 0; }  //Graph pointer should always be zero
  if( file!=0 ){
    graph = (TGraph*)file->Get(name);
    grapharr->Add(graph); //add graphs from file too
  }
  if( graph==0 ){
    TGraph* gexists = (TGraph*)grapharr->FindObject(name);
    if( gexists==0 ){
      graph = new TGraph();
      grapharr->Add(graph); //Add new graphs to array
    }
    else{ graph = gexists; }
  }
  graph->SetNameTitle(name,title);
  return 1;//1 if histogram loaded or exists, 0 otherwise
}

Int_t StFwdAnaData::MakeGraph(TFile* file, TObjArray* grapharr, TGraphErrors*& graph, const char* name, const char* title )
{
  if( graph!=0 ){ return 0; }  //Graph pointer should always be zero
  if( file!=0 ){ graph = (TGraphErrors*)file->Get(name); }
  if( graph==0 ){
    TGraphErrors* gexists = (TGraphErrors*)grapharr->FindObject(name);
    if( gexists==0 ){
      graph = new TGraphErrors();
    }
    else{ graph = gexists; }
  }
  graph->SetNameTitle(name,title);
  grapharr->Add(graph);
  return 1;//1 if histogram loaded or exists, 0 otherwise
}


void StFwdAnaData::DoTssaAna( TH1* npi0[][2], TH1* h1_rawasymVphi[][StFwdAnaData::NXFBIN] )
{
  //Double_t pi = TMath::Pi();
  //TH1* h1_rawasymVphi[2][NENERGYBIN];  //Histograms of computed raw asymmetries for blue and yellow beams for each energy bin
  //memset(h1_rawasymVphi,0,sizeof(h1_rawasymVphi));
  
  //for( int ibeam = 0; ibeam<2; ++ibeam ){
  for( int ixbin = 0; ixbin<StFwdAnaData::NXFBIN; ++ixbin ){
    int ibeam = 0; //Blue beam first since that uses the normal STAR direction convention
    for( int iphibin=0; iphibin<StFwdAnaData::NPHIBIN/2 ; ++iphibin ){
      Double_t nupatphi         = npi0[ibeam][0]->GetBinContent(npi0[ibeam][0]->GetBin(iphibin+1,ixbin+1));
      Double_t ndownatphipluspi = npi0[ibeam][1]->GetBinContent(npi0[ibeam][1]->GetBin(iphibin+1+StFwdAnaData::NPHIBIN/2,ixbin+1));
      Double_t ndownatphi       = npi0[ibeam][1]->GetBinContent(npi0[ibeam][1]->GetBin(iphibin+1,ixbin+1));
      Double_t nupatphipluspi   = npi0[ibeam][0]->GetBinContent(npi0[ibeam][0]->GetBin(iphibin+1+StFwdAnaData::NPHIBIN/2,ixbin+1));
      
      Double_t nupdown = sqrt( nupatphi * ndownatphipluspi );
      Double_t ndownup = sqrt( ndownatphi * nupatphipluspi );
      Double_t numerator = nupdown - ndownup;
      Double_t denominator = nupdown + ndownup;
      //std::cout << "|iphibin:"<<iphibin << "|iebin:"<<iebin << "|ibeam:"<<0 << std::endl;
      //std::cout << " + |nu0:"<< nupatphi << "|ndp:"<<ndownatphipluspi << "|nd0:"<<ndownatphi << "|nup:"<<nupatphipluspi << "|numer:"<<numerator << "|denom:"<<denominator << std::endl;
	
      if( denominator != 0 ){
	// Propagate errors for A_N
	Double_t AnErr = sqrt(ndownatphi*nupatphi*nupatphipluspi + ndownatphipluspi*nupatphi*nupatphipluspi + ndownatphi*ndownatphipluspi*nupatphi + ndownatphi*ndownatphipluspi*nupatphipluspi)/(denominator*denominator);
	//std::cout << " + |An:"<<numerator/denominator<< "|nu0_err:"<<nupatphi_err << "|ndp_err:"<<ndownatphipluspi_err << "|nd0_err:"<<ndownatphi_err << "|nup_err:"<< nupatphipluspi_err << "|AnErr:"<<AnErr << std::endl;
	  
	h1_rawasymVphi[ibeam][ixbin]->SetBinContent(iphibin+1,numerator/denominator);
	h1_rawasymVphi[ibeam][ixbin]->SetBinError(iphibin+1,AnErr);
      }
      else{
	//If denomnator is zero set bin to zero to avoid bad division and set large error
	h1_rawasymVphi[ibeam][ixbin]->SetBinContent(iphibin+1,0);
	h1_rawasymVphi[ibeam][ixbin]->SetBinError(iphibin+1,1);
      }
    }
    ibeam = 1; //Yellow beam next since need to flip the order because "left" for yellow is the opposite of the "left" for blue
    for( int iphibin=StFwdAnaData::NPHIBIN/2; iphibin<StFwdAnaData::NPHIBIN; ++iphibin ){
      Double_t nupatphi         = npi0[ibeam][0]->GetBinContent(npi0[ibeam][0]->GetBin(iphibin+1,ixbin+1));
      Double_t ndownatphipluspi = npi0[ibeam][1]->GetBinContent(npi0[ibeam][1]->GetBin(iphibin+1-StFwdAnaData::NPHIBIN/2,ixbin+1));
      Double_t ndownatphi       = npi0[ibeam][1]->GetBinContent(npi0[ibeam][1]->GetBin(iphibin+1,ixbin+1));
      Double_t nupatphipluspi   = npi0[ibeam][0]->GetBinContent(npi0[ibeam][0]->GetBin(iphibin+1-StFwdAnaData::NPHIBIN/2,ixbin+1));
      
      Double_t nupdown = sqrt( nupatphi * ndownatphipluspi );
      Double_t ndownup = sqrt( ndownatphi * nupatphipluspi );
      Double_t numerator = nupdown - ndownup;
      Double_t denominator = nupdown + ndownup;
      //std::cout << "|iphibin:"<<iphibin << "|iebin:"<<iebin << "|ibeam:"<<1 << std::endl;
      //std::cout << " + |nu0:"<< nupatphi << "|ndp:"<<ndownatphipluspi << "|nd0:"<<ndownatphi << "|nup:"<<nupatphipluspi << "|numer:"<<numerator << "|denom:"<<denominator << std::endl;
	
      if( denominator != 0 ){
	// Propagate errors for A_N
	Double_t AnErr = sqrt(ndownatphi*nupatphi*nupatphipluspi + ndownatphipluspi*nupatphi*nupatphipluspi + ndownatphi*ndownatphipluspi*nupatphi + ndownatphi*ndownatphipluspi*nupatphipluspi)/(denominator*denominator);
	//std::cout << " + |An:"<<numerator/denominator << "|AnErr:"<<AnErr << std::endl;
	h1_rawasymVphi[ibeam][ixbin]->SetBinContent(iphibin+1-StFwdAnaData::NPHIBIN/2,numerator/denominator); //instead of flipping can also multiply by -1
	h1_rawasymVphi[ibeam][ixbin]->SetBinError(iphibin+1-StFwdAnaData::NPHIBIN/2,AnErr);
      }
      else{
	//If denomnator is zero set bin to zero to avoid bad division and set large error
	h1_rawasymVphi[ibeam][ixbin]->SetBinContent(iphibin+1-StFwdAnaData::NPHIBIN/2,0);
	h1_rawasymVphi[ibeam][ixbin]->SetBinError(iphibin+1-StFwdAnaData::NPHIBIN/2,1);
      }
    }
  }
}

void StFwdAnaData::DoTssaFit( TH1* h1_rawasymVphi[][StFwdAnaData::NXFBIN], TH1* h1_bluepoldata, TH1* h1_yellowpoldata, TH1* h1_AnResult[] )
{
  Double_t pi = TMath::Pi();
  //Double_t ebins[NENERGYBIN+1] = {0, 15, 20, 25, 30, 40, 55, 70, 100};  //Copied from above
  //h1_AnResult[0] = new TH1D("H1D_AnResultBlue","A_N vs. Energy;Energy;A_N",StFwdAnaData::NENERGYBIN,ebins);
  //h1_AnResult[1] = new TH1D("H1D_AnResultYellow","A_N vs. Energy;Energy;A_N",StFwdAnaData::NENERGYBIN,ebins);
  //Double_t nentries = h1_poldata->GetBinContent(1);
  for( int ibeam = 0; ibeam<2; ++ibeam ){
    for( int ixbin = 0; ixbin<StFwdAnaData::NXFBIN; ++ixbin ){
      //std::stringstream ss_fname;
      //ss_fname << "AnFit_" << ibeam << "_" << iebin;
      //TF1* AnFit = new TF1("AnFit","[0]*cos(x+[1])+[2]",-pi/2.0,pi/2.0);
      TF1* AnFit = new TF1("AnFit","[0]*cos(x+[1])",-pi/2.0,pi/2.0);
      AnFit->SetParName(0,"A");
      AnFit->SetParName(1,"#phi_{0}");
      //AnFit->SetParName(2,"yoff");
      h1_rawasymVphi[ibeam][ixbin]->Fit(AnFit,"R");
      Double_t rawasym = AnFit->GetParameter(0);
      //Double_t totalpolpercent = h1_poldata->GetBinContent(ibeam+2);  //Since blue beam [0] was stored in bin 2 and yellow beam [1] was stored in bin 3
      //Double_t lumcorrectedpol = totalpolpercent/nentries;            //luminosity corrected polarization
      Double_t lumcorrectedpol = (ibeam==0)?(h1_bluepoldata->GetMean()/100.0):(h1_yellowpoldata->GetMean()/100.0); //Divide by 100 since value is a percent
      h1_AnResult[ibeam]->SetBinContent( ixbin+1, rawasym/lumcorrectedpol );
      h1_AnResult[ibeam]->SetBinError( ixbin+1, AnFit->GetParError(0)/lumcorrectedpol );
    }
  }
}

void StFwdAnaData::DoPi0Fits(TH3* mH3F_invmass, TH1* hist_proj[] )
{
  for( short ixbin=0; ixbin<StFwdAnaData::NXFBIN; ++ixbin ){
    if( hist_proj[ixbin]==0 ){
      std::stringstream histname;
      histname << "H1F_InvMass_xf"<<ixbin;
      hist_proj[ixbin] = (TH1*)((TH3F*)mH3F_invmass)->ProjectionZ( histname.str().c_str(), 1,StFwdAnaData::NPHIBIN, ixbin+1,ixbin+1 );
      hist_proj[ixbin]->SetTitle( histname.str().c_str() );
    }
    TF1* PeakFit = new TF1("PeakFit",skewgaus,0.1,0.2,4);
    PeakFit->SetParameter(0,hist_proj[ixbin]->GetBinContent(hist_proj[ixbin]->GetMaximumBin()));
    PeakFit->SetParameter(1,0.135);
    PeakFit->SetParameter(2,0.03);
    PeakFit->SetParameter(3,0);
    //PeakFit->SetParLimits(3,0,10); //Force positive skew
    //PeakFit->FixParameter(3,0); //debuging for gaussian
    //TF1* Bg1Fit = new TF1("Bg1Fit","[0] +[1]*x +[2]*x*x +[3]*x*x*x + [4]*x*x*x*x",0.3,0.4);
    //TF1* Bg1Fit = new TF1("Bg1Fit","1 +[0]*x +[1]*(2*x*x-1) +[2]*(4*x*x*x-3*x)",0.3,0.4); //Fit to Chebyshev
    //TF1* Bg1Fit = new TF1("Bg1Fit","[0] +[1]*x*x +[2]*x*x*x*x",0.3,0.4); //Fit to even
    //TF1* Bg1Fit = new TF1("Bg1Fit","[0] +[1]*x +[2]*x*x*x",0.3,0.4);     //Fit to odd
    TF1* Bg1Fit = new TF1("Bg1Fit",pol4bg,0,0.9,5);
    TF1* Bg2Fit = new TF1("Bg2Fit","[0] +[1]*x +[2]*x*x +[3]*x*x*x + [4]*x*x*x*x",0.7,0.9);
    hist_proj[ixbin]->Fit(PeakFit,"QR");
    hist_proj[ixbin]->Fit(Bg1Fit,"QR+");
    hist_proj[ixbin]->Fit(Bg2Fit,"QR+");
    //TF1* GlobalFit = new TF1("GlobalFit","[0] +[1]*x +[2]*x*x +[3]*x*x*x +[4]*x*x*x*x + gaus(5)",0,0.45);
    //TF1* GlobalFit = new TF1("GlobalFit","1 +[0]*x +[1]*(2*x*x-1) +[2]*(4*x*x*x-3*x) + gaus(3)",0,0.4);
    //TF1* GlobalFit = new TF1("GlobalFit",pol4skewgaus,0,0.45,9);
    TF1* GlobalFit = new TF1("GlobalFit",pol4skewgaus,0.0,1.0,9);
    GlobalFit->SetParameter(0,Bg1Fit->GetParameter(0));
    GlobalFit->SetParameter(1,Bg1Fit->GetParameter(1));
    GlobalFit->SetParameter(2,Bg1Fit->GetParameter(2));
    GlobalFit->SetParameter(3,Bg1Fit->GetParameter(3));
    GlobalFit->SetParameter(4,Bg1Fit->GetParameter(4));
    //GlobalFit->SetParameter(5,PeakFit->GetParameter(0)); //DOn't use from first fit
    GlobalFit->SetParameter(5,hist_proj[ixbin]->GetBinContent(hist_proj[ixbin]->GetMaximumBin()));
    GlobalFit->SetParName(5,"amplitude");
    //GlobalFit->SetParameter(6,PeakFit->GetParameter(1));
    GlobalFit->SetParameter(6,0.135);
    GlobalFit->SetParName(6,"mean");
    GlobalFit->SetParameter(7,0.03);
    GlobalFit->SetParName(7,"sigma");
    GlobalFit->SetParameter(8,0);  //Fix the skew parameter in global fit since skew should only be affected by peak region
    GlobalFit->SetParName(8,"skew");
    //GlobalFit->SetParameter(3,PeakFit->GetParameter(0));
    //GlobalFit->SetParameter(4,PeakFit->GetParameter(1));
    //GlobalFit->SetParameter(5,PeakFit->GetParameter(2));
    hist_proj[ixbin]->Fit(GlobalFit,"R+");
    TF1* BgFit = new TF1("BgFit","[0] +[1]*x +[2]*x*x +[3]*x*x*x +[4]*x*x*x*x", 0,1.0);
    //TF1* BgFit = new TF1("BgFit","1 +[0]*x +[1]*(2*x*x-1) +[2]*(4*x*x*x-3*x)",0,0.5); //Fit to Chebyshev
    BgFit->FixParameter(0,Bg1Fit->GetParameter(0));
    BgFit->FixParameter(1,Bg1Fit->GetParameter(1));
    BgFit->FixParameter(2,Bg1Fit->GetParameter(2));
    BgFit->FixParameter(3,Bg1Fit->GetParameter(3));
    BgFit->FixParameter(4,Bg1Fit->GetParameter(4));
    hist_proj[ixbin]->Fit(BgFit,"RQ+");
    TF1* BgGlobalFit = new TF1("BgGlobalFit","[0] +[1]*x +[2]*x*x +[3]*x*x*x +[4]*x*x*x*x", 0,1.0);
    //TF1* BgFit = new TF1("BgGlobalFit","1 +[0]*x +[1]*(2*x*x-1) +[2]*(4*x*x*x-3*x)",0,0.5); //Fit to Chebyshev
    BgGlobalFit->FixParameter(0,GlobalFit->GetParameter(0));
    BgGlobalFit->FixParameter(1,GlobalFit->GetParameter(1));
    BgGlobalFit->FixParameter(2,GlobalFit->GetParameter(2));
    BgGlobalFit->FixParameter(3,GlobalFit->GetParameter(3));
    BgGlobalFit->FixParameter(4,GlobalFit->GetParameter(4));
    hist_proj[ixbin]->Fit(BgGlobalFit,"RQ+");
  }
}

void StFwdAnaData::DoBgCorrectedAn(TH1* h1_invmass_xf[], TH1* h1_an_inc, TH1* h1_an_bg, TH1* h1_anresult )
{
  if( h1_anresult==0 ){ return; }
  if( h1_invmass_xf==0 ){ return; }
  if( h1_an_inc==0 ){ return; }
  if( h1_an_bg==0 ){ return; }
  for( Int_t ixbin=0; ixbin<StFwdAnaData::NXFBIN; ++ixbin ){
    TF1* bgfunc = 0;
    if( ixbin<=(StFwdAnaData::NXFBIN-1) ){ bgfunc = h1_invmass_xf[ixbin]->GetFunction("BgGlobalFit"); }
    //if( ixbin<4 ){ bgfunc = h1_invmass_en[ixbin]->GetFunction("BgGlobalFit"); }
    else{ bgfunc = h1_invmass_xf[ixbin]->GetFunction("Bg2Fit"); }  //For all but the last energy bin this function had the most reasonable background shape(*/
    if( bgfunc==0 ){ continue; }
    Double_t xlowbin = h1_invmass_xf[ixbin]->FindBin(0.1);
    Double_t xhighbin = h1_invmass_xf[ixbin]->FindBin(0.2);
    Double_t npi0_inc = h1_invmass_xf[ixbin]->Integral(xlowbin,xhighbin,"width");
    Double_t npi0_bg = bgfunc->Integral(0.1,0.2);
    Double_t ratio = npi0_bg/npi0_inc;
    Double_t ansignal = (h1_an_inc->GetBinContent(ixbin+1) - ratio*h1_an_bg->GetBinContent(ixbin+1)) / (1.0-ratio);
    Double_t aninc_err = h1_an_inc->GetBinError(ixbin+1);
    Double_t anbg_err = h1_an_bg->GetBinError(ixbin+1);
    Double_t ansig_err = (1.0/(1.0-ratio))*sqrt(aninc_err*aninc_err + ratio*ratio*anbg_err*anbg_err);
    h1_anresult->SetBinContent(ixbin+1, ansignal);
    h1_anresult->SetBinError(ixbin+1, ansig_err );
  }
}

Double_t StFwdAnaData::pol4bg(Double_t* x, Double_t* par)
{
  //Rejecting these points gives good background at both the high and low end
  if( x[0]<0.045 ){ TF1::RejectPoint(); return 0; }
  if( 0.09<x[0] && x[0]<0.21 ){ TF1::RejectPoint(); return 0; }
  if( 0.4<x[0] && x[0]<0.6 ){ TF1::RejectPoint(); return 0; }
  return par[0] + par[1]*x[0] + par[2]*x[0]*x[0] + par[3]*x[0]*x[0]*x[0] + par[4]*x[0]*x[0]*x[0]*x[0];
}

Double_t StFwdAnaData::skewgaus(Double_t*x, Double_t* par)
{
  if( par[2]==0 ){ return 1.e30; }
  Double_t xarg = (x[0]-par[1])/par[2];
  Double_t gaus = (1.0/(TMath::Sqrt(2.0*TMath::Pi())))*TMath::Exp(-0.5*xarg*xarg);
  Double_t skew = 0.5*(1.0+TMath::Erf( (par[3]*xarg)/TMath::Sqrt2() ));
  return (2.0/par[2])*par[0]*gaus*skew;
}

Double_t StFwdAnaData::pol4skewgaus(Double_t*x, Double_t* par)
{
  //if( x[0]<0.045 ){ TF1::RejectPoint(); return 0; }
  if( x[0]<0.045 || (0.4<x[0] && x[0]<0.6) ){ TF1::RejectPoint(); return 0; } //Avoid low mass region and 0.4 to 0.6 to skip eta meson mass
  Double_t pol4 = par[0] + par[1]*x[0] + par[2]*x[0]*x[0] + par[3]*x[0]*x[0]*x[0] + par[4]*x[0]*x[0]*x[0]*x[0];
  if( par[7]==0 ){ return 1.e30; }
  Double_t xarg = (x[0]-par[6])/par[7];
  Double_t gaus = (1.0/(TMath::Sqrt(2.0*TMath::Pi())))*TMath::Exp(-0.5*xarg*xarg);
  Double_t skew = 0.5*(1.0+TMath::Erf( (par[8]*xarg)/TMath::Sqrt2() ));
  return pol4 + (2.0/par[7])*par[5]*gaus*skew;
}

Int_t StFwdAnaData::GetColor(Double_t Value, Double_t MinVal, Double_t MaxVal)
{
  Double_t MinHue = 275.0;
  Double_t MaxHue = 0.0;
  Float_t Red,Green,Blue;
  if( Value < MinVal ){ TColor::HSV2RGB(MinHue,1.0,1.0, Red,Green,Blue); }
  else if( Value > MaxVal ){ TColor::HSV2RGB(MaxHue,1.0,1.0, Red,Green,Blue); }
  else
    {
      Double_t percent = (Value-MinVal)/(MaxVal-MinVal);
      Float_t Hue = ((MaxHue-MinHue)*percent)+MinHue;
      TColor::HSV2RGB(Hue,1.0,1.0, Red,Green,Blue);
    }
    return TColor::GetColor( Red, Green, Blue );
}

void StFwdAnaData::epdColl(TClonesArray*& epdmucoll, StEpdCollection*& epdcoll)
{
  epdmucoll = mMuEpdHits;
  epdcoll = mEpdColl;
  return;
}

void StFwdAnaData::resetEvent()
{  
  //Reset these mutable pointers set in make so that it can be easily checked which ones are modified on the next event call
  mMuDstMkr = 0;
  mMuDst = 0;
  mMuEvent = 0;
  mTrigData = 0;
  mRunInfo = 0;
  mMuFcsColl = 0;
  
  mMuEpdHits = 0;
  mEpdColl = 0;
  
  mValidTrigFound = false;
  mEmTrigFound = false;
  mTrigEm0 = -1;
  mTrigEm1 = -1;
  mTrigEm2 = -1;
  mTrigEm3 = -1;

  mEvtData->Clear();
  mPhArr->Clear("C");
  mPhPairArr->Clear("C");
}

void StFwdAnaData::setEventBit(bool val)
{
  if( val ){ mTreeOnBitMap |= 0x01; }
  else{ mTreeOnBitMap &= ~(0x01); }
}

void StFwdAnaData::setPhotonOn(bool val)
{
  if( val ){ mTreeOnBitMap |= 0x02; }
  else{ mTreeOnBitMap &= ~(0x02); }
}

void StFwdAnaData::setPhPairOn(bool val)
{
  if( val ){ mTreeOnBitMap |= 0x04; }
  else{ mTreeOnBitMap &= ~(0x04); }
}

/*#ifndef __CINT__
void StFwdAnaData::SetTrigs(const char* trigname,...)
{
  va_list args;
  va_start(args,trigname);
  
  char* name = va_arg(args,char*);
  mTargetTrig.emplace_back(name);

  va_end(args);
}
#endif*/

bool StFwdAnaData::isEventOn() const
{
  if( mTreeOnBitMap & 0x01 ){ return true; }
  else{ return false; }
}

bool StFwdAnaData::isPhotonOn() const
{
  if( mTreeOnBitMap & 0x02 ){ return true; }
  else{ return false; }
}

bool StFwdAnaData::isPhPairOn() const
{
  if( mTreeOnBitMap & 0x04 ){ return true; }
  else{ return false; }
}

PolData* StFwdAnaData::getPolData(Int_t fillnum)
{
  PolData* poldat = 0;
  auto itr = mPolarizationData.find(fillnum);
  if( itr != mPolarizationData.end() ){
    poldat = itr->second;
  }
  return poldat;
}

bool StFwdAnaData::exceedTrigPt(Double_t checkpt)
{
  bool exceedtrigpt = false;
  //Float_t trigptthr = -1;
  //std::string trigname = "";
  Int_t ntrig = getNtrig();
  for( Int_t itrig=0; itrig<ntrig; ++itrig ){
    Float_t ptthres = fcsPtThr(getTrig(itrig));
    std::string thistrig = fcsTrigNameFromId(getTrig(itrig),mMuEvent->runNumber());
    if( checkpt>=ptthres ){ exceedtrigpt=true; break; /*trigptthr=ptthres; trigname=thistrig;*/ }
  }
  return exceedtrigpt;
}

void StFwdAnaData::loadTree(TFile* file)
{
  //mFile_Output = file;
  if( file==0 ){ std::cout << "LoadDataFromFile - WARNING:Null file given" << std::endl; return; }
  //if( tree!=0 ){ std::cout << "LoadDataFromFile - WARNING:Overwriting TTree pointer" << std::endl; }
  if( mDataTree!=0 ){ std::cout << "LoadDataFromFile - WARNING:Internal TTree pointer not zero must have been intialized elsewhere\n -> Deleting old data for new" << std::endl; delete mDataTree; mDataTree=0; }
  mDataTree = (TTree*)file->Get("DataTree");
  //tree = mDataTree;
  if( mDataTree!=0 ){
    //Set event branches
    if( mEvtData!=0 ){ std::cout << "LoadDataFromFile - WARNING:Internal #StFwdDataEvent not zero must have been intialized elsewhere\n -> Deleting old data for new" << std::endl; delete mEvtData; mEvtData=0; }
    if( isEventOn() ){
      if( mDataTree->Branch("EventInfo")!=0 && mDataTree->Branch("TriggerInfo")!=0 ){
	mEvtData     = new StFwdDataEvent();
	mDataTree->SetBranchAddress("EventInfo",&mEvtData);
	//Set trigger branches
	((TLeaf*)mDataTree->GetBranch("TriggerInfo")->GetListOfLeaves()->At(0))->SetAddress(&(mEvtData->mNTrig));
	((TLeaf*)mDataTree->GetBranch("TriggerInfo")->GetListOfLeaves()->At(1))->SetAddress(&(mEvtData->mTriggers));
      }
      else{ std::cout << "LoadDataFromFile - WARNING:No \"EventInfo\" and no \"TriggerInfo\" branch found in mDataTree it could be that the tree was generated without this option." << std::endl;
      }
    }
    
    if( mPhArr!=0 ){ std::cout << "LoadDataFromFile - WARNING:Internal #StFcsPhotonCandidate array not zero must have been intialized elsewhere\n -> Deleting old data for new" << std::endl; delete mPhArr; mPhArr=0; }
    if( isPhotonOn() ){
      if( mDataTree->Branch("Photon")!=0 ){
	mPhArr       = new TClonesArray("StFcsPhotonCandidate");
	mDataTree->SetBranchAddress("Photon",&mPhArr);
      }
      else{
	std::cout << "LoadDataFromFile - WARNING:No \"Photon\" branch found in mDataTree it could be that the tree was generated without this option." << std::endl;
      }
    }
    
    if( mPhPairArr!=0 ){ std::cout << "LoadDataFromFile - WARNING:Internal #StFcsPairCandidate array not zero must have been intialized elsewhere\n -> Deleting old data for new" << std::endl; delete mPhPairArr; mPhPairArr=0; }
    if( isPhPairOn() ){
      if( mDataTree->Branch("Pair")!=0 ){
	mPhPairArr      = new TClonesArray("StFcsPairCandidate");  
	mDataTree->SetBranchAddress("Pair",&mPhPairArr);
      }
      else{ std::cout << "LoadDataFromFile - WARNING:No \"Pair\" branch found in mDataTree it could be that the tree was generated without this option." << std::endl; }
    }
  }
  //else{ std::cout << "LoadDataFromFile - WARNING:Pi0Tree not found in file" << std::endl; }
}

void StFwdAnaData::makeTree(TFile* file)
{
  //mFile_Output = file;
  if( file==0 ){
    std::cout << "StFwdAnaData::makeTree has no ROOT file" << std::endl;
    return;
  }
  file->cd(); //File expected to be nonzero here if everything initialized correctly
  if( mTreeOnBitMap!=0 ){
    mDataTree     = new TTree("DataTree","Tree with StFcsPairCandidate");
  }
  //These are still needed in Make so even if you are not writing the tree still need these objects
  mEvtData     = new StFwdDataEvent();
  mPhArr       = new TClonesArray("StFcsPhotonCandidate");
  mPhPairArr      = new TClonesArray("StFcsPairCandidate");

  if( mTreeOnBitMap!=0 ){
    if( isEventOn() ){
      mDataTree->Branch("EventInfo","StFwdDataEvent",&mEvtData);
      mDataTree->Branch("TriggerInfo",0,"NTrig/I:Trig[NTrig]/I");
      ((TLeaf*)mDataTree->GetBranch("TriggerInfo")->GetListOfLeaves()->At(0))->SetAddress(&(mEvtData->mNTrig));
      ((TLeaf*)mDataTree->GetBranch("TriggerInfo")->GetListOfLeaves()->At(1))->SetAddress(&(mEvtData->mTriggers));
    }
    if( isPhotonOn() ){ mDataTree->Branch("Photon",&mPhArr); }
    if( isPhPairOn() ){ mDataTree->Branch("Pair",&mPhPairArr); }
  }
}


void StFwdAnaData::Print(Option_t* opt) const
{
  TString option(opt);
  option.ToLower();
  if( option.Contains("a") ){ option = "etgp"; }
  if( mEvtData!=0 && option.Contains("e") ){ mEvtData->Print(); }
  if( option.Contains("t") ){
    Int_t ntrig = getNTrig();
    std::cout << "## Trigger Information|NTrig:"<<ntrig << std::endl;
    for( int i=0; i<ntrig; ++i ){
      std::cout << " + |TrigId:"<<getTrig(i);
      if( mEvtData!=0 ){ std::cout << "|TrigName:"<< fcsTrigNameFromId(getTrig(i),mEvtData->mRunNum); }
      std::cout << std::endl;
    }
  }
  if( option.Contains("g") ){
    std::cout << "## Photon Information|Size:"<<mPhArr->GetEntriesFast() << std::endl;
    for( int i=0; i<mPhArr->GetEntriesFast(); ++i ){
      std::cout << " + ";
      mPhArr->At(i)->Print();
    }
  }
  if( option.Contains("p") ){
    std::cout << "## PhPair Information|Size:"<<mPhPairArr->GetEntriesFast() << std::endl;
    for( int i=0; i<mPhPairArr->GetEntriesFast(); ++i ){
      std::cout << " + ";
      mPhPairArr->At(i)->Print();
    }
  }
}

 
Int_t StFwdAnaData::ReadPolFile(const char* filename)
{
  std::ifstream in_polfile(filename);       //input (in_) polarization (pol) file
  if( !in_polfile.is_open() ){ return 0; }
  Int_t fillnum = 0;
  Int_t value = 0;
  Double_t fvalue = 0;
  //[Don't use eof in while loop since bit only gets set after reading file so it will loop twice on last line](https://stackoverflow.com/questions/5605125/why-is-iostreameof-inside-a-loop-condition-i-e-while-stream-eof-cons)
  while( in_polfile >> fillnum ){
    if( fillnum<=32000 ){ break; }
    PolData* temp = new PolData();
    temp->mFillNum = fillnum;                             //First entry is fill number
    in_polfile >> value; temp->mBeamEn = value;           //Second entry is beam energy
    in_polfile >> value; temp->mStartTime = value;        //Third entry is start time
    in_polfile >> value;                                  //Fourth entry is stop time (drop)
    in_polfile >> fvalue; temp->mBlueP0 = fvalue;         //Fifth entry is blue beam polarization
    in_polfile >> fvalue; temp->mBlueErrP0 = fvalue;      //Sixth entry is blue beam polarization error
    in_polfile >> fvalue; temp->mBluedPdT = fvalue;       //Seventh entry is blue beam polarization decay
    in_polfile >> fvalue; temp->mBlueErrdPdT = fvalue;    //Eighth entry is the blue beam polarization decay error
    in_polfile >> fvalue; temp->mYellowP0 = fvalue;       //Ninth entry is yellow beam polarization
    in_polfile >> fvalue; temp->mYellowErrP0 = fvalue;    //Tenth entry is yellow beam polarization error
    in_polfile >> fvalue; temp->mYellowdPdT = fvalue;     //Eleventh entry is yellow beam polarization decay
    in_polfile >> fvalue; temp->mYellowErrdPdT = fvalue;  //Twelfth entry is the yellow beam polarization decay error
    mPolarizationData.emplace(fillnum,temp);
  }
  in_polfile.close();
  //for( std::map<Int_t,PolData*>::iterator itr=mPolarizationData.begin(); itr!=mPolarizationData.end(); ++itr ){ itr->second->Print(); }
  return mPolarizationData.size();
}

void StFwdAnaData::readFcsTrigTxtFile(const char* filename)
{
  std::ifstream infile(filename);
  if( !infile.is_open() ){ LOG_ERROR << "StFwdAnaData::readTxtFile - Unable to open file" << endm; }
  while( !infile.eof() ){
    std::string name;
    infile >> name;
    if( name.size()==0 ){ continue; }
    bool namefound = false;
    for( unsigned int i=0; i<mListOfFcsTriggers.size(); ++i ){
      if( mListOfFcsTriggers.at(i) == name ){ namefound = true; }
    }
    if( !namefound ){ mListOfFcsTriggers.push_back(name); }
    UInt_t trigid;
    Int_t startrun;
    Int_t endrun;
    infile >> trigid >> startrun >> endrun;
    //@[September 16, 2024] > To use emplace just create the object on the fly as hinted [here](https://cplusplus.com/forum/beginner/283109/)
    //                         + A better solution without neccessary object creation [here](https://www.machinet.net/tutorial-eng/how-to-use-emplace-in-cpp-maps)
    //                         + Same method emphasized [here](https://stackoverflow.com/questions/68645539/how-to-use-emplace-in-map-for-custom-class)
    //                         + and [here](https://stackoverflow.com/questions/6162201/c11-use-case-for-piecewise-construct-of-pair-and-tuple)
    auto inserted = mAllFcsTrigRanges.emplace(std::piecewise_construct,
					   std::forward_as_tuple(trigid),
					   std::forward_as_tuple(name.c_str(),trigid,startrun,endrun)
					   );
    if( !inserted.second ){ LOG_WARN << "StFwdAnaData::readTxtFile - Trigger Ids not unique found at least two with ID:"<<trigid << endm; }
    else{ SetTriggerPtThresholds( (*(inserted.first)).second ); }
  }
  infile.close();
}

const char* StFwdAnaData::fcsTrigNameFromId(Int_t trigidtomatch, Int_t runnumber) const
{
  auto founditr = mAllFcsTrigRanges.find(trigidtomatch);
  if( founditr==mAllFcsTrigRanges.end() ){ return "NF"; }
  else{
    if( founditr->second.ValidRun(runnumber) ){ return founditr->second.mName.c_str(); }
    else{ return "NF"; }
  }
}

void StFwdAnaData::SetTriggerPtThresholds(TrigRange& input)
{
  //List order matches the table on the website https://www.star.bnl.gov/protected/spin/akio/fcs/run22trg.html
  if( input.mName=="fcsJP2" )    { input.mPtThreshold = 8; return; }
  if( input.mName=="fcsJPA1" )   { input.mPtThreshold = 6; return; }
  if( input.mName=="fcsJPA0" )   { input.mPtThreshold = 4; return; }
  if( input.mName=="fcsJPBC1" )  { input.mPtThreshold = 6; return; }
  if( input.mName=="fcsJPBC0" )  { input.mPtThreshold = 4; return; }
  if( input.mName=="fcsJPDE1" )  { input.mPtThreshold = 6; return; }
  if( input.mName=="fcsJPDE0" )  { input.mPtThreshold = 4; return; }
  if( input.mName=="fcsDiJP" )   { input.mPtThreshold = 5; input.mPtThresholdAsym = 5; return; }
  if( input.mName=="fcsDiJPAsy" ){ input.mPtThreshold = 5; input.mPtThresholdAsym = 4; return; }
  if( input.mName=="fcsDY" )     { input.mPtThreshold = 1; return; }
  if( input.mName=="fcsJPsi" )   { input.mPtThreshold = 0.7; return; }
  //if( input.mName=="fcsDYNoEpd" ){ input.mPtThreshold = 1; return; } //Not in trigger map
  if( input.mName=="fcsDYAsy" )  { input.mPtThreshold = 1; input.mPtThresholdAsym=0.7; return; }
  if( input.mName=="fcsHad2" )   { input.mPtThreshold = 6; return; }
  if( input.mName=="fcsHad1" )   { input.mPtThreshold = 4; return; }
  if( input.mName=="fcsHad0" )   { input.mPtThreshold = 2; return; }
  if( input.mName=="fcsEM2" )    { input.mPtThreshold = 5; return; }
  if( input.mName=="fcsEM1" )    { input.mPtThreshold = 3.5; return; }
  if( input.mName=="fcsEM0" )    { input.mPtThreshold = 2; return; }
  if( input.mName=="fcsELE2" )   { input.mPtThreshold = 1; return; }
  if( input.mName=="fcsEM3" )    { input.mPtThreshold = 1; return; }
  //TPC triggers match the non TPC counterpart
  if( input.mName=="fcsEM2_tpc" )    { input.mPtThreshold = 5; return; }
  if( input.mName=="fcsEM1_tpc" )    { input.mPtThreshold = 3.5; return; }
  if( input.mName=="fcsEM0_tpc" )    { input.mPtThreshold = 2; return; }

  //Debug triggers or triggers unable to find on website
  if( input.mName=="fcs_led" )       { input.mPtThreshold = 0; return; }
  if( input.mName=="fcs_HcalTot" )   { input.mPtThreshold = 0; return; }
  if( input.mName=="fcs_EcalTot" )   { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSS7-HTOT-S" )  { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSS6-ETOT-S" )  { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSS5-HHT-S" )   { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSS4-EHT-S" )   { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSS3-EM3-S" )   { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSS2-ELE3-S" )  { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSS1-ELE1-S" )  { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSS0-ELE0-S" )  { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSN7-HTOT-N" )  { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSN6-ETOT-N" )  { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSN5-HHT-N" )   { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSN4-EHT-N" )   { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSN3-EM3-N" )   { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSN2-ELE2-N" )  { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSN1-ELE1-N" )  { input.mPtThreshold = 0; return; }
  if( input.mName=="FCSN0-ELE0-N" )  { input.mPtThreshold = 0; return; }
  if( input.mName=="fcsHTOT-S" )     { input.mPtThreshold = 0; return; }
  if( input.mName=="fcsHTOT-N" )     { input.mPtThreshold = 0; return; }
  if( input.mName=="fcsHHT-S" )      { input.mPtThreshold = 0; return; }
  if( input.mName=="fcsHHT-N" )      { input.mPtThreshold = 0; return; }
  if( input.mName=="fcsETOT-S" )     { input.mPtThreshold = 0; return; }
  if( input.mName=="fcsETOT-N" )     { input.mPtThreshold = 0; return; }
  if( input.mName=="fcsEHT-S" )      { input.mPtThreshold = 0; return; }
  if( input.mName=="fcsEHT-N" )      { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS15-DiELEA" )  { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS15_DiELEA" )  { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS14-DiJPAsy" ) { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS13-DiJP" )    { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS12-JPDE0" )   { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS11-JPBC0" )   { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS10-JPA0" )    { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS09-JPDE1" )   { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS08-JPBC1" )   { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS07-JPA1" )    { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS06-JP2" )     { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS05-EM2" )     { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS04-EM1" )     { input.mPtThreshold = 0; return; }
  if( input.mName=="FCS03-EM0" )     { input.mPtThreshold = 0; return; }

  LOG_WARN << "Unable to find trigger P_T==E_T threshold for '" << input.mName << "'" << endm;
  return;
}

int StFwdAnaData::sizeOfFcsTriggers() const
{
  return mListOfFcsTriggers.size();
}

const char* StFwdAnaData::fcsTriggerName(int idx) const
{
  return mListOfFcsTriggers.at(idx).c_str();
}

Float_t StFwdAnaData::fcsPtThr(Int_t trigid) const
{
  auto itr=mAllFcsTrigRanges.find(trigid);
  if( itr!=mAllFcsTrigRanges.end() ){ return itr->second.mPtThreshold; }
  else{ return 999; } //if no trigger id exists then return a large value to avoid including bad triggers
}

Float_t StFwdAnaData::fcsPtThrAsym(Int_t trigid) const
{
  auto itr=mAllFcsTrigRanges.find(trigid);
  if( itr!=mAllFcsTrigRanges.end() ){ return itr->second.mPtThresholdAsym; }
  else{ return 999; } //if no trigger id exists then return a large value to avoid including bad triggers
}


