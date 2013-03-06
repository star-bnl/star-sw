
void getCluAvgEff(Char_t* signalFile="signalShapes.root", Int_t diskNr=3, Bool_t onlyQuadB=false)
{
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
   plFgtQuads_dbMap();

   TFile f(signalFile);
   //TFile f("signalShapes.root");
 char buffer[100];

 sprintf(buffer,"allClusterCountsDisk_%d",diskNr);//counting different for this histo
 TH2D* hEff=(TH2D*)f.Get(buffer);

 sprintf(buffer,"radioDiskNonEff_%d",diskNr-1);
 TH2D* hNonEff=(TH2D*)f.Get(buffer);
 // hEff->Rebin2D(2,2); //merge wo bins...
 // hNonEff->Rebin2D(2,2);

 Double_t max=hEff->GetXaxis()->GetXmax();
 Double_t min=hEff->GetXaxis()->GetXmin();
 Int_t minCounts=10;
 // cout <<"max: " << max << " min: " << min << " numBins: "<< h->GetNbinsX() <<endl;

 TH2D OverallEff("overallEff","overallEff",hEff->GetNbinsX(),min,max,hEff->GetNbinsY(),min,max);
 TH2D OverallCounts("overallCounts","overallCounts",hEff->GetNbinsX(),min,max,hEff->GetNbinsY(),min,max);
 TH2D OverallFound("overallFound","overallFound",hEff->GetNbinsX(),min,max,hEff->GetNbinsY(),min,max);
 OverallEff.GetXaxis()->SetTitle("x [cm]");
 OverallEff.GetYaxis()->SetTitle("y [cm]");
 OverallCounts.GetXaxis()->SetTitle("x [cm]");
 OverallCounts.GetYaxis()->SetTitle("y [cm]");
 OverallFound.GetXaxis()->SetTitle("x [cm]");
 OverallFound.GetYaxis()->SetTitle("y [cm]");



Double_t eff=0;
Int_t count=0;

 Int_t sumEff=0;
 Int_t sumNonEff=0;

 Double_t overallErr=0;
for(Int_t i=1;i<hEff->GetNbinsX()+1;i++)
  {
    for(Int_t j=1;j<hEff->GetNbinsY()+1;j++)
      {
	///do you only want to use Quad B?
	if(onlyQuadB)
	  {
	    if(i<(hEff->GetNbinsX()-1)/2|| j>(hEff->GetNbinsY()-1)/2)
	      continue;
	  }
	//Int_t gBin=h->GetBin(i,j);
	//	Double_t xpos=h->GetXaxis()->GetBinCenter(gBin);
	//	Double_t ypos=h->GetYaxis()->GetBinCenter(gBin);
	//	if(xpos<0 || ypos >0)
	//	  continue;


	    Int_t numEff=hEff->GetBinContent(i,j);
	    Int_t numNonEff=hNonEff->GetBinContent(i,j);
	    Int_t numCounts=numEff+numNonEff;
	    //	    cout <<"numEff: " << numEff <<" nonEff: " << numNonEff << " counts: " << numCounts<<endl;
	    Double_t efficiency=0;
	    if(numCounts>minCounts)
	      {
		efficiency=(Double_t)numEff/(Double_t)numCounts;
	      }
	    if(numEff>0)
	      {
		sumEff+=numEff;
		sumNonEff+=numNonEff;
	      }
	    OverallEff.SetBinContent(i,j,efficiency);
	    OverallCounts.SetBinContent(i,j,numCounts);
	    OverallFound.SetBinContent(i,j,numEff);

	if(numCounts>minCounts)
	  {
	    //
	    Double_t relErr=999;
	    Double_t err=999;
	    if(numEff>0)
	      {
		relErr=sqrt((1/(Double_t)(numEff+numNonEff)+1/(Double_t)numEff));
		err=relErr*efficiency;

	      }
	    else
	      {
		err=1/sqrt(numNonEff);
	      }
	    eff+=efficiency/(err*err);
	    overallErr+=(1/(err*err));

	    count++;
	  }
      }
  }

 Double_t avgEff=sumEff/(Double_t)(sumEff+sumNonEff);
 //binomial error, beware of 0
 Double_t errOnEffNum=sqrt(avgEff*(1-avgEff)*(sumEff+sumNonEff));
 Double_t altErr= ((Double_t)1/(Double_t)(sumEff+sumNonEff))*sqrt(sumEff*(Double_t)(1-sumEff/(sumEff+sumNonEff)));
 sprintf(buffer,"Average Efficiency is %f +- %f",avgEff,altErr);
 //15 degrees
 Float_t rotationRadians=-0.261799388;
 Float_t rotationRadians2=-1.83259571;
 Float_t rotation=-15;
 Float_t innerR=11.5;
 Float_t outerR=38;
 TLatex t1(-30,0,buffer);
 TArc outerA(0,0,outerR,90+rotation,-90+rotation);
 TArc innerA(0,0,innerR,90+rotation,-90+rotation);
 // cout <<"cos rot: " << cos(rotation) <<" sin: " << sin(rotation) <<endl;
 TLine l1(innerR*cos(rotationRadians),innerR*sin(rotationRadians),outerR*cos(rotationRadians),outerR*sin(rotationRadians));
 TLine l2(innerR*cos(rotationRadians2),innerR*sin(rotationRadians2),outerR*cos(rotationRadians2),outerR*sin(rotationRadians2));
 l1.SetLineWidth(3);
 l2.SetLineWidth(3);
 outerA.SetLineWidth(3);
 innerA.SetLineWidth(3);

 // plGood();

 int apvMap[]={5,6,7,8,9,12,13,14,15,16};
 char buffer2[200];
 TCanvas* c2=new TCanvas("ceff","ceff",1,1,800,800);
 for(int i=0;i<10;i++)
   {
     //     sprintf(buffer2,"cD%d_apv%d",diskNr,i);
     //     c2=new TCanvas(buffer2,buffer2,1,1,800,800);
     if(i==0)
       {
	 sprintf(buffer2,"overallEff_D%d.pdf(",diskNr);
	 Char_t buffer3[100];
	 sprintf(buffer3,"overallEff_D%d.png",diskNr);
     OverallEff.Draw("colz");
     outerA.Draw();
     innerA.Draw();
     OverallEff.Draw("same colz");

     t1.Draw();
     l1.Draw();
     l2.Draw();
	 c2.SaveAs(buffer3);
       }
     else
       {
	 if(i==9)
	   sprintf(buffer2,"overallEff_D%d.pdf)",diskNr);
	 else
	   sprintf(buffer2,"overallEff_D%d.pdf",diskNr);
       }
     OverallEff.Draw("colz");
     outerA.Draw();
     innerA.Draw();
     
     OverallEff.Draw("same colz");
     t1.Draw();
     l1.Draw();
     l2.Draw();
     sprintf(buffer,"overallEff_D%d_APV%d.pdf",diskNr,apvMap[i]);
     plAPV(apvMap[i],1,'B');
     c2->SaveAs(buffer);
     cout <<"printing to " << buffer2 <<endl;
     c2->Print(buffer2,"pdf");
 }
 TCanvas c("effs","effs",1,1,800,800);


 //counts

 TCanvas cC("counts","counts",1,1,800,800);
 OverallCounts.Draw("colz");
 outerA.Draw();
 innerA.Draw();
 
 OverallCounts.Draw("same colz");
 l1.Draw();
 l2.Draw();
 cC.SaveAs("overallCounts.png");


 TCanvas cf("found","found",1,1,800,800);
 OverallFound.Draw("colz");
 outerA.Draw();
 innerA.Draw();
 OverallFound.Draw("same colz");
 l1.Draw();
 l2.Draw();
 cf.SaveAs("overallFound.png");

 sprintf(buffer,"overall found");
 sprintf(buffer2,"cFoundD",diskNr);
 c2=new TCanvas(buffer2,buffer2,1,1,800,800);
 TLatex t2(-30,0,buffer);
 for(int i=0;i<10;i++)
   {
     if(i==0)
       {
	 sprintf(buffer2,"overallFound_D%d.pdf(",diskNr);
	 Char_t buffer3[100];
	 sprintf(buffer3,"overallFound_D%d.png",diskNr);
	 sprintf(buffer2,"overallFound_D%d.pdf(",diskNr);
	 OverallFound.Draw("colz");
     outerA.Draw();
     innerA.Draw();
	 OverallFound.Draw("same colz");
 l1.Draw();
 l2.Draw();
	 c2.SaveAs(buffer3);
       }
     else
       {
	 if(i==9)
	   sprintf(buffer2,"overallFound_D%d.pdf)",diskNr);
	 else
	   sprintf(buffer2,"overallFound_D%d.pdf",diskNr);
       }
     OverallEff.Draw("colz");
     outerA.Draw();
     innerA.Draw();
     
     //     OverallFound.Draw("same colz");
     t2.Draw();
     l1.Draw();
     l2.Draw();
     sprintf(buffer,"overallFound_D%d_APV%d.png",diskNr,apvMap[i]);
     plAPV(apvMap[i],1,'B');
     c2.SaveAs(buffer);
     cout <<"printing to " << buffer2 <<endl;
     c2->Print(buffer2,"pdf");
 }






 // c.SaveAs("overallEff.C");
//cout <<"avg eff: " << eff/count <<endl;
 cout <<"Hits found: " << sumEff <<" Hits not found: " << sumNonEff<< " efficiency: " <<avgEff<<" +- " << altErr<<endl;

}

double pi=2.*acos(0.);
double rad2deg=pi/180.;
//==================
enum {mxFgtElect=30720, mxFgtApv=22, mxFgtPln=2, kFgtPlnP=0, kFgtPlnR=1 };
char *plnC[mxFgtPln]={"Phi","R"};

//  plane[R,P], r1(cm), r2(cm), phi1(rad), phi2(rad)

struct FgtStripDbItem{
  int electId,geoId, rdo,arm,apv,chan, disc,strip;
  float  r1, r2, phi1, phi2; // cm, rad
  char layer;  // P,R
  char quad; // A-D
  char name[10];   
  int stat; // 0 is good
  float ped, sigPed;
};

FgtStripDbItem stripDb[mxFgtElect];


// mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
//                MAIN 
// mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

void plFgtQuads_dbMap() { // ipl: 0=Phi, 1=R; apv<0=all
  memset(stripDb,0, sizeof(stripDb));
  readFgtStripMap_CSV("fgtMapDump-jan28.csv");
  printf("reading of CSV done\n"); 

  for(int i=0;i<10;i++) {
    if(stripDb[i].rdo<=0) continue;
    printStripItem(stripDb+i);
  }

  printf(" execute:  plAPV()  , plPlane()  , plGood(1) ,  plRstrips(1);  \n");
  // plAPV(); //  plAPV( int apv=0,  int disc=1, char quad='A' )
  //plPlane(1); // 0=P, 1=R

  //plGood(2);
  //plRstrips(1); // ioct=0=L, ioct=1=S
}

//--------------------------------------------------
//--------------------------------------------------
<//--------------------------------------------------
void plGood(  int disc=1, int  ipl=-1 ) {
  double par_fgtQuadMaxR=39.7;//cm, limit for displaiyng
  int par_globView=0; 

  TString canN=Form("fgt_disc%d_good",disc);
  if(ipl>=0) canN=Form("fgt_%d_%s",disc,plnC[ipl]);
  TH2F *hQuad=new TH2F( "aa","aa",20,-par_fgtQuadMaxR,par_fgtQuadMaxR,20,-par_fgtQuadMaxR,par_fgtQuadMaxR);
  gStyle->SetOptStat(0) ;
  gStyle->SetFillColor(kWhite);
  c=new TCanvas( canN,canN,700,720);
  hQuad->Draw("SAME");
  //  gPad->SetGrid();
  int nstr=0;

  nstr+=plotStrips( -1, disc, 'A', ipl, -15);
  nstr+=plotStrips( -1, disc, 'B', ipl, -15-90);
  nstr+=plotStrips( -1, disc, 'D', ipl, 75);
  nstr+=plotStrips( -1, disc, 'C', ipl, 75+90);

  TString tit;
  tit=Form("FGT: disc %d,  good strips=%d ;  STAR ref.   X (cm); STAR ref.  Y (cm)",disc,  nstr);
  
  if(ipl>=0) tit=Form("FGT: disc %d  plane=%s,  GOOD strip=%d ;  local X (cm); local Y (cm)",disc,  plnC[ipl], nstr);

  hQuad->SetTitle(tit);
  c->Print();
}



//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void plRstrips( int ioct ) {
  int  ipl=1;
  int disc=1; char quad='A';
  double par_fgtQuadMaxR=39.7;//cm, limit for displaiyng
  int par_globView=0; 

  TString canN=Form("fgt_%d%c_%s_str_oct%d",disc,quad,plnC[ipl],ioct);
  TH2F *hQuad=new TH2F( "aa","aa",200,-0.5,par_fgtQuadMaxR,200,-0.5,par_fgtQuadMaxR);
  gStyle->SetOptStat(0) ;
  gStyle->SetFillColor(kWhite);
  c=new TCanvas( canN,canN,700,720);
  hQuad->Draw("SAME");
  //  gPad->SetGrid();
  int nstr=0;

  nstr=plotStrips( 0+17*ioct, disc, quad, ipl,0,1);
  nstr=plotStrips( 1+17*ioct, disc, quad, ipl,0,2);
  nstr=plotStrips( 2+17*ioct, disc, quad, ipl,0,8);
  nstr=plotStrips( 3+17*ioct, disc, quad, ipl,0,4);
  nstr=plotStrips( 4+17*ioct, disc, quad, ipl,0,6);

  TString tit;
  tit=Form("FGT: quad %d%c,  plane=%s,  strip=%d ;  local X (cm); local Y (cm)",disc, quad, plnC[ipl], nstr);
  hQuad->SetTitle(tit);
  c->Print();
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void plPlane(  int  ipl=0, int disc=1, char quad='A' ) {
  double par_fgtQuadMaxR=39.7;//cm, limit for displaiyng
  int par_globView=0; 

  TString canN=Form("fgt_%d%c_%s",disc,quad,plnC[ipl]);
  TH2F *hQuad=new TH2F( "aa","aa",200,-0.5,par_fgtQuadMaxR,200,-0.5,par_fgtQuadMaxR);
  gStyle->SetOptStat(0) ;
  gStyle->SetFillColor(kWhite);
  c=new TCanvas( canN,canN,700,720);
  hQuad->Draw("SAME");
  //  gPad->SetGrid();
  int nstr=0;

  nstr=plotStrips( -1, disc, quad, ipl);

  TString tit;
  tit=Form("FGT: quad %d%c,  plane=%s,  strip=%d ;  local X (cm); local Y (cm)",disc, quad, plnC[ipl], nstr);
  hQuad->SetTitle(tit);
  c->Print();
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void plAPV( int apv=0,  int disc=1, char quad='A' ) {

  double par_fgtQuadMaxR=39.7;//cm, limit for displaiyng
  int par_globView=0; 

  TString canN=Form("fgt_%d%c_apv%d",disc,quad,apv);
  TH2F *hQuad=new TH2F( "aa","aa",200,-0.5,par_fgtQuadMaxR,200,-0.5,par_fgtQuadMaxR);
  gStyle->SetOptStat(0) ;
  gStyle->SetFillColor(kWhite);
  //  c=new TCanvas( canN,canN,700,720);
  hQuad->Draw("SAME");
  //  gPad->SetGrid();
  int nstr=0;
  TText *tx;
  //  if(disc==1 && quad=='A') 
  //    tx=new TText(5,40.3,"same mapping for quads 1A,1C,2A,3B,4A,5B,6A");  
  //  if(disc==1 && quad=='B') 
  //    tx=new TText(5,40.3,"same mapping for quads 1B,1D,2B,3A,4B,5A,6B");  
  //  tx->Draw(); tx->SetTextSize(0.03);

  nstr=plotStrips( apv, disc, quad,-1);

  TString tit;
  if( apv>=0) tit=Form("FGT: quad %d%c,  APV=%d,  Nstrip=%d ;  local X (cm); local Y (cm)",disc, quad, apv, nstr);
  hQuad->SetTitle(tit);
  //  c->Print(Form("fgt_%d%c_apv%02d.ps",disc,quad,apv));
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
int plotStrips(int apv, int disc, char quad, int ipl, int phiDeg=0, int iCol0) {

  Float_t rotDeg=105;
  Bool_t secondOct=false;
  //  if(apv>4)
  //    {
  //      secondOct=true;
  //    }

  //  apv-=5;
  int lineWidth=0;
  int nstr=0;
  for(int i=0;i<mxFgtElect;i++) {
    FgtStripDbItem *S=stripDb+i;
    if(S->geoId<0) continue;
    if(S->disc!=disc) continue;
    if(S->quad!=quad) continue;
    if(ipl>=0) 
      if(S->layer!=plnC[ipl][0]) continue;
    if(phiDeg) 
      if(S->stat) continue;

    //printStripItem(S);
    if(nstr<5) printStripItem(S);
    
    int iCol=kBlue;
    if( S->strip <=360) iCol=kRed; 
    
    if(apv>=0) { 
      iCol=kBlue;
      lineWidth=1;
      if(S->apv!=apv) continue;    
    }


    if(iCol0) iCol=iCol0;

    nstr++;
    if(apv>=0) {      //print strip ID on the right
      tx=new TText(42-(nstr%2)*2., 42.-(nstr+S->chan/32)/3., S->name+2);
      if(S->layer=='P') tx->SetTextColor(kBlue);
      //B      tx->Draw(); 
tx->SetTextSize(0.014);
 if(nstr==1) { tx=new TText(39.7, 42.9,"by electID"); // tx->Draw();
 tx->SetTextSize(0.02);}
    }
  
    if( S->layer=='P') { //ppppppppppppppppppppp
      int keep= ( S->strip%720)==0;
      if(S->strip%7!=0 && !keep) continue;


      //      phiDeg+=0.94;
      //      if(phiDeg>2*pi)
      //	phiDeg-=(2*pi);
      //      cout <<"phiDeg: " << phiDeg;
      //      phiDeg=phiDeg-rotDeg;
      //      cout << " now : " << phiDeg;
      phiDeg=-rotDeg;
      double phi1=S->phi1+phiDeg*rad2deg, phi2=S->phi2+phiDeg*rad2deg;

      //           phi1-=rotDeg*rad2deg;
      //            phi2-=rotDeg*rad2deg;
      cout <<" phi1: " << phi1 <<" phi2: " << phi2 <<endl;
      double x1=S->r1*cos(phi1), x2=S->r2*cos(phi2);
      double y1=S->r1*sin(phi1), y2=S->r2*sin(phi2);
      

      ln=new TLine(x1,y1,x2,y2);
      ln->Draw(); ln->SetLineColor(iCol);
      ln->SetLineWidth( lineWidth);
      
      if(S->strip%35==0 || keep) {
	double x3=x2+1., y3=y2+0.6;
	ln=new TLine(x2*1.005,y2*1.005,x3,y3);
	ln->Draw(); ln->SetLineColor(iCol);
	ln->SetLineWidth(0);
	tx=new TText(x3,y3,Form("%c%03d",S->layer,S->strip)); 
		tx->Draw(); tx->SetTextColor(iCol); tx->SetTextSize(0.02);
	tx->SetTextAngle(phiDeg+10.);
      }      
     } // end of P-plane 

    if(  S->layer=='R'  ) { //RRRRRRRRRRRRRRRRRRRRRRR
      int kk;
      if( S->strip <400) {  kk=S->strip;}  
      else {   kk=S->strip-400;}  
      int keep= (kk %279)==0;
      if(kk%7!=0 && !keep) continue;
            
      double phi1Deg=S->phi1/rad2deg +phiDeg;
      double phi2Deg=S->phi2/rad2deg+phiDeg;

      //      phi1Deg-=rotDeg;
      //      phi2Deg-=rotDeg;
 
      TArc * ar=new TArc(0.,0,S->r1, phi1Deg, phi2Deg); 
      ar->SetFillStyle(0); ar->SetLineColor(iCol);
      ar->Draw("only");
      ar->SetLineWidth( lineWidth);
      
      if(kk%35==0 || keep) {
	double x2,y2,x3,y3,dx,dy;
	double phiAvr=(S->phi1+S->phi2)/2.;
	if(phiDeg) { // rotate P-strips
	double delPhi=phiDeg*rad2deg;
	phiAvr+=delPhi;
      }

	x2=S->r1*cos(phiAvr); y2=S->r2*sin(phiAvr);
	x3=x2-0.3; y3=y2+0.6;
	dx=-1.; dy=0.2;
	if(S->apv<5 ) { 
	  x3=x2+0.2; y3=y2+0.7;
	  dy=.3; dx=-0.7;
	}  
	ln=new TLine(x2,y2,x3,y3);
	ln->Draw(); ln->SetLineColor(iCol);
	ln->SetLineWidth(0);
	tx=new TText(x3+dx,y3+dy,Form("%c%03d",S->layer,S->strip)); 
		tx->Draw();  tx->SetTextSize(0.02);//tx->SetTextColor(iCol);
	tx->SetTextAngle(phiDeg-10.);
      }      
     } // end of R-plane 

    } // end of strip loop
  return nstr;  
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

void printStripItem( FgtStripDbItem *S) {
  printf("%d%c_%c-plane  strip=%d  geo=%d elec=%d apv%d chan%d, R[%.1f, %.1f]cm Phi[%.2f, %.2f]deg %s stat=%d  ped=%.1f\n",S->disc,S->quad,S->layer,S->strip, S->geoId,S->electId,S->apv, S->chan, S->r1, S->r2, S->phi1/rad2deg, S->phi2/rad2deg, S->name, S->stat, S->ped);

}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void readFgtStripMap_CSV(char *fname){
  FILE *fd=fopen(fname,"r"); assert(fd);

  const int mx=1000;
  char buf[mx];
  int k=0, nok=0;

  float ord,lowSpan,upSpan;

  FgtStripDbItem S;
  while(true) {
    char * ret=fgets(buf,mx,fd);
    if(ret==0) break;
    if(buf[0]=='#') continue;

    char *item=strtok(buf,","); // init 'strtok'
    int i=0;
    
    do {
      i++;
      //printf("i=%d, item=%s=\n",i,item);
      switch(i){
      case 1: S.electId=atoi(item); break;
      case 2: S.geoId=atoi(item); break;

      case 3: S.rdo=atoi(item); break;
      case 4: S.arm=atoi(item); break;
      case 5: S.apv=atoi(item); break;
      case 6: S.chan=atoi(item); break;

      case 7: S.disc=atoi(item); break;
      case 8: S.quad=item[0]; break;
      case 9: S.layer=item[0]; break;
      case 10: S.strip=atoi(item); break;
 
      case 11: sscanf(item,"%f",&ord);  break;
      case 12: sscanf(item,"%f",&lowSpan);  break;
      case 13: sscanf(item,"%f",&upSpan);  break;
	
      case 14: sscanf(item,"%s",S.name);  break;

      case 15: S.stat=atoi(item); break;
      case 16: sscanf(item,"%f",&S.ped);  break;
      case 17: sscanf(item,"%f",&S.sigPed);  break;
      default:
      }
      
    } while (item=strtok(0,","));  // advance by one name

    k++;
    if(S.geoId<0)  {
      stripDb[S.electId].geoId=-1;
      continue; // skip invalid records
    }
    nok++;

    if (S.layer=='P') {
      S.phi1=S.phi2=ord;
      S.r1=lowSpan;
      S.r2=upSpan;
    } else {
      S.r1=S.r2=ord;
      S.phi1=lowSpan;
      S.phi2=upSpan;
    }

    // printf("k=%d nok=%d %d-%c\n",k,nok,S.quad,S.layer );    printStripItem(&S);
		 
    assert(S.electId>=0);
    assert(S.electId<mxFgtElect);
   
    stripDb[S.electId]=S;
       
     //if(k>282) break;
  }
  printf ("total # of  APV channels mapped %d, see records %d\n",nok,k);
}


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void doAll() {
  // print APVs in disc 1
  char quad='B';
  //  for(int apv=0; apv<5;apv++) plAPV(apv,1,quad);
  // for(int apv=17; apv<22;apv++) plAPV(apv,1,quad);
  //for(int apv=5; apv<10;apv++) plAPV(apv,1,quad);
  //for(int apv=12; apv<17;apv++) plAPV(apv,1,quad);

  for(int disc=1;disc<=6;disc++) plGood(disc);

}
