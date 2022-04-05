#include "float.h"
#include "calculateCumulant.h"

void calculateCumulant(const char* histFileName){

  //  gSystem->Load("myProjectProfile_C.so"); //user defined integration

  double r0Sq=r0*r0;
  double perCent=0.01;


  TString xLabel = "Pseudorapidity";
  if (isPidFlow) { xLabel = "Rapidity"; }  
  

  TFile* f = new TFile(histFileName,"UPDATE");

  struct histFullHars {

    TProfile2D**   mHistCumul2D;
    TProfile**     mHistCumulEta;
    TProfile**     mHistCumulPt;

    TProfile2D*    mHistCumulMix2D;
    TProfile*      mHistCumulMixEta;
    TProfile*      mHistCumulMixPt;

    TH2D**         mHist_v2D;
    TH1D**         mHist_vEta;
    TH1D**         mHist_vPt;



    TH2D*         mHistMix_v2D;
    TH1D*         mHistMix_vEta;
    TH1D*         mHistMix_vPt;




    Double_t       mCumulIntegG0[nCumulIntegOrders*nCumulInteg_qMax]; 
    TH1D*          mHistMultSum;    //histo for holding mMultSum.
    TH1D*          mHistNEvent;

    TH1D*          mHistMeanWgtSqrSum; // sum <w^2> over evts.

    Double_t       mCumulIntegG0Mix[nCumulMixHar_pMax*nCumulMixHar_qMax];  

    TH1D**         mHistCumulIntegG0Sum; //summation of G value.
    TH1D**         mHistCumulIntegG0MixSum; //summation of G value.

  };

  struct histFulls {
   TProfile**  mHistCumul; //integrated cumulant from differential
   TH1D**      mHist_v; //integrated flow from differential

   TProfile*   mHistCumulMix; //cumulant for mix 1st and 2nd hars.
   TH1D*       mHistMix_v; //integrated flow from differential

    struct histFullHars histFullHar[nHars];
  };

  struct histFulls histFull[nSels]; 


  TString* histTitle;
  TString* histTitle2;


  //================        fill in pointers    ===================
  //for each selection
  for (int k = 0; k < nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);

    histFull[k].mHistCumul = new TProfile*[nCumulDiffOrders];
    histFull[k].mHist_v    = new TH1D*[nCumulDiffOrders];
  
    for (int ord = 0; ord < nCumulDiffOrders; ord++) {
      char theCumulOrderChar[2]; // if >10, need to use char*
      sprintf(theCumulOrderChar,"%d",(ord+1)*2);
      histTitle = new TString("Flow_Cumul_Order"); 
      histTitle->Append(*theCumulOrderChar);           
      histTitle->Append("_Sel");                      
      histTitle->Append(*countSels);        
      histFull[k].mHistCumul[ord] = (TProfile *)f->Get(histTitle->Data());
      delete histTitle;

      histTitle = new TString("Flow_Cumul_v_Order");
      histTitle->Append(*theCumulOrderChar);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histFull[k].mHist_v[ord] = 
	new  TH1D(*(histFull[k].mHistCumul[ord]->ProjectionX(histTitle->Data(),"e")));
      histFull[k].mHist_v[ord]->SetTitle(histTitle->Data());
      histFull[k].mHist_v[ord]->SetXTitle("harmonic");
      histFull[k].mHist_v[ord]->SetYTitle("v (%)");
      if (ord>0) histFull[k].mHist_v[ord]->Scale(1./profScale);
      delete histTitle;

    }


      ///////////// mixed harmonic
    
      histTitle = new TString("Flow_CumulMix");
      histTitle->Append("_Sel");                      
      histTitle->Append(*countSels); 
      histFull[k].mHistCumulMix = (TProfile *)f->Get(histTitle->Data());
      delete histTitle;


      histTitle = new TString("Flow_CumulMix_v_Sel");
      histTitle->Append(*countSels);
      histFull[k].mHistMix_v = 
	new  TH1D(*(histFull[k].mHistCumulMix->ProjectionX(histTitle->Data(),"e")));
      histFull[k].mHistMix_v->SetTitle(histTitle->Data());
      histFull[k].mHistMix_v->SetXTitle("place for v1 from mixed harmonic");
      histFull[k].mHistMix_v->SetYTitle("v (%)");
      histFull[k].mHistMix_v->Scale(1./profScale);
      delete histTitle;
      ///////////////




    // for each harmonic
    for (int j = 0; j < nHars; j++) {
      char countHars[2];
      sprintf(countHars,"%d",j+1);

      histFull[k].histFullHar[j].mHistCumul2D  =  
	new TProfile2D*[nCumulDiffOrders];
      histFull[k].histFullHar[j].mHistCumulEta = 
	new TProfile*[nCumulDiffOrders];
      histFull[k].histFullHar[j].mHistCumulPt  = 
	new TProfile*[nCumulDiffOrders];

      histFull[k].histFullHar[j].mHist_v2D  = 
        new TH2D*[nCumulDiffOrders];
      histFull[k].histFullHar[j].mHist_vEta = 
        new TH1D*[nCumulDiffOrders];
      histFull[k].histFullHar[j].mHist_vPt  = 
        new TH1D*[nCumulDiffOrders];


      ///////////mixed har
      if (j==0){ //only j==0 makes sense here.

	histTitle = new TString("Flow_CumulMix2D");
	histTitle->Append("_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
        histFull[k].histFullHar[j].mHistCumulMix2D  = 
               (TProfile2D *)f->Get(histTitle->Data());
        delete histTitle;



	histTitle = new TString("Flow_CumulMixEta");
	histTitle->Append("_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	histFull[k].histFullHar[j].mHistCumulMixEta =  
               (TProfile *)f->Get(histTitle->Data());
        delete histTitle;

	histTitle = new TString("Flow_CumulMixPt");
	histTitle->Append("_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	histFull[k].histFullHar[j].mHistCumulMixPt =  
               (TProfile *)f->Get(histTitle->Data());
        delete histTitle;



      histFull[k].histFullHar[j].mHistCumulIntegG0MixSum =
        new       TH1D*[nCumulMixHar_pMax*nCumulMixHar_qMax];


    for (int pq  = 0; pq < nCumulMixHar_pMax*nCumulMixHar_qMax ; pq++) { 
	int pIndex = (pq/nCumulMixHar_qMax);
        int qIndex        = pq%nCumulMixHar_qMax;
	
	char pIndexChar[2];
        char qIndexChar[2]; // if >10, need to use char*
	sprintf(pIndexChar,"%d",pIndex); 
        sprintf(qIndexChar,"%d",qIndex);

  	histTitle = new TString("Flow_CumulIntegG0MixSum");
	histTitle->Append("_GenFunIdxp");
	histTitle->Append(*pIndexChar);
	histTitle->Append("_GenFunIdxq");
	histTitle->Append(*qIndexChar);
	histTitle->Append("_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	histFull[k].histFullHar[j].mHistCumulIntegG0MixSum[pq]=
               (TH1D *)f->Get(histTitle->Data());
        delete histTitle;  

	histFull[k].histFullHar[j].mCumulIntegG0Mix[pq] = 0.;
    }
      }


      ////////////////



      // For each cumulant order
      for (int ord = 0; ord < nCumulDiffOrders; ord++) {
	char theCumulOrderChar[2]; // if >10, need to use char*
	sprintf(theCumulOrderChar,"%d",(ord+1)*2);
	
	histTitle = new TString("Flow_Cumul2D_Order");
	histTitle->Append(*theCumulOrderChar);
	histTitle->Append("_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
        histFull[k].histFullHar[j].mHistCumul2D[ord] = 
               (TProfile2D *)f->Get(histTitle->Data());
        delete histTitle;

	histTitle = new TString("Flow_CumulEta_Order");
	histTitle->Append(*theCumulOrderChar);
	histTitle->Append("_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
        histFull[k].histFullHar[j].mHistCumulEta[ord]= 
               (TProfile *)f->Get(histTitle->Data());
        delete histTitle;


	histTitle = new TString("Flow_CumulPt_Order");
	histTitle->Append(*theCumulOrderChar);
	histTitle->Append("_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	histFull[k].histFullHar[j].mHistCumulPt[ord] = 
               (TProfile *)f->Get(histTitle->Data());
        delete histTitle;  

      }


      histFull[k].histFullHar[j].mHistCumulIntegG0Sum =
      new TH1D*[nCumulIntegOrders*nCumulInteg_qMax];

      for (int pq = 0; pq <  nCumulIntegOrders*nCumulInteg_qMax; pq++) {
	int cumulIndex = (pq/nCumulInteg_qMax) + 1; // like 1,2,3. 
	//not begining with 0. That is "p" in Eq. (B1, PG5).
        int qIndex = pq%(nCumulInteg_qMax); // like 0,1,..qMax-1 
	//begining with 0. Just like Eq. (B3, PG5).

	char theCumulOrderChar[2];
        char qIndexOrderChar[2]; // if >10, need to use char*
	sprintf(theCumulOrderChar,"%d",cumulIndex*2); 
        sprintf(qIndexOrderChar,"%d",qIndex);

  	histTitle = new TString("Flow_CumulIntegG0Sum_Order");
	histTitle->Append(*theCumulOrderChar);
	histTitle->Append("_GenFunIdx");
	histTitle->Append(*qIndexOrderChar);
	histTitle->Append("_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	histFull[k].histFullHar[j].mHistCumulIntegG0Sum[pq] =
               (TH1D *)f->Get(histTitle->Data());
        delete histTitle;  

	histFull[k].histFullHar[j].mCumulIntegG0[pq] = 0.;

      }




      histTitle = new TString("Flow_CumulMultSum_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistMultSum =
             (TH1D *)f->Get(histTitle->Data());
        delete histTitle;  


      histTitle = new TString("Flow_CumulNEvent_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistNEvent =
             (TH1D *)f->Get(histTitle->Data());
        delete histTitle;  


      histTitle = new TString("Flow_CumulMeanWgtSqrSum_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistMeanWgtSqrSum =
             (TH1D *)f->Get(histTitle->Data());
      delete histTitle;


    }
  }

 //================   end of fill in pointers    ===================




  for (int k = 0; k < nSels; k++) {

    char countSels[2];
    sprintf(countSels,"%d",k+1);

    double  meanIntegV[nHars];     // V**1
    double  meanIntegV2[nHars];    // V**2
    double  meanIntegV3[nHars];    // V**3
    double  meanIntegV4[nHars];    // V**4
    double  cumulInteg1[nHars];    // outside of harmonic loop
    double  cumulInteg2[nHars];
    double  cumulInteg3[nHars];
    double  q2[nHars]; // for old method. <Q>**2 in (74) of old paper.
    double  q4[nHars];
    double  q6[nHars];


    double cumulIntegMix[nHars];
    double meanIntegVMix[nHars];
    
    for (int j = 0; j < nHars; j++) {
      meanIntegV[j]  = 0.;
      meanIntegV2[j] = 0.;
      meanIntegV3[j] = 0.;
      meanIntegV4[j] = 0.;
      cumulInteg1[j] = 0.;
      cumulInteg2[j] = 0.;
      cumulInteg3[j] = 0.;
      cumulIntegMix[j] = 0.;
      meanIntegVMix[j] = 0.;
    }

    for (int j = 0; j < nHars; j++) {


      char countHars[2];
      sprintf(countHars,"%d",j+1);

      cout<<" ========== Sel"<<k+1<<" Har"<<j+1<<" ==========="<<endl;

      cout<<" event # "<<histFull[k].histFullHar[j].mHistNEvent->GetBinContent(1)<<endl;

      double mAvMult = 
      	histFull[k].histFullHar[j].mHistMultSum->GetBinContent(1)/
	histFull[k].histFullHar[j].mHistNEvent->GetBinContent(1);

      cout<<"mAvMult Sel"<<k<<" har"<<j<<" "<<mAvMult<<endl;


      double CpInteg[nCumulIntegOrders]; // Cp in (B4, PG6)
      
      for (int pq = 0; pq < nCumulIntegOrders; pq ++) CpInteg[pq] = 0.;
      for (int pq = 0; pq < nCumulIntegOrders*nCumulInteg_qMax; pq++) {   

	int theCumulOrder = (pq/nCumulInteg_qMax) + 1; // like 1,2,3.   

	histFull[k].histFullHar[j].mCumulIntegG0[pq] =
        histFull[k].histFullHar[j].mHistCumulIntegG0Sum[pq]->GetBinContent(1)/
        histFull[k].histFullHar[j].mHistNEvent->GetBinContent(1);

	//	cout<<" --- histFull[k].histFullHar[j].mCumulIntegG0[pq] "<<histFull[k].histFullHar[j].mCumulIntegG0[pq]<<endl;

	if (!isNewMethod){
	  CpInteg[theCumulOrder-1] +=
	    (log(histFull[k].histFullHar[j].mCumulIntegG0[pq]) /
	     ((float)nCumulInteg_qMax));
	} else {
	  CpInteg[theCumulOrder-1] +=
	    (mAvMult*(pow(histFull[k].histFullHar[j].mCumulIntegG0[pq], 1./mAvMult) -1.) /
	     float(nCumulInteg_qMax)); // (B3, PG6) 
	  //          cout<<" CpInteg[theCumulOrder-1] "<<CpInteg[theCumulOrder-1]<<endl;
	}
      }

      cumulInteg1[j] = // (B5, PG7)
	(3.*CpInteg[1-1] - (3./2.)*CpInteg[2-1] + (1./3.)*CpInteg[3-1]) / r0Sq;
      
      cumulInteg2[j] = ((-10.*CpInteg[1-1]) + (8.*CpInteg[2-1]) - 
			 (2.*CpInteg[3-1])) / (r0Sq*r0Sq); 
      
      cumulInteg3[j] = ( (18.*CpInteg[1-1]) - (18.*CpInteg[2-1]) + (6.*CpInteg[3-1]))
			/ (r0Sq*r0Sq*r0Sq);
      

      for (int ord = 0; ord < nCumulDiffOrders; ord++) {
	char theCumulOrderChar[2]; // if >10, need to use char*
	sprintf(theCumulOrderChar,"%d",(ord+1)*2);
	

	
	histTitle = new TString("Flow_Cumul_vEta_Order");
	histTitle->Append(*theCumulOrderChar);
	histTitle->Append("_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	histFull[k].histFullHar[j].mHist_vEta[ord] = 
	  new  TH1D(*(histFull[k].histFullHar[j].mHistCumulEta[ord]->
		      ProjectionX(histTitle->Data(),"e")));
	histFull[k].histFullHar[j].mHist_vEta[ord]->SetTitle(histTitle->Data());
	histFull[k].histFullHar[j].mHist_vEta[ord]->SetXTitle((char*)xLabel.Data());
	histFull[k].histFullHar[j].mHist_vEta[ord]->SetYTitle("v (%)");
        if (ord>0) histFull[k].histFullHar[j].mHist_vEta[ord]->Scale(1./profScale);
	delete histTitle;
	
	histTitle = new TString("Flow_Cumul_vPt_Order");
	histTitle->Append(*theCumulOrderChar);
	histTitle->Append("_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);

	//    if you want integrate for only TPC/FTPC, you need to do
	//  1. load myProjectProfile.C at the begining
	//  2. append "_TPC" or "_FTPC" here to the title
	//  3. use myProjectionY (X) to get mHist_vPt or mHist_vEta.
	//		histTitle->Append("_TPC");

   	  histTitle2 = new TString("Flow_Cumul2D_Order");
   	  histTitle2->Append(*theCumulOrderChar);
	  histTitle2->Append("_Sel");
	  histTitle2->Append(*countSels);
	  histTitle2->Append("_Har");
	  histTitle2->Append(*countHars);

       	  histFull[k].histFullHar[j].mHist_vPt[ord] = 
	    //uncomment this line if you want to integrate over different range
	    //myProjectionY((TProfile2D *)f->Get(histTitle2->Data()) , 0., 1., 1, ((j==0 || j== 2) ? 1 : 0) );
		   	  new  TH1D(*(histFull[k].histFullHar[j].mHistCumulPt[ord]->ProjectionX(histTitle->Data(),"e")));
	histFull[k].histFullHar[j].mHist_vPt[ord]->SetTitle(histTitle->Data());
	histFull[k].histFullHar[j].mHist_vPt[ord]->SetName(histTitle->Data());
	histFull[k].histFullHar[j].mHist_vPt[ord]->SetXTitle("Pt (GeV)");
	histFull[k].histFullHar[j].mHist_vPt[ord]->SetYTitle("v (%)");
       	if (ord>0) histFull[k].histFullHar[j].mHist_vPt[ord]->Scale(1./profScale);
	delete histTitle; 
        delete histTitle2;


	//do not reverse the order of getting v2D and vPt !!

	histTitle = new TString("Flow_Cumul_v2D_Order");
	histTitle->Append(*theCumulOrderChar);
	histTitle->Append("_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	histFull[k].histFullHar[j].mHist_v2D[ord] = 
	  new TH2D(*(histFull[k].histFullHar[j].mHistCumul2D[ord]->
		     ProjectionXY(histTitle->Data(),"e")));
	histFull[k].histFullHar[j].mHist_v2D[ord]->SetTitle(histTitle->Data());
	histFull[k].histFullHar[j].mHist_v2D[ord]->SetXTitle((char*)xLabel.Data());
	histFull[k].histFullHar[j].mHist_v2D[ord]->SetYTitle("Pt (GeV)");
	histFull[k].histFullHar[j].mHist_v2D[ord]->SetZTitle("v (%)");
        if (ord>0) histFull[k].histFullHar[j].mHist_v2D[ord]->Scale(1./profScale);
	delete histTitle;



      }


      //In the case if cumulant fails, let <V> etc. to be 1. to avoid crash.
	meanIntegV[j]  = (cumulInteg1[j]>0.) ? sqrt(cumulInteg1[j]) : 1.;           // <v>    2-part, m=1
	meanIntegV2[j] = (cumulInteg1[j]>0.) ? cumulInteg1[j] : 1.;                 // <v**2> 2-part, m=2
	meanIntegV3[j] = (cumulInteg2[j]<0.) ? pow(-1.*cumulInteg2[j], 3./4.) : 1.; // <v**3> 4-part, m=1
	meanIntegV4[j] = (cumulInteg2[j]<0.) ? -1.*cumulInteg2[j] : 1.;             // <v**4> 4-part, m=2


    

      if (m_M==1) { // Eq. (PG14) 

	histFull[k].histFullHar[j].mHist_v2D[0]->Scale(1./(meanIntegV[j]*perCent)); // (34a)
	histFull[k].histFullHar[j].mHist_v2D[1]->Scale(-1./(meanIntegV3[j]*perCent));// (34b)
	histFull[k].histFullHar[j].mHist_vEta[0]->Scale(1./(meanIntegV[j]*perCent)); // (34a)
	histFull[k].histFullHar[j].mHist_vEta[1]->Scale(-1./(meanIntegV3[j]*perCent)); // (34b)
	histFull[k].histFullHar[j].mHist_vPt[0]->Scale(1./(meanIntegV[j]*perCent)); // (34a)
	histFull[k].histFullHar[j].mHist_vPt[1]->Scale(-1./(meanIntegV3[j]*perCent)); // (34b)
      } else if (m_M==2) {
	histFull[k].histFullHar[j].mHist_v2D[0]->Scale(1./(meanIntegV2[j]*perCent)); // (35a)
        histFull[k].histFullHar[j].mHist_v2D[1]->Scale(-0.5/(meanIntegV4[j]*perCent)); // (35b)
	histFull[k].histFullHar[j].mHist_vEta[0]->Scale(1./(meanIntegV2[j]*perCent)); 	// (35a)
        histFull[k].histFullHar[j].mHist_vEta[1]->Scale(-0.5/(meanIntegV4[j]*perCent) ); // (35b)
	histFull[k].histFullHar[j].mHist_vPt[0]->Scale(1./(meanIntegV2[j]*perCent)); // (35a)
        histFull[k].histFullHar[j].mHist_vPt[1]->Scale(-0.5/(meanIntegV4[j]*perCent)); // (35b)
      }

        if (cumulInteg1[j]<0.) {
         cout<<" Sel"<<k+1<<", <V**2> less than zero ! v"<<j+1<<" from 2 particle correlation failed."<<endl;
	 histFull[k].histFullHar[j].mHist_v2D[0]->Reset();
	 histFull[k].histFullHar[j].mHist_vEta[0]->Reset();
	 histFull[k].histFullHar[j].mHist_vPt[0]->Reset();
	}


        if (-1.*cumulInteg2[j]<0.) {
         cout<<" Sel"<<k+1<<", <V**4> less than zero ! v"<<j+1<<" from 4 particle correlation failed."<<endl;
      	 histFull[k].histFullHar[j].mHist_v2D[1]->Reset();
	 histFull[k].histFullHar[j].mHist_vEta[1]->Reset();
	 histFull[k].histFullHar[j].mHist_vPt[1]->Reset();
	}

    }// end of  for (int j = 0; j < nHars; j++) 




   /////////////  v1{3} calculation. mixed harmonics.


    for (int j = 0; j < nHars; j++) {

      if (j != 0) continue; //only j==0 makes sense for mixed har.
          
      char countHars[2];
      sprintf(countHars,"%d",j+1);
      
      cout<<" ========== Sel"<<k+1<<" Har"<<j+1<<" ===== v1{3} ======"<<endl;

      double mAvMult = 
      	histFull[k].histFullHar[j].mHistMultSum->GetBinContent(1)/
	histFull[k].histFullHar[j].mHistNEvent->GetBinContent(1);

      //      cout<<" mAveMult "<<mAvMult<<endl;

      double mAveMeanWgtSqr = // <w**2>
	histFull[k].histFullHar[j].mHistMeanWgtSqrSum->GetBinContent(1)/
	histFull[k].histFullHar[j].mHistNEvent->GetBinContent(1);

      cout<<" histFull[k].histFullHar[j].mHistMeanWgtSqrSum->GetBinContent(1) "<<histFull[k].histFullHar[j].mHistMeanWgtSqrSum->GetBinContent(1)<<endl;
      cout<<" histFull[k].histFullHar[j].mHistNEvent->GetBinContent(1) "<<histFull[k].histFullHar[j].mHistNEvent->GetBinContent(1)<<endl;


       double  CpqMix[nCumulMixHar_pMax][nCumulMixHar_qMax];//(30)
	  for (int pq = 0; pq < nCumulMixHar_pMax*nCumulMixHar_qMax; pq++) {
        	int pIndex = pq/nCumulMixHar_qMax;
                int qIndex = pq%nCumulMixHar_qMax;



        histFull[k].histFullHar[j].mCumulIntegG0Mix[pq] =
      histFull[k].histFullHar[j].mHistCumulIntegG0MixSum[pq]->GetBinContent(1)/
      histFull[k].histFullHar[j].mHistNEvent->GetBinContent(1);


        CpqMix[pIndex][qIndex]=mAvMult*(pow(histFull[k].histFullHar[j].mCumulIntegG0Mix[pq], 1./mAvMult) -1.);


	  }

	  double CpxMix[nCumulMixHar_pMax];
          double CpyMix[nCumulMixHar_pMax];


          for (int p =0; p<nCumulMixHar_pMax; p++){
	    CpxMix[p] = (1./(4.*r0Mix))*(CpqMix[p][0] - CpqMix[p][2]);
	    CpyMix[p] = (1./(4.*r0Mix))*(CpqMix[p][3] - CpqMix[p][1]);

	  }


          cumulIntegMix[j] = (1./(4.*r0Mix*r0Mix))*(
                        CpxMix[0]-CpyMix[1]-CpxMix[2]+CpyMix[3]
			+CpxMix[4]-CpyMix[5]-CpxMix[6]+CpyMix[7]);

	  //use v2{4}
       	  double  tempMeanV = pow(-1.*cumulInteg2[1]*mAveMeanWgtSqr*mAveMeanWgtSqr,1./4.); // <wgt*v2>
	  //use v2{2}
	  //  double  tempMeanV = pow(cumulInteg1[1]*mAveMeanWgtSqr,1./2.); // <wgt*v2>

          cout<<"mAveMeanWgtSqr "<<mAveMeanWgtSqr<<endl;
          cout<<" cmumulInteg2[1] "<<cumulInteg2[1]<<endl;
          cout<<" tempMeanV "<<tempMeanV<<endl;
	  double  tempMeanVMixSq = cumulIntegMix[j]/tempMeanV; //(24) 
 

	    /////////// following block is for calculating delta c{3} 's square
	    /////////// see Eq. 35.
	    //	    double chi1 = sqrt(tempMeanVMixSq)*sqrt(mAvMult); //(36)
	    //            double chi2 = tempMeanV*sqrt(mAvMult);
	    //            double deltaC3Sqr = (2. + 4.*chi1*chi1 + 2.*chi2*chi2 + 4.*chi1*chi1*chi2*chi2+chi1*chi1*chi1*chi1)/(2.*mAvMult*mAvMult*mAvMult*(histFull[k].histFullHar[j].mHistNEvent->GetBinContent(1)));
	    //
	    //            cout<<" C3              is "<<cumulIntegMix[j]<<endl;
	    //            cout<<" delta C3 square is "<<deltaC3Sqr<<endl;
	    /////////////////////////////////////////////////////////////
 

	histTitle = new TString("Flow_CumulMix_v2D_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	histFull[k].histFullHar[j].mHistMix_v2D = 
	  new TH2D(*(histFull[k].histFullHar[j].mHistCumulMix2D->
		     ProjectionXY(histTitle->Data(),"e")));
	histFull[k].histFullHar[j].mHistMix_v2D->SetTitle(histTitle->Data());
	histFull[k].histFullHar[j].mHistMix_v2D->SetXTitle((char*)xLabel.Data());
	histFull[k].histFullHar[j].mHistMix_v2D->SetYTitle("Pt (GeV)");
	histFull[k].histFullHar[j].mHistMix_v2D->SetZTitle("v (%)");
        histFull[k].histFullHar[j].mHistMix_v2D->Scale(1./profScale);

	delete histTitle;



	histTitle = new TString("Flow_CumulMix_vEta_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);
	histFull[k].histFullHar[j].mHistMix_vEta = 
	  new  TH1D(*(histFull[k].histFullHar[j].mHistCumulMixEta->
		      ProjectionX(histTitle->Data(),"e")));
	histFull[k].histFullHar[j].mHistMix_vEta->SetTitle(histTitle->Data());
	histFull[k].histFullHar[j].mHistMix_vEta->SetXTitle((char*)xLabel.Data());
	histFull[k].histFullHar[j].mHistMix_vEta->SetYTitle("v (%)");
        histFull[k].histFullHar[j].mHistMix_vEta->Scale(1./profScale);


	delete histTitle;
	




	histTitle = new TString("Flow_CumulMix_vPt_Sel");
	histTitle->Append(*countSels);
	histTitle->Append("_Har");
	histTitle->Append(*countHars);

	//		    histTitle->Append("_TPC");

   	  histTitle2 = new TString("Flow_CumulMix2D_Sel");
	  histTitle2->Append(*countSels);
	  histTitle2->Append("_Har");
	  histTitle2->Append(*countHars);




	histFull[k].histFullHar[j].mHistMix_vPt =
	  //myProjectionY((TProfile2D *)f->Get(histTitle2->Data()) , 0., 1. , 1, ((j==0)?1:0) );
		 new  TH1D(*(histFull[k].histFullHar[j].mHistCumulMixPt->ProjectionX(histTitle->Data(),"e")));

	histFull[k].histFullHar[j].mHistMix_vPt->SetTitle(histTitle->Data());
	histFull[k].histFullHar[j].mHistMix_vPt->SetXTitle("Pt (GeV)");
	histFull[k].histFullHar[j].mHistMix_vPt->SetYTitle("v (%)");
        histFull[k].histFullHar[j].mHistMix_vPt->Scale(1./profScale);

	delete histTitle;
        delete histTitle2;	



	  if (tempMeanVMixSq>0.){
    meanIntegVMix[j] = sqrt(tempMeanVMixSq);//(24) <w1v1>

	histFull[k].histFullHar[j].mHistMix_v2D->Scale(1./(tempMeanV*meanIntegVMix[j]*perCent)); 
	histFull[k].histFullHar[j].mHistMix_vEta->Scale(1./(tempMeanV*meanIntegVMix[j]*perCent));
	histFull[k].histFullHar[j].mHistMix_vPt->Scale(1./(tempMeanV*meanIntegVMix[j]*perCent)); 
        histFull[k].mHistMix_v->Scale(1./(tempMeanV*meanIntegVMix[j]*perCent)); 
       } else {
	 histFull[k].histFullHar[j].mHistMix_v2D->Reset();
	 histFull[k].histFullHar[j].mHistMix_vEta->Reset();
	 histFull[k].histFullHar[j].mHistMix_vPt->Reset();
	 histFull[k].mHistMix_v->Reset();
       cout<<"### <wgt*v1>**2 = "<<tempMeanVMixSq<<" < 0. 3-part mixed har ana. failed "<<endl;
       }



    } //end of Har loop for Mixed har.




    if (m_M==1) {
      
      TH1D* histOfMeanIntegV = new TH1D(*(histFull[k].mHist_v[0]));
      histOfMeanIntegV->Reset();
      
      TH1D* histOfMeanIntegV3 = new TH1D(*(histFull[k].mHist_v[1]));
      histOfMeanIntegV3->Reset();
      
      for (int j = 1; j < nHars+1; j++) {
	histOfMeanIntegV->SetBinContent(j, 1./(meanIntegV[j-1]*perCent));
	histOfMeanIntegV->SetBinError(j,0.);
	histOfMeanIntegV3->SetBinContent(j, -1./(meanIntegV3[j-1]*perCent));
	histOfMeanIntegV3->SetBinError(j,0.);
      }
      histFull[k].mHist_v[0]->Multiply(histOfMeanIntegV);
      histFull[k].mHist_v[1]->Multiply(histOfMeanIntegV3);
      
      //force to be zero if the value is "nan"
      for (int ord = 0; ord < nCumulDiffOrders; ord++){
	for (int j=0; j<histFull[k].mHist_v[ord]->GetNbinsX(); j++) {
          if ( !(histFull[k].mHist_v[ord]->GetBinContent(j) < FLT_MAX &&
                 histFull[k].mHist_v[ord]->GetBinContent(j) > -1.*FLT_MAX) ) {
	    histFull[k].mHist_v[ord]->SetBinContent(j,0.);
	    histFull[k].mHist_v[ord]->SetBinError(j,0.);
	  }
	}
      }

      for (int j = 1; j < nHars+1; j++) {
        if (histFull[k].mHist_v[0]->GetBinContent(j)< FLT_MAX &&
            histFull[k].mHist_v[0]->GetBinContent(j)> -1.*FLT_MAX) 
	cout << "##### 2-part v" << j << " = (" 
	     << histFull[k].mHist_v[0]->GetBinContent(j) 
	     <<" +/- "<< histFull[k].mHist_v[0]->GetBinError(j)<<" )"<<endl;
        if (histFull[k].mHist_v[1]->GetBinContent(j)< FLT_MAX &&
            histFull[k].mHist_v[1]->GetBinContent(j)> -1.*FLT_MAX) 
	cout << "##### 4-part v" << j << " = (" 
	     << histFull[k].mHist_v[1]->GetBinContent(j) 
	     <<" +/- "<< histFull[k].mHist_v[1]->GetBinError(j)<<" )"<<endl;
      }
      
      delete histOfMeanIntegV; 
      delete histOfMeanIntegV3;
      
    } else if (m_M==2) {
      
      TH1D* histOfMeanIntegV2 = new TH1D(*(histFull[k].mHist_v[0]));
      histOfMeanIntegV2->Reset();
      
      TH1D* histOfMeanIntegV4 = new TH1D(*(histFull[k].mHist_v[1]));
      histOfMeanIntegV4->Reset();
      
      for (int j = 1; j < nHars+1; j++) {
	histOfMeanIntegV2->SetBinContent(j, 1./(meanIntegV2[j-1]*perCent));
	histOfMeanIntegV2->SetBinError(j,0.);
	histOfMeanIntegV4->SetBinContent(j, -0.5/(meanIntegV4[j-1]*perCent));
	histOfMeanIntegV4->SetBinError(j,0.);
      }
      histFull[k].mHist_v[0]->Multiply(histOfMeanIntegV2);
      histFull[k].mHist_v[1]->Multiply(histOfMeanIntegV4);

      //force to be zero if the value is "nan"
      for (int ord = 0; ord < nCumulDiffOrders; ord++){
	for (int j=0; j<histFull[k].mHist_v[ord]->GetNbinsX(); j++) {
          if ( !(histFull[k].mHist_v[ord]->GetBinContent(j) < FLT_MAX &&
                 histFull[k].mHist_v[ord]->GetBinContent(j) > -1.*FLT_MAX) ){
	    histFull[k].mHist_v[ord]->SetBinContent(j,0.);
	    histFull[k].mHist_v[ord]->SetBinError(j,0.);
	  }
	}
      }

      for (int j = 1; j < nHars+1; j++) {

        if (histFull[k].mHist_v[0]->GetBinContent(j)< FLT_MAX &&
            histFull[k].mHist_v[0]->GetBinContent(j)> -1.*FLT_MAX) 
	cout << "##### 2-part v" << j << " = (" 
	     << histFull[k].mHist_v[0]->GetBinContent(j) 
	     <<") +/- "<< histFull[k].mHist_v[0]->GetBinError(j)<<endl;
        if (histFull[k].mHist_v[1]->GetBinContent(j)< FLT_MAX &&
            histFull[k].mHist_v[1]->GetBinContent(j)> -1.*FLT_MAX) 
	cout << "##### 4-part v" << j << " = (" 
	     << histFull[k].mHist_v[1]->GetBinContent(j) 
	     <<") +/- "<< histFull[k].mHist_v[1]->GetBinError(j)<<endl;
      }
      
      delete histOfMeanIntegV2; 
      delete histOfMeanIntegV4;
    }

  }



  // ============== write out ====================

  f->cd();

  for (int k = 0; k < nSels; k++) {

   for (int ord = 0; ord < nCumulDiffOrders; ord++) {
     histFull[k].mHist_v[ord]->Write(histFull[k].mHist_v[ord]->GetTitle(),TObject::kOverwrite | TObject::kSingleKey);
   }

     histFull[k].mHistMix_v->Write(histFull[k].mHistMix_v->GetTitle(),TObject::kOverwrite | TObject::kSingleKey);


    for (int j = 0; j < nHars; j++) {
      cout<<" writting ........................."<<endl;
      for (int ord = 0; ord < nCumulDiffOrders; ord++){
	histFull[k].histFullHar[j].mHist_v2D[ord]->Write(histFull[k].histFullHar[j].mHist_v2D[ord]->GetTitle(),TObject::kOverwrite | TObject::kSingleKey);
	histFull[k].histFullHar[j].mHist_vEta[ord]->Write(histFull[k].histFullHar[j].mHist_vEta[ord]->GetTitle(),TObject::kOverwrite | TObject::kSingleKey);
	histFull[k].histFullHar[j].mHist_vPt[ord]->Write(histFull[k].histFullHar[j].mHist_vPt[ord]->GetTitle(),TObject::kOverwrite | TObject::kSingleKey);
      }


      if (j==0){
        cout<<" j "<<j<<" k "<<k<<endl;   
	cout<<"  histFull[k].histFullHar[j].mHistMix_v2D "<<histFull[k].histFullHar[j].mHistMix_v2D->GetTitle()<<endl;

      (histFull[k].histFullHar[j].mHistMix_v2D)->Write((histFull[k].histFullHar[j].mHistMix_v2D)->GetTitle(),TObject::kOverwrite | TObject::kSingleKey);
      (histFull[k].histFullHar[j].mHistMix_vEta)->Write((histFull[k].histFullHar[j].mHistMix_vEta)->GetTitle(),TObject::kOverwrite | TObject::kSingleKey);
      (histFull[k].histFullHar[j].mHistMix_vPt)->Write((histFull[k].histFullHar[j].mHistMix_vPt)->GetTitle(),TObject::kOverwrite | TObject::kSingleKey);

      }

   }
 }
  //=============================================

  f->Close();

}
