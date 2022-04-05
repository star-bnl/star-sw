void alignTPC(){

  gSystem->Load("libPhysics");
  gStyle->SetFrameBorderMode(0);
  gStyle->SetPadBorderMode(0);

  TVector3 EAO[25], EBO[25], EDO[25]; 
  TVector3 WAO[25], WBO[25], WDO[25]; 

  TVector3 tempVect(0,0,0);
  TVector3 crossVect(0,0,0);
  TVector3 ortVect(0,0,0);
  TVector3 uniVect(0,0,0);
  TVector3 nullVect(0,0,0);

  TVector3 axisSurveyX(1.,0.,0.);
  TVector3 axisSurveyY(0.,1.,0.);
  TVector3 axisSurveyZ(0.,0.,1.);

  double tAngle =0.;

  EAO[13].SetXYZ(-0.27465, -2.31703, 0.47066);
  EAO[14].SetXYZ(-0.47431, -2.31705, 0.27091);
  EAO[15].SetXYZ(-0.54726, -2.31713, -0.00182);
  EAO[16].SetXYZ(-0.47394, -2.31718, -0.27460);
  EAO[17].SetXYZ(-0.27426, -2.31711, -0.47420);
  EAO[18].SetXYZ(-0.00134, -2.31699, -0.54718);
  EAO[19].SetXYZ(0.27141, -2.31675, -0.47399);
  EAO[20].SetXYZ(0.47104, -2.31663, -0.27427);
  EAO[21].SetXYZ(0.54403, -2.31668, -0.00137);
  EAO[22].SetXYZ(0.47082, -2.31674, 0.27141);
  EAO[23].SetXYZ(0.27101, -2.31683, 0.47091);
  EAO[24].SetXYZ(-0.00182, -2.31695, 0.54387);

  EBO[13].SetXYZ( -0.59588, -2.31725, 1.02658);
  EBO[14].SetXYZ(-1.03053, -2.31719, 0.59175);
  EBO[15].SetXYZ(-1.18926, -2.31731, -0.00211);
  EBO[16].SetXYZ(-1.03017, -2.31692, -0.59588);
  EBO[17].SetXYZ(-0.59496, -2.31720, -1.03037);
  EBO[18].SetXYZ(-0.00110, -2.31688, -1.18911);
  EBO[19].SetXYZ(0., 0., 0.);
  EBO[20].SetXYZ(1.02724, -2.31631, -0.59508);
  EBO[21].SetXYZ(1.18602, -2.31632, -0.00115);
  EBO[22].SetXYZ(1.02673, -2.31650, 0.59253);
  EBO[23].SetXYZ(0.59184, -2.31677, 1.02697);
  EBO[24].SetXYZ(-0.00208, -2.31696, 1.18566);

  EDO[13].SetXYZ(-0.95525, -2.31701, 1.64841);
  EDO[14].SetXYZ(0., 0., 0.);
  EDO[15].SetXYZ(0., 0., 0.);
  EDO[16].SetXYZ(-1.65192, -2.31734, -0.95513);
  EDO[17].SetXYZ(-0.95400, -2.31674, -1.65246);
  EDO[18].SetXYZ(0., 0., 0.);
  EDO[19].SetXYZ(0.95194, -2.31608, -1.65172);
  EDO[20].SetXYZ(1.64927, -2.31585, -0.95392);
  EDO[21].SetXYZ(0., 0., 0.);
  EDO[22].SetXYZ(0., 0., 0.);
  EDO[23].SetXYZ(0.95070, -2.31622, 1.64905);
  EDO[24].SetXYZ(-0.00234, -2.31674, 1.90405);

  WAO[1].SetXYZ(0., 0., 0.);
  WAO[2].SetXYZ(0.46923, 2.31308, 0.27104);
  WAO[3].SetXYZ(0.54225, 2.31310, -0.00181);
  WAO[4].SetXYZ(0.46903, 2.31310, -0.27453);
  WAO[5].SetXYZ(0.26930, 2.31289, -0.47419);
  WAO[6].SetXYZ(-0.00342, 2.31277, -0.54732);
  WAO[7].SetXYZ(-0.27622, 2.31275, -0.47430);
  WAO[8].SetXYZ(-0.47585, 2.31267, -0.27457);
  WAO[9].SetXYZ(-0.54889, 2.31278, -0.00186);
  WAO[10].SetXYZ(-0.47583, 2.31282, 0.27090);
  WAO[11].SetXYZ(-0.27613, 2.31298, 0.47050);
  WAO[12].SetXYZ(-0.00335, 2.31298, 0.54363);

  WBO[1].SetXYZ(0.59054, 2.31298, 1.02659);
  WBO[2].SetXYZ(1.02529, 2.31314, 0.59191);
  WBO[3].SetXYZ(1.18429, 2.31314, -0.00186);
  WBO[4].SetXYZ(1.02510, 2.31299, -0.59558);
  WBO[5].SetXYZ(0.59031, 2.31274, -1.03022);
  WBO[6].SetXYZ(-0.00346, 2.31251, -1.18937);
  WBO[7].SetXYZ(-0.59727, 2.31251, -1.03034);
  WBO[8].SetXYZ(-1.03189, 2.31226, -0.59552);
  WBO[9].SetXYZ(-1.19092, 2.31227, -0.00178);
  WBO[10].SetXYZ(-1.03191, 2.31243, 0.59190);
  WBO[11].SetXYZ(-0.59707, 2.31270, 1.02648);
  WBO[12].SetXYZ(-0.00330, 2.31279, 1.18557);

  WDO[1].SetXYZ(0.94962, 2.31289, 1.64844);
  WDO[2].SetXYZ(1.64729, 2.31318, 0.95101);
  WDO[3].SetXYZ(1.90240, 2.31331, -0.00176);
  WDO[4].SetXYZ(0., 0., 0.);
  WDO[5].SetXYZ(0.94945, 2.31280, -1.65202);
  WDO[6].SetXYZ(-0.00344, 2.31223, -1.90736);
  WDO[7].SetXYZ(-0.95633, 2.31209, -1.65220);
  WDO[8].SetXYZ(0., 0., 0.);
  WDO[9].SetXYZ(0., 0., 0.);
  WDO[10].SetXYZ(-1.65380, 2.31169, 0.95098);
  WDO[11].SetXYZ(0., 0., 0.);
  WDO[12].SetXYZ(0., 0., 0.);

  TH3F *tpcCenterInSurvey = new TH3F("tpcCenterInSurvey","tpcCenterInSurvey",101,-10.1,10.1,101,-10.1,10.1,101,-10.1,10.1);
  TH1F *tpcCenterInSurveyX = new TH1F("tpcCenterInSurveyX","tpcCenterInSurveyX",201,-10.05,10.05);
  tpcCenterInSurveyX->SetXTitle("X_{survey} (mm)");
  TH1F *tpcCenterInSurveyY = new TH1F("tpcCenterInSurveyY","tpcCenterInSurveyY",201,-10.05,10.05);
  tpcCenterInSurveyY->SetXTitle("Y_{survey} (mm)");
  TH1F *tpcCenterInSurveyZ = new TH1F("tpcCenterInSurveyZ","tpcCenterInSurveyZ",201,-10.05,10.05);
  tpcCenterInSurveyZ->SetXTitle("Z_{survey} (mm)");

  TH3F *tpcWTiltInSurvey = new TH3F("tpcWTiltInSurvey","tpcWTiltInSurvey",101,-0.00101,0.00101,101,-0.00101,0.00101,101,-0.00101,0.00101);
  TH1F *tpcWTiltInSurveyX = new TH1F("tpcWTiltInSurveyX","tpcWTiltInSurveyX",201,-0.001005,0.001005);
  TH1F *tpcWTiltInSurveyY = new TH1F("tpcWTiltInSurveyY","tpcWTiltInSurveyY",201,-0.001005,0.001005);
  TH1F *tpcWTiltInSurveyZ = new TH1F("tpcWTiltInSurveyZ","tpcWTiltInSurveyZ",201,-0.001005,0.001005);

  TH3F *tpcETiltInSurvey = new TH3F("tpcETiltInSurvey","tpcETiltInSurvey",101,-1010,1010,101,-1010,1010,101,-1010,1010);
  TH1F *tpcETiltInSurveyX = new TH1F("tpcETiltInSurveyX","tpcETiltInSurveyX",201,-1005,1005);
  TH1F *tpcETiltInSurveyY = new TH1F("tpcETiltInSurveyY","tpcETiltInSurveyY",201,-1005,1005);
  TH1F *tpcETiltInSurveyZ = new TH1F("tpcETiltInSurveyZ","tpcETiltInSurveyZ",201,-1005,1005);

  TH1F *tpcWXInSurveyXY = new TH1F("tpcWXInSurveyXY","tpcWXInSurveyXY",201,-1.005,1.005);
  tpcWXInSurveyXY->SetXTitle("Angle (mrad)");
  TH1F *tpcWXInSurveyXZ = new TH1F("tpcWXInSurveyXZ","tpcWXInSurveyXZ",201,-1.005,1.005);
  tpcWXInSurveyXZ->SetXTitle("Angle (mrad)");

  TH1F *tpcWYInSurveyXY = new TH1F("tpcWYInSurveyXY","tpcWYInSurveyXY",201,-1.005,1.005);
  tpcWYInSurveyXY->SetXTitle("Angle (mrad)");
  TH1F *tpcWYInSurveyYZ = new TH1F("tpcWYInSurveyYZ","tpcWYInSurveyYZ",201,-1.005,1.005);
  tpcWYInSurveyYZ->SetXTitle("Angle (mrad)");

  TH1F *tpcWZInSurveyXZ = new TH1F("tpcWZInSurveyXZ","tpcWZInSurveyXZ",201,-1.005,1.005);
  TH1F *tpcWZInSurveyYZ = new TH1F("tpcWZInSurveyYZ","tpcWZInSurveyYZ",201,-1.005,1.005);

  TH1F *tpcEXInSurveyXY = new TH1F("tpcEXInSurveyXY","tpcEXInSurveyXY",201,-1.005,1.005);
  TH1F *tpcEXInSurveyXZ = new TH1F("tpcEXInSurveyXZ","tpcEXInSurveyXZ",201,-1.005,1.005);

  TH1F *tpcEYInSurveyXY = new TH1F("tpcEYInSurveyXY","tpcEYInSurveyXY",201,-1.005,1.005);
  tpcEYInSurveyXY->SetXTitle("Angle (mrad)");
  TH1F *tpcEYInSurveyYZ = new TH1F("tpcEYInSurveyYZ","tpcEYInSurveyYZ",201,-1.005,1.005);
  tpcEYInSurveyYZ->SetXTitle("Angle (mrad)");

  TH1F *tpcEZInSurveyXZ = new TH1F("tpcEZInSurveyXZ","tpcEZInSurveyXZ",201,-1.005,1.005);
  TH1F *tpcEZInSurveyYZ = new TH1F("tpcEZInSurveyYZ","tpcEZInSurveyYZ",201,-1.005,1.005);



  //
  // To calculate the center of the TPC (perfect cylinder, 12 perfectly symetric sectors at each end)
  // Calculate the midle of a line going from one end to the other through the center of the TPC
  // W1-E17, W2-E16, W3-E15, W4-E14, W5-E13
  // W6-E24, W7-E23, W8-E22, W9-E21, W10-E20, W11-E19, W12-E18
  for(int iSec=2; iSec<=5; iSec++){ // No WAO[1] data
    tempVect.SetXYZ(0.,0.,0.);  
    tempVect += WAO[iSec]; tempVect += EAO[18-iSec]; tempVect *= 0.5; tempVect *= 1000;
    tpcCenterInSurvey->Fill(tempVect.X(),tempVect.Y(),tempVect.Z());
    tpcCenterInSurveyX->Fill(tempVect.X());
    tpcCenterInSurveyY->Fill(tempVect.Y());
    tpcCenterInSurveyZ->Fill(tempVect.Z());
    printf("%f\n",tempVect.X());
  }
  for(int iSec=6; iSec<=12; iSec++){
    tempVect.SetXYZ(0.,0.,0.);  
    tempVect += WAO[iSec]; tempVect += EAO[30-iSec]; tempVect *= 0.5; tempVect *= 1000;
    tpcCenterInSurvey->Fill(tempVect.X(),tempVect.Y(),tempVect.Z());
    tpcCenterInSurveyX->Fill(tempVect.X());
    tpcCenterInSurveyY->Fill(tempVect.Y());
    tpcCenterInSurveyZ->Fill(tempVect.Z());
    printf("%f\n",tempVect.X());
  }

  for(int iSec=1; iSec<=5; iSec++){ 
    tempVect.SetXYZ(0.,0.,0.);  
    tempVect += WBO[iSec]; tempVect += EBO[18-iSec]; tempVect *= 0.5; tempVect *= 1000;
    tpcCenterInSurvey->Fill(tempVect.X(),tempVect.Y(),tempVect.Z());
    tpcCenterInSurveyX->Fill(tempVect.X());
    tpcCenterInSurveyY->Fill(tempVect.Y());
    tpcCenterInSurveyZ->Fill(tempVect.Z());
    printf("%f\n",tempVect.X());
  }
  for(int iSec=6; iSec<=12; iSec++){
    if(iSec!=11){ // No EBO[19] data
      tempVect.SetXYZ(0.,0.,0.);  
      tempVect += WBO[iSec]; tempVect += EBO[30-iSec]; tempVect *= 0.5; tempVect *= 1000;
      tpcCenterInSurvey->Fill(tempVect.X(),tempVect.Y(),tempVect.Z());
      tpcCenterInSurveyX->Fill(tempVect.X());
      tpcCenterInSurveyY->Fill(tempVect.Y());
      tpcCenterInSurveyZ->Fill(tempVect.Z());
      printf("%f\n",tempVect.X());
    }
  }

  for(int iSec=1; iSec<=5; iSec++){ 
    if(iSec!=3&&iSec!=4){ // No EDO[19] data
      tempVect.SetXYZ(0.,0.,0.);  
      tempVect += WDO[iSec]; tempVect += EDO[18-iSec]; tempVect *= 0.5; tempVect *= 1000;
      tpcCenterInSurvey->Fill(tempVect.X(),tempVect.Y(),tempVect.Z());
      tpcCenterInSurveyX->Fill(tempVect.X());
      tpcCenterInSurveyY->Fill(tempVect.Y());
      tpcCenterInSurveyZ->Fill(tempVect.Z());
      printf("%f\n",tempVect.X());
    }
  }
  for(int iSec=6; iSec<=12; iSec++){
    if(iSec!=8&&iSec!=9&&iSec!=11&&iSec!=12){ // No EDO[19] data
      tempVect.SetXYZ(0.,0.,0.);  
      tempVect += WDO[iSec]; tempVect += EDO[30-iSec]; tempVect *= 0.5; tempVect *= 1000;
      tpcCenterInSurvey->Fill(tempVect.X(),tempVect.Y(),tempVect.Z());
      tpcCenterInSurveyX->Fill(tempVect.X());
      tpcCenterInSurveyY->Fill(tempVect.Y());
      tpcCenterInSurveyZ->Fill(tempVect.Z());
      printf("%f\n",tempVect.X());
    }
  }

  //
  // To calculate the angle TPC with respect to the Surveynet,
  // Calculate the normal vector of a line in the endcap plane
  // 
  // A targets
  for(int iSec=1; iSec<=10; iSec++){
    if(WAO[iSec]!=nullVect){
      for(int jSec=iSec+1; jSec<=11; jSec++){
	if(WAO[jSec]!=nullVect){
	  tempVect.SetXYZ(0.,0.,0.);  
	  tempVect += WAO[iSec];  tempVect -= WAO[jSec];
	  for(int kSec=jSec+1; kSec<=12; kSec++){
	    if(WAO[kSec]!=nullVect){	
	      crossVect.SetXYZ(0.,0.,0.);  
	      crossVect += WAO[jSec];  crossVect -= WAO[kSec];
	      crossVect = crossVect.Cross(tempVect);
	      uniVect = crossVect.Unit();
	      //printf("(%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());

	      tAngle = TMath::ATan2(uniVect.X(),uniVect.Y());
	      if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
	      if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
	      tAngle *= 1e3;
	      if (tAngle>10000) printf("WAO (%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());
	      //printf("In XY plane, Angle to Y %f\n",tAngle);
	      tpcWYInSurveyXY->Fill(tAngle);
	      tAngle = TMath::ATan2(uniVect.Z(),uniVect.Y());
	      if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
	      if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
	      tAngle *= 1e3;
	      if (tAngle>10000) printf("WAO (%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());
	      //printf("In YZ plane, Angle to Y %f\n",tAngle);
	      tpcWYInSurveyYZ->Fill(tAngle);
	    }
	  }
	}
      }
    }
  }

  for(int iSec=1; iSec<=9; iSec++){
    if(WAO[iSec]!=nullVect){
      for(int jSec=iSec+1; jSec<=10; jSec++){
	if(WAO[jSec]!=nullVect){
	  tempVect.SetXYZ(0.,0.,0.);  
	  tempVect += WAO[iSec];  tempVect -= WAO[jSec];
	  for(int kSec=jSec+1; kSec<=11; kSec++){
	    if(WAO[kSec]!=nullVect){	
	      for(int lSec=kSec+1; lSec<=12; lSec++){
		if(WAO[lSec]!=nullVect){	
		  crossVect.SetXYZ(0.,0.,0.);  
		  crossVect += WAO[kSec];  crossVect -= WAO[lSec];
		  crossVect = crossVect.Cross(tempVect);
		  uniVect = crossVect.Unit();
		  
		  tAngle = TMath::ATan2(uniVect.X(),uniVect.Y());
		  if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
		  if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
		  tAngle *= 1e3;
		  if (tAngle>10000) printf("WAO (%d,%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,lSec,uniVect.X(),uniVect.Y(),uniVect.Z());
		  //printf("In XY plane, Angle to Y %f\n",tAngle);
		  tpcWYInSurveyXY->Fill(tAngle);
		  tAngle = TMath::ATan2(uniVect.Z(),uniVect.Y());
		  if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
		  if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
		  tAngle *= 1e3;
		  if (tAngle>10000) printf("WAO (%d,%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,lSec,uniVect.X(),uniVect.Y(),uniVect.Z());
		  //printf("In YZ plane, Angle to Y %f\n",tAngle);
		  tpcWYInSurveyYZ->Fill(tAngle);
		}
	      }
	    }
	  }
	}
      }
    }
  }
  
  // B targets
  for(int iSec=1; iSec<=10; iSec++){
    if(WBO[iSec]!=nullVect){
      for(int jSec=iSec+1; jSec<=11; jSec++){
	if(WBO[jSec]!=nullVect){
	  tempVect.SetXYZ(0.,0.,0.);  
	  tempVect += WBO[iSec];  tempVect -= WBO[jSec];
	  for(int kSec=jSec+1; kSec<=12; kSec++){
	    if(WBO[kSec]!=nullVect){	
	      crossVect.SetXYZ(0.,0.,0.); 
	      crossVect += WBO[jSec];  crossVect -= WBO[kSec];
	      crossVect = crossVect.Cross(tempVect);
	      uniVect = crossVect.Unit();

	      tAngle = TMath::ATan2(uniVect.X(),uniVect.Y());
	      if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
	      if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
	      tAngle *= 1e3;
	      if (tAngle>10000) printf("WBO (%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());
	      //printf("In XY plane, Angle to Y %f\n",tAngle);
	      tpcWYInSurveyXY->Fill(tAngle);
	      tAngle = TMath::ATan2(uniVect.Z(),uniVect.Y());
	      if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
	      if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
	      tAngle *= 1e3;
	      if (tAngle>10000) printf("WBO (%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());
	      //printf("In YZ plane, Angle to Y %f\n",tAngle);
	      tpcWYInSurveyYZ->Fill(tAngle);
	    }
	  }
	}
      }
    }
  }

  for(int iSec=1; iSec<=9; iSec++){
    if(WBO[iSec]!=nullVect){
      for(int jSec=iSec+1; jSec<=10; jSec++){
	if(WBO[jSec]!=nullVect){
	  tempVect.SetXYZ(0.,0.,0.);  
	  tempVect += WBO[iSec];  tempVect -= WBO[jSec];
	  for(int kSec=jSec+1; kSec<=11; kSec++){
	    if(WBO[kSec]!=nullVect){	
	      for(int lSec=kSec+1; lSec<=12; lSec++){
		if(WBO[lSec]!=nullVect){	
		  crossVect.SetXYZ(0.,0.,0.);  
		  crossVect += WBO[kSec];  crossVect -= WBO[lSec];
		  crossVect = crossVect.Cross(tempVect);
		  uniVect = crossVect.Unit();

		  tAngle = TMath::ATan2(uniVect.X(),uniVect.Y());
		  if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
		  if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
		  tAngle *= 1e3;
		  if (tAngle>10000) printf("WBO (%d,%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,lSec,uniVect.X(),uniVect.Y(),uniVect.Z());
		  //printf("In XY plane, Angle to Y %f\n",tAngle);
		  tpcWYInSurveyXY->Fill(tAngle);
		  tAngle = TMath::ATan2(uniVect.Z(),uniVect.Y());
		  if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
		  if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
		  tAngle *= 1e3;
		  if (tAngle>10000) printf("WBO (%d,%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,lSec,uniVect.X(),uniVect.Y(),uniVect.Z());
		  //printf("In YZ plane, Angle to Y %f\n",tAngle);
		  tpcWYInSurveyYZ->Fill(tAngle);
		}
	      }
	    }
	  }
	}
      }
    }
  }

  // D targets
  for(int iSec=1; iSec<=10; iSec++){
    if(WDO[iSec]!=nullVect){
      for(int jSec=iSec+1; jSec<=11; jSec++){
	if(WDO[jSec]!=nullVect){
	  tempVect.SetXYZ(0.,0.,0.);  
	  tempVect += WDO[iSec];  tempVect -= WDO[jSec];
	  for(int kSec=jSec+1; kSec<=12; kSec++){
	    if(WDO[kSec]!=nullVect){	
	      crossVect.SetXYZ(0.,0.,0.);  
	      crossVect += WDO[jSec];  crossVect -= WDO[kSec];
	      crossVect = crossVect.Cross(tempVect);
	      uniVect = crossVect.Unit();

	      tAngle = TMath::ATan2(uniVect.X(),uniVect.Y());
	      if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
	      if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
	      tAngle *= 1e3;
	      if (tAngle>10000) printf("WDO (%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());
	      //printf("In XY plane, Angle to Y %f\n",tAngle);
	      tpcWYInSurveyXY->Fill(tAngle);
	      tAngle = TMath::ATan2(uniVect.Z(),uniVect.Y());
	      if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
	      if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
	      tAngle *= 1e3;
	      if (tAngle>10000) printf("WDO (%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());
	      //printf("In YZ plane, Angle to Y %f\n",tAngle);
	      tpcWYInSurveyYZ->Fill(tAngle);
	    }
	  }
	}
      }
    }
  }

  for(int iSec=1; iSec<=9; iSec++){
    if(WDO[iSec]!=nullVect){
      for(int jSec=iSec+1; jSec<=10; jSec++){
	if(WDO[jSec]!=nullVect){
	  tempVect.SetXYZ(0.,0.,0.);  
	  tempVect += WDO[iSec];  tempVect -= WDO[jSec];
	  for(int kSec=jSec+1; kSec<=11; kSec++){
	    if(WDO[kSec]!=nullVect){	
	      for(int lSec=kSec+1; lSec<=12; lSec++){
		if(WDO[lSec]!=nullVect){	
		  crossVect.SetXYZ(0.,0.,0.);  
		  crossVect += WDO[kSec];  crossVect -= WDO[lSec];
		  crossVect = crossVect.Cross(tempVect);
		  uniVect = crossVect.Unit();
		  
		  tAngle = TMath::ATan2(uniVect.X(),uniVect.Y());
		  if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
		  if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
		  tAngle *= 1e3;
		  if (tAngle>10000) printf("WDO (%d,%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,lSec,uniVect.X(),uniVect.Y(),uniVect.Z());
		  //printf("In XY plane, Angle to Y %f\n",tAngle);
		  tpcWYInSurveyXY->Fill(tAngle);
		  tAngle = TMath::ATan2(uniVect.Z(),uniVect.Y());
		  if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
		  if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
		  tAngle *= 1e3;
		  if (tAngle>10000) printf("WDO (%d,%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,lSec,uniVect.X(),uniVect.Y(),uniVect.Z());
		  //printf("In YZ plane, Angle to Y %f\n",tAngle);
		  tpcWYInSurveyYZ->Fill(tAngle);
		}
	      }
	    }
	  }
	}
      }
    }
  }


  // East
  // A targets
  for(int iSec=13; iSec<=22; iSec++){
    if(EAO[iSec]!=nullVect){
      for(int jSec=iSec+1; jSec<=23; jSec++){
	if(EAO[jSec]!=nullVect){
	  tempVect.SetXYZ(0.,0.,0.);  
	  tempVect += EAO[iSec];  tempVect -= EAO[jSec];
	  for(int kSec=jSec+1; kSec<=24; kSec++){
	    if(EAO[kSec]!=nullVect){	
	      crossVect.SetXYZ(0.,0.,0.);  
	      crossVect += EAO[jSec];  crossVect -= EAO[kSec];
	      crossVect = crossVect.Cross(tempVect);
	      uniVect = crossVect.Unit();

	      tAngle = TMath::ATan2(uniVect.X(),uniVect.Y());
	      if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
	      if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
	      tAngle *= 1e3;
	      if (tAngle>10000) printf("EAO (%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());
	      //printf("In XY plane, Angle to Y %f\n",tAngle);
	      tpcEYInSurveyXY->Fill(tAngle);
	      tAngle = TMath::ATan2(uniVect.Z(),uniVect.Y());
	      if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
	      if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
	      tAngle *= 1e3;
	      if (tAngle>10000) printf("EAO (%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());
	      //printf("In YZ plane, Angle to Y %f\n",tAngle);
	      tpcEYInSurveyYZ->Fill(tAngle);
	    }
	  }
	}
      }
    }
  }

  for(int iSec=13; iSec<=21; iSec++){
    if(EAO[iSec]!=nullVect){
      for(int jSec=iSec+1; jSec<=22; jSec++){
	if(EAO[jSec]!=nullVect){
	  tempVect.SetXYZ(0.,0.,0.);  
	  tempVect += EAO[iSec];  tempVect -= EAO[jSec];
	  for(int kSec=jSec+1; kSec<=23; kSec++){
	    if(EAO[kSec]!=nullVect){	
	      for(int lSec=kSec+1; lSec<=24; lSec++){
		if(EAO[lSec]!=nullVect){	
		  crossVect.SetXYZ(0.,0.,0.);  
		  crossVect += EAO[kSec];  crossVect -= EAO[lSec];
		  crossVect = crossVect.Cross(tempVect);
		  uniVect = crossVect.Unit();
		  
		  tAngle = TMath::ATan2(uniVect.X(),uniVect.Y());
		  if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
		  if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
		  tAngle *= 1e3;
		  if (tAngle>10000) printf("EAO (%d,%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,lSec,uniVect.X(),uniVect.Y(),uniVect.Z());
		  //printf("In XY plane, Angle to Y %f\n",tAngle);
		  tpcEYInSurveyXY->Fill(tAngle);
		  tAngle = TMath::ATan2(uniVect.Z(),uniVect.Y());
		  if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
		  if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
		  tAngle *= 1e3;
		  if (tAngle>10000) printf("EAO (%d,%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,lSec,uniVect.X(),uniVect.Y(),uniVect.Z());
		  //printf("In YZ plane, Angle to Y %f\n",tAngle);
		  tpcEYInSurveyYZ->Fill(tAngle);
		}
	      }
	    }
	  }
	}
      }
    }
  }

  // B targets
  for(int iSec=13; iSec<=22; iSec++){
    if(EBO[iSec]!=nullVect){
      for(int jSec=iSec+1; jSec<=23; jSec++){
	if(EBO[jSec]!=nullVect){
	  tempVect.SetXYZ(0.,0.,0.);  
	  tempVect += EBO[iSec];  tempVect -= EBO[jSec];
	  for(int kSec=jSec+1; kSec<=24; kSec++){
	    if(EBO[kSec]!=nullVect){	
	      crossVect.SetXYZ(0.,0.,0.);  
	      crossVect += EBO[jSec];  crossVect -= EBO[kSec];
	      crossVect = crossVect.Cross(tempVect);
	      uniVect = crossVect.Unit();

	      tAngle = TMath::ATan2(uniVect.X(),uniVect.Y());
	      if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
	      if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
	      tAngle *= 1e3;
	      if (tAngle>10000) printf("EBO (%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());
	      //printf("In XY plane, Angle to Y %f\n",tAngle);
	      tpcEYInSurveyXY->Fill(tAngle);
	      tAngle = TMath::ATan2(uniVect.Z(),uniVect.Y());
	      if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
	      if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
	      tAngle *= 1e3;
	      if (tAngle>10000) printf("EBO (%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());
	      //printf("In YZ plane, Angle to Y %f\n",tAngle);
	      tpcEYInSurveyYZ->Fill(tAngle);
	    }
	  }
	}
      }
    }
  }

  for(int iSec=13; iSec<=21; iSec++){
    if(EBO[iSec]!=nullVect){
      for(int jSec=iSec+1; jSec<=22; jSec++){
	if(EBO[jSec]!=nullVect){
	  tempVect.SetXYZ(0.,0.,0.);  
	  tempVect += EBO[iSec];  tempVect -= EBO[jSec];
	  for(int kSec=jSec+1; kSec<=23; kSec++){
	    if(EBO[kSec]!=nullVect){	
	      for(int lSec=kSec+1; lSec<=24; lSec++){
		if(EBO[lSec]!=nullVect){	
		  crossVect.SetXYZ(0.,0.,0.);  
		  crossVect += EBO[kSec];  crossVect -= EBO[lSec];
		  crossVect = crossVect.Cross(tempVect);
		  uniVect = crossVect.Unit();
		  
		  tAngle = TMath::ATan2(uniVect.X(),uniVect.Y());
		  if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
		  if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
		  tAngle *= 1e3;
		  if (tAngle>10000) printf("EBO (%d,%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,lSec,uniVect.X(),uniVect.Y(),uniVect.Z());
		  //printf("In XY plane, Angle to Y %f\n",tAngle);
		  tpcEYInSurveyXY->Fill(tAngle);
		  tAngle = TMath::ATan2(uniVect.Z(),uniVect.Y());
		  if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
		  if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
		  tAngle *= 1e3;
		  if (tAngle>10000) printf("EBO (%d,%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,lSec,uniVect.X(),uniVect.Y(),uniVect.Z());
		  //printf("In YZ plane, Angle to Y %f\n",tAngle);
		  tpcEYInSurveyYZ->Fill(tAngle);
		}
	      }
	    }
	  }
	}
      }
    }
  }

  // D targets
  for(int iSec=13; iSec<=22; iSec++){
    if(EDO[iSec]!=nullVect){
      for(int jSec=iSec+1; jSec<=23; jSec++){
	if(EDO[jSec]!=nullVect){
	  tempVect.SetXYZ(0.,0.,0.);  
	  tempVect += EDO[iSec];  tempVect -= EDO[jSec];
	  for(int kSec=jSec+1; kSec<=24; kSec++){
	    if(EDO[kSec]!=nullVect){	
	      crossVect.SetXYZ(0.,0.,0.);  
	      crossVect += EDO[jSec];  crossVect -= EDO[kSec];
	      crossVect = crossVect.Cross(tempVect);
	      uniVect = crossVect.Unit();

	      tAngle = TMath::ATan2(uniVect.X(),uniVect.Y());
	      if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
	      if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
	      tAngle *= 1e3;
	      if (tAngle>10000) printf("EDO (%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());
	      //printf("In XY plane, Angle to Y %f\n",tAngle);
	      tpcEYInSurveyXY->Fill(tAngle);
	      tAngle = TMath::ATan2(uniVect.Z(),uniVect.Y());
	      if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
	      if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
	      tAngle *= 1e3;
	      if (tAngle>10000) printf("EDO (%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,uniVect.X(),uniVect.Y(),uniVect.Z());
	      //printf("In YZ plane, Angle to Y %f\n",tAngle);
	      tpcEYInSurveyYZ->Fill(tAngle);
	    }
	  }
	}
      }
    }
  }

  for(int iSec=13; iSec<=21; iSec++){
    if(EDO[iSec]!=nullVect){
      for(int jSec=iSec+1; jSec<=22; jSec++){
	if(EDO[jSec]!=nullVect){
	  tempVect.SetXYZ(0.,0.,0.);  
	  tempVect += EDO[iSec];  tempVect -= EDO[jSec];
	  for(int kSec=jSec+1; kSec<=23; kSec++){
	    if(EDO[kSec]!=nullVect){	
	      for(int lSec=kSec+1; lSec<=24; lSec++){
		if(EDO[lSec]!=nullVect){	
		  crossVect.SetXYZ(0.,0.,0.);  
		  crossVect += EDO[kSec];  crossVect -= EDO[lSec];
		  crossVect = crossVect.Cross(tempVect);
		  uniVect = crossVect.Unit();
		  
		  tAngle = TMath::ATan2(uniVect.X(),uniVect.Y());
		  if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
		  if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
		  tAngle *= 1e3;
		  if (tAngle>10000) printf("EDO (%d,%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,lSec,uniVect.X(),uniVect.Y(),uniVect.Z());
		  //printf("In XY plane, Angle to Y %f\n",tAngle);
		  tpcEYInSurveyXY->Fill(tAngle);
		  tAngle = TMath::ATan2(uniVect.Z(),uniVect.Y());
		  if (tAngle>TMath::Pi()/2.) tAngle -= TMath::Pi();
		  if (tAngle<-TMath::Pi()/2.) tAngle += TMath::Pi();
		  tAngle *= 1e3;
		  if (tAngle>10000) printf("EDO (%d,%d,%d,%d) (%f,%f,%f)\n",iSec,jSec,kSec,lSec,uniVect.X(),uniVect.Y(),uniVect.Z());
		  //printf("In YZ plane, Angle to Y %f\n",tAngle);
		  tpcEYInSurveyYZ->Fill(tAngle);
		}
	      }
	    }
	  }
	}
      }
    }
  }



/*   for(int iSec=13; iSec<=15; iSec++){ */
/*     tempVect.SetXYZ(0.,0.,0.);   */
/*     tempVect += EAO[iSec]; tempVect -= EAO[iSec+6]; */
/*     crossVect.SetXYZ(0.,0.,0.);   */
/*     crossVect += EAO[iSec+3]; crossVect -= EAO[iSec+3+6]; */
/*     crossVect = crossVect.Cross(tempVect); */
    
/*     uniVect = crossVect.Unit(); */
/*     uniVect.SetX(0.); */
/*     tAngle = uniVect.Angle(axisSurveyY); */
/*     tAngle *= 1e3; */
/*     printf("In YZ plane, Angle to Y %f\n",tAngle); */
/*     uniVect = crossVect.Unit(); */
/*     uniVect.SetZ(0.); */
/*     tAngle = uniVect.Angle(axisSurveyY); */
/*     tAngle *= 1e3; */
/*     printf("In XY plane, Angle to Y %f\n",tAngle); */

/*   } */

}
