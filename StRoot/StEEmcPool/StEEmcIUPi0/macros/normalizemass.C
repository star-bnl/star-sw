//This macro normalizes the spin-dependent pi0 yield according to relative luminosity after we have generated EEMC pi0 trees according to pi0 finder software. We read these pi0 trees, initialize the luminosity for each fill and each run accordingly, and save the information we want into an output root file for further usage.
//Make sure you understand the spin states in this macro according to the definition of STAR database because we have two beams here: Yellow and Blue. They are defined in different sequences by the luminosity file, but here we make them consistent with the STAR database.
//Author: Weihong He.


class StChain;
class StEEmcIUPi0Reader;
class StEEmcIUMixEvent;

StEEmcIUMixEvent  *realEvent = 0;

Long64_t nevents = 0;

TString fillPath;
TFile *outfile = 0;

TH1F *hMassFill = 0;
TH1F *hZggFill  = 0;
TH1F *hPhiFill  = 0;

TH1F *hMassUU = 0; /* inv. mass in (B=+Y=+) spin state */
TH1F *hMassUD = 0; /* inv. mass in (B=+Y=-) spin state */
TH1F *hMassDU = 0; /* inv. mass in (B=-Y=+) spin state */
TH1F *hMassDD = 0; /* inv. mass in (B=-Y=-) spin state */

TH1F *hMassLU = 0, *hMassUL = 0;
TH1F *hMassLD = 0, *hMassUR = 0;
TH1F *hMassRU = 0, *hMassDL = 0;
TH1F *hMassRD = 0, *hMassDR = 0;

TH1F *hMassTU = 0, *hMassUT = 0;
TH1F *hMassTD = 0, *hMassUB = 0;
TH1F *hMassBU = 0, *hMassDT = 0;
TH1F *hMassBD = 0, *hMassDB = 0;

TH2F *hMassTimeUU = 0;
TH2F *hMassTimeUD = 0;
TH2F *hMassTimeDU = 0;
TH2F *hMassTimeDD = 0;

TH2F *hMassPtUU = 0;
TH2F *hMassPtUD = 0;
TH2F *hMassPtDU = 0;
TH2F *hMassPtDD = 0;

//uble_t pt_bins[] = { 0., 4.5, 6.5, 8.5, 12.5, 16.5, 20.5 };
Double_t pt_bins[] = { 0., 4.5, 5.5, 6.5, 7.5, 9.5, 11.5, 21.5 };

TH1F *hDsmvtx = 0;
TH1F *hTimebin = 0;

TH1F *hBXstar = 0;

Int_t fill_number = 0;
Int_t run_number  = 0;


const Int_t NMAX_FILLS = 200;
const Char_t *fill_pattern_file = "fillPatternBxRun6_070412.txt";
Int_t fill_numbers[NMAX_FILLS];
Int_t fill_pattern[NMAX_FILLS][120];
Int_t current_fill_pattern[120];
Int_t current_fill;

const Int_t NMAX_RUNS = 22000;
const Char_t *bbc_lumin_file = "relLumi06_070614.txt";
Int_t run_numbers[NMAX_RUNS];
Int_t bbc_lumin[NMAX_RUNS][16][4];
Int_t current_bbc_lumin[16][4];
Int_t current_run;
Int_t bbc_valid = 0;




// bad idea #define CORRECT_PT


// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void normalizemass(Int_t nevents=-1,
		const Char_t *mydir="/star/institutions/iucf/hew/7863/",		const Char_t *ofile="ttree.root")
{



  //Char_t *mydirs[] =
  //{
  //#if 1
  //  "/star/institutions/iucf/hew/Histout/7918/",
  //#endif

  //}; 

  sortFills( nevents, mydir, 1,ofile );

}

void sortFills( Int_t nevents, 
		Char_t *mydir,
		Int_t nfills,
		const Char_t *ofile )
{

#if 0
  //  current_fill = 7863;
  TString adir=mydir;
  TString bdir=adir( adir.Last('/')-4, adir.Last('/')-1 );
  current_fill = bdir.Atoi();
  std::cout << Form("---- current fill = %i ", current_fill) << std::endl;
#endif


   gStyle->SetHistMinimumZero();
  gStyle->SetPalette(1);
  //  gROOT->LoadMacro("macros/fitter.C");
  //gROOT->LoadMacro("fitter2.C");
  
  initFillPattern();      
  initBbcLumin();  


  outfile=new TFile(ofile,"recreate");
  outfile->cd();
  hMassFill = new TH1F(Form("hMass%s","Fill"),Form("Inv. mass run=%s","Fill"),120,0.,1.2);
  hZggFill  = new TH1F(Form("hZgg%s","Fill"), Form("z_{#gamma #gamma} run=%s","Fill"),50,0.,1.);
  hPhiFill  = new TH1F(Form("hPhi%s","Fill"), Form("#phi-bin run=%s","Fill"),60,0.,60.);

  hMassUU = new TH1F("hMassUU","Inv. mass spin=(B=+Y=+);M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassUD = new TH1F("hMassUD","Inv. mass spin=(B=+Y=-);M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassDU = new TH1F("hMassDU","Inv. mass spin=(B=-Y=+);M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassDD = new TH1F("hMassDD","Inv. mass spin=(B=-Y=-);M_{#gamma #gamma} [GeV]",120,0.,1.2);

  hMassUL = new TH1F("hMassUL","Inv. mass L spin=B+;M_{#gamma #gamma} [GeV]",120,0.,1.2); 
  hMassUR = new TH1F("hMassUR","Inv. mass R spin=B+;M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassDL = new TH1F("hMassDL","Inv. mass L spin=B-;M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassDR = new TH1F("hMassDR","Inv. mass R spin=B-;M_{#gamma #gamma} [GeV]",120,0.,1.2);

  hMassLU = new TH1F("hMassLU","Inv. mass L spin=Y+;M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassRU = new TH1F("hMassRU","Inv. mass R spin=Y+;M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassLD = new TH1F("hMassLD","Inv. mass L spin=Y-;M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassRD = new TH1F("hMassRD","Inv. mass R spin=Y-;M_{#gamma #gamma} [GeV]",120,0.,1.2);

   //

  hMassUT = new TH1F("hMassUT","Inv. mass T spin=B+;M_{#gamma #gamma} [GeV]",120,0.,1.2); 
  hMassUB = new TH1F("hMassUB","Inv. mass B spin=B+;M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassDT = new TH1F("hMassDT","Inv. mass T spin=B-;M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassDB = new TH1F("hMassDB","Inv. mass B spin=B-;M_{#gamma #gamma} [GeV]",120,0.,1.2);

  hMassTU = new TH1F("hMassTU","Inv. mass T spin=Y+;M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassBU = new TH1F("hMassBU","Inv. mass B spin=Y+;M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassTD = new TH1F("hMassTD","Inv. mass T spin=Y-;M_{#gamma #gamma} [GeV]",120,0.,1.2);
  hMassBD = new TH1F("hMassBD","Inv. mass B spin=Y-;M_{#gamma #gamma} [GeV]",120,0.,1.2);

  //

  hMassTimeUU = new TH2F("hMassTimeUU","Inv. mass spin=(B=+Y=+);M_{#gamma #gamma} [GeV]",120,0.,1.2,16,0.,16.);
  hMassTimeUD = new TH2F("hMassTimeUD","Inv. mass spin=(B=+Y=-);M_{#gamma #gamma} [GeV]",120,0.,1.2,16,0.,16.);
  hMassTimeDU = new TH2F("hMassTimeDU","Inv. mass spin=(B=-Y=+);M_{#gamma #gamma} [GeV]",120,0.,1.2,16,0.,16.);
  hMassTimeDD = new TH2F("hMassTimeDD","Inv. mass spin=(B=-Y==);M_{#gamma #gamma} [GeV]",120,0.,1.2,16,0.,16.);

  //
  
  hMassPtUU = new TH2F("hMassPtUU","Inv. mass spin=(B=+Y=+);M_{#gamma #gamma} [GeV];pT Gev/c",120,0.,1.2,50,0.0,25.0);
  hMassPtUD = new TH2F("hMassPtUD","Inv. mass spin=(B=+Y=-);M_{#gamma #gamma} [GeV];pT Gev/c",120,0.,1.2,50,0.0,25.0);
  hMassPtDU = new TH2F("hMassPtDU","Inv. mass spin=(B=-Y=+);M_{#gamma #gamma} [GeV];pT Gev/c",120,0.,1.2,50,0.0,25.0);
  hMassPtDD = new TH2F("hMassPtDD","Inv. mass spin=(B=-Y=-);M_{#gamma #gamma} [GeV];pT Gev/c",120,0.,1.2,50,0.0,25.0);


  TH1 *hh[] = { hMassUU, hMassUD, hMassDU, hMassDD, 
		hMassUL, hMassUR, hMassDL, hMassDR,
		hMassLU, hMassRU, hMassLD, hMassRD,
		hMassUT, hMassUB, hMassDT, hMassDB,
		hMassTU, hMassBU, hMassTD, hMassBD,
		hMassTimeUU, hMassTimeUD, hMassTimeDU, hMassTimeDD,
		hMassPtUU, hMassPtUD, hMassPtDU, hMassPtDD
  };
  //for ( Int_t ii=0;ii<sizeof(hh)/sizeof(TH1*);ii++ ) hh[ii]->Sumw2();


  hBXstar = new TH1F("hBXstar","Beam x-ing number @ STAR IP",120,0.,120.);
#if 0
  for ( Int_t i=0;i<120;i++ )
    {
      if ( !current_fill_pattern[i] ) continue;
      TMarker *mark=new TMarker(0.5+(float)i,0.,20);
      mark->SetMarkerColor(4);
      hBXstar->GetListOfFunctions()->Add(mark);
    }
#endif

  hDsmvtx = new TH1F("hDsmvtx","DSM vertex",512,0.,512.);
  hTimebin = new TH1F("hTimebin","BBC time bin",17,-1.,16.);

  LoadLibs();


  /*
   ********************************************************************
   ********************************************************************
   **
   ** Loop over all fills
   **
   ********************************************************************
   ********************************************************************
   */


  for ( UInt_t jj=0;jj<nfills;jj++ )
    {
      //const Char_t *mydir = mydirs[jj];
  

      TString adir=mydir;
      TString bdir=adir( adir.Last('/')-4, adir.Last('/')-1 );
      current_fill = bdir.Atoi();

      getFillPattern(current_fill);
      fillPath=mydir;
      fill_number = current_fill;


      /*
       **************************************************
       **************************************************
       **
       ** Loop over all runs in the fill
       **
       **************************************************
       **************************************************
       */

      Int_t nruns = 0;
      TSystemDirectory *dir = new TSystemDirectory("fills",fillPath);
      TIter next( dir->GetListOfFiles() );
      TObject *file = 0;
      while ( file = (TObject*)next() )
	{
	  TString name=file->GetName();
	  if ( name.Contains(".") ) {
	    continue;
	  }
	  std::cout << "-------- Processing run="<<name<<" ---------------------"<<std::endl;
	  readRun( name );
	  nruns++;
	}// loop over runs

    }// for loop over fills


  /* 
   ********************************************************************
   ******************************************************************** 
   ** 
   ** Loop over all fills
   **
   ********************************************************************
   ********************************************************************   
   */

  
  outfile->Write();
    
  


  
}//end of sortfill


/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////








// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void readFill( const Char_t *name )
{



}




// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void readRun( const Char_t *name )
{

  TString run=name;
  Int_t   runid = atoi(name);
  run_number = runid;

  std::cout << std::endl << std::endl;
  std::cout << "================================= new run =================================" << std::endl;
  std::cout << std::endl;
  std::cout << "Fill number: " << current_fill << std::endl;
  std::cout << "Run number:  " << runid << std::endl;
  
  getBbcLumin(runid);

  if ( !bbc_valid ) 
    {
      std::cout << "*** run is not contained in the rel. luminosity file, abort *** " << std::endl;
      return;
    }

  std::cout << Form("+++ run %i has relative luminosities",runid) << std::endl;

#if 0
  outfile->cd();
  TH1F *hMass = new TH1F(Form("hMass%s",name),Form("Inv. mass run=%s",name),120,0.,1.2);
  TH1F *hZgg  = new TH1F(Form("hZgg%s",name), Form("z_{#gamma #gamma} run=%s",name),50,0.,1.);
  TH1F *hPhi  = new TH1F(Form("hPhi%s",name), Form("#phi-bin run=%s",name),60,0.,60.);
#endif

  StChain chain("chain");
  StEEmcIUPi0Reader reader("mRealTree");
  chainFiles(fillPath+name,&reader);
  nevents = reader.getNumberOfEvents();
  chain.ls(3);
  chain.Init();
  Int_t stat  = 0;
  Int_t event = 0;
  while ( !stat )
    {
      if ( event>=nevents ) break;
      chain.Clear();
      stat = chain.Make();
      realEvent = reader.event();
      Int_t nPairs = reader.event() -> nPairs;
      Int_t bxStar=realEvent->bxStar;
      hBXstar->Fill(bxStar);


      if ( bxStar < 0 || bxStar > 119 ) {

	std::cout << Form("************* Warning bxStar = %i fill = %i run = %i ****************",
			  bxStar,fill_number,run_number)
		  << std::endl;
	continue;

      }
      

      // reject event if not valid bunch crossing
      if ( !current_fill_pattern[bxStar] ) continue;

      Int_t dsmvtx = realEvent->mBbcTrigger.onlineTimeDifference();
      hDsmvtx->Fill(dsmvtx);

      Int_t timebin = onlineBin(dsmvtx);
      hTimebin->Fill(onlineBin(dsmvtx));

      // -------------------------------------------------------------------------------- select these --
      if ( timebin <= 0 ) continue; 
      if ( timebin > 14 ) continue;

      for ( Int_t i=0;i<nPairs;i++ )
	{

          Float_t mass   = reader.event()->mMass[i];
          Float_t pt     = reader.event()->mPT[i];
	  //#ifdef CORRECT_PT
	  //if ( mass > 0. )
	  //pt = ( 0.135 / mass ) * pt;
	  //#endif

	  
	  Float_t zgg    = reader.event()->mZgg[i];
          Float_t diff   = realEvent->mEsmdu[i] - realEvent->mEsmdv[i];
          Float_t sum    = realEvent->mEsmdu[i] + realEvent->mEsmdv[i];
          Float_t zuv    = TMath::Abs(diff)/sum;
          Float_t eta    = reader.event()->mEEmcEta[i]; 
          //if ( eta > 1.5 ) continue; 

	  Int_t phibin = reader.event()->mTower1[i] / 12;
	  Int_t sector = phibin / 5;
	  Int_t subsec = phibin % 5;
	  Int_t etabin = reader.event()->mTower1[i] % 12;

	  Bool_t left   = sector < 5;
	  Bool_t right  = sector > 5;
	  Bool_t top    = sector < 2 || sector > 8; // 9 10 11 0 1
	  Bool_t bottom = sector > 2 && sector < 8; // 3 4 5 6 7

	  Int_t spin4  = realEvent->mSpin4;

#if 0
	  std::cout << "[" << event << "/" << nevents << "]"
		    << " npair=" <<  realEvent->nPairs
		    << " mass="  <<  realEvent->mMass[i]
		    << " pt="    <<  pt << " org=" << realEvent->mPT[i]
		    << " bx="    <<  realEvent->bxStar
		    << " zgg="   <<  realEvent->mZgg[i]
		    << " spin4=" <<  realEvent->mSpin4
		    << " dsmvtx=" << realEvent->mBbcTrigger.onlineTimeDifference()
		    << " sum smd=" << sum
		    << " diff smd=" << diff
		    << std::endl;	  
#endif

	  //if ( pt < 4.5 ) continue;	

	  //Int_t ntracks = reader.event()->mNumberOfTracks[i];
	  //Int_t npoints = reader.event()->mNumberOfPoints[i];

	  //$$$if ( ntracks ) continue;


	  //cout<<"timebin="<<timebin<<endl;	  
	  Float_t Nuu = current_bbc_lumin[timebin][0];
	  Float_t Ndu = current_bbc_lumin[timebin][1]; // Note that "du" and "ud" in the rel. lum files refers to YB
	  Float_t Nud = current_bbc_lumin[timebin][2]; 
	  Float_t Ndd = current_bbc_lumin[timebin][3];
	  //cout<<"uu="<<Nuu<<" du="<<Ndu<<" ud="<<Nud<<" dd="<<Ndd<<endl;
	  if ( spin4==5 )  { 
	    hMassUU->Fill(mass, 1.0);      
	    hMassTimeUU->Fill(mass, (float)timebin, 1.0 ); 
	    hMassPtUU->Fill(mass, pt, 1.0 );
	  }
	  if ( spin4==9 )  { 
	    hMassUD->Fill(mass, Nuu/Nud);  
	    hMassTimeUD->Fill(mass, (float)timebin, Nuu/Nud ); 
	    hMassPtUD->Fill(mass, pt, Nuu/Nud );
	  }
	  if ( spin4==6 )  { 
	    hMassDU->Fill(mass, Nuu/Ndu);  
	    hMassTimeDU->Fill(mass, (float)timebin, Nuu/Ndu ); 
	    hMassPtDU->Fill(mass,pt,Nuu/Ndu);
	  }
	  if ( spin4==10 ) { 
	    hMassDD->Fill(mass, Nuu/Ndd);  
	    hMassTimeDD->Fill(mass, (float)timebin, Nuu/Ndd ); 
	    hMassPtDD->Fill(mass,pt,Nuu/Ndd);
	  }

	  Bool_t blue_plus  = (spin4 == 5 || spin4 == 6);
	  Bool_t blue_minus = !blue_plus;

	  Bool_t yell_plus = (spin4 == 5 || spin4 == 9 );
	  Bool_t yell_minus = !yell_plus;

	  
	  if ( left  && blue_plus  ) hMassUL->Fill(mass, 1.0);
	  if ( left  && blue_minus ) hMassDL->Fill(mass, (Nuu+Ndu)/(Nud+Ndd) );
	  if ( right && blue_plus  ) hMassUR->Fill(mass, 1.0);
	  if ( right && blue_minus ) hMassDR->Fill(mass, (Nuu+Ndu)/(Nud+Ndd) );

	  if ( left  && yell_plus  ) hMassLU->Fill(mass, 1.0);
	  if ( left  && yell_minus ) hMassLD->Fill(mass, (Nuu+Nud)/(Ndu+Ndd) );
	  if ( right && yell_plus  ) hMassRU->Fill(mass, 1.0);
	  if ( right && yell_minus ) hMassRD->Fill(mass, (Nuu+Nud)/(Ndu+Ndd) );

	  if ( top  && blue_plus  ) hMassUT->Fill(mass, 1.0);
	  if ( top  && blue_minus ) hMassDT->Fill(mass, (Nuu+Ndu)/(Nud+Ndd) );
	  if ( bottom && blue_plus  ) hMassUB->Fill(mass, 1.0);
	  if ( bottom && blue_minus ) hMassDB->Fill(mass, (Nuu+Ndu)/(Nud+Ndd) );

	  if ( top  && yell_plus  ) hMassTU->Fill(mass, 1.0);
	  if ( top  && yell_minus ) hMassTD->Fill(mass, (Nuu+Nud)/(Ndu+Ndd) );
	  if ( bottom && yell_plus  ) hMassBU->Fill(mass, 1.0);
	  if ( bottom && yell_minus ) hMassBD->Fill(mass, (Nuu+Nud)/(Ndu+Ndd) );

	  
	  //	  hMass->Fill(mass);	  
	  hMassFill->Fill(mass);
	  if ( mass > 0.1 && mass < 0.18 ) {
	    //	    hZgg->Fill(zgg);	    
	    hZggFill->Fill(zgg);
	    //	    hPhi->Fill(phibin);	    
	    hPhiFill->Fill(phibin);
	  }

	}
      event++;
    }

  //----------------------------------------------------------- fit mass spectrum --
  //$$$  fitter(hMass);
  

}//end of readRun





Int_t onlineBin( Int_t tdiff )
{

  static Int_t mybins[]=
    //      0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16
    {  1,  50, 110, 130, 140, 180, 215, 240, 260, 300, 340, 360, 400, 440, 480, 500, 512};//2006???

  for ( UInt_t ii=0;ii<sizeof(mybins)/sizeof(Int_t)-1;ii++ )
    {
      if ( tdiff >=mybins[ii] && tdiff < mybins[ii+1] ) return ii;
    }
  return -1;

}







// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void chainFiles(const Char_t *path, StEEmcIUPi0Reader *reader)
{ 

  std::cout << "chaining files " << path << std::endl;

  TSystemDirectory *dir = new TSystemDirectory("dir",path);
  
  TIter next( dir->GetListOfFiles() );
  TObject *file = 0;
  while ( file = (TObject*)next() )
    {
      TString name=file->GetName();
  
      if ( name.Contains("root") ) {
        reader->chainFile(name);
  
      }
  
    }

  delete dir;

}














// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void getFillPattern( Int_t fill )
{
  for ( Int_t i=0;i<NMAX_FILLS;i++ )
    {
      Int_t myfill = fill_numbers[i];
      if ( fill == myfill ) 
	{
	  for ( Int_t j=0;j<120;j++ ) current_fill_pattern[j]=fill_pattern[i][j];
	  goto FINISH;
	}
      else if ( fill < 0 ) break;
    }
  for ( Int_t j=0;j<120;j++ ) current_fill_pattern[j]=-1;

 FINISH:
  std::cout << "-------------------------------------------------------------------------------" << std::endl;
  std::cout << "-------------------------------------------------------------------------------" << std::endl;
  std::cout << "Current fill pattern:"<< std::endl;
  std::cout << current_fill << " ";
  for ( Int_t j=0;j<60;j++ ) std::cout << current_fill_pattern[j] << " ";
  std::cout << std::endl << "     ";
  for ( Int_t j=60;j<120;j++ ) std::cout << current_fill_pattern[j] << " ";
  std::cout << std::endl;
  std::cout << "-------------------------------------------------------------------------------" << std::endl;
  std::cout << "-------------------------------------------------------------------------------" << std::endl;

  return;
}

void initFillPattern()
{
  ifstream fills( fill_pattern_file );
  Int_t count=0;
  while ( !fills.eof() )
    {
      fills >> fill_numbers[count];
      for ( Int_t i=0;i<120;i++ ) fills >> fill_pattern[count][i];
      count++;
      if ( count >= NMAX_FILLS ) break;
      fill_numbers[count]=-1;
    }
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

void getBbcLumin( Int_t run )
{

  for ( Int_t i=0;i<NMAX_RUNS;i++ )
    {
      if ( run == run_numbers[i] ) 
	{
	  bbc_valid = run;
	  for ( Int_t j=0;j<16;j++ )
	    {
	      current_bbc_lumin[j][0] = bbc_lumin[i][j][0];
	      current_bbc_lumin[j][1] = bbc_lumin[i][j][1];
	      current_bbc_lumin[j][2] = bbc_lumin[i][j][2];
	      current_bbc_lumin[j][3] = bbc_lumin[i][j][3];
	    }
	  goto FINISH;
	}
    }

  for ( Int_t j=0;j<16;j++ ) for ( Int_t k=0;k<4;k++ ) current_bbc_lumin[j][k]=-1;
  bbc_valid = 0;

 FINISH:
  std::cout << "-------------------------------------------------------------------------------" << std::endl;
  std::cout << "-------------------------------------------------------------------------------" << std::endl;
  std::cout << "Relative luminosities for run " << bbc_valid << std::endl;
  for ( Int_t j=0;j<16;j++ ) 
    {
      std::cout << j << " ";
      for ( Int_t k=0;k<4;k++ ) std::cout << current_bbc_lumin[j][k] << " ";
      std::cout << std::endl;
    }
  std::cout << "-------------------------------------------------------------------------------" << std::endl;
  std::cout << "-------------------------------------------------------------------------------" << std::endl;  
}

void initBbcLumin()
{

  Int_t count = 0;
  ifstream bbc( bbc_lumin_file );
  while ( !bbc.eof() ) 
    {
 
      Int_t myfill;
      bbc >> myfill;
      if ( myfill < 0 ) break;
      Int_t myrun;
      bbc >> myrun;
      Int_t myboard;
      bbc >> myboard;
      Int_t mybin;
      bbc >> mybin;
      Int_t uu,du,ud,dd;
      bbc >> uu;
      bbc >> du;
      bbc >> ud;
      bbc >> dd;


      if ( mybin > 1 ) assert(myrun==run_numbers[count]); // error detected in file
      run_numbers[count] = myrun;
      bbc_lumin[count][mybin][0] = uu;
      bbc_lumin[count][mybin][1] = du;
      bbc_lumin[count][mybin][2] = ud;
      bbc_lumin[count][mybin][3] = dd;
       if ( mybin == 15 ) count++; // increment to next run on last timebin

    }

}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void LoadLibs()
{
  //-- Load muDst shared libraries --
  gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcSimulatorMaker");

  gSystem->Load("StEEmcA2EMaker");
  gSystem->Load("StEEmcIUPi0");
  gSystem->Load("StSpinDbMaker");

}
