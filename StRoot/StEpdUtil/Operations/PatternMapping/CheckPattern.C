// This simple macro reads in the output made by "runBfcEpd.C", which tells
// what is the "level" of the ADC of each tile.  We are using this to test
// mapping using PedAsPhys runs.
//
// If things are working, the file should match up with one of our Seven Patterns


int level[2][12][31];  // Vped level.  0,1, or 2

void SetPattern(int patternNumber);  // this just fills the array level[][][] with the numbers 0,1,2


void CheckPattern(char* txtFileName, int patternNumber=4)
{
  SetPattern(patternNumber);
  ifstream ifs;
  //  ifs.open(Form("%d/ExtractedLevels.txt",runNumber));
  ifs.open(txtFileName);
  int ew,pp,tt,ilev,adc;
  int nerrors=0;
  for (int i=0; i<744; i++){
    ifs >> ew >> pp >> tt >> ilev >> adc;
    int ewindex=(ew<0)?0:1;
    if (ilev!=level[ewindex][pp-1][tt-1]){
      cout << "MISMATCH!!! ew/pp/tt = " << ew << "/" << pp << "/" << tt << "\n";
      nerrors++;
    }
  }
  ifs.close();

  cout << "Matching file " << txtFileName << " with Pattern " << patternNumber << " yields " << nerrors << " errors\n";

}


void SetPattern(int patternNumber){

  // we'll set it to -1 here.  If anybody is still -1 at the end, then there is a problem
  for (int ew=0; ew<2; ew++){
    for (int pp=1; pp<13; pp++){
      for (int tt=1; tt<32; tt++){
	level[ew][pp-1][tt-1]=-1;
      }
    }
  }

  switch (patternNumber){
  case 1:  // East at 1, West at 2
    for (int pp=1; pp<13; pp++){
      for (int tt=1; tt<32; tt++){
	level[0][pp-1][tt-1] = 1;
	level[1][pp-1][tt-1] = 2;
      }
    }
    break;
  case 2:   // break each wheel into thirds
    for (int ew=0; ew<2; ew++){
      for (int pp=1; pp<13; pp++){
	int lev;
	if (pp<5) lev=0;
	if ((pp>4)&&(pp<9)) lev=1;
	if (pp>8) lev=2;
	for (int tt=1; tt<32; tt++){
	  level[ew][pp-1][tt-1]=lev;
	}
      }
    }
    break;
  case 3:    // break each 1/3 wheel into three pieces
    for (int ew=0; ew<2; ew++){
      for (int pp=1; pp<13; pp++){
	for (int tt=1; tt<32; tt++){
	  if ((pp==1)||(pp==5)||(pp==9)) level[ew][pp-1][tt-1]=0;
	  if ((pp==2)||(pp==6)||(pp==10)) level[ew][pp-1][tt-1]=1;
	  if ((pp==4)||(pp==8)||(pp==12)) level[ew][pp-1][tt-1]=2;
	  if ((pp==3)||(pp==7)||(pp==11)){
	    if (tt%2==1){   // odd
	      level[ew][pp-1][tt-1] = 1;
	    }
	    else {          // even
	      level[ew][pp-1][tt-1] = 2;
	    }
	  }
	}
      }
    }
    break;
  case 4:       // localize potential swap to the SECTOR level
    for (int ew=0; ew<2; ew++){
      for (int pp=1; pp<13; pp++){
	for (int tt=1; tt<32; tt+=2){    // NOTE increment by TWO -- we are looping over ODD tiles
	  if ((pp==1)||(pp==2)||(pp==5)||(pp==6)||(pp==9)||(pp==10)) level[ew][pp-1][tt-1] = 0;
	  if ((pp==4)||(pp==8)||(pp==12)) level[ew][pp-1][tt-1] = 1;
	  if ((pp==3)||(pp==7)||(pp==11)) level[ew][pp-1][tt-1] = 2;
	}
	for (int tt=2; tt<32; tt+=2){    // NOTE increment by TWO -- we are looping over EVEN tiles
	  if ((pp==3)||(pp==7)||(pp==11)) level[ew][pp-1][tt-1] = 0;
	  if ((pp==1)||(pp==2)||(pp==5)||(pp==6)||(pp==9)||(pp==10)) level[ew][pp-1][tt-1] = 1;
	  if ((pp==4)||(pp==8)||(pp==12)) level[ew][pp-1][tt-1] = 2;
	}
      }
    }
    break;
  case 5:      // low look within sectors
    for (int ew=0; ew<2; ew++){
      for (int pp=1; pp<13; pp++){
	for (int tt=1; tt<12; tt+=2){
	  level[ew][pp-1][tt-1] = 0;
	}
	for (int tt=13; tt<22; tt+=2){
	  level[ew][pp-1][tt-1] = 1;
	}
	for (int tt=23; tt<32; tt+=2){
	  level[ew][pp-1][tt-1] = 2;
	}

	for (int tt=2; tt<11; tt+=2){
	  level[ew][pp-1][tt-1] = 0;
	}
	for (int tt=12; tt<21; tt+=2){
	  level[ew][pp-1][tt-1] = 1;
	}
	for (int tt=22; tt<31; tt+=2){
	  level[ew][pp-1][tt-1] = 2;
	}
      }
    }
    break;
  case 6:     // breaking into pairs (and some singlets)
    for (int ew=0; ew<2; ew++){
      for (int pp=1; pp<13; pp++){
	level[ew][pp-1][1-1] = 0;
	level[ew][pp-1][2-1] = 0;
	level[ew][pp-1][3-1] = 0;
	level[ew][pp-1][4-1] = 0;
	level[ew][pp-1][5-1] = 1;
	level[ew][pp-1][6-1] = 1;
	level[ew][pp-1][7-1] = 1;
	level[ew][pp-1][8-1] = 1;
	level[ew][pp-1][9-1] = 2;
	level[ew][pp-1][10-1] =2 ;
	level[ew][pp-1][11-1] = 2;
	level[ew][pp-1][12-1] = 0;
	level[ew][pp-1][13-1] = 0;
	level[ew][pp-1][14-1] = 0;
	level[ew][pp-1][15-1] = 0;
	level[ew][pp-1][16-1] = 1;
	level[ew][pp-1][17-1] = 1;
	level[ew][pp-1][18-1] = 1;
	level[ew][pp-1][19-1] = 1;
	level[ew][pp-1][20-1] = 2;
	level[ew][pp-1][21-1] = 2;
	level[ew][pp-1][22-1] = 0;
	level[ew][pp-1][23-1] = 0;
	level[ew][pp-1][24-1] = 0;
	level[ew][pp-1][25-1] = 0;
	level[ew][pp-1][26-1] = 1;
	level[ew][pp-1][27-1] = 1;
	level[ew][pp-1][28-1] = 1;
	level[ew][pp-1][29-1] = 1;
	level[ew][pp-1][30-1] = 2;
	level[ew][pp-1][31-1] = 2;
      }
    }
    break;
  case 7:    // last disambiguity
    for (int ew=0; ew<2; ew++){
      for (int pp=1; pp<13; pp++){
	level[ew][pp-1][1-1] = 0;
	level[ew][pp-1][2-1] = 0;
	level[ew][pp-1][3-1] = 1;
	level[ew][pp-1][4-1] = 1;
	level[ew][pp-1][5-1] = 0;
	level[ew][pp-1][6-1] = 0;
	level[ew][pp-1][7-1] = 1;
	level[ew][pp-1][8-1] = 1;
	level[ew][pp-1][9-1] = 0;
	level[ew][pp-1][10-1] = 2;
	level[ew][pp-1][11-1] = 1;
	level[ew][pp-1][12-1] = 0;
	level[ew][pp-1][13-1] = 0;
	level[ew][pp-1][14-1] = 1;
	level[ew][pp-1][15-1] = 1;
	level[ew][pp-1][16-1] = 0;
	level[ew][pp-1][17-1] = 0;
	level[ew][pp-1][18-1] = 1;
	level[ew][pp-1][19-1] = 1;
	level[ew][pp-1][20-1] = 2;
	level[ew][pp-1][21-1] = 2;
	level[ew][pp-1][22-1] = 0;
	level[ew][pp-1][23-1] = 0;
	level[ew][pp-1][24-1] = 1;
	level[ew][pp-1][25-1] = 1;
	level[ew][pp-1][26-1] = 0;
	level[ew][pp-1][27-1] = 0;
	level[ew][pp-1][28-1] = 1;
	level[ew][pp-1][29-1] = 1;
	level[ew][pp-1][30-1] = 2;
	level[ew][pp-1][31-1] = 2;
      }
    }
    break;
  default:
    cout << "No!  not allowed pattern!\n";
    break;
  }
  
  // now check whether everybody has been set
  for (int ew=0; ew<2; ew++){
    for (int pp=1; pp<13; pp++){
      for (int tt=1; tt<32; tt++){
	if (level[ew][pp-1][tt-1]<0)
	  cout << "Hey, no level set for EW=" << ew << " PP=" << pp << " TT=" << tt << endl;
      }
    }
  }
  return;
}

