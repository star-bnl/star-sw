// $Id: l0sim.cxx,v 1.1 2000/12/13 00:09:25 akio Exp $
///////////////////////////////////////////////////////////////////////////////
// l0sim
//
//  L0 trigger Simulator for Peripheral Collisions 
//
///////////////////////////////////////////////////////////////////////////////

#include "StEventTypes.h"
#include "Stypes.h"
#include "TH1.h"
#include "StL0Trigger.h"
#include "StTriggerDetectorCollection.h"
#include "StCtbTriggerDetector.h"
#include "StMwcTriggerDetector.h"
#include "StVpdTriggerDetector.h"
#include "StZdcTriggerDetector.h"

void printslat(int m[120][2]){
  int i, j;
  cout << "           :  south -> top -> north -> bottom" << endl;  
  cout << "           :  13-1 60-59 |  58-44 | 43-29 | 28-14" << endl;
  for(j=1; j>-1; j--){
    cout << "west / eta= " << j << " : ";
    for(i=12; i>-1; i--){cout << " " << m[i][j];}
    for(i=59; i>57; i--){cout << " " << m[i][j];} cout << " | ";
    for(i=57; i>42; i--){cout << " " << m[i][j];} cout << " | ";
    for(i=42; i>27; i--){cout << " " << m[i][j];} cout << " | ";
    for(i=27; i>12; i--){cout << " " << m[i][j];} 
    cout << endl;
  } 
  for(j=0; j<2; j++){
    cout << "east / eta= " << j << " : ";
    for(i=102; i<117; i++){cout << " " << m[i][j];}cout << " | ";
    for(i=117; i<120; i++){cout << " " << m[i][j];} 
    for(i=60; i<71; i++){cout << " " << m[i][j];} cout << " | ";
    for(i=71; i<87; i++){cout << " " << m[i][j];} cout << " | ";
    for(i=87; i<102; i++){cout << " " << m[i][j];};
    cout << endl;
  } 
  cout << "           :  103-117 | 118-120 61-72 | 73-87 | 88-102" << endl;
}

void printpatch(int m[4][4]){  
  cout << "       :  south top/north top/north bottom/south bottom" << endl;
  for(int j=0; j<4; j++){    
    cout << "eta= " << j << " : ";
    for(int i=0; i<4; i++){
      cout << " " << m[i][j];
    }
    cout << endl;
  }
}

void lut0normal(StCtbTriggerDetector &ctb, int r[120][2], int p=0){
  for(int i=0; i<120; i++){for(int j=0; j<2; j++){r[i][j]=(int)ctb.mips(i,j,0);}}
  if(p>0){cout << "CTB MIP = " << endl; printslat(r);}
}

void lut0veto(StCtbTriggerDetector &ctb, int r[120][2], int p=0){
  for(int i=0; i<120; i++){
    for(int j=0; j<4; j++){
      int mip = (int)ctb.mips(i,j,0);
      if( (0<=i && i<8) || (19<=i && i<34) ||
	  (49<=i && i<60) || (60<=i && i<68) ||
	  (79<=i && i<94) || (109<=i && i<120) ){
	if(mip<3){r[i][j]=0;}
	else if(mip>16){r[i][j]=mip;}
	else{r[i][j]=17;}
      }else{
	r[i][j]=mip;
      }
    }
  }
  if(p>0){cout << "CTB MIP (after vetoing) = " << endl; printslat(r);}
}

void lut0ignore(StCtbTriggerDetector &ctb, int r[120][2], int p=0){
  for(int i=0; i<120; i++){
    for(int j=0; j<4; j++){
      int mip = (int)ctb.mips(i,j,0);
      if( (0<=i && i<8) || (19<=i && i<34) ||
	  (49<=i && i<60) || (60<=i && i<68) ||
	  (79<=i && i<94) || (109<=i && i<120) ){
	if(mip<3){r[i][j]=0;}
	else if(mip>16){r[i][j]=mip;}
	else{r[i][j]=0;}
      }else{
	r[i][j]=mip;
      }
    }
  }
  if(p>0){cout << "CTB MIP (after ignoring) = " << endl; printslat(r);}
}

void dsm0sum(int c[120][2], int r[4][4], int p=0){
  int i, j;
  for(i=0; i<4; i++){for(j=0; j<4; j++){r[i][j] = 0;}}
  for(i=0;   i<15;  i++){for(j=0; j<2; j++){r[0][2+j]+=c[i][j];}}
  for(i=15;  i<30;  i++){for(j=0; j<2; j++){r[3][2+j]+=c[i][j];}}
  for(i=30;  i<45;  i++){for(j=0; j<2; j++){r[2][2+j]+=c[i][j];}}
  for(i=45;  i<60;  i++){for(j=0; j<2; j++){r[1][2+j]+=c[i][j];}}
  for(i=60;  i<75;  i++){for(j=0; j<2; j++){r[1][2+j]+=c[i][j];}}
  for(i=75;  i<90;  i++){for(j=0; j<2; j++){r[2][1-j]+=c[i][j];}}
  for(i=90;  i<105; i++){for(j=0; j<2; j++){r[3][1-j]+=c[i][j];}}
  for(i=105; i<120; i++){for(j=0; j<2; j++){r[0][1-j]+=c[i][j];}}
  if(p>0){cout << "CTB patch sum (15 slats) = " << endl; printpatch(r);}
}

void lut1normal(int c[4][4], int r[4][4], int p=0){
  for(int i=0; i<4; i++){for(int j=0; j<4; j++){r[i][j]=c[i][j];}}
  if(p>0){cout << "CTB patch LUT (15 slats) = " << endl; printpatch(r);}
}

void lut1pc4(int c[4][4], int r[4][4], int p=0){
  for(int i=0; i<4; i++){
    for(int j=0; j<4; j++){
      if(c[i][j]<3){r[i][j]=0;}
      else if(c[i][j]>9){r[i][j]=17;}
      else{
	if(i==0 || i==3) r[i][j]=1;
	else r[i][j]=4;
      }
    }
  }
  if(p>0){cout << "CTB patch LUT (PC4) = " << endl; printpatch(r);}
}

void dsm1sum(int c[4][4], int r[2], int p=0){
  r[0]=0; r[1]=0;
  for(int i=0; i<2; i++){for(int j=0; j<4; j++){r[0]+=c[i][j];}}
  for(int i=2; i<4; i++){for(int j=0; j<4; j++){r[1]+=c[i][j];}}
  if(p>0) cout << "DSM1 sum west/east = " << r[0] << " / " << r[1] << endl;
}

void lut2normal(int c[2], int r[2], int p=0){
  for(int i=0; i<2; i++){r[i]=c[i];}
  if(p>0) cout << "LUT2 west/east = " << r[0] << " / " << r[1] << endl;
}

void lut2pc4(int c[2], int r[2] , int p=0){
  for(int i=0; i<2; i++){
    if(c[i]==1 || c[i]==4 || c[i]==5) r[i]=c[i];
    else r[i]=0;
  }
  if(p>0) cout << "LUT2 (pc4) west/east = " << r[0] << " / " << r[1] << endl;
}

void dsm2sum(int c[2], int r[1], int p=0){
  r[0]=0; for(int i=0; i<<2; i++){r[0]+=c[i];}
  if(p>0) cout << "DSM2 sum total = " << r[0] << endl;
}

Float_t ctbmips(Float_t mip, int i, int j, int k){
  if(mip<3) return 0.0;
  if(mip>15) return mip;
  if(mip>10) return 16.0;
  if(i<5) return 16.0;
  if(i<20) return mip;
  if(i<35) return 16.0;
  if(i<50) return mip;
  if(i<65) return 16.0;
  if(i<80) return mip;
  if(i<95) return 16.0;
  if(i<110) return mip;
  return 16.0;
}

int l0sim(StEvent *event, TH1F* m_ctbtrg){
  int i,j,k,res=0;
  
  // cuts
  float threshold = 3; // threshold in Npart for each slat to be "hit"
  float min_sum = 10.0;  
  float max_sum = 100.0;
  float qmin = 3;      // threshold for quater ctbsum 
  
  StL0Trigger* l0 = event->l0Trigger();
  StTriggerDetectorCollection* trg = event->triggerDetectorCollection();
  StCtbTriggerDetector& ctb = trg->ctb();
  StMwcTriggerDetector& mwc = trg->mwc();
  StVpdTriggerDetector& vpd = trg->vpd();
  StZdcTriggerDetector& zdc = trg->zdc();
  
  if(!&ctb){
    cout << "Didn't find CTB" << endl;    
    return -1;
  }
  
  // calculate global sum/hits
  float ctbsum=0.0, ctbsumped=0.0; int nhit = 0;
  for(i=0; i<120; i++){
    for(j=0; j<2; j++){
      if(ctb.mips(i,j,0)>=threshold) nhit ++;
      ctbsum += ctb.mips(i,j,0);
      if(ctb.mips(i,j,0)<5) ctbsumped += ctb.mips(i,j,0);
    }
  }
  cout << "CTB # of hits " << nhit << " above nmip > " << threshold << endl;
  cout << "CTB MIP total sum = " << ctbsum << endl;

  // calculate tilted quarters
  Float_t ctbsums[4];
  Int_t nhits[4];
  for(i=0; i<4; i++){ctbsums[i]=0.0; nhits[i]=0;}
  for(i=0; i<5; i++){for(j=0; j<2; j++){
      if(ctb.mips(i,j,0)>=threshold) nhits[0]++;
      ctbsums[0] += ctb.mips(i,j,0);
  }}    
  for(i=5; i<20; i++){for(j=0; j<2; j++){
    if(ctb.mips(i,j,0)>=threshold) nhits[1]++;
    ctbsums[1] += ctb.mips(i,j,0);
  }}
  for(i=20; i<35; i++){for(j=0; j<2; j++){
    if(ctb.mips(i,j,0)>=threshold) nhits[2]++;
    ctbsums[2] += ctb.mips(i,j,0);
  }}
  for(i=35; i<50; i++){for(j=0; j<2; j++){
    if(ctb.mips(i,j,0)>=threshold) nhits[3]++;
    ctbsums[3] += ctb.mips(i,j,0);
  }}
  for(i=50; i<60; i++){for(j=0; j<2; j++){
    if(ctb.mips(i,j,0)>=threshold) nhits[0]++;
    ctbsums[0] += ctb.mips(i,j,0);
  }}
  for(i=60; i<65; i++){for(j=0; j<2; j++){
    if(ctb.mips(i,j,0)>=threshold) nhits[0]++;
    ctbsums[0] += ctb.mips(i,j,0);
  }}    
  for(i=65; i<80; i++){for(j=0; j<2; j++){
    if(ctb.mips(i,j,0)>=threshold) nhits[3]++;
    ctbsums[3] += ctb.mips(i,j,0);
  }}
  for(i=80; i<95; i++){for(j=0; j<2; j++){
    if(ctb.mips(i,j,0)>=threshold) nhits[2]++;
    ctbsums[2] += ctb.mips(i,j,0);
  }}
  for(i=95; i<110; i++){for(j=0; j<2; j++){
    if(ctb.mips(i,j,0)>=threshold) nhits[1]++;
    ctbsums[1] += ctb.mips(i,j,0);
  }}
  for(i=110; i<120; i++){for(j=0; j<2; j++){
    if(ctb.mips(i,j,0)>=threshold) nhits[0]++;
    ctbsums[0] += ctb.mips(i,j,0);
  }}
  
  cout << "CTB # of hits in quarters ";
  for(i=0; i<4; i++){cout << nhits[i] << " ";} cout << endl;
  cout << "CTB MIP sum in quarters  ";
  for(i=0; i<4; i++){cout << ctbsums[i] << " ";} cout << endl;
  
  // calculate ctbsum for 15 slat patch
  Float_t ctbpatch[4][4], ctbpatchsum = 0.0, east = 0.0, west = 0.0; 
  for(i=0; i<4; i++){for(j=0; j<4; j++){ctbpatch[i][j] = 0.0;}}
  for(i=0;   i<13;  i++){for(j=0; j<2; j++){ctbpatch[0][2+j]+=ctbmips(ctb.mips(i,j,0),i,j,0);}}
  for(i=13;  i<28;  i++){for(j=0; j<2; j++){ctbpatch[3][2+j]+=ctbmips(ctb.mips(i,j,0),i,j,0);}}
  for(i=28;  i<43;  i++){for(j=0; j<2; j++){ctbpatch[2][2+j]+=ctbmips(ctb.mips(i,j,0),i,j,0);}}
  for(i=43;  i<58;  i++){for(j=0; j<2; j++){ctbpatch[1][2+j]+=ctbmips(ctb.mips(i,j,0),i,j,0);}}
  for(i=58;  i<60;  i++){for(j=0; j<2; j++){ctbpatch[0][2+j]+=ctbmips(ctb.mips(i,j,0),i,j,0);}}
  for(i=60;  i<73;  i++){for(j=0; j<2; j++){ctbpatch[1][1-j]+=ctbmips(ctb.mips(i,j,0),i,j,0);}}
  for(i=73;  i<88;  i++){for(j=0; j<2; j++){ctbpatch[2][1-j]+=ctbmips(ctb.mips(i,j,0),i,j,0);}}
  for(i=88;  i<103; i++){for(j=0; j<2; j++){ctbpatch[3][1-j]+=ctbmips(ctb.mips(i,j,0),i,j,0);}}
  for(i=103; i<118; i++){for(j=0; j<2; j++){ctbpatch[0][1-j]+=ctbmips(ctb.mips(i,j,0),i,j,0);}}
  for(i=118; i<120; i++){for(j=0; j<2; j++){ctbpatch[1][1-j]+=ctbmips(ctb.mips(i,j,0),i,j,0);}}
  
  // patch sum
  cout << "CTB patch (15 slats sum) : " << endl;
  for(i=0; i<4; i++){
    for(j=0; j<4; j++){
      cout << "  " << ctbpatch[i][j];
      ctbpatchsum += ctbpatch[i][j];
      if(j<2) {east += ctbpatch[i][j];}
      else    {west += ctbpatch[i][j];}
    }
    cout << endl;
  }
  cout << "Total = " << ctbpatchsum << endl;
  cout << "east/west = " << east <<" / "<<west<<endl;
  
  // pc2 trigger
  Float_t ctbpatcht1[4][4], ctbpatchsum1 = 0.0; 
  cout << "PC2 trigger : " << endl;
  for(i=0; i<4; i++){
    for(j=0; j<4; j++){
      if(ctbpatch[i][j]<1){ctbpatcht1[i][j] = 0.0;}
      else if(ctbpatch[i][j]>15.0){ctbpatcht1[i][j] = ctbpatch[i][j];}
      // else if(ctbpatch[i][j]>10.0){ctbpatcht1[i][j] = 16.0;}
      else{
	ctbpatcht1[i][j]=1.0;
      }
      cout << "  " << ctbpatcht1[i][j];
      ctbpatchsum1 += ctbpatcht1[i][j];
    } 
    cout << endl;
  } 
  cout << "Total = " << ctbpatchsum1  << endl;
  
  // improved pc2 trigger
  Float_t ctbpatcht12[4][4], ctbpatchsum12 = 0.0; 
  cout << "Improved PC2 trigger : " << endl;
  for(i=0; i<4; i++){
    for(j=0; j<4; j++){
      if(ctbpatch[i][j]<3){ctbpatcht12[i][j] = 0.0;}
      else if(ctbpatch[i][j]>15.0){ctbpatcht12[i][j] = ctbpatch[i][j];}
      else if(ctbpatch[i][j]>10.0){ctbpatcht12[i][j] = 16.0;}
      else{
	ctbpatcht12[i][j]=1.0;
      }
      cout << "  " << ctbpatcht12[i][j];
      ctbpatchsum12 += ctbpatcht12[i][j];
    } 
    cout << endl;
  } 
  cout << "Total = " << ctbpatchsum12  << endl;
  
  // 5-6-9/5-15  trigger
  Float_t ctbpatcht2[4][4], ctbpatchsum2 = 0.0; 
  cout << "5-6-9/5-15 trigger : " << endl;
  for(i=0; i<4; i++){
    for(j=0; j<4; j++){
      if(ctbpatch[i][j]<3){ctbpatcht2[i][j] = 0.0;}
      else if(ctbpatch[i][j]>15.0){ctbpatcht2[i][j] = ctbpatch[i][j];}
      else if(ctbpatch[i][j]>10.0){ctbpatcht2[i][j] = 16.0;}
      else{
	if(i==0)      ctbpatcht2[0][j]=1.0;
	else if(i==1) ctbpatcht2[1][j]=16.0;
	else if(i==2) ctbpatcht2[2][j]=4.0;
	else if(i==3) ctbpatcht2[3][j]=16.0;
      }
      cout << "  " << ctbpatcht2[i][j];
      ctbpatchsum2 += ctbpatcht2[i][j];
    } 
    cout << endl;
  } 
  cout << "Total = " << ctbpatchsum2 << endl;
  
  // 15 trigger
  Float_t ctbpatcht3[4][4], ctbpatchsum3 = 0.0; 
  cout << "15 trigger : " << endl;
  for(i=0; i<4; i++){
    for(j=0; j<4; j++){
      if(ctbpatch[i][j]<3){ctbpatcht3[i][j] = 0.0;}
      else if(ctbpatch[i][j]>15.0){ctbpatcht3[i][j] = ctbpatch[i][j];}
      else if(ctbpatch[i][j]>10.0){ctbpatcht3[i][j] = 16.0;}
      else{
	if(i==0)     ctbpatcht3[0][j]=2.0;
	else if(i==1) ctbpatcht3[1][j]=3.0;
	else if(i==2) ctbpatcht3[2][j]=13.0;
	else if(i==3) ctbpatcht3[3][j]=12.0;
      }
      cout << "  " << ctbpatcht3[i][j];
      ctbpatchsum3 += ctbpatcht3[i][j];
    } 
    cout << endl;
  } 
  cout << "total = " << ctbpatchsum3 << endl;
  
  // falk+akio trigger
  Float_t ctbpatcht4[4][4], ctbpatchsum4 = 0.0, east2 = 0.0, west2 = 0.0; 
  cout << "falk + akio trigger : " << endl;
  for(i=0; i<4; i++){
    for(j=0; j<4; j++){
      if(ctbpatch[i][j]<3){ctbpatcht4[i][j] = 0.0;}
      else if(ctbpatch[i][j]>15.0){ctbpatcht4[i][j] = ctbpatch[i][j];}
      else if(ctbpatch[i][j]>10.0){ctbpatcht4[i][j] = 16.0;}
      else{
	if(i==0)      ctbpatcht4[0][j]=3.0;
	else if(i==1) ctbpatcht4[1][j]=5.0;
	else if(i==2) ctbpatcht4[2][j]=5.0;
	else if(i==3) ctbpatcht4[3][j]=3.0;
      }
      cout << "  " << ctbpatcht4[i][j];
      if(j<2){east2 += ctbpatcht4[i][j];}
      else   {west2 += ctbpatcht4[i][j];}
      ctbpatchsum4 += ctbpatcht4[i][j];
    } 
    cout << endl;
  } 
  cout << "total = " << ctbpatchsum4 << endl;
  cout << "east/west = " << east2 <<" / "<< west2 <<endl;
  
  //DSM simulations
  //LUT 0 (each slats)
  int lut0_normal[120][2], lut0_veto[120][2], lut0_ignore[120][2];
  lut0normal(ctb, lut0_normal, 1);
  lut0veto(ctb, lut0_veto, 0);
  lut0ignore(ctb, lut0_ignore , 0);
  //DSM 0 (15 slats sum)
  int dsm0_normal[4][4], dsm0_veto[4][4], dsm0_ignore[4][4];
  dsm0sum(lut0_normal, dsm0_normal, 0);
  dsm0sum(lut0_veto, dsm0_veto, 0);
  dsm0sum(lut0_ignore, dsm0_ignore, 0);
  //LUT 1 (patches)
  int lut1_normal[4][4], lut1_veto[4][4], lut1_ignore[4][4];
  lut1normal(dsm0_normal, lut1_normal, 0);
  lut1pc4(dsm0_veto, lut1_veto, 0);
  lut1pc4(dsm0_ignore, lut1_ignore, 0);
  //DSM 1 (8 patches sum)
  int dsm1_normal[2], dsm1_veto[2], dsm1_ignore[2];
  dsm1sum(lut1_normal, dsm1_normal, 0);
  dsm1sum(lut1_veto, dsm1_veto, 0);
  dsm1sum(lut1_ignore, dsm1_ignore, 0);
  //LUT 2 (east/west)
  int lut2_normal[2], lut2_veto[2], lut2_ignore[2];
  lut2normal(dsm1_normal, lut2_normal, 0);
  lut2pc4(dsm1_veto, lut2_veto, 0);
  lut2pc4(dsm1_ignore, lut2_ignore, 0);
  //DSM 2 (east + west sum)
  int dsm2_normal[1], dsm2_veto[1], dsm2_ignore[1];
  dsm2sum(lut2_normal, dsm2_normal, 0);
  dsm2sum(lut2_veto, dsm2_veto, 0);
  dsm2sum(lut2_ignore, dsm2_ignore, 0);
  
  //fill trigger histogram
  Int_t pec = 0, nprim = 0; 
  Float_t offset = 30.0;
  StSPtrVecTrackNode& trknode = event->trackNodes();
  int nnode=trknode.size();
  for(i=0; i<nnode; i++){
    if(trknode[i]->entries(primary)>0) nprim++;
  }
  if(nprim>0){pec = 1;} // define pec events = found primary vertex/track
  
  cout << "Tiggered for : " << endl;
  m_ctbtrg->Fill(0.0);  if(pec) m_ctbtrg->Fill(offset);
  if(ctbsum>min_sum){
    cout << "  (1) ctb_sum > " << min_sum << endl;
    m_ctbtrg->Fill(1.0); if(pec) m_ctbtrg->Fill(1.0+offset);
  }
  if(ctbsum<max_sum){
    cout << "  (2) ctb_sum < " << max_sum << endl;
    m_ctbtrg->Fill(2.0); if(pec) m_ctbtrg->Fill(2.0+offset);
  }
  if(ctbsum>min_sum && ctbsum<max_sum){
    cout << "  (3) 1 and 2" << endl;
    m_ctbtrg->Fill(3.0); if(pec) m_ctbtrg->Fill(3.0+offset);
  } 
  if(nhit>1){
    cout << "  (4) nhit > 1" << endl;
    m_ctbtrg->Fill(4.0); if(pec) m_ctbtrg->Fill(4.0+offset);
  }
  if(nhit>1 && ctbsum<max_sum){
    cout << "  (5) 2 and 4" << endl;	    
    m_ctbtrg->Fill(5.0); if(pec) m_ctbtrg->Fill(5.0+offset);
  }
  if(nhits[0]>0 && nhits[2]>0 && ctbsum<max_sum){
    cout << "  (6) 2 and nhit>0 for 1st and 3rd quarter" << endl;	         
    m_ctbtrg->Fill(6.0); if(pec) m_ctbtrg->Fill(6.0+offset);
  }    
  if(nhits[1]>0 && nhits[3]>0 && ctbsum<max_sum){
    cout << "  (7) 2 and nhit>0 for 2nd and 4th quarter" << endl;	         
    m_ctbtrg->Fill(7.0); if(pec) m_ctbtrg->Fill(7.0+offset);
  }
  if(nhits[0]>0 && nhits[1]<1 && nhits[2]>0 && nhits[3]<1 && ctbsum<max_sum){
    cout << "  (8) 2 and nhit>0 for 1st and 3rd quarter and nhit=0 for 2nd and 4th" << endl;	         
    m_ctbtrg->Fill(8.0); if(pec) m_ctbtrg->Fill(8.0+offset);
  }    
  if(nhits[0]<1 && nhits[1]>0 && nhits[2]<1 && nhits[3]>0 && ctbsum<max_sum){
    cout << "  (9) 2 and nhit>0 for 2nd and 4th quarter and nhit=0 for 1st and 3rd" << endl;	         
    m_ctbtrg->Fill(9.0); if(pec) m_ctbtrg->Fill(9.0+offset);
  }
  if(ctbsums[0]>qmin && ctbsums[2]>qmin && ctbsum<max_sum){
    cout << "  (10) 2 and ctbsum> "<<qmin<<" for 1st and 3rd quarter" << endl;	         
    m_ctbtrg->Fill(10.0); if(pec) m_ctbtrg->Fill(10.0+offset);
  }    
  if(ctbsums[1]>qmin && ctbsums[3]>qmin && ctbsum<max_sum){
    cout << "  (11) 2 and ctbsum> "<<qmin<<" for 2nd and 4th quarter" << endl;	         
    m_ctbtrg->Fill(11.0); if(pec) m_ctbtrg->Fill(11.0+offset);
  }    
  if(ctbsums[0]>qmin && ctbsums[2]>qmin && ctbsums[1]<=qmin && ctbsums[3]<=qmin && ctbsum<max_sum){
    cout << "  (12) 2 and ctbsum> "<<qmin<<" for 1st and 3rd quarter, and <= for 2/4" << endl;
    m_ctbtrg->Fill(12.0); if(pec) m_ctbtrg->Fill(12.0+offset);
  }    
  if(ctbsums[1]>qmin && ctbsums[3]>qmin && ctbsums[0]<=qmin && ctbsums[2]<=qmin && ctbsum<max_sum){
    cout << "  (13) 2 and ctbsum> "<<qmin<<" for 2nd and 4th quarter, and <= for 1/3" << endl;
    m_ctbtrg->Fill(13.0); if(pec) m_ctbtrg->Fill(13.0+offset);
  }    
  if(0 < ctbpatchsum1 && ctbpatchsum1 < 11){
    cout << "  (14) PC2 trigger " <<  endl;
    m_ctbtrg->Fill(14.0); if(pec) m_ctbtrg->Fill(14.0+offset);
  }    
  if(1 < ctbpatchsum12 && ctbpatchsum12 < 5){
    cout << "  (15) PC2 trigger " <<  endl;
    m_ctbtrg->Fill(15.0); if(pec) m_ctbtrg->Fill(15.0+offset);
  }    
  if(4 < ctbpatchsum2 && ctbpatchsum2 < 16){
    cout << "  (16) 5-15 trigger " <<  endl;
    m_ctbtrg->Fill(16.0); if(pec) m_ctbtrg->Fill(16.0+offset);
  }    
  if(ctbpatchsum2 == 5 || ctbpatchsum2 == 6 || ctbpatchsum2 == 9){
    cout << "  (17) 5-6-9 trigger " <<  endl;
    m_ctbtrg->Fill(17.0); if(pec) m_ctbtrg->Fill(17.0+offset);
  }    
  if(ctbpatchsum3 == 15){
    cout << "  (18) 15 trigger " <<  endl;
    m_ctbtrg->Fill(18.0); if(pec) m_ctbtrg->Fill(18.0+offset);
  }    
  if(ctbpatchsum4 == 8  || ctbpatchsum4 == 11 || ctbpatchsum4 == 13){
    cout << "  (19) falk+akio trigger " <<  endl;
    m_ctbtrg->Fill(19.0); if(pec) m_ctbtrg->Fill(19.0+offset);
  }    
  if((west2 == 8  && east2 ==0) || 
     (west2 == 11 && east2 ==0) || 
     (west2 == 13 && east2 ==0) || 
     (east2 == 8  && west2 ==0) || 
     (east2 == 11 && west2 ==0) || 
     (east2 == 13 && west2 ==0) ){
    cout << "  (20) falk+akio west/east trigger " <<  endl;
    m_ctbtrg->Fill(20.0); if(pec) m_ctbtrg->Fill(20.0+offset);
  }    
  if(west > 3 && east > 3){ 
    cout << "  (21) both east and west has hit " <<  endl;
    m_ctbtrg->Fill(21.0); if(pec) m_ctbtrg->Fill(21.0+offset);
  }    
  if((west > 3 && east <= 3) || (east > 3 && west <= 3)){
    cout << "  (22) only east or west has hit " <<  endl;
    m_ctbtrg->Fill(22.0); if(pec) m_ctbtrg->Fill(22.0+offset);
  }    
  if(4<dsm2_veto[0] && dsm2_veto[0]<11){
    cout << "  (23) PC4 " <<  endl;
    m_ctbtrg->Fill(23.0); if(pec) m_ctbtrg->Fill(23.0+offset);
  }    
  if(4<dsm2_ignore[0] && dsm2_ignore[0]<11){
    cout << "  (24) PC4.1 " <<  endl;
    m_ctbtrg->Fill(24.0); if(pec) m_ctbtrg->Fill(24.0+offset);
  }    

  return res;
} 
