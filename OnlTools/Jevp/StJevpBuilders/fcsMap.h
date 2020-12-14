/** FCS detectorId **/
enum StFcsDetectorId{
    kFcsEcalNorthDetId=0,
    kFcsEcalSouthDetId=1,
    kFcsHcalNorthDetId=2,
    kFcsHcalSouthDetId=3,
    kFcsPresNorthDetId=4,
    kFcsPresSouthDetId=5
};

/** FCS basic constants **/
enum StFcsConstants {
    kFcsNDet=6,
    kFcsEHP=3,
    kFcsEcalHcal=2,
    kFcsNorthSouth=2,
    kFcsMaxId=748,
    kFcsEcalNCol=22,
    kFcsEcalNRow=34,
    kFcsEcalMaxId=748,
    kFcsHcalNCol=13,
    kFcsHcalNRow=20,
    kFcsHcalMaxId=260,
    kFcsPresMaxId=192,
    kFcsPresNCol=16,
    kFcsPresNRow=12,
    kFcsMaxDepCrate=5,
    kFcsMaxDepBd=24,
    kFcsMaxDepCh=32,
    kFcsEcal4x4NCol=9,
    kFcsEcal4x4NRow=15,
    kFcsHcal4x4NCol=5,
    kFcsHcal4x4NRow=9
};

//DEP map
Short_t mMap_ehp[kFcsNDet][kFcsMaxId];
Short_t mMap_ns [kFcsNDet][kFcsMaxId];
Short_t mMap_crt[kFcsNDet][kFcsMaxId];
Short_t mMap_slt[kFcsNDet][kFcsMaxId];
Short_t mMap_dep[kFcsNDet][kFcsMaxId];
Short_t mMap_ch [kFcsNDet][kFcsMaxId];
Short_t mMap_ppb[kFcsNDet][kFcsMaxId];
Short_t mMap_ppp[kFcsNDet][kFcsMaxId];
Short_t mMap_pph[kFcsNDet][kFcsMaxId];
Short_t mMap_wcol[kFcsNDet][kFcsMaxId];
Short_t mMap_jcol[kFcsNDet][kFcsMaxId];

//Reverse map
Short_t mRMap_det[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh];
Short_t mRMap_id [kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh];
Short_t mRMap_crt[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh];
Short_t mRMap_slt[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxDepCh];

//SC map
const unsigned short kFcsMaxBranch=2;
const unsigned short kFcsMaxAddr=16;
const unsigned short kFcsMaxSiPM=4;
Short_t mScMap_ehp[kFcsNDet][kFcsMaxId];
Short_t mScMap_ns[kFcsNDet][kFcsMaxId];
Short_t mScMap_dep[kFcsNDet][kFcsMaxId];
Short_t mScMap_bra[kFcsNDet][kFcsMaxId];
Short_t mScMap_add[kFcsNDet][kFcsMaxId];
Short_t mScMap_sipm[kFcsNDet][kFcsMaxId];
Short_t mScMap_pp[kFcsNDet][kFcsMaxId];
Short_t mScMap_j[kFcsNDet][kFcsMaxId];

//Reverse SC map
Short_t mRScMap_det[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxBranch][kFcsMaxAddr][kFcsMaxSiPM];
Short_t mRScMap_id[kFcsEHP][kFcsNorthSouth][kFcsMaxDepBd][kFcsMaxBranch][kFcsMaxAddr][kFcsMaxSiPM];

//PatchPanel Map
const short EPPMap[8][6][3]={ // {dep#,low_ch/high_ch,pwr&ctrl row#}
    {{20, 0, 1},{20, 1,-1},  //PPB1 P2,P3
     { 0, 0, 2},{ 0, 1,-1},  //PPB1 P4,P5
     { 1, 0, 3},{ 1, 1,-1}}, //PPB1 P6,P7
    {{ 2, 0,-2},{ 2, 1,-1},  //PPB2 P2,P3
     { 3, 0,-2},{ 3, 1,-1},  //PPB2 P4,P5
     { 4, 0,-2},{ 4, 1,-1}}, //PPB2 P6,P7
    {{22, 0, 4},{23, 0,-1},  //PPB3 P2,P3
     { 5, 0, 5},{ 5, 1,-1},  //PPB3 P4,P5
     { 6, 0, 6},{ 6, 1,-1}}, //PPB3 P6,P7
    {{ 7, 0, 7},{ 7, 1,-1},  //PPB4 P2,P3
     { 8, 0, 8},{ 8, 1,-1},  //PPB4 P4,P5
     { 9, 0, 9},{ 9, 1,-1}}, //PPB4 P6,P7
    {{10, 0,10},{10, 1,-1},  //PPB5 P2,P3
     {11, 0,11},{11, 1,-1},  //PPB5 P4,P5
     {12, 0,12},{12, 1,-1}}, //PPB5 P6,P7
    {{13, 0,13},{13, 1,-1},  //PPB6 P2,P3
     {14, 0,14},{14, 1,-1},  //PPB6 P4,P5
     {22, 1,15},{23, 1,-1}}, //PPB6 P6,P7
    {{15, 0,-2},{15, 1,-1},  //PPB7 P2,P3
     {16, 0,-2},{16, 1,-1},  //PPB7 P4,P5
     {17, 0,-2},{17, 1,-1}}, //PPB7 P6,P7
    {{18, 0,16},{18, 1,-1},  //PPB8 P2,P3
     {19, 0,17},{19, 1,-1},  //PPB8 P4,P5
     {21, 0,-1},{21, 1,-1}}  //PPB8 P6,P7
};

const short HPPMap[4][6][3]={ // {dep#,low_ch/high_ch,pwr&ctrl row#}
    {{ 6, 0, 1},{ 6, 1,-1},  //PPB1 P2,P3
     { 0, 0, 2},{ 1, 0,-1},  //PPB1 P4,P5
     {-1,-1,-1},{-1,-1,-1}}, //PPB1 P6,P7
    {{ 2, 0, 3},{ 0, 1,-1},  //PPB2 P2,P3
     { 1, 1, 4},{ 2, 1,-1},  //PPB2 P4,P5
     { 8, 0, 5},{-1,-1,-1}}, //PPB2 P6,P7
    {{ 8, 1, 6},{-1,-1,-1},  //PPB3 P2,P3
     { 3, 0, 7},{ 4, 0,-1},  //PPB3 P4,P5
     { 5, 0, 8},{ 3, 1,-1}}, //PPB3 P6,P7
    {{ 4, 1, 9},{ 5, 1,-1},  //PPB4 P2,P3
     { 7, 0,10},{ 7, 1,-1},  //PPB4 P4,P5
     {-1,-1,-1},{-1,-1,-1}}  //PPB4 P6,P7
};

Short_t	EMapPPB[24][2];
Short_t EMapPPP[24][2];
Short_t EMapSCDEP[17];
Short_t EMapSCBRA[17];
Short_t EMapSCPP[17];
Short_t EMapSCJ[17];
Short_t	HMapPPB[24][2];
Short_t HMapPPP[24][2];
Short_t HMapSCDEP[10];
Short_t HMapSCBRA[10];
Short_t HMapSCPP[10];
Short_t HMapSCJ[10];

char* colW[4]={"Green ","Brown ","Orange","Blue  "};
char* colJ[8]={"Blue  ","Orange","Violet","Black ",
	       "Yellow","Red   ","Grey  ","Blue  "};
float leng[8]={     6.5,     6.5,     5.0,    5.0,
      	            3.5,     3.5,     8.0,    8.0};

Int_t maxDetectorId() {return kFcsNDet;}

Int_t detectorId(int eh, int ns) { 
    if(eh>=0 && eh<kFcsEHP && ns>=0 && ns<kFcsNorthSouth) return eh*2 + ns;
    return -1;
}

Int_t ecalHcalPres(Int_t det) {
    if(det==0 || det==1) return 0;
    if(det==2 || det==3) return 1;
    if(det==4 || det==5) return 2;
    return -1;
}

Int_t northSouth(Int_t det){   
    return det%2;
}

Int_t nRow(Int_t det){ 
    int ehp=ecalHcalPres(det);
    int ns = northSouth(det);
    if     (ehp==0){return kFcsEcalNRow;}
    else if(ehp==1){return kFcsHcalNRow;}
    else if(ehp==2){return kFcsPresNRow;}
    return -1;
}

Int_t nColumn(Int_t det) { 
    int ehp=ecalHcalPres(det);
    int ns = northSouth(det);
    if     (ehp==0){return kFcsEcalNCol;}
    else if(ehp==1){return kFcsHcalNCol;}
    else if(ehp==2){return kFcsPresNCol;}
    return -1;
}

Int_t maxId(Int_t det) { 
    int ehp=ecalHcalPres(det);
    if     (ehp==0){return kFcsEcalMaxId;}
    else if(ehp==1){return kFcsHcalMaxId;}
    else if(ehp==2){return kFcsPresMaxId;}
    return -1;
}

Int_t getRowNumber(Int_t det, Int_t id) { 
    if(id<0 || id>=maxId(det)) return -1;
    return id/nColumn(det) + 1;
}

Int_t getColumnNumber(Int_t det, Int_t id) { 
    if(id<0 || id>=maxId(det)) return -1;
    return id%nColumn(det) + 1;
}

Int_t getId(Int_t det, Int_t row, Int_t col) { 
    if(row<=0 || row>nRow(det) || nRow(det)<0) return -1;
    if(col<=0 || col>nColumn(det) || nRow(det)<0) return -1;    
    return col - 1 + nColumn(det)*(row-1);
}                                                                                        

Int_t getDepCh(Int_t dep, Int_t ch) { 
  return dep*kFcsMaxDepCh + ch;
}                                                                                        

Float_t getDetectorAngle(Int_t det) { 
    if(det==0) return  1.73;
    if(det==1) return  1.73;
    if(det==2) return  1.73;
    if(det==3) return  1.73;
    if(det==4) return  0.0;
    if(det==5) return  0.0;
    return 0.0;
}

Float_t getXWidth(Int_t det) { 
    if(det==0) return  5.542+0.05;
    if(det==1) return  5.542+0.05;
    if(det==2) return  10.00+0.02;
    if(det==3) return  10.00+0.02;
    if(det==4) return  85.0;
    if(det==5) return  85.0;
    return 0.0;
}

Float_t getYWidth(Int_t det) { 
    if(det==4) return  5.00+0.02;
    if(det==5) return  5.00+0.02;
    return getXWidth(det);
}

Float_t getZDepth(Int_t det) {
    if(det==0 || det==1) {return 30.97;} //66*(0.4+0.01+0.01)+(66-1)*0.05
    if(det==2 || det==3) {return 84.24;} //36*2.34
    else                 {return 1.0;}
}

Float_t getShowerMaxZ(Int_t det) { 
    if(det==0 || det==1) return 15.0;
    if(det==2 || det==3) return 25.0;
    return 0.0;
}

Int_t   getZeroSuppression(Int_t det) {return 1;}

void getDepfromId(Int_t detectorId, Int_t id, Int_t &ehp, Int_t &ns, Int_t &crt, Int_t &slt, Int_t &dep, Int_t &ch) {
    ehp=-1; ns=-1; crt=-1; slt=-1; dep=-1; ch=-1;
    if(detectorId<0 || detectorId>=kFcsNDet) return;
    if(id<0 || id>=kFcsMaxId) return;
    ehp = mMap_ehp[detectorId][id];
    ns  = mMap_ns [detectorId][id];
    crt = mMap_crt[detectorId][id];
    slt = mMap_slt[detectorId][id];
    dep = mMap_dep[detectorId][id];
    ch  = mMap_ch [detectorId][id];
    return;
}

void getIdfromDep(Int_t ehp, Int_t ns, Int_t dep, Int_t ch, Int_t &detectorId, Int_t &id, Int_t &crt, Int_t &slt) {
    detectorId=6; id=4095; crt=0; slt=0;
    if(ehp<0 || ehp>=kFcsEHP) return;
    if(ns<0  || ns>=kFcsNorthSouth) return;
    if(dep<0 || dep>=kFcsMaxDepBd) return;
    if(ch<0  || ch>=kFcsMaxDepCh) return;
    detectorId = mRMap_det[ehp][ns][dep][ch];
    id         = mRMap_id [ehp][ns][dep][ch];
    crt        = mRMap_crt[ehp][ns][dep][ch];
    slt        = mRMap_slt[ehp][ns][dep][ch];
    return;
}

int getNDep(Int_t ehp, Int_t ns) {
    switch(ehp){
    case 0: return 24;
    case 1: return 9;
    case 2: return 6;
    case 3: return 3;
    default: return 0;
    }
}

int jacketColor(int ehp, int dep, int ch){
    // char* colJ[8]={"Blue  ","Orange","Violet","Yellow",
    //                "Green ","Red   ","Grey  ","Black "};
    switch(ehp){
    case 0: 
	if(dep<=19) return dep%5;
	switch(dep){
	case 20: case 21:
	    if(ch<4) return 7;
	    return ch/4;
	case 22: return 6;
	case 23: return 5;
	default: return -1;
	}
    case 1:	
	switch(dep){
	case 0: return 0;
	case 1: return 1;
	case 2: return 2;
	case 3: return 3;
	case 4: return 4;
	case 5: return 6;
	case 6: 
	    if(ch<8 ) return 3;
	    if(ch<16) return 4;
	    if(ch<24) return 6;
	    return -1;
	case 7: 
	    if(ch<8 ) return 0;
	    if(ch<16) return 1;
	    if(ch<24) return 2;
	    return -1;
	case 8: return 5;
	default: return -1;
	}
    default:
	return -1;
    }
}
    
void  makePPMap(){
    for(int b=1; b<=8; b++){
	for(int p=2; p<=7; p++){
	    short dep = EPPMap[b-1][p-2][0];
	    short lh  = EPPMap[b-1][p-2][1];
	    short scr = EPPMap[b-1][p-2][2];
	    if(dep>=0){
		EMapPPB[dep][lh]=b;
		EMapPPP[dep][lh]=p;
		if(scr>0){
		    EMapSCDEP[scr-1]=dep;
		    EMapSCBRA[scr-1]=lh;
		    EMapSCPP[scr-1]=b;
		    EMapSCJ[scr-1]=p/2;
		}
	    }
	}
    }
    for(int b=1; b<=4; b++){
	for(int p=2; p<=7; p++){
	    short dep = HPPMap[b-1][p-2][0];
	    short lh  = HPPMap[b-1][p-2][1];
	    short scr = HPPMap[b-1][p-2][2];
	    if(dep>=0){
		HMapPPB[dep][lh]=b;
		HMapPPP[dep][lh]=p;
		if(scr>0){
		    HMapSCDEP[scr-1]=dep;
		    HMapSCBRA[scr-1]=lh;
		    HMapSCPP[scr-1]=b;
		    HMapSCJ[scr-1]=p/2;
		}
	    }
	}
    }
}

void  makeMap(){
    makePPMap();
    int ehp,crt,slt,dep,ch,det,id;
    for(int det=0; det<kFcsNDet; det++){
      for(int id=0; id<kFcsMaxId; id++){
	mMap_ehp[det][id]=-1;
	mMap_ch[det][id]=-1;
	mMap_ppb[det][id]=-1;
	mMap_ppp[det][id]=-1;
	mMap_pph[det][id]=-1;
	mMap_wcol[det][id]=-1;
	mMap_jcol[det][id]=-1;
	mScMap_ehp[det][id]=-1;
	mScMap_pp[det][id]=-1;
	mScMap_j[det][id]=-1;
      }
    }
    for(int ehp=0; ehp<kFcsEHP; ehp++){
      for(int ns=0; ns<kFcsNorthSouth; ns++){
        for(int dep=0; dep<kFcsMaxDepBd; dep++){
	  for(int ch=0; ch<kFcsMaxDepCh; ch++){
	    mRMap_det[ehp][ns][dep][ch]=6;
	    mRMap_id[ehp][ns][dep][ch]=-1;
	  }
	  for(int bra=0; bra<kFcsMaxBranch; bra++){
	    for(int add=0; add<kFcsMaxAddr; add++){
	      for(int sipm=0; sipm<kFcsMaxSiPM; sipm++){
		mRScMap_det[ehp][ns][dep][bra][add][sipm]=-1;
	      }
	    }
	  }
	}
      }
    }
    
    //Ecal                                                                                                                        
    ehp=0;
    for(int ns=0; ns<2; ns++){
        id=0;
	det = ns;
        for(int row=1; row<=kFcsEcalNRow; row++){
            for(int col=1; col<=kFcsEcalNCol; col++){
                if     (row== 1){crt=1+ns*2; slt=16; dep=20; ch=col-1;} //top row    (ch0-21)  
                else if(row==34){crt=1+ns*2; slt=17; dep=21; ch=col-1;} //bottom row (ch0-21)
		else if(col== 1){crt=1+ns*2; slt=18; dep=22; ch=row-2;} //near side column (ch0-31)
                else if(col==22){crt=1+ns*2; slt=19; dep=23; ch=row-2;} //far side column (ch0-31)
                else{
                    crt=ns*4;
                    dep=(col-2)/4 + (row-2)/8*5;
		    slt=dep;
                    ch =(col-2)%4 + ((row-2)%8)*4;
                }
		mMap_ehp[det][id] = ehp; 
		mMap_ns [det][id] = ns; 
		mMap_crt[det][id] = crt; 
		mMap_slt[det][id] = slt; 
		mMap_dep[det][id] = dep; 
		mMap_ch [det][id]   =ch ; 
		mRMap_det[ehp][ns][dep][ch] = det;
		mRMap_id [ehp][ns][dep][ch] = id ;
		mRMap_crt[ehp][ns][dep][ch] = crt;
		mRMap_slt[ehp][ns][dep][ch] = slt;
		
		//cable
		int lh= ch/16;		
		int b = EMapPPB[dep][lh];
		int p = EMapPPP[dep][lh];
		int h = (ch%16)/4 + (p-2)*4 + 1;
		int w = ch%4;
		int j = jacketColor(ehp,dep,ch);
		mMap_ppb[det][id] = b; 
		mMap_ppp[det][id] = p; 
		mMap_pph[det][id] = h;
		mMap_wcol[det][id] = w; 
		mMap_jcol[det][id] = j;

		//SC map
		int scr=(row-1)/2;
		unsigned short scdep=EMapSCDEP[scr];
		unsigned short bra=EMapSCBRA[scr];
		unsigned short pp=EMapSCPP[scr];
		unsigned short pj=EMapSCJ[scr];;
		unsigned short add=(col-1)/2;
		unsigned short sipm;
		if(ns==0){ 
		    sipm = (col-1)%2 + ((row-1)%2)*2;
		}else{       
		    sipm = col%2 + ((row-1)%2)*2;
		}
		mScMap_ehp[det][id]  = ehp;
		mScMap_ns[det][id]   = ns;
		mScMap_dep[det][id]  = scdep;
		mScMap_bra[det][id]  = bra;
		mScMap_add[det][id]  = add;
		mScMap_sipm[det][id] = sipm;
		mScMap_pp[det][id]   = pp;
		mScMap_j[det][id]    = pj;
		mRScMap_det[ehp][ns][scdep][bra][add][sipm]=det;
		mRScMap_id[ehp][ns][scdep][bra][add][sipm]=id;

		id++;
            }
        }
    }

    //Hcal
    ehp=1;
    for(int ns=0; ns<2; ns++){
	id=0;
	det = ns + 2;
	crt = 1+ns*2;
        for(int row=1; row<=kFcsHcalNRow; row++){
            for(int col=1; col<=kFcsHcalNCol; col++){
                if     (col==13){dep=8; ch=row-1;}                         //far side column (ch0-19)
                else if(row== 1){dep=6; ch=(col-1)%4 + ((col-1)/4)*8    ;} //top row (ch0,1,2,3, 8, 9...19)             
                else if(row== 2){dep=6; ch=(col-1)%4 + ((col-1)/4)*8 + 4;} //2nd row (ch4,5,6,7,12,13...23)            
                else if(row==19){dep=7; ch=(col-1)%4 + ((col-1)/4)*8    ;} //2nd bottom row             
                else if(row==20){dep=7; ch=(col-1)%4 + ((col-1)/4)*8 + 4;} //bottom row  
                else{
		    dep= (col-1)/4 + ((row-3)/8)*3;
                    ch = (col-1)%4 + ((row-3)%8)*4;
                }
		slt=dep;
		mMap_ehp[det][id] = ehp; 
		mMap_ns [det][id] = ns; 
		mMap_crt[det][id] = crt; 
		mMap_slt[det][id] = slt; 
		mMap_dep[det][id] = dep; 
		mMap_ch[det][id]  =ch ; 
		mRMap_det[ehp][ns][dep][ch] = det;
		mRMap_id [ehp][ns][dep][ch] = id ;
		mRMap_crt[ehp][ns][dep][ch] = crt;
		mRMap_slt[ehp][ns][dep][ch] = slt;

		//cable
		int lh= ch/16;		
		int b = HMapPPB[dep][lh];
		int p = HMapPPP[dep][lh];
		int h = (ch%16)/4 + (p-2)*4 + 1;
		int w = ch%4;
		int j = jacketColor(ehp,dep,ch);
		mMap_ppb[det][id] = b; 
		mMap_ppp[det][id] = p; 
		mMap_pph[det][id] = h;
		mMap_wcol[det][id] = w; 
		mMap_jcol[det][id] = j;

		//SC map
		unsigned short feerow = (row-1)/2;
		unsigned short scdep=HMapSCDEP[feerow];
		unsigned short bra=HMapSCBRA[feerow];
		unsigned short pp=HMapSCPP[feerow];
		unsigned short pj=HMapSCJ[feerow];
		unsigned short add=col-1;
		unsigned short sipm;
		if(ns==0){ 
		    sipm = (row-1)%2;
		}else{       
		    sipm = row%2;
		}
		mScMap_ehp[det][id]  = ehp;
		mScMap_ns[det][id]   = ns;
		mScMap_dep[det][id]  = scdep;
		mScMap_bra[det][id]  = bra;
		mScMap_add[det][id]  = add;
		mScMap_pp[det][id]   = pp;
		mScMap_j[det][id]    = pj;
		mScMap_sipm[det][id] = sipm;
		mRScMap_det[ehp][ns][scdep][bra][add][sipm]=det;
		mRScMap_id[ehp][ns][scdep][bra][add][sipm]=id;
		id++;
            }
        }
    }

    //EPD west as PRES 
    //   EPD PP=  1 ~ 6 ==> ns=0 (north) and dep=0 (near top) to 5 (near bottom)
    //   EPD PP= 12 ~ 7 ==> ns=1 (south) and dep=0 (near top) to 5 (near bottom), odd/even reversed
    ehp=2;
    for(int ns=0; ns<2; ns++){
	det = ns + 4;
	crt = 1+ns*2;
	for(int dep=0; dep<6; dep++){
            for(int ch=0; ch<32; ch++){
		slt=dep+10;
		id=dep*32+ch;
		mMap_ehp[det][id] = ehp; 
		mMap_ns [det][id] = ns; 
		mMap_crt[det][id] = crt; 
		mMap_slt[det][id] = slt; 
		mMap_dep[det][id] = dep; 
		mMap_ch[det][id]  = ch ; 
		mRMap_det[ehp][ns][dep][ch] = det;
		mRMap_id [ehp][ns][dep][ch] = id ;
		mRMap_crt[ehp][ns][dep][ch] = crt;
		mRMap_slt[ehp][ns][dep][ch] = slt;
            }
        }
    }
}

void getIdfromEPD(Int_t pp, Int_t tt, Int_t& det, Int_t &id){
    det=-1; 
    id=-1;
    int row,col;
    if(tt<0 || tt>=32) return;
    if(pp>=1 && pp<=6){ //north side
	det=4;
	row=(pp-1)*2 + (tt+1)%2 + 1;
	col=tt/2;
    }else if(pp>=7 && pp<=12){ //south side
	det=5;
	row=(12-pp)*2 + (tt)%2 + 1;
	col=tt/2;
    }
    id=(row-1)*16 + col;
}

void getEPDfromId(Int_t det, Int_t id, Int_t &pp, Int_t &tt){
    int row=id/16 + 1;
    int col=id%16 + 1;
    if(det==4){
	pp = (row-1)/2 + 1;
	tt = (col-1)*2 - (row-1)%2 + 1;
    }else if(det==5){
	pp = 12 - (row-1)/2;
	tt = (col-1)*2 + (row-1)%2;
    }
}

void printHeader(FILE* f, int flag=0, int csv=0){
  fprintf(f,"### Detector\n");
  fprintf(f,"#det : 0/1=ECal-N/S 2/3=Hcal-N/S  4/5=PRS-N/S\n");
  fprintf(f,"#id  : 0~747 for Ecal  0~259 for Hcal\n");
  fprintf(f,"#row : 1~34 for Ecal  1~20 for Hcal\n");
  fprintf(f,"#col : 1~22 for Ecal  1~13 for Hcal\n");
  fprintf(f,"### Readout\n");
  fprintf(f,"#ehp : 0=ECal  1=Hcal  2=PRS\n");
  fprintf(f,"#ns  : 0=north  1=south\n");
  fprintf(f,"#crt : 0=EcalNorth  1=MixNorth  2=Main  3=MixSouth  4=EcalSouth\n");
  fprintf(f,"#slt : 0~19\n");
  fprintf(f,"#dep : 0~24 for Ecal  0~7 for Hcal  0~3 for Pres\n");
  fprintf(f,"#ch  : 0~31 for all DEP\n");
  fprintf(f,"### Patchpanel and cable\n");  
  fprintf(f,"#FRow: FEEBd Row = (row-1)/2   [0-16 for Ecal  0-9  for Hcal]\n");
  fprintf(f,"#FCol: FEEBd Col = FEEBd Addr  [0-10 for Ecal  0-12 for Hcal]\n");
  fprintf(f,"#SiPM: FEEBd Ch#               [0-3  for Ecal  0-1  for Hcal]\n");
  fprintf(f,"#ppb : PatchPanel Board#       [1~8  for Ecal  1-4  for Hcal]\n");
  fprintf(f,"#ppp : PatchPanel MDR cable P# [2-7]\n");
  fprintf(f,"#pph : PatchPanel Header#      [1-24]\n");
  fprintf(f,"#jcol: Cable Jacket color\n");
  fprintf(f,"#wcol: Cable Wire color\n");  
  fprintf(f,"#length: Cable length\n");
  if(csv==0){
      if(flag==0){
	  fprintf(f,"#det id row col     ehp  ns crt slt dep  ch   Frow Fcol SiPM ppb ppp pph jcol   wcol  length\n");
      }else{
	  fprintf(f,"#ehp ns dep  ch   crt slt   det  id row col   Frow Fcol SiPM ppb ppp pph jcol   wcol  length\n");
      }
  }else{
      if(flag==0){
	  fprintf(f,"#det,id,row,col,ehp,ns,crt,slt,dep,ch,Frow,Fcol,SiPM,ppb,ppp,pph,jcol,wcol,length\n");
      }else{
	  fprintf(f,"#ehp,ns,dep,ch,crt,slt,det,id,row,col,Frow,Fcol,SiPM,ppb,ppp,pph,jcol,wcol,length\n");
      }
  }
}

void printHeader2(FILE* f){
    fprintf(f,"# Css-DDch\n");
    fprintf(f,"#  C  : crate 0~4 for EcalN,MixN,Main,MixS,EcalS\n");
    fprintf(f,"#  ss : slot (0~19)\n");
    fprintf(f,"#  DD : DEP bd# (0~24 for Ecal, 0~7 for Hcal\n");
    fprintf(f,"#  ch : DEP ch# (0~31 for all DEP)\n");
}

void printHeader3(FILE* f){
    fprintf(f,"# crt-slt : ns ehp dep : DNiii-RR/CC DNiii-RR/CC DNiii-RR/CC ...\n");
    fprintf(f,"#   D : \"E\"cal, \"H\"cal, \"P\"res\n");
    fprintf(f,"#   N : North or South\n");
    fprintf(f,"# iii : id\n");
    fprintf(f,"#  RR : Row\n");
    fprintf(f,"#  CC : Column\n");
}

void printHeader4(FILE* f, int flag=0){
  fprintf(f,"### Detector\n");
  fprintf(f,"#det : 0/1=ECal N/S, 2/3=Hcal N/S , 4/5=PRS N/S\n");
  fprintf(f,"#id  : 0~747 for Ecal, 0~259 for Hcal\n");
  fprintf(f,"#row : 1~34 for Ecal, 1~20 for Hcal\n");
  fprintf(f,"#col : 1~22 for Ecal, 1~13 for Hcal\n");
  fprintf(f,"### Slow Control\n");
  fprintf(f,"#ehp        : 0=ECal, 1=Hcal, 2=PRS\n");
  fprintf(f,"#ns         : 0=north, 1=south\n");
  fprintf(f,"#dep        : 0~24 for Ecal, 0~7 for Hcal, 0~3 for Pres\n");
  fprintf(f,"#branch     : 0~1\n");
  fprintf(f,"#switch addr: 0~15\n");
  fprintf(f,"#SiPM#      : 0~3 for Ecal, 0~1 for Hcal&Pres\n");
  fprintf(f,"### Patchpanel and cable\n");  
  fprintf(f,"#ppb : PatchPanel Board# (1~8 for ecal, 1-4 for hcal)\n");
  fprintf(f,"#J   : PatchPanel SC conection (J1~J3)\n");
  if(flag==0){
      fprintf(f,"#det id row col     ehp  ns dep bra add SiPM  ppb  J\n");
  }else{
      fprintf(f,"#ehp  ns dep bra add SiPM   det  id row col\n");
  }
}

void printMap(){
    int ehp,ns,crt,slt,dep,ch,det,id,row,col;
    
    FILE *f1  = fopen("fcsMap.txt","w");           printHeader(f1);
    FILE *f1c = fopen("fcsMap.csv","w");           printHeader(f1c,0,1);
    FILE *f1e = fopen("fcs_ecal_readout_map.csv","w"); printHeader(f1e);
    FILE *f1h = fopen("fcs_hcal_readout_map.csv","w"); printHeader(f1h);
    FILE *f1p = fopen("fcs_pres_readout_map.csv","w"); printHeader(f1p);    
    FILE *f2 = fopen("fcsMap2.txt","w");    printHeader2(f2);    
    FILE *f3 = fopen("fcsDepMap.txt","w");  printHeader(f3,1);    
    FILE *f3c= fopen("fcsDepMap.csv","w");  printHeader(f3c,1,1);    
    FILE *f4 = fopen("fcsDepMap2.txt","w"); printHeader3(f4);    
    FILE *f5  = fopen("fcsScMap.txt","w");    printHeader4(f5);
    FILE *f5e = fopen("fcs_ecal_sc_map.csv","w"); printHeader4(f5e);
    FILE *f5h = fopen("fcs_hcal_sc_map.csv","w"); printHeader4(f5h);
    FILE *f5p = fopen("fcs_pres_sc_map.csv","w"); printHeader4(f5p);
    FILE *f6  = fopen("fcsScRevMap.txt","w"); printHeader4(f6,1);

    FILE *f7  = fopen("fcsEpdMap.txt","w");    
    FILE *fpp = fopen("fcsPPMap.txt","w");    

    char* EHP[3]={"Ecal","Hcal","Pres"};
    char* CRT[5]={"EN","MN","MA","MS","ES"};
    char* DET[6]={"EN","ES","HN","HS","PN","PS"};
    
    //Ecal
    for(ns=0; ns<2; ns++){
	det=ns;
	id=0;
	fprintf(f2,"Ecal NS=%1d\n",ns);
	for(row=1; row<=nRow(det); row++){
	    for(col=1; col<=nColumn(det); col++){
		if(mMap_ehp[det][id]>=0){
		    fprintf(f1,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d    %2d  %2d  %2d     %2d  P%1d H%02d %6s %6s %3.1f\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns [det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch [det][id],
			    (row-1)/2,
			    mScMap_add[det][id],
			    mScMap_sipm[det][id],
			    mMap_ppb[det][id],
			    mMap_ppp[det][id],
			    mMap_pph[det][id],
			    colJ[mMap_jcol[det][id]],
			    colW[mMap_wcol[det][id]],
			    leng[mMap_jcol[det][id]]);
		    fprintf(f1c,"%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%2d,%2d,%2d,%2d,P%1d,H%02d,%6s,%6s,%3.1f\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns [det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch [det][id],
			    (row-1)/2,
			    mScMap_add[det][id],
			    mScMap_sipm[det][id],
			    mMap_ppb[det][id],
			    mMap_ppp[det][id],
			    mMap_pph[det][id],
			    colJ[mMap_jcol[det][id]],
			    colW[mMap_wcol[det][id]],
			    leng[mMap_jcol[det][id]]);
		    fprintf(f1e,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns [det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch [det][id]); 
		    fprintf(f2,"%1d%02d-%02d%02d ",
			    mMap_crt[det][id],mMap_slt[det][id],
			    mMap_dep[det][id],mMap_ch[det][id]);
		}
		if(mScMap_ehp[det][id]>=0){
		    fprintf(f5,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d    %2d   J%1d\n",
                            det,id,row,col,
                            mScMap_ehp[det][id],
                            mScMap_ns [det][id],
                            mScMap_dep[det][id],
                            mScMap_bra[det][id],
                            mScMap_add[det][id],
                            mScMap_sipm[det][id],
                            mScMap_pp[det][id],
                            mScMap_j[det][id]);
		    fprintf(f5e,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
                            det,id,row,col,
                            mScMap_ehp[det][id],
                            mScMap_ns [det][id],
                            mScMap_dep[det][id],
                            mScMap_bra[det][id],
                            mScMap_add[det][id],
                            mScMap_sipm[det][id]);
		}
		id++;
	    }
	    fprintf(f2,"\n");
	}
    }
    
    //Hcal
    for(int ns=0; ns<2; ns++){
	det=ns+2;
	id=0;
	fprintf(f2,"Hcal NS=%1d\n",ns);
	for(row=1; row<=nRow(det); row++){
	    for(col=1; col<=nColumn(det); col++){
		if(mMap_ehp[det][id]>=0){
		    fprintf(f1,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d    %2d  %2d  %2d     %2d  P%1d H%02d %6s %6s\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns[det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch[det][id],
                            (row-1)/2,
                            mScMap_add[det][id],
                            mScMap_sipm[det][id],
			    mMap_ppb[det][id],
			    mMap_ppp[det][id],
			    mMap_pph[det][id],
			    colJ[mMap_jcol[det][id]],
			    colW[mMap_wcol[det][id]]); 
		    fprintf(f1c,"%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%3d,%2d,%2d,%2d,%2d,P%1d,H%02d,%6s,%6s\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns[det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch[det][id],
                            (row-1)/2,
                            mScMap_add[det][id],
                            mScMap_sipm[det][id],
			    mMap_ppb[det][id],
			    mMap_ppp[det][id],
			    mMap_pph[det][id],
			    colJ[mMap_jcol[det][id]],
			    colW[mMap_wcol[det][id]]); 
		    fprintf(f1h,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns[det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch[det][id]); 
		    fprintf(f2,"%1d%02d-%02d%02d ",
			    mMap_crt[det][id],mMap_slt[det][id],
			    mMap_dep[det][id],mMap_ch[det][id]);
		}
		if(mScMap_ehp[det][id]>=0){
		    fprintf(f5,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d    %2d   J%1d\n",
                            det,id,row,col,
                            mScMap_ehp[det][id],
                            mScMap_ns [det][id],
                            mScMap_dep[det][id],
                            mScMap_bra[det][id],
                            mScMap_add[det][id],
                            mScMap_sipm[det][id],
                            mScMap_pp[det][id],
                            mScMap_j[det][id]);
		    fprintf(f5h,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
                            det,id,row,col,
                            mScMap_ehp[det][id],
                            mScMap_ns [det][id],
                            mScMap_dep[det][id],
                            mScMap_bra[det][id],
                            mScMap_add[det][id],
                            mScMap_sipm[det][id]);
		}
		id++;
	    }
	    fprintf(f2,"\n");
	}
    }
    
    //Prs
    for(int ns=0; ns<2; ns++){
	det=ns+4;
	id=0;
	fprintf(f2,"PRS NS=%1d\n",ns);
	for(row=1; row<=nRow(det); row++){
	    for(col=1; col<=nColumn(det); col++){
		if(mMap_ehp[det][id]>=0){
		    fprintf(f1,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns[det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch[det][id]); 
		    fprintf(f1p,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
			    det,id,row,col,
			    mMap_ehp[det][id], 
			    mMap_ns[det][id], 
			    mMap_crt[det][id],
			    mMap_slt[det][id],
			    mMap_dep[det][id],
			    mMap_ch[det][id]); 
		    fprintf(f2,"%1d%02d-%02d%02d ",
			    mMap_crt[det][id],mMap_slt[det][id],
			    mMap_dep[det][id],mMap_ch[det][id]);
		}
		if(mScMap_ehp[det][id]>=0){
		    fprintf(f5,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
                            det,id,row,col,
                            mScMap_ehp[det][id],
                            mScMap_ns [det][id],
                            mScMap_dep[det][id],
                            mScMap_bra[det][id],
                            mScMap_add[det][id],
                            mScMap_sipm[det][id]);
		    fprintf(f5p,"%3d %3d %3d %3d     %3d %3d %3d %3d %3d %3d\n",
                            det,id,row,col,
                            mScMap_ehp[det][id],
                            mScMap_ns [det][id],
                            mScMap_dep[det][id],
                            mScMap_bra[det][id],
                            mScMap_add[det][id],
                            mScMap_sipm[det][id]);
		}
		id++;
	    }
	    fprintf(f2,"\n");
	}
    }
        
    //DEP map
    for(ehp=0; ehp<kFcsEHP; ehp++){
	for(ns=0; ns<2; ns++){
	    for(dep=0; dep<kFcsMaxDepBd; dep++){
		if(ehp==1 && dep>8) break;
		if(ehp==2 && dep>3) break;
		int flag=0;
		for(ch=0; ch<kFcsMaxDepCh; ch++){
		    if(mRMap_det[ehp][ns][dep][ch]<0){
			if(flag==1 && ch%8==7) fprintf(f4,"\n");
			continue;
		    }
		    flag=1;
		    det = mRMap_det[ehp][ns][dep][ch];
		    id  = mRMap_id[ehp][ns][dep][ch];
		    crt=mRMap_crt[ehp][ns][dep][ch];
		    slt=mRMap_slt[ehp][ns][dep][ch];
		    if(ch==0){
			fprintf(f4,"%2s%2d : NS=%1d %1d(%4s) DEP%02d : ",CRT[crt],slt,ns,ehp,EHP[ehp],dep);
		    }else if(ch%8==0){
			fprintf(f4,"                          : ");
		    }
		    if(det>=0 && det<kFcsNDet){
			row=getRowNumber(det,id);
			col=getColumnNumber(det,id);
			if(ehp<2){
 			    fprintf(f3,"%3d %3d %3d %3d   %2s %2d   %3d %3d %3d %3d      %2d  %2d  %2d     %2d  P%1d H%02d %6s %6s %3.1f\n",
				    ehp,ns,dep,ch,
				    CRT[crt],slt,
				    det,id,row,col,
				    (row-1)/2,
				    mScMap_add[det][id],
				    mScMap_sipm[det][id],
				    mMap_ppb[det][id],
				    mMap_ppp[det][id],
				    mMap_pph[det][id],
				    colJ[mMap_jcol[det][id]],
				    colW[mMap_wcol[det][id]],
				    leng[mMap_jcol[det][id]]); 
			    fprintf(f3c,"%3d,%3d,%3d,%3d,%2s,%2d,%3d,%3d,%3d,%3d,%2d,%2d,%2d,%2d,P%1d,H%02d,%6s,%6s,%3.1f\n",
				    ehp,ns,dep,ch,
				    CRT[crt],slt,
				    det,id,row,col,
				    (row-1)/2,
				    mScMap_add[det][id],
				    mScMap_sipm[det][id],
				    mMap_ppb[det][id],
				    mMap_ppp[det][id],
				    mMap_pph[det][id],
				    colJ[mMap_jcol[det][id]],
				    colW[mMap_wcol[det][id]],
				    leng[mMap_jcol[det][id]]); 
			}else{
 			    fprintf(f3,"%3d %3d %3d %3d   %2s %2d   %3d %3d %3d %3d\n",
				    ehp,ns,dep,ch,
				    CRT[crt],slt,
				    det,id,row,col);
			    fprintf(f3c,"%3d,%3d,%3d,%3d,%2s,%2d,%3d,%3d,%3d,%3d\n",
				    ehp,ns,dep,ch,
				    CRT[crt],slt,
				    det,id,row,col);
			}
		    }
		    fprintf(f4,"%2s%03d-%02d/%02d ",DET[det],id,row,col);  
		    if(ch%8==7) fprintf(f4,"\n");
		}
	    }
	}
    }

    //SC reverse map
    for(ehp=0; ehp<kFcsEHP; ehp++){
	for(ns=0; ns<2; ns++){
	    for(dep=0; dep<kFcsMaxDepBd; dep++){
		if(ehp==1 && dep>8) break;
		if(ehp==2 && dep>3) break;
		int flag=0;
		for(int bra=0; bra<kFcsMaxBranch; bra++){
		    for(int add=0; add<kFcsMaxAddr; add++){
			for(int sipm=0; sipm<kFcsMaxSiPM; sipm++){
			    if(mRScMap_det[ehp][ns][dep][bra][add][sipm]>=0){
				int det=mRScMap_det[ehp][ns][dep][bra][add][sipm];
				int id =mRScMap_id[ehp][ns][dep][bra][add][sipm];
				int col=getColumnNumber(det,id);
				int row=getRowNumber(det,id);
				fprintf(f6,"%3d %3d %3d %3d %3d %3d     %3d %3d %3d %3d\n",
					ehp,ns,dep,bra,add,sipm,
					det,id,row,col);
			    }
			}
		    }
		}
	    }
	}
    }
    
    //EPD map
    fprintf(f7,"#ehp ns crt slt dep  ch    det  id row col     pp  tt   Reversed(det id r c)\n");
    for(int det=4; det<=5; det++){
	for(int r=1; r<=nRow(det); r++){
	    for(int c=1; c<=nColumn(det); c++){
		int id = getId(det,r,c);
		int ehp,ns,crt,slt,dep,ch,det2,id2,r2,c2,pp,tt;
		getDepfromId(det,id,ehp,ns,crt,slt,dep,ch);
		getEPDfromId(det,id,pp,tt);
		getIdfromEPD(pp,tt,det2,id2);
		c2=getColumnNumber(det2,id2);
		r2=getRowNumber(det2,id2);
		fprintf(f7,"%3d %3d %3d %3d %3d %3d    %3d %3d %3d %3d    %3d %3d   %3d %3d %3d %3d\n",
			ehp,ns,crt,slt,dep,ch,
			det,id,r,c,
			pp,tt,
			det2,id2,r2,c2);
	    }
	}
    }

    for(int ehp=0; ehp<2; ehp++){
	int bmax=0;
	if(ehp==0) {bmax=8; fprintf(fpp,"Ecal\n");}
	if(ehp==1) {bmax=4; fprintf(fpp,"Hcal\n");}
	fprintf(fpp,"PPB# P# DEP  Ch  T                                                              Pwr/ctrl Row#\n");    
	for(int b=1; b<=bmax; b++){
	    for(int p=2; p<=7; p++){
		int ns =0;
		int dep,lh,scr;
		if(ehp==0){
		    dep = EPPMap[b-1][p-2][0];
		    lh  = EPPMap[b-1][p-2][1];
		    scr = EPPMap[b-1][p-2][2];
		}else{
		    dep = HPPMap[b-1][p-2][0];
		    lh  = HPPMap[b-1][p-2][1];
		    scr = HPPMap[b-1][p-2][2];
		}		    		
		fprintf(fpp,"%2d %2d ",b,p);
		if(lh>=0) {
		    fprintf(fpp,"%2d  %02d-%02d  ",dep,lh*16,lh*16+15);
		    for(int i=0; i<4; i++){
			int t   = (p-2)*4+i;
			int ch  = lh*16 + i*4;
			int det = mRMap_det[ehp][ns][dep][ch];
			int id  = mRMap_id[ehp][ns][dep][ch]; 
			int row = getRowNumber(det,id);
			int col = jacketColor(ehp,dep,ch);
			if(id>=0) { 
			    fprintf(fpp,"T%02d=R%02d-%6s  ",t,row,colJ[col]);
			}else{
			    fprintf(fpp,"T%02d=            ",t);
			}
		    }
		}else{
		    fprintf(fpp," -                                                                         ");
		}
		if(p%2==0) {
		    fprintf(fpp,"J%1d   ",p/2);
		    if(scr>0) fprintf(fpp,"%2d,%2d",scr*2-1,scr*2);
		    else if(scr==-2) fprintf(fpp,"no power");
		    else      fprintf(fpp,"-");
		}
		fprintf(fpp,"\n");
	    }
	}
    }

    fclose(f1);
    fclose(f1c);
    fclose(f1e);
    fclose(f1h);
    fclose(f1p);
    fclose(f2);
    fclose(f3);
    fclose(f3c);
    fclose(f4);
    fclose(f5);
    fclose(f5e);
    fclose(f5h);
    fclose(f5p);
    fclose(f6);
    fclose(f7);
    fclose(fpp);
}
