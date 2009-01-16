#include "SSD_Reader.hh"
#include "StDaqLib/EVP/ssdReader.h"

#include <assert.h>
#include <math.h>

static unsigned short log8to10_table[256] = {
    0,    1,    2,    3,    4,    5,    6,    7,
    8,    9,   10,   11,   12,   13,   14,   15,
   16,   17,   18,   19,   20,   21,   22,   23,
   24,   25,   26,   27,   28,   29,   30,   31,
   32,   33,   34,   35,   36,   37,   38,   39,
   40,   41,   42,   43,   44,   45,   46,   47,
   48,   49,   50,   51,   52,   53,   54,   55,
   56,   57,   58,   59,   60,   61,   62,   63,
   64,   65,   66,   67,   68,   69,   70,   71,
   72,   73,   74,   75,   76,   77,   78,   79,
   80,   81,   82,   83,   84,   85,   86,   87,
   88,   89,   90,   91,   92,   93,   94,   95,
   96,   97,   98,   99,  100,  101,  102,  103,
  104,  105,  106,  107,  108,  110,  112,  114,
  116,  118,  120,  122,  124,  127,  129,  131,
  133,  136,  138,  140,  143,  145,  147,  150,
  152,  155,  158,  160,  163,  166,  168,  171,
  174,  177,  180,  182,  185,  188,  192,  195,
  198,  201,  204,  208,  211,  214,  218,  221,
  225,  228,  232,  236,  240,  243,  247,  251,
  255,  259,  263,  267,  271,  275,  280,  284,
  288,  293,  297,  302,  306,  311,  316,  321,
  325,  330,  335,  341,  346,  351,  356,  362,
  367,  373,  379,  384,  390,  396,  402,  408,
  414,  420,  427,  433,  439,  446,  452,  459,
  466,  473,  480,  487,  494,  502,  509,  517,
  525,  532,  540,  548,  556,  564,  573,  581,
  589,  598,  607,  615,  624,  634,  643,  652,
  662,  672,  681,  691,  701,  711,  722,  732,
  743,  753,  764,  775,  786,  798,  809,  821,
  833,  845,  857,  870,  882,  895,  907,  920,
  934,  947,  961,  975,  989, 1003, 1017, 1023 };

// #include <mapPad.h>
// #include <mapTime.h>

using namespace OLDEVP;

int SSD_Reader::ssdData(int  ladder,
			char eastWest,
			int  channel,
			int &data,
			int &ped,
			int &noise) {

  int cnt=0,strip,pad,i,det,pos,mtime,time,daqLadder;
  static unsigned int cache1[12288];
  static unsigned int cache2[12288];
  static int daqLadderInit=-1;

  data = -1; ped  = -1; noise= -1;

  // Set daqLadder.
  daqLadder = SSDDaqLadder(eastWest,ladder);


  
  // For speed, we cache the data for one ladder.  Adapted from Boucham's special.C.
  if(daqLadderInit!=daqLadder) {
    daqLadderInit=daqLadder;
    for(i=0;i<12288;i++) { cache1[i]=0; cache2[i]=0; } // This is necessary -- not all members are reset if ssd.mode==0.
    if(ssd.mode==0) { // real data
      pos=0;
      for(time=0;time<192;time++) { // Though "time" is not used explicitly, it's involved in incrementeing "pos".
        for(pad=0;pad<64;pad++) {
	  if (!ssd.counts[daqLadder][pad]) continue;
          for(mtime=0;mtime<ssd.counts[daqLadder][pad];mtime++) {
            if (time==ssd.strip[daqLadder][pad][mtime])
            {
              det=pos/768; strip=pos%768;
              assert(pos>=0&&pos<12288);
              cache1[pos]=log8to10_table[ssd.adc[daqLadder][pad][mtime]];
              cnt++;
            } // if(time
          }   // for(mtime
          pos++;
        }     // for(pad
      }       // for(time
    } else if(ssd.mode==1) { // pedestal data
      pos=0;
      for(time=0;time<192;time++) {
        for(pad=0;pad<64;pad++) {
          cache1[pos]=ssd.adc[daqLadder][pad][time];
          cache2[pos]=ssd.strip[daqLadder][pad][time]; // "strip"s role in ped data is completely different than in real data.
          pos++;
        }
      }
    } else {
      assert(0); // unknown value of ssd.mode
    }
  }         // if(daqLadderInit ...

  if(ssd.mode==0) {
    data=cache1[channel];
  } else if(ssd.mode==1) {
    ped=cache1[channel];
    noise=cache2[channel];
  } else {
    assert(0); // unknown value of ssd.mode
  }

  return ssd.mode ;
} 

SSD_Reader::SSD_Reader(EventReader *er) {

  //static int call=0;

  char *datap; // ,ew;

  datap=er->getDATAP(); 
  if (datap) {

  unsigned int UTime = er->getEventInfo().UnixTime;
  struct tm *time=gmtime((time_t*) &UTime);

  //LDate = (((1900+time->tm_year)*100 + 1 + time->tm_mon)*100 + time->tm_mday)*100;
  //LDate = yyyymmdd
  //year 2005 = 105 
  //month : starts from 0
  LDate = (1900+time->tm_year)*10000 + (1 + time->tm_mon)*100 + time->tm_mday;
  LTime = (time->tm_hour*100 + time->tm_min)*100 + time->tm_sec;
  //cout << "year=" << time->tm_year << " month = " << time->tm_mon << " day=" << time->tm_mday << endl;
#ifndef NEW_DAQ_READER_DAQLIB
     OLDEVP::ssdReader(datap); // call the "event pool" code
  }
#else
  else {
     daqReader *rdr=er->getDaqReader(); assert(rdr);
     ::ssdReader((char*)rdr); // call the "event pool" code
  }
#endif
}

int SSD_Reader::SSDDaqLadder(char eastWest, int ladder)
{
  int daqLadder=-1;
  //cout <<"LDate ="   << LDate << endl;
  if ( (LDate < 20040731) && (LTime < 1)){
    // Year4 data format stops on July 31st 2004 
    // cout << "SSD_Reader::SSDDaqLadder : Year4 data " << endl;
    switch(eastWest) {
    case 'E':
      switch(ladder) {
      case   1: daqLadder=38; break;
      case   2: daqLadder=36; break;
      case   3: daqLadder=39; break;
      case   9: daqLadder=29; break;
      case  10: daqLadder=26; break;
      case  11: daqLadder=28; break;
      case  12: daqLadder=23; break;
      case  13: daqLadder=20; break;
      case  19: daqLadder=30; break;
      case  20: daqLadder=33; break;
      }
      break;
      
    case 'W':
      switch(ladder) {
      case   1: daqLadder=18; break;
      case   2: daqLadder=16; break;
      case   3: daqLadder=19; break;
      case   9: daqLadder= 9; break;
      case  10: daqLadder= 6; break;
      case  11: daqLadder= 8; break;
      case  12: daqLadder= 3; break;
      case  13: daqLadder= 0; break;
      case  19: daqLadder=10; break;
      case  20: daqLadder=13; break;
      }
      break;
    default: assert(0); // Someone called this function with a bad value of eastWest.
    } 
  }
  if ((LDate >= 20040731) && (LDate <= 20050731))
    {
      // Year 2005 data format post July 31st 2004
      //cout << "SSD_Reader::SSDDaqLadder : Year5 data " << endl;
      switch(eastWest) {
      case 'E':
	switch(ladder) {
	case   1: daqLadder=27; break;
	case   2: daqLadder=29; break;
	case   3: daqLadder=26; break;
	case   4: daqLadder=28; break;
	case   5: daqLadder=25; break;
	case   6: daqLadder=22; break;
	case   7: daqLadder=24; break;
	case   8: daqLadder=21; break;
	case   9: daqLadder=23; break;
	case  10: daqLadder=20; break;
	case  11: daqLadder=30; break;
	case  12: daqLadder=33; break;
	case  13: daqLadder=31; break;
	case  14: daqLadder=34; break;
	case  15: daqLadder=32; break;
	case  16: daqLadder=35; break;
	case  17: daqLadder=38; break;
	case  18: daqLadder=36; break;
	case  19: daqLadder=39; break;
	case  20: daqLadder=37; break;
	}
	break;
	
      case 'W':
	switch(ladder) {
	case   1: daqLadder=7; break;
	case   2: daqLadder=9; break;
	case   3: daqLadder=6; break;
	case   4: daqLadder=8; break;
	case   5: daqLadder=5; break;
	case   6: daqLadder=2; break;
	case   7: daqLadder=4; break;
	case   8: daqLadder=1; break;
	case   9: daqLadder=3; break;
	case  10: daqLadder=0; break;
	case  11: daqLadder=10; break;
	case  12: daqLadder=13; break;
	case  13: daqLadder=11; break;
	case  14: daqLadder=14; break;
	case  15: daqLadder=12; break;
	case  16: daqLadder=15; break;
	case  17: daqLadder=18; break;
	case  18: daqLadder=16; break;
	case  19: daqLadder=19; break;
	case  20: daqLadder=17; break;
	}
	break;
      default: assert(0); // Someone called this function with a bad value of eastWest.
      }
    }
  if(LDate > 20060731) {
    // Year 2007 data format post July 31st 2006
    //cout << "SSD_Reader::SSDDaqLadder : Year7 data " << endl;
    switch(eastWest) {
    case 'E':
      switch(ladder) {
      case   1: daqLadder=27; break;
      case   2: daqLadder=29; break;
      case   3: daqLadder=26; break;
      case   4: daqLadder=28; break;
      case   5: daqLadder=25; break;
      case   6: daqLadder=22; break;
      case   7: daqLadder=24; break;
      case   8: daqLadder=21; break;
      case   9: daqLadder=23; break;
      case  10: daqLadder=20; break;
      case  11: daqLadder=30; break;
      case  12: daqLadder=33; break;
      case  13: daqLadder=31; break;
      case  14: daqLadder=34; break;
      case  15: daqLadder=32; break;
      case  16: daqLadder=35; break;
      case  17: daqLadder=38; break;
      case  18: daqLadder=36; break;
      case  19: daqLadder=39; break;
      case  20: daqLadder=37; break;
      }
      break;
      
    case 'W':
      switch(ladder) {
      case   1: daqLadder=17; break;
      case   2: daqLadder=19; break;
      case   3: daqLadder=16; break;
      case   4: daqLadder=18; break;
      case   5: daqLadder=15; break;
      case   6: daqLadder=12; break;
      case   7: daqLadder=14; break;
      case   8: daqLadder=11; break;
      case   9: daqLadder=13; break;
      case  10: daqLadder=10; break;
      case  11: daqLadder=0; break;
      case  12: daqLadder=3; break;
      case  13: daqLadder=1; break;
      case  14: daqLadder=4; break;
      case  15: daqLadder=2; break;
      case  16: daqLadder=5; break;
      case  17: daqLadder=8; break;
      case  18: daqLadder=6; break;
      case  19: daqLadder=9; break;
     case  20: daqLadder=7; break;
      }
      break;
    default: assert(0); // Someone called this function with a bad value of eastWest.
    }  
  }
  // Check ladder and eastWest args.  Also check completeness of above switches.
  assert(daqLadder>=0 && daqLadder < 40); 
  return daqLadder;
}
