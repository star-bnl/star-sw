 
#include <cstdlib>
#include <cassert>
#include <cstdio>

#include "EEname2Index.h"
//__________________________________________________
//__________________________________________________
void  EEindexRange(int secID,int &ix1, int &ix2){
  // returns range of valid index for given sector
  assert(secID>=0);
  assert(secID<=12);
  ix1=(secID-1)*1000;
  ix2=ix1+999;
}


//__________________________________________________
//__________________________________________________
void  EEindex2Name(int ix,char *name){
   assert(ix>=0);
   assert(ix<EEindexMax);

   int sec=1+ix/1000;
   assert(sec>0 && sec<=12);

   int jx=ix%1000;

   int key=jx/100;

   char ckey=' ';

   switch(key) {
   case 0: ckey='T';break;
   case 1: ckey='P';break;
   case 2: ckey='Q';break;
   case 3: ckey='R';break;
   case 4:
   case 5:
   case 6: ckey='U';break;
   case 7:
   case 8:
   case 9: ckey='V';break;
   default:
     printf("EEindex2Name()  Logical Error1: invalid index=%d\n",ix);
     exit(-1);
   }

   //   printf("key=%d ckey=%c, jx=%d\n",key, ckey,jx);
      
   switch(ckey) {
   case 'T': 
   case 'P': 
   case 'Q': 
   case 'R': {
     jx=jx%100 -1;
     int sub ='A'+jx/12;
     assert(sub>='A' && sub<='E');
     int eta=1+jx%12;
     assert(eta>0 && eta <=12);
     sprintf(name,"%2.2d%c%c%2.2d",sec,ckey,sub,eta);
     break;}

   case 'V': jx-=300;
   case 'U': {
    int strip=jx-400;
    assert(strip>0 && strip<=288);
    sprintf(name,"%2.2d%c%3.3d",sec,ckey,strip);
    break;}
  
  default:
    printf("EEindex2Name()  Logical Error2: invalid index=%d\n",ix);
    exit(-1);
   }

   //   printf("EEindex2Name(%d) -->%s \n",ix,name);
}


//__________________________________________________
//__________________________________________________
int  EEname2Index(const char *name){

  int index=-1;

  int sec=atoi(name);
  assert(sec>0 && sec<=12);
  
  index=(sec-1)*1000;

  if(sec<10 && name[0]!='0') name--; // compensate for missing proceeding zero in sectorID
  char key=name[2];
  
  switch(key) {
  case 'R': index+=100;
  case 'Q': index+=100;
  case 'P': index+=100;
  case 'T': {
    int sub =name[3];
    assert(sub>='A' && sub<='E');
    int eta=atoi(name+4);
    assert(eta>0 && eta <=12);
    index+=(sub-'A')*12 +eta;
  }  break;
  case 'V': index+=300;
  case 'U': {index+=400;
    int strip=atoi(name+3);
    assert(strip>0 && strip<=288);
    index+=strip;
  } break;
  
  default:
    printf("EEname2Index('%s')  Logical Error3: invalid index=%d\n",name,index);
    exit(-1);
  }

  //  printf("EEname2Index(%s)-->%d\n",name,index);
  
  assert(index>=0);
  assert(index<EEindexMax);
  
  return index;
  
}
