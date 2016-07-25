#include <stdio.h>
#include <string.h>
#include "TString.h"
#include "TDatime.h"
#include "Riostream.h"
Int_t NF = 0;
FILE *fp[73];
Char_t lines[73][121];
//________________________________________________________________________________
UInt_t ReadNextLine(Int_t i) {
  if (! fp[i]) return 0;
  TDatime date;
  Int_t y,m,d,H,M,S;
  UInt_t ut;
  Char_t line[121];
 READ:
  if (! fgets(&line[0],120,fp[i])) {
    NF--; 
    fclose(fp[i]); 
    fp[i] = (FILE *) 0; 
    return 0;
  }
  else {if (strncmp(line,"2005", 4)) goto READ;}
  //  cout << lines[i];
  if (! strcmp(&line[19],&lines[i][19])) goto READ;
  strcpy(lines[i],line);
  if (i < 72) {// ANL time
    sscanf(&lines[i][0],"%4d-%2d-%2d %2d:%2d:%2d",&y,&m,&d,&H,&M,&S);
    date.Set(y,m,d,H,M,S);
    ut = date.Convert()+6*3600;
    if (m > 4 || m == 4 && d > 3 || m == 4 && d == 3 && H >= 2) ut -= 3600; 
  } else { // GMT
    sscanf(&lines[i][0],"%4d-%2d-%2d %2d:%2d:%2d",&y,&m,&d,&H,&M,&S);
    date.Set(y,m,d,H,M,S);
    ut = date.Convert(); 
  }
  return ut;
}
//________________________________________________________________________________
void svtRDOsort() {
  TString FileName(""); 
  for (Int_t i = 0; i < 73; i++) {
    if (i < 72) {
      FileName = Form("SLC%i.text",i);
    } else {
      FileName = "../svtRDOsFull.txtdb01";
    }
    fp[i] = fopen(FileName,"r");
    if (! fp[i]) {
      cout << "Can't open file " << FileName << endl;
      return;
    }
    NF++;
  }
  UInt_t utMin = 1999999999;
UInt_t utime[73];
  for (Int_t i = 0; i < 73; i++) utime[i] = utMin;
  //             1112418000
  Int_t iMin  = -1;
  // Init
  for (Int_t i = 0; i < 73; i++) {
    if (fp[i]) utime[i] = ReadNextLine(i);
  }
  while (NF) {
    //1234567890123456789
    //2005-19-01 18:27:31
    utMin = 1999999999;
    iMin  = -1;
    for (Int_t i = 0; i < 73; i++) {
      if (utime[i] < utMin) {iMin = i; utMin = utime[i];}
    }
    if (iMin < 0) break;
    TDatime dt(utime[iMin]);
    if (dt.GetDate() > 20330000) break;
    cout << Form("%8d.%06d",dt.GetDate(),dt.GetTime()) << " " << &lines[iMin][19];
    if (fp[iMin]) utime[iMin] = ReadNextLine(iMin);
  }
}
