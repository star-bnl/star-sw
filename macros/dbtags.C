void dbtags() {
static const char *DBaliases[]={
  "sd97"   ,     "sd98",  "year_1a",  "year_1b",  "year_1c",
  "es99"   ,     "er99",     "dc99",  "year_1d",  "year_1e",
  "y2000"  ,   "year_2a", "year_2b",  "year2001", "year2003",
  "y2003x" ,   "y2003a",   "y2003b",   
  "y2004"  ,   "y2004x",   "y2004a",  "y2004b",   "y2004c", 
  "y2005"  ,   "y2005x",   "y2005b",  "y2005c",
          0
 };   

 static const int   DBdates[]=  {
  19970101,    19980101,   19990101,   19990501,   19991001,
  19990615,    19990616,   19991206,   19991101,   19991201,
  20000614,    20010610,   20010501,   20010615,   20021115, 
  20021115,    20021115,   20021115,  
  20031120,    20031120,   20031120,   20031120,   20031125,
  20041030,    20041030,   20041101,   20041201,
         0
 };
 
 static const int   DBtimes[]=  {
         0,           0,          0,          0,          0,
         0,      120000,      80000,          0,          0,
    175430,           0,          0,          0,          0,        
         0,           0,          0,         
	 0,           0,          0,          0,          0,
	 0,           0,          0,          0,
         0
 };
 Int_t i = 0;
 Int_t Ndt = sizeof(DBtimes)/sizeof(int) - 1;
 TArrayI idxT(Ndt); Int_t *idx = idxT.GetArray();
 TArrayD dT(Ndt); Double_t *d = dT.GetArray();
 for (i = 0; i < Ndt && DBaliases[i]; i++) {
   d[i] = DBdates[i] + ((double) DBtimes[i])/1000000.;
   TMath::Sort(Ndt,d,idx,0);
 }
 for (i = 0; i < Ndt && DBaliases[i]; i++) {
   Int_t j = idx[i];
   printf("{\"%s\",\t%6d,%6d},\n",DBaliases[j],DBdates[j],DBtimes[j]);
 }
}
