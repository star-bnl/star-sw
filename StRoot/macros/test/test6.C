// #include "St_pamc_Module.h"
test6(){
Int_t i;
Int_t j;
Int_t k;
printf(" Creating the tables \n");

St_scalars  *sc = new St_scalars(10);
St_vectors  *ve = new St_vectors(10);

printf(" Calling the 'pamc'module \n");

table_head_st *t1_h = sc.GetHeader();
table_head_st *t2_h = ve.GetHeader(); 

t1_h.nok = 10;
t2_h.nok = 10;

TFile ff("pamc.root");

ve.Read("vectors");
sc.Read("scalars");

printf(" Checking the results \n");

table_head_st *t1_h = sc.GetHeader();
table_head_st *t2_h = ve.GetHeader(); 

printf(" Table = %s, type = %s_st \n", t1_h->name, t1_h->type);
printf(" Table = %s, type = %s_st \n", t2_h->name, t2_h->type);
printf(" t1_h.nok = %i, t2_h.nok = %i \n", t1_h.nok, t2_h.nok);
printf(" t1_h.rbytes = %i, t2_h.rbytes = %i \n", t1_h.rbytes, t2_h.rbytes);

scalars_st *t1 = sc.GetTable();
vectors_st *t2 = ve.GetTable();

for(k=0;k<(t1_h->nok);k++) {
    printf(" scalars_st t1[%i]: \n ", k);
    printf("\t aShort=%i \n", t1[k].aShort);
    printf("\t aUshort=%i\n", t1[k].aUshort);
    printf("\t aLong=%i\n",   t1[k].aLong);
    printf("\t aUlong=%i\n",  t1[k].aUlong);
    printf("\t aChar=%c\n",   t1[k].aChar);
    printf("\t aOctet=%x\n",  t1[k].aOctet);
    printf("\t aFloat=%f\n",  t1[k].aFloat);
    printf("\t aDouble=%e\n", t1[k].aDouble);
    printf("\n");
}
for(k=0;k<(t2_h->nok);k++){
    printf(" vectors_st t2[%i]: \n ", k);


 //*-*      t2[i].bShorts[j] = i+j;
      printf("\t bShorts[3]: ");
      for(j=0;j<3;j++) printf("\t %i ", t2[k].bShorts[j]);
      printf("\n");

 //*-*	    t2[i].bUshorts[j] = i+j;
      printf("\t bUshorts[3]: ");
      for(j=0;j<3;j++) printf("\t %i ", t2[k].bUshorts[j]);
      printf("\n");

 //*-*      t2[i].bLongs[j] = i+j;
      printf("\t bLongs[3]: ");
      for(j=0;j<3;j++) printf("\t %i ", t2[k].bLongs[j]);
      printf("\n");

 //*-*      t2[i].bUlongs[j] = i+j;
      printf("\t bUlongs[3]: ");
      for(j=0;j<3;j++) printf("\t %i ", t2[k].bUlongs[j]);
      printf("\n");

 //*-*      t2[i].bChars[j] = '-';
    printf("\t aOctet=%x\n", t1[k].aOctet);
      printf("\t bChars[3]: ");
      for(j=0;j<3;j++) printf("\t '%x' ", t2[k].bChars[j]);
      printf("\n");

 //*-*      t2[i].bOctets[j] = i+j;
      printf("\t bOctets[3]: ");
      for(j=0;j<3;j++) printf("\t %x ", t2[k].bOctets[j]);
      printf("\n");

 //*-*      t2[i].bFloats[j] = i+j;
      printf("\t bFloats[3]: ");
      for(j=0;j<3;j++) printf("\t %f ", t2[k].bFloats[j]);
      printf("\n");

 //*-*       t2[i].bDoubles[j] = i+j;
      printf("\t bDoubles[3]: ");
      for(j=0;j<3;j++) printf("\t %e ", t2[k].bDoubles[j]);
      printf("\n");
     
 }
 
}
