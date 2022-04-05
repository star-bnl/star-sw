#ifndef __CINT__
#include "TRMatrix.h"
#else
class TRMatrix;
#endif
void Newguy(const char *t1, const char *t2) {
 cout << " ---------------------------------------------------------------" << endl <<
         " Routing " << t2 << " testing " << t1 << endl;
}
//________________________________________________________________________________
void Tmxm() {// F110
  /* Initialized data */
    TRMatrix ac(3,2," 0. 1. 2. 3. 4. 5."); 
    TRMatrix acA(2,3,ac.GetArray());
    TRMatrix bc(2,4," 0. 20. 30. 40. 50. 60. 70. 80.");
    TRMatrix bcA(4,2,bc.GetArray());
  //TRMatrix c1c(3,4,"50. 60. 70. 80. 170. 220. 270. 320. 290.  380. 470. 560.");
    TRMatrix c1c(3,4,"50. 60. 70. 80. 150. 220. 270. 320. 250.  380. 470. 560.");
    TRMatrix c1cA(4,3,c1c.GetArray());
    TRMatrix c2c(3,3,"3. 4. 5. 9. 14. 19. 15. 24. 33.");
    TRMatrix d__(3,4,"2. 4. 6. 8. 102. 104. 106. 108. 202. 204. 206. 208.");
    TRMatrix d_A(4,3,d__.GetArray());
    TRMatrix d_3(3,3,d__.GetArray());
    TRMatrix bct(bc,TRArray::kTransposed);
    TRMatrix act(ac,TRArray::kTransposed);
    Double_t zeru = 1.e6;
    /*        TEST FOR MXMPY-1-2-3 */
    Newguy("MXMPY-1-2-3.", "TMXM    ");
    TRMatrix *temp = 0;
    temp = new TRMatrix(ac,TRArray::kAxB,bc); //[3,2]*[2,4]
    if (temp->Verify(c1c)) {
      cout << "ac:" << ac << endl;
      cout << "bc:" << bc << endl;
      cout << "Temp kAxB: ac x bc" << *temp << endl;
      cout << "c1c " << c1c << endl;
    }
    else cout << "ac,kAxB,bc Passed" << endl;
    delete temp;
    temp = new TRMatrix(ac,TRArray::kAxBT,bct);//[3,2]*[4,2]T
    if (temp->Verify(c1c)) {
      cout << "act:" << act << endl;
      cout << "bct:" << bct << endl;
      cout << "Temp kAxBT: ac x bctT " << *temp << endl;
      cout << "c1c " << c1c << endl;
    }
    else cout << "ac,kAxBT,bct Passed" << endl;
    delete temp;
    temp = new TRMatrix(act,TRArray::kATxB,bc);//[2,3]T*[2,4]
    if (temp->Verify(c1c)) {
      cout << "act:" << act << endl;
      cout << "bc:" << bc << endl;
      cout << "Temp kATxB: actT x bc" << *temp << endl;
      cout << "c1c " << c1c << endl;
    }
    else cout << "act,kATxB,bc Passed" << endl;
    delete temp;

    temp = new TRMatrix(act,TRArray::kATxBT,bct);//[2,3]T*[4,2]T
    if (temp->Verify(c1c)) {
      cout << "act:" << act << endl;
      cout << "bct:" << bct << endl;
      cout << "Temp kATxBT: actT x bctT" << *temp << endl;
      cout << "c1c " << c1c << endl;
    }
    else cout << "act,kATxBT,bct Passed" << endl;
    delete temp;
    
    temp = new TRMatrix(ac,TRArray::kAxB,acA); //[3,2]*[2,3]
    if (temp->Verify(c2c)) {
      cout << "ac:" << ac << endl;
      cout << "acA:" << acA << endl;
      cout << "Temp kAxB: ac x acA" << *temp << endl;
      cout << "c2c " << c2c << endl;
    }
    else cout << "ac,kAxB,acA Passed" << endl;
    delete temp;
    /*        TEST FOR MXMAD-12-3- */
    Newguy("MXMAD-1-2-3.", "TMXM    ");
    TRMatrix aplus(c1c);
    aplus += d__;
    TRMatrix amin(c1c);
    amin  -= d__;
    TRMatrix aplus1(ac,TRArray::kAxB,acA);
    TRMatrix amin2(aplus1);
    aplus1 += d_3;
    amin2  -= d_3;
    temp = new TRMatrix(d__);
    temp->Add(ac,TRArray::kAxB,bc);
    if (temp->Verify(aplus)) {
      cout << "ac:" << ac << endl;
      cout << "acA:" << acA << endl;
      cout << "Temp Add kAxB: ac x acA" << *temp << endl;
      cout << "aplus " << aplus << endl;
    }
    else cout << "Add ac,kAxB,acA Passed" << endl;
    delete temp;

    temp = new TRMatrix(d__);
    temp->Add(ac,TRArray::kAxBT,bct);
    if (temp->Verify(aplus)) {
      cout << "ac:" << ac << endl;
      cout << "bct:" << bct << endl;
      cout << "Temp Add kAxBT: ac x bctT" << *temp << endl;
      cout << "aplus " << aplus << endl;
    }
    else cout << "Add ac,kAxBT,bct Passed" << endl;
    delete temp;

    temp = new TRMatrix(d__);
    temp->Add(act,TRArray::kATxB,bc);
    if (temp->Verify(aplus)) {
      cout << "act:" << act << endl;
      cout << "bc:" << bc << endl;
      cout << "Temp Add kATxB: actT x bc" << *temp << endl;
      cout << "aplus " << aplus << endl;
    }
    else cout << "Add act,kATxB,bc Passed" << endl;
    delete temp;

    
    temp = new TRMatrix(d__);
    temp->Add(act,TRArray::kATxBT,bct);
    if (temp->Verify(aplus)) {
      cout << "act:" << act << endl;
      cout << "bct:" << bct << endl;
      cout << "Temp Add kATxBT: actT x bctT" << *temp << endl;
      cout << "aplus " << aplus << endl;
    }
    else cout << "Add act,kATxBT,bct Passed" << endl;
    delete temp;

    temp = new TRMatrix(d_3);
    temp->Add(ac,TRArray::kAxB,acA);
    if (temp->Verify(aplus1)) {
      cout << "ac:" << ac << endl;
      cout << "acA:" << acA << endl;
      cout << "Temp Add kAxB: ac x acA" << *temp << endl;
      cout << "aplus " << aplus << endl;
    }
    else cout << "Add act,kATxBT,bct Passed" << endl;
    delete temp;
/*        TEST FOR MXMUB-1-2-3 */
    Newguy("MXMUB-1-2-3.", "TMXM    ");
    TRMatrix dmin__(3,4);
    dmin__ -= d__;
    temp = new TRMatrix(d__);
    temp->Substruct(ac,TRArray::kAxB,bc);
    if (temp->Verify(amin)) {
      cout << "ac:" << ac << endl;
      cout << "bc:" << ac << endl;
      cout << "Temp kAxB: ac x bc" << *temp << endl;
      cout << "amin " << amin << endl;
    }
    else cout << "Substruct(ac,kAxB,bc) Passed" << endl;
    delete temp;

    temp = new TRMatrix(d__);
    temp->Substruct(ac,TRArray::kAxBT,bct);
    if (temp->Verify(amin)) {
      cout << "ac:" << ac << endl;
      cout << "bct:" << act << endl;
      cout << "Temp kAxBT: ac x bctT" << *temp << endl;
      cout << "amin " << amin << endl;
    }
    else cout << "Substruct(ac,kAxBT,bct) Passed" << endl;
    delete temp;

    temp = new TRMatrix(d__);
    temp->Substruct(act,TRArray::kATxB,bc);
    if (temp->Verify(amin)) {
      cout << "act:" << act << endl;
      cout << "bc:" << bc << endl;
      cout << "Temp kATxB: actT x bc" << *temp << endl;
      cout << "amin " << amin << endl;
    }
    else cout << "Substruct(act,kATxB,bc) Passed" << endl;
    delete temp;

    temp = new TRMatrix(d__);
    temp->Substruct(act,TRArray::kATxBT,bct);
    if (temp->Verify(amin)) {
      cout << "act:" << act << endl;
      cout << "bct:" << bct << endl;
      cout << "Temp kATxBT: actT x bctT" << *temp << endl;
      cout << "amin " << amin << endl;
    }
    else cout << "Substruct(act,kATxBT,bct) Passed" << endl;
    delete temp;


    temp = new TRMatrix(d_3);
    temp->Substruct(ac,TRArray::kAxB,acA);
    if (temp->Verify(amin2)) {
      cout << "ac:" << ac << endl;
      cout << "acA:" << acA << endl;
      cout << "Temp kAxB: ac xx acA" << *temp << endl;
      cout << "amin2 " << amin2 << endl;
    }
    else cout << "Substruct(ac,kAxB,acA) Passed" << endl;
    delete temp;
} /* tmxm_ */

//____________________________________________________________________________________
void ttrinv()
{
  // ttrinv.F -- translated by f2c (version 19970219).
  /* Initialized data */
  
  TRSymMatrix ac(4,"1. 0. 0. 2. 0. 13. 4. 0. 23. 77. ");
  TRSymMatrix rc(4," 1.45679 0. 0. -.191358 0. .1882716 -.0185185 0. -.0462963 .02777778 ");
  
  Newguy("TRINV -TRSINV.", "TTRINV  ");
  Double_t zerlev = 1e-4;
  TRSymMatrix *B = new  TRSymMatrix(ac,TRArray::kInverted);
  if (B->Verify(rc,zerlev)) {
    cout << "ac:" << ac << endl;
    cout << "rc:" << rc << endl;
    cout << "B ac,kInverted" << *B << endl;
  }
  else cout << "ac,kInverted Passed" << endl;
  delete B;

} /* trinv */


//____________________________________________________________________________________
void ttrla()
{
// ttrla.F -- translated by f2c (version 19970219).
    /* Initialized data */
  TRSymMatrix dc(4," 1. 2. 3. 0. 0. 0. 4. 5. 0. 6. ");
  //?  TRMatrix  ec(4,3,"4. 7. 3. 17. 32. 18. 0. 0. 0. 43. 64. 44.");
  TRSymMatrix ac(4,"1. 0. 0. 2. 0. 13. 4. 0. 23. 77.");
  TRSymMatrix sc(3,"1384. 1712. 2201. 858. 1075. 538.");
  TRSymMatrix atsac(3," 239. 331. 447. 248. 345. 257. ");
  TRSymMatrix qsqc(4," 1265. 1594. 2009. 0. 0. 0. 1940. 2446. 0. 2980. ");
  TRMatrix sac(4,3," 18. 23. 19. 27. 37. 28. 0. 0. 0. 43. 64. 44. ");
  TRMatrix asc(3,4," 30. 44. 0. 69. 34. 49. 0. 74. 17. 26.  0. 42.");
  //  TRMatrix atsac[6] = { 239. 331. 447. 248. 345. 257. };

  Double_t zerlev = 1e-6;

  Newguy("TRAAT-TRATA.", "TTRLA   ");
  TRMatrix  ec(4,3,"4. 7. 3. 3. 6. 4. 0. 5. 5. 2. 1. 2.");
  TRSymMatrix *A = new TRSymMatrix(ec,TRArray::kAxAT);
  TRSymMatrix wc(4,"74. 66. 61. 50. 50. 50. 21. 20. 15. 9.");
  if (A->Verify(wc,zerlev)) {
    cout << "ec:" << ec << endl;
    cout << "wc:" << wc << endl;
    cout << "A ec,kAxAT" << *A << endl;
  }
  else cout << "ec,kAxAT passed" << endl;
  delete A;

  TRMatrix  ecA(3,4,ec.GetArray());
  TRSymMatrix *A = new TRSymMatrix(ecA,TRArray::kATxA);
  TRSymMatrix xc(4,"77. 62. 69. 17.  23. 10. 52. 45. 11. 38.");
  if (A->Verify(xc,zerlev)) {
    cout << "ecA:" << ecA << endl;
    cout << "xc:" << xc << endl;
    cout << "A ecA,kATxA" << *A << endl;
  }
  else cout << "ecA,kATxA passed" << endl;
  delete A;
  
  Newguy("TRASAT-TRATSA-TRQSQ.", "TTRLA   ");
  TRSymMatrix *A2 = new  TRSymMatrix(ecA,TRArray::kAxSxAT,ac);
  if (A2->Verify(sc,zerlev)) {
    cout << "ecA:" << ecA << endl;
    cout << "ac:" << ac << endl;
    cout << "A ecA,kAxSxAT,ac" << *A2 << endl;
  }
  else cout << "ecA,kAxSxATx,ac Passed" << endl;
  delete A2;

  TRSymMatrix *A3 = new  TRSymMatrix(ec,TRArray::kATxSxA,dc);
  if (A3->Verify(atsac,zerlev)) {
    cout << "ec:" << ec << endl;
    cout << "dc:" << dc << endl;
    cout << "A ec,kATxSxA,dc" << *A3 << endl;
  }
  else cout << "ec,kATxSxA,dc Passed" << endl;
  delete A3;

  TRSymMatrix *A4 = new  TRSymMatrix(dc,TRArray::kRxSxR,ac);
  if (A4->Verify(qsqc,zerlev)) {
    cout << "dc:" << dc << endl;
    cout << "ac:" << ac << endl;
    cout << "A dc,kRxSxR,ac" << *A4 << endl;
  }
  else cout << "dc,kRxSxR,ac Passed" << endl;
  delete A4;
  
  Newguy("TRSA-TRAS-TRSAT-TRATS.", "TTRLA   ");
  TRMatrix *A5 = new  TRMatrix(dc,TRArray::kSxA, ec);
  if (A5->Verify(sac,zerlev)) {
    cout << "dc:" << dc << endl;
    cout << "ec:" << ec << endl;
    cout << "A dc,kSxA, ec" << *A5 << endl;
  }
  else cout << "dc,kSxA, ec Passed" << endl;
  delete A5;


  TRMatrix *A6 = new  TRMatrix(ecA,TRArray::kAxS,dc);
  if (A6->Verify(asc,zerlev)) {
    cout << "ecA:" << ecA << endl;
    cout << "asc:" << asc << endl;
    cout << "" << *A6 << endl;
  }
  else cout << "acA,kAxS,dc Passed" << endl;
  delete A6;


  TRMatrix *A7 = new  TRMatrix(dc,TRArray::kSxAT,ecA);
  TRMatrix satc(4,3,"30. 34. 17. 44. 49. 26. 0. 0. 0. 69. 74. 42.");
  if (A7->Verify(satc,zerlev)) {
    cout << "dc:" << dc << endl;
    cout << "ecA:" << ecA << endl;
    cout << "A dc,kSxAT,ecA" << *A7 << endl;
  }
  else cout << "dc,kSxAT,ecA Passed" << endl;
  delete A7;
  TRMatrix *A8 = new  TRMatrix(ec,TRArray::kATxS,dc);
  TRMatrix  atsc(3,4," 18. 27. 0. 43. 23. 37. 0. 64. 19. 28. 0. 44.");
  if (A8->Verify(atsc,zerlev)) {
    cout << "ec:" << ec << endl;
    cout << "dc:" << dc << endl;
    cout << "A ec,kATxS,dc" << *A8 << endl;
  }
  else cout << "ec,kATxS,dc Passed" << endl;
  delete A8;

  Newguy("TRPCK-TRUPCK.", "TTRLA   ");
  TRMatrix upckc(4,4," 1.,2.,4.,7.,2.,3.,5.,8.,4.,5.,6.,9.,7.,8.,9.,10.");
  TRSymMatrix pckc(4,"1.,2.,3.,4.,5.,6.,7.,8.,9.,10.");
  TRSymMatrix *A9 = new  TRSymMatrix(upckc);
  if (A9->Verify(pckc,zerlev)) {
    cout << "upckc:" << upckc << endl;
    cout << "pckc:" << pckc << endl;
    cout << "A upckc" << *A9 << endl;
  }
  else cout << "packing of upckc Passed" << endl;
  delete A9;
  
  TRMatrix *A10 = new  TRMatrix(pckc);
  if (A10->Verify(upckc,zerlev)) {
    cout << "pckc:" << pckc << endl;
    cout << "upckc:" << upckc << endl;
    cout << "A  pckc" << *A10 << endl;
  }
  else cout << "unpacking of pckc Passed" << endl;


  TRSymMatrix *A11 = new  TRSymMatrix(*A10);
  if (A11->Verify(pckc,zerlev)) {
    cout << "A10:" << *A10 << endl;
    cout << "pckc:" << pckc << endl;
    cout << "A11:" << *A11 << endl;
  }
  else cout << "packing of upckc Passed" << endl;
  TRMatrix *A12 = new  TRMatrix(*A11);
  if (A12->Verify(upckc,zerlev)) {
    cout << "A11:" << *A11 << endl;
    cout << "upckc:" << upckc << endl;
    cout << "A12" << *A12 << endl;
  }
  else cout << "unpacking of A12 Passed" << endl;
  delete A10;
  delete A11;
  delete A12;

} /* trla */

//________________________________________________________________________________
void Micky(){
#ifdef __CINT__
  //  gSystem->Load("libStar");
  //  gSystem->Load("TR");
  //  gSystem->Load("libRootKernel.so");
#endif
#if 1
  Tmxm(); 
  ttrinv();
#endif
  ttrla();
}
