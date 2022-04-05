#ifndef __CINT__
#include "StRoot/TR/TRMatrixD.h"
#else
class TRMatrixD;
#endif
void Newguy(const char *t1, const char *t2) {
 cout << " ---------------------------------------------------------------" << endl <<
         " Routing " << t2 << " testing " << t1 << endl;
}
//________________________________________________________________________________
void Tmxm() {// F110
  /* Initialized data */
    TRMatrixD ac(3,2," 0. 1. 2. 3. 4. 5."); 
    TRMatrixD acA(2,3,ac.GetArray());
    TRMatrixD bc(2,4," 0. 20. 30. 40. 50. 60. 70. 80.");
    TRMatrixD bcA(4,2,bc.GetArray());
  //TRMatrixD c1c(3,4,"50. 60. 70. 80. 170. 220. 270. 320. 290.  380. 470. 560.");
    TRMatrixD c1c(3,4,"50. 60. 70. 80. 150. 220. 270. 320. 250.  380. 470. 560.");
    TRMatrixD c1cA(4,3,c1c.GetArray());
    TRMatrixD c2c(3,3,"3. 4. 5. 9. 14. 19. 15. 24. 33.");
    TRMatrixD d__(3,4,"2. 4. 6. 8. 102. 104. 106. 108. 202. 204. 206. 208.");
    TRMatrixD d_A(4,3,d__.GetArray());
    TRMatrixD d_3(3,3,d__.GetArray());
    TRMatrixD bct(bc,kTransposed);
    TRMatrixD act(ac,kTransposed);
    Double_t zeru = 1.e6;
    /*        TEST FOR MXMPY-1-2-3 */
    Newguy("MXMPY-1-2-3.", "TMXM    ");
    TRMatrixD *temp = 0;
    temp = new TRMatrixD(ac,kAxB,bc); //[3,2]*[2,4]
    if (temp->Verify(c1c)) {
      cout << "ac:" << ac << endl;
      cout << "bc:" << bc << endl;
      cout << "Temp kAxB: ac x bc" << *temp << endl;
      cout << "c1c " << c1c << endl;
    }
    else cout << "ac,kAxB,bc Passed" << endl;
    delete temp;
    temp = new TRMatrixD(ac,kAxBT,bct);//[3,2]*[4,2]T
    if (temp->Verify(c1c)) {
      cout << "act:" << act << endl;
      cout << "bct:" << bct << endl;
      cout << "Temp kAxBT: ac x bctT " << *temp << endl;
      cout << "c1c " << c1c << endl;
    }
    else cout << "ac,kAxBT,bct Passed" << endl;
    delete temp;
    temp = new TRMatrixD(act,kATxB,bc);//[2,3]T*[2,4]
    if (temp->Verify(c1c)) {
      cout << "act:" << act << endl;
      cout << "bc:" << bc << endl;
      cout << "Temp kATxB: actT x bc" << *temp << endl;
      cout << "c1c " << c1c << endl;
    }
    else cout << "act,kATxB,bc Passed" << endl;
    delete temp;

    temp = new TRMatrixD(act,kATxBT,bct);//[2,3]T*[4,2]T
    if (temp->Verify(c1c)) {
      cout << "act:" << act << endl;
      cout << "bct:" << bct << endl;
      cout << "Temp kATxBT: actT x bctT" << *temp << endl;
      cout << "c1c " << c1c << endl;
    }
    else cout << "act,kATxBT,bct Passed" << endl;
    delete temp;
    
    temp = new TRMatrixD(ac,kAxB,acA); //[3,2]*[2,3]
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
    TRMatrixD aplus(c1c);
    aplus += d__;
    TRMatrixD amin(c1c);
    amin  -= d__;
    TRMatrixD aplus1(ac,kAxB,acA);
    TRMatrixD amin2(aplus1);
    aplus1 += d_3;
    amin2  -= d_3;
    temp = new TRMatrixD(d__);
    temp->Add(ac,kAxB,bc);
    if (temp->Verify(aplus)) {
      cout << "ac:" << ac << endl;
      cout << "acA:" << acA << endl;
      cout << "Temp Add kAxB: ac x acA" << *temp << endl;
      cout << "aplus " << aplus << endl;
    }
    else cout << "Add ac,kAxB,acA Passed" << endl;
    delete temp;

    temp = new TRMatrixD(d__);
    temp->Add(ac,kAxBT,bct);
    if (temp->Verify(aplus)) {
      cout << "ac:" << ac << endl;
      cout << "bct:" << bct << endl;
      cout << "Temp Add kAxBT: ac x bctT" << *temp << endl;
      cout << "aplus " << aplus << endl;
    }
    else cout << "Add ac,kAxBT,bct Passed" << endl;
    delete temp;

    temp = new TRMatrixD(d__);
    temp->Add(act,kATxB,bc);
    if (temp->Verify(aplus)) {
      cout << "act:" << act << endl;
      cout << "bc:" << bc << endl;
      cout << "Temp Add kATxB: actT x bc" << *temp << endl;
      cout << "aplus " << aplus << endl;
    }
    else cout << "Add act,kATxB,bc Passed" << endl;
    delete temp;

    
    temp = new TRMatrixD(d__);
    temp->Add(act,kATxBT,bct);
    if (temp->Verify(aplus)) {
      cout << "act:" << act << endl;
      cout << "bct:" << bct << endl;
      cout << "Temp Add kATxBT: actT x bctT" << *temp << endl;
      cout << "aplus " << aplus << endl;
    }
    else cout << "Add act,kATxBT,bct Passed" << endl;
    delete temp;

    temp = new TRMatrixD(d_3);
    temp->Add(ac,kAxB,acA);
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
    TRMatrixD dmin__(3,4);
    dmin__ -= d__;
    temp = new TRMatrixD(d__);
    temp->Substruct(ac,kAxB,bc);
    if (temp->Verify(amin)) {
      cout << "ac:" << ac << endl;
      cout << "bc:" << ac << endl;
      cout << "Temp kAxB: ac x bc" << *temp << endl;
      cout << "amin " << amin << endl;
    }
    else cout << "Substruct(ac,kAxB,bc) Passed" << endl;
    delete temp;

    temp = new TRMatrixD(d__);
    temp->Substruct(ac,kAxBT,bct);
    if (temp->Verify(amin)) {
      cout << "ac:" << ac << endl;
      cout << "bct:" << act << endl;
      cout << "Temp kAxBT: ac x bctT" << *temp << endl;
      cout << "amin " << amin << endl;
    }
    else cout << "Substruct(ac,kAxBT,bct) Passed" << endl;
    delete temp;

    temp = new TRMatrixD(d__);
    temp->Substruct(act,kATxB,bc);
    if (temp->Verify(amin)) {
      cout << "act:" << act << endl;
      cout << "bc:" << bc << endl;
      cout << "Temp kATxB: actT x bc" << *temp << endl;
      cout << "amin " << amin << endl;
    }
    else cout << "Substruct(act,kATxB,bc) Passed" << endl;
    delete temp;

    temp = new TRMatrixD(d__);
    temp->Substruct(act,kATxBT,bct);
    if (temp->Verify(amin)) {
      cout << "act:" << act << endl;
      cout << "bct:" << bct << endl;
      cout << "Temp kATxBT: actT x bctT" << *temp << endl;
      cout << "amin " << amin << endl;
    }
    else cout << "Substruct(act,kATxBT,bct) Passed" << endl;
    delete temp;


    temp = new TRMatrixD(d_3);
    temp->Substruct(ac,kAxB,acA);
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
  
  TRSymMatrixD ac(4,"1. 0. 0. 2. 0. 13. 4. 0. 23. 77. ");
  TRSymMatrixD rc(4," 1.45679 0. 0. -.191358 0. .1882716 -.0185185 0. -.0462963 .02777778 ");
  
  Newguy("TRINV -TRSINV.", "TTRINV  ");
  zerlev = 1e-4;
  TRSymMatrixD *B = new  TRSymMatrixD(ac,kInverted);
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
  TRSymMatrixD dc(4," 1. 2. 3. 0. 0. 0. 4. 5. 0. 6. ");
  //?  TRMatrixD  ec(4,3,"4. 7. 3. 17. 32. 18. 0. 0. 0. 43. 64. 44.");
  TRSymMatrixD ac(4,"1. 0. 0. 2. 0. 13. 4. 0. 23. 77.");
  TRSymMatrixD sc(3,"1384. 1712. 2201. 858. 1075. 538.");
  TRSymMatrixD atsac(3," 239. 331. 447. 248. 345. 257. ");
  TRSymMatrixD qsqc(4," 1265. 1594. 2009. 0. 0. 0. 1940. 2446. 0. 2980. ");
  TRMatrixD sac(4,3," 18. 23. 19. 27. 37. 28. 0. 0. 0. 43. 64. 44. ");
  TRMatrixD asc(3,4," 30. 44. 0. 69. 34. 49. 0. 74. 17. 26.  0. 42.");
  //  TRMatrixD atsac[6] = { 239. 331. 447. 248. 345. 257. };

  Double_t zerlev = 1e-6;

  Newguy("TRAAT-TRATA.", "TTRLA   ");
  TRMatrixD  ec(4,3,"4. 7. 3. 3. 6. 4. 0. 5. 5. 2. 1. 2.");
  TRSymMatrixD *A = new TRSymMatrixD(ec,kAxAT);
  TRSymMatrixD wc(4,"74. 66. 61. 50. 50. 50. 21. 20. 15. 9.");
  if (A->Verify(wc,zerlev)) {
    cout << "ec:" << ec << endl;
    cout << "wc:" << wc << endl;
    cout << "A ec,kAxAT" << *A << endl;
  }
  else cout << "ec,kAxAT passed" << endl;
  delete A;

  TRMatrixD  ecA(3,4,ec.GetArray());
  TRSymMatrixD *A = new TRSymMatrixD(ecA,kATxA);
  TRSymMatrixD xc(4,"77. 62. 69. 17.  23. 10. 52. 45. 11. 38.");
  if (A->Verify(xc,zerlev)) {
    cout << "ecA:" << ecA << endl;
    cout << "xc:" << xc << endl;
    cout << "A ecA,kATxA" << *A << endl;
  }
  else cout << "ecA,kATxA passed" << endl;
  delete A;
  
  Newguy("TRASAT-TRATSA-TRQSQ.", "TTRLA   ");
  TRSymMatrixD *A2 = new  TRSymMatrixD(ecA,kAxSxAT,ac);
  if (A2->Verify(sc,zerlev)) {
    cout << "ecA:" << ecA << endl;
    cout << "ac:" << ac << endl;
    cout << "A ecA,kAxSxAT,ac" << *A2 << endl;
  }
  else cout << "ecA,kAxSxATx,ac Passed" << endl;
  delete A2;

  TRSymMatrixD *A3 = new  TRSymMatrixD(ec,kATxSxA,dc);
  if (A3->Verify(atsac,zerlev)) {
    cout << "ec:" << ec << endl;
    cout << "dc:" << dc << endl;
    cout << "A ec,kATxSxA,dc" << *A3 << endl;
  }
  else cout << "ec,kATxSxA,dc Passed" << endl;
  delete A3;

  TRSymMatrixD *A4 = new  TRSymMatrixD(dc,kRxSxR,ac);
  if (A4->Verify(qsqc,zerlev)) {
    cout << "dc:" << dc << endl;
    cout << "ac:" << ac << endl;
    cout << "A dc,kRxSxR,ac" << *A4 << endl;
  }
  else cout << "dc,kRxSxR,ac Passed" << endl;
  delete A4;
  
  Newguy("TRSA-TRAS-TRSAT-TRATS.", "TTRLA   ");
  TRMatrixD *A5 = new  TRMatrixD(dc,kSxA, ec);
  if (A5->Verify(sac,zerlev)) {
    cout << "dc:" << dc << endl;
    cout << "ec:" << ec << endl;
    cout << "A dc,kSxA, ec" << *A5 << endl;
  }
  else cout << "dc,kSxA, ec Passed" << endl;
  delete A5;


  TRMatrixD *A6 = new  TRMatrixD(ecA,kAxS,dc);
  if (A6->Verify(asc,zerlev)) {
    cout << "ecA:" << ecA << endl;
    cout << "asc:" << asc << endl;
    cout << "" << *A6 << endl;
  }
  else cout << "acA,kAxS,dc Passed" << endl;
  delete A6;


  TRMatrixD *A7 = new  TRMatrixD(dc,kSxAT,ecA);
  TRMatrixD satc(4,3,"30. 34. 17. 44. 49. 26. 0. 0. 0. 69. 74. 42.");
  if (A7->Verify(satc,zerlev)) {
    cout << "dc:" << dc << endl;
    cout << "ecA:" << ecA << endl;
    cout << "A dc,kSxAT,ecA" << *A7 << endl;
  }
  else cout << "dc,kSxAT,ecA Passed" << endl;
  delete A7;
  TRMatrixD *A8 = new  TRMatrixD(ec,kATxS,dc);
  TRMatrixD  atsc(3,4," 18. 27. 0. 43. 23. 37. 0. 64. 19. 28. 0. 44.");
  if (A8->Verify(atsc,zerlev)) {
    cout << "ec:" << ec << endl;
    cout << "dc:" << dc << endl;
    cout << "A ec,kATxS,dc" << *A8 << endl;
  }
  else cout << "ec,kATxS,dc Passed" << endl;
  delete A8;

  Newguy("TRPCK-TRUPCK.", "TTRLA   ");
  TRMatrixD upckc(4,4," 1.,2.,4.,7.,2.,3.,5.,8.,4.,5.,6.,9.,7.,8.,9.,10.");
  TRSymMatrixD pckc(4,"1.,2.,3.,4.,5.,6.,7.,8.,9.,10.");
  TRSymMatrixD *A9 = new  TRSymMatrixD(upckc);
  if (A9->Verify(pckc,zerlev)) {
    cout << "upckc:" << upckc << endl;
    cout << "pckc:" << pckc << endl;
    cout << "A upckc" << *A9 << endl;
  }
  else cout << "packing of upckc Passed" << endl;
  delete A9;
  
  TRMatrixD *A10 = new  TRMatrixD(pckc);
  if (A10->Verify(upckc,zerlev)) {
    cout << "pckc:" << pckc << endl;
    cout << "upckc:" << upckc << endl;
    cout << "A  pckc" << *A10 << endl;
  }
  else cout << "unpacking of pckc Passed" << endl;


  TRSymMatrixD *A11 = new  TRSymMatrixD(*A10);
  if (A11->Verify(pckc,zerlev)) {
    cout << "A10:" << *A10 << endl;
    cout << "pckc:" << pckc << endl;
    cout << "A11:" << *A11 << endl;
  }
  else cout << "packing of upckc Passed" << endl;
  TRMatrixD *A12 = new  TRMatrixD(*A11);
  if (A11->Verify(upckc,zerlev)) {
    cout << "A11:" << *A11 << endl;
    cout << "upckc:" << upckc << endl;
    cout << "A12" << *A12 << endl;
  }
  else cout << "unpacking of A11 Passed" << endl;
  delete A10;
  delete A11;
  delete A12;

} /* trla */

//________________________________________________________________________________
void Micky(){
#ifdef __CINT__
  //  gSystem->Load("libStar");
  //  gSystem->Load("TR");
  gSystem->Load("../Linux-gcc-dbg/libRootKernel.so");
#endif
#if 1
  Tmxm(); 
  ttrinv();
#endif
  ttrla();
}
