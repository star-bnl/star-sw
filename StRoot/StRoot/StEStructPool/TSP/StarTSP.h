/*********************************************************************
 *
 *  $Id: StarTSP.h,v 1.2 2011/08/02 20:43:55 prindle Exp $
 *
 *  Author: Duncan Prindle
 *
 *********************************************************************
 *
 *  Discription:
 *    Interface to code for solving travelling Salesperson Problem.
 *    Currently using Keld Helsgaun (keld@ruc.dk). See
 *        http://www.akira.ruc.dk/~keld/research/LKH/
 *
 ********************************************************************/

#ifndef StarTSP_h
#define StarTSP_h

#include <TROOT.h>
#include <TChain.h>
#include <TChainElement.h>
#include <TLeaf.h>

class StarTSP {
  private:
  public:
    StarTSP(int iDimension);
    virtual ~StarTSP();

    int* getFileNumbers(TChain *ch, int nGood, int *index);
    int* sortLists(float *mult, float *z, float *f);

    int dimension;
    float mMultScale;
    float mZScale;
    float mZDCScale;

    ClassDef(StarTSP,1)
};


#endif
