//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#ifndef KFPartMatch_H
#define KFPartMatch_H

#include <vector>

struct KFPartMatch // used for Reco to MC match as well as for MC to Reco
{
  KFPartMatch():ids(),idsMI(){};
  
  bool IsMatched() const { return ids.size() != 0 || idsMI.size() != 0; };
  bool IsMatchedWithPdg() const { return ids.size() != 0; };
  int  GetBestMatch() const { 
    if      (ids.size()   != 0) return ids[0];
    else if (idsMI.size() != 0) return idsMI[0];
    else return -1;
  };
  int  GetBestMatchWithPdg() const { 
    if      (ids.size()   != 0) return ids[0];
    else return -1;
  };
  std::vector<int> ids;
  std::vector<int> idsMI; // matched but pdg is different - miss identification
};

#endif
