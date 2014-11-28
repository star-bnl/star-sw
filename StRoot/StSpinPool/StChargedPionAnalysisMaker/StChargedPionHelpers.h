#ifndef STAR_StChargedPionHelpers
#define STAR_StChargedPionHelpers

// $Id: StChargedPionHelpers.h,v 1.3 2010/07/16 20:23:38 rfatemi Exp $

/*****************************************************************************
 * @class StChargedPionHelpers
 * @author Adam Kocoloski
 *
 * Description of class.
 *****************************************************************************/

#include <vector>
using std::vector;
 
class TLorentzVector;

class StChargedPionBaseEv;
class StChargedPionEvent;
class StChargedPionJet;
class StChargedPionMcEvent;
class StChargedPionTrack;

class StJet;
class StJets;
class StJetSkimEvent;
class StMiniMcEvent;
class StMuTrack;
class StPythiaEvent;
class TrackToJetIndex;

#include "TObject.h"

class StChargedPionHelpers : public TObject 
{
public:
    virtual ~StChargedPionHelpers();

    static void translateEvent(StJetSkimEvent*, StChargedPionBaseEv*);
    static void translateJet(StJet*, vector<TLorentzVector*>, StChargedPionJet*);
    static void translateJets(StJets*, StChargedPionBaseEv*);
    static void translateJets(StJets*, StChargedPionMcEvent*);
    static void translatePythia(const StPythiaEvent*, StChargedPionMcEvent*);
    static void translateMinimc(const StMiniMcEvent*, StChargedPionMcEvent*);
    static void translateMuDst(StChargedPionEvent*);
    static void translateMuDst(StChargedPionMcEvent*);
    static void translateTrack(const StMuTrack*, StChargedPionTrack*);

private:
    static void translateMuDst(StChargedPionBaseEv *ev);

    ClassDef(StChargedPionHelpers, 1)
};

#endif

/*****************************************************************************
 * $Log: StChargedPionHelpers.h,v $
 * Revision 1.3  2010/07/16 20:23:38  rfatemi
 * Changes in PionAnalysisMaker code to ensure backward compatibility with updates in the Jet code.  Affects how to access particles in the jets.
 *
 * Revision 1.1  2008/07/17 17:06:30  kocolosk
 * big-bang integration StChargedPionMcEvent framework
 *
 *****************************************************************************/
