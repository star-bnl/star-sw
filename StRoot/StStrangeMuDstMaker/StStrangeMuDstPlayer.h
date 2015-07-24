#ifndef StStrangeMuDstPlayer
#define StStrangeMuDstPlayer

class StFile;

class StrangeMuDstPlayer {
 public:
  StrangeMuDstPlayer();
  virtual ~StrangeMuDstPlayer() {}
  void Make(Int_t NEvents, StFile* input, Char_t *output="evMuDst.root");
  void Filter(Int_t NEvents, StFile* input, Char_t *output="evMuDst.root");
  void Play(Int_t NEvents, StFile* input, Char_t *output="evMuDst.root");
  void Copy(Int_t NEvents, StFile* input, Char_t *output="evMuDst.root");
  void DoFileSplit(Bool_t val = kTRUE);
  void DoMC(Bool_t val = kTRUE);
  void DoT0Abort(Bool_t val = kTRUE);
  //   void DoTopMapFix(Bool_t val = kTRUE); // RWitt: Commented 7/24/15 to remove dependency on StHbtMaker
  void DoReadDST(Bool_t val = kTRUE);
  void DoV0(Bool_t val = kTRUE);
  void DoXi(Bool_t val = kTRUE);
  void DoKink(Bool_t val = kTRUE);
  void SetEvPrimTracks(unsigned int);
  void SetEvPrimVertexZ(Float_t);
  void SetV0DecayLength(Float_t);
  void SetV0DcaDaughtersToPrimVertex(Float_t);
  void SetV0DcaToPrimVertex(Float_t);
  void SetV0DcaDaughters(Float_t);
  void SetV0NumTpcHits(unsigned int);
  void SetXiDecayLength(Float_t);
  void SetXiDcaDaughters(Float_t);
  void SetXiDcaV0Daughters(Float_t);
  void SetXiDcaToPrimVertex(Float_t);
  void SetXiDcaV0ToPrimVertex(Float_t);
  void SetXiDcaBachelorToPrimVertex(Float_t);
 protected:
  Bool_t doFileSplit;
  Bool_t doMC;
  Bool_t doT0Abort;
  Bool_t doTopMapFix;
  Bool_t doReadDST;
  Bool_t doV0;
  Bool_t doXi;
  Bool_t doKink;
  const char* outputFile;
  unsigned int evPrimTracks;
  float evPrimVertexZ;
  float v0DecayLength;
  float v0DcaDaughtersToPrimVertex;
  float v0DcaToPrimVertex;
  float v0DcaDaughters;
  float v0NumTpcHits;
  float xiDecayLength;
  float xiDcaDaughters;
  float xiDcaV0Daughters;
  float xiDcaToPrimVertex;
  float xiDcaV0ToPrimVertex;
  float xiDcaBachelorToPrimVertex;
  ClassDef(StrangeMuDstPlayer,0)
};

inline void StrangeMuDstPlayer::SetEvPrimTracks(unsigned int val)
            { evPrimTracks = val; }
inline void StrangeMuDstPlayer::SetEvPrimVertexZ(Float_t val)
            { evPrimVertexZ = val; }
inline void StrangeMuDstPlayer::SetV0DecayLength(Float_t val)
            { v0DecayLength = val; }
inline void StrangeMuDstPlayer::SetV0DcaDaughtersToPrimVertex(Float_t val)
            { v0DcaDaughtersToPrimVertex = val; }
inline void StrangeMuDstPlayer::SetV0DcaToPrimVertex(Float_t val)
            { v0DcaToPrimVertex = val; }
inline void StrangeMuDstPlayer::SetV0DcaDaughters(Float_t val)
            { v0DcaDaughters = val; }
inline void StrangeMuDstPlayer::SetV0NumTpcHits(unsigned int val)
            { v0NumTpcHits = val; }
inline void StrangeMuDstPlayer::SetXiDecayLength(Float_t val)
            { xiDecayLength = val; }
inline void StrangeMuDstPlayer::SetXiDcaDaughters(Float_t val)
            { xiDcaDaughters = val; }
inline void StrangeMuDstPlayer::SetXiDcaV0Daughters(Float_t val)
            { xiDcaV0Daughters = val; }
inline void StrangeMuDstPlayer::SetXiDcaToPrimVertex(Float_t val)
            { xiDcaToPrimVertex = val; }
inline void StrangeMuDstPlayer::SetXiDcaV0ToPrimVertex(Float_t val)
            { xiDcaV0ToPrimVertex = val; }
inline void StrangeMuDstPlayer::SetXiDcaBachelorToPrimVertex(Float_t val)
            { xiDcaBachelorToPrimVertex = val; }
inline void StrangeMuDstPlayer::DoFileSplit(Bool_t val)
            { doFileSplit = val; }
inline void StrangeMuDstPlayer::DoMC(Bool_t val)
            { doMC = val; }
inline void StrangeMuDstPlayer::DoT0Abort(Bool_t val)
            { doT0Abort = val; }
// RWitt: Commented 7/24/15 to remove dependency on StHbtMaker
// inline void StrangeMuDstPlayer::DoTopMapFix(Bool_t val)
//             { doTopMapFix = val; }
inline void StrangeMuDstPlayer::DoReadDST(Bool_t val)
            { doReadDST = val; }
inline void StrangeMuDstPlayer::DoV0(Bool_t val)
            { doV0 = val; }
inline void StrangeMuDstPlayer::DoXi(Bool_t val)
            { doXi = val; }
inline void StrangeMuDstPlayer::DoKink(Bool_t val)
            { doKink = val; }
#endif

