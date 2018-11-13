/**
 * \class StPicoBTofHit
 * \brief Stores BTOF hit information
 *
 * The StPicoBTofHit holds inofmation about hits in BTOF
 */

#ifndef StPicoBTofHit_h
#define StPicoBTofHit_h

// C++ headers
#include <limits>

// ROOT headers
#include "TObject.h"

//_________________
class StPicoBTofHit : public TObject {

 public:
  /// Default consturctor
  StPicoBTofHit();
  /// Constructor that takes id
  StPicoBTofHit(int id);
  /// Copy constructor
  StPicoBTofHit(const StPicoBTofHit &hit);
  /// Destructor
  virtual ~StPicoBTofHit();
  /// Print hit information
  virtual void Print(const Char_t* option = "") const;  ///< Print trigger info

  //
  // Getters
  //

  /// Return ID of the hit
  Int_t   id() const;
  /// Return tray number
  Int_t   tray() const;
  /// Return module number
  Int_t   module() const;
  /// Return cell number
  Int_t   cell() const;

  //
  // Setters
  //

  /// Set ID of the hit
  void setId(Int_t id);
  /// Set ID of the track using track, module and cell
  void setId(Int_t tray, Int_t module, Int_t cell);

 private:

  /// Id encodes (tray-1)*192+(module-1)*6+(cell-1):
  Short_t mId;     

  ClassDef(StPicoBTofHit, 2)
};

//
// Getters
//
inline Int_t StPicoBTofHit::id() const { return mId; }
inline Int_t StPicoBTofHit::tray() const { return mId / 192 + 1; }
inline Int_t StPicoBTofHit::module() const { return (mId % 192) / 6 + 1; }
inline Int_t StPicoBTofHit::cell() const { return mId / 6 + 1; }

//
// Setters
//
inline void StPicoBTofHit::setId(Int_t id) {
  if (id<0) {
    mId = -1;
  }
  else {
    mId = (id > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)id;
  }
}
#endif
