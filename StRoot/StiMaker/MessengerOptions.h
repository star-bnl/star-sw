#ifndef __CINT__
typedef pair<unsigned int, TGCheckButton*> MessengerPair;
#else
class MessengerPair;
#endif

#ifndef __CINT__
typedef vector<MessengerPair> MsgPairVec;
#else
class MsgPairVec;
#endif

class MessengerOptionsDialog : public TGTransientFrame
{
private:
    
    TGCompositeFrame     *f1, *f2, *f3;
    TGButton             *fTestButton, *fCloseButton;
    
    MsgPairVec fC;
    
    TGGroupFrame         *fG1;
    TGLayoutHints        *fL1, *fL2, *fL3, *fL4, *fL21;
    TGGC                  fRedTextGC;

    void updateMessenger();
    
public:
    MessengerOptionsDialog(const TGWindow *p, const TGWindow *main, UInt_t w, UInt_t h,
			   UInt_t options = kVerticalFrame);
    virtual ~MessengerOptionsDialog();
    virtual void CloseWindow();
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
};
