#ifndef StEventToolkit_h
#define StEventToolkit_h

class StEvent;
class StSPtrVecTrackNode;

class StEventToolkit {
private:
  StEventToolkit(){ mEvent = 0;}
 public:
static StEventToolkit* instance();
void SetEvent(const StEvent *evt) 	{mEvent = evt;}

const StSPtrVecTrackNode* getTrackContainer();

private:
const StEvent *mEvent;
};
#endif
