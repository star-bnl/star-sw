/*! 
 * /file  StiDisplayManager.h
 * /brief Abstract Display Manager
 * /author Claude A Pruneau, Wayne State University, 
 * /date   March 2002
 * /copyright 2002, STAR  Experiment at BNL, All rights reserved.  
 *  
 * Permission to use, copy, modify and distribute this software and its
 * documentation strictly for non-commercial purposes is hereby granted 
 * without fee, provided that the above copyright notice appears in all
 * copies and that both the copyright notice and this permission notice
 * appear in the supporting documentation. The authors make no claims 
 * about the suitability of this software for any purpose. It is     
 * provided "as is" without express or implied warranty.             
 */
#ifndef StiDisplayManager_HH
#define StiDisplayManager_HH

class StiDrawable;

class StiDisplayManager
{
public:
  //Singleton access
  static void kill();
  
  virtual void addDrawable(StiDrawable*)=0;
  //clean up after each event (e.g., remove tracks)
  virtual void reset()=0;
  
  virtual void cd()=0;
  virtual void update()=0;
  virtual void draw()=0;
  
  virtual void setVisible(bool value)=0;
  virtual void setVisible(const StiDrawable*, bool value)=0;
  virtual void setVisible(const char * name, bool value)=0;
  
  virtual void setView(int view)=0;
  virtual void print() const=0;
  
 protected: 
  
  static StiDisplayManager * sinstance;
};

#endif
