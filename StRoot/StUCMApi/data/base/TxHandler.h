/*
 * TxHandler.h
 *
 * Created on Aug 20, 2007
 *
 * Author: Zhengqiang Liang (Wayne State University)
 *            Valeri Fine (Brookhaven National Laboratory)
 *            Jerome Lauret (Brookhaven National Laboratory)
 *            Stephen Tramer (Tech-X Corp.)
 *
 *
 * This file is part of the UCM project funded under an SBIR
 * Copyright (c) 2007-2008 STAR Collaboration - Brookhaven National Laboratory
 *
 * @(#)cpp/api:$Id: TxHandler.h,v 1.1 2008/12/08 21:10:30 fine Exp $
 *
 *
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * STAR Scheduler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with STAR Scheduler; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
 
#ifndef TXHANDLER_H
#define TXHANDLER_H

#include <string>
#include "TxModule.h"

/**
 * Base class for all handlers.  Provided for extensibility -
 * and because all handlers need access to the module.  Never
 * intended to be directly instantiated.
 */
class TxHandler {
 public: 
  enum Operation {
    SELECT,
    INSERT,
    DELETE,
    UPDATE
  };

 protected:
  /**
   * Constructor: Set module to NULL.  Note that if
   * a subclass calls this constructor implicitly then
   * it MUST perform some kind of checking for an end
   * result which is a NULL module.
   */
  TxHandler();
  
  /**
   * Constructor: Set module to provided value.
   * @throws TxDataException If module == NULL.  Note that in this situation
   *                         the subclass is expected to check for NULL modules.
   */
  TxHandler(TxModule* module);

  /**
   * Destructor: Delete module.
   */
  ~TxHandler();

  /**
   * Returns the module in a safe manner, by checking to make sure it
   * is non-NULL.
   */
  TxModule* getModule() const;

  /**
   * Sets the module in a safe manner, by checking to make sure it
   * is non-NULL.
   */
  void setModule(TxModule* module);

 private:
  TxModule* module_;
};

#endif
