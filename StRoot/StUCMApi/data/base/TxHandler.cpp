/*
 * TxHandler.cpp
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
 * @(#)cpp/api:$Id: TxHandler.cpp,v 1.1 2008/12/08 21:10:29 fine Exp $
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
 
#include "TxHandler.h"
#include "TxModuleFactory.h"
#include "TxDataException.h"


TxHandler::TxHandler()
  : module_(NULL)
{} // EMPTY


TxHandler::TxHandler(TxModule* module)
  : module_(NULL)
{
  setModule(module);
}


TxHandler::~TxHandler()
{} // EMPTY - Don't delete module since it's a static object!


TxModule* TxHandler::getModule() const 
{
  if (!module_) 
  {
    throw TxDataException("Attempted to perform operation on NULL module.",
			  TxDataException::STORE,
			  TxUCMException::ERROR);
  }
  return module_;
}


void TxHandler::setModule(TxModule* module)
{
  if (!module) 
  {
    throw TxDataException("Attempted to associate handler to NULL module.", 
			  TxDataException::STORE,
			  TxUCMException::ERROR);
  }
  
  if (module_) {
    delete module_;
  }

  module_ = module;
}
