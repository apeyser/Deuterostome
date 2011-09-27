/*

Copyright 2011 Alexander Peyser & Wolfgang Nonner

This file is part of Deuterostome.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
#ifndef CPPTESTERCODE_H
#define CPPTESTERCODE_H

#include "cppplugin.h"

#if __cplusplus

#include <cstddef>

namespace Cpp {struct Tester;}
using namespace Cpp;

extern "C" 
{
#else
  #include <stdlib.h>
  typedef struct Tester {} Tester;
#endif
wrapperMHs(1, Tester, init, Tester**);
wrapperMHs(2, Tester, addElem, Tester*, int);
wrapperMHs(2, Tester, getElem, Tester*, int*);
wrapperMHs(1, Tester, removeElem, Tester*);
wrapperMHs(1, Tester, resetElems, Tester*);
wrapperMHs(1, Tester, create, void**);
wrapperMHs(2, Tester, createSized, void**, size_t);
wrapperMHs(1, Tester, destroy, void*);
wrapperMHs(1, Tester, fini, Tester*);
wrapperMHs(2, Tester, big, size_t, Tester*);
#if __cplusplus
}
#endif

#endif
