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
