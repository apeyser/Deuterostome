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
	wrapperMHs(init, Tester**);
	wrapperMHs(addElem, Tester*, int);
	wrapperMHs(getElem, Tester*, int*);
	wrapperMHs(removeElem, Tester*);
	wrapperMHs(resetElems, Tester*);
	wrapperMHs(create, void**);
	wrapperMHs(createSized, void**, size_t);
	wrapperMHs(destroy, void*);
	wrapperMHs(fini, Tester*);
	wrapperMHs(big, size_t, Tester*);
#if __cplusplus
}
#endif

#endif
