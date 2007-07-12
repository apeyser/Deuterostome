#ifndef CPPTESTERCODE_H
#define CPPTESTERCODE_H

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
  extern int (*init)(Tester**);
  extern int (*addElem)(Tester*, int v);
  extern int (*getElem)(Tester*, int* i);
  extern int (*removeElem)(Tester*);
  extern int (*resetElems)(Tester*);
  extern int (*create)(void**);
  extern int (*createSized)(void**, size_t);
  extern int (*destroy)(void*);
  extern int (*fini)(Tester*);
#if __cplusplus
}
#endif

#endif
