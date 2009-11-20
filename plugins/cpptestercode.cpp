#include "cpptestercode.h"

using namespace Plugins;
#include <deque>

namespace Cpp 
{
  using namespace std;
  
  struct Tester
  {
    typedef deque<int> dq;
    dq* l;
    char* buf;

    Tester(void) : l(new dq()), buf(NULL) {};
    ~Tester(void) {delete l; delete buf;};
    
    static void init(Tester** t) {*t = new Tester();};
    static void addElem(Tester* t, int i) {t->l->push_back(i);};
    static void getElem(Tester* t, int* i) {*i = (*t->l)[*i];};
    static void removeElem(Tester* t) {t->l->pop_back();};
    static void resetElems(Tester* t) {
      delete t->l;
      t->l = new Tester::dq();
    };
    static void fini(Tester* t) {delete t;};
    static void create(void** i) {*i = new int;};
    static void destroy(void* i) {delete (int*) i;};
    static void createSized(void** i, size_t s) {*i = new char[s];};
		static void big(size_t size, Tester* t) {t->buf = new char[size];};
  };
}

using namespace Cpp;

wrapperMs(1, Tester, init, Tester**);
wrapperMs(2, Tester, addElem, Tester*, int);
wrapperMs(2, Tester, getElem, Tester*, int*);
wrapperMs(1, Tester, removeElem, Tester*);
wrapperMs(1, Tester, resetElems, Tester*);
wrapperMs(1, Tester, create, void**);
wrapperMs(2, Tester, createSized, void**, size_t);
wrapperMs(1, Tester, destroy, void*);
wrapperMs(1, Tester, fini, Tester*);
wrapperMs(2, Tester, big, size_t, Tester*);
