#include "cpptestercode.h"
#include "newfunc.h"

using namespace Plugins;
#include <deque>

namespace Cpp 
{
  using namespace std;
  
  struct Tester
  {
    typedef deque<int> dq;
    dq* l;

    Tester(void) : l(new dq()) {};
    ~Tester(void) {delete l;};
    
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
  };
}

using namespace Cpp;

int (*init)(Tester**) = wrapper1<Tester**, Tester::init>;
int (*addElem)(Tester*,int) = wrapper2<Tester*, int, Tester::addElem>;
int (*getElem)(Tester*,int*) = wrapper2<Tester*, int*, Tester::getElem> ;
int (*removeElem)(Tester*) = wrapper1<Tester*, Tester::removeElem>;
int (*resetElems)(Tester*) = wrapper1<Tester*, Tester::resetElems>;
int (*create)(void**) = wrapper1<void**, Tester::create>;
int (*createSized)(void**, size_t) 
   = wrapper2<void**, size_t, Tester::createSized>;
int (*destroy)(void*) = wrapper1<void*, Tester::destroy>;
int (*fini)(Tester*) = wrapper1<Tester*, Tester::fini>;
