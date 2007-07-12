#include "cpptestercode.h"
#include "newfunc.h"

using namespace Cpp;
using namespace Plugins;

Tester::Tester(void)
		: l(new dq()) 
{}

Tester::~Tester(void) 
{
		delete l;
}

Tester* Tester::tester = NULL;

extern "C" 
{
		WrapperMF(init, Init,{Tester::tester = new Tester();});
		Wrapper1MF(addElem, AddElem, int, {Tester::tester->l->push_back(a);});
		Wrapper1MF(getElem, GetElem, int*, {*a = (*Tester::tester->l)[*a];});
		WrapperMF(removeElem, RemoveElem, {Tester::tester->l->pop_back();});
		WrapperMF(resetElems, ResetElems, {						\
						delete Tester::tester->l;											\
						Tester::tester->l = new Tester::dq();});
		WrapperMF(fini, Fini, {delete Tester::tester;});
		Wrapper1MF(create, Create, void**, {*a = new int;});
		Wrapper1MF(destroy, Destroy, void*, {delete (int*) a;});
		Wrapper2MF(createSized, CreateSized, void**, size_t, {*a = new char[b];});
}

